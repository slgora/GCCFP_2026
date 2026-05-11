# =============================================================================
# PRODUCTION SDM PIPELINE — 7-Batch Manual Parallel via 7 R Sessions
# Only species NOT in sdm_completed_species.txt will be processed!
# =============================================================================

# ===========================
# SET THIS AT THE TOP OF EACH SESSION
MY_BATCH <- 1    # Set to a unique number 1 .. 7 for each session!
# ===========================

setwd("C:/Users/SoftwareAdmin/Desktop/MassScale_SDMs_GapAnalysis")
closeAllConnections(); gc()

# ----------- LOAD PACKAGES ----------
##install.packages("remotes")
##remotes::install_github("sjevelazco/flexsdm")
library(Rcpp)
library(dplyr)
library(sf)
library(terra)
library(readr)
library(fst)
library(tibble)
library(stringr)
library(tidyr)
library(purrr)
library(maxnet)
library(flexsdm)

cat(capture.output(sessionInfo()), sep = "\n")  # diagnostic (optional)

set.seed(1234)

# ----------- CONFIG VARIABLES ----------
PRES_CAP         <- 5000
BG_POINT_CAP     <- 5000
MIN_RECORDS      <- 8
MAXNET_CLASSES   <- "lq"
PRE_PICKED_VARS  <- c("wc2.1_2.5m_bio_1","wc2.1_2.5m_bio_2","wc2.1_2.5m_bio_4",
                      "wc2.1_2.5m_bio_5","wc2.1_2.5m_bio_6","wc2.1_2.5m_bio_12",
                      "wc2.1_2.5m_bio_15","wc2.1_2.5m_bio_18")

inputs_root <- "C:/Users/SoftwareAdmin/Desktop/MassScale_SDMs_GapAnalysis/GCCFP_inputs"
occ_all_fst <- file.path(inputs_root, "occurrences", "occ_prepped_all_species.fst")
bio_rds     <- file.path(inputs_root, "worldclim", "worldclim_v2.1_bio_2.5m_wrapped_LOCAL.rds")
eco_path    <- file.path(inputs_root, "ecoregions", "tnc_terr_ecoregions.gpkg")
output_root    <- file.path(getwd(), "SDM_all_outputs_24hr_run")
sdm_export_dir <- file.path(output_root, "production_sdms_FAST24")
batch_log_root <- file.path(output_root, "batch_logs")
terra_tmp_root <- file.path(output_root, "terra_tmp_workers_fast")
for (d in c(output_root, sdm_export_dir, batch_log_root, terra_tmp_root))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
run_log_file <- file.path(batch_log_root,
                          paste0("production_log_FAST24_batch", MY_BATCH, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
)

# -------------- LOAD GLOBAL INPUTS --------------
message("Loading global inputs ...")
bioVars_wrapped <- readRDS(bio_rds)
bioVars_check   <- terra::unwrap(bioVars_wrapped)
present_vars <- intersect(PRE_PICKED_VARS, names(bioVars_check))
if (length(present_vars) < 6) {
  message("WARNING: pre-picked var names not found.")
  PRE_PICKED_VARS <- names(bioVars_check)[1:min(8, terra::nlyr(bioVars_check))]
  message("Falling back to first ", length(PRE_PICKED_VARS), " layers.")
} else {
  PRE_PICKED_VARS <- present_vars
}
message("Using BIO vars (", length(PRE_PICKED_VARS), "): ",
        paste(PRE_PICKED_VARS, collapse = ", "))
rm(bioVars_check); gc()

ecoregions <- sf::st_read(eco_path, quiet = TRUE) |>
  sf::st_make_valid() |> sf::st_zm(drop = TRUE, what = "ZM")
ecoregions <- ecoregions[sf::st_geometry_type(ecoregions) %in%
                           c("POLYGON","MULTIPOLYGON"), ]

# -------------- LOAD OCCURRENCE DATA AND EXCLUDE COMPLETED SPECIES --------------
speciesData_all <- fst::read_fst(occ_all_fst)

# --------- EXCLUDE SPECIES ALREADY MODELED (key update!) ---------
if (file.exists("sdm_completed_species.txt")) {
  species_done <- unique(readLines("sdm_completed_species.txt"))
  message(sprintf("Loaded %d completed species from sdm_completed_species.txt", length(species_done)))
} else {
  species_done <- character(0)
  message("No sdm_completed_species.txt found, all species included.")
}
speciesData_all <- speciesData_all[ !speciesData_all$taxon %in% species_done, ]
message(sprintf("Processing %d occurrence records after excluding completed species.", nrow(speciesData_all)))

# -------------- SPECIES QUEUE --------------
message("Building species queue (smallest-first) ...")
species_counts <- speciesData_all |>
  dplyr::group_by(taxon) |>
  dplyr::summarise(n_records = dplyr::n(), .groups = "drop") |>
  dplyr::filter(n_records >= MIN_RECORDS) |>
  dplyr::arrange(n_records)
queue_taxa_full <- species_counts$taxon

# --------- MANUAL SPLIT OVER 7 SESSIONS ---------
n_batches <- 7
n_species <- length(queue_taxa_full)
species_per_batch <- ceiling(n_species / n_batches)
my_start <- (MY_BATCH - 1) * species_per_batch + 1
my_end   <- min(MY_BATCH * species_per_batch, n_species)
queue_taxa <- queue_taxa_full[my_start:my_end]

message(sprintf("[Batch %d/%d] Will process %d species: species %d to %d of %d total", 
                MY_BATCH, n_batches, length(queue_taxa), my_start, my_end, n_species))

# Take only this batch's species for fitting etc.
occ_split <- speciesData_all |>
  dplyr::filter(taxon %in% queue_taxa,
                !is.na(longitude), !is.na(latitude)) |>
  dplyr::distinct(taxon, longitude, latitude, .keep_all = TRUE) |>
  dplyr::group_split(taxon)
names(occ_split) <- vapply(occ_split, function(d) d$taxon[1], character(1))
ecoregions_packed <- list(geom_wkb = sf::st_as_binary(sf::st_geometry(ecoregions)))

# --------- FITTING FUNCTION --------------
fit_one_species_fast <- function(sp,
                                 sd1,
                                 ecoregions_packed,
                                 bioVars_wrapped,
                                 sdm_export_dir,
                                 terra_tmp_root,
                                 PRES_CAP, BG_POINT_CAP,
                                 PRE_PICKED_VARS, MAXNET_CLASSES) {
  worker_tmp <- file.path(terra_tmp_root, paste0("w_", Sys.getpid()))
  dir.create(worker_tmp, showWarnings = FALSE, recursive = TRUE)
  terra::terraOptions(todisk = FALSE, tempdir = worker_tmp, memfrac = 0.3)
  bioVars   <- terra::unwrap(bioVars_wrapped)
  bioVars   <- bioVars[[PRE_PICKED_VARS]]
  ecos_geom <- sf::st_as_sfc(ecoregions_packed$geom_wkb, crs = 4326)
  ecoregions <- sf::st_sf(geometry = ecos_geom)
  t0 <- Sys.time()
  out_tif <- file.path(sdm_export_dir,
                       paste0(gsub(" ", "_", sp), "_sdm_binary_gap.tif"))
  if (file.exists(out_tif)) {
    return(tibble::tibble(
      species = sp, status = "skipped_exists", qc_reason = NA_character_,
      thr_value = NA_real_, n_suitable_cells = NA_integer_,
      n_vars = NA_integer_, n_records = nrow(sd1),
      elapsed_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")),
      completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  }
  step <- "init"
  res <- tryCatch({
    step <- "occurrences"
    if (nrow(sd1) < 8) stop("LT_8_RECORDS")
    if (nrow(sd1) > PRES_CAP) sd1 <- sd1[sample.int(nrow(sd1), PRES_CAP), ]
    pres <- tibble::tibble(x = sd1$longitude, y = sd1$latitude, pr_ab = 1L)
    pres_sf <- sf::st_as_sf(pres, coords = c("x","y"),
                            crs = 4326, remove = FALSE)
    step <- "calib_area (ecoregions)"
    hit  <- sf::st_intersects(ecoregions, pres_sf, sparse = FALSE)
    used <- ecoregions[apply(hit, 1, any), ]
    if (nrow(used) == 0) stop("EMPTY_CALIB_AREA")
    ca      <- used |> sf::st_make_valid() |> sf::st_union() |> sf::st_sf()
    ca_vect <- terra::vect(ca)
    step <- "crop bioVars"
    bio_local <- terra::crop(bioVars, ca_vect) |> terra::mask(ca_vect)
    step <- "sample_background"
    n_bg <- min(BG_POINT_CAP, terra::ncell(bio_local))
    bg <- flexsdm::sample_background(
      data = pres, x = "x", y = "y", n = n_bg,
      method = "random", rlayer = bio_local[[1]], calibarea = ca_vect)
    bg$pr_ab <- 0L
    pres_xy <- pres[, c("x","y","pr_ab")]
    bg_xy   <- bg[,   c("x","y","pr_ab")]
    all_pts <- dplyr::bind_rows(pres_xy, bg_xy)
    step <- "sdm_extract"
    all_pts <- flexsdm::sdm_extract(
      data = all_pts, x = "x", y = "y",
      env_layer = bio_local, filter_na = TRUE)
    if (nrow(all_pts) < 8) stop("TOO_FEW_AFTER_EXTRACT")
    step <- "maxnet (lq, native)"
    keep_vars <- intersect(PRE_PICKED_VARS, names(all_pts))
    if (length(keep_vars) < 2) stop("NO_PREDICTORS")
    p_vec <- all_pts$pr_ab
    x_mat <- as.data.frame(all_pts[, keep_vars, drop = FALSE])
    ok_rows <- stats::complete.cases(x_mat) &
      apply(x_mat, 1, function(z) all(is.finite(z)))
    p_vec <- p_vec[ok_rows]; x_mat <- x_mat[ok_rows, , drop = FALSE]
    if (sum(p_vec == 1) < 5 || sum(p_vec == 0) < 50)
      stop("INSUFFICIENT_DATA_FOR_MAXNET")
    mn <- maxnet::maxnet(p = p_vec, data = x_mat,
                         f = maxnet::maxnet.formula(p_vec, x_mat,
                                                    classes = MAXNET_CLASSES))
    if (is.null(mn) || !inherits(mn, "maxnet")) stop("MAXNET_FIT_FAILED")
    step <- "terra::predict"
    pred_fun <- function(model, newdata, ...) {
      stats::predict(model, newdata = newdata, type = "cloglog", clamp = TRUE)
    }
    suit_cont <- terra::predict(
      object = bio_local[[keep_vars]], model = mn,
      fun = pred_fun, na.rm = TRUE)
    if (terra::nlyr(suit_cont) > 1) suit_cont <- suit_cont[[1]]
    test_vals <- terra::values(suit_cont); test_vals <- test_vals[is.finite(test_vals)]
    if (length(unique(round(test_vals, 4))) < 3)
      stop(paste0("PREDICT_RETURNED_CONSTANT n_unique=",
                  length(unique(round(test_vals, 4)))))
    step <- "compute threshold (max TSS)"
    train_preds <- terra::extract(suit_cont, cbind(all_pts$x, all_pts$y))
    if (is.data.frame(train_preds)) train_preds <- train_preds[[1]]
    train_df <- data.frame(obs = all_pts$pr_ab, pred = train_preds)
    train_df <- train_df[is.finite(train_df$pred), ]
    if (sum(train_df$obs == 1) < 2 || sum(train_df$obs == 0) < 2)
      stop("INSUFFICIENT_OBS_FOR_THRESHOLD")
    cand <- sort(unique(stats::quantile(
      train_df$pred, probs = seq(0.01, 0.99, by = 0.01), na.rm = TRUE)))
    tss_vals <- vapply(cand, function(th) {
      tp <- sum(train_df$pred >= th & train_df$obs == 1)
      fn <- sum(train_df$pred <  th & train_df$obs == 1)
      fp <- sum(train_df$pred >= th & train_df$obs == 0)
      tn <- sum(train_df$pred <  th & train_df$obs == 0)
      sens <- tp / (tp + fn); spec <- tn / (tn + fp)
      sens + spec - 1
    }, numeric(1))
    thr_val <- cand[which.max(tss_vals)]
    if (!is.finite(thr_val)) stop("THRESHOLD_NA")
    step <- "build binary + write"
    rcl <- matrix(c(-Inf, thr_val, 0, thr_val, Inf, 1), ncol = 3, byrow = TRUE)
    binary <- terra::classify(suit_cont, rcl, include.lowest = TRUE)
    binary <- terra::mask(binary, ca_vect)
    terra::writeRaster(binary, filename = out_tif, overwrite = TRUE,
                       gdal = c("COMPRESS=DEFLATE","PREDICTOR=2","TILED=YES"))
    n_suit <- sum(terra::values(binary) == 1, na.rm = TRUE)
    list(status = "sdm_success", reason = NA_character_,
         thr_value = thr_val, n_suitable_cells = n_suit,
         n_vars = length(keep_vars))
  }, error = function(e) list(status = "failed",
                              reason = paste0(step, " | ", conditionMessage(e)),
                              thr_value = NA_real_,
                              n_suitable_cells = NA_integer_,
                              n_vars = NA_integer_))
  tibble::tibble(
    species          = sp,
    status           = res$status,
    qc_reason        = res$reason,
    thr_value        = res$thr_value,
    n_suitable_cells = res$n_suitable_cells,
    n_vars           = res$n_vars,
    n_records        = nrow(sd1),
    elapsed_sec      = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    completed_at     = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

# ----------- LOGGING HELP ------------
append_log <- function(row, log_file) {
  if (!file.exists(log_file)) readr::write_csv(row, log_file)
  else readr::write_csv(row, log_file, append = TRUE)
}

# ----------- RUN THE SPECIES FOR THIS BATCH, SEQUENTIALLY ----------
overall_start <- Sys.time()
chunk_size <- 50
chunks <- split(queue_taxa, ceiling(seq_along(queue_taxa) / chunk_size))
n_done_session <- 0; n_failed_session <- 0; n_skipped_session <- 0

for (ci in seq_along(chunks)) {
  ck <- chunks[[ci]]
  t_chunk <- Sys.time()
  message(sprintf("── Batch %d Chunk %d/%d  (species %d-%d of %d in this batch) ──",
                  MY_BATCH, ci, length(chunks),
                  (ci-1)*chunk_size + 1,
                  min(ci*chunk_size, length(queue_taxa)),
                  length(queue_taxa)))
  results_chunk <- purrr::map_dfr(
    ck,
    function(sp) fit_one_species_fast(
      sp,
      sd1                = occ_split[[sp]],
      ecoregions_packed  = ecoregions_packed,
      bioVars_wrapped    = bioVars_wrapped,
      sdm_export_dir     = sdm_export_dir,
      terra_tmp_root     = terra_tmp_root,
      PRES_CAP           = PRES_CAP,
      BG_POINT_CAP       = BG_POINT_CAP,
      PRE_PICKED_VARS    = PRE_PICKED_VARS,
      MAXNET_CLASSES     = MAXNET_CLASSES)
  )
  for (i in seq_len(nrow(results_chunk))) append_log(results_chunk[i, ], run_log_file)
  n_succ <- sum(results_chunk$status == "sdm_success")
  n_fail <- sum(results_chunk$status == "failed")
  n_skip <- sum(results_chunk$status == "skipped_exists")
  n_done_session    <- n_done_session    + n_succ
  n_failed_session  <- n_failed_session  + n_fail
  n_skipped_session <- n_skipped_session + n_skip
  el_chunk        <- as.numeric(difftime(Sys.time(), t_chunk, units = "secs"))
  el_total        <- as.numeric(difftime(Sys.time(), overall_start, units = "secs"))
  done_so_far     <- min(ci * chunk_size, length(queue_taxa))
  remaining_sp    <- length(queue_taxa) - done_so_far
  rate_sp_per_sec <- done_so_far / el_total
  eta_hr          <- if (rate_sp_per_sec > 0) remaining_sp / rate_sp_per_sec / 3600 else NA
  message(sprintf("  Chunk done in %.1f min   |   succ=%d  fail=%d  skip=%d",
                  el_chunk/60, n_succ, n_fail, n_skip))
  message(sprintf("  Session totals: succ=%d  fail=%d  skip=%d   |   ETA: %.1f hr (%.2f days)",
                  n_done_session, n_failed_session, n_skipped_session,
                  eta_hr, eta_hr/24))
  gc()
}

overall_elapsed_hr <- as.numeric(difftime(Sys.time(), overall_start, units = "hours"))
message(sprintf("\n══════════════════════════════════════════════════════"))
message(sprintf("  BATCH %d COMPLETE", MY_BATCH))
message(sprintf("  Wall time this session: %.2f hr (%.2f days)",
                overall_elapsed_hr, overall_elapsed_hr/24))
message(sprintf("  Succeeded this session: %d", n_done_session))
message(sprintf("  Failed this session:    %d", n_failed_session))
message(sprintf("  Skipped (already done): %d", n_skipped_session))
message(sprintf("  Log: %s", run_log_file))
message(sprintf("══════════════════════════════════════════════════════"))