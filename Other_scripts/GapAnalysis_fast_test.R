# =============================================================================
# Gap analysis — 10-species hybrid SDM test
# Plain R script (not RMarkdown). Run interactively or via Rscript.
# =============================================================================

setwd("C:/Users/SoftwareAdmin/Desktop/MassScale_SDMs_GapAnalysis")
closeAllConnections(); gc()

# ── 1. PACKAGES ───────────────────────────────────────────────────────────────
list.of.packages <- c(
  "remotes","fst","dplyr","tidyr","stringr","terra","sf",
  "leaflet","openxlsx","knitr","kableExtra","readr","tibble"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages))
  install.packages(new.packages, dependencies = c("Depends","Imports"))

if (!requireNamespace("GapAnalysis", quietly = TRUE))
  remotes::install_github("CIAT-DAPA/GapAnalysis")

suppressPackageStartupMessages({
  library(GapAnalysis); library(fst); library(dplyr); library(tidyr)
  library(stringr); library(terra); library(sf); library(openxlsx)
  library(readr); library(tibble)
})

# ── 2. CONFIG ─────────────────────────────────────────────────────────────────
outputs_folder <- "outputs"
if (!dir.exists(outputs_folder)) dir.create(outputs_folder, recursive = TRUE)

sdm_root <- "C:/Users/SoftwareAdmin/Desktop/MassScale_SDMs_GapAnalysis/outputs/timing_test_sdms_hybrid_10sp"

occ_path <- "C:/Users/SoftwareAdmin/Desktop/MassScale_SDMs_GapAnalysis/GCCFP_inputs/occurrences/occ_prepped_all_species.fst"
eco_path <- "C:/Users/SoftwareAdmin/Desktop/MassScale_SDMs_GapAnalysis/GCCFP_inputs/ecoregions/tnc_terr_ecoregions.gpkg"
pa_path  <- "C:/Users/SoftwareAdmin/Desktop/MassScale_SDMs_GapAnalysis/GCCFP_inputs/protected_areas/wdpa_2.5m_all.tif"

test_species <- c(
  "Diospyros toposia",
  "Saribus jeanneneyi",
  "Hedlundia roopiana",
  "Copernicia hospita",
  "Fegimanra africana",
  "Cardiospermum microcarpum",
  "Rubus deliciosus",
  "Acalypha poiretii",
  "Ficus reflexa",
  "Agastache foeniculum"
)

# ── 3. DISCOVER COMPLETED SDMs ────────────────────────────────────────────────
completed_files <- list.files(sdm_root, pattern = "_sdm_binary_gap\\.tif$",
                              full.names = FALSE)
completed_species <- gsub("_sdm_binary_gap\\.tif$", "", completed_files) |>
  (\(x) gsub("_", " ", x))()

species_list <- intersect(completed_species, test_species)
n_completed  <- length(species_list)

message("══════════════════════════════════════════════════════")
message("  SDMs available for test species: ", n_completed, " / ",
        length(test_species))
message("  Folder: ", sdm_root)
message("══════════════════════════════════════════════════════")
if (n_completed == 0) stop("No completed SDMs found in: ", sdm_root)

# ── 4. OCCURRENCE DATA ────────────────────────────────────────────────────────
if (!file.exists(occ_path)) stop("Occurrence file not found: ", occ_path)

occData_all <- fst::read_fst(occ_path) |>
  dplyr::rename(species = taxon) |>
  dplyr::filter(species %in% species_list)

occData_gap <- occData_all |>
  dplyr::filter(type %in% c("G","H"))

species_with_occ <- unique(occData_gap$species)
species_no_occ   <- setdiff(species_list, species_with_occ)
if (length(species_no_occ) > 0) {
  message("WARNING: ", length(species_no_occ),
          " species have an SDM but no G/H occurrence records — skipping:")
  message(paste(" -", species_no_occ, collapse = "\n"))
}
species_list <- intersect(species_list, species_with_occ)

message("\nSpecies with SDM + G/H occurrences ready for gap analysis: ",
        length(species_list))
message("\nOccurrence summary (G/H only):")
print(occData_gap |> dplyr::count(species, type, sort = TRUE))

# ── 5. LOAD SDMs ──────────────────────────────────────────────────────────────
sdmList <- lapply(species_list, function(sp) {
  clean_name <- gsub(" ", "_", sp)
  terra::rast(file.path(sdm_root, paste0(clean_name, "_sdm_binary_gap.tif")))
})
names(sdmList) <- species_list

# ── 6. ECOREGIONS ─────────────────────────────────────────────────────────────
if (!file.exists(eco_path)) stop("Ecoregions file not found: ", eco_path)

ecos_sf <- sf::st_read(eco_path, quiet = TRUE) |>
  sf::st_make_valid() |>
  sf::st_zm(drop = TRUE, what = "ZM")

if ("ECO_ID_U" %in% names(ecos_sf)) {
  geom_col <- attr(ecos_sf, "sf_column")
  ecos_sf  <- ecos_sf[, c("ECO_ID_U", geom_col)]
}

nan_check <- sapply(sf::st_geometry(ecos_sf), function(g) {
  tryCatch(grepl("NaN", sf::st_as_text(g), fixed = TRUE), error = function(e) TRUE)
})
if (sum(nan_check) > 0) ecos_sf <- ecos_sf[!nan_check, ]

is_poly <- sf::st_geometry_type(ecos_sf) %in% c("POLYGON","MULTIPOLYGON")
if (sum(!is_poly) > 0) ecos_sf <- ecos_sf[is_poly, ]

target_crs <- "EPSG:4326"
ecos_sf    <- sf::st_transform(ecos_sf, target_crs)
ecos_clean <- terra::vect(ecos_sf)

# ── 7. PROTECTED AREAS ────────────────────────────────────────────────────────
if (!file.exists(pa_path))
  stop("Protected areas raster not found at:\n  ", pa_path)

proAreas <- terra::rast(pa_path)
if (terra::crs(proAreas, proj = TRUE) != terra::crs(target_crs))
  proAreas <- terra::project(proAreas, target_crs, method = "near")
proAreas <- terra::subst(proAreas, 0, NA)
proAreas[!(is.na(proAreas) | proAreas == 1)] <- NA

# ── 8. PREPARE SDMs ───────────────────────────────────────────────────────────
sdmList <- lapply(species_list, function(sp) {
  r <- sdmList[[sp]]
  if (terra::crs(r, proj = TRUE) != terra::crs(ecos_clean, proj = TRUE))
    r <- terra::project(r, terra::crs(ecos_clean), method = "near")
  r <- terra::subst(r, 0, NA)
  r[!(is.na(r) | r == 1)] <- NA
  r
})
names(sdmList) <- species_list

taxa <- species_list
message("\nFinal species count for gap analysis: ", length(taxa))

# ── 9. DIAGNOSTIC: print installed function signatures (for QA) ───────────────
message("\nGapAnalysis function signatures (this version):")
for (fn in c("SRSex","SRSin","GRSex","GRSin","ERSex","ERSin",
             "FCSex","FCSin","FCSc_mean",
             "checkOccurrences","checksdm","checkProtectedAreas",
             "generateGBuffers")) {
  if (exists(fn, where = "package:GapAnalysis", inherits = FALSE)) {
    message("  ", fn, ": ", paste(deparse(args(get(fn))), collapse = " "))
  }
}

# ── 10. HELPERS ───────────────────────────────────────────────────────────────
get_occurrence_totals <- function(taxon, occ_all) {
  x            <- occ_all[occ_all$species == taxon, , drop = FALSE]
  valid_coords <- is.finite(x$longitude) & is.finite(x$latitude)
  data.frame(
    Taxon                          = taxon,
    total_records_all              = nrow(x),
    total_with_coordinates_all     = sum(valid_coords, na.rm = TRUE),
    total_g_records_all            = sum(x$type == "G", na.rm = TRUE),
    g_records_with_coordinates_all = sum(x$type == "G" & valid_coords, na.rm = TRUE),
    total_h_records_all            = sum(x$type == "H", na.rm = TRUE),
    h_records_with_coordinates_all = sum(x$type == "H" & valid_coords, na.rm = TRUE),
    total_observations_all         = nrow(x)
  )
}

# ── 11. PATCHED ERSex / ERSin (use efficient zonal stats) ─────────────────────
ERSex <- function(taxon, sdm, occurrenceData, gBuffer, ecoregions, idColumn) {
  d1 <- occurrenceData |>
    dplyr::filter(occurrenceData$species == taxon) |>
    terra::vect(geom = c("longitude","latitude"),
                crs  = "+proj=longlat +datum=WGS84")
  d1$color <- ifelse(d1$type == "H", yes = "#1184d4", no = "#6300f0")
  
  ecoregions$id_column <- as.data.frame(ecoregions)[[idColumn]]
  ecos_cropped         <- terra::crop(ecoregions, sdm)
  
  ids  <- as.character(as.data.frame(ecos_cropped)[["id_column"]])
  uid  <- sort(unique(ids))
  ecos_cropped[["eco_int"]] <- match(ids, uid)
  eco_r <- terra::rasterize(ecos_cropped, sdm, field = "eco_int")
  
  sdm_z        <- terra::zonal(sdm, eco_r, fun = "sum", na.rm = TRUE)
  names(sdm_z) <- c("eco_int","sdmSum")
  sdm_z$id_column <- uid[as.integer(sdm_z$eco_int)]
  
  eco2           <- sdm_z[sdm_z$sdmSum > 0, ]
  totalEcosCount <- nrow(eco2)
  ecoSelect      <- ecos_cropped[ecos_cropped$id_column %in% eco2$id_column, ]
  
  if (is.character(gBuffer$data)) {
    ers <- 0; gEcoCounts <- 0; missingEcos <- ecoSelect
  } else {
    gb <- gBuffer$data
    if (terra::crs(gb, proj = TRUE) != terra::crs(sdm, proj = TRUE))
      gb <- terra::project(gb, terra::crs(sdm))
    
    b1    <- terra::rasterize(x = gb, y = sdm) |> terra::mask(sdm)
    buf_z <- terra::zonal(b1, eco_r, fun = "sum", na.rm = TRUE)
    names(buf_z)    <- c("eco_int","bufferSum")
    buf_z$id_column <- uid[as.integer(buf_z$eco_int)]
    
    ecoGrouped <- eco2 |>
      dplyr::left_join(buf_z[, c("id_column","bufferSum")], by = "id_column") |>
      dplyr::mutate(bufferSum = dplyr::case_when(
        is.na(bufferSum)  ~ 0,
        is.nan(bufferSum) ~ 0,
        TRUE              ~ bufferSum))
    
    gEcoIds    <- ecoGrouped$id_column[ecoGrouped$bufferSum > 0]
    gEcoCounts <- length(gEcoIds)
    missingEcos <- ecoSelect[!ecoSelect$id_column %in% gEcoIds, ]
    ers <- if (totalEcosCount == 0) 0 else min(100, (gEcoCounts / totalEcosCount) * 100)
  }
  
  out_df <- dplyr::tibble(
    Taxon                        = taxon,
    `Ecoregions with records`    = totalEcosCount,
    `Ecoregions within G buffer` = gEcoCounts,
    `ERS exsitu`                 = ers
  )
  list(results = out_df, ecoGaps = missingEcos, map = NULL)
}

ERSin <- function(taxon, sdm, occurrenceData, protectedAreas, ecoregions, idColumn) {
  pro     <- terra::crop(protectedAreas, sdm)
  pro     <- terra::resample(pro, sdm, method = "near")
  proMask <- pro * sdm
  
  ecoregions$id_column <- as.data.frame(ecoregions)[, idColumn]
  ecos_cropped         <- terra::crop(ecoregions, sdm)
  
  ids  <- as.character(as.data.frame(ecos_cropped)[["id_column"]])
  uid  <- sort(unique(ids))
  ecos_cropped[["eco_int"]] <- match(ids, uid)
  eco_r <- terra::rasterize(ecos_cropped, sdm, field = "eco_int")
  
  sdm_z        <- terra::zonal(sdm, eco_r, fun = "sum", na.rm = TRUE)
  names(sdm_z) <- c("eco_int","totEco")
  sdm_z$id_column <- uid[as.integer(sdm_z$eco_int)]
  
  selectedIds  <- sdm_z$id_column[sdm_z$totEco > 0]
  nEcoModel    <- length(selectedIds)
  selectedEcos <- ecos_cropped[ecos_cropped$id_column %in% selectedIds, ]
  
  pro_z        <- terra::zonal(proMask, eco_r, fun = "sum", na.rm = TRUE)
  names(pro_z) <- c("eco_int","totPro")
  pro_z$id_column <- uid[as.integer(pro_z$eco_int)]
  
  protectedIds <- pro_z$id_column[pro_z$totPro > 0]
  nProModel    <- length(protectedIds)
  missingEcos  <- selectedEcos[!selectedEcos$id_column %in% protectedIds, ]
  
  ers <- if (nEcoModel == 0 || nProModel == 0) 0 else (nProModel / nEcoModel) * 100
  
  df <- dplyr::tibble(
    Taxon                             = taxon,
    "Ecoregions within model"         = nEcoModel,
    "Ecoregions with protected areas" = nProModel,
    "ERS insitu"                      = ers
  )
  list(results = df, missingEcos = missingEcos, map = NULL)
}

message("✓ ERSex and ERSin patched")

# ── 12. RUN GAP ANALYSIS ──────────────────────────────────────────────────────
results_list <- list()
failed_list  <- list()

for (taxon in taxa) {
  occurrenceData <- occData_gap[occData_gap$species == taxon, ]
  sdm            <- sdmList[[taxon]]
  
  message("\n── Processing: ", taxon,
          " (", which(taxa == taxon), "/", length(taxa), ") ──────────────────")
  message("  Occurrence rows (G/H): ", nrow(occurrenceData))
  
  if (all(is.na(terra::values(sdm)))) {
    message("  SKIP: SDM raster has no values.")
    failed_list[[taxon]] <- "SDM raster all NA"
    next
  }
  
  skip   <- FALSE
  reason <- NA_character_
  
  # SRSex — uses arg `occurrence_Data` (underscore + capital D)
  srsex <- tryCatch(
    SRSex(taxon = taxon, occurrence_Data = occurrenceData),
    error = function(e) { skip <<- TRUE; reason <<- paste0("SRSex: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  occurrences <- tryCatch(
    checkOccurrences(csv = occurrenceData, taxon = taxon),
    error = function(e) { skip <<- TRUE; reason <<- paste0("checkOccurrences: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  sdm_checked <- tryCatch(
    checksdm(sdm),
    error = function(e) { skip <<- TRUE; reason <<- paste0("checksdm: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  # checkProtectedAreas — uses arg `protectedAreas` (with the s)
  proArea_cropped <- terra::crop(proAreas, terra::ext(sdm_checked))
  proArea_checked <- tryCatch(
    checkProtectedAreas(protectedAreas = proArea_cropped, sdm = sdm_checked),
    error = function(e) { skip <<- TRUE; reason <<- paste0("checkProtectedAreas: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  if (terra::crs(proArea_checked, proj = TRUE) != terra::crs(sdm_checked, proj = TRUE))
    proArea_checked <- terra::project(proArea_checked, sdm_checked, method = "near")
  proArea_checked <- terra::resample(proArea_checked, sdm_checked, method = "near")
  
  gBuffer <- tryCatch(
    generateGBuffers(taxon = taxon, occurrenceData = occurrences$data,
                     bufferDistM = 50000),
    error = function(e) { skip <<- TRUE; reason <<- paste0("generateGBuffers: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  grsex <- tryCatch(
    GRSex(taxon = taxon, sdm = sdm_checked, gBuffer = gBuffer),
    error = function(e) { skip <<- TRUE; reason <<- paste0("GRSex: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  ersex <- tryCatch(
    ERSex(taxon = taxon, sdm = sdm_checked, occurrenceData = occurrences$data,
          gBuffer = gBuffer, ecoregions = ecos_clean, idColumn = "ECO_ID_U"),
    error = function(e) { skip <<- TRUE; reason <<- paste0("ERSex: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  fcsex <- tryCatch(
    FCSex(taxon = taxon, srsex = srsex, grsex = grsex, ersex = ersex),
    error = function(e) { skip <<- TRUE; reason <<- paste0("FCSex: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  srsin <- tryCatch(
    SRSin(taxon = taxon, sdm = sdm_checked, occurrenceData = occurrences$data,
          protectedAreas = proArea_checked),
    error = function(e) { skip <<- TRUE; reason <<- paste0("SRSin: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  grsin <- tryCatch(
    GRSin(taxon = taxon, sdm = sdm_checked, protectedAreas = proArea_checked),
    error = function(e) { skip <<- TRUE; reason <<- paste0("GRSin: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  ersin <- tryCatch(
    ERSin(taxon = taxon, sdm = sdm_checked, occurrenceData = occurrences$data,
          protectedAreas = proArea_checked, ecoregions = ecos_clean,
          idColumn = "ECO_ID_U"),
    error = function(e) { skip <<- TRUE; reason <<- paste0("ERSin: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  fcsin <- tryCatch(
    FCSin(taxon = taxon, srsin = srsin, grsin = grsin, ersin = ersin),
    error = function(e) { skip <<- TRUE; reason <<- paste0("FCSin: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  fcsmean <- tryCatch(
    FCSc_mean(taxon = taxon, fcsin = fcsin, fcsex = fcsex),
    error = function(e) { skip <<- TRUE; reason <<- paste0("FCSc_mean: ", e$message); NULL }
  )
  if (skip) { failed_list[[taxon]] <- reason; next }
  
  message("  ✓ Done: ", taxon)
  
  results_list[[taxon]] <- list(
    srsex = srsex, occurrences = occurrences, sdm_checked = sdm_checked,
    proArea_checked = proArea_checked, eco_checked = ecos_clean,
    grsex = grsex, ersex = ersex, fcsex = fcsex,
    srsin = srsin, grsin = grsin, ersin = ersin,
    fcsin = fcsin, fcsmean = fcsmean
  )
}

message("\n══════════════════════════════════════════════════════")
message("  Gap analysis complete")
message("  Succeeded: ", length(results_list))
message("  Failed:    ", length(failed_list))
message("══════════════════════════════════════════════════════")

if (length(failed_list) > 0) {
  message("\nFailure reasons:")
  for (sp in names(failed_list))
    message("  - ", sp, ": ", failed_list[[sp]])
}

# ── 13. RESULTS TABLES (printed to console) ───────────────────────────────────
run_summary <- data.frame(
  metric = c(
    "Test species requested",
    "SDM files found in folder",
    "Species with G/H occurrence records",
    "Species skipped (no G/H records)",
    "Species attempted in gap analysis",
    "Species succeeded",
    "Species failed"
  ),
  count = c(length(test_species), n_completed, length(species_with_occ),
            length(species_no_occ), length(taxa),
            length(results_list), length(failed_list))
)
message("\nRun summary:")
print(run_summary, row.names = FALSE)

if (length(results_list) > 0) {
  
  exsitu_table <- do.call(rbind, lapply(names(results_list), function(taxon) {
    r <- results_list[[taxon]]
    data.frame(
      Species          = taxon,
      SRS_exsitu       = r$srsex$`SRS exsitu`,
      GRS_exsitu       = r$fcsex$`GRS exsitu`,
      ERS_exsitu       = r$fcsex$`ERS exsitu`,
      FCS_exsitu       = r$fcsex$`FCS exsitu`,
      FCS_exsitu_score = r$fcsex$`FCS existu score`
    )
  }))
  message("\nEx-situ metrics:");   print(exsitu_table, row.names = FALSE)
  
  insitu_table <- do.call(rbind, lapply(names(results_list), function(taxon) {
    r <- results_list[[taxon]]
    data.frame(
      Species          = taxon,
      SRS_insitu       = r$fcsin$`SRS insitu`,
      GRS_insitu       = r$fcsin$`GRS insitu`,
      ERS_insitu       = r$fcsin$`ERS insitu`,
      FCS_insitu       = r$fcsin$`FCS insitu`,
      FCS_insitu_score = r$fcsin$`FCS insitu score`
    )
  }))
  message("\nIn-situ metrics:");   print(insitu_table, row.names = FALSE)
  
  combined_table <- do.call(rbind, lapply(names(results_list), function(taxon) {
    r <- results_list[[taxon]]
    data.frame(
      Species         = taxon,
      FCSc_min        = r$fcsmean$FCSc_min,
      FCSc_max        = r$fcsmean$FCSc_max,
      FCSc_mean       = r$fcsmean$FCSc_mean,
      FCSc_min_class  = r$fcsmean$FCSc_min_class,
      FCSc_max_class  = r$fcsmean$FCSc_max_class,
      FCSc_mean_class = r$fcsmean$FCSc_mean_class
    )
  }))
  message("\nCombined FCS metrics:"); print(combined_table, row.names = FALSE)
  
  occurrence_totals_table <- do.call(rbind, lapply(names(results_list), function(taxon) {
    get_occurrence_totals(taxon, occData_all)
  }))
  message("\nOccurrence totals:"); print(occurrence_totals_table, row.names = FALSE)
}

# ── 14. SAVE METRICS TO EXCEL ─────────────────────────────────────────────────
if (length(results_list) > 0) {
  
  all_metrics <- do.call(rbind, lapply(names(results_list), function(taxon) {
    r <- results_list[[taxon]]
    clean <- function(x) {
      df <- as.data.frame(t(unlist(x)))
      df <- df[, !grepl("(?i)^(taxon|species)$", names(df)), drop = FALSE]
      df
    }
    occ_totals <- get_occurrence_totals(taxon, occData_all)
    row <- cbind(
      data.frame(Taxon = taxon),
      clean(r$srsex),
      clean(r$grsex$results),
      clean(r$ersex$results),
      clean(r$fcsex),
      clean(r$srsin$results),
      clean(r$grsin$results),
      clean(r$ersin$results),
      clean(r$fcsin),
      clean(r$fcsmean)
    )
    row <- row[, !duplicated(names(row)), drop = FALSE]
    
    if ("Total records" %in% names(row))
      row[["Total records"]] <- occ_totals$total_records_all
    if ("Total with cooordinates" %in% names(row))
      row[["Total with cooordinates"]] <- occ_totals$total_with_coordinates_all
    if ("Total G records" %in% names(row))
      row[["Total G records"]] <- occ_totals$total_g_records_all
    if ("G records with coordinates" %in% names(row))
      row[["G records with coordinates"]] <- occ_totals$g_records_with_coordinates_all
    if ("Total H records" %in% names(row))
      row[["Total H records"]] <- occ_totals$total_h_records_all
    if ("H records with coordinates" %in% names(row))
      row[["H records with coordinates"]] <- occ_totals$h_records_with_coordinates_all
    if ("Total Observations" %in% names(row))
      row[["Total Observations"]] <- occ_totals$total_observations_all
    row
  }))
  
  for (col in names(all_metrics)) {
    num <- suppressWarnings(as.numeric(as.character(all_metrics[[col]])))
    if (all(is.na(num) == is.na(all_metrics[[col]]))) {
      all_metrics[[col]] <- round(num, 2)
    }
  }
  
  run_summary_out <- data.frame(Metric = run_summary$metric,
                                Count  = run_summary$count)
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "gapAnalysis_results")
  openxlsx::addWorksheet(wb, "run_summary")
  openxlsx::writeData(wb, "gapAnalysis_results", all_metrics,     startRow = 1, startCol = 1)
  openxlsx::writeData(wb, "run_summary",         run_summary_out, startRow = 1, startCol = 1)
  
  out_file <- file.path(outputs_folder,
                        paste0("gapAnalysis_results_test10_hybrid_",
                               format(Sys.Date(), "%Y%m%d"),
                               "_n", length(results_list), "species.xlsx"))
  openxlsx::saveWorkbook(wb, file = out_file, overwrite = TRUE)
  message("\nSaved metrics: ", out_file)
}

if (length(failed_list) > 0) {
  failed_out <- file.path(outputs_folder,
                          paste0("gapAnalysis_failed_test10_hybrid_",
                                 format(Sys.Date(), "%Y%m%d"), ".csv"))
  readr::write_csv(
    data.frame(species = names(failed_list), reason = unlist(failed_list)),
    failed_out)
  message("Saved failure log: ", failed_out)
}

message(
  "\n──────────────────────────────────────────────────────────────────────\n",
  "  GAP ANALYSIS WORKFLOW COMPLETE — 10-SPECIES HYBRID TEST\n",
  "  SDMs found:   ", n_completed, "\n",
  "  Succeeded:    ", length(results_list), "\n",
  "  Failed:       ", length(failed_list), "\n",
  "──────────────────────────────────────────────────────────────────────\n"
)

