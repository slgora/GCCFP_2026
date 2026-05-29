# =====================================================================
# CONSERVATION THREAT ASSESSMENT WORKFLOW (EOO/AOO-based)
# =====================================================================
# This script performs a conservation threat assessment for all species in your
# occurrences dataset using geographic range metrics:
#   - Extent of Occurrence (EOO; km²)
#   - Area of Occupancy (AOO; km²)
#
# It then assigns preliminary conservation categories using IUCN Criterion B
# (geographic range) thresholds based on EOO and AOO, and combines them into an
# overall conservation category per species.
#
# Outputs (written to out_dir):
#   - eoo_aoo_chunk_####.csv (one per chunk)
#   - eoo_aoo_all_species_combined.csv
#   - metrics_summary_table.xlsx
#   - overall_conservation_category_barplot.png
#   - eoo_vs_aoo_scatter.png
#   - wcfp_plantlist_with_EOO_AOO_v2.xlsx
#
# install.packages(c("readr","dplyr","sf","progress","openxlsx","readxl"))
# =====================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(sf)
  library(progress)
  library(openxlsx)
  library(readxl)
})

# ---------------------------------
# DATA READ In and set output path
# --------------------------------
infile <- "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Final_datasets/occurrences_dataset_2026-05-28.csv"

out_dir <- "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Final_metrics/EOO_AOO"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

species_col <- "wcfp_name_match"
lat_col <- "latitude"
lon_col <- "longitude"

chunk_size_species <- 1000

# AOO cell size (IUCN default = 2x2 km grid)
cell_size_m <- 2000
cell_area_km2 <- (cell_size_m * cell_size_m) / 1e6  # 4 km²

# CRS
crs_ll <- 4326
crs_m  <- 6933

# Minimum distinct points
min_points_eoo <- 3
min_points_aoo <- 1

# IUCN Criterion B thresholds
EOO_CR <- 100
EOO_EN <- 5000
EOO_VU <- 20000

AOO_CR <- 10
AOO_EN <- 500
AOO_VU <- 2000

# -----------------------------
# IUCN codes
# -----------------------------
cols <- c(
  "EX" = "#000000",
  "EW" = "#4D4D4D",
  "CR" = "darkred",
  "EN" = "orange",
  "VU" = "gold",
  "NT" = "#7FBF7B",
  "LC" = "green",
  "DD" = "#377EB8",
  "NE" = "#984EA3"
)

cat_labels <- c(
  "EX" = "Extinct (EX)",
  "EW" = "Extinct in the Wild (EW)",
  "CR" = "Critically Endangered (CR)",
  "EN" = "Endangered (EN)",
  "VU" = "Vulnerable (VU)",
  "NT" = "Near Threatened (NT)",
  "LC" = "Least Concern (LC)",
  "DD" = "Data Deficient (DD)",
  "NE" = "Not Evaluated (NE)"
)

cat_order_codes <- c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "NE")

# For selecting "most threatened" among assessed categories
severity_order <- c("LC", "VU", "EN", "CR")

# -----------------------------
# 1) READ + CLEAN
# -----------------------------
to_numeric_clean <- function(x) {
  x <- trimws(gsub(",", "", as.character(x)))
  suppressWarnings(as.numeric(x))
}

occ_raw <- read_csv(infile, show_col_types = FALSE) %>%
  transmute(
    tax = .data[[species_col]],
    lat = to_numeric_clean(.data[[lat_col]]),
    lon = to_numeric_clean(.data[[lon_col]])
  )

occ <- occ_raw %>%
  filter(
    !is.na(tax), tax != "",
    !is.na(lat), !is.na(lon),
    is.finite(lat), is.finite(lon),
    lat >= -90, lat <= 90,
    lon >= -180, lon <= 180
  ) %>%
  distinct(tax, lat, lon, .keep_all = TRUE)

cat("Raw rows:", nrow(occ_raw), "\n")
cat("Cleaned rows (valid + de-dup):", nrow(occ), "\n")
cat("Total species:", n_distinct(occ$tax), "\n\n")

all_species <- sort(unique(occ$tax))
n_species <- length(all_species)
n_chunks <- ceiling(n_species / chunk_size_species)

cat("Chunk size (species):", chunk_size_species, "\n")
cat("Total chunks:", n_chunks, "\n\n")

# -----------------------------
# 2) METRICS FUNCTION
# -----------------------------
calc_metrics_one_taxon <- function(sf_taxon_m, cell_size_m, cell_area_km2,
                                   min_points_eoo = 3, min_points_aoo = 1) {
  coords <- st_coordinates(sf_taxon_m)
  
  keep <- !duplicated(paste(coords[, "X"], coords[, "Y"], sep = "_"))
  coords <- coords[keep, , drop = FALSE]
  sf_taxon_m <- sf_taxon_m[keep, , drop = FALSE]
  
  n <- nrow(sf_taxon_m)
  
  eoo_km2 <- NA_real_
  if (n >= min_points_eoo) {
    hull <- st_convex_hull(st_union(sf_taxon_m))
    eoo_km2 <- as.numeric(st_area(hull)) / 1e6
  }
  
  aoo_km2 <- NA_real_
  if (n >= min_points_aoo) {
    ix <- floor(coords[, "X"] / cell_size_m)
    iy <- floor(coords[, "Y"] / cell_size_m)
    n_cells <- dplyr::n_distinct(paste(ix, iy, sep = "_"))
    aoo_km2 <- n_cells * cell_area_km2
  }
  
  data.frame(
    number_of_unique_occurrence_points = n,
    EOO_km2 = eoo_km2,
    AOO_km2 = aoo_km2
  )
}

# -----------------------------
# 3) Category functions (IUCN Criterion B-based threat assessment)
# -----------------------------
category_from_eoo <- function(eoo_km2) {
  if (is.na(eoo_km2) || !is.finite(eoo_km2)) return("DD")
  if (eoo_km2 < EOO_CR) return("CR")
  if (eoo_km2 < EOO_EN) return("EN")
  if (eoo_km2 < EOO_VU) return("VU")
  "LC"
}

category_from_aoo <- function(aoo_km2) {
  if (is.na(aoo_km2) || !is.finite(aoo_km2)) return("DD")
  if (aoo_km2 < AOO_CR) return("CR")
  if (aoo_km2 < AOO_EN) return("EN")
  if (aoo_km2 < AOO_VU) return("VU")
  "LC"
}

combine_overall <- function(cat_eoo, cat_aoo) {
  if (cat_eoo == "DD" && cat_aoo != "DD") return(cat_aoo)
  if (cat_aoo == "DD" && cat_eoo != "DD") return(cat_eoo)
  if (cat_eoo == "DD" && cat_aoo == "DD") return("DD")
  
  eoo_level <- match(cat_eoo, severity_order)
  aoo_level <- match(cat_aoo, severity_order)
  severity_order[max(eoo_level, aoo_level, na.rm = TRUE)]
}

# -----------------------------
# 4) CHUNK LOOP (NO PLOTTING HERE)
# -----------------------------
chunk_files <- character(0)

pb <- progress_bar$new(
  format = "Chunks [:bar] :current/:total (:percent) | elapsed: :elapsed | eta: :eta",
  total = n_chunks,
  clear = FALSE,
  width = 80
)

t0 <- Sys.time()

for (i in seq_len(n_chunks)) {
  sp_start <- (i - 1) * chunk_size_species + 1
  sp_end <- min(i * chunk_size_species, n_species)
  sp_chunk <- all_species[sp_start:sp_end]
  
  occ_chunk <- occ %>% filter(tax %in% sp_chunk)
  
  pts_ll <- st_as_sf(occ_chunk, coords = c("lon", "lat"), crs = crs_ll, remove = FALSE)
  pts_m  <- st_transform(pts_ll, crs_m)
  
  res_chunk <- pts_m %>%
    group_by(tax) %>%
    group_modify(~ calc_metrics_one_taxon(.x, cell_size_m, cell_area_km2,
                                          min_points_eoo = min_points_eoo,
                                          min_points_aoo = min_points_aoo)) %>%
    ungroup() %>%
    rename(Species = tax) %>%
    mutate(
      EOO_km2 = round(EOO_km2, 1),
      AOO_km2 = round(AOO_km2, 2)
    )
  
  res_chunk$Category_EOO <- vapply(res_chunk$EOO_km2, category_from_eoo, character(1))
  res_chunk$Category_AOO <- vapply(res_chunk$AOO_km2, category_from_aoo, character(1))
  res_chunk$Overall_conservation_category <- mapply(combine_overall, res_chunk$Category_EOO, res_chunk$Category_AOO)
  
  res_chunk <- res_chunk[, c(
    "Species",
    "number_of_unique_occurrence_points",
    "EOO_km2",
    "AOO_km2",
    "Category_EOO",
    "Category_AOO",
    "Overall_conservation_category"
  )]
  
  chunk_path <- file.path(out_dir, sprintf("eoo_aoo_chunk_%04d.csv", i))
  write.csv(res_chunk, chunk_path, row.names = FALSE)
  chunk_files <- c(chunk_files, chunk_path)
  
  pb$message(sprintf(
    "Chunk %d/%d | species %d-%d of %d | rows: %d | saved: %s",
    i, n_chunks, sp_start, sp_end, n_species, nrow(occ_chunk), basename(chunk_path)
  ))
  pb$tick()
  
  rm(occ_chunk, pts_ll, pts_m, res_chunk)
  gc()
}

cat("\nAll chunks complete. Total elapsed:", format(Sys.time() - t0), "\n\n")

# -----------------------------
# 5) COMBINE CHUNKS
# -----------------------------
cat("Combining", length(chunk_files), "chunk files...\n")

all_results <- bind_rows(lapply(chunk_files, read_csv, show_col_types = FALSE)) %>%
  distinct(Species, .keep_all = TRUE) %>%
  arrange(Species)

combined_path <- file.path(out_dir, "eoo_aoo_all_species_combined.csv")
write.csv(all_results, combined_path, row.names = FALSE)

cat("Saved combined results:", combined_path, "\n")
cat("Total species in combined results:", nrow(all_results), "\n\n")

all_results <- all_results %>%
  mutate(
    EOO_km2 = as.numeric(EOO_km2),
    AOO_km2 = as.numeric(AOO_km2),
    number_of_unique_occurrence_points = as.numeric(number_of_unique_occurrence_points)
  )

N_total <- nrow(all_results)

# -----------------------------
# 6) EXCEL SUMMARY TABLE (metrics)
# -----------------------------
summary_by_cat <- all_results %>%
  group_by(Overall_conservation_category) %>%
  summarise(
    N_Species = n(),
    Percent = round(100 * N_Species / N_total, 2),
    
    OccPoints_Min = suppressWarnings(min(number_of_unique_occurrence_points, na.rm = TRUE)),
    OccPoints_Median = suppressWarnings(median(number_of_unique_occurrence_points, na.rm = TRUE)),
    OccPoints_Mean = round(suppressWarnings(mean(number_of_unique_occurrence_points, na.rm = TRUE)), 2),
    OccPoints_Max = suppressWarnings(max(number_of_unique_occurrence_points, na.rm = TRUE)),
    
    EOO_Min_km2 = round(suppressWarnings(min(EOO_km2, na.rm = TRUE)), 1),
    EOO_Median_km2 = round(suppressWarnings(median(EOO_km2, na.rm = TRUE)), 1),
    EOO_Mean_km2 = round(suppressWarnings(mean(EOO_km2, na.rm = TRUE)), 1),
    EOO_Max_km2 = round(suppressWarnings(max(EOO_km2, na.rm = TRUE)), 1),
    EOO_NA = sum(is.na(EOO_km2)),
    
    AOO_Min_km2 = round(suppressWarnings(min(AOO_km2, na.rm = TRUE)), 2),
    AOO_Median_km2 = round(suppressWarnings(median(AOO_km2, na.rm = TRUE)), 2),
    AOO_Mean_km2 = round(suppressWarnings(mean(AOO_km2, na.rm = TRUE)), 2),
    AOO_Max_km2 = round(suppressWarnings(max(AOO_km2, na.rm = TRUE)), 2),
    AOO_NA = sum(is.na(AOO_km2)),
    
    .groups = "drop"
  )

summary_by_cat <- summary_by_cat %>%
  mutate(Overall_conservation_category = factor(Overall_conservation_category, levels = cat_order_codes)) %>%
  arrange(Overall_conservation_category) %>%
  mutate(Overall_conservation_category = as.character(Overall_conservation_category))

summary_total <- all_results %>%
  summarise(
    Overall_conservation_category = "TOTAL",
    N_Species = n(),
    Percent = 100,
    
    OccPoints_Min = suppressWarnings(min(number_of_unique_occurrence_points, na.rm = TRUE)),
    OccPoints_Median = suppressWarnings(median(number_of_unique_occurrence_points, na.rm = TRUE)),
    OccPoints_Mean = round(suppressWarnings(mean(number_of_unique_occurrence_points, na.rm = TRUE)), 2),
    OccPoints_Max = suppressWarnings(max(number_of_unique_occurrence_points, na.rm = TRUE)),
    
    EOO_Min_km2 = round(suppressWarnings(min(EOO_km2, na.rm = TRUE)), 1),
    EOO_Median_km2 = round(suppressWarnings(median(EOO_km2, na.rm = TRUE)), 1),
    EOO_Mean_km2 = round(suppressWarnings(mean(EOO_km2, na.rm = TRUE)), 1),
    EOO_Max_km2 = round(suppressWarnings(max(EOO_km2, na.rm = TRUE)), 1),
    EOO_NA = sum(is.na(EOO_km2)),
    
    AOO_Min_km2 = round(suppressWarnings(min(AOO_km2, na.rm = TRUE)), 2),
    AOO_Median_km2 = round(suppressWarnings(median(AOO_km2, na.rm = TRUE)), 2),
    AOO_Mean_km2 = round(suppressWarnings(mean(AOO_km2, na.rm = TRUE)), 2),
    AOO_Max_km2 = round(suppressWarnings(max(AOO_km2, na.rm = TRUE)), 2),
    AOO_NA = sum(is.na(AOO_km2))
  )

metrics_summary <- bind_rows(summary_by_cat, summary_total)

wb <- createWorkbook()
addWorksheet(wb, "Summary")
writeData(wb, "Summary", metrics_summary)
freezePane(wb, "Summary", firstRow = TRUE)
addFilter(wb, "Summary", row = 1, cols = 1:ncol(metrics_summary))
setColWidths(wb, "Summary", cols = 1:ncol(metrics_summary), widths = "auto")

summary_xlsx_path <- file.path(out_dir, "metrics_summary_table.xlsx")
saveWorkbook(wb, summary_xlsx_path, overwrite = TRUE)

cat("Saved summary table (Excel):", summary_xlsx_path, "\n\n")

# -----------------------------
# 7) FIGURES
# -----------------------------
safe_log10 <- function(x) ifelse(is.finite(x) & !is.na(x) & x > 0, log10(x), NA_real_)

# ---- Barplot: Overall category counts (full labels)
category_counts <- table(factor(all_results$Overall_conservation_category, levels = cat_order_codes))
category_counts <- category_counts[category_counts > 0]

bar_cols  <- cols[names(category_counts)]
bar_names <- unname(cat_labels[names(category_counts)])

ymax <- max(category_counts)
ylim_top <- ymax * 1.15

png(filename = file.path(out_dir, "overall_conservation_category_barplot.png"),
    width = 1600, height = 1000, res = 150)

par(mar = c(14, 8, 6, 2), xpd = NA)

bar_mids <- barplot(category_counts,
                    col = bar_cols,
                    border = NA,
                    ylim = c(0, ylim_top),
                    names.arg = bar_names,
                    las = 2,
                    cex.names = 0.95,
                    main = sprintf("Overall conservation category (all species; N = %s)",
                                   format(N_total, big.mark = ",")),
                    ylab = "")

mtext("Number of species", side = 2, line = 5.5, cex = 1.0)

text(x = bar_mids,
     y = category_counts,
     labels = format(category_counts, big.mark = ","),
     pos = 3,
     offset = 0.8,
     cex = 0.95)

dev.off()

# ---- Scatter: EOO vs AOO
png(filename = file.path(out_dir, "eoo_vs_aoo_scatter.png"),
    width = 1400, height = 900, res = 150)

par(mar = c(6, 6, 6, 2))
plot(safe_log10(all_results$EOO_km2), safe_log10(all_results$AOO_km2),
     pch = 19,
     col = cols[as.character(all_results$Overall_conservation_category)],
     xlab = "log10(Extent of Occurrence (EOO), km²)",
     ylab = "log10(Area of Occupancy (AOO), km²)",
     main = sprintf("Extent of Occurrence (EOO) vs Area of Occupancy (AOO)\n(all species; N = %s)",
                    format(N_total, big.mark = ",")))

present_cats <- names(which(table(all_results$Overall_conservation_category) > 0))
present_cats <- present_cats[present_cats %in% names(cols)]

legend("bottomright",
       legend = unname(cat_labels[present_cats]),
       col = cols[present_cats],
       pch = 19, cex = 0.9, bg = "white")

dev.off()

cat("Saved figures:\n")
cat(" -", file.path(out_dir, "overall_conservation_category_barplot.png"), "\n")
cat(" -", file.path(out_dir, "eoo_vs_aoo_scatter.png"), "\n")

# =====================================================================
# SEPARATE STEP: Merge metrics into plant list and save v2
# =====================================================================
plant_in <- "C:/Users/sarah/Downloads/wcfp_plantlist_2026_05_27.xlsx"
metrics_in <- combined_path
plant_out <- "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Final_metrics/EOO_AOO/wcfp_plantlist_with_EOO_AOO.xlsx"

wcfp_plantlist <- read_excel(plant_in)
metrics <- read.csv(metrics_in, stringsAsFactors = FALSE)

metrics2 <- metrics %>%
  rename(wcfp_name_match = Species) %>%
  distinct(wcfp_name_match, .keep_all = TRUE)

wcfp_plantlist_v2 <- wcfp_plantlist %>%
  left_join(metrics2, by = "wcfp_name_match")

wb2 <- createWorkbook()
addWorksheet(wb2, "wcfp_plantlist_with_metrics")
writeData(wb2, "wcfp_plantlist_with_metrics", wcfp_plantlist_v2)
freezePane(wb2, "wcfp_plantlist_with_metrics", firstRow = TRUE)
addFilter(wb2, "wcfp_plantlist_with_metrics", row = 1, cols = 1:ncol(wcfp_plantlist_v2))
setColWidths(wb2, "wcfp_plantlist_with_metrics", cols = 1:ncol(wcfp_plantlist_v2), widths = "auto")
saveWorkbook(wb2, plant_out, overwrite = TRUE)

cat("\nSaved plant list with metrics:", plant_out, "\n")
cat("Rows in plant list:", nrow(wcfp_plantlist_v2), "\n")
cat("Rows with matched EOO:", sum(!is.na(wcfp_plantlist_v2$EOO_km2)), "\n")
cat("Rows with matched AOO:", sum(!is.na(wcfp_plantlist_v2$AOO_km2)), "\n")

### end script ###
