# =============================================================================
# WCFP Complementarity Project: COMPLETE GBIF OCCURRENCE DATA WORKFLOW
# =============================================================================
#
# This script performs a full, reproducible workflow for handling GBIF plant
# occurrence data for the WCFP project. It covers the following steps:
#
#   1. Downloads and/or loads old and new WCFP plantlists.
#   2. Extracts GBIF occurrence data for each species in chunked batches,
#      supporting very large queries and memory management.
#   3. Merges and combines all chunked occurrence records for each plantlist.
#   4. Cleans and standardizes the merged GBIF data via:
#         - Removing fossil specimen records
#         - Joining to plantlists for standardized taxon names
#         - Robust NA and encoding fixes
#         - Addition of genus/species columns and other harmonizations
#   5. Produces a final, combined (old + new) WCFP GBIF standardized dataset
#   6. Optionally: Allows fast reload and combination of pre-exported "Part 1" and
#      "Part 2" full datasets for rapid downstream use with only light cleaning.
#
# Outputs:
#   - Fully standardized, joined, and harmonized GBIF occurrence CSV:
#             (old + new) -> combined_processed
#   - Raw, quickly reloadable, lightly cleaned version via rbind of
#     pre-generated Part 1 and Part 2 full datasets
#
# Usage:
#   - Uncomment section 7 steps as needed for initial extraction/merging
#   - Section 8 can always be run for fast appending of already-exported Part 1/2
#   - Adjust file paths as needed for your computing environment
#
# No emojis used. Script is modular and annotated throughout.
#
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Package Setup
# -----------------------------------------------------------------------------
required_pkgs <- c(
  "pbapply", "readxl", "dplyr", "httr", "jsonlite", "tidyverse",
  "purrr", "data.table", "readr", "readxl", "stringr", "vroom", "DBI", "duckdb", "arrow"
)
to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(required_pkgs, require, character.only = TRUE))

# -----------------------------------------------------------------------------
# 2. Source Data Extraction Function
# -----------------------------------------------------------------------------
source("~/GCCFP/Functions/get_GBIF_data.R")

# -----------------------------------------------------------------------------
# 3. Define File and Directory Paths
# -----------------------------------------------------------------------------
old_species_path <- "G:/.shortcut-targets-by-id/1kQBvFIumhKQHnxSXNhr7w70dvuHgheEO/GCCFP/Data and analyses/Processed_data/WCFP_plantlist_processed_2025-08-07.xlsx"
new_species_path <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/WCFP_plantlist_new/WCFP_plantlist_new_species_standardized_2026-01-06.xlsx"

gbif_chunk_dir_old  <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/WCFP_plantlist_datasets/Data/gbif_chunks_old_species"
gbif_chunk_dir_new  <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/WCFP_plantlist_datasets/Data/gbif_chunks_new_species"

gbif_merged_csv_old <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/WCFP_plantlist_datasets/Data/gbif_chunks_old_species/gbif_old_species_combined_clean.csv"
gbif_merged_csv_new <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/WCFP_plantlist_datasets/Data/gbif_chunks_new_species/gbif_new_species_combined_clean.csv"

old_processed_out   <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/WCFP_plantlist_datasets/Data/WCFP_old_species_GBIF_data_all.csv"
new_processed_out   <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/WCFP_plantlist_datasets/Data/WCFP_new_species_GBIF_data_all.csv"
combined_processed  <- "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/WCFP_plantlist_datasets/Data/WCFP_combined_GBIF_data_2026-01-06.csv"

# For reload/quick combine of old Part 1 and Part 2 datasets
part1_path <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Raw_data/GBIF_wcfp_p1.csv"
part2_path <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Raw_data/GBIF_wcfp_p2.xlsx"
combined_gbif_simple_path <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/GBIF_data_all.csv"

# -----------------------------------------------------------------------------
# 4. Function: Extract and Save Chunked GBIF Query Results
# -----------------------------------------------------------------------------
extract_gbif_occurrences_by_chunks <- function(species_list, chunk_dir, chunk_size = 500) {
  dir.create(chunk_dir, showWarnings = FALSE, recursive = TRUE)
  chunks <- split(species_list, ceiling(seq_along(species_list)/chunk_size))
  failed_taxa <- list()
  for (i in seq_along(chunks)) {
    message(sprintf("Processing chunk %d of %d...", i, length(chunks)))
    chunk <- chunks[[i]]
    chunk_results <- pbapply::pblapply(chunk, function(name) {
      result <- get_gbif_data(name)
      if (is.null(result)) failed_taxa <<- c(failed_taxa, name)
      result
    })
    saveRDS(chunk_results, file = file.path(chunk_dir, sprintf("gbif_chunk_%03d.rds", i)))
    chunk_clean <- chunk_results[!sapply(chunk_results, is.null)]
    if (length(chunk_clean) > 0) {
      chunk_df <- bind_rows(chunk_clean)
      write_csv(chunk_df, file.path(chunk_dir, sprintf("gbif_chunk_%03d_clean.csv", i)))
    }
  }
  write_lines(failed_taxa, file.path(chunk_dir, "gbif_failed_taxa.txt"))
  return(invisible(NULL))
}

# -----------------------------------------------------------------------------
# 5. Function: Merge GBIF Chunked CSV Results
# -----------------------------------------------------------------------------
merge_gbif_csv_chunks <- function(chunk_dir, merged_csv_path, batch_size = 10) {
  csv_files <- list.files(chunk_dir, pattern = "_clean\\.csv$", full.names = TRUE)
  n_batches <- ceiling(length(csv_files) / batch_size)
  temp_dir <- file.path(chunk_dir, "temp_batches")
  dir.create(temp_dir, showWarnings = FALSE)
  batch_paths <- character(n_batches)
  pb <- txtProgressBar(min = 0, max = n_batches, style = 3)
  for (i in seq_len(n_batches)) {
    files <- csv_files[seq(1 + (i-1) * batch_size, min(i * batch_size, length(csv_files)))]
    batch_df <- data.table::rbindlist(lapply(files, fread, showProgress=FALSE), use.names = TRUE, fill = TRUE)
    batch_path <- file.path(temp_dir, paste0("batch_", i, ".csv"))
    fwrite(batch_df, batch_path)
    batch_paths[i] <- batch_path
    setTxtProgressBar(pb, i)
    rm(batch_df); gc()
  }
  close(pb)
  gbif_merged <- data.table::rbindlist(lapply(batch_paths, fread), use.names = TRUE, fill = TRUE)
  fwrite(gbif_merged, merged_csv_path)
  message(sprintf("Combined %d rows into %s", nrow(gbif_merged), merged_csv_path))
  return(invisible(NULL))
}

# -----------------------------------------------------------------------------
# 6. Function: Clean, Standardize, and Process Raw GBIF Combined CSVs
# -----------------------------------------------------------------------------
process_gbif_raw <- function(raw_csv_path, output_csv, veglist_path) {
  dat <- read_csv(raw_csv_path, show_col_types = FALSE)
  dat <- dat %>%
    select(
      taxon_name_submitted = lookup_name,
      basis_of_record      = basisOfRecord,
      latitude             = decimalLatitude,
      longitude            = decimalLongitude,
      country_code         = countryCode,
      country              = country,
      state_province       = stateProvince,
      locality             = locality,
      inst_code            = institutionCode,
      collection_code      = collectionCode
    )
  dat <- dat %>%
    filter(basis_of_record != "FOSSIL_SPECIMEN") %>%
    filter(taxon_name_submitted != "#ERROR!")
  veg_list <- read_excel(veglist_path)
  dat$taxon_name_submitted <- gsub("Ã×", "×", dat$taxon_name_submitted, fixed = TRUE)
  dat <- dat %>%
    left_join(veg_list %>% select(taxon_name_accepted, Standardized_taxa),
              by = c("taxon_name_submitted" = "taxon_name_accepted")) %>%
    mutate(
      Standardized_taxa = as.character(Standardized_taxa),
      taxon_name_submitted = as.character(taxon_name_submitted),
      Standardized_taxa = str_trim(Standardized_taxa),
      Standardized_taxa = na_if(Standardized_taxa, ""),
      Standardized_taxa = na_if(Standardized_taxa, "#ERROR!"),
      Standardized_taxa = if_else(
        str_to_lower(Standardized_taxa) %in% c("no_match", "no match", "nomatch"),
        NA_character_, Standardized_taxa, missing = NA_character_),
      Standardized_taxa = coalesce(Standardized_taxa, taxon_name_submitted)
    )
  extract_genus_species <- function(name) {
    name %>%
      str_to_lower() %>%
      str_replace_all("(?i)(^|\\s)[×x](?=\\s|$)", "\\1") %>%
      str_squish() %>%
      str_extract("^\\S+\\s+\\S+")
  }
  dat <- dat %>%
    mutate(
      genus_species = extract_genus_species(Standardized_taxa),
      data_source   = "GBIF",
      inst_type     = "Botanic garden"
    )
  write_csv(dat, output_csv)
  message(sprintf("Wrote standardized processed file: %s", output_csv))
  return(dat)
}

# -----------------------------------------------------------------------------
# 7. Modular Pipeline Execution (Uncomment steps as needed)
# -----------------------------------------------------------------------------

# --- OLD SPECIES GBIF DATA ---
# (A) Extract occurrences in chunks for old species (run ONCE as needed)
# old_plantlist <- read_excel(old_species_path)
# species_list_old <- old_plantlist$taxon_name_accepted
# extract_gbif_occurrences_by_chunks(species_list_old, gbif_chunk_dir_old)

# (B) Merge chunked csvs to one file for old species (run ONCE as needed)
# merge_gbif_csv_chunks(gbif_chunk_dir_old, gbif_merged_csv_old)

# (C) Process standardized fields for old species
# process_gbif_raw(gbif_merged_csv_old, old_processed_out, old_species_path)

# --- NEW SPECIES GBIF DATA ---
# (A) Extract occurrences in chunks for new species (run ONCE as needed)
# new_plantlist <- read_excel(new_species_path)
# species_list_new <- new_plantlist$taxon_name_accepted
# extract_gbif_occurrences_by_chunks(species_list_new, gbif_chunk_dir_new)

# (B) Merge chunked csvs to one file for new species (run ONCE as needed)
# merge_gbif_csv_chunks(gbif_chunk_dir_new, gbif_merged_csv_new)

# (C) Process standardized fields for new species
process_gbif_raw(gbif_merged_csv_new, new_processed_out, new_species_path)

# --- FINAL COMBINED (OLD + NEW) PROCESSED DATA ---
old_dat <- read_csv(old_processed_out)
new_dat <- read_csv(new_processed_out)
combined_dat <- bind_rows(old_dat, new_dat)
write_csv(combined_dat, combined_processed)
message(sprintf("Combined old + new GBIF standardized data: %s", combined_processed))

# -----------------------------------------------------------------------------
# 8. QUICK RELOAD AND APPEND PRE-EXPORTED PART 1 & PART 2 DATASETS
# -----------------------------------------------------------------------------
# This section creates a fast-combined, light-cleaned version for fast downstream loading.

# Load Part 1 (CSV)
WCFP_GBIF_data_p1 <- read_csv(part1_path)

WCFP_GBIF_data_p1 <- WCFP_GBIF_data_p1 %>%
  filter(basisOfRecord != "FOSSIL_SPECIMEN") %>%
  mutate(data_source = "GBIF") %>%
  rename(
    taxa             = scientificName,
    basis_of_record  = basisOfRecord,
    latitude         = decimalLatitude,
    longitude        = decimalLongitude,
    country_code     = countryCode,
    state_province   = stateProvince,
    inst_code        = institutionCode,
    collection_code  = collectionCode
  )

# Load Part 2 (XLSX; change to read_csv() if needed)
WCFP_GBIF_data_p2 <- read_excel(part2_path)

WCFP_GBIF_data_p2 <- WCFP_GBIF_data_p2 %>%
  filter(basisOfRecord != "FOSSIL_SPECIMEN") %>%
  mutate(data_source = "GBIF") %>%
  rename(
    taxa             = scientificName,
    basis_of_record  = basisOfRecord,
    latitude         = decimalLatitude,
    longitude        = decimalLongitude,
    country_code     = countryCode,
    state_province   = stateProvince,
    inst_code        = institutionCode,
    collection_code  = collectionCode
  )

# Combine and Save
WCFP_GBIF_data_all_simple <- bind_rows(WCFP_GBIF_data_p1, WCFP_GBIF_data_p2)

write.csv(WCFP_GBIF_data_all_simple, combined_gbif_simple_path, row.names = FALSE)
message(sprintf("Reloaded and combined part 1 + part 2 GBIF data: %s", combined_gbif_simple_path))

# =============================================================================
# END OF COMPLETE WORKFLOW SCRIPT
# =============================================================================