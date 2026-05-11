# ============================================================================
# IUCN Red List Extraction Workflow Template
# ============================================================================
# NOTE: You MUST have a valid IUCN Red List API token to use this workflow.
#       Register for free at: https://apiv3.iucnredlist.org/api/v3/token
#
# Description:
#   - Extracts IUCN Red List status, IUCN Red List ID (sis_taxon_id), and related info
#     for any list of species (with genus and species columns) using the iucnredlist R package.
#   - Writes results and lookup failures to CSV files.
#
# Features:
#   - Works with any .csv or .xlsx species list (specify column and file below)
#   - Outputs Red List category, IUCN Red List ID, year, assessment date, rationale, URL, etc.
#   - Failures (not matched in IUCN) recorded to a separate CSV
#   - Safely handles missing values
#
# How to use:
#   1. Install/load the packages below
#   2. Set your IUCN Red List API token as IUCN_TOKEN below
#   3. Edit the input file/column names as needed
#   4. Run to produce results/failures CSVs
# ============================================================================

# ---- 1. Install/load packages ----
# remotes::install_github("IUCN-UK/iucnredlist")
# install.packages(c("dplyr", "tibble", "purrr", "readr", "tidyr"))
suppressPackageStartupMessages({
  library(iucnredlist)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(readr)
  library(tidyr)
})

# ---- 2. Set your IUCN Red List API token ----
IUCN_TOKEN <- "YOUR_REAL_IUCN_REDLIST_KEY_HERE"  # <--- Insert your API key here

# ---- 3. Load your input species list (.csv or .xlsx), must contain genus and species ----
input_file <- "your_species_list.csv"             # <--- change to your file name
species_col_genus <- "genus"                      # <--- edit column name as needed
species_col_species <- "species"                  # <--- edit column name as needed

# ----- Example for .csv -----
input_df <- read_csv(input_file)

# ----- Example for .xlsx (uncomment if needed) -----
# library(readxl)
# input_df <- readxl::read_excel(input_file, sheet = 1)

# ----- If you have a single column with names -----
# input_df <- input_df %>% tidyr::separate(your_single_name_col, into = c("genus", "species"), sep = " ", remove = FALSE)

stopifnot(all(c(species_col_genus, species_col_species) %in% names(input_df)))

# ---- 4. Helper for NA-fallback and field collapsing ----
`%||%` <- function(a, b) if (!is.null(a)) a else b
safe_collapse <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (length(x) == 0) return(NA_character_)
  if (is.list(x) && !is.data.frame(x)) x <- unlist(x)
  paste(x, collapse = "; ")
}

# ---- 5. Extraction for one genus/species ----
extract_redlist_fields <- function(api, genus, species) {
  tryCatch({
    assessments <- assessments_by_name(api, genus = genus, species = species)
    if (nrow(assessments) == 0) return(NULL)
    ix <- which(assessments$latest == TRUE)
    if(length(ix) == 0) ix <- 1
    assessment_id <- assessments$assessment_id[ix][1]
    sis_taxon_id <- as.character(assessments$sis_taxon_id[ix][1])
    scientificName <- assessments$taxon_scientific_name[ix][1] %||% paste(genus, species)
    redlistCategory <- assessments$red_list_category_code[ix][1] %||% NA_character_
    yearPublished <- assessments$year_published[ix][1] %||% NA_character_
    url <- assessments$url[ix][1] %||% NA_character_
    # More fields as needed:
    raw <- assessment_data(api, assessment_id)
    parsed <- parse_assessment_data(raw)
    tibble(
      genus           = genus,
      species         = species,
      iucnRedlistID   = sis_taxon_id,
      scientificName  = scientificName,
      redlistCategory = redlistCategory,
      yearPublished   = yearPublished,
      assessmentDate  = safe_collapse(parsed$assessment_date),
      rationale       = safe_collapse(parsed$rationale),
      url             = url
    )
  }, error = function(e) { return(NULL) })
}

# ---- 6. Run batch extraction ----
cat("Connecting to IUCN API and querying species...\n")
api <- init_api(IUCN_TOKEN)
species_tbl <- input_df %>% select(
  genus   = !!species_col_genus,
  species = !!species_col_species
)
results_list <- pmap(species_tbl, ~extract_redlist_fields(api, ..1, ..2))
results_tbl <- bind_rows(results_list)
failed <- species_tbl[which(sapply(results_list, is.null)), ]

# ---- 7. Save output ----
write_csv(results_tbl, "iucn_redlist_results.csv")
if (nrow(failed)) {
  write_csv(failed, "iucn_redlist_failures.csv")
  cat(nrow(failed), "taxa not found on IUCN Red List (see iucn_redlist_failures.csv)\n")
}
cat("IUCN Red List lookup complete. Results: iucn_redlist_results.csv\n")
if(nrow(results_tbl)) print(head(results_tbl, 5))