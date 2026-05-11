# =========================================================================
# Taxon ID Batch Extraction Workflow Template
# =========================================================================
# Description:
#   - This workflow batch-extracts widely used plant taxon IDs for a species list
#     using the Global Names Verifier API (https://verifier.globalnames.org) and
#     the BGCI PlantSearch API (https://www.bgci.org). You can enable/disable any
#     ID type (WFO, GBIF, NPGS/GRIN, IPNI, BGCI) with arguments.
#   - Outputs are written in a subfolder as Excel (.xlsx), a lookup checkpoint CSV,
#     and per-ID unmatched lists (optional).
#
# Features:
#   - Supports input as .csv or .xlsx
#   - Flexible: Any column name for species names
#   - Fast: Uses all but 2 CPU cores and is resumable
#   - No hard-coded file names, paths, or example species—just plug in yours!
#
# USAGE EXAMPLE:
#   1) Prepare your species list file (CSV or Excel) with one column containing
#      scientific names (e.g., "Species_Name").
#
#   2) Set the file path and species column below.
#
#   3) Run this script!
#
#   Example call:
#     result <- taxon_id_workflow(
#       input_file       = "my_species_list.csv", # or .xlsx
#       species_col      = "Species_Name",
#       output_dir       = "taxon_id_results",
#       extract_wfo      = TRUE,
#       extract_gbif     = TRUE,
#       extract_npgs     = TRUE,
#       extract_ipni     = TRUE,
#       extract_bgci     = TRUE,
#       export_unmatched = TRUE,
#       verbose          = TRUE
#     )
# =========================================================================

# ---- LOAD LIBRARIES ----
suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(stringr)
  library(httr)
  library(jsonlite)
  library(readr)
  library(future.apply)
  library(purrr)
  library(tibble)
})

# ---- MAIN FUNCTION: Batch Taxon ID Extraction ----
taxon_id_workflow <- function(
    input_file,
    sheet = NULL,                  # For Excel: sheet name or index
    species_col = "Species_Name",  # Column with species names
    output_dir = "taxon_id_out",   # Where to write output files
    extract_wfo  = TRUE,           # Enable World Flora Online ID extraction
    extract_gbif = TRUE,           # Enable GBIF matched name and GBIF ID
    extract_npgs = TRUE,           # Enable NPGS/GRIN nomen number
    extract_ipni = TRUE,           # Enable IPNI ID
    extract_bgci = TRUE,           # Enable BGCI PlantSearch ID
    chunk_size   = 150,            # Query batch size per chunk
    resume       = TRUE,           # Resume from previous lookup cache?
    export_unmatched = TRUE,       # Write unmatched lists for each ID type
    verbose      = TRUE
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  filebase <- tools::file_path_sans_ext(basename(input_file))
  lookup_csv  <- file.path(output_dir, paste0(filebase, "_taxon_id_lookup.csv"))
  output_xlsx <- file.path(output_dir, paste0(filebase, "_with_ids.xlsx"))
  unmatched_csv <- function(field) file.path(output_dir, paste0(filebase, "_no_", tolower(field), ".csv"))
  # ---- Read Input File ----
  if (endsWith(input_file, ".xlsx")) {
    sheets <- readxl::excel_sheets(input_file)
    if (is.null(sheet)) sheet <- sheets[1]
    df <- readxl::read_excel(input_file, sheet = sheet)
  } else if (endsWith(input_file, ".csv")) {
    df <- readr::read_csv(input_file, show_col_types = FALSE)
  } else stop("Supported input formats: .xlsx, .csv")
  stopifnot(species_col %in% colnames(df))
  df <- df %>% mutate(Species_Name = as.character(.data[[species_col]]))
  # ---- Tidy/canonicalize species names ----
  clean_species <- function(x) x %>%
    stringr::str_replace_all("\t", " ") %>%
    stringr::str_remove_all("\\+") %>%
    stringr::str_replace_all("×", " ") %>%
    stringr::str_replace_all("\\b[xX]\\b", " ") %>%
    stringr::str_squish()
  df <- df %>% mutate(Species_Name = clean_species(Species_Name))
  species_to_query <- unique(na.omit(df$Species_Name))
  # ---- Parallel setup ----
  workers <- max(1, parallel::detectCores() - 2)
  future::plan(future::multisession, workers = workers)
  # ---- GN Verifier query wrappers ----
  gn_query_one_source <- function(taxa, source_id) {
    taxa_format <- gsub(" ", "+", taxa)
    url <- paste0(
      "https://verifier.globalnames.org/api/v1/verifications/", taxa_format,
      "?data_sources=", as.character(source_id),
      "&all_matches=false&capitalize=true&species_group=false&fuzzy_uninomial=false&stats=false&main_taxon_threshold=0.8"
    )
    tryCatch({
      r <- httr::GET(url, httr::timeout(30))
      if (r$status_code != 200) stop("status ", r$status_code)
      txt <- httr::content(r, "text", encoding = "UTF-8")
      jsonlite::fromJSON(txt, simplifyVector = FALSE, simplifyDataFrame = FALSE)
    }, error = function(e) list(error = e$message))
  }
  extract_bestResult <- function(res) {
    if (is.null(res) || !is.list(res) || "error" %in% names(res)) return(NULL)
    if (is.null(res$names) || length(res$names) < 1) return(NULL)
    br <- res$names[[1]]$bestResult
    if (is.null(br) || length(br) == 0) return(NULL)
    br
  }
  ext_wfo  <- function(res) {
    br <- extract_bestResult(res); if (is.null(br)) return(NA_character_)
    cands <- c(br$outlink, br$recordId, br$currentRecordId) |> as.character()
    hit <- stringr::str_extract(cands, "(?<=wfo-)\\d{10}")
    hit <- hit[!is.na(hit)]; if (length(hit)) hit[[1]] else NA_character_
  }
  ext_gbif_name <- function(res) {
    br <- extract_bestResult(res); if (is.null(br)) return(NA_character_)
    nm <- c(br$currentName, br$matchedName) |> as.character()
    nm <- nm[!is.na(nm) & nzchar(nm)]
    if (length(nm)) nm[[1]] else NA_character_
  }
  ext_gbif_id <- function(res) {
    br <- extract_bestResult(res); if (is.null(br)) return(NA_character_)
    outlink <- as.character(br$outlink)
    m <- stringr::str_match(outlink, "gbif\\.org/species/(\\d+)")[,2]
    if (!is.na(m)) m else NA_character_
  }
  ext_npgs <- function(res) {
    br <- extract_bestResult(res); if (is.null(br)) return(NA_character_)
    outlink <- as.character(br$outlink)
    id <- stringr::str_match(outlink, "[?&]id=(\\d+)")[,2]
    if (!is.na(id)) return(id)
    id2 <- stringr::str_extract(outlink, "\\d+(?=[^\\d]*$)")
    if (!is.na(id2)) id2 else NA_character_
  }
  ext_ipni <- function(res) {
    br <- extract_bestResult(res); if (is.null(br)) return(NA_character_)
    outlink <- as.character(br$outlink)
    m <- stringr::str_match(outlink, "ipni\\.org/(?:n|names|name)/([0-9]+-[0-9]+)(?:\\b|/|\\?|#|$)")[,2]
    if (!is.na(m)) return(m)
    NA_character_
  }
  # ---- BGCI ----
  ext_bgci <- function(taxon_clean) {
    words <- stringr::str_split(taxon_clean, " ")[[1]]
    genus   <- if (length(words) >= 1) words[1] else NA_character_
    epithet <- if (length(words) >= 2) words[2] else NA_character_
    if (is.na(genus) || is.na(epithet)) return(list(taxon_id = NA_character_, status = "invalid"))
    url <- httr::modify_url(
      "https://datatools.bgci.org/api/plants/search",
      query = list(
        `filter[genus]` = genus,
        `filter[specific_epithet]` = epithet,
        sort = "name", page = 1
      )
    )
    tryCatch({
      r <- httr::GET(url, httr::timeout(30),
                     httr::add_headers(
                       `Accept`  = "application/json",
                       `Origin`  = "https://plantsearch.bgci.org",
                       `Referer` = "https://plantsearch.bgci.org/"),
                     httr::user_agent("Mozilla/5.0"))
      if (r$status_code != 200) return(list(taxon_id = NA_character_, status = paste0("http_", r$status_code)))
      txt <- httr::content(r, as = "text", encoding = "UTF-8")
      js <- jsonlite::fromJSON(txt)
      if (!is.null(js$data) && nrow(js$data) > 0) {
        row <- js$data[1, ]
        return(list(
          taxon_id   = as.character(row$id),
          search_name = as.character(row$name),
          search_status = as.character(row$status),
          url = paste0("https://plantsearch.bgci.org/taxon/", row$id)
        ))
      }
      list(taxon_id = NA_character_, status = "no_id_found")
    }, error = function(e) list(taxon_id = NA_character_, status = "error"))
  }
  # ---- Resume existing cache? ----
  lookup <- tibble(Species_Name = character())
  if (resume && file.exists(lookup_csv))
    lookup <- readr::read_csv(lookup_csv, show_col_types = FALSE) %>%
    mutate(across(everything(), as.character))
  todo <- setdiff(species_to_query, lookup$Species_Name)
  chunks <- split(todo, ceiling(seq_along(todo) / chunk_size))
  # ---- Main batch loop ----
  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    if (isTRUE(verbose)) cat("Processing chunk", i, "/", length(chunks), "-", length(chunk), "species...\n")
    res_wfo   <- if (extract_wfo)  future_lapply(chunk, function(x) gn_query_one_source(x, 196)) else rep(list(NULL), length(chunk))
    res_gbif  <- if (extract_gbif) future_lapply(chunk, function(x) gn_query_one_source(x, 11))  else rep(list(NULL), length(chunk))
    res_npgs  <- if (extract_npgs) future_lapply(chunk, function(x) gn_query_one_source(x, 6))   else rep(list(NULL), length(chunk))
    res_ipni  <- if (extract_ipni) future_lapply(chunk, function(x) gn_query_one_source(x, 167)) else rep(list(NULL), length(chunk))
    gbif_name <- vapply(res_gbif, ext_gbif_name, FUN.VALUE = character(1))
    gbif_idn  <- vapply(res_gbif, ext_gbif_id,   FUN.VALUE = character(1))
    wfo_idn   <- vapply(res_wfo,  ext_wfo,       FUN.VALUE = character(1))
    npgs_idn  <- vapply(res_npgs, ext_npgs,      FUN.VALUE = character(1))
    ipni_idn  <- vapply(res_ipni, ext_ipni,      FUN.VALUE = character(1))
    # BGCI (first two words, fully robust)
    bgci_clean <- chunk %>% stringr::str_squish() %>% stringr::str_extract("^\\S+\\s+\\S+")
    res_bgci   <- if (extract_bgci) future_lapply(bgci_clean, ext_bgci) else rep(list(list(taxon_id=NA)), length(chunk))
    bgci_idn   <- vapply(res_bgci, function(x) if (!is.null(x$taxon_id) && length(x$taxon_id) == 1) x$taxon_id else NA_character_, character(1))
    bgci_name  <- vapply(res_bgci, function(x) if (!is.null(x$search_name) && length(x$search_name) == 1) x$search_name else NA_character_, character(1))
    bgci_sts   <- vapply(res_bgci, function(x) if (!is.null(x$search_status) && length(x$search_status) == 1) x$search_status else NA_character_, character(1))
    bgci_url   <- vapply(res_bgci, function(x) if (!is.null(x$url) && length(x$url) == 1) x$url else NA_character_, character(1))
    # Collate this chunk
    new <- tibble(
      Species_Name = chunk,
      WFO_ID_10digit = if (extract_wfo) wfo_idn else NULL,
      GBIF_TaxonName = if (extract_gbif) gbif_name else NULL,
      GBIF_ID        = if (extract_gbif) gbif_idn else NULL,
      NPGS_TaxonID   = if (extract_npgs) npgs_idn else NULL,
      IPNI_ID        = if (extract_ipni) ipni_idn else NULL,
      BGCI_TaxonID   = if (extract_bgci) bgci_idn else NULL,
      BGCI_PlantSearch_Name   = if (extract_bgci) bgci_name else NULL,
      BGCI_PlantSearch_Status = if (extract_bgci) bgci_sts else NULL,
      BGCI_URL       = if (extract_bgci) bgci_url else NULL
    )
    lookup <- bind_rows(lookup, new)
    readr::write_csv(lookup, lookup_csv)
    Sys.sleep(0.2)
  }
  # ---- Join and write ----
  out <- dplyr::left_join(df, lookup, by = "Species_Name")
  writexl::write_xlsx(out, output_xlsx)
  # ---- Export unmatched per field ----
  if (export_unmatched) {
    cols <- c(WFO_ID_10digit = extract_wfo, GBIF_TaxonName = extract_gbif,
              NPGS_TaxonID = extract_npgs, IPNI_ID = extract_ipni,
              BGCI_PlantSearch_Name = extract_bgci)
    for (field in names(cols)[cols]) {
      miss <- lookup %>% filter(is.na(.data[[field]]) | !nzchar(.data[[field]]))
      if (nrow(miss)) readr::write_csv(miss, unmatched_csv(field))
    }
  }
  if (isTRUE(verbose)) cat("Finished!\n Main output: ", output_xlsx,"\n Lookup csv: ", lookup_csv, "\n")
  invisible(list(output_xlsx = output_xlsx, output_lookup = lookup_csv))
}

# ==== USAGE EXAMPLE (uncomment and edit as needed) ====
# result <- taxon_id_workflow(
#   input_file       = "my_species_list.csv", # or .xlsx
#   species_col      = "Species_Name",
#   output_dir       = "taxon_id_results",
#   extract_wfo      = TRUE,
#   extract_gbif     = TRUE,
#   extract_npgs     = TRUE,
#   extract_ipni     = TRUE,
#   extract_bgci     = TRUE,
#   export_unmatched = TRUE,
#   verbose          = TRUE
# )
# cat("See:", result$output_xlsx, "\n")

# ==== TO RUN: ====
# 1. Save as an .R script or use interactively.
# 2. Edit 'input_file' and 'species_col' in the function call to match your data.
# 3. Run the 'taxon_id_workflow' call (above).
# 4. Review output Excel/CSV files in your output_dir!