#' Summarizes and compares record counts per institute from two data sources for a set of crops,
#' assigns organization type, checks EURISCO presence, and determines which data source to keep.
#'
#' @param genesys_df Data frame of Genesys accessions (must include "inst_code").
#' @param wiews_df Data frame of WIEWS accessions (must include "inst_code").
#' @param institute_names_no_syn Data frame for organization types (must include "inst_code", "wiews_org_type").
#' @param eurisco_path File path to EURISCO Excel sheet (must have "inst_code").
#' @return Data frame: inst_code, Genesys_records, WIEWS_records, wiews_org_type, EURISCO, keep.
#' @import dplyr
#' @import readxl
#' @export
select_data_source <- function(
    genesys_df,
    wiews_df,
    institute_names_no_syn,
    eurisco_path
) {
  # --- Input checks ---
  stopifnot("inst_code" %in% colnames(genesys_df))
  stopifnot("inst_code" %in% colnames(wiews_df))
  stopifnot(all(c("inst_code", "wiews_org_type") %in% colnames(institute_names_no_syn)))
  
  # --- Read EURISCO safely ---
  eurisco_list <- tryCatch(
    readxl::read_excel(eurisco_path),
    error = function(e) stop("Could not read EURISCO file: ", e$message)
  )
  stopifnot("inst_code" %in% colnames(eurisco_list))
  eurisco_codes <- unique(eurisco_list$inst_code)
  
  # --- Count records per inst_code ---
  genesys_counts <- genesys_df %>%
    dplyr::count(inst_code, name = "Genesys_records")
  wiews_counts <- wiews_df %>%
    dplyr::count(inst_code, name = "WIEWS_records")
  
  # --- Full set of institutes from all sources ---
  all_inst_codes <- union(
    union(genesys_counts$inst_code, wiews_counts$inst_code),
    union(institute_names_no_syn$inst_code, eurisco_codes)
  ) %>% unique() %>% as.data.frame()
  colnames(all_inst_codes) <- "inst_code"
  
  # --- Merge all info ---
  summary_table <- all_inst_codes %>%
    left_join(genesys_counts, by = "inst_code") %>%
    left_join(wiews_counts, by = "inst_code") %>%
    left_join(institute_names_no_syn[, c("inst_code", "wiews_org_type")], by = "inst_code") %>%
    mutate(
      Genesys_records = tidyr::replace_na(Genesys_records, 0L),
      WIEWS_records   = tidyr::replace_na(WIEWS_records, 0L),
      wiews_org_type = tidyr::replace_na(wiews_org_type, "Unknown"),
      EURISCO = inst_code %in% eurisco_codes
    )
  
  # --- Selection logic for 'keep' column ---
  summary_table <- summary_table %>%
    mutate(
      keep = dplyr::case_when(
        wiews_org_type == "CGIAR" & Genesys_records > 0 ~ "Genesys",
        Genesys_records > WIEWS_records ~ "Genesys",
        WIEWS_records > Genesys_records ~ "WIEWS",
        Genesys_records == WIEWS_records ~ "Genesys",
        TRUE ~ NA_character_
      )
    )
  
  # --- Select only relevant columns ---
  summary_table %>%
    select(inst_code, Genesys_records, WIEWS_records, wiews_org_type, EURISCO, keep)
}
