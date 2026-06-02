#' Selects data source with most records for each genus-species + institute pair,
#' always keeping Genesys for CGIAR orgs.
#'
#' @param acc_dataset Data frame with accession records. Must contain columns:
#'   wcfp_name_match, inst_code, data_source
#' @param institute_names_no_syn Data frame with columns: inst_code, wiews_org_category
#' @param eurisco_path File path to EURISCO Excel sheet (must have "inst_code")
#' @param preferred_order Character vector listing preferred sources in order,
#'   e.g. c("Genesys","WIEWS","GBIF_living","Cano")
#' @return Data frame with one row per genus-species + inst_code:
#'   species, inst_code, wiews_org_category, EURISCO, per-source record counts,
#'   duplicate, and best_data_source
#' @import dplyr
#' @import readxl
#' @import tidyr
#' @export
select_data_source <- function(
    acc_dataset,
    institute_names_no_syn,
    eurisco_path,
    preferred_order = c("Genesys", "WIEWS", "GBIF_living", "Cano")
) {
  stopifnot(all(c("wcfp_name_match", "inst_code", "data_source") %in% colnames(acc_dataset)))
  stopifnot(all(c("inst_code", "wiews_org_category") %in% colnames(institute_names_no_syn)))
  
  eurisco_list <- tryCatch(
    readxl::read_excel(eurisco_path),
    error = function(e) stop("Could not read EURISCO file: ", e$message)
  )
  stopifnot("inst_code" %in% colnames(eurisco_list))
  eurisco_codes <- unique(eurisco_list$inst_code)
  
  institute_names_no_syn_one <- institute_names_no_syn %>%
    dplyr::select(inst_code, wiews_org_category) %>%
    dplyr::distinct(inst_code, .keep_all = TRUE)
  
  # Count records per wcfp_name_match, inst_code, and data_source
  count_table <- acc_dataset %>%
    dplyr::count(wcfp_name_match, inst_code, data_source, name = "records") %>%               # wcfp_name_match = normalized genus-species
    dplyr::ungroup()                                                                          # Here the function counts distinct genus-species + institution combination, by data source
  
  # Create wide count table and force one row per wcfp_name_match + inst_code
  count_table_wide <- count_table %>%
    dplyr::mutate(data_source_col = paste0(data_source, "_records")) %>%
    dplyr::select(wcfp_name_match, inst_code, data_source_col, records) %>%
    tidyr::pivot_wider(
      names_from = data_source_col,
      values_from = records,
      values_fill = 0
    ) %>%
    dplyr::group_by(wcfp_name_match, inst_code) %>%
    dplyr::summarise(
      dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # Flag institute/name pairs present in more than one data source
  duplicate_table <- count_table %>%
    dplyr::group_by(wcfp_name_match, inst_code) %>%
    dplyr::summarise(
      duplicate = ifelse(dplyr::n() > 1, "Y", ""),
      .groups = "drop"
    )
  
  # Select best source by max records; tie-break by preferred_order
  best_sources <- count_table %>%
    dplyr::group_by(wcfp_name_match, inst_code) %>%
    dplyr::mutate(max_records = max(records)) %>%
    dplyr::filter(records == max_records) %>%
    dplyr::arrange(match(data_source, preferred_order), .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(wcfp_name_match, inst_code, data_source)
  
  # CGIAR institutes with Genesys present
  genesys_cgiar <- count_table %>%
    dplyr::inner_join(
      institute_names_no_syn_one %>%
        dplyr::filter(wiews_org_category == "CGIAR"),
      by = "inst_code"
    ) %>%
    dplyr::filter(data_source == "Genesys" & records > 0) %>%
    dplyr::transmute(wcfp_name_match, inst_code, force_genesys = TRUE)
  
  # Apply CGIAR override
  best_sources <- best_sources %>%
    dplyr::left_join(genesys_cgiar, by = c("wcfp_name_match", "inst_code")) %>%
    dplyr::mutate(
      best_data_source = ifelse(!is.na(force_genesys) & force_genesys, "Genesys", data_source)
    ) %>%
    dplyr::select(wcfp_name_match, inst_code, best_data_source)
  
  # Final output: one row per wcfp_name_match + inst_code
  output <- count_table_wide %>%
    dplyr::left_join(institute_names_no_syn_one, by = "inst_code") %>%
    dplyr::mutate(EURISCO = inst_code %in% eurisco_codes) %>%
    dplyr::left_join(duplicate_table, by = c("wcfp_name_match", "inst_code")) %>%
    dplyr::left_join(best_sources, by = c("wcfp_name_match", "inst_code"))
  
  output %>%
    dplyr::mutate(
      max_records_total = do.call(
        pmax,
        c(dplyr::pick(dplyr::ends_with("_records")), na.rm = TRUE)
      )
    ) %>%
    dplyr::arrange(dplyr::desc(max_records_total), wcfp_name_match, inst_code) %>%
    dplyr::select(
      species = wcfp_name_match,
      inst_code,
      wiews_org_category,
      EURISCO,
      dplyr::ends_with("_records"),
      duplicate,
      best_data_source
    )
}
