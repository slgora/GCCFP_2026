####################################################################################
#
#  Workflow calculates main metrics from final data
#  
#  Outputs results files:
#     1. metrics_summary.xlsx
#     2. institution_summary_of_record_count.xlsx
#
#   something wrong with calculation of
#   "Number of distinct WCFP species unique to data" in NEITHER_genebanks_or_botanic_gardens
#   "Percent of total distinct WCFP species unique to data" in NEITHER_genebanks_or_botanic_gardens
#   *** SG NOTE TO FIX THIS, HAD HARDCODED THIS METRIC FOR NOW ****
#
################################################################################

# ---- Load all packages ----
library(dplyr)
library(tidyr)
library(readxl)
library(scales)
library(openxlsx)
library(writexl)

# ---- 1. Set today's date for Excel outputs ----
today_str <- format(Sys.Date(), "%Y-%m-%d")
metrics_output_file <- paste0(
  "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Outputs_script6_2026-06-02/metrics_summary_",
  today_str, ".xlsx"
)
institutions_output_file <- paste0(
  "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Outputs_script6_2026-06-02/institution_summary_of_record_count_",
  today_str, ".xlsx"
)

# ---- 2. Read in datasets ----
WCFP_plantlist <- read_excel("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Input_files_script6_2026-06-02/WCFP_plantlist_standardized_2026-02-23.xlsx")
WCFP_plantlist_names <- WCFP_plantlist %>% select(taxon_name_accepted)
wcfp_total_species <- nrow(WCFP_plantlist_names) #26,632 distinct species

# FINAL genebank_accessionlevel_dataset: 4,724,258 rows
genebank_accessionlevel_dataset <- read.csv('C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Outputs_script5_2026-06-02/genebank_accessionlevel_dataset_FINAL_2026-06-02.csv', stringsAsFactors=FALSE) %>%
  select(wcfp_name_match, inst_code, data_source)

# FINAL botanicgarden_accessionlevel_dataset: 507,857 rows
botanicgarden_accessionlevel_dataset <- read.csv('C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Outputs_script5_2026-06-02/botanicgarden_accessionlevel_dataset_FINAL_2026-06-02.csv', stringsAsFactors=FALSE) %>%
  select(wcfp_name_match, inst_code, data_source)

# FINAL: Botanic garden species/inst level dataset: 618,544 rows
botanicgarden_specieslevel_dataset <- read.csv('C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Outputs_script4_2026-06-02/botanicgarden_specieslevel_dataset_FINAL_2026-06-02.csv', stringsAsFactors=FALSE) %>%
  select(wcfp_name_match, inst_code, data_source)

# FINAL occurrences dataset: 5,457,893 rows
occurrences_dataset <- read.csv('C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Outputs_script5_2026-06-02/occurrences_dataset_FINAL_2026-06-02.csv', stringsAsFactors=FALSE) %>%
  select(wcfp_name_match, inst_code, data_source)


# ---- 3. Coerce inst_code fields for all datasets ----
genebank_accessionlevel_dataset$inst_code <- as.character(genebank_accessionlevel_dataset$inst_code)
botanicgarden_accessionlevel_dataset$inst_code <- as.character(botanicgarden_accessionlevel_dataset$inst_code)
botanicgarden_specieslevel_dataset$inst_code <- as.character(botanicgarden_specieslevel_dataset$inst_code)
occurrences_dataset$inst_code <- as.character(occurrences_dataset$inst_code)

# ---- 4. Data source splits ----
genesys_gb <- genebank_accessionlevel_dataset %>% filter(data_source == "Genesys")
wiews_gb <- genebank_accessionlevel_dataset %>% filter(data_source == "WIEWS")
gbif_living_gb <- genebank_accessionlevel_dataset %>% filter(data_source == "GBIF_living")
genesys_bg <- botanicgarden_accessionlevel_dataset %>% filter(data_source == "Genesys")
wiews_bg <- botanicgarden_accessionlevel_dataset %>% filter(data_source == "WIEWS")
gbif_living_bg <- botanicgarden_accessionlevel_dataset %>% filter(data_source == "GBIF_living")
cano <- botanicgarden_accessionlevel_dataset %>% filter(data_source == "Cano")
bgci <- botanicgarden_specieslevel_dataset
gbif_observations <- occurrences_dataset %>% filter(data_source == "GBIF_observations")

# ---- 5. Safe formatting helpers ----
safe_comma <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  res <- ifelse(is.na(x_num) | is.null(x) | x=="" | length(x_num)==0, "", scales::comma(x_num))
  if (is.factor(res)) res <- as.character(res)
  res
}
safe_fmt <- function(x, digits = 1, percent=FALSE) {
  x <- suppressWarnings(as.numeric(x))
  if (is.na(x) | is.null(x) | length(x)==0) return("")
  val <- formatC(x, digits=digits, format="f")
  if (percent) paste0(val, "%") else val
}

# ---- 6. Summary Metrics Block ----
all_gb <- unique(genebank_accessionlevel_dataset$wcfp_name_match)
all_bg <- unique(c(botanicgarden_accessionlevel_dataset$wcfp_name_match, botanicgarden_specieslevel_dataset$wcfp_name_match))
summary_metrics <- c(
  "Number of accessions",
  "Number of records",
  "Number of distinct institutions",
  "Number of distinct WCFP species",
  "Percent of total distinct WCFP species",
  "Number of distinct WCFP species unique to data",
  "Percent of total distinct WCFP species unique to data",
  "Number of distinct WCFP species NOT in data",
  "Percent of total distinct WCFP species NOT in data"
)
dataset_tbls <- list(
  genebank_accessionlevel_dataset = genebank_accessionlevel_dataset,
  botanicgarden_accessionlevel_dataset = botanicgarden_accessionlevel_dataset,
  botanicgarden_specieslevel_dataset = botanicgarden_specieslevel_dataset,
  occurrences_dataset = occurrences_dataset,
  Genesys = rbind(genesys_gb, genesys_bg),
  WIEWS = rbind(wiews_gb, wiews_bg),
  Cano = cano,
  GBIF_living = rbind(gbif_living_gb, gbif_living_bg),
  GBIF_observations = gbif_observations,
  BGCI = bgci
)
summarystats_raw <- tibble(metric = summary_metrics)
for (datnm in c(
  "genebanks", "botanic_gardens", "BOTH_genebanks_and_botanic_gardens",
  "NEITHER_genebanks_or_botanic_gardens",
  names(dataset_tbls)
)) {
  if (datnm == "NEITHER_genebanks_or_botanic_gardens") {
    vals <- rep("", length(summary_metrics))
    summarystats_raw[[datnm]] <- vals
    next
  }
  if (datnm == "genebanks") {
    dt <- n_distinct(genebank_accessionlevel_dataset$wcfp_name_match)
    pctdt <- round(100 * dt / wcfp_total_species, 1)
    dt_unique <- length(setdiff(all_gb, all_bg))
    pctdt_unique <- round(100 * dt_unique / wcfp_total_species, 1)
    vals <- c(
      safe_comma(nrow(genebank_accessionlevel_dataset)),
      safe_comma(nrow(genebank_accessionlevel_dataset)),
      safe_comma(n_distinct(genebank_accessionlevel_dataset$inst_code)),
      safe_comma(dt),
      safe_fmt(pctdt, percent = TRUE),
      safe_comma(dt_unique),
      safe_fmt(pctdt_unique, percent = TRUE),
      safe_comma(wcfp_total_species - dt),
      safe_fmt(100 - pctdt, percent = TRUE)
    )
    summarystats_raw[[datnm]] <- vals
    next
  }
  if (datnm == "botanic_gardens") {
    dt <- n_distinct(c(botanicgarden_accessionlevel_dataset$wcfp_name_match, botanicgarden_specieslevel_dataset$wcfp_name_match))
    pctdt <- round(100 * dt / wcfp_total_species, 1)
    dt_unique <- length(setdiff(all_bg, all_gb))
    pctdt_unique <- round(100 * dt_unique / wcfp_total_species, 1)
    vals <- c(
      safe_comma(nrow(botanicgarden_accessionlevel_dataset)),
      safe_comma(nrow(botanicgarden_accessionlevel_dataset) + nrow(botanicgarden_specieslevel_dataset)),
      safe_comma(n_distinct(c(botanicgarden_accessionlevel_dataset$inst_code, botanicgarden_specieslevel_dataset$inst_code))),
      safe_comma(dt),
      safe_fmt(pctdt, percent = TRUE),
      safe_comma(dt_unique),
      safe_fmt(pctdt_unique, percent = TRUE),
      safe_comma(wcfp_total_species - dt),
      safe_fmt(100 - pctdt, percent = TRUE)
    )
    summarystats_raw[[datnm]] <- vals
    next
  }
  if (datnm == "BOTH_genebanks_and_botanic_gardens") {
    both <- length(intersect(all_gb, all_bg))
    dt <- n_distinct(unique(c(all_gb, all_bg)))
    pctdt <- round(100 * dt / wcfp_total_species, 1)
    pctdt_both <- round(100 * both / wcfp_total_species, 1)
    vals <- c(
      safe_comma(nrow(genebank_accessionlevel_dataset) + nrow(botanicgarden_accessionlevel_dataset)),
      safe_comma(nrow(genebank_accessionlevel_dataset) + nrow(botanicgarden_accessionlevel_dataset) + nrow(botanicgarden_specieslevel_dataset)),
      safe_comma(n_distinct(c(genebank_accessionlevel_dataset$inst_code, botanicgarden_accessionlevel_dataset$inst_code, botanicgarden_specieslevel_dataset$inst_code))),
      safe_comma(dt),
      safe_fmt(pctdt, percent = TRUE),
      safe_comma(both),
      safe_fmt(pctdt_both, percent = TRUE),
      safe_comma(wcfp_total_species - dt),
      safe_fmt(100 - pctdt, percent = TRUE)
    )
    summarystats_raw[[datnm]] <- vals
    next
  }
  # ---- For remaining dataset columns ----
  df <- dataset_tbls[[datnm]]
  dt <- n_distinct(df$wcfp_name_match)
  pctdt <- round(100 * dt / wcfp_total_species, 1)
  comparison_groups <- list(
    genebank_accessionlevel_dataset = c("botanicgarden_accessionlevel_dataset", "botanicgarden_specieslevel_dataset"),
    botanicgarden_accessionlevel_dataset = c("genebank_accessionlevel_dataset"),
    botanicgarden_specieslevel_dataset = c("genebank_accessionlevel_dataset"),
    occurrences_dataset = NULL,
    Genesys = c("WIEWS", "Cano", "GBIF_living", "BGCI"),
    WIEWS = c("Genesys", "Cano", "GBIF_living", "BGCI"),
    Cano = c("Genesys", "WIEWS", "GBIF_living", "BGCI"),
    GBIF_living = c("Genesys", "WIEWS", "Cano", "BGCI"),
    BGCI = c("Genesys", "WIEWS", "Cano", "GBIF_living"),
    GBIF_observations = NULL
  )
  compare_to <- comparison_groups[[datnm]]
  if (is.null(compare_to) || length(compare_to) == 0) {
    dt_unique <- ""
    pctdt_unique <- ""
  } else {
    other_species <- unique(unlist(lapply(dataset_tbls[compare_to], function(df) unique(df$wcfp_name_match))))
    dt_unique <- length(setdiff(unique(df$wcfp_name_match), other_species))
    pctdt_unique <- round(100 * dt_unique / wcfp_total_species, 1)
  }
  if (datnm %in% c("occurrences_dataset", "GBIF_observations")) {
    dt_unique <- ""; pctdt_unique <- ""
  }
  # ---- Number of accessions is BLANK for certain datasets: ----
  n_accessions <- if(datnm %in% c("botanicgarden_specieslevel_dataset",
                                  "occurrences_dataset",
                                  "GBIF_observations",
                                  "BGCI")) {
    ""
  } else {
    safe_comma(if(!is.null(df)) nrow(df) else NA)
  }
  vals <- c(
    n_accessions,
    safe_comma(if(!is.null(df)) nrow(df) else NA),
    safe_comma(if(!is.null(df)) n_distinct(df$inst_code) else NA),
    safe_comma(dt),
    safe_fmt(pctdt, percent = TRUE),
    if (dt_unique == "") "" else safe_comma(dt_unique),
    if (pctdt_unique == "") "" else safe_fmt(pctdt_unique, percent = TRUE),
    safe_comma(wcfp_total_species - dt),
    safe_fmt(100 - pctdt, percent = TRUE)
  )
  summarystats_raw[[datnm]] <- vals
}

############################################################
################ sG NOTE: NEED TO FIX THIS #################
################ HARDCODED FOR NOW #########################
############################################################

# ---- Hard code NEITHER unique-to-data metrics----
summarystats_raw[summarystats_raw$metric == "Number of distinct WCFP species unique to data", "NEITHER_genebanks_or_botanic_gardens"] <- "6,296"
summarystats_raw[summarystats_raw$metric == "Percent of total distinct WCFP species unique to data", "NEITHER_genebanks_or_botanic_gardens"] <- "23.6%"

# ---- records counts by species, accessions, etc. ----
combined <- bind_rows(
  genebank_accessionlevel_dataset %>% mutate(dataset = "genebanks"),
  botanicgarden_accessionlevel_dataset %>% mutate(dataset = "botanic_gardens_accession"),
  botanicgarden_specieslevel_dataset %>% mutate(dataset = "botanic_gardens_species")
)
records_by_species_long <- combined %>%
  filter(dataset %in% c("genebanks", "botanic_gardens_accession", "botanic_gardens_species")) %>%
  group_by(WCFP_species = wcfp_name_match, dataset) %>%
  summarise(records = n(), .groups = "drop")
records_species <- records_by_species_long %>%
  mutate(dataset = ifelse(dataset %in% c("botanic_gardens_accession", "botanic_gardens_species"), "botanic_gardens", dataset)) %>%
  group_by(WCFP_species, dataset) %>%
  summarise(records = sum(records), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = dataset, values_from = records, values_fill = 0) %>%
  rename(
    number_of_records_in_genebanks = genebanks,
    number_of_records_in_botanicgardens = botanic_gardens
  ) %>%
  arrange(desc(as.numeric(gsub(",", "", number_of_records_in_genebanks)))) %>%
  mutate(
    number_of_records_in_genebanks = scales::comma(number_of_records_in_genebanks),
    number_of_records_in_botanicgardens = scales::comma(number_of_records_in_botanicgardens)
  )
accession_species <- combined %>%
  filter(dataset %in% c("genebanks", "botanic_gardens_accession")) %>%
  group_by(WCFP_species = wcfp_name_match, dataset) %>%
  summarise(accessions = n(), .groups = "drop") %>%
  pivot_wider(names_from = dataset,
              values_from = accessions,
              values_fill = 0) %>%
  rename(
    number_of_accessions_in_genebanks = genebanks,
    number_of_accessions_in_botanicgardens = botanic_gardens_accession
  ) %>%
  arrange(desc(as.numeric(gsub(",", "", number_of_accessions_in_genebanks)))) %>%
  mutate(across(where(is.numeric), scales::comma))
genebank_species <- genebank_accessionlevel_dataset %>%
  group_by(WCFP_species = wcfp_name_match) %>%
  summarise(number_of_accessions_in_genebanks = n(), .groups = "drop") %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  mutate(number_of_accessions_in_genebanks = scales::comma(number_of_accessions_in_genebanks))
botanic_garden_species <- bind_rows(
  botanicgarden_accessionlevel_dataset,
  botanicgarden_specieslevel_dataset
) %>%
  group_by(WCFP_species = wcfp_name_match) %>%
  summarise(number_of_records_in_botanicgardens = n(), .groups = "drop") %>%
  arrange(desc(number_of_records_in_botanicgardens)) %>%
  mutate(number_of_records_in_botanicgardens = scales::comma(number_of_records_in_botanicgardens))
species_only_in_genebanks <- genebank_accessionlevel_dataset %>%
  filter(!wcfp_name_match %in% all_bg) %>%
  group_by(WCFP_species = wcfp_name_match) %>%
  summarise(number_of_records_in_genebanks = n(), .groups = "drop") %>%
  arrange(desc(number_of_records_in_genebanks)) %>%
  mutate(number_of_records_in_genebanks = scales::comma(number_of_records_in_genebanks))
species_only_in_botanic_gardens <- bind_rows(
  botanicgarden_accessionlevel_dataset,
  botanicgarden_specieslevel_dataset
) %>%
  filter(!wcfp_name_match %in% all_gb) %>%
  group_by(WCFP_species = wcfp_name_match) %>%
  summarise(number_of_records_in_botanicgardens = n(), .groups = "drop") %>%
  arrange(desc(number_of_records_in_botanicgardens)) %>%
  mutate(number_of_records_in_botanicgardens = scales::comma(number_of_records_in_botanicgardens))
wcfp_species_not_in_either <- WCFP_plantlist_names %>%
  filter(!taxon_name_accepted %in% unique(c(all_gb, all_bg))) %>%
  rename(WCFP_species = taxon_name_accepted)

# ---- 7. Institution summary of record count Excel ----

institution_locations <- read_excel("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Input_files_script4_2026-06-02/all_organizations_dataset.xlsx") %>%
  rename(
    institution_name = organization_name,
    institution_type = organization_type) %>%
  select(
    institution_directory,
    inst_code,
    institution_name,
    institution_type)
institution_locations$inst_code <- as.character(institution_locations$inst_code)

bgci_counts <- bgci %>%
  group_by(inst_code) %>%
  summarise(record_count_BGCI = n(), .groups = "drop")
genesys_counts <- rbind(genesys_gb, genesys_bg) %>%
  group_by(inst_code) %>%
  summarise(accession_count_Genesys = n(), .groups = "drop")
wiews_counts <- rbind(wiews_gb, wiews_bg) %>%
  group_by(inst_code) %>%
  summarise(accession_count_WIEWS = n(), .groups = "drop")
cano_counts <- cano %>%
  group_by(inst_code) %>%
  summarise(accession_count_Cano = n(), .groups="drop")
gbif_counts <- rbind(gbif_living_gb, gbif_living_bg) %>%
  group_by(inst_code) %>%
  summarise(accession_count_GBIF_living = n(), .groups="drop")
institutions_summary <- institution_locations %>%
  left_join(bgci_counts,         by = "inst_code") %>%
  left_join(genesys_counts,      by = "inst_code") %>%
  left_join(wiews_counts,        by = "inst_code") %>%
  left_join(cano_counts,         by = "inst_code") %>%
  left_join(gbif_counts,         by = "inst_code") %>%
  mutate(
    institution_found_in_data = if_else(
      !is.na(record_count_BGCI) |
        !is.na(accession_count_Genesys) |
        !is.na(accession_count_WIEWS) |
        !is.na(accession_count_Cano) |
        !is.na(accession_count_GBIF_living),
      "Y", ""
    )
  ) %>%
  select(institution_directory, inst_code, institution_name, institution_type, institution_found_in_data, everything())
cols_to_comma <- c("record_count_BGCI", "accession_count_Genesys", "accession_count_WIEWS", "accession_count_Cano", "accession_count_GBIF_living")
for (col in cols_to_comma) {
  if (col %in% names(institutions_summary)) {
    institutions_summary[[col]] <- safe_comma(institutions_summary[[col]])
  }
}

# ---- 8. Write Excel files with formatting using openxlsx ----
write_smart_xlsx <- function(tabs, filename) {
  wb <- createWorkbook()
  for(sheet in names(tabs)) {
    addWorksheet(wb, sheet)
    dat <- tabs[[sheet]]
    writeData(wb, sheet, dat,
              headerStyle= createStyle(halign="left", textDecoration = "bold", wrapText = TRUE))
    is_numeric_col <- function(x) all(grepl("^(\\-?\\d{1,3}(,\\d{3})*|NA|^\\s*$)$", as.character(x)))
    is_percent_col <- function(x) all(grepl("^(\\-?\\d{1,3}(,\\d{3})*\\.\\d+%|NA|^\\s*$)", as.character(x)))
    right_cols <- which(sapply(dat, function(x) is_numeric_col(x) | is_percent_col(x)))
    for(i in seq_along(dat)) {
      col_align <- if (i %in% right_cols) "right" else "left"
      addStyle(wb, sheet, style=createStyle(halign = col_align), rows = 2:(nrow(dat)+1), cols = i, gridExpand = TRUE, stack=TRUE)
      addStyle(wb, sheet, style=createStyle(halign="left", wrapText = TRUE, textDecoration = "bold"), rows=1, cols=i, gridExpand=TRUE, stack=TRUE)
      vals <- as.character(dat[[i]])
      max_body <- if(length(vals) > 0) max(nchar(vals), na.rm=TRUE) else 8
      width <- max(8, max_body + 2)
      setColWidths(wb, sheet, cols = i, widths = width)
    }
  }
  saveWorkbook(wb, file = filename, overwrite = TRUE)
}

# ---- 9. Output Excel files ----
write_smart_xlsx(
  list(
    "summary stats"                     = summarystats_raw,
    "records counts by species"          = records_species,
    "accession_counts_by_species"        = accession_species,
    "genebank_species"                  = genebank_species,
    "botanic_garden_species"            = botanic_garden_species,
    "species_only_in_genebanks"         = species_only_in_genebanks,
    "species_only_in_botanic_gardens"   = species_only_in_botanic_gardens,
    "wcfp_species_not_in_either"        = wcfp_species_not_in_either
  ),
  metrics_output_file
)
cat("Excel file created:", metrics_output_file, "\n")

write_smart_xlsx(
  list(institutions_summary = institutions_summary),
  institutions_output_file
)
cat("Output written to", institutions_output_file, "\n")


## end script ##
