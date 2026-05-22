###############################################################################
#                                                                             #
#    PETER G DEDUPLICATION METHODOLOGY VERSION 1 (V1)                         #
#    Duplicate removal by selection of data source with greatest number of    #
#    records per instcode (1 data source per instcode))                       #
#                                                                             #
#    Deduplication Method:                                                    #
#    - For each organization (inst_code), only ONE data source is kept:       #
#      whichever source has greatest number of records for that inst_code.    #
#    - Duplicates across Genesys, WIEWS, BGCI, Cano, and GBIF-living are      #
#      removed by this single-source-per-institution rule.                    #
#                                                                             #                                                                   #
#      Harmonize datasets Aand prep data (ei: inst_code field as character)   #
#                                                                             #
#      DEDUPLICATION STEPS:                                                   #
#         - Step 1: For each inst_code, keep only the source with most rows   #
#         - Step 2: Remove duplicates by DOI, prioritizing Genesys            #
#         - Step 3: Remove duplicates by composite ID                         #
#         - Step 4: After merging back in non-Genesys/WIEWS data, repeat      #
#         - Step 5: Assign org type; filter for Genebank/BG only              #
#                                                                             #
#      Annotate and summarize all actions taken in wide/per-species format    #
#      Output deduplication summary                                           #
#                                                                             #
###############################################################################

# ---- 0. Load required libraries ---------------------------------------------#
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(purrr)
library(stringr)

# ---- 1. Read in data ----

# Genesys data: 3,618,693 rows
genesys_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GENESYS_data_prepped_2026-03-05.csv")
# WIEWS data: 3,236,562 rows
wiews_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/WIEWS_data_prepped_2026-03-05.csv")
# BGCI data: 626,307 rows
bgci_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/BGCI_data_prepped_2026-03-09.csv")
# Cano data: 391,214 rows
cano_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/Cano_data_prepped_2026-03-09.csv")
# GBIF living data: 118,198 rows
gbif_living_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_living_2026-03-11.csv")

# ---- 2. RENAME fields before type coercion ----
genesys_df <- genesys_df %>%
  rename(
    latitude = LATITUDE,
    longitude = LONGITUDE,
    inst_code = INSTCODE
  )
wiews_df <- wiews_df %>%
  rename(
    latitude = LATITUDE,
    longitude = LONGITUDE,
    inst_code = INSTCODE
  )
bgci_df <- bgci_df %>%
  rename(
    inst_code = ex_situ_garden_id
  )

# -- Type coercion as character for inst_code --
genesys_df$inst_code      <- as.character(genesys_df$inst_code)
wiews_df$inst_code        <- as.character(wiews_df$inst_code)
bgci_df$inst_code         <- as.character(bgci_df$inst_code)
cano_df$inst_code         <- as.character(cano_df$inst_code)
gbif_living_df$inst_code  <- as.character(gbif_living_df$inst_code)

# ---- 3. Correct invalid inst_codes ----
instcode_corrections$invalid_inst_code <- trimws(instcode_corrections$invalid_inst_code)
instcode_corrections$valid_inst_code   <- trimws(instcode_corrections$valid_inst_code)
fix_inst_code <- function(df) {
  df$inst_code <- trimws(df$inst_code)
  df <- df %>%
    left_join(instcode_corrections, by = c("inst_code" = "invalid_inst_code")) %>%
    mutate(inst_code = if_else(!is.na(valid_inst_code), valid_inst_code, inst_code)) %>%
    select(-valid_inst_code)
  df
}
genesys_df <- fix_inst_code(genesys_df)
wiews_df   <- fix_inst_code(wiews_df)
cano_df    <- fix_inst_code(cano_df)
gbif_living_df <- fix_inst_code(gbif_living_df)

# ---- Helper: species+source+step summary ----
per_source_summary <- function(df, step_label, sources = c("Genesys","WIEWS","BGCI","Cano","GBIF_living"),
                               species_col = "WCFP_name_match") {
  bysrc <- df %>%
    group_by(across(all_of(species_col)), data_source) %>%
    summarize(
      total_records = n(),
      records_with_coords = sum(!is.na(latitude) & latitude != "" & !is.na(longitude) & longitude != ""),
      .groups = "drop"
    ) %>%
    mutate(data_source = factor(data_source, levels = sources)) %>%
    mutate(step = step_label)
  wide <- tidyr::pivot_wider(
    bysrc,
    id_cols = all_of(species_col),
    names_from = c("data_source", "step"),
    values_from = c("total_records", "records_with_coords"),
    names_glue = "{.value}_{data_source}_{step}"
  )
  if (!species_col %in% names(wide)) wide[[species_col]] <- NA
  wide
}

# ---- Custom deduplication annotation ----
dup_annot <- function(before_df, after_df, step_label) {
  coords_before <- before_df %>%
    group_by(data_source) %>%
    summarize(coords_before = sum(!is.na(latitude) & latitude != "" & !is.na(longitude) & longitude != ""), .groups="drop")
  coords_after <- after_df %>%
    group_by(data_source) %>%
    summarize(coords_after = sum(!is.na(latitude) & latitude != "" & !is.na(longitude) & longitude != ""), .groups="drop")
  coords_join <- full_join(coords_before, coords_after, by="data_source") %>%
    mutate(coords_before = ifelse(is.na(coords_before), 0L, coords_before),
           coords_after = ifelse(is.na(coords_after), 0L, coords_after),
           coords_removed = coords_before - coords_after)
  counts_before <- before_df %>% count(data_source, name = "before")
  counts_after  <- after_df  %>% count(data_source, name = "after")
  counts_join   <- full_join(counts_before, counts_after, by = "data_source") %>%
    mutate(before = ifelse(is.na(before), 0L, before),
           after = ifelse(is.na(after), 0L, after),
           removed = before - after)
  annot <- list(
    label = step_label,
    per_source = counts_join %>%
      left_join(coords_join, by="data_source") %>%
      select(data_source, before, after, removed, coords_before, coords_after, coords_removed)
  )
  return(annot)
}
dup_annotations <- list()

# ---- Deduplication Workflow  ----
species_summary_list <- list()
species_summary_list[['raw']] <- per_source_summary(
  bind_rows(genesys_df, wiews_df, bgci_df, cano_df, gbif_living_df), "raw")

# STEP 1: Genesys or WIEWS by record count
genesys_counts <- genesys_df %>% group_by(inst_code) %>% summarize(n_genesys = n())
wiews_counts <- wiews_df %>% group_by(inst_code) %>% summarize(n_wiews = n())
instcode_counts <- full_join(genesys_counts, wiews_counts, by = "inst_code") %>%
  mutate(
    n_genesys = ifelse(is.na(n_genesys), 0L, n_genesys),
    n_wiews   = ifelse(is.na(n_wiews),   0L, n_wiews),
    keep_source = case_when(
      n_genesys >= n_wiews ~ "Genesys",
      n_genesys <  n_wiews ~ "WIEWS"
    )
  )
gen_wiews_df_step0 <- bind_rows(genesys_df, wiews_df)
gen_wiews_df <- bind_rows(
  genesys_df %>% filter(inst_code %in% instcode_counts$inst_code[instcode_counts$keep_source=="Genesys"]),
  wiews_df   %>% filter(inst_code %in% instcode_counts$inst_code[instcode_counts$keep_source=="WIEWS"])
)
dup_annotations[[length(dup_annotations)+1]] <- dup_annot(gen_wiews_df_step0, gen_wiews_df, "step1_instcode_selection")
species_summary_list[['step1']] <- per_source_summary(
  bind_rows(gen_wiews_df, bgci_df, cano_df, gbif_living_df), "step1")

# -----------------------------------------------------------------------------#
# ------------------- Clean fields and Create Unique ID -----------------------#
# -----------------------------------------------------------------------------#
# This section ensures the key identifier fields are cleaned (no leading/trailing whitespace)
# and creates a unique accession ID for deduplication and merging:
#      - ID = ACCENUMB + inst_code + WCFP_name_match
#      - Spaces within the concatenated ID string are entirely removed

# Trim whitespace in key fields for consistency
gen_wiews_df$ACCENUMB          <- trimws(gen_wiews_df$ACCENUMB)
gen_wiews_df$inst_code         <- trimws(gen_wiews_df$inst_code)
gen_wiews_df$WCFP_name_match   <- trimws(gen_wiews_df$WCFP_name_match)

# Create unique ID: concatenate ACCENUMB, inst_code, WCFP_name_match (no separator)
gen_wiews_df$ID <- paste0(
  gen_wiews_df$ACCENUMB,
  gen_wiews_df$inst_code,
  gen_wiews_df$WCFP_name_match
)

# Remove all spaces that may exist in the new ID
gen_wiews_df <- gen_wiews_df %>%
  mutate(ID = str_replace_all(ID, " ", ""))

# The resulting 'ID' uniquely identifies each accession for downstream duplicate handling.

# STEP 2: Remove duplicate DOI
gen_wiews_df_step1 <- gen_wiews_df
gen_wiews_df <- gen_wiews_df %>%
  arrange(DOI, desc(data_source == "Genesys")) %>%
  group_by(DOI) %>%
  mutate(rowno = row_number()) %>%
  filter(is.na(DOI) | DOI == "" | rowno == 1) %>%
  ungroup() %>%
  select(-rowno)
dup_annotations[[length(dup_annotations)+1]] <- dup_annot(gen_wiews_df_step1, gen_wiews_df, "step2_doi_dedup")
species_summary_list[['step2']] <- per_source_summary(
  bind_rows(gen_wiews_df, bgci_df, cano_df, gbif_living_df), "step2")

# STEP 3: Remove duplicate IDs
# (ID is already constructed and cleaned above)
gen_wiews_df <- gen_wiews_df %>%
  arrange(ID, desc(data_source == "Genesys")) %>%
  group_by(ID) %>%
  mutate(rowno = row_number()) %>%
  filter(is.na(ID) | ID == "" | rowno == 1) %>%
  ungroup() %>%
  select(-rowno, -ID)
dup_annotations[[length(dup_annotations)+1]] <- dup_annot(gen_wiews_df_step1, gen_wiews_df, "step3_id_dedup")
species_summary_list[['step3']] <- per_source_summary(
  bind_rows(gen_wiews_df, bgci_df, cano_df, gbif_living_df), "step3")

# STEP 4: Most records per inst_code
genesys_df <- gen_wiews_df %>% filter(data_source == "Genesys")
wiews_df   <- gen_wiews_df %>% filter(data_source == "WIEWS")
genesys_df$inst_code      <- as.character(genesys_df$inst_code)
wiews_df$inst_code        <- as.character(wiews_df$inst_code)
bgci_df$inst_code         <- as.character(bgci_df$inst_code)
cano_df$inst_code         <- as.character(cano_df$inst_code)
gbif_living_df$inst_code  <- as.character(gbif_living_df$inst_code)
combined_df_step3 <- bind_rows(genesys_df, wiews_df, bgci_df, cano_df, gbif_living_df)
instcode_src_counts <- combined_df_step3 %>%
  group_by(inst_code, data_source) %>%
  summarize(n = n(), .groups = "drop")
instcode_top_source <- instcode_src_counts %>%
  group_by(inst_code) %>% arrange(desc(n)) %>% slice(1) %>%
  select(inst_code, data_source)
combined_df <- combined_df_step3 %>%
  left_join(instcode_top_source, by = c("inst_code", "data_source")) %>%
  filter(!is.na(data_source))
dup_annotations[[length(dup_annotations)+1]] <- dup_annot(combined_df_step3, combined_df, "step4_best_datasource_per_inst_code")
species_summary_list[['step4']] <- per_source_summary(combined_df, "step4")

# STEP 5: Organization assignment and filtering
org_guide <- org_guide %>%
  mutate(inst_code = trimws(inst_code),
         organization_type = trimws(organization_type))
combined_df$inst_code <- trimws(combined_df$inst_code)
combined_df <- combined_df %>%
  mutate(
    organization_type = dplyr::case_when(
      data_source == "BGCI" ~ "Botanic garden",
      data_source == "Cano" ~ "Botanic garden",
      TRUE ~ organization_type
    )
  ) %>%
  filter(!(data_source == "BGCI" & inst_code %in% c("4678", "4606"))) %>%
  left_join(
    org_guide %>% select(inst_code, organization_type) %>% filter(!is.na(inst_code)),
    by = "inst_code", suffix = c("", ".org_guide")
  ) %>%
  mutate(
    organization_type = ifelse(
      !(data_source %in% c("BGCI", "Cano")) & !is.na(organization_type.org_guide),
      organization_type.org_guide,
      organization_type
    )
  ) %>%
  select(-organization_type.org_guide) %>%
  filter(
    !is.na(organization_type),
    organization_type %in% c("Genebank", "Botanic garden")
  )
dup_annotations[[length(dup_annotations)+1]] <- dup_annot(combined_df, combined_df, "step5_genebank_or_botanicgarden")
species_summary_list[['final']] <- per_source_summary(combined_df, "final")

# ---- Combine all summaries into WIDE per species ----
final_species_summary <- reduce(
  species_summary_list, function(a, b) full_join(a, b, by = "WCFP_name_match"))
names(final_species_summary)[1] <- "species_name"
final_total_cols_records <- grep("^total_records_.*_final$", names(final_species_summary), value=TRUE)
final_total_cols_coords  <- grep("^records_with_coords_.*_final$", names(final_species_summary), value=TRUE)
final_species_summary$total_records_final <- rowSums(final_species_summary[,final_total_cols_records], na.rm=TRUE)
final_species_summary$records_with_coords_final <- rowSums(final_species_summary[,final_total_cols_coords], na.rm=TRUE)
final_species_summary <- final_species_summary %>%
  arrange(desc(total_records_final))

# ---- Create duplicate annotation table ----
dup_annot_table <- bind_rows(
  lapply(dup_annotations, function(x) {
    x$per_source %>%
      mutate(
        step = x$label
      )
  })
) %>%
  select(
    step, data_source, before, after, removed, coords_before, coords_after, coords_removed
  )

# ---- Output summary ----
output_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_2026/testing/Outputs"
write_xlsx(
  list(
    duplicate_annotations = dup_annot_table,
    species_deduplication_summary = final_species_summary
  ),
  file.path(output_dir, "deduplication_summary.xlsx")
)

# end script #