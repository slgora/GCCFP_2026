################################################################################                                                                             
#    Duplicate removal by selection of data source with greatest number of    #
#    records per instcode (1 data source per inst_code)                       #
#                                                                             #
#    Deduplication Method:                                                    #
#    - For each organization (inst_code), only ONE data source is kept:       #
#      whichever source has greatest number of records for that inst_code.    #
#    - Duplicates across Genesys, WIEWS, BGCI, Cano, and GBIF-living are      #
#      removed by this single-source-per-institution rule.                    #
#                                                                             #                                                                   
#      Harmonize datasets and prep data (ei: inst_code field as character)    #
#                                                                             #
#      DEDUPLICATION STEPS:                                                   #
#         - Step 1: Remove duplicates in Genesys/WIEWS by data source         #
#                   for each inst_code keep only source with most records     #
#                   proritizing Genesys for CGIAR institions                  #
#         - Step 2: Remove duplicates in Genesys/WIEWS by DOI                 #
#                   prioritizing records with lat/long, otherwise-Genesys     #
#         - Step 3: Remove duplicates in Genesys/WIEWS by ID                  #
#                   ID = acce_numb + inst_code +genus_species                 #
#                   prioritizing records with lat/long, otherwise-Genesys     #
#         - Step 4: Remove duplicates across all data sources                 #
#                   selecting data source with greatest number of records     #
#                   proritizing Genesys for CGIAR institions                  #
#        ASSIGN ORG TYPE:                                                     #
#         - Step 5: Assign org type                                           #
#                   filter for Genebank/Botanic garden only                   #
#                                                                             # 
#      Outputs:                                                               #
#       Deduplication summary tables for review                               #
#       Deduplicated datasets with org type assigned                          #
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

### RAW DATASETS ###
# Genesys data: 3,618,693 rows
genesys_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GENESYS_data_prepped_2026-03-05.csv")
# WIEWS data: 3,236,562 rows
wiews_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/WIEWS_data_prepped_2026-03-05.csv")
# BGCI data: 626,307 rows
bgci_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/BGCI_data_prepped_2026-03-09.csv")
# Cano data: 391,214 rows
cano_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/Cano_data_prepped_2026-03-09.csv")
# GBIF living data: 118,198 rows
gbif_living_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_living_2026-05-28.csv")

### GUIDE FILES ###
# guide file to assign instcode to LC field in Cano
cano_assign_INSTCODE_to_LC <- read_excel("C:/Users/sarah/Downloads/cano_assign_INSTCODE_to_LC.xlsx")
# guide file to correct invalid/outdated instcodes
instcode_corrections <- read_excel("C:/Users/sarah/Downloads/invalid_instcodes.xlsx")
# guide file of all organizations in FAO WIEWS and BGCI GardenSearch institution databases
org_guide <- read_excel("C:/Users/sarah/Downloads/all_organizatons_final.xlsx")
# guide file with wiews org type (CGIAR, etc) assignment for selection data sources
institute_names_no_syn <- read_excel("C:/Users/sarah/Downloads/FAO_WIEWS_organizations_PG_2026.xlsx")
institute_names_no_syn <- subset(institute_names_no_syn, select = c(inst_code, wiews_org_type))  %>% drop_na()
CGIAR_instcodes <- institute_names_no_syn %>% filter(wiews_org_type == "CGIAR")
# guide file of EURISCO instcodes assignment for selection data sources review
eurisco_path <- "C:/Users/sarah/Downloads/eurisco_inst_code_list.xlsx")
# guide file to assign instcodes to GBIF
gbif_assign_INSTCODE_to_ID <- read_excel("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Guide_files/gbif_corrected_map.xlsx")


# ---- 1. Prep data ----
# Assign inst_code to LC field for Cano
cano_assign_INSTCODE_to_LC <- cano_assign_INSTCODE_to_LC %>%
  select(-data_source,
         -organization_type)
cano_df <- cano_df %>% right_join(cano_assign_INSTCODE_to_LC, by = "LC")

# Assign inst_code by inst_ID_GBIF field for GBIF
# gbif_living_df <- gbif_living_df %>%
  left_join(gbif_assign_INSTCODE_to_ID, by = "inst_ID_GBIF")

# ---- 2. RENAME fields before type coercion ----
genesys_df <- genesys_df %>%
  rename(
    latitude = LATITUDE,
    longitude = LONGITUDE,
    inst_code = INSTCODE)
wiews_df <- wiews_df %>%
  rename(
    latitude = LATITUDE,
    longitude = LONGITUDE,
    inst_code = INSTCODE)
bgci_df <- bgci_df %>%
  rename(
    inst_code = ex_situ_garden_id)
gbif_living_df <- gbif_living_df %>%
  rename(
    inst_code = inst_code_WIEWS2)

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

## Row counts before deduplication:
# Genesys: 3,618,693 rows
# WIEWS: 3,236,562 rows
# Cano: 391,214 rows
# GBIF_living: 118,198 rows
# BGCI: 626,307 rows


# ----------------------------- Deduplication Workflow  ------------------------#

# ----------------------------------------------------------------------------- #
# ----------- STEP 1: REMOVE DUPLICATES IN GENESYS/WIEWS BY DATA SOURCE ------- #
# ----------------------------------------------------------------------------- #
#           - Remove duplicates in Genesys/WIEWS data by selecting to keep data source with greatest number of records
#           - Create selection data sources table for Genesys/WIEWS
#           - For each inst_code keep only source with most records
#           - Proritizes keeping Genesys for CGIAR institions

# source function
source("Functions/select_data_source.R")

selection_data_sources_gen_wiews <- select_data_source(
  genesys_df = genesys_df,
  wiews_df = wiews_df,
  institute_names_no_syn = institute_names_no_syn,
  eurisco_path = "C:/Users/sarah/Downloads/eurisco_inst_code_list.xlsx")

#save selection data sources table
write.csv(selection_data_sources_gen_wiews, "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Guide_files/selection_data_sources_gen_wiews_2026-05-28.csv", row.names = FALSE)

# Use selection_data_sources to filter genesys and wiews data
genesys_keep_inst <- selection_data_sources_gen_wiews %>% filter(keep == "Genesys") %>% pull(inst_code)
wiews_keep_inst   <- selection_data_sources_gen_wiews %>% filter(keep == "WIEWS") %>% pull(inst_code)
genesys_df <- genesys_df %>% filter(inst_code %in% genesys_keep_inst)
wiews_df <- wiews_df %>% filter(inst_code %in% wiews_keep_inst)

# combine GEN/WIEWS:
gen_wiews_df <- bind_rows(genesys_df, wiews_df)

## RESULT: 4,724,770 total rows in gen_wiews_df
# Genesys: 3,490,941 rows
# WIEWS: 1,233,829 rows
# 127,752 duplicates removed from Genesys
# 2,002,733 duplicates removed from WIEWS


# -----------------------------------------------------------------------------#
# ------------- STEP 2: REMOVE DUPLICATES IN GENESYS/WIEWS BY DOI -------------#
# -----------------------------------------------------------------------------#
#      - Prioritizing records with lat/long, otherwise-Genesys
#      - also removes within-source duplication

# STEP 2: Remove duplicate DOI, keep record with lat/long, else keep Genesys
gen_wiews_df <- gen_wiews_df %>%
  group_by(DOI) %>%
  mutate(
    has_latlong = !is.na(latitude) & latitude != "" & !is.na(longitude) & longitude != "",
    is_genesys = data_source == "Genesys") %>%
  mutate(
    pick_row = if (any(has_latlong)) {
      has_latlong
    } else if (any(is_genesys)) {
      is_genesys
    } else {
      row_number() == 1
    }
  ) %>%
  filter(
    is.na(DOI) | DOI == "" | pick_row) %>%
  ungroup() %>%
  select(-has_latlong, -is_genesys, -pick_row)

## RESULT: 4,717,999 total rows in gen_wiews_df
# Genesys: 3,490,941 rows
# WIEWs: 1,227,058 rows
# 0 duplicates removed from Genesys
# 6,771 duplicates removed from WIEWS


# -----------------------------------------------------------------------------#
# ------------- STEP 3: REMOVE DUPLICATES IN GENESYS/WIEWS BY ID --------------#
# -----------------------------------------------------------------------------#
#      - Create a unique accession ID for deduplication and merging:
#      - ID = ACCENUMB + inst_code + genus_species_WFO (standardized genus + species to WFO)
#      - Prioritizing records with lat/long, otherwise-Genesys
#      - also removes within-source duplication

# Clean fields and create unique ID
gen_wiews_df$ACCENUMB          <- trimws(gen_wiews_df$ACCENUMB) # Trim whitespace
gen_wiews_df$inst_code         <- trimws(gen_wiews_df$inst_code)
gen_wiews_df$genus_species_WFO   <- trimws(gen_wiews_df$genus_species_WFO)

# Create unique ID: concatenate ACCENUMB, inst_code, genus_species_WFO (no separator)
gen_wiews_df$ID <- paste0(
  gen_wiews_df$ACCENUMB,
  gen_wiews_df$inst_code,
  gen_wiews_df$genus_species_WFO)

# Remove all spaces that may exist in the new ID
# The resulting 'ID' uniquely identifies each accession
gen_wiews_df <- gen_wiews_df %>% mutate(ID = str_replace_all(ID, " ", ""))

# STEP 3: Remove duplicate IDs, keep record with lat/long, else keep Genesys
gen_wiews_df <- gen_wiews_df %>%
  group_by(ID) %>%
  mutate(
    has_latlong = !is.na(latitude) & latitude != "" & !is.na(longitude) & longitude != "",
    is_genesys = data_source == "Genesys",
    pick_row = if (any(has_latlong)) {
      has_latlong
    } else if (any(is_genesys)) {
      is_genesys
    } else {
      row_number() == 1
    }
  ) %>%
  filter(
    is.na(ID) | ID == "" | pick_row) %>%
  ungroup() %>%
  select(-has_latlong, -is_genesys, -pick_row, -ID)

## RESULT: 4,714,578 total rows in gen_wiews_df
# Genesys: 3,487,567 rows
# WIEWS: 1,227,011 rows
# 3,374 duplicates removed from Genesys
# 47 duplicates removed from WIEWS


# ---------------------------------------------------------------------------------------------#
# ------------- STEP 4: REMOVE DUPLICATES BY DATA SOURCE, ACROSS ALL DATA SOURCE --------------#
# ---------------------------------------------------------------------------------------------#
#       - Select data source with most records per inst_code, across all data sources
#       - One inst_code per data source
#       - Still select to keep CGIAR institutes from Genesys
#       - All else, select data source with greatest number of records

# STEP 4: Select data source with most records per inst_code
# Make sure all inst_code columns are characters
genesys_df <- gen_wiews_df %>% filter(data_source == "Genesys")
wiews_df   <- gen_wiews_df %>% filter(data_source == "WIEWS")
genesys_df$inst_code      <- as.character(genesys_df$inst_code)
wiews_df$inst_code        <- as.character(wiews_df$inst_code)
cano_df$inst_code         <- as.character(cano_df$inst_code)
gbif_living_df$inst_code  <- as.character(gbif_living_df$inst_code)
bgci_df$inst_code         <- as.character(bgci_df$inst_code)
CGIAR_instcodes$inst_code <- as.character(CGIAR_instcodes$inst_code)

# Prep CGIAR inst_codes list
cg_institutes <- CGIAR_instcodes$inst_code

# Combine all data with a source identifier
all_df <- bind_rows(
  genesys_df      %>% mutate(source = "Genesys"),
  wiews_df        %>% mutate(source = "WIEWS"),
  cano_df         %>% mutate(source = "Cano"),
  gbif_living_df  %>% mutate(source = "GBIF_living"),
  bgci_df         %>% mutate(source = "BGCI"))

# Count records per inst_code per source
counts <- all_df %>%
  group_by(inst_code, source) %>%
  summarise(records = n(), .groups = "drop")

# Pivot to wide format and annotate CGIAR status and Genesys presence
keep_colnames <- c("Genesys_records", "WIEWS_records", "Cano_records", "GBIF_living_records", "BGCI_records")
wide_counts <- counts %>%
  tidyr::pivot_wider(
    names_from = source,
    values_from = records,
    values_fill = 0,
    names_glue = "{source}_records") %>%
  mutate(
    is_CGIAR = inst_code %in% cg_institutes,
    Genesys_present = Genesys_records > 0)

# Logic for keep column
wide_counts <- wide_counts %>%
  mutate(
    keep = dplyr::case_when(
      is_CGIAR & Genesys_present ~ "Genesys_records",
      TRUE ~ keep_colnames[max.col(select(., all_of(keep_colnames)), ties.method = "first")] ))

# Clean up for saving and review
selection_data_sources_all_save <- wide_counts %>%
  mutate(keep = case_when(
    keep == "Genesys_records" ~ "Genesys",
    keep == "WIEWS_records" ~ "WIEWS",
    keep == "Cano_records" ~ "Cano",
    keep == "GBIF_living_records" ~ "GBIF_living",
    keep == "BGCI_records" ~ "BGCI",
    TRUE ~ keep))
# Save the selection table
write.csv(selection_data_sources_all_save, "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Guide_files/selection_data_sources_ALL_2026-05-28.csv", row.names = FALSE)

# Get inst_codes flagged to 'keep'
genesys_keep_inst      <- wide_counts %>% filter(keep == "Genesys_records") %>% pull(inst_code)
wiews_keep_inst        <- wide_counts %>% filter(keep == "WIEWS_records") %>% pull(inst_code)
cano_keep_inst         <- wide_counts %>% filter(keep == "Cano_records") %>% pull(inst_code)
gbif_living_keep_inst  <- wide_counts %>% filter(keep == "GBIF_living_records") %>% pull(inst_code)
bgci_keep_inst         <- wide_counts %>% filter(keep == "BGCI_records") %>% pull(inst_code)

# Filter each dataset by kept inst_codes
genesys_df      <- genesys_df      %>% filter(inst_code %in% genesys_keep_inst)
wiews_df        <- wiews_df        %>% filter(inst_code %in% wiews_keep_inst)
cano_df         <- cano_df         %>% filter(inst_code %in% cano_keep_inst)
gbif_living_df  <- gbif_living_df  %>% filter(inst_code %in% gbif_living_keep_inst)
bgci_df         <- bgci_df         %>% filter(inst_code %in% bgci_keep_inst)

# Combine all dataset
combined_df <- bind_rows(
  genesys_df,
  wiews_df,
  cano_df,
  gbif_living_df,
  bgci_df)

## RESULT: 5,734,760 total rows in all data sources
# Genesys: 3,477,443 rows
# WIEWS: 1,226,927 rows 
# Cano: 374,409 rows 
# GBIF_living: 87,947 rows
# BGCI: 568,034 rows
# 10,124 duplicates removed from Genesys
# 84 duplicates removed from WIEWS
# 16,805 duplicates removed from Cano
# 30,251 duplicates removed from GBIF_living
# 58,273 duplicates removed from BGCI


# ----------------------------------------------------------------------------------------------------#
# ------------- STEP 5: ASSIGN ORG TYPE, REMOVE NON-GENEBANK/NON-BOTANIC GARDEN RECORDS --------------#
# ----------------------------------------------------------------------------------------------------#
#       - Assign organization type (genebank, botanic garden) across all datasets
#       - Records flagged as non-genebank, non-botanic garden OR no organization type assigned
#       - Genebanks removed from BGCI data

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
      TRUE ~ organization_type)) %>%
  filter(!(data_source == "BGCI" & inst_code %in% c("4678", "4606"))) %>%    #Genebanks removed from BGCI
  left_join(
    org_guide %>% select(inst_code, organization_type) %>% filter(!is.na(inst_code)),
    by = "inst_code", suffix = c("", ".org_guide")) %>%
  mutate(
    organization_type = ifelse(
      !(data_source %in% c("BGCI", "Cano")) & !is.na(organization_type.org_guide),
      organization_type.org_guide,
      organization_type)) %>%
  select(-organization_type.org_guide) %>%
  filter(
    !is.na(organization_type),
    organization_type %in% c("Genebank", "Botanic garden"))

## RESULT: 5,708,604 total rows in all data sources
# Genesys: 3,474,330 rows
# WIEWS: 1,226,927 rows
# Cano: 374,409 rows
# GBIF-living: 72,667 rows
# BGCI: 560,271 rows
# 3,113 non-genebank/non botanic garden rows removed from Genesys
# 0 non-genebank/non botanic garden rows removed from WIEWS
# 0 non-genebank/non botanic garden rows removed from Cano
# 15,280 non-genebank/non botanic garden rows removed from GBIF-living        *** Confirmed rows removed, ok
# 7,763 non-genebank/non botanic garden rows removed from BGCI


# ----------------------------------------------------------------------------------------------------#
# ------------- SAVE FINAL DEDUPLICATED DATASETS WTIH ORG TYPE ASSIGNED --------------#
# ----------------------------------------------------------------------------------------------------#

### Genesys/WIEWS data: 4,701,257 rows
# Genesys data: 3,474,330 rows
# WIEWS data: 1,226,927 rows
genesys_df <- combined_df %>% filter(data_source == "Genesys")
wiews_df <- combined_df %>% filter(data_source == "WIEWS")
gen_wiews_df <- bind_rows(genesys_df, wiews_df)
#save
write.csv(gen_wiews_df, 
          file = "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/gen_wiews_data_dedup_2026_05_28.csv",
          row.names = FALSE)

### BGCI data: 560,271 rows
bgci_df <- combined_df %>% filter(data_source == "BGCI")
#save
write.csv(bgci_df, 
          file = "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/bgci_data_dedup_2026_05_28.csv",
          row.names = FALSE)

# Cano data: 374,409 rows
cano_df <- combined_df %>% filter(data_source == "Cano")
#save
write.csv(cano_df, 
          file = "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/cano_data_dedup_2026_05_28.csv",
          row.names = FALSE)

# GBIF living data: 72,667 rows
# GBIF-living genebank: 21,461 rows
# GBIF-living botanic garden: 51,206 rows
gbif_living_df <- combined_df %>% filter(data_source == "GBIF_living")
gbif_living_genebank_df <- gbif_living_df %>% filter(organization_type_FINAL == "Genebank")
gbif_living_botanic_garden_df <- gbif_living_df %>% filter(organization_type_FINAL == "Botanic garden")
#save GBIF-living genebank
write.csv(gbif_living_genebank_df, 
          file = "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/gbif_living_genebank_data_dedup_2026_05_28.csv",
          row.names = FALSE)
#save GBIF-living botanic garden
write.csv(gbif_living_botanic_garden_df, 
          file = "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/gbif_living_botanicgarden_data_dedup_2026_05_28.csv",
          row.names = FALSE)


# GBIF observations data not processed in this script but saved to same folder location (data not yet filtered to removed missing or invalid coords)
# GBIF observations: 6,701,782 rows
gbif_observations_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_observations_2026-05-28.csv")
#save to same location
write.csv(gbif_observations_df, 
          file = "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/gbif_observations_data_2026_05_28.csv",
          row.names = FALSE)


# end script #
