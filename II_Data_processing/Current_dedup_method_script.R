

## NOTE: This script combines data processing scripts 4 + 5 into one single 
#        deduplication workflow

# SCRIPT 4: 4_Merge_genesys_wiews_and_deduplicate.R
# path: GCCFP_2026/II_Data_processing/4_Merge_genesys_wiews_and_deduplicate.R

# SCRIPT 5: 5_Assign_org_type_and_deduplicate.R
# path: GCCFP_2026/II_Data_processing/5_Assign_org_type_and_deduplicate.R


###############################################################################
#                                                                             #
#  This workflow merges, deduplicates, and processes datasets, prioritizing   # 
#   keeping records with coordinates (lat/lon) data.                          #
#                                                                             #
#  SECTION 1: Genesys/WIEWS Deduplication                                     #
#    - Datasets: Genesys, WIEWS                                               #
#    - Standardizes, merges, and deduplicates Genesys and WIEWS accessions.   #
#    - DEDUPLICATION STEPS 1,2,3                                              #
#                                                                             #
#  SECTION 2: Assign Organization Type, Remove Institutions & Duplicates      #
#    - Datasets: Genesys, WIEWS, Cano, GBIF-living, BGCI                      #
#    - Annotates all datasets with organization type.                         #
#    - Removes flagged and non-genebank/non-botanic garden institutions.      #
#    - DEDUPLICATION STEP 4                                                   #
#                                                                             #
###############################################################################


# ================================ SECTION 1 ================================ #
#                          Genesys/WIEWS Deduplication                        #
# =========================================================================== #

# Merge Genesys and WIEWS data and deduplicate

# DEDUPLICATION STEPS (3):
# STEP 1: Handle duplicate DOIs between WIEWS and Genesys:
# - Remove WIEWS if Genesys has lat/long (or both missing lat/long)
# - Remove Genesys if WIEWS has lat/long and Genesys does not

# STEP 2: Handle duplicate IDs between WIEWS and Genesys:
# ID = ACCENUMB + INSTCODE + GENUS SPECIES (standardized)      
# - Remove WIEWS if Genesys has lat/long (or both missing lat/long)
# - Remove Genesys if WIEWS has lat/long and Genesys does not

# STEP 3: Remove duplication within data source by ID, keep rows with lat/long (if available)
# ID = ACCENUMB + INSTCODE + GENUS SPECIES (standardized)      
# - Remove row if duplicate has lat/long (or both missing lat/long)

# Read libraries
library(readr)
library(measurements)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(writexl)

# --------------------------------
# ----- READ IN GUIDE FILES -----
# --------------------------------

# Invalid INSTCODEs list with corrected INSTCODES
invalid_instcodes <- read_excel("C:/Users/sarah/Downloads/invalid_instcodes.xlsx")


# ------------------------
# ----- READ IN DATA -----
# ------------------------

# Note: Data is standardized, filtered, coords corrected, and prepped

## Genesys data: 3,618,693 rows
Genesys_allcrops <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GENESYS_data_prepped_2026-03-05.csv")

## WIEWS data: 3,236,562 rows
WIEWS_allcrops <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/WIEWS_data_prepped_2026-03-05.csv")


# --------------------------------------------------------------#
# -------- Correct invalid INSTCODES in Genesys, WIEWS ---------#

# Trim whitespace in invalid INSTCODEs list for match
invalid_instcodes$invalid_inst_code <- trimws(invalid_instcodes$invalid_inst_code)
invalid_instcodes$valid_inst_code <- trimws(invalid_instcodes$valid_inst_code)

# Trim whitespace in genesys and wiews for match
Genesys_allcrops$INSTCODE <- trimws(Genesys_allcrops$INSTCODE)
WIEWS_allcrops$INSTCODE <- trimws(WIEWS_allcrops$INSTCODE)

# Replace invalid inst_codes in Genesys_allcrops with valid_inst_code
Genesys_allcrops <- Genesys_allcrops %>%
  left_join(invalid_instcodes, by = c("INSTCODE" = "invalid_inst_code")) %>%
  mutate(
    INSTCODE = if_else(!is.na(valid_inst_code), valid_inst_code, INSTCODE)
  ) %>%
  select(-valid_inst_code)

# Replace invalid inst_codes in WIEWS_allcrops with valid_inst_code
WIEWS_allcrops <- WIEWS_allcrops %>%
  left_join(invalid_instcodes, by = c("INSTCODE" = "invalid_inst_code")) %>%
  mutate(
    INSTCODE = if_else(!is.na(valid_inst_code), valid_inst_code, INSTCODE)
  ) %>%
  select(-valid_inst_code)


# -----------------------------------------------------------------------------#
# -------------------- Combine Genesys and WIEWS data -------------------------#

# Combine genesys and wiews: 6,855,255 rows
gen_wiews_df <- bind_rows(Genesys_allcrops, WIEWS_allcrops)
# Clean fields
gen_wiews_df$ACCENUMB <- trimws(gen_wiews_df$ACCENUMB)
gen_wiews_df$INSTCODE <- trimws(gen_wiews_df$INSTCODE)
gen_wiews_df$genus_species_WFO <- trimws(gen_wiews_df$genus_species_WFO)

# Create unique ID: ACCENUMB + INSTCODE + genus_species_WFO
gen_wiews_df$ID <- paste0(gen_wiews_df$ACCENUMB, gen_wiews_df$INSTCODE, gen_wiews_df$genus_species_WFO)
# Remove blanks in ID field
gen_wiews_df  <- gen_wiews_df  %>% mutate(ID = str_replace_all(ID, " ", ""))

# CHECK for rows where ACCENUMB, INSTCODE, or genus_species_WFO is NA or blank
missing_accenumb <- gen_wiews_df[is.na(gen_wiews_df$ACCENUMB) | gen_wiews_df$ACCENUMB == "", ]
missing_INSTCODE <- gen_wiews_df[is.na(gen_wiews_df$INSTCODE) | gen_wiews_df$INSTCODE == "", ]
missing_genus_species_WFO <- gen_wiews_df[is.na(gen_wiews_df$genus_species_WFO) | gen_wiews_df$genus_species_WFO == "", ]


# -----------------------------------------------------------------------------#
# ----------- Count rows with coords before dropping duplicates ---------------#

# Genesys data with coords: 1,064,304 rows
Genesys_allcrops_with_coords <- Genesys_allcrops %>%
  filter(
    !is.na(LATITUDE) & LATITUDE != "",
    !is.na(LONGITUDE) & LONGITUDE != ""  )
nrow(Genesys_allcrops_with_coords)

# WIEWS data with coords: 955,229 rows
WIEWS_allcrops_with_coords <- WIEWS_allcrops %>%
  filter(
    !is.na(LATITUDE) & LATITUDE != "",
    !is.na(LONGITUDE) & LONGITUDE != ""  )
nrow(WIEWS_allcrops_with_coords)

# Genesys/WIEWS data with coords: 2,019,533 rows
gen_wiews_with_coords <- gen_wiews_df %>%
  filter(
    !is.na(LATITUDE) & LATITUDE != "",
    !is.na(LONGITUDE) & LONGITUDE != ""  )
nrow(gen_wiews_with_coords)


# -----------------------------------------------------------------------------#
# ------------- Summaries before dropping duplicates --------------------------#

# 1. Count of standardized genus + INSTCODE by data_source BEFORE dropping duplicates
gen_wiews_counts1 <- gen_wiews_df %>%
  group_by(INSTCODE, standardized_genus, data_source) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
#save
write.csv(gen_wiews_counts1, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/De_dup_2026_03_06/gen_wiews_instcode_genus_counts_before_dropping_duplicates.csv', row.names = FALSE)

# 2. Count of standardized genus by data_source BEFORE dropping duplicates
gen_wiews_counts2 <- gen_wiews_df %>%
  group_by(standardized_genus, data_source) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
#save
write.csv(gen_wiews_counts2, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/De_dup_2026_03_06/gen_wiews_genus_counts_before_dropping_duplicates.csv', row.names = FALSE)



# -----------------------------------------------------------------------------#
# ------------------- DUPLICATE REMOVAL STEPS 1,2,3 --------------------------#

# Note: Remove duplicates between Genesys and WIEWS.
#       Prioritize keeping Genesys or data with coords.

# STEP 1: Handle duplicate DOIs between WIEWS and Genesys:
# - Remove WIEWS if Genesys has lat/long (or both missing lat/long)
# - Remove Genesys if WIEWS has lat/long and Genesys does not

genesys_doi_with_coords <- gen_wiews_df$DOI[
  gen_wiews_df$data_source == "Genesys" &
    !is.na(gen_wiews_df$DOI) & gen_wiews_df$DOI != "" &
    !is.na(gen_wiews_df$LATITUDE) & gen_wiews_df$LATITUDE != "" &
    !is.na(gen_wiews_df$LONGITUDE) & gen_wiews_df$LONGITUDE != ""]

genesys_doi_without_coords <- gen_wiews_df$DOI[
  gen_wiews_df$data_source == "Genesys" &
    !is.na(gen_wiews_df$DOI) & gen_wiews_df$DOI != "" &
    (is.na(gen_wiews_df$LATITUDE) | gen_wiews_df$LATITUDE == "" |
       is.na(gen_wiews_df$LONGITUDE) | gen_wiews_df$LONGITUDE == "")]

wiews_doi_with_coords <- gen_wiews_df$DOI[
  gen_wiews_df$data_source == "WIEWS" &
    !is.na(gen_wiews_df$DOI) & gen_wiews_df$DOI != "" &
    !is.na(gen_wiews_df$LATITUDE) & gen_wiews_df$LATITUDE != "" &
    !is.na(gen_wiews_df$LONGITUDE) & gen_wiews_df$LONGITUDE != ""]

gen_wiews_df <- gen_wiews_df %>%
  filter(
    !(data_source == "WIEWS" &
        !is.na(DOI) & DOI != "" &
        (DOI %in% genesys_doi_with_coords |
           (DOI %in% genesys_doi_without_coords & !DOI %in% wiews_doi_with_coords))),
    !(data_source == "Genesys" &
        !is.na(DOI) & DOI != "" &
        DOI %in% genesys_doi_without_coords &
        DOI %in% wiews_doi_with_coords))

# RESULT: 6,063,272 rows
# 791,983 rows removed in step 1


# STEP 2: Handle duplicate IDs between WIEWS and Genesys:
# ID = ACCENUMB + INSTCODE + GENUS SPECIES (standardized)      
# - Remove WIEWS if Genesys has lat/long (or both missing lat/long)
# - Remove Genesys if WIEWS has lat/long and Genesys does not

genesys_id_with_coords <- gen_wiews_df$ID[
  gen_wiews_df$data_source == "Genesys" &
    !is.na(gen_wiews_df$ID) & gen_wiews_df$ID != "" &
    !is.na(gen_wiews_df$LATITUDE) & gen_wiews_df$LATITUDE != "" &
    !is.na(gen_wiews_df$LONGITUDE) & gen_wiews_df$LONGITUDE != ""]

genesys_id_without_coords <- gen_wiews_df$ID[
  gen_wiews_df$data_source == "Genesys" &
    !is.na(gen_wiews_df$ID) & gen_wiews_df$ID != "" &
    (is.na(gen_wiews_df$LATITUDE) | gen_wiews_df$LATITUDE == "" |
       is.na(gen_wiews_df$LONGITUDE) | gen_wiews_df$LONGITUDE == "")]

wiews_id_with_coords <- gen_wiews_df$ID[
  gen_wiews_df$data_source == "WIEWS" &
    !is.na(gen_wiews_df$ID) & gen_wiews_df$ID != "" &
    !is.na(gen_wiews_df$LATITUDE) & gen_wiews_df$LATITUDE != "" &
    !is.na(gen_wiews_df$LONGITUDE) & gen_wiews_df$LONGITUDE != ""]

gen_wiews_df <- gen_wiews_df %>%
  filter(
    !(data_source == "WIEWS" &
        !is.na(ID) & ID != "" &
        (ID %in% genesys_id_with_coords |
           (ID %in% genesys_id_without_coords & !ID %in% wiews_id_with_coords))),
    !(data_source == "Genesys" &
        !is.na(ID) & ID != "" &
        ID %in% genesys_id_without_coords &
        ID %in% wiews_id_with_coords))

# Check duplicate DOIs within data source, NONE
dup_DOI <- gen_wiews_df %>%
  filter(!is.na(DOI) & trimws(DOI) != "") %>%
  group_by(DOI) %>%
  filter(n() > 1) %>%
  arrange(DOI)

# RESULT: 4,818,442 rows
# 1,244,830 rows removed in step 2


# STEP 3: Remove duplication within data source by ID, keep rows with lat/long (if available)
# ID = ACCENUMB + INSTCODE + GENUS SPECIES (standardized)      
# - Remove row if duplicate has lat/long (or both missing lat/long)

# Check duplicate IDs within data source, 24,520 rows
dup_ID <- gen_wiews_df %>%
  group_by(ID) %>%
  filter(n() > 1) %>%
  arrange(ID)

# Convert to data table for faster processing below (otherwise will lag)
setDT(gen_wiews_df)

gen_wiews_df <- gen_wiews_df[
  order(ID, data_source, is.na(LATITUDE), is.na(LONGITUDE)),
  .SD[1],
  by = .(ID, data_source)]

# Convert back to tibble if needed
#gen_wiews_df <- as_tibble(gen_wiews_df)  

# RESULT: 4,806,179 rows
# 12,263 rows removed in step 3


# Rename to final gen/wiews df and convert for save
gen_wiews_df_dedup <- gen_wiews_df
gen_wiews_df_dedup$STORAGE <- as.character(gen_wiews_df_dedup$STORAGE)
# Save Genesys/WIEWs merged + depduplicated
#write.csv(gen_wiews_df_dedup, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/De_dup_2026_03_06/gen_wiews_dedup_df_2026-03-06.csv', row.names = FALSE)


# ---------------------------------------- # 
# ----------- FINAL DATASET -------------- #
# FINAL Genesys/WIEWS DATA: 4,806,179 rows
# 2,049,076 total duplicate rows removed
# ----------------------------------------# 


# --------------------------------------------------------------------#
# ------------- Count rows after de-duplication -----------------------#
# ---------------------------------------------------------------------#

# Row count in dedup Genesys/WIEWS: 4,806,179 rows
nrow(gen_wiews_df_dedup)

# Row count in dedup Genesys: 3,574,451 rows
gen_wiews_df_dedup %>%
  filter(data_source == "Genesys") %>%
  nrow()

# Row count in dedup WIEWS: 1,231,728 rows
gen_wiews_df_dedup %>%
  filter(data_source == "WIEWS") %>%
  nrow()

# Row count in dedup Genesys/WIEWS with coords: 1,282,762 rows
gen_wiews_df_dedup %>%
  filter(
    !is.na(LATITUDE) & LATITUDE != "",
    !is.na(LONGITUDE) & LONGITUDE != ""  ) %>%
  nrow()

# Row count in dedup Genesys with coords: 1,063,351 rows
gen_wiews_df_dedup %>%
  filter(data_source == "Genesys") %>%
  filter(
    !is.na(LATITUDE) & LATITUDE != "",
    !is.na(LONGITUDE) & LONGITUDE != ""  ) %>%
  nrow()

# Row count in dedup WIEWS with coords: 219,411 rows
gen_wiews_df_dedup %>%
  filter(data_source == "WIEWS") %>%
  filter(
    !is.na(LATITUDE) & LATITUDE != "",
    !is.na(LONGITUDE) & LONGITUDE != ""  ) %>%
  nrow()


# -----------------------------------------------------------------------------#
# ------------- Summaries after dropping duplicates --------------------------#

# Count of standardized genus + INSTCODE by data_source AFTER dropping duplicates
gen_wiews_counts <- gen_wiews_df_dedup %>%
  group_by(INSTCODE, standardized_genus, data_source) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
#save
write.csv(gen_wiews_counts, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/De_dup_2026_03_06/gen_wiews_instcode_genus_counts_after_dropping_duplicates.csv', row.names = FALSE)

# Count of standardized genus by data_source AFTER dropping duplicates
gen_wiews_counts <- gen_wiews_df_dedup %>%
  group_by(standardized_genus, data_source) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
write.csv(gen_wiews_counts, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/De_dup_2026_03_06/gen_wiews_genus_counts_after_dropping_duplicates.csv', row.names = FALSE)


# -----------------------------------------------------------------------------#
# ---- COMBINE SUMMARIES: COMPARE BEFORE AND AFTER DROPPING DUPLICATES --------#
# -----------------------------------------------------------------------------#
#### SUMMARY 1: GENUS + INSTCODE BY DATA SOURCE COUNTS: BEFORE AND AFTER DEDUP
gen_wiews_instcode_genus_counts_before_dropping_duplicates <- read_csv("Data/Data_processing/De_dup_2026_03_06/gen_wiews_instcode_genus_counts_before_dropping_duplicates.csv")
gen_wiews_instcode_genus_counts_after_dropping_duplicates <- read_csv("Data/Data_processing/De_dup_2026_03_06/gen_wiews_instcode_genus_counts_after_dropping_duplicates.csv")

gen_wiews_instcode_genus_summary <- gen_wiews_instcode_genus_counts_before_dropping_duplicates %>%
  rename(n_before_dropping_duplicates = n) %>%
  full_join(
    gen_wiews_instcode_genus_counts_after_dropping_duplicates %>%
      rename(n_after_dropping_duplicates = n),
    by = c("INSTCODE", "standardized_genus", "data_source")
  ) %>%
  arrange(desc(n_before_dropping_duplicates))
#save
write.csv(gen_wiews_instcode_genus_summary, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/De_dup_2026_03_06/gen_wiews_instcode_genus_counts_SUMMARY.csv', row.names = FALSE)


#### SUMMARY 2: GENUS BY DATA SOURCE COUNTS: BEFORE AND AFTER DEDUP
gen_wiews_genus_counts_before_dropping_duplicates <- read_csv("Data/Data_processing/De_dup_2026_03_06/gen_wiews_genus_counts_before_dropping_duplicates.csv")
gen_wiews_genus_counts_after_dropping_duplicates <- read_csv("Data/Data_processing/De_dup_2026_03_06/gen_wiews_genus_counts_after_dropping_duplicates.csv")

gen_wiews_genus_summary <- gen_wiews_genus_counts_before_dropping_duplicates %>%
  rename(n_before_dropping_duplicates = n) %>%
  full_join(
    gen_wiews_genus_counts_after_dropping_duplicates %>%
      rename(n_after_dropping_duplicates = n),
    by = c("standardized_genus", "data_source")
  ) %>%
  arrange(desc(n_before_dropping_duplicates))
#save
write.csv(gen_wiews_genus_summary, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/De_dup_2026_03_06/gen_wiews_genus_counts_SUMMARY.csv', row.names = FALSE)



### end section 1 ###

# ================================ SECTION 2 ================================ #
#            Add Organization Type, Remove Institutions & Duplicates          #
# =========================================================================== #

# Annotate data with organization type, remove flagged organizations, remove duplicates between data

# Annotate data with organization type
#      Botanic garden
#      Genebank
#

# DEDUPLICATION STEP 4: 
# STEP 4: Remove flagged organizations
# - Remove non-genebank/non-botanic garden organizations
# - Remove duplicates between datasets
#      guide file flags duplicates to remove based on counts per org by data source.
#      Data source with higher number of records per org is selected to keep.
#      Guide file also flags to remove genebanks from BGCI data.

# -------------------------------- #
# ----- READ IN GUIDE FILE -------#
# -------------------------------- #

# Organizations guide file: 22,276 organizations from FAO WIEWS and BGCI GardenSearch
all_org <- read_excel("C:/Users/sarah/Downloads/all_organizations_locations_corrected_2026-03-09 (1).xlsx")

# -------------------------------- #
# -------- READ IN DATA ---------- #
# -------------------------------- #

# Genesys/WIEWS combined data; 4,806,179 rows
gen_wiews_df <- read_csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/De_dup_2026_03_06/gen_wiews_dedup_df_2026-03-06.csv')
cat("Raw Genesys/WIEWS rows loaded:", nrow(gen_wiews_df), "\n")

# BGCI: 626,307 rows
bgci_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/BGCI_data_prepped_2026-03-09.csv")
cat("Raw BGCI rows loaded:", nrow(bgci_df), "\n")

# Cano: 391,214 rows
cano_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/Cano_data_prepped_2026-03-09.csv")
cat("Raw Cano rows loaded:", nrow(cano_df), "\n")

# GBIF LIVING: 118,198 rows
gbif_living_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_living_2026-03-11.csv")
cat("Raw GBIF-Living rows loaded:", nrow(gbif_living_df), "\n")

# GBIF OBSERVATIONS: 6,701,782 rows
# gbif_observations <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_observations_2026-03-11.csv")



# ----------- PROCESS BGCI DATA ------------ #

# BGCI data: rename ex_situ_garden_id = inst_code
bgci_df <- bgci_df %>%
  rename(inst_code = ex_situ_garden_id) %>%
  mutate(inst_code = trimws(inst_code))

# ----------- ASSIGN ORGANIZATION TYPE to BGCI ------------#
bgci_df <- all_org %>%
  select(inst_code, organization_type, organization_name) %>%
  mutate(
    inst_code = trimws(inst_code),
    organization_type = trimws(organization_type),
    organization_name = trimws(organization_name)
  ) %>%
  right_join(bgci_df, by = c("inst_code"))

# ----------- REMOVE flagged BGCI institutions ------------#
bgci_delete_insts <- all_org %>%
  filter(delete_organization_from_BGCI_data == "delete") %>%
  pull(inst_code) %>%
  unique()
bgci_df <- bgci_df %>% filter(!inst_code %in% bgci_delete_insts)
cat("BGCI after deleting flagged institutions:", nrow(bgci_df), "\n")


# ----------- REMOVE non-genebank/non-botanic garden organizations ------------#
bgci_df <- bgci_df %>%
  filter(
    !is.na(organization_type),
    trimws(organization_type) != "",
    organization_type != "NonGenebank_NonBotanicgarden"
  )
cat("BGCI after removing non-genebank/non-botanic garden org_type:", nrow(bgci_df), "\n")

# RESULT:
# BGCI data with org type assigned: 618,544 rows
# 7,763 rows dropped
write.csv(
  bgci_df,
  'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Deduplicated_with_org_assigned/bgci_org_assigned_2026-05-19.csv',
  row.names = FALSE
)


# ----------- PROCESS GEN/WIEWS DATA ------------ #

# Make sure gen_wiews_df has inst_code column
if ("INSTCODE" %in% colnames(gen_wiews_df)) {
  gen_wiews_df <- gen_wiews_df %>% rename(inst_code = INSTCODE)
}

# ----------- ASSIGN ORGANIZATION TYPE to Gen/WIEWS ------------#
gen_wiews_df <- all_org %>%
  select(inst_code, organization_type, organization_name) %>%
  mutate(
    inst_code = trimws(inst_code),
    organization_type = trimws(organization_type),
    organization_name = trimws(organization_name)
  ) %>%
  right_join(gen_wiews_df, by = c("inst_code"))

# ----------- REMOVE flagged Gen/WIEWS institutions ------------#
genwiews_delete_insts <- all_org %>%
  filter(delete_organization_from_GenWIEWS_data == "delete") %>%
  pull(inst_code) %>%
  unique()
gen_wiews_df <- gen_wiews_df %>% filter(!inst_code %in% genwiews_delete_insts)
cat("Gen/WIEWS after deleting flagged institutions:", nrow(gen_wiews_df), "\n")

# ----------- REMOVE non-genebank/non-botanic garden organizations ------------#
gen_wiews_df <- gen_wiews_df %>%
  filter(
    !is.na(organization_type),
    trimws(organization_type) != "",
    organization_type != "NonGenebank_NonBotanicgarden"
  )
cat("Gen/WIEWS after removing non-genebank/non-botanic garden org_type:", nrow(gen_wiews_df), "\n")

# RESULT:
# Gen/WIEWS data with org type assigned: 4,803,066 rows
# 3,113 rows dropped
write.csv(
  gen_wiews_df,
  'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Deduplicated_with_org_assigned/gen_wiews_org_assigned_2026-05-19.csv',
  row.names = FALSE
)


# ----------- PROCESS CANO DATA ------------ #

# Assign org names by LC 
assign_cano_LC <- read_excel("C:/Users/sarah/Downloads/LC_assign_cano.xlsx")
assign_cano_LC <- assign_cano_LC %>%
  select(-data_source,
         -SG_duplicate_note,
         -organization_type)

cano_df <- cano_df %>%
  right_join(assign_cano_LC, by = "LC")

# ----------- ASSIGN ORGANIZATION TYPE to Cano ------------#
cano_df <- all_org %>%
  select(inst_code, organization_type, organization_name) %>%
  mutate(
    inst_code = trimws(inst_code),
    organization_type = trimws(organization_type),
    organization_name = trimws(organization_name)
  ) %>%
  right_join(cano_df, by = "inst_code")

# ----------- REMOVE flagged Cano institutions ------------#
cano_delete_insts <- all_org %>%
  filter(delete_organization_from_Cano_data == "delete") %>%
  pull(inst_code) %>%
  unique()
cano_df <- cano_df %>% filter(!inst_code %in% cano_delete_insts)
cat("Cano after deleting flagged institutions:", nrow(cano_df), "\n")

# ----------- REMOVE non-genebank/non-botanic garden organizations ------------#
cano_df <- cano_df %>%
  filter(
    !is.na(organization_type),
    trimws(organization_type) != "",
    organization_type != "NonGenebank_NonBotanicgarden"
  )
cat("Cano after removing non-genebank/non-botanic garden org_type:", nrow(cano_df), "\n")

# RESULT:
# Cano data with org type assigned: 316,489 rows
# 74,725 rows dropped
write.csv(
  cano_df,
  'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Deduplicated_with_org_assigned/cano_org_assigned_2026-05-19.csv',
  row.names = FALSE
)


# ----------- PROCESS GBIF-LIVING DATA ------------ #

# ----------- ASSIGN ORGANIZATION TYPE to GBIF-LIVING ---------------#
gbif_living_df <- gbif_living_df %>% rename(inst_code = inst_code_WIEWS)
gbif_living_df <- all_org %>%
  select(inst_code, organization_type, organization_name) %>%
  mutate(
    inst_code = trimws(inst_code),
    organization_type = trimws(organization_type),
    organization_name = trimws(organization_name)
  ) %>%
  right_join(gbif_living_df, by = "inst_code")
cat("GBIF-living after assigning org_type:", nrow(gbif_living_df), "\n")

# ----------- REMOVE flagged GBIF-living institutions ------------#
gbifliving_delete_insts <- all_org %>%
  filter(delete_organization_from_GBIF_living_data == "delete" |
           delete_organization_from_GBIF_living_data == "delete") %>%
  pull(inst_code) %>%
  unique()
gbif_living_df <- gbif_living_df %>% filter(!inst_code %in% gbifliving_delete_insts)
cat("GBIF-living after deleting flagged institutions:", nrow(gbif_living_df), "\n")

# ----------- REMOVE non-genebank/non-botanic garden organizations ------------#
gbif_living_df <- gbif_living_df %>%
  filter(
    !is.na(organization_type),
    trimws(organization_type) != "",
    organization_type != "NonGenebank_NonBotanicgarden"
  )
cat("GBIF-living after removing non-genebank/non-botanic garden org_type:", nrow(gbif_living_df), "\n")

# RESULT:
# GBIF-living data with org type assigned: 72,863 rows
# 15,691 rows dropped

## end section 2 ##