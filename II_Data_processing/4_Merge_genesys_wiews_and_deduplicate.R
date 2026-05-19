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

# --------------------------------
# ----- READ IN GUIDE FILES -----
# --------------------------------

## WIEWS guide file with institute names and FAO INSTCODE
institute_names_no_syn <- read_excel("G:/.shortcut-targets-by-id/1GnMqdK_h04rDh_GYxxYBWiyuGZFSN2GZ/GCCS metrics project shared folder/GCCSmetricsII/Data_processing/Support_files/FAO_WIEWS/FAO_WIEWS_organizations_PG.xlsx")
names(institute_names_no_syn)[names(institute_names_no_syn) == 'WIEWS instcode'] <- 'INSTCODE'
names(institute_names_no_syn)[names(institute_names_no_syn) == 'Organization authority status'] <- 'ORGANIZATIONTYPE'
institute_names_no_syn <- subset(institute_names_no_syn, select = c(INSTCODE, ORGANIZATIONTYPE))  %>% drop_na()

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

# WIEWS data with coords: 955,229 rows
WIEWS_allcrops_with_coords <- WIEWS_allcrops %>%
  filter(
    !is.na(LATITUDE) & LATITUDE != "",
    !is.na(LONGITUDE) & LONGITUDE != ""  )

# Genesys/WIEWS data with coords: 2,019,533 rows
gen_wiews_with_coords <- gen_wiews_df %>%
  filter(
    !is.na(LATITUDE) & LATITUDE != "",
    !is.na(LONGITUDE) & LONGITUDE != ""  )


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
# 1,244,850 rows removed in step 2


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



### end script ###
