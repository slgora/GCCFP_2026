################################################################################                                                                             
#    Duplicate removal by selection of data source with greatest number of    
#    records per instcode (1 data source per inst_code) for accession-level datasets.                 
#                                                                             
#    Deduplication Method:                                                    
#    - For each organization (inst_code), only ONE data source is kept:       
#      whichever source has greatest number of records for that inst_code.    
#    - Duplicates across Genesys, WIEWS, Cano, and GBIF-living are      
#      removed by this single-source-per-institution rule.                    
#                                                                                                                                               
#      Harmonize datasets and prep data (ei: inst_code field as character)    
#                                                                             
#      DEDUPLICATION STEPS:                                                   
#         - Step 1: Remove duplicates by data source         
#                   for each inst_code keep only source with most records     
#                   proritizing Genesys for CGIAR institions                  
#         - Step 2: Remove duplicates in Genesys/WIEWS by DOI                 
#                   prioritizing records with lat/long, otherwise-Genesys     
#         - Step 3: Remove duplicates in Genesys/WIEWS by ID                  
#                   ID = acce_numb + inst_code + genus_species                 
#                   prioritizing records with lat/long, otherwise-Genesys  
#
#      Deduplicated accession-level datasets are filtered for record with valid 
#       coords and combined with GBIf-observations data with valid coords to 
#       create occurrences dataset.
#                 
#      Outputs:                                                               
#       Deduplication summary tables for review                               
#       Deduplicated datasets
#        Genebank accession-level dataset (final)
#        Botanic garden accession-level (final)
#        Occurrences (final)
#                                                                             
###############################################################################

# Read in two datasets
# 1. genebank_accessionlevel_dataset
# 2. botanicgarden_accessionlevel_dataset

# ---- 0. Load required libraries ---------------------------------------------#
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(purrr)
library(stringr)

# ---- 1. Read in data ----

### DATASETS ###

# Genebank-accession-level dataset:  6,786,269 rows
# Genesys: 3,563,673 rows
# WIEWS: 3,189,177 rows
# GBIF_living: 33,419 rows
genebank_accessionlevel_dataset <- read.csv('C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Final_datasets/genebank_accessionlevel_dataset_2026-05-29.csv')

## Botanic garden accession-level dataset: 558,308 rows
# Genesys: 51,907 rows
# WIEWS: 45,948 rows
# GBIF_living: 69,239 rows
# Cano: 391,214 rows
botanicgarden_accessionlevel_dataset <- read.csv('C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Final_datasets/botanicgarden_accessionlevel_dataset_2026-05-29.csv')


### GUIDE FILES ###
# guide file with wiews org type (CGIAR, etc) assignment for selection data sources
institute_names_no_syn <- read_excel("C:/Users/sarah/Downloads/FAO_WIEWS_organizations_PG_2026.xlsx")
institute_names_no_syn <- subset(institute_names_no_syn, select = c(inst_code, wiews_org_type))  %>% drop_na()
CGIAR_instcodes <- institute_names_no_syn %>% filter(wiews_org_type == "CGIAR")
# guide file of EURISCO instcodes assignment for selection data sources review
eurisco_path <- "C:/Users/sarah/Downloads/eurisco_inst_code_list.xlsx"




# ------------------------- Deduplication Workflow  ---------------------------#

# ----------------------------------------------------------------------------- #
# ----------- STEP 1: REMOVE DUPLICATES IN GENESYS/WIEWS BY DATA SOURCE ------- #
# ----------------------------------------------------------------------------- #
#           - Remove duplicates in Genesys/WIEWS data by selecting to keep data source with greatest number of records
#           - Create selection data sources table for Genesys, WIEWS, GBIF-living, Cano
#           - For each inst_code keep only source with most records
#           - Proritizes keeping Genesys for CGIAR institions

source("Functions/select_data_source.R")

genebank_selection <- select_data_source(
  acc_dataset = genebank_accessionlevel_dataset,
  institute_names_no_syn = institute_names_no_syn,
  eurisco_path = eurisco_path,
  preferred_order = c("Genesys", "WIEWS", "GBIF_living"))
#save selection data sources table
write.csv(genebank_selection, "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Guide_files/selection_data_sources_genebankaccessionlevel_2026-05-29.csv", row.names = FALSE)

botanic_selection <- select_data_source(
  acc_dataset = botanicgarden_accessionlevel_dataset,
  institute_names_no_syn = institute_names_no_syn,
  eurisco_path = eurisco_path,
  preferred_order = c("Genesys", "WIEWS", "Cano", "GBIF_living"))
#save selection data sources table
write.csv(botanic_selection, "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Guide_files/selection_data_sources_botanicaccessionlevel_2026-05-29.csv", row.names = FALSE)


# Use selection_data_sources to filter datasets
# Filter genebank data set to kept records
genebank_accessionlevel_dataset <- genebank_accessionlevel_dataset %>%
  inner_join(
    genebank_selection %>% select(inst_code, data_source = best_data_source),
    by = c("inst_code", "data_source"))

# Filter botanic garden data set to kept records
botanicgarden_accessionlevel_dataset <- botanicgarden_accessionlevel_dataset %>%
  inner_join(
    botanic_selection %>% select(inst_code, data_source = best_data_source),
    by = c("inst_code", "data_source"))

# Example: Check counts by kept data source
table(genebank_accessionlevel_dataset$data_source)
table(botanicgarden_accessionlevel_dataset$data_source)

# genebank_accessionlevel_dataset: 4,682,342
# Genesys: 3,429,512 
# WIEWS: 1,231,369
# GBIF_living: 21,461

# botanicgarden_accessionlevel_dataset: 484,565
# Genesys: 48,192
# WIEWS: 2,376
# Cano: 374,409
# GBIF_living: 59,588



# -----------------------------------------------------------------------------#
# ------------- STEP 2: REMOVE DUPLICATES IN GENESYS/WIEWS BY DOI -------------#
# -----------------------------------------------------------------------------#
#      - Prioritizing records with lat/long, otherwise-Genesys
#      - also removes within-source duplication

genebank_accessionlevel_dataset <- genebank_accessionlevel_dataset %>%
  group_by(doi) %>%
  mutate(
    has_latlong = !is.na(latitude) & latitude != "" & !is.na(longitude) & longitude != "",
    is_genesys = data_source == "Genesys" ) %>%
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
    is.na(doi) | doi == "" | pick_row) %>%
  ungroup() %>%
  select(-has_latlong, -is_genesys, -pick_row)


botanicgarden_accessionlevel_dataset <- botanicgarden_accessionlevel_dataset %>%
  group_by(doi) %>%
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
    is.na(doi) | doi == "" | pick_row) %>%
  ungroup() %>%
  select(-has_latlong, -is_genesys, -pick_row)


# genebank_accessionlevel_dataset: 4,675,571
# Genesys: 3,429,512
# WIEWS: 1,224,598
# GBIF_living: 21,461

# botanicgarden_accessionlevel_dataset: 484,565
# Genesys: 48,192
# WIEWS: 2,376
# Cano: 374,409
# GBIF_living: 59,588



# -----------------------------------------------------------------------------#
# ------------- STEP 3: REMOVE DUPLICATES IN GENESYS/WIEWS BY ID --------------#
# -----------------------------------------------------------------------------#
#      - Create a unique accession ID for deduplication and merging:
#      - ID = ACCENUMB + inst_code + genus_species_WFO (standardized genus + species to WFO)
#      - Prioritizing records with lat/long, otherwise-Genesys
#      - also removes within-source duplication

# Clean fields and create unique ID
genebank_accessionlevel_dataset$acce_numb          <- trimws(genebank_accessionlevel_dataset$acce_numb)
genebank_accessionlevel_dataset$inst_code         <- trimws(genebank_accessionlevel_dataset$inst_code)
genebank_accessionlevel_dataset$standardized_genus_species_wfo   <- trimws(genebank_accessionlevel_dataset$standardized_genus_species_wfo)

botanicgarden_accessionlevel_dataset$acce_numb          <- trimws(botanicgarden_accessionlevel_dataset$acce_numb)
botanicgarden_accessionlevel_dataset$inst_code         <- trimws(botanicgarden_accessionlevel_dataset$inst_code)
botanicgarden_accessionlevel_dataset$standardized_genus_species_wfo   <- trimws(botanicgarden_accessionlevel_dataset$standardized_genus_species_wfo)

# Create unique ID: concatenate ACCENUMB, inst_code, standardized_genus_species_wfo (no separator)
genebank_accessionlevel_dataset$ID <- ifelse(
  genebank_accessionlevel_dataset$data_source %in% c("Genesys", "WIEWS"),
  paste0(
    genebank_accessionlevel_dataset$acce_numb,
    genebank_accessionlevel_dataset$inst_code,
    genebank_accessionlevel_dataset$standardized_genus_species_wfo),
  NA_character_)

botanicgarden_accessionlevel_dataset$ID <- ifelse(
  botanicgarden_accessionlevel_dataset$data_source %in% c("Genesys", "WIEWS"),
  paste0(
    botanicgarden_accessionlevel_dataset$acce_numb,
    botanicgarden_accessionlevel_dataset$inst_code,
    botanicgarden_accessionlevel_dataset$standardized_genus_species_wfo ),
  NA_character_)

# Remove all spaces that may exist in the new ID
# The resulting 'ID' uniquely identifies each accession for downstream duplicate handling.
genebank_accessionlevel_dataset <- genebank_accessionlevel_dataset %>% mutate(ID = str_replace_all(ID, " ", ""))
botanicgarden_accessionlevel_dataset <- botanicgarden_accessionlevel_dataset %>% mutate(ID = str_replace_all(ID, " ", ""))


# STEP 3: Remove duplicate IDs, keep record with lat/long, else keep Genesys
# also tackles within-source deduplication
genebank_accessionlevel_dataset <- genebank_accessionlevel_dataset %>%
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
    is.na(ID) | ID == "" | pick_row ) %>%
  ungroup() %>%
  select(-has_latlong, -is_genesys, -pick_row, -ID)


botanicgarden_accessionlevel_dataset <- botanicgarden_accessionlevel_dataset %>%
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
    is.na(ID) | ID == "" | pick_row ) %>%
  ungroup() %>%
  select(-has_latlong, -is_genesys, -pick_row, -ID)

# FINAL genebank_acccessionlevel_dataset: 4,672,150
# Genesys: 3,426,138
# WIEWS: 1,224,551
# GBIF_living: 21,461

# FINAL botanicgarden_accessionlevel_dataset: 484,565
# Genesys: 48,192
# WIEWS: 2,376
# Cano: 374,409
# GBIF_living: 59,588


#save
write.csv(genebank_accessionlevel_dataset, 
          file = "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/FINAL_DATA_2026_06_01/genebank_accessionlevel_dataset_dedup_2026-06-01.csv",
          row.names = FALSE)

#save
write.csv(botanicgarden_accessionlevel_dataset, 
          file = "C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/FINAL_DATA_2026_06_01/botanicgarden_accessionlevel_dataset_dedup_2026-06-01.csv",
          row.names = FALSE)




############# COMBINE ALL ACCESSION-LEVEL DATASETS WITH OCCURRENCES AND FILTER FOR RECORDS WITH VALID LAT/LONG
### RESULTING DATASET IS OCCURRENCES DATASET

# -----------------------#
# DATASET 4. Occurrences
#------------------------#

# FINAL genebank_accessionlevel_dataset : 4,672,150
# with coords: 1,237,771
genebank_accessionlevel_dataset_with_coords <- genebank_accessionlevel_dataset %>% # 
  filter( !(is.na(latitude) | is.na(longitude) | 
              !between(latitude, -90, 90) | !between(longitude, -180, 180)))

# FINAL botanicgarden_accessionlevel_dataset: 484,565
# with coords: 16,244
botanicgarden_accessionlevel_dataset_with_coords <- botanicgarden_accessionlevel_dataset %>% # 
  filter( !(is.na(latitude) | is.na(longitude) | 
              !between(latitude, -90, 90) | !between(longitude, -180, 180)))
# Drop LC (no cano data)
botanicgarden_accessionlevel_dataset_with_coords <- botanicgarden_accessionlevel_dataset_with_coords %>% select(-LC)


# RAW GBIF OBSERVATIONS: 6,701,782 rows
# read in above
gbif_observations_df_with_coords <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_observations_2026-05-28.csv")
# with coords: 4,191,231 
gbif_observations_df_with_coords <- gbif_observations_df %>% # 
  filter( !(is.na(latitude) | is.na(longitude) | 
              !between(latitude, -90, 90) | !between(longitude, -180, 180)))

#stage new fields for join below
gbif_observations_df_with_coords$acce_numb <- NA
gbif_observations_df_with_coords$doi <- NA
gbif_observations_df_with_coords$samp_stat <- NA
gbif_observations_df_with_coords$inst_type <- NA

# Select and then rename only the specified fields in gbif_observations_df
gbif_observations_df_with_coords <- gbif_observations_df_with_coords %>%
  select(
    WCFP_name_match,
    acce_numb,
    doi,
    taxa,
    genus_species,
    Standardized_taxa,
    standardized_genus,
    genus_species_WFO,
    inst_ID_GBIF,
    inst_code_WIEWS2,
    organization_name2,
    inst_type,
    collection_code,
    basis_of_record,
    samp_stat,
    country_code,
    latitude,
    longitude,
    data_source
  ) %>%
  rename(
    wcfp_name_match = WCFP_name_match,
    acce_numb = acce_numb,
    doi = doi,
    taxa = taxa,
    genus_species = genus_species,
    standardized_taxa_wfo = Standardized_taxa,
    standardized_genus_wfo = standardized_genus,
    standardized_genus_species_wfo = genus_species_WFO,
    inst_id_gbif = inst_ID_GBIF,
    inst_code = inst_code_WIEWS2,
    inst_name = organization_name2,
    inst_type = inst_type,  #make sure is blank for observations
    collection_code = collection_code,
    basis_of_record = basis_of_record,
    samp_stat = samp_stat,
    orig_cty = country_code,
    latitude = latitude,
    longitude = longitude,
    data_source = data_source )


#occurrences without coords filtered: 11,858,497

## FINAL: Occurrences dataset: 5,445,246 rows
occurrences_dataset <- rbind(genebank_accessionlevel_dataset_with_coords, 
                             botanicgarden_accessionlevel_dataset_with_coords, 
                             gbif_observations_df_with_coords)
# save
write.csv(occurrences_dataset, 'C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/FINAL_DATA_2026_06_01/occurrences_dataset_2026-06-01.csv', row.names = FALSE)

#### FINAL: Occurrences dataset: 5,445,246 rows
## Genesys: 1,026,925
#Genesys (genebanks): 1,023,097
#Genesys (botanic gardens): 3,828

## WIEWs: 206,359
#WIEWS (genbanks): 205,993
#WIEWS (botanic gardens): 366

## GBIF-living: 20,731
#GBIF-living (genebanks): 8,681
#GBIF-living (botanic gardens): 12,050

# GBIF_observations: 4,191,231

# end script #
