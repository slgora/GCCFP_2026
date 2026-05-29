
## Make 5 data sets:
# 1. Genebank accession -level
# 2. Botanic garden accession level
# 3. Botanic garden species level
# 4. Occurrences
# 5. Organizations

# Harmonize all field names across datasets for join
# added normalized taxa field 

# load libraries
library(readr)

# ----- READ IN DATA ---- # 

### Genesys/WIEWS data: 4,701,257 rows
# Genesys data: 3,474,330 rows
# WIEWS data: 1,226,927 rows
gen_wiews_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/gen_wiews_data_dedup_2026_05_28.csv")

# Cano data: 374,409 rows
cano_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/cano_data_dedup_2026_05_28.csv")

# BGCI data: 560,271 rows
bgci_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/bgci_data_dedup_2026_05_28.csv")

# GBIF-living genebank data: 21,461 rows
gbif_living_genebank_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/gbif_living_genebank_data_dedup_2026_05_28.csv")

# GBIF-living botanic garden data: 51,206 rows
gbif_living_botanic_garden_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/gbif_living_botanicgarden_data_dedup_2026_05_28.csv")


# GardenSearch + FAO WIEWs Institute directory
all_organizations_locations <- read_excel("C:/Users/sarah/Downloads/all_organizations_locations_corrected_2026-03-09 (1).xlsx")


# -----------------------------------------------------------------#
# --- Prep Gen/WIEWS data ---- #

# READ in GEN/WIEWS: 4,700,251 rows
#gen_wiews_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/gen_wiews_data_dedup_2026_05_28.csv")

#stage new fields for join below
gen_wiews_df$inst_id_gbif <- NA
gen_wiews_df$collection_code <- NA

# REMOVE FOR NOW
# "storage_type" = seed, field, cryo
#"inst_status"= "National"      "International"

# Select and then rename only the specified fields in gen_wiews_df
gen_wiews_df <- gen_wiews_df %>%
  select(
    WCFP_name_match,
    ACCENUMB,
    DOI,
    fullTaxa,
    genus_species,
    Standardized_taxa,
    standardized_genus,
    genus_species_WFO,
    inst_id_gbif,
    inst_code,
    organization_name_WIEWS,
    organization_type,
    collection_code,
    STORAGE,
    SAMPSTAT,
    ORIGCTY,
    LATITUDE,
    LONGITUDE,
    data_source
  ) %>%
  rename(
    wcfp_name_match = WCFP_name_match,
    acce_numb = ACCENUMB,
    doi = DOI,
    taxa = fullTaxa,
    genus_species = genus_species,
    standardized_taxa_wfo = Standardized_taxa,
    standardized_genus_wfo = standardized_genus,
    standardized_genus_species_wfo = genus_species_WFO,
    inst_id_gbif = inst_id_gbif,
    inst_code = inst_code,
    inst_name = organization_name_WIEWS,
    inst_type = organization_type,
    collection_code = collection_code,
    basis_of_record = STORAGE,
    samp_stat = SAMPSTAT,
    orig_cty = ORIGCTY,
    latitude = LATITUDE,
    longitude = LONGITUDE,
    data_source = data_source )


# --------------------------------------#
# DATASET 1. Genebank accession-level 
#---------------------------------------#

# Genesys/WIEWS (Genebank): 4,650,689 rows
gen_wiews_genebank_df <- gen_wiews_df %>% filter(inst_type == "Genebank")

# GBIF Living (Genebank):
#gbif_living_genebank_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/gbif_living_genebank_data_dedup_2026_05_28.csv")

#stage new fields for join below
gbif_living_genebank_df$acce_numb <- NA
gbif_living_genebank_df$doi <- NA
gbif_living_genebank_df$samp_stat <- NA

# Select and then rename only the specified fields in gen_wiews_df
gbif_living_genebank_df <- gbif_living_genebank_df%>%
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
    organization_type_FINAL,
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
    inst_type = organization_type_FINAL,
    collection_code = collection_code,
    basis_of_record = basis_of_record,
    samp_stat = samp_stat,
    orig_cty = country_code,
    latitude = latitude,
    longitude = longitude,
    data_source = data_source )


# Genebank-accession-level dataset: 4,672,150 rows
genebank_accessionlevel_dataset <- rbind(gen_wiews_genebank_df, gbif_living_genebank_df)
#save
write.csv(genebank_accessionlevel_dataset, 'C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Final_datasets/genebank_accessionlevel_dataset_2026-05-28.csv', row.names = FALSE)


# Genebank-accession-level dataset WITH COORDS: 1237771 rows
genebank_accessionlevel_dataset_with_coords <- genebank_accessionlevel_dataset %>%
  filter( !(is.na(latitude) | is.na(longitude) | 
              !between(latitude, -90, 90) | !between(longitude, -180, 180)))
#save
write.csv(genebank_accessionlevel_dataset_with_coords, 'C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Coords_datasets/genebank_accessionlevel_dataset_with_coords_2026-05-28.csv', row.names = FALSE)



# ----------------------------------------#
# DATASET 2. Botanic garden accession-level 
#-----------------------------------------#

# Cano: rows
#cano_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/cano_data_dedup_2026_05_28.csv")

# Create a new field 'taxa' by combining taxon_name and taxon_authors_accepted separated by a space
cano_df <- cano_df %>%
  mutate(taxa = paste(taxon_name, taxon_authors_accepted, sep = " "))

# Add genus_species_WFO column
cano_df <- cano_df %>%
  mutate(genus_species_WFO = extract_genus_species(taxon_name_simple))

#stage new fields for join below
cano_df$doi <- NA
cano_df$inst_id_gbif <- NA
cano_df$samp_stat <- NA
cano_df$latitude <- NA
cano_df$longitude <- NA

# Select and then rename only the specified fields in cano_df
cano_df <- cano_df %>%
  select(
    WCFP_name_match,
    item_acc_no_full,
    doi,
    taxa,
    genus_species,
    taxon_name_simple,
    standardized_genus,
    genus_species_WFO,
    LC,
    inst_id_gbif,
    inst_code,
    organization_name,
    organization_type,
    acc_numb,
    item_status_type,
    samp_stat,
    provenance_code,
    latitude,
    longitude,
    data_source
  ) %>%
  rename(
    wcfp_name_match = WCFP_name_match,
    acce_numb = item_acc_no_full,
    doi = doi,
    taxa = taxa,
    genus_species = genus_species,
    standardized_taxa_wfo = taxon_name_simple,
    standardized_genus_wfo = standardized_genus,
    standardized_genus_species_wfo = genus_species_WFO,
    LC = LC,
    inst_id_gbif = inst_id_gbif,
    inst_code = inst_code,
    inst_name = organization_name,
    inst_type = organization_type,
    collection_code = acc_numb,
    basis_of_record = item_status_type,
    samp_stat = samp_stat,
    orig_cty = provenance_code,
    latitude = latitude,
    longitude = longitude,
    data_source = data_source )



# Genesys/WIEWS (Botanic garden): 50,568 rows
gen_wiews_botanic_garden_df <- gen_wiews_df %>% filter(inst_type == "Botanic garden")
#stage new field for join below
gen_wiews_botanic_garden_df$LC <- NA

# Select and re-order fields
gen_wiews_botanic_garden_df <- gen_wiews_botanic_garden_df %>%
  select(wcfp_name_match, acce_numb, doi, taxa, genus_species, standardized_taxa_wfo, standardized_genus_wfo, standardized_genus_species_wfo,
         LC, inst_id_gbif, inst_code, inst_name, inst_type, collection_code, basis_of_record, 
         samp_stat, orig_cty, latitude, longitude, data_source )



#GBIF Living (Botanic garden):  rows
#gbif_living_botanic_garden_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/gbif_living_botanicgarden_data_dedup_2026_05_28.csv")

#stage new fields for join below
gbif_living_botanic_garden_df$acce_numb <- NA
gbif_living_botanic_garden_df$doi <- NA
gbif_living_botanic_garden_df$samp_stat <- NA
gbif_living_botanic_garden_df$LC <- NA

# Select and then rename only the specified fields
gbif_living_botanic_garden_df <- gbif_living_botanic_garden_df %>%
  select(
    WCFP_name_match,
    acce_numb,
    doi,
    taxa,
    genus_species,
    Standardized_taxa,
    standardized_genus,
    genus_species_WFO,
    LC,
    inst_ID_GBIF,
    inst_code_WIEWS2,
    organization_name2,
    organization_type_FINAL,
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
    LC = LC,
    inst_id_gbif = inst_ID_GBIF,
    inst_code = inst_code_WIEWS2,
    inst_name = organization_name2,
    inst_type = organization_type_FINAL,
    collection_code = collection_code,
    basis_of_record = basis_of_record,
    samp_stat = samp_stat,
    orig_cty = country_code,
    latitude = latitude,
    longitude = longitude,
    data_source = data_source )


## Botanic garden accession-level dataset: 476,183 rows ##
botanicgarden_accessionlevel_dataset <- rbind(cano_df, gen_wiews_botanic_garden_df, gbif_living_botanic_garden_df)
#save
write.csv(botanicgarden_accessionlevel_dataset, 'C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Final_datasets/botanicgarden_accessionlevel_dataset_2026-05-28.csv', row.names = FALSE)


## Botanic garden accession-level dataset WITH COORDS: 13,316 rows ##
botanicgarden_accessionlevel_dataset_with_coords <- botanicgarden_accessionlevel_dataset %>% 
  filter( !(is.na(latitude) | is.na(longitude) | 
              !between(latitude, -90, 90) | !between(longitude, -180, 180)))
# Drop LC (no cano data)
botanicgarden_accessionlevel_dataset_with_coords <- botanicgarden_accessionlevel_dataset_with_coords %>% select(-LC)
#save
write.csv(botanicgarden_accessionlevel_dataset_with_coords, 'C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Coords_datasets/botanicgarden_accessionlevel_dataset_with_coords_2026-05-28.csv', row.names = FALSE)



# -------------------------------------------------#
# DATASET 3. Botanic garden species/institute-level
#--------------------------------------------------#

# BGCI PlantSearch:  rows
#bgci_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Dedup_data/Dedup_data_2026_05_28/bgci_data_dedup_2026_05_28.csv")

# Select and then rename only the specified fields in bgci_df
botanicgarden_specieslevel_dataset <- bgci_df %>%
  select(
    WCFP_name_match,
    plantsearch_id,
    taxa,
    genus_species,
    Standardized_taxa,
    standardized_genus,
    genus_species_WFO,
    inst_code,
    organization_name_BGCI,
    organization_type,
    country_code,
    latitude,
    longitude,
    data_source
  ) %>%
  rename(
    wcfp_name_match = WCFP_name_match,
    plantsearch_id = plantsearch_id,
    taxa = taxa,
    genus_species = genus_species,
    standardized_taxa_wfo = Standardized_taxa,
    standardized_genus_wfo = standardized_genus,
    standardized_genus_species_wfo = genus_species_WFO,
    inst_code = inst_code,
    inst_name = organization_name_BGCI,
    inst_type = organization_type,
    country_code = country_code,
    latitude = latitude,
    longitude = longitude,
    data_source = data_source )

## Botanic garden species/inst level dataset: 550,271 rows ##
#save
write.csv(botanicgarden_specieslevel_dataset, 'C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Final_datasets/botanicgarden_specieslevel_dataset_2026-05-28.csv', row.names = FALSE)



# -----------------------#
# DATASET 4. Occurrences
#------------------------#

# genebank accession-level dataset with coords: 1,237,771 rows
genebank_accessionlevel_dataset_with_coords <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Coords_datasets/genebank_accessionlevel_dataset_with_coords_2026-05-28.csv")

# botanic garden accession-level dataset with coords: 13,316 rows
botanicgarden_accessionlevel_dataset_with_coords <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Coords_datasets/botanicgarden_accessionlevel_dataset_with_coords_2026-05-28.csv")

# GBIF observations with coords:  rows
#gbif_observations_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_observations_2026-03-12.csv")

#NEW
# SAVE RAW GBIF OBSERVATIONS: 6,701,782 rows
gbif_observations_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_observations_2026-05-28.csv")

#stage new fields for join below
gbif_observations_df$acce_numb <- NA
gbif_observations_df$doi <- NA
gbif_observations_df$samp_stat <- NA

# Select and then rename only the specified fields in gbif_observations_df
gbif_observations_df <- gbif_observations_df %>%
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
    organization_type_FINAL,
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
    inst_type = organization_type_FINAL,
    collection_code = collection_code,
    basis_of_record = basis_of_record,
    samp_stat = samp_stat,
    orig_cty = country_code,
    latitude = latitude,
    longitude = longitude,
    data_source = data_source )

#REMOVE DATA WITH INVLAID OR MISSING COORDS FROM GBIF OBSERVATIONS
# GBIF observations with valid coords: 4,191,231 rows
gbif_observations_df <- gbif_observations_df %>% # 
  filter( !(is.na(latitude) | is.na(longitude) | 
              !between(latitude, -90, 90) | !between(longitude, -180, 180)))

## Occurrences dataset: 5,442,318 rows ##
occurrences_dataset <- rbind(genebank_accessionlevel_dataset_with_coords, 
                             botanicgarden_accessionlevel_dataset_with_coords, 
                             gbif_observations_df)
#save
write.csv(occurrences_dataset, 'C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Final_datasets/occurrences_dataset_2026-05-28.csv', row.names = FALSE)



# --------------------------------#
# DATASET 5. Institution locations
#--------------------------------#

# GardenSearch + FAO WIEWs Institute directory
all_organizations_locations <- read_excel("C:/Users/sarah/Downloads/all_organizations_locations_corrected_2026-03-09 (1).xlsx")

# Keep only rows where organization_type is "Genebank" or "Botanic garden"    #6,834
all_organizations_locations <- all_organizations_locations %>%
  filter(organization_type %in% c("Genebank", "Botanic garden"))
# Keep organizations with valid lat/long
all_organizations_locations <- all_organizations_locations2 %>% # 4,891 rows
  filter( !(is.na(latitude) | is.na(longitude) | 
              !between(latitude, -90, 90) | !between(longitude, -180, 180)))

# save
write.csv(all_organizations_locations, 'C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Final_datasets/organization_locations_dataset_2026-05-28.csv', row.names = FALSE)

## end script ##
