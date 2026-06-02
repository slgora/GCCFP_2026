
#####################################################################################
#
#     Workflow assigns organization type to data, filters for 
#     genebanks/botanic gardens, harmonizes data sources to compile into respective datasets.
#     Data is prepped, instcodes corrected, and pre-filtered for WCFP species.                                   
#
#     Create 3 data sets:
#      1. Genebank accession-level dataset
#         - Genesys (genebanks) data
#         - WIEWS (genebanks) data
#         - GBIF-living (genebanks) data
#      2. Botanic garden accession-level dataset
#         - Genesys (botanic gardens) data
#         - WIEWS (botanic gardens) data
#         - GBIF-living (botanic gardens) data
#         - Cano et al data
#      3. Botanic garden species-level (FINAL)
#         - BGCI PlantSearch data
# 
#     WORKFLOW STEPS:
#     Step 1. Prep data
#     Step 2. Assign org type, filter to remove non-assigned org
#     Step 3. Combine into datasets.
#             accession-level datasets are deduplicated in following, script 5.
#
#     Outputs:      
#     Datasets with org type assigned    
#        Genebank accession-level dataset
#        Botanic garden accession-level
#        Botanic garden species-level (final)
# 
################################################################################


# ---- Load required libraries ---------------------------------------------#
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(purrr)
library(stringr)

# ----- READ IN DATA ---- # 

### RAW DATASETS ###
# Genesys data: 3,618,693 rows
genesys_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Input_files_script4_2026-06-02/GENESYS_data_prepped_2026-06-02.csv")
# WIEWS data: 3,236,562 rows
wiews_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Input_files_script4_2026-06-02/WIEWS_data_prepped_2026-06-02.csv")
# BGCI data: 626,307 rows
bgci_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Input_files_script4_2026-06-02/BGCI_data_prepped_2026-06-02.csv")
# Cano data: 391,214 rows
cano_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Input_files_script4_2026-06-02/Cano_data_prepped_2026-06-02.csv")
# GBIF living data: 118,198 rows
gbif_living_df <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Input_files_script4_2026-06-02/GBIF_data_living_2026-06-02.csv")

### GUIDE FILE ###
# guide file of all organizations in FAO WIEWS and BGCI GardenSearch institution directories
org_guide <- read_csv("C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Data_request_PG/Input_files_script4_2026-06-02/all_organizations_dataset.csv")


# -----------------------------------------------------------------#
#----------------------- STEP 1. Prep data ------------------------
# -----------------------------------------------------------------#

# Prep organization guide file
org_guide <- org_guide %>%
  mutate(inst_code = trimws(inst_code),
         organization_type = trimws(organization_type))

# -- Harmonize data: Type coercion as character for inst_code --
genesys_df$inst_code      <- as.character(genesys_df$inst_code)
wiews_df$inst_code        <- as.character(wiews_df$inst_code)
bgci_df$inst_code         <- as.character(bgci_df$inst_code)
cano_df$inst_code         <- as.character(cano_df$inst_code)
gbif_living_df$inst_code  <- as.character(gbif_living_df$inst_code)


# ----------------------------------------------------------------------------#
#------------ STEP 2. Organization assignment and filtering ------------------
# ---------------------------------------------------------------------------#

# Define function to assign org type and filter 
assign_org_type_and_filter <- function(df, guide_df,
                                       data_source_col = "data_source",
                                       inst_code_col = "inst_code",
                                       org_type_col = "organization_type",
                                       org_name_col = "organization_name",
                                       guide_org_types = c("Genebank", "Botanic garden"),
                                       bgci_cano_types = c("BGCI", "Cano"),
                                       drop_inst_codes = c(),
                                       filter_org_types = TRUE) {
  original_inst_codes <- unique(df[[inst_code_col]])
  original_row_count <- nrow(df)
  
  if (!(org_type_col %in% colnames(df))) df[[org_type_col]] <- NA_character_
  if (!(org_name_col %in% colnames(df))) df[[org_name_col]] <- NA_character_
  
  result <- df %>%
    mutate(
      !!org_type_col := case_when(
        .data[[data_source_col]] %in% bgci_cano_types ~ "Botanic garden",
        TRUE ~ .data[[org_type_col]]
      )
    ) %>%
    filter(!(!!as.name(data_source_col) == "BGCI" & .data[[inst_code_col]] %in% drop_inst_codes)) %>%
    left_join(
      guide_df %>%
        select(
          !!inst_code_col, 
          !!org_type_col, 
          !!org_name_col
        ) %>%
        filter(!is.na(.data[[inst_code_col]])),
      by = setNames(inst_code_col, inst_code_col),
      suffix = c("", ".org_guide")
    ) %>%
    mutate(
      !!org_type_col := ifelse(
        !( .data[[data_source_col]] %in% bgci_cano_types) & !is.na(.data[[paste0(org_type_col, ".org_guide")]]),
        .data[[paste0(org_type_col, ".org_guide")]],
        .data[[org_type_col]]
      ),
      !!org_name_col := ifelse(
        !( .data[[data_source_col]] %in% bgci_cano_types) & !is.na(.data[[paste0(org_name_col, ".org_guide")]]),
        .data[[paste0(org_name_col, ".org_guide")]],
        .data[[org_name_col]]
      )
    ) %>%
    select(-matches("\\.org_guide$"))
  
  if(filter_org_types) {
    result <- result %>% 
      filter(!is.na(.data[[org_type_col]]),
             .data[[org_type_col]] %in% guide_org_types)
  }
  
  final_inst_codes <- unique(result[[inst_code_col]])
  final_row_count <- nrow(result)
  num_institutions_removed <- sum(!(original_inst_codes %in% final_inst_codes))
  num_rows_removed <- original_row_count - final_row_count
  
  return(list(
    data = result,
    n_institutions_removed = num_institutions_removed,
    n_rows_removed = num_rows_removed,
    removed_inst_codes = setdiff(original_inst_codes, final_inst_codes)
  ))
}

# ----------------------------------------
# Assign org type and filter each dataset
# ----------------------------------------
res_genesys <- assign_org_type_and_filter(
  df = genesys_df, 
  guide_df = org_guide)
res_wiews <- assign_org_type_and_filter(
  df = wiews_df, 
  guide_df = org_guide)
res_bgci <- assign_org_type_and_filter(
  df = bgci_df, 
  guide_df = org_guide,
  drop_inst_codes = c("4678", "4606"))
res_cano <- assign_org_type_and_filter(
  df = cano_df, 
  guide_df = org_guide)
res_gbif <- assign_org_type_and_filter(
  df = gbif_living_df, 
  guide_df = org_guide)

# -------------------------
# Extract datasets from results
# -------------------------
genesys_df <- res_genesys$data
wiews_df <- res_wiews$data
bgci_df <- res_bgci$data
cano_df <- res_cano$data
gbif_living_df <- res_gbif$data

# -----------------------------------
# Summarize institutions/rows removed
# -----------------------------------
summary_df <- data.frame(
  dataset = c("genesys", "wiews", "bgci", "cano", "gbif_living"),
  n_institutions_removed = c(
    res_genesys$n_institutions_removed,
    res_wiews$n_institutions_removed,
    res_bgci$n_institutions_removed,
    res_cano$n_institutions_removed,
    res_gbif$n_institutions_removed ),
  n_rows_removed = c(
    res_genesys$n_rows_removed,
    res_wiews$n_rows_removed,
    res_bgci$n_rows_removed,
    res_cano$n_rows_removed,
    res_gbif$n_rows_removed))

print(summary_df)
#-------------------------------------------------
#    dataset  n_institutions_removed  n_rows_removed
#     genesys                     11           3113
#       wiews                      1           1437
#        bgci                      2           7763
#        cano                      0              0
# gbif_living                     36          15540
#---------------------------------------------------

# -------------------------
# View which inst_codes were removed from any dataset
# -------------------------
print(res_gbif$removed_inst_codes)
print(res_bgci$removed_inst_codes)


# -----------------------------------------------------------------#
# ------------ Step 3. Combine into datasets -----------------------
# -----------------------------------------------------------------#

# --- Prep Gen/WIEWS data ---- #
# combine gen/wiews
gen_wiews_df <- bind_rows(genesys_df, wiews_df)

#stage new fields for join below
gen_wiews_df$inst_id_gbif <- NA
gen_wiews_df$collection_code <- NA

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
    organization_name,
    organization_type,
    collection_code,
    STORAGE,
    SAMPSTAT,
    ORIGCTY,
    latitude,
    longitude,
    data_source) %>%
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
    inst_name = organization_name,
    inst_type = organization_type,
    collection_code = collection_code,
    basis_of_record = STORAGE,
    samp_stat = SAMPSTAT,
    orig_cty = ORIGCTY,
    data_source = data_source )


# --------------------------------------#
# DATASET 1. Genebank accession-level 
#---------------------------------------#

# Genesys/WIEWS (Genebank): 6,752,850 rows
# Genesys: 3,563,673 rows
# WIEWS: 3,189,177 rows
gen_wiews_genebank_df <- gen_wiews_df %>% filter(inst_type == "Genebank")

# GBIF Living (Genebank): 33,419 rows
gbif_living_genebank_df <- gbif_living_df %>% filter(organization_type == "Genebank")

#stage new fields for join below
gbif_living_genebank_df$acce_numb <- NA
gbif_living_genebank_df$doi <- NA
gbif_living_genebank_df$samp_stat <- NA

# Select and then rename only the specified fields
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
    inst_code,
    organization_name,
    organization_type,
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
    inst_name = organization_name,
    inst_type = organization_type,
    collection_code = collection_code,
    basis_of_record = basis_of_record,
    samp_stat = samp_stat,
    orig_cty = country_code,
    latitude = latitude,
    longitude = longitude,
    data_source = data_source )


# Genebank-accession-level dataset:  6,786,269 rows
genebank_accessionlevel_dataset <- rbind(gen_wiews_genebank_df, gbif_living_genebank_df)
#save
write.csv(genebank_accessionlevel_dataset, 'C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Final_datasets/genebank_accessionlevel_dataset_2026-05-29.csv', row.names = FALSE)


# ----------------------------------------#
# DATASET 2. Botanic garden accession-level 
#-----------------------------------------#

# Cano: 391,214 rows

#stage new fields in Cano for join below
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


# Genesys/WIEWS (Botanic garden): 97,855 rows
# Genesys: 51,907 rows
# WIEWS: 45,948 rows
gen_wiews_botanic_garden_df <- gen_wiews_df %>% filter(inst_type == "Botanic garden")

#stage new field for join below
gen_wiews_botanic_garden_df$LC <- NA

# Select and re-order fields
gen_wiews_botanic_garden_df <- gen_wiews_botanic_garden_df %>%
  select(wcfp_name_match, acce_numb, doi, taxa, genus_species, standardized_taxa_wfo, standardized_genus_wfo, standardized_genus_species_wfo,
         LC, inst_id_gbif, inst_code, inst_name, inst_type, collection_code, basis_of_record, 
         samp_stat, orig_cty, latitude, longitude, data_source )



# GBIF Living (Botanic garden): 69,239 rows
gbif_living_botanic_garden_df <- gbif_living_df %>% filter(organization_type == "Botanic garden")

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
    inst_code,
    organization_name,
    organization_type,
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
    inst_code = inst_code,
    inst_name = organization_name,
    inst_type = organization_type,
    collection_code = collection_code,
    basis_of_record = basis_of_record,
    samp_stat = samp_stat,
    orig_cty = country_code,
    latitude = latitude,
    longitude = longitude,
    data_source = data_source )


## Botanic garden accession-level dataset: 558,308 rows
botanicgarden_accessionlevel_dataset <- rbind(cano_df, gen_wiews_botanic_garden_df, gbif_living_botanic_garden_df)
#save
write.csv(botanicgarden_accessionlevel_dataset, 'C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/Final_datasets/botanicgarden_accessionlevel_dataset_2026-05-29.csv', row.names = FALSE)


# -------------------------------------------------#
# DATASET 3. Botanic garden species/institute-level
#--------------------------------------------------#

# BGCI PlantSearch: 618,544 rows

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
    organization_name,
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
    inst_name = organization_name,
    inst_type = organization_type,
    country_code = country_code,
    latitude = latitude,
    longitude = longitude,
    data_source = data_source )

## FINAL: Botanic garden species/inst level dataset: 618,544 rows
#save
write.csv(botanicgarden_specieslevel_dataset, 'C:/Users/sarah/My Drive/GCCFP_2026_NEW_processed_data/FINAL_DATA_2026_06_01/botanicgarden_specieslevel_dataset_2026-06-01.csv', row.names = FALSE)


## end script ##
