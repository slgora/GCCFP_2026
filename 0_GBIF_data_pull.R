# WCFP GBIF DATA PULL
# RELOAD RAW DATA 
# Date: 2026_01_20

# load packages
install.packages("vroom")
library(vroom)
library(readr)
library(dplyr)
library(readxl)
library(stringr)


# select only the columns we want to keep
cols_needed <- c(
  "scientificName",
  "basisOfRecord",
  "decimalLatitude",
  "decimalLongitude",
  "countryCode",
  "country",
  "stateProvince",
  "locality",
  "institutionCode",
  "collectionCode"
)

##################################
##### WCFP gbif data, PART 1 #####
##################################
# old wcfp plant list gbif data pull (located on the shared drive due to XL file size)
csv_path <- file.path(
  "G:/.shortcut-targets-by-id/1kQBvFIumhKQHnxSXNhr7w70dvuHgheEO",
  "GCCFP/OLDER/Data and analyses/Food plant distributions/GBIF_geo_data/gbif_raw_data/gbif_combined_df.csv"
)

##################################
##### WCFP gbif data, PART 2 #####
##################################
# NEW species gbif data pull
#csv_path <- file.path(
#  "C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/WCFP_plantlist_datasets/Data/gbif_chunks_new_species/gbif_new_species_combined_clean.csv")

# assign column type
WCFP_GBIF_data_all <- vroom(
  file = csv_path,
  col_select = all_of(cols_needed),
  col_types = cols(
    scientificName   = col_character(),
    basisOfRecord    = col_character(),
    decimalLatitude  = col_double(),
    decimalLongitude = col_double(),
    countryCode      = col_character(),
    country          = col_character(),
    stateProvince    = col_character(),
    locality         = col_character(),
    institutionCode  = col_character(),
    collectionCode   = col_character()
  ),
  progress = TRUE
)
#save OLD plantlist GBIF data
#write.csv(WCFP_GBIF_data_all, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/Raw_Genesys_all_data_export/WCFP_p1_GBIF_data_all_RAW_2026_01_20.csv', row.names = FALSE)

#save NEW SPECIES GBIF raw data (472,200 rows)
#write.csv(WCFP_GBIF_data_all, 'C:/Users/sarah/OneDrive/Desktop/GCCFP2/GCCFP2/Raw_Genesys_all_data_export/WCFP_p2_GBIF_data_all_RAW_2026_01_20.csv', row.names = FALSE)







# GBIF data- wcfp, p1 7,965,039 rows
WCFP_GBIF_data_p1 <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Raw_data/GBIF_wcfp_p1.csv")

WCFP_GBIF_data_p1 <- WCFP_GBIF_data_p1 %>%
  filter(basisOfRecord != "FOSSIL_SPECIMEN")

WCFP_GBIF_data_p1 <- WCFP_GBIF_data_p1 %>%
  mutate(data_source = "GBIF")

WCFP_GBIF_data_p1 <- WCFP_GBIF_data_p1 %>%
  rename(
    taxa = scientificName,
    basis_of_record = basisOfRecord,  # Renaming basisOfRecord
    latitude = decimalLatitude,       # Renaming decimalLatitude to latitude
    longitude = decimalLongitude,      # Renaming decimalLongitude to longitude
    country_code = countryCode,
    state_province = stateProvince,
    inst_code = institutionCode,
    collection_code = collectionCode )


# GBIF data- wcfp, p2 472,200 rows
WCFP_GBIF_data_p2 <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Raw_data/GBIF_wcfp_p2.xlsx")

WCFP_GBIF_data_p2 <- WCFP_GBIF_data_p2 %>%
  filter(basisOfRecord != "FOSSIL_SPECIMEN")

WCFP_GBIF_data_p2 <- WCFP_GBIF_data_p2 %>%
  mutate(data_source = "GBIF")

WCFP_GBIF_data_p2 <- WCFP_GBIF_data_p2 %>%
  rename(
    taxa = scientificName,
    basis_of_record = basisOfRecord,  # Renaming basisOfRecord
    latitude = decimalLatitude,       # Renaming decimalLatitude to latitude
    longitude = decimalLongitude,      # Renaming decimalLongitude to longitude
    country_code = countryCode,
    state_province = stateProvince,
    inst_code = institutionCode,
    collection_code = collectionCode )

# Combine old GBIF data + new species into a single data frame
# 7,965,639 rows
WCFP_GBIF_data_all <- rbind(WCFP_GBIF_data_p1, WCFP_GBIF_data_p2)

#save
write.csv(WCFP_GBIF_data_all, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/GBIF_data_all.csv', row.names = FALSE)

