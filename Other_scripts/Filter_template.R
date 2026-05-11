###############################################################
#    SPECIES LIST AND COUNTRY FILTERING TEMPLATE SCRIPT       #
###############################################################
#
#      *** HIGH PERFORMANCE WORKFLOW FOR MILLIONS OF RECORDS ***
#   This script implements efficient, vectorized filtering routines 
#   optimized for very large datasets—including those with   
#   millions of records and names. All operations use robust 
#   batch/memory-safe R approaches (dplyr, vectorized functions, sf spatial join).
#
#
# This script performs multiple kinds of data filtering to assist with:
#
#  1. **Exact Species Name Match**:
#      - Flags and outputs all records in your dataset with a species name that exactly matches
#        any entry in your reference species list (field & column fully configurable).
#
#  2. **Genus + Species Match (Normalized)**:
#      - Normalizes both species list and dataset names by extracting only the genus and species
#        (strips infraspecific ranks like variety, subspecies, etc).
#      - Flags and outputs records where genus+species match, allowing for more inclusive/relaxed matching.
#
#  3. **Normalized Taxon Match (All Ranks Retained)**:
#      - Normalizes names by standardizing formatting (capitalization, punctuation, symbol handling) while
#        retaining infraspecific structure (e.g., varieties and subspecies).
#      - Matches and flags dataset records by complete taxon, e.g., "Quercus robur var. fastigiata".
#
#  4. **Country Filtering by Latitude/Longitude (Spatial)**:
#      - Assigns a country to each dataset record with coordinate data, using GIS operations for precise placement.
#      - If a point falls outside any official country polygon (e.g., in border lakes, islands…),
#        it assigns the nearest country within a configurable buffer (default 20km).
#      - Lets you filter your dataset for specific countries of interest, by name.
#
# For each filtering strategy, the script:
#   - Adds an appropriate flag to your dataset ("Y" or "N" for match).
#   - Saves out both the "matched" and "non-matched" (for taxon, genus/species, exact name)
#     or the country-filtered subset (for spatial) as new .csv files.
#
# ********************
#      Usage
# ********************
# 1. Adjust the USER INPUTS section to set file paths, column names, and country list.
# 2. Source or run script.
# 3. Check your output directory for the results.
#
# Dependencies: sf, rnaturalearth, rnaturalearthdata, dplyr, stringr, readr, readxl, purrr
#
###############################################################

#install packages if needed
# install.packages(c("sf", "rnaturalearth", "rnaturalearthdata", "dplyr", "readr", "readxl", "stringr", "purrr"))

# load libraries
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(sf)
library(rnaturalearth)

################################################################
# 1. USER INPUTS: FILES AND COLUMN NAMES
################################################################

species_list_path    <- "C:/path/to/your/species_list.xlsx"
species_column       <- "species_name"       # in species list
dataset_path         <- "C:/path/to/your/main_dataset.csv"
dataset_species_col  <- "species"            # in dataset

lat_col <- "decimalLatitude"
lon_col <- "decimalLongitude"

output_exact_match            <- "matched_species_dataset.csv"
output_exact_nonmatch         <- "nonmatched_species_dataset.csv"
output_genus_species_match    <- "matched_by_genus_species.csv"
output_genus_species_nonmatch <- "nonmatched_by_genus_species.csv"
output_norm_taxa_match        <- "matched_by_norm_taxa.csv"
output_norm_taxa_nonmatch     <- "nonmatched_by_norm_taxa.csv"
output_country_filtered       <- "filtered_by_country.csv"

filter_countries <- c("Kenya", "Tanzania")   # COUNTRY NAMES TO KEEP

country_output_col <- "Assigned_Country"
country_buffer_km <- 20           # buffer for nearest country (km)

cat("==== MASTER FILTERING START ====\n\n")

################################################################
# 2. LOAD DATA
################################################################

species_list <- read_excel(species_list_path)
cat("Loaded species list with", nrow(species_list), "rows\n")
dataset <- read_csv(dataset_path)
cat("Loaded dataset with", nrow(dataset), "rows\n\n")

species_list[[species_column]]   <- trimws(as.character(species_list[[species_column]]))
dataset[[dataset_species_col]]   <- trimws(as.character(dataset[[dataset_species_col]]))

################################################################
# 3. === EXACT NAME MATCHING ===
################################################################
cat("[1] Filtering by exact species name...\n")

species_lookup <- unique(na.omit(species_list[[species_column]]))
dataset$species_flag <- ifelse(
  dataset[[dataset_species_col]] %in% species_lookup, "Y", "N"
)

exact_matches <- dataset[dataset$species_flag == "Y", ]
exact_nonmatches <- dataset[dataset$species_flag == "N", ]

write.csv(exact_matches, output_exact_match, row.names = FALSE)
write.csv(exact_nonmatches, output_exact_nonmatch, row.names = FALSE)

cat("   Exact match:     ", nrow(exact_matches), "records\n")
cat("   Non-match:       ", nrow(exact_nonmatches), "records\n\n")

################################################################
# 4. === GENUS + SPECIES NORMALIZED MATCHING ===
################################################################
cat("[2] Filtering by genus + species (normalized)...\n")

extract_genus_species <- function(name) {
  name %>%
    str_to_lower() %>%
    str_replace_all("\\b[×x]\\b", " × ") %>%
    str_replace_all("\\+", " + ") %>%
    str_replace_all("\\b(sect|subsect|subsp|var|f|subf|aff|cf)\\.", "") %>%
    str_squish() %>%
    str_split("\\s+") %>%
    map_chr(~ {
      tokens <- .x[.x != "×" & .x != "+"]
      if (length(tokens) >= 2) paste(tokens[1:2], collapse = " ") else NA_character_
    })
}
species_list$genus_species <- extract_genus_species(species_list[[species_column]])
dataset$genus_species      <- extract_genus_species(dataset[[dataset_species_col]])

genus_species_lookup <- unique(na.omit(species_list$genus_species))
dataset$genus_species_flag <- ifelse(
  dataset$genus_species %in% genus_species_lookup, "Y", "N"
)

genus_species_matches    <- dataset[dataset$genus_species_flag == "Y", ]
genus_species_nonmatches <- dataset[dataset$genus_species_flag == "N", ]

write.csv(genus_species_matches, output_genus_species_match, row.names = FALSE)
write.csv(genus_species_nonmatches, output_genus_species_nonmatch, row.names = FALSE)

cat("   Genus+species match:     ", nrow(genus_species_matches), "records\n")
cat("   Genus+species non-match: ", nrow(genus_species_nonmatches), "records\n\n")

################################################################
# 5. === NORMALIZED TAXON MATCHING (VARIETIES/SUBSPECIES/etc.) ===
################################################################
cat("[3] Filtering by normalized taxon name (retaining infra ranks)...\n")

norm_taxa <- function(name) {
  name %>%
    str_replace_all("'[^']*'", "") %>%
    str_replace_all("\\b[×x]\\b", " × ") %>%
    str_replace_all("\\+", " + ") %>%
    str_replace_all("\\.", "") %>%
    str_squish() %>%
    str_split("\\s+") %>%
    map_chr(~ {
      tokens <- .x[.x != "×" & .x != "+"]
      if (length(tokens) >= 1) tokens[1] <- str_to_title(tokens[1])
      if (length(tokens) >= 2) tokens[2:length(tokens)] <- str_to_lower(tokens[2:length(tokens)])
      rank_abbrevs <- c("var", "subsp", "f", "subf", "sect", "subsect", "ser", "subser",
                        "aff", "cf", "nothovar", "nothosubsp", "nothof", "aggr",
                        "sensu", "sl", "ss", "ssp", "cv", "forma", "tribe",
                        "subtribe", "convar")
      rank_index <- which(str_to_lower(tokens) %in% rank_abbrevs)[1]
      if (!is.na(rank_index) && length(tokens) >= rank_index + 1) {
        paste(c(tokens[1:2], paste0(str_to_lower(tokens[rank_index]), "."), tokens[rank_index + 1]), collapse = " ")
      } else if (length(tokens) >= 2) {
        paste(tokens[1:2], collapse = " ")
      } else {
        NA_character_
      }
    })
}
species_list$norm_taxa <- norm_taxa(species_list[[species_column]])
dataset$norm_taxa      <- norm_taxa(dataset[[dataset_species_col]])

norm_taxa_lookup <- unique(na.omit(species_list$norm_taxa))
dataset$norm_taxa_flag <- ifelse(
  dataset$norm_taxa %in% norm_taxa_lookup, "Y", "N"
)

norm_taxa_matches    <- dataset[dataset$norm_taxa_flag == "Y", ]
norm_taxa_nonmatches <- dataset[dataset$norm_taxa_flag == "N", ]

write.csv(norm_taxa_matches, output_norm_taxa_match, row.names = FALSE)
write.csv(norm_taxa_nonmatches, output_norm_taxa_nonmatch, row.names = FALSE)

cat("   Norm. taxon match:     ", nrow(norm_taxa_matches), "records\n")
cat("   Norm. taxon non-match: ", nrow(norm_taxa_nonmatches), "records\n\n")

################################################################
# 6. === ASSIGN COUNTRY BY LAT/LON, FILTER BY COUNTRY =========
################################################################
cat("[4] Assign country to coordinates and filter...\n")

# Prepare points as sf geometry
sf_points <- st_as_sf(
  dataset,
  coords = c(lon_col, lat_col),
  crs = 4326,
  remove = FALSE
)
world_countries <- ne_countries(scale = "medium", returnclass = "sf")

# Spatial join: assign country by polygon
joined <- st_join(sf_points, world_countries[, c("name_long", "iso_a2")], left = TRUE, largest = FALSE)

# Nearest country by buffered border (if needed)
n_missing <- sum(is.na(joined$name_long))
cat("     Points outside any country polygon:", n_missing, "\n")

if (n_missing > 0) {
  buffer_meters <- country_buffer_km * 1000
  world_countries_buf <- st_transform(world_countries, 3857) %>% st_buffer(buffer_meters)
  joined_buf <- st_transform(sf_points[is.na(joined$name_long), ], 3857)
  nearest <- st_join(joined_buf, world_countries_buf[, c("name_long", "iso_a2")], left = TRUE, largest = TRUE)
  nearest <- st_transform(nearest, 4326)
  joined$name_long[is.na(joined$name_long)] <- nearest$name_long
  joined$iso_a2[is.na(joined$iso_a2)] <- nearest$iso_a2
}
dataset[[country_output_col]] <- joined$name_long
cat("   Countries assigned (including nearest-country correction as needed).\n\n")

# Example: filter and save by country
filtered_by_country <- dataset %>% filter(Assigned_Country %in% filter_countries)
write.csv(filtered_by_country, output_country_filtered, row.names = FALSE)
cat("   Filtered by country. Rows kept:", nrow(filtered_by_country), "\n\n")

################################################################
# 7. === SUMMARY OF FILTERING METHODS ===
################################################################
cat("==== ALL FILTERING COMPLETE! ====\n\n")
cat("OUTPUTS:\n")
cat("   [1] By exact name:             ", output_exact_match, " & ", output_exact_nonmatch, "\n")
cat("   [2] By genus+species:          ", output_genus_species_match, " & ", output_genus_species_nonmatch, "\n")
cat("   [3] By normalized taxon:       ", output_norm_taxa_match, " & ", output_norm_taxa_nonmatch, "\n")
cat("   [4] By country (lat/lon):      ", output_country_filtered, "\n")
cat("\n")
cat("NOTES:\n")
cat("  - EXACT NAME: Full string match with your species list.\n")
cat("  - GENUS+SPECIES: Normalized to just 'genus species' for match, ignores varieties/subspecies.\n")
cat("  - NORMALIZED TAXON: Preserves infra ranks (varieties/subspecies etc), match after cleaning/capitalization.\n")
cat("  - COUNTRY (SPATIAL): Assigns country by coordinates and lets you filter by country (even if a point is outside strict borders, nearest country is assigned by buffer).\n")