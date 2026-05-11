# ---- Load all packages ----
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(stringr)

# ---- 1. Set today's date for Excel outputs ----
today_str <- format(Sys.Date(), "%Y-%m-%d")
metrics_output_file <- paste0("metrics_summary_", today_str, ".xlsx")
institutions_output_file <- "institution_summary_of_record_count.xlsx"

# ---- 2. Read in main datasets ----

# WCFP target list and plant name list
WCFP_plantlist <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/WCFP_plantlist/Standardized/WCFP_plantlist_standardized_2026-02-23.xlsx")
WCFP_plantlist_names <- WCFP_plantlist %>% select(taxon_name_accepted)
wcfp_total_species <- nrow(WCFP_plantlist_names)

# Genebank datasets
genebank_accessionlevel_dataset <- read.csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_genebank_accessionlevel_dataset_2026-03-13.csv', stringsAsFactors=FALSE) %>%
  select(wcfp_name_match, inst_code, data_source)

# Botanic garden accession- and species-level datasets
botanicgarden_accessionlevel_dataset <- read.csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_botanicgarden_accessionlevel_dataset_2026-03-13.csv', stringsAsFactors=FALSE) %>%
  select(wcfp_name_match, LC, inst_code, data_source)

botanicgarden_specieslevel_dataset <- read.csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_botanicgarden_specieslevel_dataset_2026-03-13.csv', stringsAsFactors=FALSE) %>%
  select(wcfp_name_match, inst_code, data_source)

# Occurrences dataset
occurrences_dataset <- read.csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_occurrences_dataset_2026-03-13.csv', stringsAsFactors=FALSE) %>%
  select(wcfp_name_match, inst_code, data_source)

# ---- Cano LC/inst_code harmonization ----
LC_inst_code_df <- data.frame(
  LC = c(
    "Lyon", "Bergen", "Oslo", "Wakehurst", "Dresden", "Meise", "Gothenburg", "Copenhagen",
    "Muenster", "Bogota", "SydneyRBG", "ICCP_RBGE", "Bonn", "Cartagena", "Desert", "Lund",
    "Clavijero", "Riga", "Inverleith_RBGE", "Kew", "CUBG_June2023", "JardinDesPlantes",
    "LaJaysinia", "Munich", "Tuebingen", "CJB", "Wespelaar", "BuenosAiresCT", "Valencia",
    "Toronto", "Salzburg", "Melbourne", "SydneyABG", "Versailles", "Denver", "Mobot",
    "Chicago", "Vancouver", "Dawes", "MBC", "SanDiego", "Holden", "LongwoodTandS",
    "Logan_RBGE", "Dawyck_RBGE", "Wales", "Benmore_RBGE", "Westonbirt", "Bedgebury",
    "Oxford", "Ontario", "LeHarve", "SUBG", "Stavanger", "Auckland", "Otari", "WellingtonBG",
    "ValRahmehMenton", "Cooktown", "SydneyBMBG", "Harmas", "Rogaland"
  ),
  inst_code = c(
    "3949", "NOR013", "NOR003", "GBR004", "DEU156", "BEL014", "SWE081", "DNK051",
    "DEU032", "COL090", "AUS107", "4566", "DEU038", "3866", "USA682", "SWE008",
    "MEX356", "LVA021", "GBR095", "GBR088", "GBR037", "FRA276", "FRA034",
    "DEU030", "DEU014", "CHE113", "BEL100", "ARG1311", "6769", "6322", "6212",
    "6194", "5726", "5679", "5583", "5477", "5452", "5421", "4751", "4744",
    "4701", "4697", "4677", "4572", "4571", "4565", "4563", "4560", "4552",
    "4515", "4475", "4406", "4305", "4196", "4186", "4185", "4183", "3938",
    "3739", "3736", "3638", "Rogaland"
  ),
  stringsAsFactors = FALSE
)

# Prepare Cano dataset if necessary (from botanicgarden_accessionlevel_dataset)
cano <- botanicgarden_accessionlevel_dataset %>%
  filter(data_source == "Cano") %>%
  left_join(LC_inst_code_df, by = "LC", suffix = c("", "_lookup")) %>%
  mutate(inst_code = coalesce(inst_code_lookup, inst_code)) %>%
  select(wcfp_name_match, inst_code, data_source)

# Harmonize inst_code for botanic garden accession-level
botanicgarden_accessionlevel_dataset <- botanicgarden_accessionlevel_dataset %>%
  left_join(LC_inst_code_df, by = "LC", suffix = c("", "_lookup")) %>%
  mutate(inst_code = coalesce(inst_code_lookup, inst_code)) %>%
  select(-inst_code_lookup) %>%
  select(wcfp_name_match, inst_code, data_source)

# ---- 3. Individual subsets ----
genesys_gb <- genebank_accessionlevel_dataset %>% filter(data_source == "Genesys") %>% select(wcfp_name_match, inst_code, data_source)
wiews_gb   <- genebank_accessionlevel_dataset %>% filter(data_source == "WIEWS") %>% select(wcfp_name_match, inst_code, data_source)
gbif_living_gb <- genebank_accessionlevel_dataset %>% filter(data_source == "GBIF_living") %>% select(wcfp_name_match, inst_code, data_source)
genesys_bg <- botanicgarden_accessionlevel_dataset %>% filter(data_source == "Genesys") %>% select(wcfp_name_match, inst_code, data_source)
wiews_bg <- botanicgarden_accessionlevel_dataset %>% filter(data_source == "WIEWS") %>% select(wcfp_name_match, inst_code, data_source)
gbif_living_bg <- botanicgarden_accessionlevel_dataset %>% filter(data_source == "GBIF_living") %>% select(wcfp_name_match, inst_code, data_source)
bgci   <- botanicgarden_specieslevel_dataset %>% select(wcfp_name_match, inst_code)
gbif_observations <- occurrences_dataset %>% filter(data_source == "GBIF_observations") %>% select(wcfp_name_match, inst_code, data_source)

# Fix types for join safety
for (nm in c("inst_code")) {
  botanicgarden_accessionlevel_dataset[[nm]] <- as.character(botanicgarden_accessionlevel_dataset[[nm]])
  botanicgarden_specieslevel_dataset[[nm]] <- as.character(botanicgarden_specieslevel_dataset[[nm]])
  bgci[[nm]] <- as.character(bgci[[nm]])
  genesys_gb[[nm]] <- as.character(genesys_gb[[nm]])
  wiews_gb[[nm]] <- as.character(wiews_gb[[nm]])
  gbif_living_gb[[nm]] <- as.character(gbif_living_gb[[nm]])
  cano[[nm]] <- as.character(cano[[nm]])
}



# ---- 4. Make metrics summary Excel ----

# Main long-tidy DFs
genebank_df <- genebank_accessionlevel_dataset %>% mutate(dataset = "genebanks")
botanicgarden_df <- bind_rows(
  botanicgarden_accessionlevel_dataset, 
  botanicgarden_specieslevel_dataset
) %>% mutate(dataset = "botanic_gardens")
combined <- bind_rows(genebank_df, botanicgarden_df)
all_gb <- unique(genebank_df$wcfp_name_match)
all_bg <- unique(botanicgarden_df$wcfp_name_match)
all_taxa <- union(all_gb, all_bg)

pct <- function(x) if (is.na(x)) NA else round(100 * x / wcfp_total_species, 1)

dataset_tbls <- list(
  genebank_accessionlevel_dataset = genebank_accessionlevel_dataset,
  botanicgarden_accessionlevel_dataset = botanicgarden_accessionlevel_dataset,
  botanicgarden_specieslevel_dataset = botanicgarden_specieslevel_dataset,
  occurrences_dataset = occurrences_dataset,
  Genesys = bind_rows(genesys_bg, genesys_gb),
  WIEWS = bind_rows(wiews_bg, wiews_gb),
  Cano = cano,
  GBIF_living = bind_rows(gbif_living_bg, gbif_living_gb),
  GBIF_observations = gbif_observations,
  BGCI = bgci
)
get_species <- function(df) unique(df$wcfp_name_match)
get_inst <- function(df) unique(df$inst_code)
accessions <- function(df) nrow(df)
distinct_taxa <- function(df) n_distinct(df$wcfp_name_match)
distinct_inst <- function(df) n_distinct(df$inst_code)
unique_to_this <- function(df, others) length(setdiff(get_species(df), unique(unlist(lapply(others, get_species)))))
taxa_not_in <- function(df) sum(!(WCFP_plantlist_names$taxon_name_accepted %in% get_species(df)))
percent_this <- function(df) pct(distinct_taxa(df))
percent_unique <- function(df, others) pct(unique_to_this(df, others))
percent_not_in <- function(df) pct(taxa_not_in(df))

# Pre-calculate group-level metrics:
n_accessions_genebanks    <- nrow(genebank_accessionlevel_dataset)
n_accessions_botanicgardens <- nrow(botanicgarden_accessionlevel_dataset)
n_accessions_combined     <- n_accessions_genebanks + n_accessions_botanicgardens
n_distinct_taxa_genebanks      <- n_distinct(genebank_df$wcfp_name_match)
n_distinct_taxa_botanicgardens <- n_distinct(botanicgarden_df$wcfp_name_match)
n_distinct_taxa_combined       <- n_distinct(c(genebank_df$wcfp_name_match, botanicgarden_df$wcfp_name_match))
n_unique_taxa_genebanks        <- length(setdiff(all_gb, all_bg))
n_unique_taxa_botanicgardens   <- length(setdiff(all_bg, all_gb))
n_taxa_in_both                 <- length(intersect(all_gb, all_bg))
n_taxa_not_in_data             <- nrow(WCFP_plantlist_names %>% filter(!taxon_name_accepted %in% all_taxa))
n_distinct_inst_genebanks      <- n_distinct(genebank_df$inst_code)
n_distinct_inst_botanicgardens <- n_distinct(botanicgarden_df$inst_code)
n_distinct_inst_combined       <- n_distinct(c(genebank_df$inst_code, botanicgarden_df$inst_code))

summarystats <- tibble(
  metric = c(
    "Number of accessions",
    "Number of records",
    "Number of distinct institutions",
    "Number of distinct WCFP species",
    "Percent of total distinct WCFP species",
    "Number of distinct WCFP species unique to data",
    "Percent of total distinct WCFP species unique to data",
    "Number of distinct WCFP species NOT in data",
    "Percent of total distinct WCFP species NOT in data"
  ),
  genebanks = c(
    n_accessions_genebanks,
    n_accessions_genebanks,
    n_distinct_inst_genebanks,
    n_distinct_taxa_genebanks,
    pct(n_distinct_taxa_genebanks),
    n_unique_taxa_genebanks,
    pct(n_unique_taxa_genebanks),
    n_taxa_not_in_data,
    pct(n_taxa_not_in_data)
  ),
  botanic_gardens = c(
    n_accessions_botanicgardens,
    n_accessions_botanicgardens,
    n_distinct_inst_botanicgardens,
    n_distinct_taxa_botanicgardens,
    pct(n_distinct_taxa_botanicgardens),
    n_unique_taxa_botanicgardens,
    pct(n_unique_taxa_botanicgardens),
    n_taxa_not_in_data,
    pct(n_taxa_not_in_data)
  ),
  BOTH_genebanks_and_botanic_gardens = c(
    n_accessions_combined,
    n_accessions_combined,
    n_distinct_inst_combined,
    n_distinct_taxa_combined,
    pct(n_distinct_taxa_combined),
    n_taxa_in_both,
    pct(n_taxa_in_both),
    n_taxa_not_in_data,
    pct(n_taxa_not_in_data)
  ),
  NEITHER_genebanks_or_botanic_gardens = c(
    NA, NA, NA, NA, NA, NA, NA, n_taxa_not_in_data, pct(n_taxa_not_in_data)
  )
)
for (datnm in names(dataset_tbls)) {
  others <- dataset_tbls[names(dataset_tbls) != datnm]
  rowvals <- c(
    accessions(dataset_tbls[[datnm]]),
    accessions(dataset_tbls[[datnm]]),
    distinct_inst(dataset_tbls[[datnm]]),
    distinct_taxa(dataset_tbls[[datnm]]),
    percent_this(dataset_tbls[[datnm]]),
    unique_to_this(dataset_tbls[[datnm]], others),
    percent_unique(dataset_tbls[[datnm]], others),
    taxa_not_in(dataset_tbls[[datnm]]),
    percent_not_in(dataset_tbls[[datnm]])
  )
  summarystats[[datnm]] <- rowvals
}

records_species <- combined %>%
  group_by(WCFP_species = wcfp_name_match, dataset) %>%
  summarise(records = n(), .groups = "drop") %>%
  filter(dataset %in% c("genebanks", "botanic_gardens")) %>%
  pivot_wider(names_from = dataset,
              values_from = records,
              values_fill = 0) %>%
  rename(
    number_of_records_in_genebanks = genebanks,
    number_of_records_in_botanicgardens = botanic_gardens
  ) %>%
  arrange(desc(number_of_records_in_genebanks))

accession_species <- combined %>%
  group_by(WCFP_species = wcfp_name_match, dataset) %>%
  summarise(accessions = n(), .groups = "drop") %>%
  filter(dataset %in% c("genebanks", "botanic_gardens")) %>%
  pivot_wider(names_from = dataset,
              values_from = accessions,
              values_fill = 0) %>%
  rename(
    number_of_accessions_in_genebanks = genebanks,
    number_of_accessions_in_botanicgardens = botanic_gardens
  ) %>%
  arrange(desc(number_of_accessions_in_genebanks))

genebank_species <- genebank_df %>%
  group_by(WCFP_species = wcfp_name_match) %>%
  summarise(number_of_accessions_in_genebanks = n(), .groups = "drop") %>%
  arrange(desc(number_of_accessions_in_genebanks))

botanic_garden_species <- botanicgarden_df %>%
  group_by(WCFP_species = wcfp_name_match) %>%
  summarise(number_of_records_in_botanicgardens = n(), .groups = "drop") %>%
  arrange(desc(number_of_records_in_botanicgardens))

species_only_in_genebanks <- genebank_df %>%
  filter(!wcfp_name_match %in% all_bg) %>%
  group_by(WCFP_species = wcfp_name_match) %>%
  summarise(number_of_records_in_genebanks = n(), .groups = "drop") %>%
  arrange(desc(number_of_records_in_genebanks))

species_only_in_botanic_gardens <- botanicgarden_df %>%
  filter(!wcfp_name_match %in% all_gb) %>%
  group_by(WCFP_species = wcfp_name_match) %>%
  summarise(number_of_records_in_botanicgardens = n(), .groups = "drop") %>%
  arrange(desc(number_of_records_in_botanicgardens))

wcfp_species_not_in_either <- WCFP_plantlist_names %>%
  filter(!taxon_name_accepted %in% all_taxa) %>%
  rename(WCFP_species = taxon_name_accepted)

# ---- Write metrics summary Excel ----
write_xlsx(list(
  "summary stats"                     = summarystats,
  "records counts by species"          = records_species,
  "accession_counts_by_species"        = accession_species,
  "genebank_species"                  = genebank_species,
  "botanic_garden_species"            = botanic_garden_species,
  "species_only_in_genebanks"         = species_only_in_genebanks,
  "species_only_in_botanic_gardens"   = species_only_in_botanic_gardens,
  "wcfp_species_not_in_either"        = wcfp_species_not_in_either
), path = metrics_output_file)
cat("Excel file created:", metrics_output_file, "\n")




# ---- 5. Institution summary of record count Excel ----
# Read institution location file
institution_locations <- read_excel("C:/Users/sarah/Downloads/institution_locations_dataset.xlsx")

# Prep counts for each data source
bgci_counts <- bgci %>%
  group_by(inst_code) %>%
  summarise(record_count_BGCI = n(), .groups = "drop")
genesys_counts <- genesys_gb %>%
  group_by(inst_code) %>%
  summarise(accession_count_Genesys = n(), .groups = "drop")
wiews_counts <- wiews_gb %>%
  group_by(inst_code) %>%
  summarise(accession_count_WIEWS = n(), .groups = "drop")
cano_counts <- cano %>%
  group_by(inst_code) %>%
  summarise(accession_count_Cano = n(), .groups="drop")
gbif_counts <- gbif_living_gb %>%
  group_by(inst_code) %>%
  summarise(accession_count_GBIF_living = n(), .groups="drop")

institutions_summary <- institution_locations %>%
  mutate(
    institution_found_in_data =
      if_else(
        inst_code %in% bgci_counts$inst_code |
          inst_code %in% genesys_counts$inst_code |
          inst_code %in% wiews_counts$inst_code |
          inst_code %in% cano_counts$inst_code |
          inst_code %in% gbif_counts$inst_code,
        "Y", "N"
      )
  ) %>%
  left_join(bgci_counts,         by="inst_code") %>%
  left_join(genesys_counts,      by="inst_code") %>%
  left_join(wiews_counts,        by="inst_code") %>%
  left_join(cano_counts,         by="inst_code") %>%
  left_join(gbif_counts,         by="inst_code") %>%
  mutate(across(matches("count"), ~replace_na(., 0)))

# ---- Write institution summary Excel ----
write_xlsx(list(institutions_summary = institutions_summary),
           path = institutions_output_file)
cat("Output written to", institutions_output_file, "\n")