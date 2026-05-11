# ---- 0. Load packages ----
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(stringr)

# ---- 1. Set today's date for Excel output ----
today_str <- format(Sys.Date(), "%Y-%m-%d")
output_file <- paste0("metrics_summary_", today_str, ".xlsx")

# ---- 2. Read in target list ----
WCFP_plantlist <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/WCFP_plantlist/Standardized/WCFP_plantlist_standardized_2026-02-23.xlsx")
WCFP_plantlist_names <- WCFP_plantlist %>% select(taxon_name_accepted)
wcfp_total_species <- nrow(WCFP_plantlist_names)

# ---- 3. Read in all main datasets (edit paths as needed) ----
genebank_accessionlevel_dataset <- read.csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_genebank_accessionlevel_dataset_2026-03-13.csv', stringsAsFactors=FALSE)
genebank_accessionlevel_dataset <- genebank_accessionlevel_dataset %>% 
  select(wcfp_name_match, inst_code, data_source)

botanicgarden_accessionlevel_dataset <- read.csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_botanicgarden_accessionlevel_dataset_2026-03-13.csv', stringsAsFactors=FALSE)
botanicgarden_accessionlevel_dataset <- botanicgarden_accessionlevel_dataset %>% 
  select(wcfp_name_match, LC, inst_code, data_source)

botanicgarden_specieslevel_dataset <- read.csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_botanicgarden_specieslevel_dataset_2026-03-13.csv', stringsAsFactors=FALSE)
botanicgarden_specieslevel_dataset <- botanicgarden_specieslevel_dataset %>%
  select(wcfp_name_match, inst_code, data_source)

occurrences_dataset <- read.csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_occurrences_dataset_2026-03-13.csv', stringsAsFactors=FALSE)
occurrences_dataset <- occurrences_dataset %>% select(wcfp_name_match, inst_code, data_source)

# ---- 4. Data harmonization for Cano/LC mapping and inst_code ----
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

if (exists("cano")) {
  cano <- cano %>%
    left_join(LC_inst_code_df, by = "LC", suffix = c("", "_lookup")) %>%
    mutate(inst_code = coalesce(inst_code_lookup, inst_code)) %>%
    select(-inst_code_lookup) %>%
    select(wcfp_name_match, inst_code, data_source)
}

botanicgarden_accessionlevel_dataset <- botanicgarden_accessionlevel_dataset %>%
  left_join(LC_inst_code_df, by = "LC", suffix = c("", "_lookup")) %>%
  mutate(inst_code = coalesce(inst_code_lookup, inst_code)) %>%
  select(-inst_code_lookup) %>%
  select(wcfp_name_match, inst_code, data_source)

# ---- 5. Individual source subsets for summary stats ----
genesys_gb <- genebank_accessionlevel_dataset %>% filter(data_source == "Genesys")
wiews_gb <- genebank_accessionlevel_dataset %>% filter(data_source == "WIEWS")
gbif_living_gb <- genebank_accessionlevel_dataset %>% filter(data_source == "GBIF_living")
genesys_bg <- botanicgarden_accessionlevel_dataset %>% filter(data_source == "Genesys")
wiews_bg <- botanicgarden_accessionlevel_dataset %>% filter(data_source == "WIEWS")
gbif_living_bg <- botanicgarden_accessionlevel_dataset %>% filter(data_source == "GBIF_living")
bgci <- botanicgarden_specieslevel_dataset
gbif_observations <- occurrences_dataset %>% filter(data_source == "GBIF_observations")

# Fix types
for (nm in c("inst_code")) {
  botanicgarden_accessionlevel_dataset[[nm]] <- as.character(botanicgarden_accessionlevel_dataset[[nm]])
  botanicgarden_specieslevel_dataset[[nm]] <- as.character(botanicgarden_specieslevel_dataset[[nm]])
  bgci[[nm]] <- as.character(bgci[[nm]])
}

# --- 6. Main combined long-tidy datasets for reporting ---
genebank_df <- genebank_accessionlevel_dataset %>% mutate(dataset = "genebanks")
botanicgarden_df <- bind_rows(
  botanicgarden_accessionlevel_dataset, 
  botanicgarden_specieslevel_dataset
) %>% mutate(dataset = "botanic_gardens")
combined <- bind_rows(genebank_df, botanicgarden_df)

# --- 7. Utility sets for species uniqueness logic ---
all_gb <- unique(genebank_df$wcfp_name_match)
all_bg <- unique(botanicgarden_df$wcfp_name_match)
all_taxa <- union(all_gb, all_bg)

# --- 8. Define helper functions for summary calculations ---
pct <- function(x) if (is.na(x)) NA else round(100 * x / wcfp_total_species, 1)

# ---- 9. BUILD ALL 8 SHEETS ----

# 1. summary stats -----------------
dataset_tbls <- list(
  genebank_accessionlevel_dataset = genebank_accessionlevel_dataset,
  botanicgarden_accessionlevel_dataset = botanicgarden_accessionlevel_dataset,
  botanicgarden_specieslevel_dataset = botanicgarden_specieslevel_dataset,
  occurrences_dataset = occurrences_dataset,
  Genesys = bind_rows(genesys_bg, genesys_gb),
  WIEWS = bind_rows(wiews_bg, wiews_gb),
  Cano = if (exists("cano")) cano else tibble(wcfp_name_match=character(), inst_code=character(), data_source=character()),
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

# Some metrics for "vs Botanic gardens"
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

# 2. records counts by species ----
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

# 3. accession_counts_by_species ---
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

# 4. genebank_species ----
genebank_species <- genebank_df %>%
  group_by(WCFP_species = wcfp_name_match) %>%
  summarise(number_of_accessions_in_genebanks = n(), .groups = "drop") %>%
  arrange(desc(number_of_accessions_in_genebanks))

# 5. botanic_garden_species ----
botanic_garden_species <- botanicgarden_df %>%
  group_by(WCFP_species = wcfp_name_match) %>%
  summarise(number_of_records_in_botanicgardens = n(), .groups = "drop") %>%
  arrange(desc(number_of_records_in_botanicgardens))

# 6. species_only_in_genebanks ----
species_only_in_genebanks <- genebank_df %>%
  filter(!wcfp_name_match %in% all_bg) %>%
  group_by(WCFP_species = wcfp_name_match) %>%
  summarise(number_of_records_in_genebanks = n(), .groups = "drop") %>%
  arrange(desc(number_of_records_in_genebanks))

# 7. species_only_in_botanic_gardens ----
species_only_in_botanic_gardens <- botanicgarden_df %>%
  filter(!wcfp_name_match %in% all_gb) %>%
  group_by(WCFP_species = wcfp_name_match) %>%
  summarise(number_of_records_in_botanicgardens = n(), .groups = "drop") %>%
  arrange(desc(number_of_records_in_botanicgardens))

# 8. wcfp_species_not_in_either ----
wcfp_species_not_in_either <- WCFP_plantlist_names %>%
  filter(!taxon_name_accepted %in% all_taxa) %>%
  rename(WCFP_species = taxon_name_accepted)

# ---- Final: Write all Excel sheets ----
write_xlsx(list(
  "summary stats"                     = summarystats,
  "records counts by species"          = records_species,
  "accession_counts_by_species"        = accession_species,
  "genebank_species"                  = genebank_species,
  "botanic_garden_species"            = botanic_garden_species,
  "species_only_in_genebanks"         = species_only_in_genebanks,
  "species_only_in_botanic_gardens"   = species_only_in_botanic_gardens,
  "wcfp_species_not_in_either"        = wcfp_species_not_in_either
), path = output_file)

cat("Excel file created:", output_file, "\n")