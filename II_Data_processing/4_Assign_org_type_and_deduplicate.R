# Annotate data with organization type, remove selected organizations, remove duplicates between data



# 1. Annotate data with organization type
#      Botanic garden
#      Genebank
# 2. Remove non-genebank and non-botanic garden organizations
# 3. Remove duplicates between datasets
#      guide file flags duplicates to remove based on counts per org by data source

# Load libraries
library(dplyr)
library(readr)
library(readxl)
library(writexl)

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

# BGCI: 626,307 rows
bgci_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/BGCI_data_prepped_2026-03-09.csv")

# Cano: 391,214 rows
cano_df <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/Cano_data_prepped_2026-03-09.csv")

# GBIF LIVING: 118,198 rows
gbif_living <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_living_2026-03-11.csv")

# GBIF OBSERVATIONS: 6,701,782 rows
gbif_observations <- read_csv <- ("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_observations_2026-03-11.csv")


# ------------------------------------------------------------ #
# ----------- Summary of organizations guide file ------------ #

# Define the organization types of interest
all_org_summary <- all_org %>%
  mutate(
    # Classify organization type
    org_type_group = case_when(
      organization_type == "Genebank"                              ~ "Genebank",
      organization_type == "Botanic garden"                        ~ "Botanic garden",
      is.na(organization_type) | trimws(organization_type) == ""  ~ "No organization type assigned",
      TRUE                                                         ~ "Other"
    ),
    # Flag if coordinates are available
    has_coords = !is.na(latitude) & !is.na(longitude) & latitude != "" & longitude != ""
  ) %>%
  group_by(org_type_group, data_source) %>%
  summarise(
    total_organizations = n(),
    with_coords         = sum(has_coords),
    without_coords      = sum(!has_coords),
    .groups = "drop"
  ) %>%
  arrange(org_type_group, data_source)

# View the summary table
print(all_org_summary)


# ----------------------------------------------------------------------------- #
# ---- Filter organization guide file for organizations with type assigned ---- #

# --- Filter for FAO WIEWS organizations with type assigned ---

# FAO WIEWS ORGANIZATIONS: 18,662 organizations
all_org_wiews <- all_org %>% 
  filter(data_source == "WIEWS") %>%
  select(inst_code, organization_name, organization_type) %>%
  mutate(
    inst_code         = trimws(inst_code),  #trim whitespace for match
    organization_name = trimws(organization_name),
    organization_type = trimws(organization_type)
  ) %>%
  rename(organization_name_WIEWS = organization_name)

# How many organizations HAVE organization_type assigned: 3,221
# Filter for rows where organization_type is "Genebank" or "Botanic garden"
assigned <- all_org_wiews %>%
  filter(organization_type %in% c("Genebank", "Botanic garden"))

# FAO WIEWS ORGANIZATIONS in DATA: 932 organizations
all_org_wiews_in_data <- all_org %>% 
  filter(data_source == "WIEWS") %>%
  select(inst_code, organization_type, organization_found_in_data) %>%
  mutate(
    inst_code         = trimws(inst_code),  #trim whitespace for match
    organization_type = trimws(organization_type) ) %>%
  filter(!is.na(organization_found_in_data) & trimws(organization_found_in_data) != "")

# FAO WIEWS ORGANIZATIONS in DATA with org type assigned: 921 organizations
all_org_wiews_in_data_assigned <- all_org_wiews_in_data %>%
  filter(!is.na(organization_type) & trimws(organization_type) != "")


# --- Filter for BGCI GardenSearch organizations with type assigned ---

# BGCI GARDEN SEARCH ORGANIZATIONS: 3,614 organizations
all_org_bgci <- all_org %>% 
  filter(data_source == "BGCI") %>%
  select(inst_code, organization_name, organization_type) %>%
  mutate(
    inst_code         = trimws(inst_code),  #trim whitespace for match
    organization_name = trimws(organization_name),
    organization_type = trimws(organization_type)
  ) %>%
  rename(organization_name_BGCI = organization_name)

# Check how many rows have organization_type not assigned: 1 (Seeds of Success, INSTCODE = 4604)
not_assigned <- all_org_bgci %>%
  filter(is.na(organization_type) | organization_type == "" | organization_type == "NA")

# DROP NON-BOTANIC GARDENS: 
# 1. Drop SEEDS OF SUCCESS
all_org_bgci <- all_org_bgci %>% filter(inst_code != 4606)
# 2. Drop U.S. National Plant Germplasm System (Genebank)
all_org_bgci <- all_org_bgci %>% filter(inst_code != 4678)

# 3,612 organizations
nrow(all_org_bgci)


# ----------- ASSIGN ORGANIZATION TYPE to BGCI ------------#

# BGCI data: rename ex_situ_garden_id = inst_code
bgci_df <- bgci_df %>% rename(inst_code = ex_situ_garden_id)
bgci_df <- bgci_df %>% mutate(inst_code = trimws(inst_code)) #trim whitespace for match

# ASSIGN ORG_TYPE TO BGCI
bgci_df <- all_org_bgci %>%
  right_join(bgci_df, by = c("inst_code"))

# Confirm rows in gen_wiews_df where no organization type is assigned: 7,763 rows
bgci_no_org_type <- bgci_df %>%
  filter(is.na(organization_type) | trimws(organization_type) == "")

# Show distinct inst_code where no organization type is assigned, 2 org
no_org_type_df <- bgci_df %>%
  filter(is.na(organization_type) | trimws(organization_type) == "") %>%
  select(inst_code) %>%
  distinct()

# Drop Seeds of success from BGCI data, INSTCODE = 4606 
bgci_df <- bgci_df[bgci_df$inst_code != 4606, ]
# Drop U.S. National Plant Germplasm System (Genebank), INSTCODE = 4678
bgci_df <- bgci_df[bgci_df$inst_code != 4678, ]

# rename for save
bgci_df_assigned <- bgci_df

# RESULT:
# BGCI data with org type assigned: 618,544 rows
write.csv(bgci_df_assigned, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Deduplicated_with_org_assigned/bgci_org_assigned_2026-03-10.csv', row.names = FALSE)


# Create a summary dataframe of BGCI row counts by inst_code
inst_code_summary <- bgci_df %>%
  group_by(inst_code, organization_type) %>%
  summarise(
    total_records_bgci    = n(),
    .groups = "drop") 

# ADD BGCI INSTCODE SUMMARY TO ALL ORG
all_org_with_bgci_counts <- inst_code_summary %>%
  right_join(all_org, by = c("inst_code"))

# 1,060 org found in BGCI
# save count of records in BGCI
write_xlsx(all_org_with_bgci_counts, "C:/Users/sarah/Downloads/all_org_with_bgci_counts.xlsx")


# ----------- ASSIGN ORGANIZATION TYPE to Gen/WIEWS ------------#

# ASSIGN ORG_TYPE TO GEN/WIEWS
gen_wiews_df <- all_org_wiews %>%
  right_join(gen_wiews_df, by = c("inst_code" = "INSTCODE"))

# View no org assigned rows
gen_wiews_no_org_type1 <- gen_wiews_df %>%
  filter(organization_type == "NonGenebank_NonBotanicgarden")

# View rows in gen_wiews_df where no organization type is assigned
# 3,113 rows
gen_wiews_no_org_type <- gen_wiews_df %>%
  filter(is.na(organization_type) | trimws(organization_type) == "")

# Show distinct inst_code where no organization type is assigned, 11 org in GENESYS
no_org_type_df <- gen_wiews_df %>%
  filter(is.na(organization_type) | trimws(organization_type) == "") %>%
  select(inst_code, data_source) %>%
  distinct()

# Remove rows where organization_type is "NonGenebank_NonBotanicgarden"
gen_wiews_df <- gen_wiews_df %>%
  filter(organization_type != "NonGenebank_NonBotanicgarden")

# Drop rows in Gen/WIEWS where organization_type is blank or NA
gen_wiews_df_assigned <- gen_wiews_df %>%
  filter(!is.na(organization_type) & trimws(organization_type) != "")

# RESULT:
# Gen/WIEWS with org type assigned: 4,803,066 rows
#write.csv(gen_wiews_df_assigned, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Deduplicated_with_org_assigned/gen_wiews_dedup_org_assigned_2026-03-13.csv', row.names = FALSE)

#save GENESYS dropped rows
write.csv(gen_wiews_df_assigned,'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Standardized_and_filtered/Genesys_data_DROPPED_dedup_org_assigned_2026-04-16.csv', row.names = FALSE)


# ----------- ASSIGN ORGANIZATION TYPE to Cano ------------#

# Assign org names by LC 
assign_cano_LC <- read_excel("C:/Users/sarah/Downloads/LC_assign_cano.xlsx")

# Drop the field LC from botanicgarden_accessionlevel_dataset_with_coords
assign_cano_LC <- assign_cano_LC %>%
  select(-data_source,
         -SG_duplicate_note,
         -organization_type)

# assign inst_code and org name by LC
cano_df <- cano_df %>%
  right_join(assign_cano_LC, by = "LC")

#org_type assigned all = Botanic garden


# ------------- DROP CANO duplicates in GEN/WIEWS and GBIF-living ------------#

# Drop duplicate LCs- rows where delete_organization_from_Cano_data is "delete" in guide file
cano_df_dedup <- cano_df %>%
  filter(is.na(delete_organization_from_Cano_data) | delete_organization_from_Cano_data != "delete")

cano_df_dedup <- cano_df_dedup %>%
  select(-delete_organization_from_Cano_data,
         -WFO_id,
         -cultivar_name,
         -kingdom_GBIF,
         -class_GBIF,
         -phylum_GBIF,
         -forma_name,
         -other_name,
         -item_status_date)

# 33,286 duplicates with gen/wiews dropped
# 41,439 duplicates with gbif living dropped
# 74,725 dup dropped total

# RESULT:
# Cano with org assigned, dedup: 316,489 rows
write.csv(cano_df_dedup, 'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Deduplicated_with_org_assigned/cano_dedup_org_assigned_2026-03-13.csv', row.names = FALSE)


## end script ##