# Annotate data with organization type, remove selected organizations, remove duplicates between data

# 1. Annotate data with organization type
#      Botanic garden
#      Genebank
# 2. Remove non-genebank and non-botanic garden organizations
# 3. Remove duplicates between datasets
#      guide file flags duplicates to remove based on counts per org by data source.
#      Data source with higher number of records per org is selected to keep.

# Load libraries
library(dplyr)
library(readr)
library(readxl)
library(writexl)

# -------------------------------- #
# ----- READ IN GUIDE FILE -------#
# -------------------------------- #

# Organizations guide file: 22,276 organizations from FAO WIEWS and BGCI GardenSearch
all_org <- read_excel("C:/Users/sarah/Downloads/institution_dataset_with_record_counts.xlsx")

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
gbif_living <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_living_2026-03-11.csv")
cat("Raw GBIF-Living rows loaded:", nrow(gbif_living), "\n")

# GBIF OBSERVATIONS: 6,701,782 rows
# gbif_observations <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_observations_2026-03-11.csv")



# ----------- PROCESS BGCI DATA ------------ #

# BGCI data: rename ex_situ_garden_id = inst_code
bgci_df <- bgci_df %>%
  rename(inst_code = ex_situ_garden_id) %>%
  mutate(inst_code = trimws(inst_code))
cat("BGCI after renaming inst_code:", nrow(bgci_df), "\n")

# ----------- ASSIGN ORGANIZATION TYPE to BGCI ------------#
bgci_df <- all_org %>%
  select(inst_code, organization_type, organization_name) %>%
  mutate(
    inst_code = trimws(inst_code),
    organization_type = trimws(organization_type),
    organization_name = trimws(organization_name)
  ) %>%
  right_join(bgci_df, by = c("inst_code"))
cat("BGCI after assigning org_type:", nrow(bgci_df), "\n")

# ----------- REMOVE flagged BGCI institutions ------------#
bgci_delete_insts <- all_org %>%
  filter(delete_organization_from_BGCI_data == "delete") %>%
  pull(inst_code) %>% unique()
bgci_df <- bgci_df %>% filter(!inst_code %in% bgci_delete_insts)
cat("BGCI after deleting flagged institutions:", nrow(bgci_df), "\n")

# ----------- REMOVE unassigned organization types from BGCI ------------#
bgci_df <- bgci_df %>%
  filter(
    !is.na(organization_type),
    trimws(organization_type) != "",
    organization_type != "NonGenebank_NonBotanicgarden"
  )
cat("BGCI after removing unassigned org_type:", nrow(bgci_df), "\n")

# RESULT:
# BGCI data with org type assigned: 618,544 rows
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
cat("Gen/WIEWS after renaming inst_code:", nrow(gen_wiews_df), "\n")

# ----------- ASSIGN ORGANIZATION TYPE to Gen/WIEWS ------------#
gen_wiews_df <- all_org %>%
  select(inst_code, organization_type, organization_name) %>%
  mutate(
    inst_code = trimws(inst_code),
    organization_type = trimws(organization_type),
    organization_name = trimws(organization_name)
  ) %>%
  right_join(gen_wiews_df, by = c("inst_code"))
cat("Gen/WIEWS after assigning org_type:", nrow(gen_wiews_df), "\n")

# ----------- REMOVE flagged Gen/WIEWS institutions ------------#
genwiews_delete_insts <- all_org %>%
  filter(delete_organization_from_GenWIEWS_data == "delete") %>%
  pull(inst_code) %>% unique()
gen_wiews_df <- gen_wiews_df %>% filter(!inst_code %in% genwiews_delete_insts)
cat("Gen/WIEWS after deleting flagged institutions:", nrow(gen_wiews_df), "\n")

# ----------- REMOVE unassigned organization types from Gen/WIEWS ------------#
gen_wiews_df <- gen_wiews_df %>%
  filter(
    !is.na(organization_type),
    trimws(organization_type) != "",
    organization_type != "NonGenebank_NonBotanicgarden"
  )
cat("Gen/WIEWS after removing unassigned org_type:", nrow(gen_wiews_df), "\n")

# RESULT:
# Gen/WIEWS data with org type assigned: 4,803,066 rows
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
cat("Cano after LC join:", nrow(cano_df), "\n")

# ----------- ASSIGN ORGANIZATION TYPE to Cano ------------#
cano_df <- all_org %>%
  select(inst_code, organization_type, organization_name) %>%
  mutate(
    inst_code = trimws(inst_code),
    organization_type = trimws(organization_type),
    organization_name = trimws(organization_name)
  ) %>%
  right_join(cano_df, by = "inst_code")
cat("Cano after assigning org_type:", nrow(cano_df), "\n")

# ----------- REMOVE flagged Cano institutions ------------#
cano_delete_insts <- all_org %>%
  filter(delete_organization_from_Cano_data == "delete") %>%
  pull(inst_code) %>% unique()
cano_df <- cano_df %>% filter(!inst_code %in% cano_delete_insts)
cat("Cano after deleting flagged institutions:", nrow(cano_df), "\n")

# ----------- REMOVE unassigned organization types from Cano ------------#
cano_df <- cano_df %>%
  filter(
    !is.na(organization_type),
    trimws(organization_type) != "",
    organization_type != "NonGenebank_NonBotanicgarden"
  )
cat("Cano after removing unassigned org_type:", nrow(cano_df), "\n")

# RESULT:
# Cano data with org type assigned: 316,489 rows
write.csv(
  cano_df,
  'C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Processed_data/Deduplicated_with_org_assigned/cano_org_assigned_2026-05-19.csv',
  row.names = FALSE
)


# ----------- PROCESS GBIF-LIVING DATA ------------ #

# ----------- ASSIGN ORGANIZATION TYPE to GBIF-LIVING ---------------#
gbif_living <- all_org %>%
  select(inst_code, organization_type, organization_name) %>%
  mutate(
    inst_code = trimws(inst_code),
    organization_type = trimws(organization_type),
    organization_name = trimws(organization_name)
  ) %>%
  right_join(gbif_living, by = "inst_code")
cat("GBIF-living after assigning org_type:", nrow(gbif_living), "\n")

# ----------- REMOVE flagged GBIF-living institutions ------------#
gbifliving_delete_insts <- all_org %>%
  filter(delete_organization_from_GBIF_living_data == "delete" |
           delete_organization_from_GBIF-living_data == "delete") %>%
  pull(inst_code) %>% unique()
gbif_living <- gbif_living %>% filter(!inst_code %in% gbifliving_delete_insts)
cat("GBIF-living after deleting flagged institutions:", nrow(gbif_living), "\n")

# ----------- REMOVE unassigned organization types from GBIF-living ------------#
gbif_living <- gbif_living %>%
  filter(
    !is.na(organization_type),
    trimws(organization_type) != "",
    organization_type != "NonGenebank_NonBotanicgarden"
  )
cat("GBIF-living after removing unassigned org_type:", nrow(gbif_living), "\n")

# RESULT:
# GBIF-living data with org type assigned: 72,863 rows

# ----------- SUBSET GBIF-LIVING DATA BY ORGANIZATION TYPE ------------#

# GBIF living Genebank subset
gbif_living_genebank_df <- gbif_living %>%
  filter(organization_type == "Genebank")

#save GBIF Living (Genebank): 8,216 rows
write.csv(
  gbif_living_genebank_df,
  "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_living_genebank_2026-03-12.csv",
  row.names = FALSE
)
cat("GBIF Living Genebank rows saved:", nrow(gbif_living_genebank_df), "\n")

# GBIF living Botanic garden subset
gbif_living_botanic_garden_df <- gbif_living %>%
  filter(organization_type == "Botanic garden")

# Save GBIF Living (Botanic garden): 64,647 rows
write.csv(
  gbif_living_botanic_garden_df,
  "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Data_processing/NEW/GBIF_data_living_botanicgarden_2026-03-12.csv",
  row.names = FALSE
)
cat("GBIF Living Botanic garden rows saved:", nrow(gbif_living_botanic_garden_df), "\n")


## end script ##
