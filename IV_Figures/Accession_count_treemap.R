# =============================================================================
# GCCFP Complementarity Project - Species Accession Treemap Figures
# =============================================================================
# Description:
#   This script produces paired treemap figures comparing the relative numbers
#   of accessions by species for both genebank and botanic garden sources, using
#   the "accessionlevel_species_count_summary.xlsx" summary table as data input.
#
#   Each bar or tile represents a unique WCFP-standardized taxon. The size of
#   each tile is proportional to the number of accessions for that species in
#   either genebanks (left/top) or botanic gardens (right/bottom). Labeling
#   style (with/without counts, italic or plain) and color palette (turbo,
#   orange, purple, or both) are specified for each figure version below.
#
#   These treemaps visualize the collection dominance and the taxonomic breadth
#   of ex situ conservation for the WCFP crop species list:
#     - "All species": shows the full set of taxa with at least one accession.
#     - "Top N species": restricted to top N species by number of accessions in
#        either genebank or botanic garden data.
#     - "With counts": labels include both the species name and number of accessions;
#        numbers use commas for >=10,000, none otherwise.
#     - "Multicolor"/"Turbo": default continuous palette (for diversity).
#     - "Orange" (genebank) and "Purple" (botanic garden): for institutional accent/branding.
#     - "ITALIC" in the filename indicates italicized species names in labels.
#
#   Output PNG files are named systematically: e.g.
#     treemap_accessions_allspecies_withcounts_multicolor_ITALIC.png
#     treemap_accessions_top100_nocounts_orange_purple_ITALIC.png
#
#   Each figure is arranged as a vertical patchwork: genebank treemap above,
#   botanic garden treemap below.
#
# =============================================================================

# =============================================================================
# WCFP ACCESSION-LEVEL SPECIES TREEMAP WORKFLOW
# =============================================================================
#
# Number formatting: comma if >= 10,000
# Color schemes: turbo ("multicolor"), orange, purple, or both.
# Labeling: with and without counts, and with or without italics.
# ITALIC is included in the filename if fontface is italic in that plot.
#
# =============================================================================

# Load libaries
library(readxl)
library(ggplot2)
library(treemapify)
library(patchwork)
library(scales)
library(dplyr)

# format comma
format_count <- function(x) {
  ifelse(x >= 10000, comma(x), as.character(x))
}

#read in dataframe summary of the number of accessions per species
df <- read_excel("C:/Users/sarah/Downloads/accessionlevel_species_count_summary.xlsx")




# =============================================================================
# v1. ALL SPECIES | WITH COUNTS | MULTICOLOR | ITALIC LABELS
# =============================================================================
df_a <- df %>%
  filter(number_of_accessions_in_genebanks > 0) %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  mutate(rank = row_number(),
         label = ifelse(rank <= 5000,
                        paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_genebanks)),
                        ""))

df_b <- df %>%
  filter(number_of_accessions_in_botanicgardens > 0) %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  mutate(rank = row_number(),
         label = ifelse(rank <= 5000,
                        paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_botanicgardens)),
                        ""))

fig_a <- ggplot(df_a, aes(area = number_of_accessions_in_genebanks,
                          fill = WCFP_taxa, label = label)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(colour = "black", place = "centre", size = 10, grow = FALSE, reflow = TRUE,
                    start = "topleft", fontface = "italic") +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "a) Number of accessions in genebanks") +
  theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold", hjust = 0))

fig_b <- ggplot(df_b, aes(area = number_of_accessions_in_botanicgardens,
                          fill = WCFP_taxa, label = label)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(colour = "black", place = "centre", size = 10, grow = FALSE, reflow = TRUE,
                    start = "topleft", fontface = "italic") +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "b) Number of accessions in botanic gardens") +
  theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold", hjust = 0))

combined <- fig_a / fig_b

png("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_23/treemap_accessions_allspecies_withcounts_multicolor_ITALIC.png",
    width = 10, height = 12, units = "in", res = 300)
print(combined)
dev.off()



# ==================================================================================
# v2. TOP 100 PER Genebank/Botanic garden | WITH COUNTS | MULTICOLOR | ITALIC LABELS
# ==================================================================================
df_a100 <- df %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  slice_head(n = 100) %>%
  mutate(label = paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_genebanks)))
df_b100 <- df %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  slice_head(n = 100) %>%
  mutate(label = paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_botanicgardens)))
fig_a100 <- ggplot(df_a100, aes(area = number_of_accessions_in_genebanks, fill = WCFP_taxa, label = label)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(colour = "black", place = "centre", size = 10, grow = FALSE, reflow = TRUE,
                    start = "topleft", min.size = 0, fontface = "italic") +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "a) Number of accessions in genebanks") +
  theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold", hjust = 0))
fig_b100 <- ggplot(df_b100, aes(area = number_of_accessions_in_botanicgardens, fill = WCFP_taxa, label = label)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(colour = "black", place = "centre", size = 10, grow = FALSE, reflow = TRUE,
                    start = "topleft", min.size = 0, fontface = "italic") +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "b) Number of accessions in botanic gardens") +
  theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold", hjust = 0))
combined_100 <- fig_a100 / fig_b100
png("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_23/treemap_accessions_top100_withcounts_multicolor_ITALIC.png",
    width = 10, height = 12, units = "in", res = 300)
print(combined_100)
dev.off()



# =================================================================================
# v3. TOP 100 PER Genebank/Botanic garden | NO COUNTS | MULTICOLOR | ITALIC LABELS
# =================================================================================
df_a100_nc <- df %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  slice_head(n = 100)
df_b100_nc <- df %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  slice_head(n = 100)
fig_a100_nc <- ggplot(df_a100_nc, aes(area = number_of_accessions_in_genebanks, fill = WCFP_taxa, label = WCFP_taxa)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(colour = "black", place = "centre", size = 10, grow = FALSE, reflow = TRUE,
                    start = "topleft", min.size = 0, fontface = "italic") +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "a) Number of accessions in genebanks") +
  theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold", hjust = 0))
fig_b100_nc <- ggplot(df_b100_nc, aes(area = number_of_accessions_in_botanicgardens, fill = WCFP_taxa, label = WCFP_taxa)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(colour = "black", place = "centre", size = 10, grow = FALSE, reflow = TRUE,
                    start = "topleft", min.size = 0, fontface = "italic") +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "b) Number of accessions in botanic gardens") +
  theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold", hjust = 0))
combined_100_nc <- fig_a100_nc / fig_b100_nc
png("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_23/treemap_accessions_top100_nocounts_multicolor_ITALIC.png",
    width = 10, height = 12, units = "in", res = 300)
print(combined_100_nc)
dev.off()



# =============================================================================
# v4. ALL SPECIES | WITH COUNTS | ORANGE & PURPLE | ITALIC LABELS
# =============================================================================
orange_pal_all <- colorRampPalette(c("#BF360C", "#FFB74D", "#FFE0B2", "#FFF3E0","#E65100", "#FB8C00"))(nrow(df_a))
purple_pal_all <- colorRampPalette(c("#F3E5F5", "#E1BEE7", "#CE93D8", "#AB47BC", "#7B1FA2", "#4A148C"))(nrow(df_b))
fig_a_op <- ggplot(df_a, aes(area = number_of_accessions_in_genebanks, fill = WCFP_taxa, label = label)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(colour = "black", place = "centre", size = 10, grow = FALSE, reflow = TRUE,
                    start = "topleft", fontface = "italic") +
  scale_fill_manual(values = orange_pal_all, guide = "none") +
  labs(title = "a) Number of accessions in genebanks") +
  theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold", hjust = 0))
fig_b_op <- ggplot(df_b, aes(area = number_of_accessions_in_botanicgardens, fill = WCFP_taxa, label = label)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(colour = "black", place = "centre", size = 10, grow = FALSE, reflow = TRUE,
                    start = "topleft", fontface = "italic") +
  scale_fill_manual(values = purple_pal_all, guide = "none") +
  labs(title = "b) Number of accessions in botanic gardens") +
  theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold", hjust = 0))
combined_op <- fig_a_op / fig_b_op
png("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_23/treemap_accessions_allspecies_withcounts_orange_purple_ITALIC.png",
    width = 10, height = 12, units = "in", res = 300)
print(combined_op)
dev.off()




### end script ##