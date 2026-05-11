# Install packages if needed
# install.packages(c("ggplot2", "treemapify", "patchwork", "scales", "readxl", "dplyr"))

library(readxl)
library(ggplot2)
library(treemapify)
library(patchwork)
library(scales)
library(dplyr)

# -- Read data --
df <- read_excel("C:/Users/sarah/Downloads/accessionlevel_species_count_summary.xlsx")

# -- Custom comma formatting: only add comma for >= 10,000 --
format_count <- function(x) {
  ifelse(x >= 10000, comma(x), as.character(x))
}



#---------------- v1: ALL SPECIES --------------#

# -- Filter out tiny species for speed --
df_a <- df %>%
  filter(number_of_accessions_in_genebanks >= 10) %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  mutate(
    rank = row_number(),
    label = ifelse(
      rank <= 500,
      paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_genebanks)),
      ""
    )
  )

df_b <- df %>%
  filter(number_of_accessions_in_botanicgardens >= 10) %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  mutate(
    rank = row_number(),
    label = ifelse(
      rank <= 500,
      paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_botanicgardens)),
      ""
    )
  )

# -- Check how many species remain --
message("Genebanks species: ", nrow(df_a))
message("Botanic gardens species: ", nrow(df_b))

# -- Figure A: Genebanks --
fig_a <- ggplot(df_a, aes(
  area = number_of_accessions_in_genebanks,
  fill = WCFP_taxa,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    grow = FALSE,
    reflow = TRUE,
    start = "topleft"
  ) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "a) Number of accessions in genebanks") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# -- Figure B: Botanic Gardens --
fig_b <- ggplot(df_b, aes(
  area = number_of_accessions_in_botanicgardens,
  fill = WCFP_taxa,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    grow = FALSE,
    reflow = TRUE,
    start = "topleft"
  ) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "b) Number of accessions in botanic gardens") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# -- Combine A and B vertically --
combined <- fig_a / fig_b

# -- Save directly (skip displaying in RStudio) --
png(
  filename = "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_08/treemap_accessions_all_species.png",
  width = 10,
  height = 12,
  units = "in",
  res = 300
)
print(combined)
dev.off()



#----------------------------------------------

# Install packages if needed
# install.packages(c("ggplot2", "treemapify", "patchwork", "scales", "readxl", "dplyr"))

library(readxl)
library(ggplot2)
library(treemapify)
library(patchwork)
library(scales)
library(dplyr)

# -- Read data --
df <- read_excel("C:/Users/sarah/Downloads/accessionlevel_species_count_summary.xlsx")

# -- Custom comma formatting: only add comma for >= 10,000 --
format_count <- function(x) {
  ifelse(x >= 10000, comma(x), as.character(x))
}

# -- Top 100 species only --
df_a <- df %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  slice_head(n = 100) %>%
  mutate(label = paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_genebanks)))

df_b <- df %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  slice_head(n = 100) %>%
  mutate(label = paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_botanicgardens)))

# -- Figure A: Genebanks --
fig_a <- ggplot(df_a, aes(
  area = number_of_accessions_in_genebanks,
  fill = WCFP_taxa,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0
  ) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "a) Number of accessions in genebanks") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# -- Figure B: Botanic Gardens --
fig_b <- ggplot(df_b, aes(
  area = number_of_accessions_in_botanicgardens,
  fill = WCFP_taxa,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0
  ) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "b) Number of accessions in botanic gardens") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# -- Combine A and B vertically --
combined <- fig_a / fig_b

# -- Save directly (skip displaying in RStudio) --
png(
  filename = "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_08/treemap_accessions_top100species.png",
  width = 10,
  height = 12,
  units = "in",
  res = 300
)
print(combined)
dev.off()


# ------------------- TOP 100 SPECIES: NO COUNTS --------#

# Install packages if needed
# install.packages(c("ggplot2", "treemapify", "patchwork", "scales", "readxl", "dplyr"))

library(readxl)
library(ggplot2)
library(treemapify)
library(patchwork)
library(scales)
library(dplyr)

# -- Read data --
df <- read_excel("C:/Users/sarah/Downloads/accessionlevel_species_count_summary.xlsx")

# -- Top 100 species only --
df_a <- df %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  slice_head(n = 100)

df_b <- df %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  slice_head(n = 100)

# -- Figure A: Genebanks --
fig_a <- ggplot(df_a, aes(
  area = number_of_accessions_in_genebanks,
  fill = WCFP_taxa,
  label = WCFP_taxa
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    fontface = "italic",
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0
  ) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "a) Number of accessions in genebanks") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# -- Figure B: Botanic Gardens --
fig_b <- ggplot(df_b, aes(
  area = number_of_accessions_in_botanicgardens,
  fill = WCFP_taxa,
  label = WCFP_taxa
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    fontface = "italic",
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0
  ) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "b) Number of accessions in botanic gardens") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# -- Combine A and B vertically --
combined <- fig_a / fig_b

# -- Save directly (skip displaying in RStudio) --
png(
  filename = "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_08/treemap_accessions_top100species_no_counts.png",
  width = 10,
  height = 12,
  units = "in",
  res = 300
)
print(combined)
dev.off()





# ------------  TOP 100- NO COUNTS, COLOR SCHEME= ORGANGE, PURPLE----- #
# Install packages if needed
# install.packages(c("ggplot2", "treemapify", "patchwork", "scales", "readxl", "dplyr"))

library(readxl)
library(ggplot2)
library(treemapify)
library(patchwork)
library(scales)
library(dplyr)

# -- Read data --
df <- read_excel("C:/Users/sarah/Downloads/accessionlevel_species_count_summary.xlsx")

# -- Top 100 species only --
df_a <- df %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  slice_head(n = 100)

df_b <- df %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  slice_head(n = 100)

# -- Orange palette for genebanks (100 shades) --
orange_pal <- colorRampPalette(c("#FFF3E0", "#FFE0B2", "#FFB74D", "#FB8C00", "#E65100", "#BF360C"))(100)

# -- Purple palette for botanic gardens (100 shades) --
purple_pal <- colorRampPalette(c("#F3E5F5", "#E1BEE7", "#CE93D8", "#AB47BC", "#7B1FA2", "#4A148C"))(100)

# -- Figure A: Genebanks --
fig_a <- ggplot(df_a, aes(
  area = number_of_accessions_in_genebanks,
  fill = WCFP_taxa,
  label = WCFP_taxa
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    fontface = "italic",
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0
  ) +
  scale_fill_manual(values = orange_pal, guide = "none") +
  labs(title = "a) Number of accessions in genebanks") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# -- Figure B: Botanic Gardens --
fig_b <- ggplot(df_b, aes(
  area = number_of_accessions_in_botanicgardens,
  fill = WCFP_taxa,
  label = WCFP_taxa
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    fontface = "italic",
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0
  ) +
  scale_fill_manual(values = purple_pal, guide = "none") +
  labs(title = "b) Number of accessions in botanic gardens") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# -- Combine A and B vertically --
combined <- fig_a / fig_b

# -- Save directly (skip displaying in RStudio) --
png(
  filename = "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_08/treemap_accessions_top100species_no_counts_orange_purple.png",
  width = 10,
  height = 12,
  units = "in",
  res = 300
)
print(combined)
dev.off()


# --------- All species, with counts, orange and purple -------#

# Install packages if needed
# install.packages(c("ggplot2", "treemapify", "patchwork", "scales", "readxl", "dplyr"))

library(readxl)
library(ggplot2)
library(treemapify)
library(patchwork)
library(scales)
library(dplyr)

# -- Read data --
df <- read_excel("C:/Users/sarah/Downloads/accessionlevel_species_count_summary.xlsx")

# -- Custom comma formatting: only add comma for >= 10,000 --
format_count <- function(x) {
  ifelse(x >= 10000, comma(x), as.character(x))
}

# -- All species, filter out zeros, labels for top 100 --
df_a <- df %>%
  filter(number_of_accessions_in_genebanks > 0) %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  mutate(
    rank = row_number(),
    label = ifelse(
      rank <=5000,
      paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_genebanks)),
      ""
    )
  )

df_b <- df %>%
  filter(number_of_accessions_in_botanicgardens > 0) %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  mutate(
    rank = row_number(),
    label = ifelse(
      rank <= 5000,
      paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_botanicgardens)),
      ""
    )
  )

# -- Check counts --
message("Genebanks species: ", nrow(df_a))
message("Botanic gardens species: ", nrow(df_b))

# -- Orange palette for genebanks --
orange_pal <- colorRampPalette(c("#FFF3E0", "#FFE0B2", "#FFB74D", "#FB8C00", "#E65100", "#BF360C"))(nrow(df_a))

# -- Purple palette for botanic gardens --
purple_pal <- colorRampPalette(c("#F3E5F5", "#E1BEE7", "#CE93D8", "#AB47BC", "#7B1FA2", "#4A148C"))(nrow(df_b))

# -- Build plots --

fig_a <- ggplot(df_a, aes(
  area = number_of_accessions_in_genebanks,
  fill = WCFP_taxa,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0
  ) +
  scale_fill_manual(values = orange_pal, guide = "none") +
  labs(title = "a) Number of accessions in genebanks") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

fig_b <- ggplot(df_b, aes(
  area = number_of_accessions_in_botanicgardens,
  fill = WCFP_taxa,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0
  ) +
  scale_fill_manual(values = purple_pal, guide = "none") +
  labs(title = "b) Number of accessions in botanic gardens") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# -- Combine A and B vertically --
combined <- fig_a / fig_b

# -- Save as PNG --
png(
  filename = "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_08/treemap_accessions_with_counts_orange_purple.png",
  width = 10,
  height = 12,
  units = "in",
  res = 600              #updated
)
print(combined)
dev.off()

# -- Save as PDF --
pdf(
  file = "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_08/treemap_accessions_with_counts_orange_purple.pdf",
  width = 10,
  height = 12
)
print(combined)
dev.off()





# -------------------- UPDATED 2026-04-15 -----------------------#
#---------------- v1: ALL SPECIES UPDATED --------------#

# -- Read data --
df <- read_excel("C:/Users/sarah/Downloads/accessionlevel_species_count_summary.xlsx")

# -- Custom comma formatting: only add comma for >= 10,000 --
format_count <- function(x) {
  ifelse(x >= 10000, comma(x), as.character(x))
}

# -- Filter out tiny species for speed --
df_a <- df %>%
  filter(number_of_accessions_in_genebanks > 0) %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  mutate(
    rank = row_number(),
    label = ifelse(
      rank <= 5000,
      paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_genebanks)),
      ""
    )
  )

df_b <- df %>%
  filter(number_of_accessions_in_botanicgardens > 0) %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  mutate(
    rank = row_number(),
    label = ifelse(
      rank <= 5000,
      paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_botanicgardens)),
      ""
    )
  )

# -- Check how many species remain --
message("Genebanks species: ", nrow(df_a))
message("Botanic gardens species: ", nrow(df_b))

# -- Figure A: Genebanks --
fig_a <- ggplot(df_a, aes(
  area = number_of_accessions_in_genebanks,
  fill = WCFP_taxa,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0
  ) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "a) Number of accessions in genebanks") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# -- Figure B: Botanic Gardens --
fig_b <- ggplot(df_b, aes(
  area = number_of_accessions_in_botanicgardens,
  fill = WCFP_taxa,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0
  ) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "b) Number of accessions in botanic gardens") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# -- Combine A and B vertically --
combined <- fig_a / fig_b

# -- Save directly (skip displaying in RStudio) --
png(
  filename = "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_08/treemap_accessions_all_species_2026-04-15_2.png",
  width = 10,
  height = 12,
  units = "in",
  res = 300
)
print(combined)
dev.off()










#---------------- v1: ALL SPECIES UPDATED --------------#

df <- read_excel("C:/Users/sarah/Downloads/accessionlevel_species_count_summary.xlsx")

format_count <- function(x) {
  ifelse(x >= 10000, comma(x), as.character(x))
}

df_a <- df %>%
  filter(number_of_accessions_in_genebanks > 0) %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  mutate(
    rank = row_number(),
    label = ifelse(
      rank <= 5000,
      paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_genebanks)),
      ""
    )
  )

df_b <- df %>%
  filter(number_of_accessions_in_botanicgardens > 0) %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  mutate(
    rank = row_number(),
    label = ifelse(
      rank <= 5000,
      paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_botanicgardens)),
      ""
    )
  )

message("Genebanks species: ", nrow(df_a))
message("Botanic gardens species: ", nrow(df_b))

fig_a <- ggplot(df_a, aes(area = number_of_accessions_in_genebanks, fill = WCFP_taxa, label = label)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(colour = "black", place = "centre", size = 10, grow = FALSE, reflow = TRUE, start = "topleft", min.size = 0, fontface = "italic") +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "a) Number of accessions in genebanks") +
  theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold", hjust = 0))

fig_b <- ggplot(df_b, aes(area = number_of_accessions_in_botanicgardens, fill = WCFP_taxa, label = label)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(colour = "black", place = "centre", size = 10, grow = FALSE, reflow = TRUE, start = "topleft", min.size = 0, fontface = "italic") +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "b) Number of accessions in botanic gardens") +
  theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold", hjust = 0))

combined <- fig_a / fig_b

png(
  filename = "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_08/treemap_accessions_all_species_ITALIC.png",
  width = 10, height = 12, units = "in", res = 300
)
print(combined)
dev.off()








#---------------- v2: ALL SPECIES - ORANGE & PURPLE (ITALIC) --------------#

library(readxl)
library(ggplot2)
library(treemapify)
library(patchwork)
library(scales)
library(dplyr)

# -- Read data --
df <- read_excel("C:/Users/sarah/Downloads/accessionlevel_species_count_summary.xlsx")

# -- Custom comma formatting: only add comma for >= 10,000 --
format_count <- function(x) {
  ifelse(x >= 10000, comma(x), as.character(x))
}

# -- All species, filter out zeros, labels for top 5000 --
df_a <- df %>%
  filter(number_of_accessions_in_genebanks > 0) %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  mutate(
    rank = row_number(),
    label = ifelse(
      rank <= 5000,
      paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_genebanks)),
      ""
    )
  )

df_b <- df %>%
  filter(number_of_accessions_in_botanicgardens > 0) %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  mutate(
    rank = row_number(),
    label = ifelse(
      rank <= 5000,
      paste0(WCFP_taxa, "\n", format_count(number_of_accessions_in_botanicgardens)),
      ""
    )
  )

# -- Check counts --
message("Genebanks species: ", nrow(df_a))
message("Botanic gardens species: ", nrow(df_b))

# -- Orange palette for genebanks --
orange_pal <- colorRampPalette(c("#FFF3E0", "#FFE0B2", "#FFB74D", "#FB8C00", "#E65100", "#BF360C"))(nrow(df_a))

# -- Purple palette for botanic gardens --
purple_pal <- colorRampPalette(c("#F3E5F5", "#E1BEE7", "#CE93D8", "#AB47BC", "#7B1FA2", "#4A148C"))(nrow(df_b))

# -- Build plots --

fig_a <- ggplot(df_a, aes(
  area = number_of_accessions_in_genebanks,
  fill = WCFP_taxa,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0,
    fontface = "italic"
  ) +
  scale_fill_manual(values = orange_pal, guide = "none") +
  labs(title = "a) Number of accessions in genebanks") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

fig_b <- ggplot(df_b, aes(
  area = number_of_accessions_in_botanicgardens,
  fill = WCFP_taxa,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0,
    fontface = "italic"
  ) +
  scale_fill_manual(values = purple_pal, guide = "none") +
  labs(title = "b) Number of accessions in botanic gardens") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# -- Combine A and B vertically --
combined <- fig_a / fig_b

# -- Save as PNG --
png(
  filename = "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_08/treemap_accessions_all_species_orange_purple_ITALIC.png",
  width = 10, height = 12, units = "in", res = 300
)
print(combined)
dev.off()







# 2026-04-23

# ----------- TOP 50 species: Single combined Treemap ----#
# Top 50 species with the greatest number of accessions in genebanks
# Top 50 species with the greatest number of accessions in botanic gardens

library(readxl)
library(ggplot2)
library(treemapify)
library(scales)
library(dplyr)

# -- Read data --
df <- read_excel("C:/Users/sarah/Downloads/accessionlevel_species_count_summary.xlsx")

# -- Custom comma formatting: only add comma for >= 10,000 --
format_count <- function(x) {
  ifelse(x >= 10000, comma(x), as.character(x))
}

# ---------------------------
# 1) Build TOP 50 for each source (same ordering logic as your original)
# ---------------------------

df_gen <- df %>%
  filter(number_of_accessions_in_genebanks > 0) %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  slice_head(n = 50) %>%
  mutate(
    source = "Genebanks",
    accessions = number_of_accessions_in_genebanks,
    label = paste0(WCFP_taxa, "\n", format_count(accessions)),
    # unique fill id so genebanks/botanic gardens don't collide on same species name
    fill_id = paste0(source, "__", WCFP_taxa)
  ) %>%
  select(WCFP_taxa, source, accessions, label, fill_id)

df_bot <- df %>%
  filter(number_of_accessions_in_botanicgardens > 0) %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  slice_head(n = 50) %>%
  mutate(
    source = "Botanic gardens",
    accessions = number_of_accessions_in_botanicgardens,
    label = paste0(WCFP_taxa, "\n", format_count(accessions)),
    fill_id = paste0(source, "__", WCFP_taxa)
  ) %>%
  select(WCFP_taxa, source, accessions, label, fill_id)

message("Genebanks species (top 50): ", nrow(df_gen))
message("Botanic gardens species (top 50): ", nrow(df_bot))

# ---------------------------
# 2) SAME palette stops as your original figure
#    (just sized to top-50 instead of nrow(df_a)/nrow(df_b))
# ---------------------------

orange_pal_50 <- colorRampPalette(
  c("#FFF3E0", "#FFE0B2", "#FFB74D", "#FB8C00", "#E65100", "#BF360C")
)(nrow(df_gen))

purple_pal_50 <- colorRampPalette(
  c("#F3E5F5", "#E1BEE7", "#CE93D8", "#AB47BC", "#7B1FA2", "#4A148C")
)(nrow(df_bot))

# Map colors to each tile in-order (so appearance behaves like your old code:
# sorted by desc(accessions), then colors assigned down the list)
fill_values <- c(
  setNames(orange_pal_50, df_gen$fill_id),
  setNames(purple_pal_50, df_bot$fill_id)
)

# ---------------------------
# 3) Combine to ONE treemap
# ---------------------------
df_top <- bind_rows(df_gen, df_bot)

fig <- ggplot(df_top, aes(
  area = accessions,
  fill = fill_id,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    size = 10,
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0,
    fontface = "italic"
  ) +
  scale_fill_manual(values = fill_values, guide = "none") +
  labs(title = "Top 50 species by accessions (genebanks + botanic gardens)") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

# ---------------------------
# 4) Save to your requested location
# ---------------------------
png(
  filename = "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_23/1_treemap_accessions_TOP50_genebanks_botanic_SINGLE_orange_purple_ITALIC.png",
  width = 10, height = 12, units = "in", res = 300
)
print(fig)
dev.off()


################## USE THIS ONE  ##################
################# TOP 50 species ##################
### try 2:
library(readxl)
library(ggplot2)
library(treemapify)
library(scales)
library(dplyr)

# -- Read data --
df <- read_excel("C:/Users/sarah/Downloads/accessionlevel_species_count_summary.xlsx")

# -- Custom comma formatting: only add comma for >= 10,000 --
format_count <- function(x) {
  ifelse(x >= 10000, comma(x), as.character(x))
}

# ---------------------------
# 1) Top 50 per source
# ---------------------------
df_gen <- df %>%
  filter(number_of_accessions_in_genebanks > 0) %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  slice_head(n = 50) %>%
  mutate(
    source = "Genebanks",
    accessions = number_of_accessions_in_genebanks,
    label = paste0(WCFP_taxa, "\n", format_count(accessions)),
    fill_id = paste0(source, "__", WCFP_taxa)
  ) %>%
  select(WCFP_taxa, source, accessions, label, fill_id)

df_bot <- df %>%
  filter(number_of_accessions_in_botanicgardens > 0) %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  slice_head(n = 50) %>%
  mutate(
    source = "Botanic gardens",
    accessions = number_of_accessions_in_botanicgardens,
    label = paste0(WCFP_taxa, "\n", format_count(accessions)),
    fill_id = paste0(source, "__", WCFP_taxa)
  ) %>%
  select(WCFP_taxa, source, accessions, label, fill_id)

message("Genebanks species (top 50): ", nrow(df_gen))
message("Botanic gardens species (top 50): ", nrow(df_bot))

# ---------------------------
# 2) Same palette stops as your original
# ---------------------------
#orange_pal_50 <- colorRampPalette(
#  c("#FFF3E0", "#FFE0B2", "#FFB74D", "#FB8C00", "#E65100", "#BF360C")
#)(nrow(df_gen))

orange_pal_50 <- colorRampPalette(
  c("#BF360C", "#FFB74D", "#FFE0B2", "#FFF3E0","#E65100", "#FB8C00")
)(nrow(df_gen))

purple_pal_50 <- colorRampPalette(
  c("#F3E5F5", "#E1BEE7", "#CE93D8", "#AB47BC", "#7B1FA2", "#4A148C")
)(nrow(df_bot))

# ---------------------------
# 3) RANDOMIZE color assignment (to mimic the "scattered" look)
# ---------------------------
#set.seed(1)  # change this number if you want a different random arrangement
set.seed(4)  # change this number if you want a different random arrangement

fill_values <- c(
  setNames(sample(orange_pal_50, length(orange_pal_50), replace = FALSE), df_gen$fill_id),
  setNames(sample(purple_pal_50, length(purple_pal_50), replace = FALSE), df_bot$fill_id)
)

# ---------------------------
# 4) Combine + plot ONE treemap
# ---------------------------
df_top <- bind_rows(df_gen, df_bot)

fig <- ggplot(df_top, aes(
  area = accessions,
  fill = fill_id,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    #size = 10,
    size = 12,    #UPDATED
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0,
    fontface = "italic"
  ) +
  scale_fill_manual(values = fill_values, guide = "none") +
  labs(title = "Number of accessions of top 50 species in genebanks and botanic gardens") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

#View
print(fig)

# ---------------------------
# 5) Save as PNG (your requested location)
# ---------------------------
png(
  filename = "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_23/3_treemap_accessions_TOP50_genebanks_botanic_SINGLE_orange_purple_ITALIC.png",
  width = 10, height = 12, units = "in", res = 300
)
print(fig)
dev.off()





##################
### TOP 25 species

# -- Read data --
df <- read_excel("C:/Users/sarah/Downloads/accessionlevel_species_count_summary.xlsx")

# -- Custom comma formatting: only add comma for >= 10,000 --
format_count <- function(x) {
  ifelse(x >= 10000, comma(x), as.character(x))
}

# ---------------------------
# 1) Top 25 per source
# ---------------------------
df_gen <- df %>%
  filter(number_of_accessions_in_genebanks > 0) %>%
  arrange(desc(number_of_accessions_in_genebanks)) %>%
  slice_head(n = 25) %>%
  mutate(
    source = "Genebanks",
    accessions = number_of_accessions_in_genebanks,
    label = paste0(WCFP_taxa, "\n", format_count(accessions)),
    fill_id = paste0(source, "__", WCFP_taxa)
  ) %>%
  select(WCFP_taxa, source, accessions, label, fill_id)

df_bot <- df %>%
  filter(number_of_accessions_in_botanicgardens > 0) %>%
  arrange(desc(number_of_accessions_in_botanicgardens)) %>%
  slice_head(n = 25) %>%
  mutate(
    source = "Botanic gardens",
    accessions = number_of_accessions_in_botanicgardens,
    label = paste0(WCFP_taxa, "\n", format_count(accessions)),
    fill_id = paste0(source, "__", WCFP_taxa)
  ) %>%
  select(WCFP_taxa, source, accessions, label, fill_id)

message("Genebanks species (top 25): ", nrow(df_gen))
message("Botanic gardens species (top 25): ", nrow(df_bot))

# ---------------------------
# 2) Same palette stops as your original
# ---------------------------
#orange_pal_25 <- colorRampPalette(
#  c("#FFF3E0", "#FFE0B2", "#FFB74D", "#FB8C00", "#E65100", "#BF360C")
#)(nrow(df_gen))

orange_pal_25 <- colorRampPalette(
  c("#BF360C", "#FFB74D", "#FFE0B2", "#FFF3E0","#E65100", "#FB8C00")
)(nrow(df_gen))

purple_pal_25 <- colorRampPalette(
  c("#F3E5F5", "#E1BEE7", "#CE93D8", "#AB47BC", "#7B1FA2", "#4A148C")
)(nrow(df_bot))

# ---------------------------
# 3) RANDOMIZE color assignment (to mimic the "scattered" look)
# ---------------------------
set.seed(4)  # change this number if you want a different random arrangement

fill_values <- c(
  setNames(sample(orange_pal_25, length(orange_pal_25), replace = FALSE), df_gen$fill_id),
  setNames(sample(purple_pal_25, length(purple_pal_25), replace = FALSE), df_bot$fill_id)
)

# ---------------------------
# 4) Combine + plot ONE treemap
# ---------------------------
df_top <- bind_rows(df_gen, df_bot)

fig <- ggplot(df_top, aes(
  area = accessions,
  fill = fill_id,
  label = label
)) +
  geom_treemap(colour = "black", size = 0.5, start = "topleft") +
  geom_treemap_text(
    colour = "black",
    place = "centre",
    #size = 10,
    size = 12,    #UPDATED
    grow = FALSE,
    reflow = TRUE,
    start = "topleft",
    min.size = 0,
    fontface = "italic"
  ) +
  scale_fill_manual(values = fill_values, guide = "none") +
  labs(title = "Number of accessions of top 25 species in genebanks and botanic gardens") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0)
  )

#View
print(fig)

# ---------------------------
# 5) Save as PNG (your requested location)
# ---------------------------
png(
  filename = "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/2026_04_23/3_treemap_accessions_TOP25_genebanks_botanic_SINGLE_orange_purple_ITALIC.png",
  width = 10, height = 12, units = "in", res = 300
)
print(fig)
dev.off()



