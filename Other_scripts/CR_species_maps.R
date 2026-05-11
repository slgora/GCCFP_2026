## Map: Species richness map for all CR species in WCFP
## Author: Sarah Gora

# Load required packages
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(viridis)
library(rnaturalearth)


#####################################################
#### STEP 0: Filter occurrences for CR species ######
#####################################################

# CR species
CR_species <- read_excel("C:/Users/sarah/Downloads/CR_species.xlsx")
cr_species_list <- unique(trimws(na.omit(CR_species$taxon_name_accepted)))

# occurrences data: 5,489,253 rows
occurrences_data <- read_csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_occurrences_dataset_2026-03-13.csv')

# add CR species flag
occurrences_data$CR_species <- ifelse(
  trimws(occurrences_data$wcfp_name_match) %in% cr_species_list,
  "Y",
  "N")

# filter for CR species
cr_occurrences <- occurrences_data[occurrences_data$CR_species == "Y", ]
# count of unique CR species in occurrences data
cr_occ_species_list <- unique(trimws(na.omit(cr_occurrences$wcfp_name_match)))

# check non CR species, filter out: 5,478,393 rows
not_cr_occurrences <- occurrences_data[occurrences_data$CR_species == "N", ]

#save
# 10,860 rows; 134 species
write.csv(cr_occurrences, "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_CR_occurrences_dataset_2026-04-01.csv", row.names = FALSE)




# ---------------------------
# 1) Load CR occurrences dataset
cr_occurrences <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_CR_occurrences_dataset_2026-04-01.csv")
occurrences_data <- cr_occurrences

# ---------------------------
# 2) Spatial Data Preparation
# Load world land polygons (exclude Antarctica)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")
world_4326 <- st_transform(world, 4326)

# Convert occurrences to sf
occurrences_sf <- st_as_sf(
  occurrences_data,
  coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
)

# Spatial join: assign each occurrence to a country (land only)
occurrences_land <- st_join(occurrences_sf, world_4326, join = st_within, left = FALSE)

# ---------------------------
# 3) Calculate country-level species richness
# The spatial join adds the `admin` column (country name) from the world polygons
country_richness <- occurrences_land %>%
  st_drop_geometry() %>%
  filter(!is.na(admin)) %>%
  group_by(admin) %>%
  summarise(richness = n_distinct(wcfp_name_match), .groups = "drop")

# ---------------------------
# 4) Join richness data with the world map for plotting
map_data <- world_4326 %>%
  left_join(country_richness, by = "admin")

# ---------------------------
# 5) Check for unmatched countries
unmatched <- country_richness %>%
  filter(!admin %in% world_4326$admin)

if (nrow(unmatched) > 0) {
  message("WARNING: ", nrow(unmatched), " countries in your data did not match the world map:")
  print(unmatched)
} else {
  message("All countries matched successfully.")
}

# ---------------------------
# 6) Summary stats
message("Max species richness per country: ", max(map_data$richness, na.rm = TRUE))
message("Total distinct species: ", n_distinct(occurrences_land$wcfp_name_match))
message("Total countries with data: ", nrow(country_richness))

# ---------------------------
# 7) Plot the map
p <- ggplot(map_data) +
  geom_sf(aes(fill = richness), color = "gray40", size = 0.15) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "gray95",
    name = "Species richness\n(# distinct species)"
  ) +
  theme(
    panel.grid = element_line(color = "transparent"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.03, 0.05),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.box.margin = margin(0, 0, 0, 0)
  )

print(p)

# ---------------------------
# 8) Save the plot
output_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/CR_species_2026-04-01/Country_level_maps"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

output_file <- file.path(output_dir, "WCFP_CR_species_richness_map.png")
ragg::agg_png(output_file, width = 12, height = 7, units = "in", res = 300)
print(p)
dev.off()

message("Map saved to: ", output_file)


# save country richness
# save to Excel
write_xlsx(
  country_richness,
  path = "C:/Users/sarah/Downloads/CR_species_country_richness.xlsx"
)




# ---------------------------------------------------------------------#


## Map: Species richness map for all CR species in WCFP (country level)
## Author: Sarah Gora
## Three versions:
##   V1 — Simple style (original)
##   V2 — Green gradient with graticules (styled after grid maps)
##   V3 — Magma palette with graticules

# Load required packages
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")
if (!requireNamespace("cowplot", quietly = TRUE)) install.packages("cowplot")
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(viridis)
library(scales)
library(grid)
library(cowplot)
library(rnaturalearth)

# ==============================================================================
# OUTPUT DIRECTORY
# ==============================================================================
output_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/CR_species_2026-04-01/Country_level_maps"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ==============================================================================
# 1) LOAD AND PREPARE DATA
# ==============================================================================
cr_occurrences <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_CR_occurrences_dataset_2026-04-01.csv")
occurrences_data <- cr_occurrences

# Load world land polygons (exclude Antarctica)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")
world_4326 <- st_transform(world, 4326)

# Convert occurrences to sf
occurrences_sf <- st_as_sf(
  occurrences_data,
  coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
)

# Spatial join: assign each occurrence to a country (land only)
occurrences_land <- st_join(occurrences_sf, world_4326, join = st_within, left = FALSE)

# Calculate country-level species richness
country_richness <- occurrences_land %>%
  st_drop_geometry() %>%
  filter(!is.na(admin)) %>%
  group_by(admin) %>%
  summarise(richness = n_distinct(wcfp_name_match), .groups = "drop")

# Join richness to map polygons
map_data <- world_4326 %>%
  left_join(country_richness, by = "admin")

# Summary stats
max_richness <- max(country_richness$richness, na.rm = TRUE)
top_countries <- country_richness %>%
  filter(richness == max_richness)

message("Max species richness per country: ", max_richness)
message("Country/countries with max richness:")
for (i in seq_len(nrow(top_countries))) {
  message("  ", top_countries$admin[i], ": ", top_countries$richness[i], " species")
}
message("Total distinct species: ", n_distinct(occurrences_land$wcfp_name_match))
message("Total countries with data: ", nrow(country_richness))

# ==============================================================================
# HELPER FUNCTIONS (for V2 and V3)
# ==============================================================================

label_comma_over_10000 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (abs(v) > 9999) scales::comma(v) else as.character(v)
  })
}

add_graticule_breaks <- function(
    lon_breaks = seq(-180, 180, by = 60),
    lat_breaks = seq(-60, 80, by = 20)
) {
  list(
    scale_x_continuous(breaks = lon_breaks),
    scale_y_continuous(breaks = lat_breaks)
  )
}

manual_graticule_theme <- function() {
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.minor = element_blank()
  )
}

manual_graticule_coord <- function() {
  coord_sf(crs = 8857)
}

add_manual_graticule_labels <- function(
    lon_breaks = seq(-180, 180, by = 60),
    lat_breaks = seq(-60, 80, by = 20),
    text_size = 3,
    text_color = "grey35",
    lon_label_lat = -80,
    lat_label_lon = -179.5,
    lat_nudge_x = -200000,
    lon_nudge_y = -200000
) {
  lon_pts <- sf::st_as_sf(
    data.frame(lon = lon_breaks, lat = lon_label_lat),
    coords = c("lon", "lat"), crs = 4326
  )
  lon_pts$lab <- paste0(
    abs(lon_breaks), "\u00B0",
    ifelse(lon_breaks < 0, "W", ifelse(lon_breaks > 0, "E", ""))
  )
  lon_pts$lab[lon_breaks == 0] <- "0\u00B0"
  
  lat_pts <- sf::st_as_sf(
    data.frame(lon = lat_label_lon, lat = lat_breaks),
    coords = c("lon", "lat"), crs = 4326
  )
  lat_pts$lab <- paste0(
    abs(lat_breaks), "\u00B0",
    ifelse(lat_breaks < 0, "S", ifelse(lat_breaks > 0, "N", ""))
  )
  lat_pts$lab[lat_breaks == 0] <- "0\u00B0"
  
  list(
    geom_sf_text(
      data = lon_pts, aes(label = lab),
      size = text_size, color = text_color,
      vjust = 0, nudge_y = lon_nudge_y
    ),
    geom_sf_text(
      data = lat_pts, aes(label = lab),
      size = text_size, color = text_color,
      hjust = 1, nudge_x = lat_nudge_x
    )
  )
}

# Save function for V2/V3: map + separate legend placement
save_map_both <- function(p, filename, output_dir,
                          width = 14, height = 7, dpi = 600,
                          fill_scale = NULL) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # 1. Main map — no legend
  p_main <- p +
    guides(fill = "none") +
    theme(legend.position = "none")
  
  # 2. Extract fill legend
  p_fill_legend <- p +
    guides(
      fill = guide_colorbar(
        direction      = "vertical",
        title.position = "top",
        barheight      = unit(38, "mm"),
        barwidth       = unit(6,  "mm")
      )
    ) +
    theme(legend.position = "left")
  
  if (!is.null(fill_scale)) {
    p_fill_legend <- p_fill_legend + fill_scale
  }
  
  fill_legend_grob <- cowplot::get_legend(p_fill_legend)
  
  # 3. Compose final map with legend
  final_map <- cowplot::ggdraw() +
    cowplot::draw_plot(p_main, 0, 0, 1, 1) +
    cowplot::draw_plot(
      fill_legend_grob,
      x = 0.13, y = 0.34,
      width = 0.09, height = 0.28
    )
  
  # Save PNG
  png_path <- file.path(output_dir, paste0(filename, ".png"))
  ggsave(filename = png_path, plot = final_map,
         width = width, height = height, dpi = dpi, bg = "white")
  cat(sprintf("Saved: %s\n", png_path))
  
  # Save PDF
  pdf_path <- file.path(output_dir, paste0(filename, ".pdf"))
  ggsave(filename = pdf_path, plot = final_map,
         width = width, height = height, device = cairo_pdf, bg = "white")
  cat(sprintf("Saved: %s\n", pdf_path))
}

# ==============================================================================
# Shared graticule settings
# ==============================================================================
lon_breaks <- seq(-180, 180, by = 60)
lat_breaks <- seq(-60, 80, by = 20)

# Prepare projected world outline for V2/V3 (equal-area projection CRS 8857)
world_plot <- st_transform(world_4326, 8857)
map_data_proj <- st_transform(map_data, 8857)

# ==============================================================================
# VERSION 1 — Simple style (magma, no graticules)
# ==============================================================================
p1 <- ggplot(map_data) +
  geom_sf(aes(fill = richness), color = "gray40", size = 0.15) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "gray95",
    name = "Species richness\n(# distinct species)"
  ) +
  theme(
    panel.grid = element_line(color = "transparent"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.03, 0.05),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.box.margin = margin(0, 0, 0, 0)
  )

print(p1)

# Save PNG
output_file_v1_png <- file.path(output_dir, "WCFP_CR_species_richness_country_v1_simple.png")
ragg::agg_png(output_file_v1_png, width = 12, height = 7, units = "in", res = 300)
print(p1)
dev.off()
message("V1 PNG saved to: ", output_file_v1_png)

# Save PDF
output_file_v1_pdf <- file.path(output_dir, "WCFP_CR_species_richness_country_v1_simple.pdf")
ggsave(
  filename = output_file_v1_pdf,
  plot     = p1,
  width    = 12,
  height   = 7,
  device   = cairo_pdf,
  bg       = "white"
)
message("V1 PDF saved to: ", output_file_v1_pdf)

# ==============================================================================
# VERSION 2 — Green gradient with graticules (styled like grid maps)
# ==============================================================================
p2 <- ggplot() +
  geom_sf(data = world_plot, fill = "grey98", color = NA) +
  geom_sf(data = map_data_proj, aes(fill = richness), color = NA) +
  geom_sf(data = world_plot, fill = NA, color = "grey80", linewidth = 0.2) +
  scale_fill_gradient(
    #low = "lightgreen", high = "darkgreen",
    low = "#FDCBC0", high = "#8B0000",
    na.value = "white",
    name = "Number of species",
    labels = label_comma_over_10000
  ) +
  labs(title = NULL) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  add_graticule_breaks(lon_breaks = lon_breaks, lat_breaks = lat_breaks) +
  manual_graticule_theme() +
  manual_graticule_coord() +
  add_manual_graticule_labels(lon_breaks = lon_breaks, lat_breaks = lat_breaks)

save_map_both(
  p2,
  "WCFP_CR_species_richness_country_v2_green",
  output_dir,
  fill_scale = scale_fill_gradient(
    low = "lightgreen", high = "darkgreen",
    na.value = "white",
    name = "Number of species",
    labels = label_comma_over_10000
  )
)

save_map_both(
  p2,
  "WCFP_CR_species_richness_country_v4_red",
  output_dir,
  fill_scale = scale_fill_gradient(
    low = "#FDCBC0", high = "#8B0000",
    na.value = "white",
    name = "Number of species",
    labels = label_comma_over_10000
  )
)
# ==============================================================================
# VERSION 3 — Magma palette with graticules
# ==============================================================================
p3 <- ggplot() +
  geom_sf(data = world_plot, fill = "grey98", color = NA) +
  geom_sf(data = map_data_proj, aes(fill = richness), color = NA) +
  geom_sf(data = world_plot, fill = NA, color = "grey80", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "white",
    name = "Number of species",
    labels = label_comma_over_10000
  ) +
  labs(title = NULL) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  add_graticule_breaks(lon_breaks = lon_breaks, lat_breaks = lat_breaks) +
  manual_graticule_theme() +
  manual_graticule_coord() +
  add_manual_graticule_labels(lon_breaks = lon_breaks, lat_breaks = lat_breaks)

save_map_both(
  p3,
  "WCFP_CR_species_richness_country_v3_magma",
  output_dir,
  fill_scale = scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "white",
    name = "Number of species",
    labels = label_comma_over_10000
  )
)

message("All four versions saved to: ", output_dir)










## Map: Species richness map for all CR species in WCFP (TDWG Level 3)
## Author: Sarah Gora
## Red gradient with graticules

# Load required packages
if (!requireNamespace("viridis", quietly = TRUE)) install.packages("viridis")
if (!requireNamespace("cowplot", quietly = TRUE)) install.packages("cowplot")
library(dplyr)
library(readr)
library(sf)
sf::sf_use_s2(FALSE)  # Disable S2 to avoid strict spherical geometry checks
library(ggplot2)
library(viridis)
library(scales)
library(grid)
library(cowplot)
library(rnaturalearth)

# ==============================================================================
# OUTPUT DIRECTORY
# ==============================================================================
output_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/CR_species_2026-04-01/L3_maps"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ==============================================================================
# 1) LOAD AND PREPARE DATA
# ==============================================================================
cr_occurrences <- read_csv("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_CR_occurrences_dataset_2026-04-01.csv")
occurrences_data <- cr_occurrences

# Load TDWG Level 3 shapefile, repair, and remove Antarctica
twg_level3 <- st_read("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/CR_species_2026-04-01/L3/level3/level3.shp")
twg_level3 <- st_make_valid(twg_level3)
twg_level3 <- twg_level3 %>%
  filter(LEVEL1_COD != "9")  # TDWG Level 1 code "9" = Antarctica
twg_level3_4326 <- st_transform(twg_level3, 4326)
twg_level3_4326 <- st_make_valid(twg_level3_4326)

# Clip TDWG polygons to remove extreme Arctic edges that cause line artifacts
clip_box <- st_bbox(c(xmin = -180, ymin = -60, xmax = 180, ymax = 83), crs = 4326) %>%
  st_as_sfc()
twg_level3_4326 <- st_intersection(twg_level3_4326, clip_box)
twg_level3_4326 <- st_make_valid(twg_level3_4326)

# Load world land polygons (exclude Antarctica) — same as country-level maps
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::select(iso_a3, geometry, name_long = name) %>%
  dplyr::filter(name_long != "Antarctica")
world_4326 <- st_transform(world, 4326)
world_4326 <- st_make_valid(world_4326)

# Convert occurrences to sf
occurrences_sf <- st_as_sf(
  occurrences_data,
  coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
)

# Filter to land only (same as country-level maps)
occurrences_land <- st_join(occurrences_sf, world_4326, join = st_within, left = FALSE)

# ==============================================================================
# 2) Spatial join: assign land occurrences to a TDWG Level 3 region
# ==============================================================================
occurrences_twg <- st_join(occurrences_land, twg_level3_4326, join = st_within, left = FALSE)

# ==============================================================================
# 2b) Recover unmatched land points by assigning to nearest TDWG L3 region
# ==============================================================================
all_with_twg <- st_join(occurrences_land, twg_level3_4326, join = st_within, left = TRUE)
unmatched <- all_with_twg %>%
  filter(is.na(LEVEL3_COD)) %>%
  select(-LEVEL3_COD, -LEVEL3_NAM, -LEVEL2_COD, -LEVEL1_COD)

if (nrow(unmatched) > 0) {
  message("Recovering ", nrow(unmatched), " unmatched land occurrences via nearest TDWG L3 region...")
  nearest_idx <- st_nearest_feature(unmatched, twg_level3_4326)
  recovered <- cbind(
    st_drop_geometry(unmatched),
    st_drop_geometry(twg_level3_4326[nearest_idx, c("LEVEL3_COD", "LEVEL3_NAM", "LEVEL2_COD", "LEVEL1_COD")])
  ) %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  occurrences_twg <- bind_rows(occurrences_twg, recovered)
  message("Recovered ", nrow(recovered), " occurrences. Total now: ", nrow(occurrences_twg))
} else {
  message("All land occurrences matched a TDWG L3 region.")
}

# ==============================================================================
# 3) Calculate TDWG Level 3 species richness
# ==============================================================================
twg_richness <- occurrences_twg %>%
  st_drop_geometry() %>%
  filter(!is.na(LEVEL3_COD)) %>%
  group_by(LEVEL3_COD, LEVEL3_NAM) %>%
  summarise(richness = n_distinct(wcfp_name_match), .groups = "drop")

# ==============================================================================
# 4) Join richness to TDWG Level 3 polygons
# ==============================================================================
map_data_twg <- twg_level3_4326 %>%
  left_join(twg_richness, by = c("LEVEL3_COD", "LEVEL3_NAM"))

# ==============================================================================
# 5) Summary stats
# ==============================================================================
max_richness <- max(twg_richness$richness, na.rm = TRUE)
top_regions <- twg_richness %>%
  filter(richness == max_richness)

message("Max species richness per TDWG L3 region: ", max_richness)
message("Region(s) with max richness:")
for (i in seq_len(nrow(top_regions))) {
  message("  ", top_regions$LEVEL3_NAM[i], " (", top_regions$LEVEL3_COD[i], "): ", top_regions$richness[i], " species")
}
message("Total distinct species: ", n_distinct(occurrences_twg$wcfp_name_match))
message("Total TDWG L3 regions with data: ", nrow(twg_richness))
message("Occurrences on land: ", nrow(occurrences_land))
message("Occurrences matched to TDWG L3 (including recovered): ", nrow(occurrences_twg))

# Check for unmatched regions
unmatched_regions <- twg_richness %>%
  filter(!LEVEL3_COD %in% twg_level3_4326$LEVEL3_COD)

if (nrow(unmatched_regions) > 0) {
  message("WARNING: ", nrow(unmatched_regions), " TDWG L3 regions did not match the shapefile:")
  print(unmatched_regions)
} else {
  message("All TDWG L3 regions matched successfully.")
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

label_comma_over_10000 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (abs(v) > 9999) scales::comma(v) else as.character(v)
  })
}

add_graticule_breaks <- function(
    lon_breaks = seq(-180, 180, by = 60),
    lat_breaks = seq(-60, 80, by = 20)
) {
  list(
    scale_x_continuous(breaks = lon_breaks),
    scale_y_continuous(breaks = lat_breaks)
  )
}

manual_graticule_theme <- function() {
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.minor = element_blank()
  )
}

manual_graticule_coord <- function() {
  coord_sf(crs = 8857)
}

add_manual_graticule_labels <- function(
    lon_breaks = seq(-180, 180, by = 60),
    lat_breaks = seq(-60, 80, by = 20),
    text_size = 3,
    text_color = "grey35",
    lon_label_lat = -80,
    lat_label_lon = -179.5,
    lat_nudge_x = -200000,
    lon_nudge_y = -200000
) {
  lon_pts <- sf::st_as_sf(
    data.frame(lon = lon_breaks, lat = lon_label_lat),
    coords = c("lon", "lat"), crs = 4326
  )
  lon_pts$lab <- paste0(
    abs(lon_breaks), "\u00B0",
    ifelse(lon_breaks < 0, "W", ifelse(lon_breaks > 0, "E", ""))
  )
  lon_pts$lab[lon_breaks == 0] <- "0\u00B0"
  
  lat_pts <- sf::st_as_sf(
    data.frame(lon = lat_label_lon, lat = lat_breaks),
    coords = c("lon", "lat"), crs = 4326
  )
  lat_pts$lab <- paste0(
    abs(lat_breaks), "\u00B0",
    ifelse(lat_breaks < 0, "S", ifelse(lat_breaks > 0, "N", ""))
  )
  lat_pts$lab[lat_breaks == 0] <- "0\u00B0"
  
  list(
    geom_sf_text(
      data = lon_pts, aes(label = lab),
      size = text_size, color = text_color,
      vjust = 0, nudge_y = lon_nudge_y
    ),
    geom_sf_text(
      data = lat_pts, aes(label = lab),
      size = text_size, color = text_color,
      hjust = 1, nudge_x = lat_nudge_x
    )
  )
}

# Save function: map + separate legend placement
save_map_both <- function(p, filename, output_dir,
                          width = 14, height = 7, dpi = 600,
                          fill_scale = NULL) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # 1. Main map — no legend
  p_main <- p +
    guides(fill = "none") +
    theme(legend.position = "none")
  
  # 2. Extract fill legend
  p_fill_legend <- p +
    guides(
      fill = guide_colorbar(
        direction      = "vertical",
        title.position = "top",
        barheight      = unit(38, "mm"),
        barwidth       = unit(6,  "mm")
      )
    ) +
    theme(legend.position = "left")
  
  if (!is.null(fill_scale)) {
    p_fill_legend <- p_fill_legend + fill_scale
  }
  
  fill_legend_grob <- cowplot::get_legend(p_fill_legend)
  
  # 3. Compose final map with legend
  final_map <- cowplot::ggdraw() +
    cowplot::draw_plot(p_main, 0, 0, 1, 1) +
    cowplot::draw_plot(
      fill_legend_grob,
      x = 0.13, y = 0.34,
      width = 0.09, height = 0.28
    )
  
  # Save PNG
  png_path <- file.path(output_dir, paste0(filename, ".png"))
  ggsave(filename = png_path, plot = final_map,
         width = width, height = height, dpi = dpi, bg = "white")
  cat(sprintf("Saved: %s\n", png_path))
  
  # Save PDF
  pdf_path <- file.path(output_dir, paste0(filename, ".pdf"))
  ggsave(filename = pdf_path, plot = final_map,
         width = width, height = height, device = cairo_pdf, bg = "white")
  cat(sprintf("Saved: %s\n", pdf_path))
}

# ==============================================================================
# Shared graticule settings
# ==============================================================================
lon_breaks <- seq(-180, 180, by = 60)
lat_breaks <- seq(-60, 80, by = 20)

# Prepare projected data for plotting (equal-area projection CRS 8857)
world_plot <- st_transform(world_4326, 8857)
map_data_twg_proj <- st_transform(map_data_twg, 8857)

# ==============================================================================
# TDWG Level 3 — Red gradient with graticules
# ==============================================================================
p_twg <- ggplot() +
  geom_sf(data = world_plot, fill = "grey98", color = NA) +
  geom_sf(data = map_data_twg_proj, aes(fill = richness), color = "grey80", linewidth = 0.2) +
  geom_sf(data = world_plot, fill = NA, color = "grey80", linewidth = 0.2) +
  scale_fill_gradient(
    low = "#FDCBC0", high = "#8B0000",
    na.value = "white",
    name = "Number of species",
    breaks = c(4, 8, 10),
    labels = c("4", "8", "10")
  ) +
  labs(title = NULL) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA)) +
  add_graticule_breaks(lon_breaks = lon_breaks, lat_breaks = lat_breaks) +
  manual_graticule_theme() +
  manual_graticule_coord() +
  add_manual_graticule_labels(lon_breaks = lon_breaks, lat_breaks = lat_breaks)

save_map_both(
  p_twg,
  "CR_species-richness-TDWG-L3_map",
  output_dir,
  fill_scale = scale_fill_gradient(
    low = "#FDCBC0", high = "#8B0000",
    na.value = "white",
    name = "Number of species",
    breaks = c(4, 8, 10),
    labels = c("4", "8", "10")
  )
)

message("TDWG Level 3 map saved to: ", output_dir)