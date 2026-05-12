# ================================================================================
# GCCFP Complementarity Project: Occurrence Density + Species Richness Maps with 
#    Genebank and Botanic garden Points Workflow
# ================================================================================
# Description:
#   - Generates overlays for 20km grid: occurrence and species richness heatmaps
#   - Overlays: Botanic Garden, Genebank, and both, using buffered polygons (10km radius)
#   - Output: PNG to specified path, color scales matching playground
#   - Fix 1: new_scale("fill") instead of new_scale_fill() [ggplot2 4.0.x compat]
#   - Fix 2: inst_type as real column on buffer sf objects, not hardcoded string in aes()
#   - Fix 3: show.legend = "polygon" removed [breaks guide processing in ggplot2 4.0.x]


# --- Libraries ---
library(dplyr)
library(readxl)
library(readr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(stringr)
library(scales)
library(grid)
library(cowplot)
library(ggnewscale)

# --- Parameters & Paths ---
output_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/FINAL_2026-03-20/Sp_richness_gb_bg_locations_maps"

# ------------------------------------------------------------------------------
# --- Data Loading ---
# ------------------------------------------------------------------------------

# Occurrences: 5,489,253 rows
# note: use wcfp_name_match field
occurrences_data <- read_csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_occurrences_dataset_2026-03-13.csv')


# ---Prep organizations dataset ----

# Botanic garden and genebank locations: 22,276 organizations
org_geo_data <- read_excel("C:/Users/sarah/Downloads/all_institutions_locations_corrected_2026-03-09.xlsx")

#trim whitespace
org_geo_data <- org_geo_data %>%
  mutate(
    organization_type = str_trim(organization_type),
    latitude          = str_trim(latitude),
    longitude         = str_trim(longitude))

#select only GENEBANKS and BOTANIC GARDENS: 6,836
org_geo_data <- org_geo_data %>%
  filter(organization_type %in% c("Genebank", "Botanic garden"))

#drop organizations with no lat/longs: 4,893
org_geo_data <- org_geo_data %>%
  filter(
    !is.na(latitude) & latitude != "",
    !is.na(longitude) & longitude != "")

#convert lat/long to numeric
org_geo_data <- org_geo_data %>%
  mutate(
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude) )

#check for invalid coords, zero invalid coords
org_geo_data_invalid <- org_geo_data %>%
  filter(!between(latitude, -90, 90) | !between(longitude, -180, 180))




#-------------------------------------------------------------------------------------
# --- Map: Species Richness Heatmap with Jittered Botanic Garden & Genebank Points ---
# ------------------------------------------------------------------------------------

# --- Parameters ---
crs_equalarea <- 8857  # Equal Earth Projection
pt_size        <- 0.8  # Point size for all institution locations

org_geo_data_both <- org_geo_data %>%
  mutate(
    jittered_latitude  = jitter(latitude,  amount = 0.02),
    jittered_longitude = jitter(longitude, amount = 0.02)
  )

# --- Summary stats ---
cat("--- Botanic Gardens + Genebanks ---\n")
cat("Number of botanic gardens mapped:", sum(org_geo_data_both$organization_type == "Botanic garden", na.rm = TRUE), "\n")
cat("Number of genebanks mapped:      ", sum(org_geo_data_both$organization_type == "Genebank",       na.rm = TRUE), "\n")
cat("Total institutions mapped:       ", nrow(org_geo_data_both), "\n\n")

org_geo_sf_both <- st_as_sf(
  org_geo_data_both,
  coords = c("jittered_longitude", "jittered_latitude"),
  crs    = 4326,
  remove = FALSE
)

botanic_gardens_proj <- st_transform(
  org_geo_sf_both %>% filter(organization_type == "Botanic garden"),
  crs = crs_equalarea
)
genebanks_proj <- st_transform(
  org_geo_sf_both %>% filter(organization_type == "Genebank"),
  crs = crs_equalarea
)

# ------------------------------------------------------------------------------
# --- Spatial Data Preparation ---
# ------------------------------------------------------------------------------

# 1. World land polygons (exclude Antarctica)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin != "Antarctica")
world_4326 <- st_transform(world, 4326)

# 2. Convert occurrences to sf
occurrences_sf <- st_as_sf(
  occurrences_data,
  coords = c("longitude", "latitude"),
  crs = 4326, remove = FALSE
)

# 3. Filter to land only
occurrences_land <- st_join(occurrences_sf, world_4326, join = st_within, left = FALSE)

# 4. Grid / heatmap creation function
make_grid_and_tables <- function(occurrences_land, crs_equalarea, cell_size) {
  occurrences_proj <- st_transform(occurrences_land, crs = crs_equalarea)
  grid    <- st_make_grid(occurrences_proj, cellsize = c(cell_size, cell_size), what = "polygons")
  grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)
  
  point_grid_join <- st_join(occurrences_proj, grid_sf, left = FALSE)
  
  density_table <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    tally(name = "count")
  
  richness_table <- point_grid_join %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    summarise(richness = n_distinct(wcfp_name_match))
  
  grid_with_counts <- left_join(grid_sf, density_table, by = "grid_id")
  grid_with_counts$count[is.na(grid_with_counts$count)] <- 0
  
  grid_with_richness <- left_join(grid_sf, richness_table, by = "grid_id")
  grid_with_richness$richness[is.na(grid_with_richness$richness)] <- 0
  
  list(
    grid_sf            = grid_sf,
    grid_with_counts   = grid_with_counts,
    grid_with_richness = grid_with_richness
  )
}

# 5. Filter occurrence points by institution type
bg_points <- occurrences_land %>% filter(inst_type == "Botanic garden", data_source != "GBIF_observations")
gb_points <- occurrences_land %>% filter(inst_type == "Genebank")

# ------------------------------------------------------------------------------
# --- 20km Grid, Buffered Overlays ---
# ------------------------------------------------------------------------------

crs_equalarea <- 8857
cell_size     <- 20000

# 1. Build grid tables
grid_tables        <- make_grid_and_tables(occurrences_land, crs_equalarea, cell_size)
grid_with_counts   <- grid_tables$grid_with_counts
grid_with_richness <- grid_tables$grid_with_richness

# 2. Mask grid to land
world_proj         <- st_transform(world_4326, crs = crs_equalarea)
grid_land          <- st_filter(grid_with_counts,   world_proj)
grid_richness_land <- st_filter(grid_with_richness, world_proj)
world_plot         <- world_proj


# 3. Project, buffer, and add inst_type column to overlay points
pt_size <- 0.002                                                                 # <<<<<< added 
buffer_size   <- cell_size / 2  # 10,000m

bg_points_proj <- st_transform(bg_points, crs = crs_equalarea)
gb_points_proj <- st_transform(gb_points, crs = crs_equalarea)

bg_points_buf  <- st_buffer(bg_points_proj, dist = buffer_size)
gb_points_buf  <- st_buffer(gb_points_proj, dist = buffer_size)

# FIX 2: inst_type must be a real column on the sf object for aes(fill = inst_type)   # <<<< added
bg_points_buf$inst_type <- "Botanic garden"
gb_points_buf$inst_type <- "Genebank"

# 4. Project grid for plotting
grid_land_proj          <- st_transform(grid_land,          crs = crs_equalarea)
grid_richness_land_proj <- st_transform(grid_richness_land, crs = crs_equalarea)


# ------------------------------------------------------------------------------
# --- Helper Functions ---
# ------------------------------------------------------------------------------

label_comma_over_10000 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (abs(v) > 9999) scales::comma(v) else as.character(v)
  })
}

add_graticule_breaks <- function(
    lon_breaks = seq(-180, 180, by = 60),
    lat_breaks = seq(-60,   80, by = 20)
) {
  list(
    scale_x_continuous(breaks = lon_breaks),
    scale_y_continuous(breaks = lat_breaks)
  )
}

manual_graticule_theme <- function() {
  theme(
    axis.title       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.35),
    panel.grid.minor = element_blank()
  )
}

manual_graticule_coord <- function() {
  coord_sf()
}

add_manual_graticule_labels <- function(
    lon_breaks    = seq(-180, 180, by = 60),
    lat_breaks    = seq(-60,   80, by = 20),
    text_size     = 3,
    text_color    = "grey35",
    lon_label_lat = -80,
    lat_label_lon = -179.5,
    lat_nudge_x   = -200000,
    lon_nudge_y   = -200000
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
      size = text_size, color = text_color, vjust = 0, nudge_y = lon_nudge_y
    ),
    geom_sf_text(
      data = lat_pts, aes(label = lab),
      size = text_size, color = text_color, hjust = 1, nudge_x = lat_nudge_x
    )
  )
}

# set lat lon breaks for map
lon_breaks <- seq(-180, 180, by = 60)
lat_breaks <- seq(-60,   80, by = 20)



# ------------------------------------------------------------------------------
# --- Map Definitions ---
# ------------------------------------------------------------------------------

map_definitions <- list(
  list(
    type       = "richness",
    overlays   = list(
      list(name = c("Botanic garden", "Genebank"), data = list(botanic_gardens_proj, genebanks_proj), 
           color = c("purple", "orange"),      
           file = "FigS4.Speciesrichness-20km_botanicgarden-genebank-locations_map")
    ),
    fill_aes   = aes(fill = richness),
    fill_title = "Number of species",
    grid_data  = grid_richness_land_proj
  )
)

# ------------------------------------------------------------------------------
# --- Map Generation Loop ---
# ------------------------------------------------------------------------------

# Generate maps 
for (map_def in map_definitions) {
  for (overlay in map_def$overlays) {
    
    # ---- Base map and heatmap fill scale ----
    p <- ggplot() +
      geom_sf(data = world_plot, fill = "grey98", color = NA) +
      geom_sf(data = map_def$grid_data, mapping = map_def$fill_aes, color = NA) +
      scale_fill_gradient(
        low      = "lightgreen",
        high     = "darkgreen",
        trans    = "log10",
        na.value = "white",
        name     = map_def$fill_title,
        labels   = label_comma_over_10000
      ) +
      geom_sf(data = world_plot, fill = NA, color = "grey80", linewidth = 0.2) +
      new_scale("colour")   # Institution overlays use 'colour' not 'fill'!
    
    is_overlay       <- is.list(overlay$data) || !is.null(overlay$inst_type)
    has_two_overlays <- is.list(overlay$data) && length(overlay$data) == 2
    
    if (has_two_overlays) {
      name1 <- overlay$name[1]; col1 <- overlay$color[1]
      name2 <- overlay$name[2]; col2 <- overlay$color[2]
      inst_types  <- c(name1, name2)
      inst_colors <- c(col1, col2)
      # Buffered Inst polygons: don't show in legend
      p <- p +
        geom_sf(data = overlay$data[[1]], aes(colour = inst_type), fill = NA, 
                alpha = 0.8,                                                        #alpha updated, temp
                #alpha = 0.5, 
                show.legend = FALSE) +
        geom_sf(data = overlay$data[[2]], aes(colour = inst_type), fill = NA, 
                alpha = 0.8,                                                     #alpha updated, temp
                #alpha = 0.5, 
                show.legend = FALSE)
      # Dummy points for legend only
      df_legend <- data.frame(inst_type = inst_types, lon = Inf, lat = Inf)
      p <- p +
        geom_point(
          data = df_legend, aes(x = lon, y = lat, colour = inst_type),
          shape = 16, size = 3, inherit.aes = FALSE, show.legend = TRUE
        ) +
        scale_colour_manual(
          name   = "Institution type",
          values = setNames(inst_colors, inst_types)
        )
    } else if (is_overlay) {
      inst_name  <- overlay$name
      inst_color <- overlay$color
      p <- p +
        geom_sf(data = overlay$data, aes(colour = inst_type), fill = NA, 
                alpha = 0.8,                                                     #alpha updated, temp
                #alpha = 0.5, 
                show.legend = FALSE)
      # Dummy point for legend only
      df_legend <- data.frame(inst_type = inst_name, lon = Inf, lat = Inf)
      p <- p +
        geom_point(
          data = df_legend, aes(x = lon, y = lat, colour = inst_type),
          shape = 16, size = 3, inherit.aes = FALSE, show.legend = TRUE
        ) +
        scale_colour_manual(
          name   = "Institution type",
          values = setNames(inst_color, inst_name)
        )
    }
    
    # ---- Shared theme & graticule ----
    p <- p +
      labs(title = NULL) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "white", color = NA)) +
      add_graticule_breaks(lon_breaks = lon_breaks, lat_breaks = lat_breaks) +
      manual_graticule_theme() +
      manual_graticule_coord() +
      add_manual_graticule_labels(lon_breaks = lon_breaks, lat_breaks = lat_breaks)
    
    # ---- Save with correct legend logic ----
    save_map_both(p, overlay$file, output_dir)
  }
}


# --- Save map or maps ---
save_map_both(p, "FigS4.Speciesrichness-20kmgrid_botanicgarden-genebank-locations_map", output_dir)


# --- Crop white space from saved PNGs and PDFs ---
# Run this after Script has saved all maps

# --- Libraries ---
library(magick)   # install.packages("magick")

# --- Parameters ---
output_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/FINAL_2026-03-20/Sp_richness_gb_bg_locations_maps"

# Fuzz: how aggressively to trim near-white pixels (0-100%)
fuzz <- 2

# Border: white padding (in pixels) added back after trimming
# Increase for more breathing room around the map
border_px <- 20

# ------------------------------------------------------------------------------
# PNG cropping
# ------------------------------------------------------------------------------
png_files <- list.files(output_dir, pattern = "\\.png$", full.names = TRUE)

cat("--- Cropping PNGs ---\n")
for (f in png_files) {
  img         <- image_read(f)
  img_trimmed <- image_trim(img, fuzz = fuzz)
  img_padded  <- image_border(img_trimmed, color = "white",
                              geometry = paste0(border_px, "x", border_px))
  image_write(img_padded, path = f)
  info <- image_info(img_padded)
  cat(sprintf("Cropped: %s  [%d x %d px]\n", basename(f), info$width, info$height))
}

# ------------------------------------------------------------------------------
# PDF cropping
# ------------------------------------------------------------------------------
pdf_files <- list.files(output_dir, pattern = "\\.pdf$", full.names = TRUE)

cat("\n--- Cropping PDFs ---\n")
for (f in pdf_files) {
  img         <- image_read_pdf(f, density = 300)
  img_trimmed <- image_trim(img, fuzz = fuzz)
  img_padded  <- image_border(img_trimmed, color = "white",
                              geometry = paste0(border_px, "x", border_px))
  image_write(img_padded, path = f, format = "pdf")
  cat(sprintf("Cropped: %s\n", basename(f)))
}

cat("\nDone. All files cropped in place.\n")


### end script ###