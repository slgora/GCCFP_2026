library(ggplot2)

# Create the data frame
plants <- data.frame(
  group = c("Food Plants", "Rhododendron", "Erica", "Ebenaceae", "Dipterocarps",
            "Conifers", "Oak", "Magnolia", "Cycads",
            "Whitebeams, Rowans, and Service Trees", "Acer", "Nothofagus"),
  count = c(26632, 1200, 860, 855, 695, 650, 500, 300, 350, 288, 160, 43)
)

# Reorder factor levels by count (descending)
plants$group <- reorder(plants$group, plants$count)

# Custom label: comma only for numbers over 10,000
plants$label <- ifelse(plants$count > 10000,
                       format(plants$count, big.mark = ","),
                       as.character(plants$count))

# Plot
ggplot(plants, aes(x = group, y = count)) +
  geom_col(fill = "#2a9d8f", show.legend = FALSE, width = 0.8) +
  geom_text(aes(label = label),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = scales::comma) +
  labs(
    x = NULL,
    y = "Number of Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 30, 10, 10)
  )

# Save the figure
ggsave("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/species_count_by_GCC.png",
       width = 10, height = 7, dpi = 300)











#-----------------------------------#

# CREATE CORP IMAGES
library(magick)

img <- image_read("C:/Users/sarah/Downloads/GCCs_all.jpg")
img <- image_border(img, color = "white", geometry = "50x50")

icons <- list(
  "Acer"          = list(cx=76,  cy=73,  cw=122),
  "Conifers"      = list(cx=200, cy=73,  cw=122),
  "Cycads"        = list(cx=318, cy=73,  cw=115),
  "Dipterocarps"  = list(cx=440, cy=73,  cw=125),
  "Ebenaceae"     = list(cx=555, cy=73,  cw=115),
  "Erica"         = list(cx=675, cy=73,  cw=125),
  "Magnolia"      = list(cx=76,  cy=205, cw=122),
  "Nothofagus"    = list(cx=190, cy=205, cw=120),
  "Oak"           = list(cx=305, cy=205, cw=112),
  "Rhododendron"  = list(cx=430, cy=205, cw=135),
  "Whitebeams"    = list(cx=555, cy=205, cw=115),
  "Food_Plants"   = list(cx=675, cy=205, cw=125)
)

out_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/icon_crops"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

crop_h <- 130
text_extra <- 22

for (name in names(icons)) {
  i <- icons[[name]]
  cx <- i$cx + 50
  cy <- i$cy + 50
  cw <- i$cw
  
  x_off <- floor(cx - cw / 2)
  y_off <- floor(cy - crop_h / 2)
  
  geom <- paste0(cw, "x", (crop_h + text_extra), "+", x_off, "+", y_off)
  icon <- image_crop(img, geom)
  
  out_path <- file.path(out_dir, paste0(name, ".png"))
  image_write(icon, out_path)
  cat("Saved:", name, "-", image_info(icon)$width, "x", image_info(icon)$height, "\n")
}

cat("\nAll crops saved to:", out_dir, "\n")




### CROP IMAGES
icon_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/icon_crops"

files <- list.files(icon_dir, pattern = "\\.png$", full.names = TRUE)

# Step 1: Trim all images and find max dimensions
trimmed <- list()
max_w <- 0
max_h <- 0

for (f in files) {
  img <- image_read(f)
  img <- image_trim(img, fuzz = 10)
  info <- image_info(img)
  max_w <- max(max_w, info$width)
  max_h <- max(max_h, info$height)
  trimmed[[f]] <- img
  cat(basename(f), "- trimmed to:", info$width, "x", info$height, "\n")
}

cat("\nMax dimensions:", max_w, "x", max_h, "\n")

# Step 2: Pad and save at native resolution with 300 DPI
for (f in files) {
  img <- trimmed[[f]]
  
  img <- image_extent(img,
                      geometry = paste0(max_w, "x", max_h),
                      gravity = "center",
                      color = "white")
  
  image_write(img, f, format = "png", density = "300x300")
  cat("Saved:", basename(f), "->", max_w, "x", max_h, "\n")
}

cat("\nDone! All images trimmed and padded to", max_w, "x", max_h, "at 300 DPI\n")



# ------------------------------------------------#
# ----- updated figure with icons-----------------#
library(ggplot2)
library(ggimage)

# Icon directory
icon_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/icon_crops"

# Map group names to icon file paths
icon_paths <- list(
  "Acer"          = file.path(icon_dir, "Acer.png"),
  "Conifers"      = file.path(icon_dir, "Conifers.png"),
  "Cycads"        = file.path(icon_dir, "Cycads.png"),
  "Dipterocarps"  = file.path(icon_dir, "Dipterocarps.png"),
  "Ebenaceae"     = file.path(icon_dir, "Ebenaceae.png"),
  "Erica"         = file.path(icon_dir, "Erica.png"),
  "Magnolia"      = file.path(icon_dir, "Magnolia.png"),
  "Nothofagus"    = file.path(icon_dir, "Nothofagus.png"),
  "Oak"           = file.path(icon_dir, "Oak.png"),
  "Rhododendron"  = file.path(icon_dir, "Rhododendron.png"),
  "Whitebeams, Rowans, and Service Trees" = file.path(icon_dir, "Whitebeams.png"),
  "Food Plants"   = file.path(icon_dir, "Food_Plants.png")
)

# Create the data frame
plants <- data.frame(
  group = c("Food Plants", "Rhododendron", "Erica", "Ebenaceae", "Dipterocarps",
            "Conifers", "Oak", "Magnolia", "Cycads",
            "Whitebeams, Rowans, and Service Trees", "Acer", "Nothofagus"),
  count = c(26632, 1200, 860, 855, 695, 650, 500, 300, 350, 288, 160, 43)
)

plants$group <- reorder(plants$group, plants$count)
plants$label <- ifelse(plants$count > 10000,
                       format(plants$count, big.mark = ","),
                       as.character(plants$count))
plants$icon <- sapply(as.character(plants$group), function(g) icon_paths[[g]])

plants$icon_y <- ifelse(as.character(plants$group) == "Dipterocarps", -750, -900)

# Plot
ggplot(plants, aes(x = group, y = count)) +
  geom_col(fill = "#2a9d8f", width = 0.8) +
  geom_text(aes(label = label), hjust = -0.1, size = 3.5) +
  geom_image(aes(x = group, y = icon_y, image = icon), size = 0.055, asp = 1.5) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.15)),
                     breaks = c(0, 10000, 20000, 30000),
                     labels = c("", "10,000", "20,000", "30,000")) +
  labs(x = NULL, y = "Number of Species") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 30, 10, 10),
    axis.text.y = element_blank()
  )

# Save
ggsave("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/species_count_by_GCC_icons.png",
       width = 12, height = 7, dpi = 300, bg = "white")















##################### ALT Figure ##############

#----------------------------- TREEMAP ----------------------------------

library(ggplot2)
library(treemapify)
library(ggimage)

icon_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/icon_crops"

plants <- data.frame(
  group = c("Food Plants", "Rhododendron", "Erica", "Ebenaceae", "Dipterocarps",
            "Conifers", "Oak", "Magnolia", "Cycads", "Whitebeams", "Acer", "Nothofagus"),
  count = c(26632, 1200, 860, 855, 695, 650, 500, 300, 350, 288, 160, 43)
)

plants$label <- paste0(plants$group, "\n", format(plants$count, big.mark = ","))

ggplot(plants, aes(area = count, fill = group, label = label)) +
  geom_treemap(color = "white", size = 2) +
  geom_treemap_text(place = "centre", grow = FALSE, size = 12, color = "white",
                    fontface = "bold", lineheight = 0.85) +
  scale_fill_manual(values = c(
    "Food Plants" = "#2a9d8f", "Rhododendron" = "#e76f51", "Erica" = "#9b5de5",
    "Ebenaceae" = "#f4a261", "Dipterocarps" = "#e9c46a", "Conifers" = "#264653",
    "Oak" = "#6a994e", "Magnolia" = "#bc6c25", "Cycads" = "#0077b6",
    "Whitebeams" = "#80b918", "Acer" = "#5a189a", "Nothofagus" = "#d62828")) +
  theme_void() +
  theme(legend.position = "none")

ggsave("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/species_treemap.png",
       width = 12, height = 8, dpi = 300, bg = "white")


#----------------------------- CIRCLE PACKING ----------------------------------
library(ggplot2)
library(ggimage)
library(packcircles)

icon_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/icon_crops"

plants <- data.frame(
  group = c("Food Plants", "Rhododendron", "Erica", "Ebenaceae", "Dipterocarps",
            "Conifers", "Oak", "Magnolia", "Cycads", "Whitebeams", "Acer", "Nothofagus"),
  count = c(26632, 1200, 860, 855, 695, 650, 500, 300, 350, 288, 160, 43)
)

icon_files <- c("Food_Plants.png", "Rhododendron.png", "Erica.png", "Ebenaceae.png",
                "Dipterocarps.png", "Conifers.png", "Oak.png", "Magnolia.png",
                "Cycads.png", "Whitebeams.png", "Acer.png", "Nothofagus.png")
plants$icon <- file.path(icon_dir, icon_files)

# Pack circles with radius proportional to sqrt(count)
packing <- circleProgressiveLayout(plants$count, sizetype = "area")
plants$x <- packing$x
plants$y <- packing$y
plants$r <- packing$radius

# Circle outlines
circle_dat <- circleLayoutVertices(packing, npoints = 100)

# Icon size proportional to radius
plants$img_size <- plants$r / max(plants$r) * 0.18

plants$label <- paste0(plants$group, "\n", format(plants$count, big.mark = ","))

ggplot() +
  geom_polygon(data = circle_dat, aes(x, y, group = id),
               fill = "#2a9d8f", alpha = 0.15, color = "#2a9d8f", linewidth = 0.5) +
  geom_image(data = plants, aes(x = x, y = y, image = icon, size = I(img_size))) +
  geom_text(data = plants, aes(x = x, y = y - r * 0.7, label = label),
            size = 2.5, lineheight = 0.85) +
  coord_fixed() +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))

ggsave("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/species_circle_packing.png",
       width = 12, height = 10, dpi = 300, bg = "white")




# ---------------- RADIAL BAR-------------
library(ggplot2)
library(ggimage)

icon_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/icon_crops"

plants <- data.frame(
  group = c("Food Plants", "Rhododendron", "Erica", "Ebenaceae", "Dipterocarps",
            "Conifers", "Oak", "Magnolia", "Cycads", "Whitebeams", "Acer", "Nothofagus"),
  count = c(26632, 1200, 860, 855, 695, 650, 500, 300, 350, 288, 160, 43)
)

icon_files <- c("Food_Plants.png", "Rhododendron.png", "Erica.png", "Ebenaceae.png",
                "Dipterocarps.png", "Conifers.png", "Oak.png", "Magnolia.png",
                "Cycads.png", "Whitebeams.png", "Acer.png", "Nothofagus.png")
plants$icon <- file.path(icon_dir, icon_files)

plants <- plants[order(-plants$count), ]
n <- nrow(plants)

bar_width <- 2 * pi / n
inner_r <- 2000

plants$angle_start <- seq(0, 2 * pi - bar_width, length.out = n) + pi/2
plants$angle_end <- plants$angle_start + bar_width * 0.85
plants$angle_mid <- (plants$angle_start + plants$angle_end) / 2
plants$outer_r <- inner_r + plants$count

n_pts <- 50
wedge_list <- list()
for (i in 1:n) {
  angles <- seq(plants$angle_start[i], plants$angle_end[i], length.out = n_pts)
  x_inner <- inner_r * cos(rev(angles))
  y_inner <- inner_r * sin(rev(angles))
  x_outer <- plants$outer_r[i] * cos(angles)
  y_outer <- plants$outer_r[i] * sin(angles)
  
  wedge_list[[i]] <- data.frame(
    x = c(x_inner, x_outer),
    y = c(y_inner, y_outer),
    group_name = plants$group[i],
    id = i
  )
}
wedges <- do.call(rbind, wedge_list)

plants$icon_x <- (plants$outer_r + 1200) * cos(plants$angle_mid)
plants$icon_y <- (plants$outer_r + 1200) * sin(plants$angle_mid)

plants$label_x <- (plants$outer_r + 2500) * cos(plants$angle_mid)
plants$label_y <- (plants$outer_r + 2500) * sin(plants$angle_mid)

# Calculate tight bounds from all plotted elements
all_x <- c(wedges$x, plants$icon_x, plants$label_x)
all_y <- c(wedges$y, plants$icon_y, plants$label_y)
pad <- 1500

p <- ggplot() +
  geom_polygon(data = wedges, aes(x = x, y = y, group = id),
               fill = "#2a9d8f", color = "white", linewidth = 0.5) +
  geom_image(data = plants, aes(x = icon_x, y = icon_y, image = icon),
             size = 0.035, asp = 1) +
  geom_text(data = plants,
            aes(x = label_x, y = label_y,
                label = format(count, big.mark = ",")),
            angle = 0, size = 3.5, fontface = "bold") +
  coord_fixed(xlim = c(min(all_x) - pad, max(all_x) + pad),
              ylim = c(min(all_y) - pad, max(all_y) + pad),
              clip = "off") +
  theme_void() +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

# Calculate aspect ratio to match the data
x_range <- max(all_x) + pad - (min(all_x) - pad)
y_range <- max(all_y) + pad - (min(all_y) - pad)
aspect <- x_range / y_range
fig_height <- 14
fig_width <- fig_height * aspect

print(p)

ggsave("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/species_radial.png",
       plot = p, width = fig_width, height = fig_height, dpi = 300, bg = "white")



#---------------- LOLLIPOP-----------------#
library(ggplot2)
library(ggimage)

icon_dir <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/icon_crops"

plants <- data.frame(
  group = c("Food Plants", "Rhododendron", "Erica", "Ebenaceae", "Dipterocarps",
            "Conifers", "Oak", "Magnolia", "Cycads", "Whitebeams", "Acer", "Nothofagus"),
  count = c(26632, 1200, 860, 855, 695, 650, 500, 300, 350, 288, 160, 43)
)

icon_files <- c("Food_Plants.png", "Rhododendron.png", "Erica.png", "Ebenaceae.png",
                "Dipterocarps.png", "Conifers.png", "Oak.png", "Magnolia.png",
                "Cycads.png", "Whitebeams.png", "Acer.png", "Nothofagus.png")
plants$icon <- file.path(icon_dir, icon_files)

plants$group <- reorder(plants$group, -plants$count)

plants$label <- ifelse(plants$count >= 10000,
                       format(plants$count, big.mark = ","),
                       as.character(plants$count))

plants$icon_y <- plants$count + 1500

plants$icon_nudge_x <- 0
plants$icon_nudge_x[plants$group == "Dipterocarps"] <- 0.05

ggplot(plants, aes(x = group)) +
  geom_segment(aes(x = group, xend = group, y = 0, yend = count),
               color = "#2a9d8f", linewidth = 1.2) +
  geom_image(aes(y = icon_y, image = icon, x = as.numeric(group) + icon_nudge_x),
             size = 0.06, asp = 1.5) +
  geom_text(aes(y = icon_y, label = label), vjust = -5, size = 4.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12)),
                     breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000),
                     labels = c("0", "5000", "10,000", "15,000", "20,000", "25,000", "30,000")) +
  labs(x = NULL, y = "Number of Species") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16)
  )

ggsave("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/GCC_species_count_fig/species_lollipop.png",
       width = 18, height = 12, dpi = 300, bg = "white")
