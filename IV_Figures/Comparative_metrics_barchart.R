# =============================================================================
# Complementarity figure - Figure 3
# All numbers calculated from CURRENT DATASETS
# =============================================================================

library(readr)
library(readxl)
library(dplyr)
library(grid)

# ---- Helper: comma formatting ----
comma_format <- function(n) {
  ifelse(abs(n) < 10000, as.character(n), format(n, big.mark = ",", scientific = FALSE))
}

# ---- Data in ----
genebank_accessionlevel_dataset     <- read_csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_genebank_accessionlevel_dataset_2026-03-13.csv')
botanicgarden_accessionlevel_dataset <- read_csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_botanicgarden_accessionlevel_dataset_2026-03-13.csv')
botanicgarden_specieslevel_dataset   <- read_csv('C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Data/Datasets_FINAL/WCFP_botanicgarden_specieslevel_dataset_2026-03-13.csv')
plantlist                           <- read_excel("C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/WCFP_plantlist/Standardized/WCFP_plantlist_standardized_2026-02-23_updated.xlsx")

# =============================================================================
# === DYNAMIC METRICS BLOCK ===
# =============================================================================

# Bar 1: Number of institutions
n_inst_genebank <- length(unique(genebank_accessionlevel_dataset$inst_code))
n_inst_botgarden <- length(unique(c(
  botanicgarden_accessionlevel_dataset$inst_code,
  botanicgarden_specieslevel_dataset$inst_code)))
n_inst_total <- n_distinct(
  c(
    genebank_accessionlevel_dataset$inst_code,
    botanicgarden_accessionlevel_dataset$inst_code,
    botanicgarden_specieslevel_dataset$inst_code))

# Bar 2: Number of species
all_species <- unique(plantlist$taxon_name_accepted)
species_in_genebank <- unique(genebank_accessionlevel_dataset$wcfp_name_match)
species_in_botgarden <- unique(c(
  botanicgarden_accessionlevel_dataset$wcfp_name_match,
  botanicgarden_specieslevel_dataset$wcfp_name_match))
total_species <- length(all_species)
only_genebank  <- sum((all_species %in% species_in_genebank) & !(all_species %in% species_in_botgarden))
only_botgarden <- sum((all_species %in% species_in_botgarden) & !(all_species %in% species_in_genebank))
both           <- sum((all_species %in% species_in_genebank) & (all_species %in% species_in_botgarden))
neither        <- sum(!(all_species %in% species_in_genebank) & !(all_species %in% species_in_botgarden))

# Bar 3: Number of accessions
n_acc_genebank  <- nrow(genebank_accessionlevel_dataset)
n_acc_botgarden <- nrow(botanicgarden_accessionlevel_dataset)
n_acc_total     <- n_acc_genebank + n_acc_botgarden

# Proportions for bar widths
bar1_genebank_prop  <- n_inst_genebank / n_inst_total
bar1_botgarden_prop <- n_inst_botgarden / n_inst_total

bar2_only_genebank_prop  <- only_genebank  / total_species
bar2_only_botgarden_prop <- only_botgarden / total_species
bar2_both_prop           <- both           / total_species
bar2_neither_prop        <- neither        / total_species

bar3_genebank_prop  <- n_acc_genebank / n_acc_total
bar3_botgarden_prop <- n_acc_botgarden / n_acc_total

# =============================================================================
# === DRAWING + OUTPUT CODE ===
# =============================================================================

rect_npc <- function(xl, xr, yb, yt, lwd = 0.9, col = "black", fill = NA) {
  grid.rect(
    x = (xl + xr) / 2, y = (yb + yt) / 2,
    width = xr - xl, height = yt - yb,
    default.units = "npc",
    gp = gpar(col = col, fill = fill, lwd = lwd, lineend = "butt")
  )
}

make_mapper <- function(pad = c(l = 0.018, r = 0.012, b = 0.020, t = 0.030)) {
  function(xl, xr, yb, yt) {
    list(
      xl = pad["l"] + xl * (1 - pad["l"] - pad["r"]),
      xr = pad["l"] + xr * (1 - pad["l"] - pad["r"]),
      yb = pad["b"] + yb * (1 - pad["b"] - pad["t"]),
      yt = pad["b"] + yt * (1 - pad["b"] - pad["t"])
    )
  }
}

draw_bar <- function(map,
                     yb, yt,
                     top_title,
                     boxes,                 
                     bar_xl = 0.00,
                     bar_xr = 1.00,
                     title_frac = 0.16,    
                     bar_lwd = 1.1,
                     box_lwd = 0.9,
                     title_cex = 0.90,
                     text_cex = 0.88) {
  h <- yt - yb
  y_title_b <- yt - title_frac * h
  y_bar_t   <- y_title_b
  bb_title <- map(bar_xl, bar_xr, y_title_b, yt)
  grid.text(
    top_title,
    x = bb_title$xl, y = bb_title$yt,
    default.units = "npc",
    just = c("left", "top"),
    gp = gpar(cex = title_cex, fontface = 2, lineheight = 0.95)
  )
  bb_bar <- map(bar_xl, bar_xr, yb, y_bar_t)
  rect_npc(bb_bar$xl, bb_bar$xr, bb_bar$yb, bb_bar$yt, lwd = bar_lwd, fill = NA)
  for (b in boxes) {
    ib <- map(
      bar_xl + (bar_xr - bar_xl) * b$xl,
      bar_xl + (bar_xr - bar_xl) * b$xr,
      yb, y_bar_t
    )
    rect_npc(ib$xl, ib$xr, ib$yb, ib$yt, lwd = box_lwd, fill = b$fill)
    bar_width <- ib$xr - ib$xl
    dynamic_cex <- text_cex * max(0.75, min(1, bar_width * 5))
    if (bar_width < 0.10) {
      grid.text(
        b$text,
        x = (ib$xl + ib$xr)/2, y = (ib$yb + ib$yt)/2,
        default.units = "npc",
        just = c("centre", "centre"),
        gp = gpar(cex = dynamic_cex, fontface = 2, lineheight = 0.95),
        rot = 90
      )
    } else {
      grid.text(
        b$text,
        x = (ib$xl + ib$xr)/2, y = (ib$yb + ib$yt)/2,
        default.units = "npc",
        just = c("centre", "centre"),
        gp = gpar(cex = dynamic_cex, fontface = 2, lineheight = 0.95)
      )
    }
  }
}

draw_figure <- function() {
  grid.newpage()
  map <- make_mapper()
  col_genebanks <- "#F28E2B"
  col_botanic   <- "#7B61FF"
  col_both      <- "#59A14F"
  col_neither   <- "#9D9D9D"
  
  gap <- 0.045
  band_h <- (1 - 2 * gap) / 3
  
  y1t <- 1.00; y1b <- y1t - band_h
  y2t <- y1b - gap; y2b <- y2t - band_h
  y3t <- y2b - gap; y3b <- y3t - band_h
  
  bar_xl <- 0.00; bar_xr <- 1.00
  
  # Bar 1
  draw_bar(
    map, yb = y1b, yt = y1t,
    top_title = sprintf("a) Number of institutions = %s", comma_format(n_inst_total)),
    boxes = list(
      list(
        xl = 0.00,
        xr = bar1_genebank_prop,
        text = sprintf("Genebanks\n%s", comma_format(n_inst_genebank)), 
        fill = col_genebanks
      ),
      list(
        xl = bar1_genebank_prop, 
        xr = 1.00,
        text = sprintf("Botanic gardens\n%s", comma_format(n_inst_botgarden)), 
        fill = col_botanic
      )
    ),
    bar_xl = bar_xl, bar_xr = bar_xr
  )
  
  # Bar 2
  draw_bar(
    map, yb = y2b, yt = y2t,
    top_title = sprintf("b) Number of species = %s", comma_format(total_species)),
    boxes = list(
      list(
        xl = 0.00,
        xr = bar2_only_genebank_prop,
        text = sprintf("Only genebanks\n%s", comma_format(only_genebank)), 
        fill = col_genebanks
      ),
      list(
        xl = bar2_only_genebank_prop,
        xr = bar2_only_genebank_prop + bar2_only_botgarden_prop,
        text = sprintf("Only botanic gardens\n%s", comma_format(only_botgarden)), 
        fill = col_botanic
      ),
      list(
        xl = bar2_only_genebank_prop + bar2_only_botgarden_prop,
        xr = bar2_only_genebank_prop + bar2_only_botgarden_prop + bar2_both_prop,
        text = sprintf("Both\n%s", comma_format(both)), 
        fill = col_both
      ),
      list(
        xl = bar2_only_genebank_prop + bar2_only_botgarden_prop + bar2_both_prop,
        xr = 1.00,
        text = sprintf("Neither\n%s", comma_format(neither)), 
        fill = col_neither
      )
    ),
    bar_xl = bar_xl, bar_xr = bar_xr
  )
  
  # Bar 3
  draw_bar(
    map, yb = y3b, yt = y3t,
    top_title = sprintf("c) Number of accessions = %s", comma_format(n_acc_total)),
    boxes = list(
      list(
        xl = 0.00,
        xr = bar3_genebank_prop,
        text = sprintf("Genebanks\n%s", comma_format(n_acc_genebank)), 
        fill = col_genebanks
      ),
      list(
        xl = bar3_genebank_prop,
        xr = 1.00,
        text = sprintf("Botanic gardens\n%s", comma_format(n_acc_botgarden)), 
        fill = col_botanic
      )
    ),
    bar_xl = bar_xl, bar_xr = bar_xr
  )
}

out_pdf <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/FINAL_2026-03-20/Fig3.Comparative-metrics_barchart.pdf"
out_png <- "C:/Users/sarah/OneDrive/Desktop/GCCFP_final/GCCFP_final/Figures/FINAL_2026-03-20/Fig3.Comparative-metrics_barchart.png"
dir.create(dirname(out_pdf), recursive = TRUE, showWarnings = FALSE)

# PDF
cairo_pdf(out_pdf, width = 8.5, height = 4.8)
draw_figure()
dev.off()

# PNG (600 dpi)
png(out_png, width = 8.5, height = 4.8, units = "in", res = 600, type = "cairo")
draw_figure()
dev.off()