# ============================================================
# Script: Species–area and sampling relationships analysis
# Project: Neotropical dry forest bees
# Author: Herrera-Motta et al.
# Description:
# This script computes species–area relationships at the country
# level for DRYFLOR and TEOW regions. It:
# 1. Calculates biome area per country
# 2. Merges area with richness and occurrence data
# 3. Fits log–log linear models
# 4. Generates publication-ready plots with confidence intervals
# ============================================================

library(terra)
library(ggplot2)
library(dplyr)
library(gridExtra)

# =========================
# Paths
# =========================
setwd("./NDF_bees_project/")

america_path <- "./Shapefiles/America/Neotropics_croped.shp"

dryflor_path <- "./Shapefiles/STDF/dryflor/dryflor_metric.shp"
teow_path    <- "./Shapefiles/STDF/teow/teow_metric.shp"

dryflor_csv <- "./Data/summary_diversity/dryflor/summary_per_country_dryflor.csv"
teow_csv    <- "./Data/summary_diversity/teow/summary_per_country_teow.csv"

dryflor_outdir <- "./Shapefiles/STDF/per_country/dryflor/"
teow_outdir    <- "./Shapefiles/STDF/per_country/teow/"

dir.create(dryflor_outdir, showWarnings = FALSE, recursive = TRUE)
dir.create(teow_outdir, showWarnings = FALSE, recursive = TRUE)

# =========================
# Base objects
# =========================

america <- vect(america_path)

countries_dryflor <- c(
  "Argentina","Bolivia","Brazil","Colombia","Costa Rica","Cuba",
  "Dominican Republic","Ecuador","Guatemala","Honduras","Haiti",
  "Jamaica","Saint Lucia","Mexico","Nicaragua","Peru",
  "Puerto Rico","Paraguay","El Salvador","Venezuela"
)

countries_teow <- c(
  "Bolivia","Brazil","Colombia","Costa Rica","Cuba",
  "Dominican Republic","Ecuador","Grenada","Guatemala","Honduras",
  "Haiti","Jamaica","Saint Lucia","Montserrat","Mexico",
  "Nicaragua","Panama","Peru","Puerto Rico","El Salvador",
  "Trinidad and Tobago","Venezuela"
)

# =========================
# Helper functions
# =========================

theme_species_area <- function() {
  theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      plot.title   = element_text(face = "bold")
    )
}

compute_country_area <- function(america_vect, biome_vect, countries, outdir, prefix) {
  america_proj <- project(america_vect, crs(biome_vect))
  
  results <- data.frame(country = character(), area = numeric())
  
  for (country_name in countries) {
    country_vect <- america_proj[america_proj$COUNTRY == country_name, ]
    biome_crop   <- crop(biome_vect, country_vect)
    
    file_name <- tolower(gsub(" ", "_", country_name))
    outfile   <- file.path(outdir, paste0(prefix, "_", file_name, ".shp"))
    
    writeVector(biome_crop, outfile, overwrite = TRUE)
    
    area_km2 <- sum(expanse(biome_crop, unit = "km"))
    
    results <- rbind(results,
      data.frame(country = country_name, area = area_km2)
    )
  }
  
  results
}

prepare_species_area_data <- function(area_df, summary_df, richness_col, occurrences_col) {
  out <- data.frame(
    country = area_df$country,
    area = as.numeric(area_df$area),
    richness = as.numeric(summary_df[[richness_col]]),
    occurrences = as.numeric(summary_df[[occurrences_col]])
  )
  
  subset(out, area > 0 & richness > 0 & occurrences > 0)
}

make_prediction_df <- function(model, predictor_name, data_df) {
  newdata <- data.frame(
    seq(min(data_df[[predictor_name]]), max(data_df[[predictor_name]]), length = 100)
  )
  names(newdata) <- predictor_name
  
  pred <- predict(model, newdata = newdata, interval = "confidence")
  cbind(newdata, pred)
}

plot_log_relationship <- function(data_df, pred_df, model, x_var, y_var, color, title, x_lab, y_lab) {
  
  ggplot(data_df, aes_string(x = x_var, y = y_var)) +
    geom_point(color = color) +
    geom_ribbon(
      data = pred_df,
      aes_string(x = x_var, ymin = "exp(lwr)", ymax = "exp(upr)"),
      fill = color, alpha = 0.2, inherit.aes = FALSE
    ) +
    geom_line(
      data = pred_df,
      aes_string(x = x_var, y = "exp(fit)"),
      color = color, linewidth = 1, inherit.aes = FALSE
    ) +
    scale_x_log10() +
    scale_y_log10() +
    labs(title = title, x = x_lab, y = y_lab) +
    theme_species_area()
}

# =========================
# DRYFLOR
# =========================

dryflor_vect <- vect(dryflor_path)
summary_per_country_dryflor <- read.csv(dryflor_csv)

dryflor_area_df <- compute_country_area(
  america, dryflor_vect, countries_dryflor,
  dryflor_outdir, "dryflor"
)

dryflor_species_area <- prepare_species_area_data(
  dryflor_area_df, summary_per_country_dryflor,
  "Species", "total"
)

model_rich_occ_dryflor <- lm(log(richness) ~ log(occurrences), data = summary_per_country_dryflor)
pred_rich_occ_dryflor  <- make_prediction_df(model_rich_occ_dryflor, "occurrences", summary_per_country_dryflor)

# =========================
# TEOW
# =========================

teow_vect <- vect(teow_path)
summary_per_country_teow <- read.csv(teow_csv)

teow_area_df <- compute_country_area(
  america, teow_vect, countries_teow,
  teow_outdir, "teow"
)

teow_species_area <- prepare_species_area_data(
  teow_area_df, summary_per_country_teow,
  "species", "total"
)

model_rich_occ_teow <- lm(log(richness) ~ log(occurrences), data = summary_per_country_teow)
pred_rich_occ_teow  <- make_prediction_df(model_rich_occ_teow, "occurrences", summary_per_country_teow)

# =========================
# Final panel (example)
# =========================

plots_area <- list(
  plot_log_relationship(summary_per_country_dryflor, pred_rich_occ_dryflor, model_rich_occ_dryflor,
                        "occurrences", "richness", "darkblue", "a", "Occurrences", "Richness"),
  plot_log_relationship(summary_per_country_teow, pred_rich_occ_teow, model_rich_occ_teow,
                        "occurrences", "richness", "darkred", "b", "Occurrences", "Richness")
)

grid_species_occ_area <- grid.arrange(grobs = plots_area, ncol = 2)

ggsave(
  "./Figures/species_occ_area.jpg",
  grid_species_occ_area,
  width = 20,
  height = 10,
  units = "cm",
  dpi = 300
)
