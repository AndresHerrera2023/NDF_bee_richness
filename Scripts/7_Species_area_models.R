### Species-area relationships
### Project: Neotropical dry forest bees
### Authors: Herrera-Motta et al.
### Last update: 2026-03-28

library(terra)
library(ggplot2)
library(dplyr)
library(gridExtra)

# =========================
# Paths
# =========================
setwd("Z:/Andres/NDF_bees_project/2026_version/improved")

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
  "Argentina", "Bolivia", "Brazil", "Colombia", "Costa Rica", "Cuba",
  "Dominican Republic", "Ecuador", "Guatemala", "Honduras", "Haiti",
  "Jamaica", "Saint Lucia", "Mexico", "Nicaragua", "Peru",
  "Puerto Rico", "Paraguay", "El Salvador", "Venezuela"
)

countries_teow <- c(
  "Bolivia", "Brazil", "Colombia", "Costa Rica", "Cuba",
  "Dominican Republic", "Ecuador", "Grenada", "Guatemala", "Honduras",
  "Haiti", "Jamaica", "Saint Lucia", "Montserrat", "Mexico",
  "Nicaragua", "Panama", "Peru", "Puerto Rico", "El Salvador",
  "Trinidad and Tobago", "Venezuela"
)

# =========================
# Helper functions
# =========================

theme_species_area <- function() {
  theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_text(face = "bold", size = 12),
      axis.title.y = element_text(face = "bold", size = 12),
      plot.title   = element_text(face = "bold", size = 12)
    )
}

compute_country_area <- function(america_vect, biome_vect, countries, outdir, prefix) {
  america_proj <- project(america_vect, crs(biome_vect))
  
  results <- data.frame(
    country = character(),
    area = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (country_name in countries) {
    cat("Procesando:", country_name, "\n")
    
    country_vect <- america_proj[america_proj$COUNTRY == country_name, ]
    biome_crop   <- crop(biome_vect, country_vect)
    
    file_name <- tolower(gsub(" ", "_", country_name))
    outfile   <- file.path(outdir, paste0(prefix, "_", file_name, ".shp"))
    
    writeVector(biome_crop, outfile, overwrite = TRUE)
    
    area_km2 <- sum(expanse(biome_crop, unit = "km"))
    
    results <- rbind(
      results,
      data.frame(
        country = country_name,
        area = area_km2
      )
    )
    
    cat("  -> área:", round(area_km2, 2), "km2\n")
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
  
  out <- subset(
    out,
    !is.na(area) & !is.na(richness) & !is.na(occurrences) &
      area > 0 & richness > 0 & occurrences > 0
  )
  
  out
}

make_prediction_df <- function(model, predictor_name, data_df, n = 100) {
  newdata <- data.frame(
    seq(
      min(data_df[[predictor_name]], na.rm = TRUE),
      max(data_df[[predictor_name]], na.rm = TRUE),
      length = n
    )
  )
  
  names(newdata) <- predictor_name
  
  pred <- predict(model, newdata = newdata, interval = "confidence")
  cbind(newdata, pred)
}

plot_log_relationship <- function(data_df, pred_df, model, x_var, y_var, color, title, x_lab, y_lab) {
  
  # valores del modelo
  z_value  <- coef(model)[2]
  p_value  <- summary(model)$coefficients[2, 4]
  r2_value <- summary(model)$r.squared
  
  # formato de p tipo paper
  p_text <- if (p_value < 0.001) {
    "italic(p) < 0.001"
  } else {
    paste0("italic(p) == ", signif(p_value, 2))
  }
  
  # etiqueta estadística
  if (p_value < 0.001) {
    label_text <- paste0(
      "italic(z) == ", round(z_value, 2),
      " ~ ',' ~ italic(p) < 0.001",
      " ~ ',' ~ R^2 == ", round(r2_value, 2)
    )
  } else {
    label_text <- paste0(
      "italic(z) == ", round(z_value, 2),
      " ~ ',' ~ italic(p) == ", signif(p_value, 2),
      " ~ ',' ~ R^2 == ", round(r2_value, 2)
    )
  }
  
  # posición de la etiqueta
  x_annot <- min(data_df[[x_var]], na.rm = TRUE) * 1.2
  y_annot <- max(data_df[[y_var]], na.rm = TRUE) * 0.8
  
  ggplot(data_df, aes_string(x = x_var, y = y_var)) +
    geom_point(color = color) +
    geom_ribbon(
      data = pred_df,
      aes_string(x = x_var, ymin = "exp(lwr)", ymax = "exp(upr)"),
      fill = color,
      alpha = 0.2,
      inherit.aes = FALSE
    ) +
    geom_line(
      data = pred_df,
      aes_string(x = x_var, y = "exp(fit)"),
      color = color,
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    scale_x_log10() +
    scale_y_log10() +
    annotate(
      "text",
      x = x_annot,
      y = y_annot,
      label = label_text,
      hjust = 0,
      vjust = 1,
      size = 3.5,
      parse = TRUE
    ) +
    labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    theme_species_area()
}

# =========================
# DRYFLOR
# =========================

dryflor_vect <- vect(dryflor_path)
summary_per_country_dryflor <- read.csv(dryflor_csv)

dryflor_area_df <- compute_country_area(
  america_vect = america,
  biome_vect = dryflor_vect,
  countries = countries_dryflor,
  outdir = dryflor_outdir,
  prefix = "dryflor"
)

dryflor_species_area <- prepare_species_area_data(
  area_df = dryflor_area_df,
  summary_df = summary_per_country_dryflor,
  richness_col = "Species",
  occurrences_col = "total"
)
write.csv(dryflor_species_area, "./Data/summary_diversity/dryflor/dryflor_species_area.csv")

summary_per_country_dryflor <- summary_per_country_dryflor %>%
  mutate(
    richness = as.numeric(Species),
    occurrences = as.numeric(total)
  ) %>%
  filter(
    !is.na(richness), !is.na(occurrences),
    richness > 0, occurrences > 0
  )

# a) Richness vs occurrences
model_rich_occ_dryflor <- lm(log(richness) ~ log(occurrences), data = summary_per_country_dryflor)
pred_rich_occ_dryflor  <- make_prediction_df(model_rich_occ_dryflor, "occurrences", summary_per_country_dryflor)
summary(model_rich_occ_dryflor)
rich_occ_dryflor <- plot_log_relationship(
  data_df = summary_per_country_dryflor,
  pred_df = pred_rich_occ_dryflor,
  model = model_rich_occ_dryflor,
  x_var = "occurrences",
  y_var = "richness",
  color = "darkblue",
  title = "a",
  x_lab = "Occurrences",
  y_lab = "Reported richness"
)

# b) Occurrences vs area
model_occ_area_dryflor <- lm(log(occurrences) ~ log(area), data = dryflor_species_area)
pred_occ_area_dryflor  <- make_prediction_df(model_occ_area_dryflor, "area", dryflor_species_area)
summary(model_occ_area_dryflor)
occ_area_dryflor <- plot_log_relationship(
  data_df = dryflor_species_area,
  pred_df = pred_occ_area_dryflor,
  model = model_occ_area_dryflor,
  x_var = "area",
  y_var = "occurrences",
  color = "darkgreen",
  title = "b",
  x_lab = "Area (km²)",
  y_lab = "Occurrences"
)

# c) Richness vs area
model_rich_area_dryflor <- lm(log(richness) ~ log(area), data = dryflor_species_area)
pred_rich_area_dryflor  <- make_prediction_df(model_rich_area_dryflor, "area", dryflor_species_area)
summary(model_rich_area_dryflor)
rich_area_dryflor <- plot_log_relationship(
  data_df = dryflor_species_area,
  pred_df = pred_rich_area_dryflor,
  model = model_rich_area_dryflor,
  x_var = "area",
  y_var = "richness",
  color = "darkred",
  title = "c",
  x_lab = "Area (km²)",
  y_lab = "Reported richness"
)

# =========================
# TEOW
# =========================

teow_vect <- vect(teow_path)
summary_per_country_teow <- read.csv(teow_csv)

teow_area_df <- compute_country_area(
  america_vect = america,
  biome_vect = teow_vect,
  countries = countries_teow,
  outdir = teow_outdir,
  prefix = "teow"
)

teow_species_area <- prepare_species_area_data(
  area_df = teow_area_df,
  summary_df = summary_per_country_teow,
  richness_col = "species",
  occurrences_col = "total"
)

summary_per_country_teow <- summary_per_country_teow %>%
  mutate(
    richness = as.numeric(species),
    occurrences = as.numeric(total)
  ) %>%
  filter(
    !is.na(richness), !is.na(occurrences),
    richness > 0, occurrences > 0
  )

write.csv(teow_species_area, "./Data/summary_diversity/teow/teow_species_area.csv")

# d) Richness vs occurrences
model_rich_occ_teow <- lm(log(richness) ~ log(occurrences), data = summary_per_country_teow)
pred_rich_occ_teow  <- make_prediction_df(model_rich_occ_teow, "occurrences", summary_per_country_teow)
summary(model_rich_occ_teow)
rich_occ_teow <- plot_log_relationship(
  data_df = summary_per_country_teow,
  pred_df = pred_rich_occ_teow,
  model = model_rich_occ_teow,
  x_var = "occurrences",
  y_var = "richness",
  color = "darkblue",
  title = "d",
  x_lab = "Occurrences",
  y_lab = "Reported richness"
)

# e) Occurrences vs area
model_occ_area_teow <- lm(log(occurrences) ~ log(area), data = teow_species_area)
pred_occ_area_teow  <- make_prediction_df(model_occ_area_teow, "area", teow_species_area)
summary(model_occ_area_teow)
occ_area_teow <- plot_log_relationship(
  data_df = teow_species_area,
  pred_df = pred_occ_area_teow,
  model = model_occ_area_teow,
  x_var = "area",
  y_var = "occurrences",
  color = "darkgreen",
  title = "e",
  x_lab = "Area (km²)",
  y_lab = "Occurrences"
)

# f) Richness vs area
model_rich_area_teow <- lm(log(richness) ~ log(area), data = teow_species_area)
pred_rich_area_teow  <- make_prediction_df(model_rich_area_teow, "area", teow_species_area)
summary(model_rich_area_teow)
rich_area_teow <- plot_log_relationship(
  data_df = teow_species_area,
  pred_df = pred_rich_area_teow,
  model = model_rich_area_teow,
  x_var = "area",
  y_var = "richness",
  color = "darkred",
  title = "f",
  x_lab = "Area (km²)",
  y_lab = "Reported richness"
)

# =========================
# Final panel
# =========================

plots_area <- list(
  rich_occ_dryflor,
  occ_area_dryflor,
  rich_area_dryflor,
  rich_occ_teow,
  occ_area_teow,
  rich_area_teow
)

grid_species_occ_area <- grid.arrange(
  grobs = plots_area,
  ncol = 3,
  nrow = 2
)
plot(grid_species_occ_area)

ggsave(
  "./Figures/species_occ_area.jpg",
  grid_species_occ_area,
  width = 30,
  height = 20,
  units = "cm",
  dpi = 300
)
