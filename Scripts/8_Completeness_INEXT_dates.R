# ============================================================
# Script: Sampling completeness analysis with iNEXT
# Project: Neotropical dry forest bees
# Authors: Herrera-Motta et al.
# Last update: 2026-03-28
# Description:
# This script evaluates sampling completeness for DRYFLOR and
# TEOW using incidence-based rarefaction/extrapolation with
# the iNEXT package. It:
# 1. Estimates completeness per country
# 2. Generates country-level completeness plots
# 3. Estimates completeness for the whole forest region
# 4. Saves summary tables and publication-ready figures
# ============================================================

library(dplyr)
library(iNEXT)
library(ggplot2)
library(gridExtra)
library(grid)

setwd("./NDF_bees_project/")

# ============================================================
# DRYFLOR – Completeness by country
# ============================================================

dryflor_occ <- read.csv("./Data/STDF_bees_occ/dryflor/dryflor_bees_final_occ.csv")
dryflor_occ <- dryflor_occ[dryflor_occ$species != "sp", ]

# Lookup table for country codes and full names
country_lookup <- data.frame(
  countryCode = c(
    "AR","BR","PY","CO","EC","PE","DO","HT","MX","HN",
    "CR","BO","JM","GT","NI","VE","CU","PR","LC","SV"
  ),
  country = c(
    "Argentina","Brazil","Paraguay","Colombia","Ecuador","Peru",
    "D. Republic","Haiti","Mexico","Honduras","Costa Rica",
    "Bolivia","Jamaica","Guatemala","Nicaragua","Venezuela",
    "Cuba","Puerto Rico","Saint Lucia","El Salvador"
  ),
  stringsAsFactors = FALSE
)

countries <- unique(dryflor_occ$countryCode)
countries <- countries[!is.na(countries)]

completeness_df <- data.frame(
  countryCode = character(),
  n_years = numeric(),
  n_species = numeric(),
  completeness = numeric(),
  stringsAsFactors = FALSE
)

failed_countries <- character()
plots <- list()

for (ctry in countries) {
  tryCatch({
    tmp <- dryflor_occ %>%
      filter(countryCode == ctry, !is.na(year), !is.na(species))
    
    if (nrow(tmp) == 0) stop("No data available")
    
    tmp2 <- tmp %>%
      distinct(species, year)
    
    T_ctry <- tmp2 %>%
      distinct(year) %>%
      nrow()
    
    S_ctry <- tmp2 %>%
      distinct(species) %>%
      nrow()
    
    if (T_ctry < 2) stop("Fewer than 2 sampling years")
    if (S_ctry < 2) stop("Fewer than 2 species")
    
    freqs <- tmp2 %>%
      count(species, name = "freq") %>%
      pull(freq)
    
    incidence_freq <- c(T_ctry, freqs)
    
    out <- iNEXT(incidence_freq, q = 0, datatype = "incidence_freq")
    estimates <- out$AsyEst
    
    if (nrow(estimates) == 0) stop("Empty AsyEst output")
    
    completeness_df <- rbind(
      completeness_df,
      data.frame(
        countryCode = ctry,
        n_years = T_ctry,
        n_species = S_ctry,
        completeness = estimates[1, ]
      )
    )
    
    country_name <- country_lookup$country[country_lookup$countryCode == ctry]
    if (length(country_name) == 0) country_name <- ctry
    
    p <- ggiNEXT(out) +
      labs(
        title = country_name,
        x = "Sampling (years)",
        y = "Richness"
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        axis.title = element_blank(),
        legend.position = "none"
      )
    
    plots[[ctry]] <- p
    
  }, error = function(e) {
    failed_countries <<- c(failed_countries, ctry)
  })
}

dryflor_completeness <- completeness_df %>%
  mutate(prop_comp = (n_species / completeness.Estimator) * 100)

write.csv(
  dryflor_completeness,
  "./Data/summary_diversity/dryflor/dryflor_completeness.csv",
  row.names = FALSE
)

plot_names <- country_lookup$country[match(names(plots), country_lookup$countryCode)]
orden <- order(plot_names)
plots <- plots[orden]

dryflor_inext <- grid.arrange(
  grobs = plots,
  ncol = 4,
  nrow = 4,
  top = textGrob(
    "a",
    x = 0,
    hjust = 0,
    gp = gpar(fontsize = 16, fontface = "bold")
  ),
  bottom = textGrob(
    "Sampling units",
    gp = gpar(fontsize = 14, fontface = "bold")
  ),
  left = textGrob(
    "Richness",
    rot = 90,
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

ggsave(
  "./Figures/dryflor_countries.jpg",
  dryflor_inext,
  width = 30,
  height = 30,
  units = "cm",
  dpi = 300
)

# ============================================================
# TEOW – Completeness by country
# ============================================================

teow_occ <- read.csv("./Data/STDF_bees_occ/teow/teow_bees_final_occ.csv")
teow_occ <- teow_occ[teow_occ$species != "sp", ]

country_lookup <- data.frame(
  countryCode = sort(unique(teow_occ$countryCode)),
  country = c(
    "Bolivia", "Brazil", "Colombia", "Costa Rica", "Cuba",
    "Dominican Republic", "Ecuador", "Grenada", "Guatemala", "Honduras",
    "Haiti", "Jamaica", "Saint Lucia", "Montserrat", "Mexico",
    "Nicaragua", "Panama", "Peru", "Puerto Rico", "El Salvador",
    "Trinidad and Tobago", "Venezuela"
  ),
  stringsAsFactors = FALSE
)

countries <- unique(teow_occ$countryCode)
countries <- countries[!is.na(countries)]

completeness_df <- data.frame(
  countryCode = character(),
  n_years = numeric(),
  n_species = numeric(),
  completeness = numeric(),
  stringsAsFactors = FALSE
)

failed_countries <- character()
plots <- list()

for (ctry in countries) {
  tryCatch({
    tmp <- teow_occ %>%
      filter(countryCode == ctry, !is.na(year), !is.na(species))
    
    if (nrow(tmp) == 0) stop("No data available")
    
    tmp2 <- tmp %>%
      distinct(species, year)
    
    T_ctry <- tmp2 %>%
      distinct(year) %>%
      nrow()
    
    S_ctry <- tmp2 %>%
      distinct(species) %>%
      nrow()
    
    if (T_ctry < 2) stop("Fewer than 2 sampling years")
    if (S_ctry < 2) stop("Fewer than 2 species")
    
    freqs <- tmp2 %>%
      count(species, name = "freq") %>%
      pull(freq)
    
    incidence_freq <- c(T_ctry, freqs)
    
    out <- iNEXT(incidence_freq, q = 0, datatype = "incidence_freq")
    estimates <- out$AsyEst
    
    if (nrow(estimates) == 0) stop("Empty AsyEst output")
    
    completeness_df <- rbind(
      completeness_df,
      data.frame(
        countryCode = ctry,
        n_years = T_ctry,
        n_species = S_ctry,
        completeness = estimates[1, ]
      )
    )
    
    country_name <- country_lookup$country[country_lookup$countryCode == ctry]
    if (length(country_name) == 0) country_name <- ctry
    
    p <- ggiNEXT(out) +
      labs(
        title = country_name,
        x = "Sampling (years)",
        y = "Richness"
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 16),
        axis.text = element_text(size = 13),
        axis.title = element_blank(),
        legend.position = "none"
      )
    
    plots[[ctry]] <- p
    
  }, error = function(e) {
    failed_countries <<- c(failed_countries, ctry)
  })
}

teow_completeness <- completeness_df %>%
  mutate(prop_comp = (n_species / completeness.Estimator) * 100)

write.csv(
  teow_completeness,
  "./Data/summary_diversity/teow/teow_completeness.csv",
  row.names = FALSE
)

plot_names <- country_lookup$country[match(names(plots), country_lookup$countryCode)]
orden <- order(plot_names)
plots <- plots[orden]

teow_inext <- grid.arrange(
  grobs = plots,
  ncol = 4,
  nrow = 5,
  bottom = textGrob(
    "Sampling units",
    gp = gpar(fontsize = 14, fontface = "bold")
  ),
  left = textGrob(
    "Richness",
    rot = 90,
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

ggsave(
  "./Figures/teow_countries.jpg",
  teow_inext,
  width = 30,
  height = 30,
  units = "cm",
  dpi = 300
)

# ============================================================
# Whole-forest completeness – DRYFLOR
# ============================================================

dryflor_occ <- read.csv("./Data/STDF_bees_occ/dryflor/dryflor_bees_final_occ.csv")

dryflor_occ <- dryflor_occ %>%
  filter(species != "sp", !is.na(year), !is.na(species))

dryflor_pa <- dryflor_occ %>%
  distinct(species, year)

T_dryflor <- dryflor_pa %>%
  distinct(year) %>%
  nrow()

S_dryflor <- dryflor_pa %>%
  distinct(species) %>%
  nrow()

freqs <- dryflor_pa %>%
  count(species, name = "freq") %>%
  pull(freq)

incidence_freq <- c(T_dryflor, freqs)

out <- iNEXT(incidence_freq, q = 0, datatype = "incidence_freq")

dryflor_completeness <- out$AsyEst %>%
  mutate(
    n_years = T_dryflor,
    n_species = S_dryflor,
    prop_comp = (n_species / Estimator) * 100
  )

write.csv(
  dryflor_completeness,
  "./Data/summary_diversity/dryflor/dryflor_completeness_whole_forest.csv",
  row.names = FALSE
)

dryflor_inext <- ggiNEXT(out) +
  labs(
    title = "a",
    x = "Sampling units (years)",
    y = "Richness"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    axis.title = element_blank(),
    legend.position = "none"
  )

# ============================================================
# Whole-forest completeness – TEOW
# ============================================================

teow_occ <- read.csv("./Data/STDF_bees_occ/teow/teow_bees_final_occ.csv")

teow_occ <- teow_occ %>%
  filter(species != "sp", !is.na(year), !is.na(species))

teow_pa <- teow_occ %>%
  distinct(species, year)

T_teow <- teow_pa %>%
  distinct(year) %>%
  nrow()

S_teow <- teow_pa %>%
  distinct(species) %>%
  nrow()

freqs <- teow_pa %>%
  count(species, name = "freq") %>%
  pull(freq)

incidence_freq <- c(T_teow, freqs)

out <- iNEXT(incidence_freq, q = 0, datatype = "incidence_freq")

teow_completeness <- out$AsyEst %>%
  mutate(
    n_years = T_teow,
    n_species = S_teow,
    prop_comp = (n_species / Estimator) * 100
  )

write.csv(
  teow_completeness,
  "./Data/summary_diversity/teow/teow_completeness_whole_forest.csv",
  row.names = FALSE
)

teow_inext <- ggiNEXT(out) +
  labs(
    title = "b",
    x = "Sampling units (years)",
    y = "Richness"
  ) +
  ylim(0, 2000) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    axis.title = element_blank(),
    legend.position = "none"
  )

plots_all <- list(dryflor_inext, teow_inext)

whole_forest_panel <- grid.arrange(
  grobs = plots_all,
  ncol = 2,
  nrow = 1,
  bottom = textGrob(
    "Sampling units",
    gp = gpar(fontsize = 14, fontface = "bold")
  ),
  left = textGrob(
    "Richness",
    rot = 90,
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

ggsave(
  "./Figures/dryflor_inext_whole_forest.tif",
  whole_forest_panel,
  width = 17,
  height = 9,
  units = "cm",
  dpi = 300
)
