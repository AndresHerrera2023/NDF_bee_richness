# ============================================================
# Script: Species and genus richness (PAM) analysis
# Project: Neotropical dry forest bees
# Author: [Your Name]
# Description:
# This script generates presence–absence matrices (PAMs) and
# calculates species and genus richness across multiple spatial
# resolutions (50, 75, and 100 km grids) for both DRYFLOR and
# TEOW regions using the biosurvey package.
# ============================================================

# Required libraries:
if(!require(remotes)){
  install.packages("remotes")
}

# Install biosurvey package (if needed)
remotes::install_github("claununez/biosurvey", force = TRUE)
remotes::install_github("claununez/biosurvey", build_vignettes = TRUE)

# Libraries
library(biosurvey)
library(terra)
library(dplyr)

# Set working directory
setwd("./NDF_bees_project/2026_version/")

# ============================================================
# DRYFLOR
# ============================================================

# Region of interest
region <- vect("./Shapefiles/STDF/dryflor/seasonally_dryfo_dis.shp")
plot(region)

# Species occurrences
occ_species <- read.csv("./Data/STDF_bees_occ/dryflor/dryflor_bees_final_occ.csv")
occ_species <- occ_species[occ_species$species != "sp",]
occ_species <- occ_species %>% select(longitude, latitude, species)
colnames(occ_species) <- c("Longitude","Latitude","Species")

# ---- Species richness ----

# 50 km grid
b_pam_species_50 <- prepare_base_PAM(
  data = occ_species,
  region = region,
  cell_size = 50,
  indices = "all"
)
pamin <- b_pam_species_50$PAM
pamin$Richness <- b_pam_species_50$PAM_indices$Richness
writeVector(pamin[,c("Longitude","Latitude","Richness")],
            "./Shapefiles/PAM/dryflor/species/50/dryflor_pam_species_50.shp",
            overwrite = TRUE)

# 75 km grid
b_pam_species_75 <- prepare_base_PAM(occ_species, region, 75, indices = "all")
pamin <- b_pam_species_75$PAM
pamin$Richness <- b_pam_species_75$PAM_indices$Richness
writeVector(pamin[,c("Longitude","Latitude","Richness")],
            "./Shapefiles/PAM/dryflor/species/75/dryflor_pam_species_75.shp",
            overwrite = TRUE)

# 100 km grid
b_pam_species_100 <- prepare_base_PAM(occ_species, region, 100, indices = "all")
pamin <- b_pam_species_100$PAM
pamin$Richness <- b_pam_species_100$PAM_indices$Richness
writeVector(pamin[,c("ID","Longitude","Latitude","Richness")],
            "./Shapefiles/PAM/dryflor/species/100/dryflor_pam_species_100.shp",
            overwrite = TRUE)

# ---- Genus richness ----

occ_genus <- read.csv("./Data/STDF_bees_occ/dryflor/dryflor_bees_final_occ.csv") %>%
  select(longitude, latitude, genus)
colnames(occ_genus) <- c("Longitude","Latitude","Species")

# 50 km
b_pam_genus_50 <- prepare_base_PAM(occ_genus, region, 50, indices = "all")
pamin <- b_pam_genus_50$PAM
pamin$Richness <- b_pam_genus_50$PAM_indices$Richness
writeVector(pamin, "./Shapefiles/PAM/dryflor/genera/50/dryflor_pam_genera_50.shp", overwrite = TRUE)

# 75 km
b_pam_genus_75 <- prepare_base_PAM(occ_genus, region, 75, indices = "all")
pamin <- b_pam_genus_75$PAM
pamin$Richness <- b_pam_genus_75$PAM_indices$Richness
writeVector(pamin, "./Shapefiles/PAM/dryflor/genera/75/dryflor_pam_genera_75.shp", overwrite = TRUE)

# 100 km
b_pam_genus_100 <- prepare_base_PAM(occ_genus, region, 100, indices = "all")
pamin <- b_pam_genus_100$PAM
pamin$Richness <- b_pam_genus_100$PAM_indices$Richness
writeVector(pamin, "./Shapefiles/PAM/dryflor/genera/100/dryflor_pam_genera_100.shp", overwrite = TRUE)

# ============================================================
# TEOW
# ============================================================

region <- vect("./Shapefiles/STDF/teow/teow.shp")
plot(region)

# Species
occ_species <- read.csv("./Data/STDF_bees_occ/teow/teow_bees_final_occ.csv")
occ_species <- occ_species[occ_species$species != "sp",]
occ_species <- occ_species %>% select(longitude, latitude, species)
colnames(occ_species) <- c("Longitude","Latitude","Species")

# 50 km
b_pam_species_50 <- prepare_base_PAM(occ_species, region, 50, indices = "all")
pamin <- b_pam_species_50$PAM
pamin$Richness <- b_pam_species_50$PAM_indices$Richness
writeVector(pamin, "./Shapefiles/PAM/teow/species/50/teow_pam_species_50.shp", overwrite = TRUE)

# 75 km
b_pam_species_75 <- prepare_base_PAM(occ_species, region, 75, indices = "all")
pamin <- b_pam_species_75$PAM
pamin$Richness <- b_pam_species_75$PAM_indices$Richness
writeVector(pamin, "./Shapefiles/PAM/teow/species/75/teow_pam_species_75.shp", overwrite = TRUE)

# 100 km
b_pam_species_100 <- prepare_base_PAM(occ_species, region, 100, indices = "all")
pamin <- b_pam_species_100$PAM
pamin$Richness <- b_pam_species_100$PAM_indices$Richness
writeVector(pamin, "./Shapefiles/PAM/teow/species/100/teow_pam_species_100.shp", overwrite = TRUE)

# ---- Genus ----

occ_genus <- read.csv("./Data/STDF_bees_occ/teow/teow_bees_final_occ.csv") %>%
  select(longitude, latitude, genus)
colnames(occ_genus) <- c("Longitude","Latitude","Species")

# 50 km
b_pam_genus_50 <- prepare_base_PAM(occ_genus, region, 50, indices = "all")
writeVector(b_pam_genus_50$PAM, "./Shapefiles/PAM/teow/genera/50/teow_pam_genera_50.shp", overwrite = TRUE)

# 75 km
b_pam_genus_75 <- prepare_base_PAM(occ_genus, region, 75, indices = "all")
writeVector(b_pam_genus_75$PAM, "./Shapefiles/PAM/teow/genera/75/teow_pam_genera_75.shp", overwrite = TRUE)

# 100 km
b_pam_genus_100 <- prepare_base_PAM(occ_genus, region, 100, indices = "all")
pamin <- b_pam_genus_100$PAM
pamin$Richness <- b_pam_genus_100$PAM_indices$Richness
pamin$Richness_n <- b_pam_genus_100$PAM_indices$Richness_normalized
writeVector(pamin, "./Shapefiles/PAM/teow/genera/100/teow_pam_genera_100.shp", overwrite = TRUE)
