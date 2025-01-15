#calculating richness in cells of different size throughout the STDF Pd's

# Installing and loading required packages
if (!require(remotes)) {
  install.packages("remotes")
}

# Installing the biosurvey package from GitHub
remotes::install_github("claununez/biosurvey", force = TRUE)

# Optional: Installing the package with vignettes
remotes::install_github("claununez/biosurvey", build_vignettes = TRUE)

# Loading libraries
library(biosurvey)  # Main package for biodiversity survey analysis
library(terra)      # Spatial data handling
library(dplyr)

# Setting the working directory
setwd("./NDF_bees_project/")

#Dryflor PD

# Reading and visualizing the region of interest
region <- vect("./Shapefiles/STDF_PD/dryflor/seasonally_dryfo_dis.shp")
plot(region)

# Loading and preparing species occurrence data
occ_species <- read.csv("./Data/STDF_bees_occ/dryflor/species/species_occ_dryflor.csv")
head(occ_species)  # Preview the dataset

# Selecting and renaming relevant columns
occ_species <- occ_species %>% select(longitude, latitude, Species, source)
colnames(occ_species)[c(1, 2, 3)] <- c("Longitude", "Latitude", "Species")

# Calculating indices for species with a cell cell size of 50
b_pam_species_50 <- prepare_base_PAM(data = occ_species[, c(1, 2, 3)], region = region, 
                                     cell_size = 50, indices = "all") 

summary(b_pam_species_50)  # Summary of results

# Extracting and saving richness data
pamin <- b_pam_species_50$PAM
pamin$Richness <- b_pam_species_50$PAM_indices$Richness
pam_rich <- pamin[, c("ID", "Longitude", "Latitude", "Richness")]
writeVector(pam_rich, "./Shapefiles/PAM/dryflor/species/dryflor_PAM_species_50") #Create the folders /PAM/dryflor/species/ in your shapefiles  folder

# Repeating the process for cell sizes of 75 and 100
# cell size = 75
b_pam_species_75 <- prepare_base_PAM(data = occ_species[, c(1, 2, 3)], region = region, 
                                     cell_size = 75, indices = "all") 
summary(b_pam_species_75)
pamin <- b_pam_species_75$PAM
pamin$Richness <- b_pam_species_75$PAM_indices$Richness
pam_rich <- pamin[, c("ID", "Longitude", "Latitude", "Richness")]
writeVector(pam_rich, "./Shapefiles/PAM/dryflor/species/dryflor_PAM_species_75")

# cell size = 100
b_pam_species_100 <- prepare_base_PAM(data = occ_species[, c(1, 2, 3)], region = region, 
                                      cell_size = 100, indices = "all") 
summary(b_pam_species_100)
pamin <- b_pam_species_100$PAM
pamin$Richness <- b_pam_species_100$PAM_indices$Richness
pam_rich <- pamin[, c("ID", "Longitude", "Latitude", "Richness")]
writeVector(pam_rich, "./Shapefiles/PAM/dryflor/species/dryflor_PAM_species_100")

# Loading and preparing genus occurrence data
occ_genus <- read.csv("./Data/STDF_bees_occ/dryflor/genera/genera_occ_dryflor.csv")
head(occ_genus)  # Preview the dataset

# Selecting and renaming relevant columns
occ_genus <- occ_genus %>% select(longitude, latitude, genus)
colnames(occ_genus)[c(1, 2, 3)] <- c("Longitude", "Latitude", "Species")

# Repeating the index calculation for genera data:

# cell size = 50
b_pam_genus_50 <- prepare_base_PAM(data = occ_genus[, c(1, 2, 3)], region = region, 
                                   cell_size = 50, indices = "all") 
summary(b_pam_genus_50)
pamin <- b_pam_genus_50$PAM
pamin$Richness <- b_pam_genus_50$PAM_indices$Richness
pam_rich <- pamin[, c("ID", "Longitude", "Latitude", "Richness")]
writeVector(pamin, "./Shapefiles/PAM/dryflor/genera/dryflor_PAM_genera_50/") #Create the folders /PAM/dryflor/genera/ in your shapefiles  folder

# cell size = 75
b_pam_genus_75 <- prepare_base_PAM(data = occ_genus[, c(1, 2, 3)], region = region, 
                                   cell_size = 75, indices = "all") 
summary(b_pam_genus_75)
pamin <- b_pam_genus_75$PAM
pamin$Richness <- b_pam_genus_75$PAM_indices$Richness
pam_rich <- pamin[, c("ID", "Longitude", "Latitude", "Richness")]
writeVector(pam_rich, "./Shapefiles/PAM/dryflor/genera/dryflor_PAM_genera_75/")

# cell size = 100
b_pam_genus_100 <- prepare_base_PAM(data = occ_genus[, c(1, 2, 3)], region = region, 
                                    cell_size = 100, indices = "all") 
summary(b_pam_genus_100)
pamin <- b_pam_genus_100$PAM
pamin$Richness <- b_pam_genus_100$PAM_indices$Richness
pamin$Richness_n <- b_pam_genus_100$PAM_indices$Richness_normalized
writeVector(pamin, "./Shapefiles/PAM/dryflor/genera/dryflor_PAM_genera_100", overwrite = TRUE)

#TEOW PD

# Setting working directory and loading WWF-specific region
region <- vect("./Shapefiles/STDF_PD/teow/wwf_TDF_neotropics_dis.shp")
plot(region)

# Loading and preparing WWF species occurrence data
occ_species <- read.csv("./Data/STDF_bees_occ/teow/species/species_occ_teow.csv")
head(occ_species)
occ_species <- occ_species %>% select(longitude, latitude, Species, source)
colnames(occ_species)[c(1, 2, 3)] <- c("Longitude", "Latitude", "Species")

# Calculating indices for WWF species data
# cell size = 50
b_pam_species_50 <- prepare_base_PAM(data = occ_species[, c(1, 2, 3)], region = region, 
                                     cell_size = 50, indices = "all") 
summary(b_pam_species_50)
pamin <- b_pam_species_50$PAM
pamin$Richness <- b_pam_species_50$PAM_indices$Richness
pam_rich <- pamin[, c("ID", "Longitude", "Latitude", "Richness")]
writeVector(pam_rich, "./Shapefiles/PAM/teow/species/teow_PAM_species_50") #Create the folders /PAM/teow/species/ in your shapefiles  folder

# cell size = 75
b_pam_species_75 <- prepare_base_PAM(data = occ_species[, c(1, 2, 3)], region = region, 
                                     cell_size = 75, indices = "all") 
summary(b_pam_species_75)
pamin <- b_pam_species_75$PAM
pamin$Richness <- b_pam_species_75$PAM_indices$Richness
pam_rich <- pamin[, c("ID", "Longitude", "Latitude", "Richness")]
writeVector(pam_rich, "./Shapefiles/PAM/teow/species/teow_PAM_species_75")

# cell size = 100
b_pam_species_100 <- prepare_base_PAM(data = occ_species[, c(1, 2, 3)], region = region, 
                                      cell_size = 100, indices = "all") 
summary(b_pam_species_100)
pamin <- b_pam_species_100$PAM
pamin$Richness <- b_pam_species_100$PAM_indices$Richness
pam_rich <- pamin[, c("ID", "Longitude", "Latitude", "Richness")]
writeVector(pam_rich, "./Shapefiles/PAM/teow/species/teow_PAM_species_100")

# Repeating for TEOW genus data
# Loading genus data
occ_genus <- read.csv("./Data/STDF_bees_occ/teow/genera/genera_occ_teow.csv")
head(occ_genus)
occ_genus <- occ_genus %>% select(longitude, latitude, genus)
colnames(occ_genus)[c(1, 2, 3)] <- c("Longitude", "Latitude", "Species")

# cell size = 50
b_pam_genus_50 <- prepare_base_PAM(data = occ_genus[, c(1, 2, 3)], region = region, 
                                   cell_size = 50, indices = "all") 
summary(b_pam_genus_50)
pamin <- b_pam_genus_50$PAM
pamin$Richness <- b_pam_genus_50$PAM_indices$Richness
pam_rich <- pamin[, c("ID", "Longitude", "Latitude", "Richness")]
writeVector(pamin, "./Shapefiles/PAM/teow/genera/teow_PAM_genera_50") #Create the folders /PAM/teow/genera/ in your shapefiles  folder

# cell size = 75
b_pam_genus_75 <- prepare_base_PAM(data = occ_genus[, c(1, 2, 3)], region = region, 
                                   cell_size = 75, indices = "all") 
summary(b_pam_genus_75)
pamin <- b_pam_genus_75$PAM
pamin$Richness <- b_pam_genus_75$PAM_indices$Richness
pam_rich <- pamin[, c("ID", "Longitude", "Latitude", "Richness")]
writeVector(pamin, "./Shapefiles/PAM/teow/genera/teow_PAM_genera_75")

# cell size = 100
b_pam_genus_100 <- prepare_base_PAM(data = occ_genus[, c(1, 2, 3)], region = region, 
                                    cell_size = 100, indices = "all") 
summary(b_pam_genus_100)
pamin <- b_pam_genus_100$PAM
pamin$Richness <- b_pam_genus_100$PAM_indices$Richness
pamin$Richness_n <- b_pam_genus_100$PAM_indices$Richness_normalized
writeVector(pamin, "./Shapefiles/PAM/teow/genera/teow_PAM_genera_100", overwrite = TRUE)
