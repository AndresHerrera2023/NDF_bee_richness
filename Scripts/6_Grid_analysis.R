# ============================================================
# Script: Grid richness distribution and sampling density analysis
# Project: Neotropical dry forest bees
# Author: [Your Name]
# Description:
# This script analyzes species and genus richness distributions
# across spatial grids (50, 75, and 100 km) for DRYFLOR and TEOW.
# It calculates proportions of empty grids, richness classes,
# and generates summary tables. Additionally, it evaluates
# sampling density (number of occurrence records per grid cell).
# ============================================================

# Required libraries:
library(terra)

# Set working directory
setwd("-/NDF_bees_project/")

# ============================================================
# DRYFLOR – SPECIES
# ============================================================

# 50 km grids
spdf50 <- vect("./Shapefiles/PAM/dryflor/species/50/dryflor_pam_species_50.shp")

spdf50_all <- length(spdf50$Richness)
spdf50_zero <- length(spdf50[spdf50$Richness == 0])
spdf50_pct_zero <- (spdf50_zero / spdf50_all) * 100

spdf50_no_zero <- spdf50[spdf50$Richness != 0,]
spdf50_no_zero <- hist(spdf50_no_zero$Richness, breaks = 10)
spdf50_no_zero <- spdf50_no_zero[[2]]

spdf50_no_zero_pct <- (spdf50_no_zero[1] / sum(spdf50_no_zero)) * 100
spdf50_20_100_pct  <- (sum(spdf50_no_zero[2:5]) / sum(spdf50_no_zero)) * 100
spdf50_gt100_pct   <- (sum(spdf50_no_zero[6:13]) / sum(spdf50_no_zero)) * 100

# 75 km grids
spdf75 <- vect("./Shapefiles/PAM/dryflor/species/75/dryflor_pam_species_75.shp")

spdf75_all <- length(spdf75$Richness)
spdf75_zero <- length(spdf75[spdf75$Richness == 0])
spdf75_pct_zero <- (spdf75_zero / spdf75_all) * 100

spdf75_no_zero <- spdf75[spdf75$Richness != 0,]
spdf75_no_zero <- hist(spdf75_no_zero$Richness, breaks = 12)
spdf75_no_zero <- spdf75_no_zero[[2]]

spdf75_no_zero_pct <- (spdf75_no_zero[1] / sum(spdf75_no_zero)) * 100
spdf75_20_100_pct  <- (sum(spdf75_no_zero[2:5]) / sum(spdf75_no_zero)) * 100
spdf75_gt100_pct   <- (sum(spdf75_no_zero[6:16]) / sum(spdf75_no_zero)) * 100

# 100 km grids
spdf100 <- vect("./Shapefiles/PAM/dryflor/species/100/dryflor_pam_species_100.shp")

spdf100_all <- length(spdf100$Richness)
spdf100_zero <- length(spdf100[spdf100$Richness == 0])
spdf100_pct_zero <- (spdf100_zero / spdf100_all) * 100

spdf100_no_zero <- spdf100[spdf100$Richness != 0,]
spdf100_no_zero <- hist(spdf100_no_zero$Richness, breaks = 12)
spdf100_no_zero <- spdf100_no_zero[[2]]

spdf100_no_zero_pct <- (spdf100_no_zero[1] / sum(spdf100_no_zero)) * 100
spdf100_20_100_pct  <- (sum(spdf100_no_zero[2:5]) / sum(spdf100_no_zero)) * 100
spdf100_gt100_pct   <- (sum(spdf100_no_zero[6:17]) / sum(spdf100_no_zero)) * 100

# ============================================================
# DRYFLOR – GENERA
# ============================================================

gndf50 <- vect("./Shapefiles/PAM/dryflor/genera/50/dryflor_pam_genera_50.shp")

gndf50_all <- length(gndf50$Richness)
gndf50_zero <- length(gndf50[gndf50$Richness == 0])
gndf50_zero_pct <- (gndf50_zero / gndf50_all) * 100

gndf50_nonzero <- gndf50[gndf50$Richness != 0,]
gndf50_counts <- hist(gndf50_nonzero$Richness, breaks = 10)[[2]]

gndf50_1_10_pct <- (gndf50_counts[1] / sum(gndf50_counts)) * 100
gndf50_10_80_pct <- (sum(gndf50_counts[2:8]) / sum(gndf50_counts)) * 100
gndf50_gt80_pct <- (sum(gndf50_counts[9:10]) / sum(gndf50_counts)) * 100

# ============================================================
# TEOW – SPECIES
# ============================================================

sptw50 <- vect("./Shapefiles/PAM/teow/species/50/teow_pam_species_50.shp")

sptw50_all <- length(sptw50$Richness)
sptw50_zero <- length(sptw50[sptw50$Richness == 0])
sptw50_zero_pct <- (sptw50_zero / sptw50_all) * 100

sptw50_nonzero <- sptw50[sptw50$Richness != 0,]
sptw50_counts <- hist(sptw50_nonzero$Richness, breaks = 10)[[2]]

sptw50_1_20_pct <- (sptw50_counts[1] / sum(sptw50_counts)) * 100
sptw50_20_100_pct <- (sum(sptw50_counts[2:5]) / sum(sptw50_counts)) * 100
sptw50_gt100_pct <- (sum(sptw50_counts[6:12]) / sum(sptw50_counts)) * 100

# ============================================================
# SUMMARY TABLES
# ============================================================

dryflor_species_table <- data.frame(
  PDs = "DRYFLOR",
  Grid_km = c(50, 75, 100),
  Number_of_grids = c(spdf50_all, spdf75_all, spdf100_all),
  Empty_grids = c(spdf50_pct_zero, spdf75_pct_zero, spdf100_pct_zero),
  `1_20_species` = c(spdf50_no_zero_pct, spdf75_no_zero_pct, spdf100_no_zero_pct),
  `21_100_species` = c(spdf50_20_100_pct, spdf75_20_100_pct, spdf100_20_100_pct),
  `gt_100_species` = c(spdf50_gt100_pct, spdf75_gt100_pct, spdf100_gt100_pct)
)

teow_species_table <- data.frame(
  PDs = "TEOW",
  Grid_km = c(50, 75, 100),
  Number_of_grids = c(sptw50_all, sptw75_all, sptw100_all),
  Empty_grids = c(sptw50_zero_pct, sptw75_zero_pct, sptw100_zero_pct),
  `1_20_species` = c(sptw50_1_20_pct, sptw75_1_20_pct, sptw100_1_20_pct),
  `21_100_species` = c(sptw50_20_100_pct, sptw75_20_100_pct, sptw100_20_100_pct),
  `gt_100_species` = c(sptw50_gt100_pct, sptw75_gt100_pct, sptw100_gt100_pct)
)

# ============================================================
# DENSITY ANALYSIS (example: DRYFLOR 50 km)
# ============================================================

dryflor_pam_50 <- vect("./Shapefiles/PAM/dryflor/genera/50/dryflor_pam_genera_50.shp")

occ_dryflor <- read.csv("./Data/STDF_bees_occ/dryflor/dryflor_bees_final_occ.csv")
occ_dryflor <- vect(occ_dryflor, geom = c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84")
occ_dryflor <- project(occ_dryflor, crs(dryflor_pam_50))

dryflor_pam_50$cell_id <- 1:nrow(dryflor_pam_50)

counts <- terra::extract(dryflor_pam_50["cell_id"], occ_dryflor)
tab <- table(counts$cell_id)

dryflor_pam_50$n_records <- 0
ix <- match(as.numeric(names(tab)), dryflor_pam_50$cell_id)
dryflor_pam_50$n_records[ix] <- as.numeric(tab)

dryflor_50_mean <- mean(dryflor_pam_50$n_records[dryflor_pam_50$n_records > 0])
dryflor_50_median <- median(dryflor_pam_50$n_records[dryflor_pam_50$n_records > 0])

writeVector(
  dryflor_pam_50,
  "./Shapefiles/PAM/dryflor/records/dryflor_nrecords_50.shp",
  overwrite = TRUE
)


