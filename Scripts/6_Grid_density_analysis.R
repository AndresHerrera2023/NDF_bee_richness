### Analysis of the proportion of grid density in cells of different sizes:

#Required libraries:
library(terra)

# Set your working directory
setwd("./NDF_bees_project/")

# Grids 50 km
spdf50 <- vect("./Shapefiles/PAM/dryflor/species/dryflor_PAM_species_50/dryflor_PAM_species_50.shp")

# Grids with zero occ
spdf50_all <- length(spdf50$Richness) #1576 grids
spdf50_zero <- length(spdf50[spdf50$Richness == 0]) #876
spdf50_pct_zero <- (spdf50_zero / spdf50_all) * 100 #55.58%

# Grids with 1-20 species
spdf50_no_zero <- spdf50[spdf50$Richness != 0,]
spdf50_no_zero <- hist(spdf50_no_zero$Richness, breaks = 10)
spdf50_no_zero <- spdf50_no_zero[[2]]
spdf50_no_zero_pct <- (spdf50_no_zero[1] / sum(spdf50_no_zero)) * 100
spdf50_no_zero_pct  #83.71%

# Grids with 20-100 species
spdf50_20_100_pct  <- (sum(spdf50_no_zero[2:5]) / sum(spdf50_no_zero)) * 100
spdf50_20_100_pct   #14.57%

# Grids with >100 species
spdf50_gt100_pct  <- (sum(spdf50_no_zero[6:12]) / sum(spdf50_no_zero)) * 100
spdf50_gt100_pct  # 1.71%

# Proportion check
sum(spdf50_no_zero_pct, spdf50_20_100_pct, spdf50_gt100_pct) # = 100%


# Grids 75 km
spdf75 <- vect("./Shapefiles/PAM/dryflor/species/dryflor_PAM_species_75/dryflor_PAM_species_75.shp")

# Grids with zero occ
spdf75_all <- length(spdf75$Richness) #818 grids
spdf75_zero <- length(spdf75[spdf75$Richness == 0]) #364
spdf75_pct_zero <- (spdf75_zero / spdf75_all) * 100 #44.49 are zero.

# Grids with 1-20 species
spdf75_no_zero <- spdf75[spdf75$Richness != 0,]
spdf75_no_zero <- hist(spdf75_no_zero$Richness, breaks = 10)
spdf75_no_zero <- spdf75_no_zero[[2]]
spdf75_no_zero_pct <- (spdf75_no_zero[1] / sum(spdf75_no_zero)) * 100
spdf75_no_zero_pct  #78.63%

# Grids with 20-100 species
spdf75_20_100_pct <- (sum(spdf75_no_zero[2:5]) / sum(spdf75_no_zero)) * 100
spdf75_20_100_pct  #18.06%

# Grids with >100 species
spdf75_gt100_pct <- (sum(spdf75_no_zero[6:14]) / sum(spdf75_no_zero)) * 100
spdf75_gt100_pct # 3.30%

# Proportion check
sum(spdf75_no_zero_pct, spdf75_20_100_pct, spdf75_gt100_pct) # = 100%

# Grids 100 km
spdf100 <- vect("./Shapefiles/PAM/dryflor/species/dryflor_PAM_species_100/dryflor_PAM_species_100.shp")

# Grids with zero occ
spdf100_all <- length(spdf100$Richness) #531 grids
spdf100_zero <- length(spdf100[spdf100$Richness == 0]) #198
spdf100_pct_zero <- (spdf100_zero / spdf100_all) * 100 #37.28 are zero.

# Grids with 1-20 species
spdf100_no_zero <- spdf100[spdf100$Richness != 0,]
spdf100_no_zero <- hist(spdf100_no_zero$Richness, breaks = 12)
spdf100_no_zero <- spdf100_no_zero[[2]]
spdf100_no_zero_pct <- (spdf100_no_zero[1] / sum(spdf100_no_zero)) * 100
spdf100_no_zero_pct  #72.37%

# Grids with 20-100 species
spdf100_20_100_pct <- (sum(spdf100_no_zero[2:5]) / sum(spdf100_no_zero)) * 100
spdf100_20_100_pct  #23.42%

# Grids with >100 species
spdf100_gt100_pct <- (sum(spdf100_no_zero[6:16]) / sum(spdf100_no_zero)) * 100
spdf100_gt100_pct # 4.20%

# Proportion check
sum(spdf100_no_zero_pct, spdf100_20_100_pct, spdf100_gt100_pct) # 100%


## Dryflor 

#Genera:

# Grids 50 km
gndf50 <- vect("./Shapefiles/PAM/dryflor/genera/dryflor_PAM_genera_50/dryflor_PAM_genera_50.shp")

# Grids with zero occurrences
gndf50_all <- length(gndf50$Richness) # 1576 grids
gndf50_zero <- length(gndf50[gndf50$Richness == 0]) # 850
gndf50_zero_pct <- (gndf50_zero / gndf50_all) * 100 # 53.93%

# Grids with 1-10 genera
gndf50_nonzero <- gndf50[gndf50$Richness != 0,]
gndf50_hist <- hist(gndf50_nonzero$Richness, breaks = 10)
gndf50_counts <- gndf50_hist[[2]]
gndf50_1_10_pct <- (gndf50_counts[1] / sum(gndf50_counts)) * 100 
gndf50_1_10_pct # 73.55%

# Grids with 10-80 genera
gndf50_10_80_pct <- (sum(gndf50_counts[2:8]) / sum(gndf50_counts)) * 100 # 26.03%
gndf50_10_80_pct

# Grids > 80 genera
gndf50_gt80_pct <- (sum(gndf50_counts[9:10]) / sum(gndf50_counts)) * 100 # 0.41%
gndf50_gt80_pct

# Proportion check
sum(gndf50_1_10_pct, gndf50_10_80_pct, gndf50_gt80_pct) # 100%

# Grids 75 km
gndf75 <- vect("./Shapefiles/PAM/dryflor/genera/dryflor_PAM_genera_75/dryflor_PAM_genera_75.shp")

# Grids with zero occurrences
gndf75_all <- length(gndf75$Richness) # 818 grids
gndf75_zero <- length(gndf75[gndf75$Richness == 0]) # 344
gndf75_zero_pct <- (gndf75_zero / gndf75_all) * 100 # 42.05%

# Grids with 1-10 genera
gndf75_nonzero <- gndf75[gndf75$Richness != 0,]
gndf75_hist <- hist(gndf75_nonzero$Richness, breaks = 10)
gndf75_counts <- gndf75_hist[[2]]
gndf75_1_10_pct <- (gndf75_counts[1] / sum(gndf75_counts)) * 100 
gndf75_1_10_pct # 66.45%

# Grids with 10-80 genera
gndf75_10_80_pct <- (sum(gndf75_counts[2:8]) / sum(gndf75_counts)) * 100 # 32.91%
gndf75_10_80_pct

# Grids with > 80 genera
gndf75_gt80_pct <- (sum(gndf75_counts[9:11]) / sum(gndf75_counts)) * 100 # 0.63%
gndf75_gt80_pct 

# Proportion check
sum(gndf75_1_10_pct, gndf75_10_80_pct, gndf75_gt80_pct) # 100%

# Grids 100 km
gndf100 <- vect("./Shapefiles/PAM/dryflor/genera/dryflor_PAM_genera_100/dryflor_PAM_genera_100.shp")

# Grids with zero occurrences
gndf100_all <- length(gndf100$Richness) # 531 grids
gndf100_zero <- length(gndf100[gndf100$Richness == 0]) # 187
gndf100_zero_pct <- (gndf100_zero / gndf100_all) * 100 # 35.21%

# Grids with 1-10 genera
gndf100_nonzero <- gndf100[gndf100$Richness != 0,]
gndf100_hist <- hist(gndf100_nonzero$Richness, breaks = 10)
gndf100_counts <- gndf100_hist[[2]]
gndf100_1_10_pct <- (gndf100_counts[1] / sum(gndf100_counts)) * 100 # 61.04%
gndf100_1_10_pct

# Grids with 10-80 genera
gndf100_10_80_pct <- (sum(gndf100_counts[2:8]) / sum(gndf100_counts)) * 100 # 37.5%
gndf100_10_80_pct

# Grids with > 80 genera
gndf100_gt80_pct <- (sum(gndf100_counts[9:12]) / sum(gndf100_counts)) * 100 # 1.45%
gndf100_gt80_pct 

# Proportion check
sum(gndf100_1_10_pct, gndf100_10_80_pct, gndf100_gt80_pct) # 100%


##TEOW

##Species

# Grids 50 km
sptw50 <- vect("./Shapefiles/PAM/teow/species/teow_PAM_species_50/teow_PAM_species_50.shp")

# Grids with zero occurrences
sptw50_all <- length(sptw50$Richness) # 1610 grids
sptw50_zero <- length(sptw50[sptw50$Richness == 0]) # 1015
sptw50_zero_pct <- (sptw50_zero / sptw50_all) * 100 # 63.04%

# Grids with 1-20 species
sptw50_nonzero <- sptw50[sptw50$Richness != 0,]
sptw50_hist <- hist(sptw50_nonzero$Richness, breaks = 10)
sptw50_counts <- sptw50_hist[[2]]
sptw50_1_20_pct <- (sptw50_counts[1] / sum(sptw50_counts)) * 100 # 83.19%
sptw50_1_20_pct

# Grids with 20-100 species
sptw50_20_100_pct <- (sum(sptw50_counts[2:5]) / sum(sptw50_counts)) * 100 # 15.46%
sptw50_20_100_pct

# Grids with >100 species
sptw50_gt100_pct <- (sum(sptw50_counts[6:11]) / sum(sptw50_counts)) * 100 # 1.34%
sptw50_gt100_pct


sum(sptw50_1_20_pct, sptw50_20_100_pct, sptw50_gt100_pct) # 100%

# Grids 75 km
sptw75 <- vect("./Shapefiles/PAM/teow/species/teow_PAM_species_75/teow_PAM_species_75.shp")

# Grids with zero occurrences
sptw75_all <- length(sptw75$Richness) # 844 grids
sptw75_zero <- length(sptw75[sptw75$Richness == 0]) # 437
sptw75_zero_pct <- (sptw75_zero / sptw75_all) * 100 # 51.77%

# Grids with 1-20 species
sptw75_nonzero <- sptw75[sptw75$Richness != 0,]
sptw75_hist <- hist(sptw75_nonzero$Richness, breaks = 10)
sptw75_counts <- sptw75_hist[[2]]
sptw75_1_20_pct <- (sptw75_counts[1] / sum(sptw75_counts)) * 100 # 75.67%
sptw75_1_20_pct 

# Grids with 20-100 species
sptw75_20_100_pct <- (sum(sptw75_counts[2:5]) / sum(sptw75_counts)) * 100 # 22.60%
sptw75_20_100_pct 

# Grids with >100 species
sptw75_gt100_pct <- (sum(sptw75_counts[6:12]) / sum(sptw75_counts)) * 100 # 1.71%
sptw75_gt100_pct

sum(sptw75_1_20_pct, sptw75_20_100_pct, sptw75_gt100_pct) # 100%

# Grids 100 km
sptw100 <- vect("./Shapefiles/PAM/teow/species/teow_PAM_species_100/teow_PAM_species_100.shp")

# Grids with zero occurrences
sptw100_all <- length(sptw100$Richness) # 519 grids
sptw100_zero <- length(sptw100[sptw100$Richness == 0]) # 214
sptw100_zero_pct <- (sptw100_zero / sptw100_all) * 100 # 41.23%

# Grids with 1-20 species
sptw100_nonzero <- sptw100[sptw100$Richness != 0,]
sptw100_hist <- hist(sptw100_nonzero$Richness, breaks = 10)
sptw100_counts <- sptw100_hist[[2]]
sptw100_1_20_pct <- (sptw100_counts[1] / sum(sptw100_counts)) * 100 # 71.47%

# Grids with 20-100 species
sptw100_20_100_pct <- (sum(sptw100_counts[2:5]) / sum(sptw100_counts)) * 100 # 25.57%

# Grids with >100 species
sptw100_gt100_pct <- (sum(sptw100_counts[6:11]) / sum(sptw100_counts)) * 100 # 2.95%

# Proportion check
sum(sptw100_1_20_pct, sptw100_20_100_pct, sptw100_gt100_pct) # 100%


##Genera 
# Grids 50 km
gntw50 <- vect("./Shapefiles/PAM/teow/genera/teow_PAM_genera_50/teow_PAM_genera_50.shp")

# Grids with zero occurrences
gntw50_all <- length(gntw50$Richness) # 1610 grids
gntw50_zero <- length(gntw50[gntw50$Richness == 0]) # 998
gntw50_zero_pct <- (gntw50_zero / gntw50_all) * 100 # 61.98%

# Grids with 1-10 genera
gntw50_nonzero <- gntw50[gntw50$Richness != 0,]
gntw50_hist <- hist(gntw50_nonzero$Richness, breaks = 8)
gntw50_counts <- gntw50_hist[[2]]
gntw50_1_10_pct <- (gntw50_counts[1] / sum(gntw50_counts)) * 100 # 72.54%

# Grids with 10-80 genera
gntw50_10_80_pct <- (sum(gntw50_counts[2:8]) / sum(gntw50_counts)) * 100 # 27.12%

# Grids with >80 genera
gntw50_gt80_pct <- (sum(gntw50_counts[9:10]) / sum(gntw50_counts)) * 100 # 0.32%

# Proportion check
sum(gntw50_1_10_pct, gntw50_10_80_pct, gntw50_gt80_pct) # 100%

# Grids 75 km
gntw75 <- vect("./Shapefiles/PAM/teow/genera/teow_PAM_genera_75/teow_PAM_genera_75.shp")

# Grids with zero occurrences
gntw75_all <- length(gntw75$Richness) # 844 grids
gntw75_zero <- length(gntw75[gntw75$Richness == 0]) # 430
gntw75_zero_pct <- (gntw75_zero / gntw75_all) * 100 # 50.94%

# Grids with 1-10 genera
gntw75_nonzero <- gntw75[gntw75$Richness != 0,]
gntw75_hist <- hist(gntw75_nonzero$Richness, breaks = 10)
gntw75_counts <- gntw75_hist[[2]]
gntw75_1_10_pct <- (gntw75_counts[1] / sum(gntw75_counts)) * 100 # 65.70%

# Grids with 10-80 genera
gntw75_10_80_pct <- (sum(gntw75_counts[2:8]) / sum(gntw75_counts)) * 100 # 19.80%

# Grids with >80 genera
gntw75_gt80_pct <- (sum(gntw75_counts[9:10]) / sum(gntw75_counts)) * 100 # 0.48%

# Proportion check
sum(gntw75_1_10_pct, gntw75_10_80_pct, gntw75_gt80_pct) # 100%

# Grids 100 km
gntw100 <- vect("./Shapefiles/PAM/teow/genera/teow_PAM_genera_100/teow_PAM_genera_100.shp")

# Grids with zero occurrences
gntw100_all <- length(gntw100$Richness) # 519 grids
gntw100_zero <- length(gntw100[gntw100$Richness == 0]) # 208
gntw100_zero_pct <- (gntw100_zero / gntw100_all) * 100 # 40.07%

# Grids with 1-10 genera
gntw100_nonzero <- gntw100[gntw100$Richness != 0,]
gntw100_hist <- hist(gntw100_nonzero$Richness, breaks = 10)
gntw100_counts <- gntw100_hist[[2]]
gntw100_1_10_pct <- (gntw100_counts[1] / sum(gntw100_counts)) * 100 # 64.30%
gntw100_1_10_pct

# Grids with 10-80 genera
gntw100_10_80_pct <- (sum(gntw100_counts[2:8]) / sum(gntw100_counts)) * 100 # 35.04%
gntw100_10_80_pct

# Grids with >80 genera
gntw100_gt80_pct <- (sum(gntw100_counts[9:10]) / sum(gntw100_counts)) * 100 # 0.64%
gntw100_gt80_pct

# Proportion check
sum(gntw100_1_10_pct, gntw100_10_80_pct, gntw100_gt80_pct) # 100%


#### Calculation of Dryflor species distribution percentages across various grid sizes 

# Percentage of empty grids (no occurrences of Dryflor species)
Empty_grids_spdf <- sum((spdf50_all * spdf50_pct_zero),
                        (spdf75_all * spdf75_pct_zero),
                        (spdf100_all * spdf100_pct_zero)) /
  sum(spdf50_all, spdf75_all, spdf100_all) 
Empty_grids_spdf  #  49.16%

# Percentage of grids with 1-20 Dryflor species
grids_spdf_1_20 <- sum((spdf50_all * spdf50_no_zero_pct),
                       (spdf75_all * spdf75_no_zero_pct),
                       (spdf100_all * spdf100_no_zero_pct)) /
  sum(spdf50_all, spdf75_all, spdf100_all) 
grids_spdf_1_20  #  80.23%

# Percentage of grids with 20-100 Dryflor species
grids_spdf_20_100 <- sum((spdf50_all * spdf50_20_100_pct),
                         (spdf75_all * spdf75_20_100_pct),
                         (spdf100_all * spdf100_20_100_pct)) /
  sum(spdf50_all, spdf75_all, spdf100_all) 
grids_spdf_20_100  #  17.15%

# Percentage of grids with more than 100 Dryflor species
grids_spdf_gt100 <- sum((spdf50_all * spdf50_gt100_pct),
                        (spdf75_all * spdf75_gt100_pct),
                        (spdf100_all * spdf100_gt100_pct)) /
  sum(spdf50_all, spdf75_all, spdf100_all) 
grids_spdf_gt100  #  6.61%

# Calculation of Dryflor genera distribution percentages across various grid categories

# Percentage of empty grids (no occurrences of Dryflor genera)
Empty_grids_gndf <- sum((gndf50_all * gndf50_zero_pct),
                        (gndf75_all * gndf75_zero_pct),
                        (gndf100_all * gndf100_zero_pct)) /
  sum(gndf50_all, gndf75_all, gndf100_all) 
Empty_grids_gndf  #  47.21%

# Percentage of grids with 1-10 Dryflor genera
grids_gndf_1_10 <- sum((gndf50_all * gndf50_1_10_pct),
                       (gndf75_all * gndf75_1_10_pct),
                       (gndf100_all * gndf100_1_10_pct)) /
  sum(gndf50_all, gndf75_all, gndf100_all) 
grids_gndf_1_10  #  69.29%

# Percentage of grids with 10-80 Dryflor genera
grids_gndf_10_80 <- sum((gndf50_all * gndf50_10_80_pct),
                        (gndf75_all * gndf75_10_80_pct),
                        (gndf100_all * gndf100_10_80_pct)) /
  sum(gndf50_all, gndf75_all, gndf100_all) 
grids_gndf_10_80  #  30.03%

# Percentage of grids with more than 80 Dryflor genera
grids_gndf_gt80 <- sum((gndf50_all * gndf50_gt80_pct),
                       (gndf75_all * gndf75_gt80_pct),
                       (gndf100_all * gndf100_gt80_pct)) /
  sum(gndf50_all, gndf75_all, gndf100_all) 
grids_gndf_gt80  #  0.66%

# Calculation of TEOW species distribution percentages across various grid categories

# Percentage of empty grids (no occurrences of TEOW species)
Empty_grids_sptw <- sum((sptw50_all * sptw50_zero_pct),
                        (sptw75_all * sptw75_zero_pct),
                        (sptw100_all * sptw100_zero_pct)) /
  sum(sptw50_all, sptw75_all, sptw100_all) 
Empty_grids_sptw  #  56.03%

# Percentage of grids with 1-20 TEOW species
grids_sptw_1_20 <- sum((sptw50_all * sptw50_1_20_pct),
                       (sptw75_all * sptw75_1_20_pct),
                       (sptw100_all * sptw100_1_20_pct)) /
  sum(sptw50_all, sptw75_all, sptw100_all) 
grids_sptw_1_20  #  79.01%

# Percentage of grids with 20-100 TEOW species
grids_sptw_20_100 <- sum((sptw50_all * sptw50_20_100_pct),
                         (sptw75_all * sptw75_20_100_pct),
                         (sptw100_all * sptw100_20_100_pct)) /
  sum(sptw50_all, sptw75_all, sptw100_all) 
grids_sptw_20_100  #  19.25%

# Percentage of grids with more than 100 TEOW species
grids_sptw_gt100 <- sum((sptw50_all * sptw50_gt100_pct),
                        (sptw75_all * sptw75_gt100_pct),
                        (sptw100_all * sptw100_gt100_pct)) /
  sum(sptw50_all, sptw75_all, sptw100_all) 
grids_sptw_gt100  #  1.73%

# Calculation of TEOW genera distribution percentages across various grid categories

# Percentage of empty grids (no occurrences of TEOW genera)
Empty_grids_gntw <- sum((gntw50_all * gntw50_zero_pct),
                        (gntw75_all * gntw75_zero_pct),
                        (gntw100_all * gntw100_zero_pct)) /
  sum(gntw50_all, gntw75_all, gntw100_all) 
Empty_grids_gntw  #  55.02%

# Percentage of grids with 1-10 TEOW genera
grids_gntw_1_10 <- sum((gntw50_all * gntw50_1_10_pct),
                       (gntw75_all * gntw75_1_10_pct),
                       (gntw100_all * gntw100_1_10_pct)) /
  sum(gntw50_all, gntw75_all, gntw100_all) 
grids_gntw_1_10  #  69.16%

# Percentage of grids with 10-80 TEOW genera
grids_gntw_20_100 <- sum((gntw50_all * gntw50_10_80_pct),
                         (gntw75_all * gntw75_10_80_pct),
                         (gntw100_all * gntw100_10_80_pct)) /
  sum(gntw50_all, gntw75_all, gntw100_all) 
grids_gntw_20_100  #  30.40%

# Percentage of grids with more than 80 TEOW genera
grids_gntw_gt100 <- sum((gntw50_all * gntw50_gt80_pct),
                        (gntw75_all * gntw75_gt80_pct),
                        (gntw100_all * gntw100_gt80_pct)) /
  sum(gntw50_all, gntw75_all, gntw100_all) 
grids_gntw_gt100  #  0.42%



