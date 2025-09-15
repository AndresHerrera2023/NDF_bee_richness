### Code: Grid analysis
### Project: Neotropical dry forest bees
### Authors: Herrera-Motta et al. 
### Last update: 09/11/25


#Required libraries:
library(terra)

# Set your working directory
setwd("./NDF_bees_project/")

# Grids 50 km
spdf50 <- vect("./Shapefiles/PAM/dryflor/species/50/dryflor_pam_species_50.shp")

# Grids with zero species
spdf50_all <- length(spdf50$Richness) #1576 grids
spdf50_zero <- length(spdf50[spdf50$Richness == 0]) #989 grids
spdf50_pct_zero <- (spdf50_zero / spdf50_all) * 100 #62.75%

# Grids with 1-20 species
spdf50_no_zero <- spdf50[spdf50$Richness != 0,]
spdf50_no_zero <- hist(spdf50_no_zero$Richness, breaks = 10)
spdf50_no_zero <- spdf50_no_zero[[2]]
spdf50_no_zero_pct <- (spdf50_no_zero[1] / sum(spdf50_no_zero)) * 100
spdf50_no_zero_pct  #82.62%

# Grids with 20-100 species
spdf50_20_100_pct  <- (sum(spdf50_no_zero[2:5]) / sum(spdf50_no_zero)) * 100
spdf50_20_100_pct   #15.16%

# Grids with >100 species
spdf50_gt100_pct  <- (sum(spdf50_no_zero[6:12]) / sum(spdf50_no_zero)) * 100
spdf50_gt100_pct  # 2.21%

# Proportion check
sum(spdf50_no_zero_pct, spdf50_20_100_pct, spdf50_gt100_pct) # = 100%


# Grids 75 km
spdf75 <- vect("./Shapefiles/PAM/dryflor/species/75/dryflor_pam_species_75.shp")

# Grids with zero species
spdf75_all <- length(spdf75$Richness) #818 grids
spdf75_zero <- length(spdf75[spdf75$Richness == 0]) #408
spdf75_pct_zero <- (spdf75_zero / spdf75_all) * 100 #49.87 are zero.

# Grids with 1-20 species
spdf75_no_zero <- spdf75[spdf75$Richness != 0,]
spdf75_no_zero <- hist(spdf75_no_zero$Richness, breaks = 10)
spdf75_no_zero <- spdf75_no_zero[[2]]
spdf75_no_zero_pct <- (spdf75_no_zero[1] / sum(spdf75_no_zero)) * 100
spdf75_no_zero_pct  #79.26%

# Grids with 20-100 species
spdf75_20_100_pct <- (sum(spdf75_no_zero[2:5]) / sum(spdf75_no_zero)) * 100
spdf75_20_100_pct  #16.34%

# Grids with >100 species
spdf75_gt100_pct <- (sum(spdf75_no_zero[6:14]) / sum(spdf75_no_zero)) * 100
spdf75_gt100_pct # 4.39%

# Proportion check
sum(spdf75_no_zero_pct, spdf75_20_100_pct, spdf75_gt100_pct) # = 100%

# Grids 100 km
spdf100 <- vect("./Shapefiles/PAM/dryflor/species/100/dryflor_pam_species_100.shp")

# Grids with zero species
spdf100_all <- length(spdf100$Richness) #531 grids
spdf100_zero <- length(spdf100[spdf100$Richness == 0]) #220
spdf100_pct_zero <- (spdf100_zero / spdf100_all) * 100 #42.43 are zero.

# Grids with 1-20 species
spdf100_no_zero <- spdf100[spdf100$Richness != 0,]
spdf100_no_zero <- hist(spdf100_no_zero$Richness, breaks = 12)
spdf100_no_zero <- spdf100_no_zero[[2]]
spdf100_no_zero_pct <- (spdf100_no_zero[1] / sum(spdf100_no_zero)) * 100
spdf100_no_zero_pct  #71.38%

# Grids with 20-100 species
spdf100_20_100_pct <- (sum(spdf100_no_zero[2:5]) / sum(spdf100_no_zero)) * 100
spdf100_20_100_pct  #22.82%

# Grids with >100 species
spdf100_gt100_pct <- (sum(spdf100_no_zero[6:15]) / sum(spdf100_no_zero)) * 100
spdf100_gt100_pct # 5.78%

# Proportion check
sum(spdf100_no_zero_pct, spdf100_20_100_pct, spdf100_gt100_pct) # 100%


## Dryflor 

#Genera:

# Grids 50 km
gndf50 <- vect("./Shapefiles/PAM/dryflor/genera/50/dryflor_pam_genera_50.shp")

# Grids with zero species
gndf50_all <- length(gndf50$Richness) # 1576 grids
gndf50_zero <- length(gndf50[gndf50$Richness == 0]) # 955
gndf50_zero_pct <- (gndf50_zero / gndf50_all) * 100 # 60.59%

# Grids with 1-10 genera
gndf50_nonzero <- gndf50[gndf50$Richness != 0,]
gndf50_hist <- hist(gndf50_nonzero$Richness, breaks = 10)
gndf50_counts <- gndf50_hist[[2]]
gndf50_1_10_pct <- (gndf50_counts[1] / sum(gndf50_counts)) * 100 
gndf50_1_10_pct # 72.30%

# Grids with 10-80 genera
gndf50_10_80_pct <- (sum(gndf50_counts[2:8]) / sum(gndf50_counts)) * 100 # 27.05%
gndf50_10_80_pct

# Grids > 80 genera
gndf50_gt80_pct <- (sum(gndf50_counts[9:10]) / sum(gndf50_counts)) * 100 # 0.64%
gndf50_gt80_pct

# Proportion check
sum(gndf50_1_10_pct, gndf50_10_80_pct, gndf50_gt80_pct) # 100%

# Grids 75 km
gndf75 <- vect("./Shapefiles/PAM/dryflor/genera/75/dryflor_pam_genera_75.shp")

# Grids with zero species
gndf75_all <- length(gndf75$Richness) # 818 grids
gndf75_zero <- length(gndf75[gndf75$Richness == 0]) # 389
gndf75_zero_pct <- (gndf75_zero / gndf75_all) * 100 # 47.55%

# Grids with 1-10 genera
gndf75_nonzero <- gndf75[gndf75$Richness != 0,]
gndf75_hist <- hist(gndf75_nonzero$Richness, breaks = 10)
gndf75_counts <- gndf75_hist[[2]]
gndf75_1_10_pct <- (gndf75_counts[1] / sum(gndf75_counts)) * 100 
gndf75_1_10_pct # 65.50%

# Grids with 10-80 genera
gndf75_10_80_pct <- (sum(gndf75_counts[2:8]) / sum(gndf75_counts)) * 100 # 33.56%
gndf75_10_80_pct

# Grids with > 80 genera
gndf75_gt80_pct <- (sum(gndf75_counts[9:10]) / sum(gndf75_counts)) * 100 # 0.93%
gndf75_gt80_pct 

# Proportion check
sum(gndf75_1_10_pct, gndf75_10_80_pct, gndf75_gt80_pct) # 100%

# Grids 100 km
gndf100 <- vect("./Shapefiles/PAM/dryflor/genera/100/dryflor_pam_genera_100.shp")

# Grids with zero occurrences
gndf100_all <- length(gndf100$Richness) # 531 grids
gndf100_zero <- length(gndf100[gndf100$Richness == 0]) # 208
gndf100_zero_pct <- (gndf100_zero / gndf100_all) * 100 # 39.17%

# Grids with 1-10 genera
gndf100_nonzero <- gndf100[gndf100$Richness != 0,]
gndf100_hist <- hist(gndf100_nonzero$Richness, breaks = 10)
gndf100_counts <- gndf100_hist[[2]]
gndf100_1_10_pct <- (gndf100_counts[1] / sum(gndf100_counts)) * 100 # 61.30%
gndf100_1_10_pct

# Grids with 10-80 genera
gndf100_10_80_pct <- (sum(gndf100_counts[2:8]) / sum(gndf100_counts)) * 100 # 36.84%
gndf100_10_80_pct

# Grids with > 80 genera
gndf100_gt80_pct <- (sum(gndf100_counts[9:11]) / sum(gndf100_counts)) * 100 # 1.85%
gndf100_gt80_pct 

# Proportion check
sum(gndf100_1_10_pct, gndf100_10_80_pct, gndf100_gt80_pct) # 100%


##TEOW

##Species

# Grids 50 km
sptw50 <- vect("./Shapefiles/PAM/teow/species/50/teow_pam_species_50.shp")

# Grids with zero species
sptw50_all <- length(sptw50$Richness) # 1475 grids
sptw50_zero <- length(sptw50[sptw50$Richness == 0]) # 976
sptw50_zero_pct <- (sptw50_zero / sptw50_all) * 100 # 66.16%

# Grids with 1-20 species
sptw50_nonzero <- sptw50[sptw50$Richness != 0,]
sptw50_hist <- hist(sptw50_nonzero$Richness, breaks = 10)
sptw50_counts <- sptw50_hist[[2]]
sptw50_1_20_pct <- (sptw50_counts[1] / sum(sptw50_counts)) * 100 # 83.96%
sptw50_1_20_pct

# Grids with 20-100 species
sptw50_20_100_pct <- (sum(sptw50_counts[2:5]) / sum(sptw50_counts)) * 100 # 14.02%
sptw50_20_100_pct

# Grids with >100 species
sptw50_gt100_pct <- (sum(sptw50_counts[6:10]) / sum(sptw50_counts)) * 100 # 2.00%
sptw50_gt100_pct


sum(sptw50_1_20_pct, sptw50_20_100_pct, sptw50_gt100_pct) # 100%

# Grids 75 km
sptw75 <- vect("./Shapefiles/PAM/teow/species/75/teow_pam_species_75.shp")

# Grids with zero species
sptw75_all <- length(sptw75$Richness) # 732 grids
sptw75_zero <- length(sptw75[sptw75$Richness == 0]) # 387
sptw75_zero_pct <- (sptw75_zero / sptw75_all) * 100 # 52.86%

# Grids with 1-20 species
sptw75_nonzero <- sptw75[sptw75$Richness != 0,]
sptw75_hist <- hist(sptw75_nonzero$Richness, breaks = 10)
sptw75_counts <- sptw75_hist[[2]]
sptw75_1_20_pct <- (sptw75_counts[1] / sum(sptw75_counts)) * 100 # 75.65%
sptw75_1_20_pct 

# Grids with 20-100 species
sptw75_20_100_pct <- (sum(sptw75_counts[2:5]) / sum(sptw75_counts)) * 100 #20.57%
sptw75_20_100_pct 

# Grids with >100 species
sptw75_gt100_pct <- (sum(sptw75_counts[6:12]) / sum(sptw75_counts)) * 100 # 3.76%
sptw75_gt100_pct

sum(sptw75_1_20_pct, sptw75_20_100_pct, sptw75_gt100_pct) # 100%

# Grids 100 km
sptw100 <- vect("./Shapefiles/PAM/teow/species/100/teow_pam_species_100.shp")

# Grids with zero species
sptw100_all <- length(sptw100$Richness) # 462 grids
sptw100_zero <- length(sptw100[sptw100$Richness == 0]) # 191
sptw100_zero_pct <- (sptw100_zero / sptw100_all) * 100 # 41.34%

# Grids with 1-20 species
sptw100_nonzero <- sptw100[sptw100$Richness != 0,]
sptw100_hist <- hist(sptw100_nonzero$Richness, breaks = 10)
sptw100_counts <- sptw100_hist[[2]]
sptw100_1_20_pct <- (sptw100_counts[1] / sum(sptw100_counts)) * 100 # 73.06%

# Grids with 20-100 species
sptw100_20_100_pct <- (sum(sptw100_counts[2:5]) / sum(sptw100_counts)) * 100 # 22.14%
sptw100_20_100_pct 
# Grids with >100 species
sptw100_gt100_pct <- (sum(sptw100_counts[6:13]) / sum(sptw100_counts)) * 100 # 4.79%
sptw100_gt100_pct 
# Proportion check
sum(sptw100_1_20_pct, sptw100_20_100_pct, sptw100_gt100_pct) # 100%


##Genera 
# Grids 50 km
gntw50 <- vect("./Shapefiles/PAM/teow/genera/50/teow_pam_genera_50.shp")

# Grids with zero species
gntw50_all <- length(gntw50$Richness) # 1475 grids
gntw50_zero <- length(gntw50[gntw50$Richness == 0]) # 947
gntw50_zero_pct <- (gntw50_zero / gntw50_all) * 100 # 64.20%

# Grids with 1-10 genera
gntw50_nonzero <- gntw50[gntw50$Richness != 0,]
gntw50_hist <- hist(gntw50_nonzero$Richness, breaks = 8)
gntw50_counts <- gntw50_hist[[2]]
gntw50_1_10_pct <- (gntw50_counts[1] / sum(gntw50_counts)) * 100 # 73.10%

# Grids with 10-80 genera
gntw50_10_80_pct <- (sum(gntw50_counts[2:8]) / sum(gntw50_counts)) * 100 # 26.70%
gntw50_10_80_pct

# Grids with >80 genera
gntw50_gt80_pct <- (sum(gntw50_counts[9:10]) / sum(gntw50_counts)) * 100 # 0.18%
gntw50_gt80_pct 

# Proportion check
sum(gntw50_1_10_pct, gntw50_10_80_pct, gntw50_gt80_pct) # 100%

# Grids 75 km
gntw75 <- vect("./Shapefiles/PAM/teow/genera/75/teow_pam_genera_75.shp")

# Grids with zero species
gntw75_all <- length(gntw75$Richness) # 732 grids
gntw75_zero <- length(gntw75[gntw75$Richness == 0]) # 370
gntw75_zero_pct <- (gntw75_zero / gntw75_all) * 100 # 50.54%

# Grids with 1-10 genera
gntw75_nonzero <- gntw75[gntw75$Richness != 0,]
gntw75_hist <- hist(gntw75_nonzero$Richness, breaks = 10)
gntw75_counts <- gntw75_hist[[2]]
gntw75_1_10_pct <- (gntw75_counts[1] / sum(gntw75_counts)) * 100 # 69.61%

# Grids with 10-80 genera
gntw75_10_80_pct <- (sum(gntw75_counts[2:8]) / sum(gntw75_counts)) * 100 # 29.83%

# Grids with >80 genera
gntw75_gt80_pct <- (sum(gntw75_counts[9:10]) / sum(gntw75_counts)) * 100 # 0.55%

# Proportion check
sum(gntw75_1_10_pct, gntw75_10_80_pct, gntw75_gt80_pct) # 100%

# Grids 100 km
gntw100 <- vect("./Shapefiles/PAM/teow/genera/100/teow_pam_genera_100.shp")

# Grids with zero species
gntw100_all <- length(gntw100$Richness) # 462 grids
gntw100_zero <- length(gntw100[gntw100$Richness == 0]) # 180
gntw100_zero_pct <- (gntw100_zero / gntw100_all) * 100 # 38.96%

# Grids with 1-10 genera
gntw100_nonzero <- gntw100[gntw100$Richness != 0,]
gntw100_hist <- hist(gntw100_nonzero$Richness, breaks = 10)
gntw100_counts <- gntw100_hist[[2]]
gntw100_1_10_pct <- (gntw100_counts[1] / sum(gntw100_counts)) * 100 # 63.82%
gntw100_1_10_pct

# Grids with 10-80 genera
gntw100_10_80_pct <- (sum(gntw100_counts[2:8]) / sum(gntw100_counts)) * 100 # 35.10%
gntw100_10_80_pct

# Grids with >80 genera
gntw100_gt80_pct <- (sum(gntw100_counts[9:11]) / sum(gntw100_counts)) * 100 # 1.06%
gntw100_gt80_pct

# Proportion check
sum(gntw100_1_10_pct, gntw100_10_80_pct, gntw100_gt80_pct) # 100%


#### Calculation of Dryflor species distribution percentages across various grid sizes 
#50

# Percentage of empty grids (no occurrences of Dryflor species)
Empty_grids_spdf <- sum((spdf50_all * spdf50_pct_zero),
                        (spdf75_all * spdf75_pct_zero),
                        (spdf100_all * spdf100_pct_zero)) /
  sum(spdf50_all, spdf75_all, spdf100_all) 
Empty_grids_spdf  #  55.28%

# Percentage of grids with 1-20 Dryflor species
grids_spdf_1_20 <- sum((spdf50_all * spdf50_no_zero_pct),
                       (spdf75_all * spdf75_no_zero_pct),
                       (spdf100_all * spdf100_no_zero_pct)) /
  sum(spdf50_all, spdf75_all, spdf100_all) 
grids_spdf_1_20  #  79.64%

# Percentage of grids with 20-100 Dryflor species
grids_spdf_20_100 <- sum((spdf50_all * spdf50_20_100_pct),
                         (spdf75_all * spdf75_20_100_pct),
                         (spdf100_all * spdf100_20_100_pct)) /
  sum(spdf50_all, spdf75_all, spdf100_all) 
grids_spdf_20_100  #  16.88%

# Percentage of grids with more than 100 Dryflor species
grids_spdf_gt100 <- sum((spdf50_all * spdf50_gt100_pct),
                        (spdf75_all * spdf75_gt100_pct),
                        (spdf100_all * spdf100_gt100_pct)) /
  sum(spdf50_all, spdf75_all, spdf100_all) 
grids_spdf_gt100  #  3.47%

# Calculation of Dryflor genera distribution percentages across various grid categories

# Percentage of empty grids (no occurrences of Dryflor genera)
Empty_grids_gndf <- sum((gndf50_all * gndf50_zero_pct),
                        (gndf75_all * gndf75_zero_pct),
                        (gndf100_all * gndf100_zero_pct)) /
  sum(gndf50_all, gndf75_all, gndf100_all) 
Empty_grids_gndf  #  53.05%

# Percentage of grids with 1-10 Dryflor genera
grids_gndf_1_10 <- sum((gndf50_all * gndf50_1_10_pct),
                       (gndf75_all * gndf75_1_10_pct),
                       (gndf100_all * gndf100_1_10_pct)) /
  sum(gndf50_all, gndf75_all, gndf100_all) 
grids_gndf_1_10  #  68.40%

# Percentage of grids with 10-80 Dryflor genera
grids_gndf_10_80 <- sum((gndf50_all * gndf50_10_80_pct),
                        (gndf75_all * gndf75_10_80_pct),
                        (gndf100_all * gndf100_10_80_pct)) /
  sum(gndf50_all, gndf75_all, gndf100_all) 
grids_gndf_10_80  #  30.65%

# Percentage of grids with more than 80 Dryflor genera
grids_gndf_gt80 <- sum((gndf50_all * gndf50_gt80_pct),
                       (gndf75_all * gndf75_gt80_pct),
                       (gndf100_all * gndf100_gt80_pct)) /
  sum(gndf50_all, gndf75_all, gndf100_all) 
grids_gndf_gt80  #  0.94%

# Calculation of TEOW species distribution percentages across various grid categories

# Percentage of empty grids (no occurrences of TEOW species)
Empty_grids_sptw <- sum((sptw50_all * sptw50_zero_pct),
                        (sptw75_all * sptw75_zero_pct),
                        (sptw100_all * sptw100_zero_pct)) /
  sum(sptw50_all, sptw75_all, sptw100_all) 
Empty_grids_sptw  #  58.22%

# Percentage of grids with 1-20 TEOW species
grids_sptw_1_20 <- sum((sptw50_all * sptw50_1_20_pct),
                       (sptw75_all * sptw75_1_20_pct),
                       (sptw100_all * sptw100_1_20_pct)) /
  sum(sptw50_all, sptw75_all, sptw100_all) 
grids_sptw_1_20  #  79.79%

# Percentage of grids with 20-100 TEOW species
grids_sptw_20_100 <- sum((sptw50_all * sptw50_20_100_pct),
                         (sptw75_all * sptw75_20_100_pct),
                         (sptw100_all * sptw100_20_100_pct)) /
  sum(sptw50_all, sptw75_all, sptw100_all) 
grids_sptw_20_100  #  17.22%

# Percentage of grids with more than 100 TEOW species
grids_sptw_gt100 <- sum((sptw50_all * sptw50_gt100_pct),
                        (sptw75_all * sptw75_gt100_pct),
                        (sptw100_all * sptw100_gt100_pct)) /
  sum(sptw50_all, sptw75_all, sptw100_all) 
grids_sptw_gt100  #  2.97%

# Calculation of TEOW genera distribution percentages across various grid categories

# Percentage of empty grids (no occurrences of TEOW genera)
Empty_grids_gntw <- sum((gntw50_all * gntw50_zero_pct),
                        (gntw75_all * gntw75_zero_pct),
                        (gntw100_all * gntw100_zero_pct)) /
  sum(gntw50_all, gntw75_all, gntw100_all) 
Empty_grids_gntw  #  56.08%

# Percentage of grids with 1-10 TEOW genera
grids_gntw_1_10 <- sum((gntw50_all * gntw50_1_10_pct),
                       (gntw75_all * gntw75_1_10_pct),
                       (gntw100_all * gntw100_1_10_pct)) /
  sum(gntw50_all, gntw75_all, gntw100_all) 
grids_gntw_1_10  #  70.54%

# Percentage of grids with 10-80 TEOW genera
grids_gntw_20_100 <- sum((gntw50_all * gntw50_10_80_pct),
                         (gntw75_all * gntw75_10_80_pct),
                         (gntw100_all * gntw100_10_80_pct)) /
  sum(gntw50_all, gntw75_all, gntw100_all) 
grids_gntw_20_100  #  29.01%

# Percentage of grids with more than 80 TEOW genera
grids_gntw_gt100 <- sum((gntw50_all * gntw50_gt80_pct),
                        (gntw75_all * gntw75_gt80_pct),
                        (gntw100_all * gntw100_gt80_pct)) /
  sum(gntw50_all, gntw75_all, gntw100_all) 
grids_gntw_gt100  #  0.44%


