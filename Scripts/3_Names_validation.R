### Code: Extract occurrences in protected areas
### Project: Neotropical dry forest bees
### Author: Andres Herrera

#Required libraries:
library(terra)

#DRYFLOR:
#Protected Areas based on DRYFLOR:
pa_dryflor <- vect("./NDF_bees_project/Shapefiles/Protected_areas/dryflor/dryflor_protected_areas.shp") 
plot(pa_dryflor, col = "black")

#DRYFLOR occurrences:
occ_dryflor <- read.csv("./NDF_bees_project/Data/STDF_bees_occ/dryflor/species/dryflor_species_occ.csv")
occ_dryflor <- vect(occ_dryflor, geom = c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84")

occ_pa_dryflor <- crop(occ_dryflor, pa_dryflor)
plot(occ_pa_dryflor)

occ_dryflor_pa <- as.data.frame(occ_pa_dryflor)
occ_dryflor_pa <- cbind(crds(occ_pa_dryflor), occ_dryflor_pa)
colnames(occ_dryflor_pa)[c(1,2)] <- c("longitude", "latitude")
occ_dryflor_pa <- occ_dryflor_pa[,c(3,1,2,4,5,6,7,8)]
write.csv(occ_dryflor_pa,"Z:/Andres/NDF_bees_project/summary_diversity/dryflor/dryflor_occ_protected_areas.csv", row.names = F)

#TEOW:
#Protected Areas based on TEOW:
pa_teow <- vect("Z:/Andres/NDF_bees_project/Shapefiles/Protected_areas/teow/teow_protected_areas2.shp") 
plot(pa_teow, col = "black")

occ_teow <- read.csv("Z:/Andres/NDF_bees_project/Data/STDF_bees_occ/teow/species/teow_species_occ.csv")
occ_teow <- vect(occ_teow, geom = c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84")

occ_pa_teow <- crop(occ_teow, pa_teow)
plot(occ_pa_teow)

occ_teow_pa <- as.data.frame(occ_pa_teow)
occ_teow_pa <- cbind(crds(occ_pa_teow), occ_teow_pa)
colnames(occ_teow_pa)[c(1,2)] <- c("longitude", "latitude")
occ_teow_pa <- occ_teow_pa[,c(3,1,2,4,5,6,7,8)]
write.csv(occ_teow_pa,"Z:/Andres/NDF_bees_project/summary_diversity/teow/teow_occ_protected_areas.csv", row.names = F)





