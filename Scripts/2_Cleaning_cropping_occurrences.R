### Code: Occurrences cleaning
### Project: Neotropical dry forest bees
### Authors: Herrera-Motta et al. 
### Last update: 09/11/25

#Required libraries:
library(rgbif)
library(dplyr)
library(readr)
library(tidyr)
library(terra)

#Setting your working Directory:
setwd("./NDF_bees_project/")

#Filtering GBIF occurrences by "Preserved_specimen" and "CatalogNumber":
gbif_merge <- list.files(path="./Data/Raw/GBIF/", pattern = ".csv$",  full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows
 head(gbif_merge)
 nrow(gbif_merge) #875983

#Removing human observations from GBIF:
gbif_basisOfRecord <- gbif_merge[gbif_merge$basisOfRecord == "PRESERVED_SPECIMEN",] 
nrow(gbif_basisOfRecord)# occ = 740713

gbif_catalogNumber <- gbif_basisOfRecord[gbif_basisOfRecord$catalogNumber != "NO APLICA" &  gbif_basisOfRecord$catalogNumber != "NO DISPONIBLE",] 
nrow(gbif_catalogNumber)# occ = 736085

gbif_catalogNumber_NA <- gbif_catalogNumber[!is.na(gbif_catalogNumber$catalogNumber),] 
nrow(gbif_catalogNumber_NA)# occ = 696466

#Filtered occurrences from GBIF:
gbif <- gbif_catalogNumber_NA[,c("countryCode", "family", "genus", "species", "decimalLongitude", "decimalLatitude")]
source <- rep("gbif", times = length(gbif$genus))
gbif$source <- source
cite <- rep("gbif", times = length(gbif$genus))
gbif$cite <- cite
head(gbif)

#Occurrences from papers:
papers <- list.files(path="./Data/raw/Literature/", pattern = ".csv$", full.names = T)  %>% 
  lapply(read_csv) %>%
  bind_rows 
papers <- papers[,c("countryCode", "family", "genus", "species", "decimalLongitude", "decimalLatitude", "cite")]
papers$source <- rep("literature", times = length(papers$genus))
papers <- papers[,c(1,2,3,4,5,6,8,7)]
head(papers)
nrow(papers) #2411

#Merging the two datasets with "sp":
conc <- bind_rows(gbif,papers) %>%
  replace_na(list(species = "sp")) %>%
  write.csv("./Data/Raw/Merged/raw_merged_occ.csv", row.names=FALSE)


#Cleaning duplicates, NAs and 0 coordinates:
occurrences <- read.csv("./Data/Raw/Merged/raw_merged_occ.csv")
nrow(occurrences)  # 698877 occ  
Finalnam <- paste0("./Data/Clean/Merged/clean_merged_occ.csv") #create this folder first  in your working directory
colnames(occurrences) <- c("countryCode", "family", "genus", "species", "longitude", "latitude", "source", "cite")
occurrences$code <-  paste(occurrences$countryCode, 
                           occurrences$family, 
                           occurrences$genus, 
                           occurrences$species,
                           occurrences$longitude, 
                           occurrences$latitude, 
                           occurrences$source, 
                           occurrences$cite, sep = "_") 
occurrences <- occurrences[!duplicated(occurrences$code), 1:9] 
nrow(occurrences) # 98369 occ
occurrences <- na.omit(occurrences[, 1:8])
nrow(occurrences) # 96165 occ
occurrences <- occurrences[occurrences$longitude != 0 & occurrences$latitude != 0, ]
nrow(occurrences) # 96161 occ
write.csv(occurrences, Finalnam, row.names = FALSE)

### Cropping with the two PDs of STDF:

#reading the clean occurrences:
occ <- read.csv("./Data/Clean/Merged/clean_merged_occ.csv")
occ <- vect(occ, geom = c("longitude", "latitude"), crs="+proj=longlat +datum=WGS84")

#DRYFLOR:
dryflor <- vect("./Shapefiles/STDF/DRYFLOR/seasonally_dryfo_dis.shp") 

#Cropping occurrences with PD:
occ_dryflor <- crop(occ, dryflor)
writeVector(occ_dryflor, "./Shapefiles/STDF_bees_occ/dryflor/dryflor_occ.shp", overwrite = TRUE)

#Setting and saving DRYFLOR occurrences as data frame:
df_dryflor <- as.data.frame(occ_dryflor)
df_dryflor <- cbind(crds(occ_dryflor), df_dryflor)
colnames(df_dryflor)[c(1,2)] <- c("longitude", "latitude")
df_dryflor <- df_dryflor[,c(3,1,2,4,5,6,7,8)]
head(df_dryflor)
nrow(df_dryflor) # 17882 occ
write.csv(df_dryflor,"./Data/Names_validation/Unverified_names/dryflor/dryflor_occ.csv", row.names = F)


#TEOW:
teow <- vect("./Shapefiles/STDF/TEOW/Tropical & Subtropical Dry Broadleaf Forest.shp") 

#Cropping occurrences with PD:
occ_teow <- crop(occ, teow)
writeVector(occ_teow, "./Shapefiles/STDF_bees_occ/teow/teow_occ.shp", overwrite = TRUE)

# Setting and saving TEOW occurrences as data frame:
df_teow <- as.data.frame(occ_teow)
df_teow <- cbind(crds(occ_teow), df_teow)
colnames(df_teow)[c(1,2)] <- c("longitude", "latitude")
df_teow <- df_teow[,c(3,1,2,4,5,6,7,8)]
head(df_teow)
nrow(df_teow) # 13612
write.csv(df_teow,"./Data/Names_validation/Unverified_names/teow/teow_occ.csv", row.names = FALSE)



#############################___DRYFLOR___###############################

#Writing DRYFLOR names for ITIS validation in txt format:
df_dryflor <- read.csv("./Data/Names_validation/Unverified_names/dryflor/dryflor_occ.csv")

spnames  <- df_dryflor %>%
  filter(species != "sp") %>%     
  distinct(species) %>%           
  pull(species)  
spnames <- c("name", spnames)
writeLines(spnames , "./Data/Names_validation/Unverified_names/dryflor/dryflor_names.txt")

# Names validation using ITIS:

valid_names <- read.csv("./Data/Names_validation/ITIS_verified_names/dryflor/dryflor_valid_names.csv")
valid_names <- subset(valid_names, NameUsage == "invalid")
valid_names <- valid_names[,c(1,3)]
df_dryflor <- merge(df_dryflor, valid_names, by = "species", all.x = TRUE)
df_dryflor$species <- ifelse(is.na(df_dryflor$AcceptedName), df_dryflor$species, df_dryflor$AcceptedName)
total_species_dryflor <- subset(df_dryflor, select = -AcceptedName)

# Names from ITIS without match (Manually edited):
manual <- read.csv("./Data/Names_validation/ITIS_verified_names/dryflor/dryflor_manual_valid_names.csv")

# Merging by occurrences:
occ_manual_names <- merge(total_species_dryflor, manual, by = "species", all.x = TRUE)

# Replacing and saving the names for species:
occ_manual_names$species <- ifelse(is.na(occ_manual_names$species_final), occ_manual_names$species, occ_manual_names$species_final)
new_genus <- sub(" .*", "", occ_manual_names$species)
occ_manual_names$genus <- ifelse(
  occ_manual_names$species != "sp" & new_genus != occ_manual_names$genus,
  new_genus,
  occ_manual_names$genus
)

# Clean up: remove temporary column
final_names <- subset(occ_manual_names, select = -species_final)

# Reorder columns and filter out removed species
final_names <- final_names[,c(2,3,4,5,6,1,7,8)] 
final_names <- final_names %>% filter(species != "remove")

# Output
nrow(final_names) # 17871 occ
write.csv(final_names,"./Data/STDF_bees_occ/dryflor/all/dryflor_bees_final_occ.csv", row.names = FALSE)

########################___TEOW__#####################################

# Writing TEOW names for ITIS validation in txt format:
df_teow <- read.csv("./Data/Names_validation/Unverified_names/teow/teow_occ.csv")

spnames <- df_teow %>%
  filter(species != "sp") %>%
  distinct(species) %>%
  pull(species)  
spnames <- c("name", spnames)

writeLines(spnames, "./Data/Names_validation/Unverified_names/teow/teow_names.txt")

# TEOW names validation using ITIS 
df_teow <- read.csv("./Data/Names_validation/Unverified_names/teow/teow_occ.csv")
valid_names <- read.csv("./Data/Names_validation/ITIS_verified_names/teow/teow_valid_names.csv")

valid_names <- subset(valid_names, NameUsage == "invalid")
valid_names <- valid_names[,c(1,3)]
df_teow <- merge(df_teow, valid_names, by = "species", all.x = TRUE)
df_teow$species <- ifelse(is.na(df_teow$AcceptedName), df_teow$species, df_teow$AcceptedName)
total_species_teow <- subset(df_teow, select = -AcceptedName)

# names from ITIS without match. Manually edited:
manual <- read.csv("./Data/Names_validation/ITIS_verified_names/teow/teow_manual_valid_names.csv")

# Merging by occurrences:
occ_manual_names <- merge(total_species_teow, manual, by = "species", all.x = TRUE)

# Replacing and saving the names for species:
occ_manual_names$species <- ifelse(is.na(occ_manual_names$species_final), occ_manual_names$species, occ_manual_names$species_final)
new_genus <- sub(" .*", "", occ_manual_names$species)

occ_manual_names$genus <- ifelse(
  occ_manual_names$species != "sp" & new_genus != occ_manual_names$genus,
  new_genus,
  occ_manual_names$genus
)

# Clean up: remove temporary column
final_names <- subset(occ_manual_names, select = -species_final)
head(final_names)
final_names <- final_names[,c(2,3,4,5,6,1,7,8)] 
final_names <- final_names %>%
  filter(species != "remove")
head(final_names)
nrow(final_names) # 13598
write.csv(final_names,"./Data/STDF_bees_occ/teow/all/teow_bees_final_occ.csv", row.names = FALSE)


