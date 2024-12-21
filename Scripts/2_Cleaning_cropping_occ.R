### 2. Cleaning the occurrences and cropping in the area of interest ###

# Installing the required packages and loading the libraries
install.packages(c("terra", "dplyr", "readr", "tidyr"))
library(terra)
library(dplyr)
library(readr)
library(tidyr)

# Setting the working directory (update the path accordingly)
setwd()  # Specify your working directory here

# Concatenating the raw data from GBIF and literature

# Loading GBIF data from CSV files in the specified directory
gbif <- list.files(path = "./Data/raw_data/gbif_raw_data/", pattern = ".csv$", full.names = TRUE) %>%
  lapply(read_csv) %>% 
  bind_rows

# Selecting specific columns from GBIF data
gbif <- gbif[, c("countryCode", "family", "genus", "species", "decimalLongitude", "decimalLatitude")]

# Adding source and reference columns to the GBIF dataset
gbif$source <- rep("gbif", times = nrow(gbif))
gbif$reference <- rep("gbif", times = nrow(gbif))
head(gbif)

# Loading literature data from CSV files in the specified directory
literature <- list.files(path = "./Data/raw_data/literature_raw_data/", pattern = ".csv$", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows 

# Selecting specific columns from literature data and adding a source column
literature <- literature[, c("countryCode", "family", "genus", "species", "decimalLongitude", "decimalLatitude", "reference")]
literature$source <- rep("literature", times = nrow(literature))
literature <- literature[, c(1, 2, 3, 4, 5, 6, 8, 7)]
head(literature)

# Merging the GBIF and literature datasets and replacing missing species with "sp"
conc <- bind_rows(gbif, literature) %>%
  replace_na(list(species = "sp")) %>%
  write.csv("./Data/raw_data/concatenated_raw_data/concatenated_raw_data.csv", row.names = FALSE)

# Cleaning the concatenated data
# Specify the path to save the cleaned dataset
Finalnam <- "./Data/clean_data/concatenated_clean_data.csv"

# Reading concatenated data and cleaning it
occurrences <- read.csv("./Data/raw_data/concatenated_raw_data/concatenated_raw_data.csv")
colnames(occurrences) <- c("countryCode", "family", "genus", "species", "longitude", "latitude", "source", "reference")

# Creating a unique code for each occurrence and removing duplicates
occurrences$code <- paste(occurrences$countryCode, occurrences$family, occurrences$genus, occurrences$species, 
                          occurrences$longitude, occurrences$latitude, occurrences$source, occurrences$reference, sep = "_")
occurrences <- occurrences[!duplicated(occurrences$code), 1:9]
occurrences <- na.omit(occurrences[, 1:8])
occurrences <- occurrences[occurrences$longitude != 0 & occurrences$latitude != 0, ]  # Removing records with zero coordinates
write.csv(occurrences, Finalnam, row.names = FALSE)

# Cropping occurrences to the area of interest using Dryflor shapefile
setwd()  # Specify your working directory
dryflor <- vect("seasonally_dryfo_dis.shp")
plot(dryflor)

# Reading cleaned occurrences and converting to spatial vector
occ_clean <- read.csv("./Data/clean_data/concatenated_clean_data.csv")
occ_clean <- vect(occ_clean, geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
points(occ_clean)

# Cropping occurrences to Dryflor area
occ_dryflor <- crop(occ_clean, dryflor)
points(occ_dryflor, col = "darkgreen")

# Saving cropped occurrences as a shapefile and CSV
writeVector(occ_dryflor, "STDF_bees_dryflor.shp", overwrite = TRUE)
df_dryflor <- as.data.frame(occ_dryflor)
df_dryflor <- cbind(crds(occ_dryflor), df_dryflor)
colnames(df_dryflor)[c(1, 2)] <- c("longitude", "latitude")
total_occ_dryflor <- df_dryflor[, c(3, 1, 2, 4, 5, 6, 7, 8)]
head(total_occ_dryflor)
write.csv(total_occ_dryflor[, 6], "./Data/name_validation/dryflor_names.csv", row.names = FALSE)

# Cropping occurrences to another area of interest using WWF TDF shapefile
setwd()  # Specify your working directory
teow <- vect("wwf_TDF_neotropics_dis.shp")
plot(teow)

# Re-reading cleaned occurrences and cropping to TEOW area
occ_clean <- read.csv("./Data/clean_data/concatenated_clean_data.csv")
occ_clean <- vect(occ_clean, geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
points(occ_clean)

occ_teow <- crop(occ_clean, teow)
points(occ_teow, col = "yellow")

# Saving cropped occurrences as a shapefile and CSV
writeVector(occ_teow, "STDF_bees_teow.shp", overwrite = TRUE)
df_teow <- as.data.frame(occ_teow)
df_teow <- cbind(crds(occ_teow), df_teow)
colnames(df_teow)[c(1, 2)] <- c("longitude", "latitude")
total_occ_teow <- df_teow[, c(3, 1, 2, 4, 5, 6, 7, 8)]
head(total_occ_teow)
write.csv(total_occ_teow[, 6], "./Data/name_validation/teow_names.csv", row.names = FALSE)
