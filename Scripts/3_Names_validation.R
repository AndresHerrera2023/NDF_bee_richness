# Load required libraries
library(terra)  
library(dplyr)

# Set the working directory to the project's folder
setwd("./NDF_bees_project/")

# Process data for the DRYFLOR bee occurrences

# Load the spatial dataset for DRYFLOR occurrences
dryflor_occ <- vect("./Shapefiles/STDF_bees_occ/dryflor/STDF_bees_dryflor.shp") 
plot(dryflor_occ)  # Plot the occurrences

# Convert the spatial data to a data frame and add longitude/latitude columns
df <- as.data.frame(dryflor_occ)
df <- cbind(crds(dryflor_occ), df)
colnames(df)[c(1, 2)] <- c("longitude", "latitude")

# Reorganize and preview the data
total_occ <- df[, c(3, 1, 2, 4, 5, 6, 7, 8)]
head(total_occ)

# Add Bolivia (countryCode == 'BO') to the dataset.
BO <- nrow(total_occ[total_occ$countryCode == 'BO', ])

# Validate species names using ITIS data
valid_names <- read.csv("./Data/name_validation/valid_names_dryflor.csv")
valid_names <- subset(valid_names, NameUsage == "invalid")  # Select invalid names
valid_names <- valid_names[, c(1, 3)]  # Keep species and accepted names

# Merge occurrences with valid names
occ_names <- merge(total_occ, valid_names, by = "species", all.x = TRUE)

# Replace species names with accepted names where applicable
occ_names$species <- ifelse(is.na(occ_names$Accepted.Name), occ_names$species, occ_names$Accepted.Name)

# Remove the 'Accepted.Name' column
total_occ_species <- subset(occ_names, select = -Accepted.Name)

# Load manually edited names for species without ITIS matches
manual <- read.csv("./Data/name_validation/manual_names_dryflor.csv")

# Merge occurrences with manual edits
occ_manual_names <- merge(total_occ_species, manual, by = "species", all.x = TRUE)

# Replace species names with manually edited names
occ_manual_names$species <- ifelse(is.na(occ_manual_names$species_final), occ_manual_names$species, occ_manual_names$species_final)

# Remove unnecessary columns and reorganize data
final_names <- subset(occ_manual_names, select = -species_final)
final_names <- final_names[, c(3, 4, 1, 5, 6, 2, 7, 8)]
final_names <- final_names %>%
  filter(species != "remove")  # Filter out entries marked as 'remove'

# Rename columns and save all occurrences to CSV
colnames(final_names)[3] <- "Species"
write.csv(final_names, "./Data/STDF_bees_occ/dryflor/all_occ/all_occ_dryflor.csv", row.names = FALSE)

# Filter data to genus level and save
genera <- final_names %>%
  select(longitude, latitude, genus, family, countryCod, source, reference)
write.csv(genera, "./Data/STDF_bees_occ/dryflor/genera/genera_occ_dryflor.csv", row.names = FALSE)

# Filter data to species level and save
species <- final_names %>%
  select(longitude, latitude, Species, genus, family, countryCod, source, reference) %>%
  filter(Species != "sp")
write.csv(species, "./Data/STDF_bees_occ/dryflor/species/species_occ_dryflor.csv", row.names = FALSE)

# Process data for the TEOW area

# Load the spatial dataset for TEOW occurrences
teow_occ <- vect("./Shapefiles/STDF_bees_occ/teow/STDF_bees_teow.shp")
plot(teow_occ)  # Plot the occurrences

# Convert the spatial data to a data frame and add longitude/latitude columns
df <- as.data.frame(teow_occ)
df <- cbind(crds(teow_occ), df)
colnames(df)[c(1, 2)] <- c("longitude", "latitude")

# Reorganize and preview the data
total_occ <- df[, c(3, 1, 2, 4, 5, 6, 7, 8)]
head(total_occ)

# Validate species names using ITIS data
valid_names <- read.csv("./Data/name_validation/valid_names_teow.csv")
valid_names <- subset(valid_names, NameUsage == "invalid")  # Select invalid names
valid_names <- valid_names[, c(1, 3)]  # Keep species and accepted names

# Merge occurrences with valid names
occ_names <- merge(total_occ, valid_names, by = "species", all.x = TRUE)

# Replace species names with accepted names where applicable
occ_names$species <- ifelse(is.na(occ_names$Accepted.Name), occ_names$species, occ_names$Accepted.Name)

# Remove the 'Accepted.Name' column
total_occ_species <- subset(occ_names, select = -Accepted.Name)

# Load manually edited names for species without ITIS matches
manual <- read.csv("./Data/name_validation/manual_names_teow.csv")

# Merge occurrences with manual edits
occ_manual_names <- merge(total_occ_species, manual, by = "species", all.x = TRUE)

# Replace species names with manually edited names
occ_manual_names$species <- ifelse(is.na(occ_manual_names$species_final), occ_manual_names$species, occ_manual_names$species_final)

# Remove unnecessary columns and reorganize data
final_names <- subset(occ_manual_names, select = -species_final)
final_names <- final_names[, c(3, 4, 1, 5, 6, 2, 7, 8)]
final_names <- final_names %>%
  filter(species != "remove")  # Filter out entries marked as 'remove'

# Rename columns and save all occurrences to CSV
colnames(final_names)[3] <- "Species"
write.csv(final_names, "./Data/STDF_bees_occ/teow/all_occ/all_occ_teow.csv", row.names = FALSE)

# Filter data to genus level and save
genera <- final_names %>%
  select(longitude, latitude, genus, family, countryCod, source, reference)
write.csv(genera, "./Data/STDF_bees_occ/teow/genera/genera_occ_teow.csv", row.names = FALSE)

# Filter data to species level and save
species <- final_names %>%
  select(longitude, latitude, Species, genus, family, countryCod, source, reference) %>%
  filter(Species != "sp")
write.csv(species, "./Data/STDF_bees_occ/teow/species/species_occ_teow.csv", row.names = FALSE)
