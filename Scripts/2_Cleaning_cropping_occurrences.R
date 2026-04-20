# ============================================================
# Script: Occurrence data cleaning and validation
# Project: Neotropical dry forest bees
# Authors: Herrera-Motta et al.
# Last update: 2026-03-15
#
# Description:
# This script:
# 1. Merges raw occurrence records from GBIF and literature
# 2. Filters records by basis of record and catalog number
# 3. Removes duplicates, missing values, and invalid coordinates
# 4. Crops occurrences to two seasonally dry tropical forest datasets:
#    DRYFLOR and TEOW
# 5. Exports species names for taxonomic validation in ITIS
# 6. Replaces invalid names with accepted or manually curated names
# 7. Saves final cleaned occurrence datasets and shapefiles
# ============================================================

# ------------------------------------------------------------
# 1. Load required libraries
# ------------------------------------------------------------
library(rgbif)
library(dplyr)
library(readr)
library(tidyr)
library(terra)

# ------------------------------------------------------------
# 2. Set working directory
# ------------------------------------------------------------
# Update this path to match your local project directory
setwd("./NDF_bees_project/2026_version/improved/")

# ------------------------------------------------------------
# 3. Read and merge raw GBIF occurrence files
# ------------------------------------------------------------
# Import all CSV files from the GBIF raw data folder and merge
# them into a single data frame
gbif_merge <- list.files(path = "./Data/Raw/GBIF/", pattern = ".csv$", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows()

head(gbif_merge)
nrow(gbif_merge)  # 875983 (now 902071)

# ------------------------------------------------------------
# 4. Filter GBIF occurrences
# ------------------------------------------------------------

# Keep only preserved specimens to exclude observations such as
# human observations or other non-vouchered records
gbif_basisOfRecord <- gbif_merge[gbif_merge$basisOfRecord == "PRESERVED_SPECIMEN", ]
nrow(gbif_basisOfRecord)  # 740713 (now 734148)

# Remove records with non-informative catalog numbers
gbif_catalogNumber <- gbif_basisOfRecord[
  gbif_basisOfRecord$catalogNumber != "NO APLICA" &
    gbif_basisOfRecord$catalogNumber != "NO DISPONIBLE", ]
nrow(gbif_catalogNumber)  # 736085 (now 729521)

# Remove records with missing catalog numbers
gbif_catalogNumber_NA <- gbif_catalogNumber[!is.na(gbif_catalogNumber$catalogNumber), ]
nrow(gbif_catalogNumber_NA)  # 696466 (now 689812)

# Keep only the variables needed for downstream analyses
gbif <- gbif_catalogNumber_NA[, c(
  "countryCode", "family", "genus", "species",
  "decimalLongitude", "decimalLatitude", "year"
)]

# Add source and citation fields
gbif$source <- rep("gbif", times = length(gbif$genus))
gbif$cite   <- rep("gbif", times = length(gbif$genus))

head(gbif)

# ------------------------------------------------------------
# 5. Read and prepare occurrence data from literature
# ------------------------------------------------------------
# Import all CSV files from the literature folder and merge them
papers <- list.files(path = "./Data/raw/Literature/", pattern = ".csv$", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows()

# Keep only relevant columns
papers <- papers[, c(
  "countryCode", "family", "genus", "species",
  "decimalLongitude", "decimalLatitude", "year", "cite"
)]

# Add source label
papers$source <- rep("literature", times = length(papers$genus))

# Reorder columns to match GBIF structure
papers <- papers[, c(1, 2, 3, 4, 5, 6, 7, 9, 8)]

head(papers)
nrow(papers)  # 2411 (now 2686)

# ------------------------------------------------------------
# 6. Merge GBIF and literature records
# ------------------------------------------------------------
# Replace missing species names with "sp" before merging
bind_rows(gbif, papers) %>%
  replace_na(list(species = "sp")) %>%
  write.csv("./Data/Raw/Merged/raw_merged_occ.csv", row.names = FALSE)

# ------------------------------------------------------------
# 7. Remove duplicates, missing values, and invalid records
# ------------------------------------------------------------
occurrences <- read.csv("./Data/Raw/Merged/raw_merged_occ.csv")
head(occurrences)
nrow(occurrences)  # 692498

# Output path for cleaned merged occurrences
Finalnam <- "./Data/Clean/Merged/clean_merged_occ.csv"  # create this folder first

# Standardize column names
colnames(occurrences) <- c(
  "countryCode", "family", "genus", "species",
  "longitude", "latitude", "year", "source", "cite"
)

# Filtering for required fields
occurrences_clean <- occurrences %>%
  filter(
    !is.na(countryCode),
    !is.na(family),
    !is.na(genus),
    !is.na(species),
    !is.na(longitude),
    !is.na(latitude)
  )

nrow(occurrences_clean)  # 675194

# Save cleaned merged occurrence table
write.csv(occurrences_clean, Finalnam, row.names = FALSE)

# ------------------------------------------------------------
# 8. Crop occurrences using the two STDF delimitations
# ------------------------------------------------------------

# Read cleaned occurrences and convert to a spatial vector
occ <- read.csv("./Data/Clean/Merged/clean_merged_occ.csv")
occ <- vect(occ, geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")

# ------------------------------------------------------------
# 8A. DRYFLOR delimitation
# ------------------------------------------------------------

# Read DRYFLOR shapefile
dryflor <- vect("./Shapefiles/STDF/DRYFLOR/seasonally_dryfo_dis.shp")

# Crop occurrences to the DRYFLOR polygon
occ_dryflor <- crop(occ, dryflor)

# Save cropped spatial data
writeVector(
  occ_dryflor,
  "./Shapefiles/STDF_bees_occ/dryflor/dryflor_occ.shp",
  overwrite = TRUE
)

# Convert cropped occurrences back to a data frame
df_dryflor <- as.data.frame(occ_dryflor)
df_dryflor <- cbind(crds(occ_dryflor), df_dryflor)

# Rename coordinate columns
colnames(df_dryflor)[c(1, 2)] <- c("longitude", "latitude")

# Reorder columns
df_dryflor <- df_dryflor[, c(3, 1, 2, 4, 5, 6, 7, 8, 9)]

head(df_dryflor)
nrow(df_dryflor)  # 122726

# Save DRYFLOR occurrences for name validation
write.csv(
  df_dryflor,
  "./Data/Names_validation/Unverified_names/dryflor/dryflor_occ.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 8B. TEOW delimitation
# ------------------------------------------------------------

# Read TEOW shapefile
teow <- vect("./Shapefiles/STDF/TEOW/teow.shp")

# Crop occurrences to the TEOW polygon
occ_teow <- crop(occ, teow)

# Save cropped spatial data
writeVector(
  occ_teow,
  "./Shapefiles/STDF_bees_occ/teow/teow_occ.shp",
  overwrite = TRUE
)

# Convert cropped occurrences back to a data frame
df_teow <- as.data.frame(occ_teow)
df_teow <- cbind(crds(occ_teow), df_teow)

# Rename coordinate columns
colnames(df_teow)[c(1, 2)] <- c("longitude", "latitude")

# Reorder columns
df_teow <- df_teow[, c(3, 1, 2, 4, 5, 6, 7, 8, 9)]

head(df_teow)
nrow(df_teow)  # 96275

# Save TEOW occurrences for name validation
write.csv(
  df_teow,
  "./Data/Names_validation/Unverified_names/teow/teow_occ.csv",
  row.names = FALSE
)

# ============================================================
# 9. Taxonomic validation workflow: DRYFLOR
# ============================================================

# ------------------------------------------------------------
# 9A. Export unique species names for ITIS validation
# ------------------------------------------------------------
df_dryflor <- read.csv("./Data/Names_validation/Unverified_names/dryflor/dryflor_occ.csv")

spnames <- df_dryflor %>%
  filter(species != "sp") %>%
  distinct(species) %>%
  pull(species)

# Add a header label required for export
spnames <- c("name", spnames)

# Save names as plain text for ITIS validation
writeLines(spnames, "./Data/Names_validation/Unverified_names/dryflor/dryflor_names.txt")

# ------------------------------------------------------------
# 9B. Replace invalid names using ITIS output
# ------------------------------------------------------------
valid_names <- read.csv("./Data/Names_validation/ITIS_verified_names/dryflor/dryflor_valid_names.csv")

# Keep only invalid names and their accepted replacements
valid_names <- subset(valid_names, NameUsage == "invalid")
valid_names <- valid_names[, c(1, 3)]

# Merge accepted names into occurrence table
df_dryflor <- merge(df_dryflor, valid_names, by = "species", all.x = TRUE)

# Replace invalid names with accepted names when available
df_dryflor$species <- ifelse(
  is.na(df_dryflor$AcceptedName),
  df_dryflor$species,
  df_dryflor$AcceptedName
)

# Remove temporary AcceptedName column
total_species_dryflor <- subset(df_dryflor, select = -AcceptedName)

# ------------------------------------------------------------
# 9C. Apply manually curated name corrections
# ------------------------------------------------------------
manual <- read.csv("./Data/Names_validation/ITIS_verified_names/dryflor/dryflor_manual_valid_names.csv")

# Merge manual corrections by species
occ_manual_names <- merge(total_species_dryflor, manual, by = "species", all.x = TRUE)

# Replace species names with manually curated final names
occ_manual_names$species <- ifelse(
  is.na(occ_manual_names$species_final),
  occ_manual_names$species,
  occ_manual_names$species_final
)

# Update genus when the corrected species name implies a different genus
new_genus <- sub(" .*", "", occ_manual_names$species)
occ_manual_names$genus <- ifelse(
  occ_manual_names$species != "sp" & new_genus != occ_manual_names$genus,
  new_genus,
  occ_manual_names$genus
)

# ------------------------------------------------------------
# 9D. Final cleanup of validated names
# ------------------------------------------------------------
final_names <- occ_manual_names %>%
  mutate(
    species = trimws(species),
    species = gsub("\u00A0", " ", species)
  ) %>%
  filter(tolower(species) != "remove") %>%
  mutate(
    species = ifelse(
      !grepl("\\s", species) & species != "sp",
      "sp",
      species
    )
  )

# Reorder columns
final_names <- final_names[, c(2, 3, 4, 5, 6, 1, 7, 8, 9)]

nrow(final_names)  # 17871 occ (now 21294) 122713

# Save final cleaned DRYFLOR occurrence table
write.csv(
  final_names,
  "./Data/STDF_bees_occ/dryflor/dryflor_bees_final_occ.csv",
  row.names = FALSE
)

# Convert to spatial object and save shapefile
dryflor_occ <- vect(final_names, geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
plot(dryflor_occ)

writeVector(
  dryflor_occ,
  "./Shapefiles/STDF_bees_occ/clean_names/dryflor/dryflor_clean_occ.shp",
  overwrite = TRUE
)

# ============================================================
# 10. Taxonomic validation workflow: TEOW
# ============================================================

# ------------------------------------------------------------
# 10A. Export unique species names for ITIS validation
# ------------------------------------------------------------
df_teow <- read.csv("./Data/Names_validation/Unverified_names/teow/teow_occ.csv")

spnames <- df_teow %>%
  filter(species != "sp") %>%
  distinct(species) %>%
  pull(species)

# Add a header label required for export
spnames <- c("name", spnames)

# Save names as plain text for ITIS validation
writeLines(spnames, "./Data/Names_validation/Unverified_names/teow/teow_names.txt")

# ------------------------------------------------------------
# 10B. Replace invalid names using ITIS output
# ------------------------------------------------------------
df_teow <- read.csv("./Data/Names_validation/Unverified_names/teow/teow_occ.csv")
valid_names <- read.csv("./Data/Names_validation/ITIS_verified_names/teow/teow_valid_names.csv")

# Keep only invalid names and their accepted replacements
valid_names <- subset(valid_names, NameUsage == "invalid")
valid_names <- valid_names[, c(1, 3)]

# Merge accepted names into occurrence table
df_teow <- merge(df_teow, valid_names, by = "species", all.x = TRUE)

# Replace invalid names with accepted names when available
df_teow$species <- ifelse(
  is.na(df_teow$AcceptedName),
  df_teow$species,
  df_teow$AcceptedName
)

# Remove temporary AcceptedName column
total_species_teow <- subset(df_teow, select = -AcceptedName)

# ------------------------------------------------------------
# 10C. Apply manually curated name corrections
# ------------------------------------------------------------
manual <- read.csv("./Data/Names_validation/ITIS_verified_names/teow/teow_manual_valid_names.csv")

# Merge manual corrections by species
occ_manual_names <- merge(total_species_teow, manual, by = "species", all.x = TRUE)

# Replace species names with manually curated final names
occ_manual_names$species <- ifelse(
  is.na(occ_manual_names$species_final),
  occ_manual_names$species,
  occ_manual_names$species_final
)

# Update genus when the corrected species name implies a different genus
new_genus <- sub(" .*", "", occ_manual_names$species)
occ_manual_names$genus <- ifelse(
  occ_manual_names$species != "sp" & new_genus != occ_manual_names$genus,
  new_genus,
  occ_manual_names$genus
)

# ------------------------------------------------------------
# 10D. Final cleanup of validated names
# ------------------------------------------------------------
final_names <- occ_manual_names %>%
  mutate(
    species = trimws(species),
    species = gsub("\u00A0", " ", species)
  ) %>%
  filter(tolower(species) != "remove") %>%
  mutate(
    species = ifelse(
      !grepl("\\s", species) & species != "sp",
      "sp",
      species
    )
  )

head(final_names)

# Reorder columns
final_names <- final_names[, c(2, 3, 4, 5, 6, 1, 7, 8, 9)]

# Extra safeguard to remove flagged records
final_names <- final_names %>%
  filter(species != "remove")

head(final_names)
nrow(final_names)  # 13598 (now 15663)

# Save final cleaned TEOW occurrence table
write.csv(
  final_names,
  "./Data/STDF_bees_occ/teow/teow_bees_final_occ.csv",
  row.names = FALSE
)

# Convert to spatial object and save shapefile
teow_occ <- vect(final_names, geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
plot(teow_occ)

writeVector(
  teow_occ,
  "./Shapefiles/STDF_bees_occ/clean_names/teow/teow_clean_occ.shp",
  overwrite = TRUE
)
