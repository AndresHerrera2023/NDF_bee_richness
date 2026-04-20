# ============================================================
# Script: Download bee occurrence data from GBIF
# Author: Andres F. Herrera-Motta
# Description:
# This script retrieves occurrence records for several bee
# families (Hymenoptera: Apoidea) from GBIF using the rgbif package.
# ============================================================

# Load required libraries
library(rgbif)
library(dplyr)  # required for the pipe operator %>%

# ------------------------------------------------------------
# 1. Retrieve GBIF taxonKeys for each bee family
# ------------------------------------------------------------
# These keys uniquely identify taxa in the GBIF database

Apidae       <- name_backbone(name = "Apidae",       rank = "family")$usageKey
Halictidae   <- name_backbone(name = "Halictidae",   rank = "family")$usageKey  
Megachilidae <- name_backbone(name = "Megachilidae", rank = "family")$usageKey  
Colletidae   <- name_backbone(name = "Colletidae",   rank = "family")$usageKey  
Andrenidae   <- name_backbone(name = "Andrenidae",   rank = "family")$usageKey  

# ------------------------------------------------------------
# 2. Define geographic scope using GADM country codes
# ------------------------------------------------------------
# Includes Latin America and Caribbean countries

GADM_ids <- c(
  "ARG","BHS","BLZ","BOL","BRA","BRB","CHL","COL","CRI","CUB","DMA","DOM",
  "ECU","SLV","GUF","GTM","GUY","HTI","HND","JAM","MEX","NIC","PAN","PRY",
  "PER","PRI","SUR","TTO","URY","VEN","AIA","ATG","CYM","GRD","MTQ","MSR",
  "KNA","LCA","VCT","TCA"
)

# ------------------------------------------------------------
# 3. Helper function to download GBIF data by taxon
# ------------------------------------------------------------
# This avoids repeating the same code for each family

download_gbif_data <- function(taxon_key, output_file, download_id) {
  
  # Request download from GBIF
  gbif_download_key <- occ_download(
    pred("taxonKey", taxon_key),       # Filter by taxon
    pred_in("gadm", GADM_ids),         # Filter by region
    pred("hasCoordinate", TRUE),       # Only records with coordinates
    pred("hasGeospatialIssue", FALSE), # Exclude problematic records
    format = "SIMPLE_CSV",
    
    # ⚠️ IMPORTANT: Never upload real credentials to GitHub
    user = "YOUR_USERNAME", 
    pwd = "YOUR_PASSWORD", 
    email = "YOUR_EMAIL"
  )
  
  print(gbif_download_key)
  
  # Retrieve and import the dataset using a known download ID
  data <- occ_download_get(download_id) %>%
    occ_download_import()
  
  # Save data to CSV file
  write.csv(data, output_file, row.names = FALSE)
}

# ------------------------------------------------------------
# 4. Download datasets for each bee family
# ------------------------------------------------------------

# Apidae
download_gbif_data(Apidae, "Apidae.csv", "0055711-260226173443078")

# Halictidae
download_gbif_data(Halictidae, "Halictidae.csv", "0055895-260226173443078")

# Megachilidae
download_gbif_data(Megachilidae, "Megachilidae.csv", "0055956-260226173443078")

# Colletidae
download_gbif_data(Colletidae, "Colletidae.csv", "0011780-250402121839773")

# Andrenidae
download_gbif_data(Andrenidae, "Andrenidae.csv", "0056225-260226173443078")

# ------------------------------------------------------------
# 5. Optional: retrieve last download using its key
# ------------------------------------------------------------
# This step can be used to re-import the most recent request

data_download <- occ_download_get(gbif_download_key, overwrite = TRUE) %>%
  occ_download_import()
