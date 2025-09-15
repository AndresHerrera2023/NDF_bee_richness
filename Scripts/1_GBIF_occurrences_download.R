### Code: Download GBIF data
### Project: Neotropical dry forest bees
### Authors: Herrera-Motta et al. 
### Last update: 09/11/25

#Set your working directory
setwd("./NDF_bees_project/Data/Raw/GBIF/")

#Required libraries:
library(rgbif)

# Get GBIF keys for family
Apidae <- name_backbone(name="Apidae", rank="family") $ usageKey  #Add your taxon
Halictidae <- name_backbone(name="Halictidae", rank="family") $ usageKey  
Megachilidae <- name_backbone(name="Megachilidae", rank="family") $ usageKey  
Colletidae <- name_backbone(name="Colletidae", rank="family") $ usageKey  
Andrenidae <- name_backbone(name="Andrenidae", rank="family") $ usageKey  
Melittidae <- name_backbone(name="Melittidae", rank="family") $ usageKey  

# List GADM_GID for each state or country we need data for:
GADM_ids <-c("ARG", "BHS", "BLZ", "BOL", "BRA", "BRB", "CHL", "COL", "CRI", "CUB", "DMA", "DOM",
  "ECU", "SLV", "GUF", "GTM", "GUY", "HTI", "HND", "JAM", "MEX", "NIC", "PAN", "PRY",
  "PER", "PRI", "SUR", "TTO", "URY", "VEN", "AIA", "ATG", "CYM", "GRD", "MTQ", "MSR",
  "KNA", "LCA", "VCT", "TCA")

# Perform a download for your desired taxon and retrieve a download key. 
# You can set download filter parameters using pred_in and pred functions

#APIDAE
gbif_download_key = occ_download(
  pred("taxonKey", Apidae), # insert taxon key for the taxon interested in
  pred_in("gadm",GADM_ids),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = "", 
  pwd = "", 
  email = ""
)
print(gbif_download_key)


d_apidae <- occ_download_get("0011676-250402121839773") %>%
  occ_download_import()
write.csv(d_apidae, "Apidae.csv", row.names = FALSE)

#HALICTIDAE
gbif_download_key = occ_download(
  pred("taxonKey", Halictidae), # insert taxon key for the taxon interested in
  pred_in("gadm",GADM_ids),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = "", 
  pwd = "", 
  email = ""
)

print(gbif_download_key)

d_halictidae <- occ_download_get("0011699-250402121839773") %>%    
  occ_download_import()
write.csv(d_halictidae, "Halictidae.csv", row.names = FALSE)

#MEGACHILIDAE
gbif_download_key = occ_download(
  pred("taxonKey", Megachilidae), # insert taxon key for the taxon interested in
  pred_in("gadm",GADM_ids),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = "", 
  pwd = "", 
  email = ""
)

print(gbif_download_key)

d_megachilidae <- occ_download_get("0011716-250402121839773") %>%
  occ_download_import()
write.csv(d_megachilidae, "Megachilidae.csv", row.names = FALSE)

#COLLETIDAE
gbif_download_key = occ_download(
  pred("taxonKey", Colletidae), # insert taxon key for the taxon interested in
  pred_in("gadm",GADM_ids),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = "", 
  pwd = "", 
  email = ""
)
print(gbif_download_key)

d_colletidae <- occ_download_get("0011780-250402121839773") %>%
  occ_download_import()
write.csv(d_colletidae, "Colletidae.csv", row.names = FALSE)


#ANDRENIDAE
gbif_download_key = occ_download(
  pred("taxonKey", Andrenidae), # insert taxon key for the taxon interested in
  pred_in("gadm",GADM_ids),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = "", 
  pwd = "", 
  email = ""
)

print(gbif_download_key)

d_andrenidae <- occ_download_get("0011831-250402121839773") %>%
  occ_download_import()
write.csv(d_andrenidae, "Andrenidae.csv", row.names = FALSE)


# Once the download has finished, read your data into R. 
data_download <- occ_download_get(gbif_download_key, overwrite = TRUE) %>%
  occ_download_import()
