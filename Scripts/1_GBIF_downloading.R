### 1. Download GBIF occurrences ###

#Set the Working directory and the folder where the occurrences data will be stored
setwd(".../NTDF_bees_paper/Data/gbif_raw_data")

# Installing the following required packages and call the libraries
install.packages("rgbif")
library(rgbif)

# Get GBIF keys for each bee family in the Neotropical region
Apidae <- name_backbone(name="Apidae", rank="family") $ usageKey  
Halictidae <- name_backbone(name="Halictidae", rank="family") $ usageKey  
Megachilidae <- name_backbone(name="Megachilidae", rank="family") $ usageKey  
Colletidae <- name_backbone(name="Colletidae", rank="family") $ usageKey  
Andrenidae <- name_backbone(name="Andrenidae", rank="family") $ usageKey  

# List GADM_ids for each country were the STDF is present
GADM_ids <-c("ANT","ARG","BHS","BLZ","BOL","BRA","CHL","COL","CRI","CUB","DMA","DOM",
             "ECU","SLV","GUF","GTM","GUY","HTI","HND","JAM","MEX","NIC","PAN","PRY",
             "PER","PRI","SUR","TTO","URY","VEN","AIA","ATG","CYM","GRD","MTQ","MSR",
             "KNA","LCA","VCT","TCA")																																																																																																																																																																																																																	

# Perform a download for your desired taxon and retrieve a download key. 
# You can set download filter parameters using pred_in and pred functions

#Apidae
gbif_download_key = occ_download(
  pred("taxonKey", Apidae), 
  pred_in("gadm",GADM_ids),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = "", 
  pwd = "", 
  email = ""
)

d_apidae <- occ_download_get('0280009-220831081235567') %>%
  occ_download_import()
write.csv(d_apidae, "Apidae.csv", row.names = FALSE)

#Halictidae
gbif_download_key = occ_download(
  pred("taxonKey", Halictidae), 
  pred_in("gadm",GADM_ids),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = "",
  pwd = "", 
  email = ""
)

d_halictidae <- occ_download_get('0280024-220831081235567') %>%  
write.csv(d_halictidae, "Halictidae.csv", row.names = FALSE)

#Megachilidae
gbif_download_key = occ_download(
  pred("taxonKey", Megachilidae), 
  pred_in("gadm",GADM_ids),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = "",
  pwd = "", 
  email = ""
  )

d_megachilidae <- occ_download_get('0282439-220831081235567') %>%
  occ_download_import()
write.csv(d_megachilidae, "Megachilidae.csv", row.names = FALSE)

#Colletidae
gbif_download_key = occ_download(
  pred("taxonKey", Colletidae), 
  pred_in("gadm",GADM_ids),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = "",
  pwd = "", 
  email = ""
  )

d_colletidae <- occ_download_get('0282446-220831081235567') %>%
  occ_download_import()
write.csv(d_colletidae, "Colletidae.csv", row.names = FALSE)

#Andrenidae
gbif_download_key = occ_download(
  pred("taxonKey", Andrenidae),
  pred_in("gadm",GADM_ids),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = "",
  pwd = "", 
  email = ""
  )

d_andrenidae <- occ_download_get('0282454-220831081235567') %>%
  occ_download_import()
write.csv(d_andrenidae, "Andrenidae.csv", row.names = FALSE)

# This download may take some time to finish. Check its status with this command. 
occ_download_wait(gbif_download_key)

# Once the download has finished, read your data into R. 
data_download <- occ_download_get(gbif_download_key, overwrite = TRUE) %>%
  occ_download_import()



