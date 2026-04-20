# ============================================================
# Script: Protected areas overlap for Neotropical dry forest bees
# Author: Herrera-Motta et al.
# Description:
# This script:
# 1. Loads WDPA protected area polygons
# 2. Crops them to the Neotropics
# 3. Merges all cropped WDPA layers
# 4. Crops protected areas to the DRYFLOR and TEOW delimitations
# 5. Extracts bee occurrence records located within protected areas
# 6. Exports the resulting occurrence tables as CSV files
# ============================================================

library(terra)

# ------------------------------------------------------------
# 1. Load WDPA shapefiles
# ------------------------------------------------------------
# These are the WDPA polygon layers split into three separate files
WDPA_shp_1 <- vect("./Shapefiles/WDPA_2026/WDPA_Mar2026_Public_shp_0/WDPA_Mar2026_Public_shp-polygons.shp")
WDPA_shp_2 <- vect("./Shapefiles//WDPA_2026/WDPA_Mar2026_Public_shp_1/WDPA_Mar2026_Public_shp-polygons.shp")
WDPA_shp_3 <- vect("./Shapefiles/WDPA_2026/WDPA_Mar2026_Public_shp_2/WDPA_Mar2026_Public_shp-polygons.shp")

# ------------------------------------------------------------
# 2. Load Neotropics shapefile
# ------------------------------------------------------------
# This shapefile is used to restrict the WDPA dataset to the
# Neotropical region only
neotropics <- vect("./Shapefiles/America/Neotropics_croped.shp")

# ------------------------------------------------------------
# 3. Crop each WDPA layer to the Neotropics
# ------------------------------------------------------------
# This reduces the size of the WDPA dataset and keeps only the
# protected areas that overlap the Neotropical region
shp1_neotropics <- crop(WDPA_shp_1, neotropics)
shp2_neotropics <- crop(WDPA_shp_2, neotropics)
shp3_neotropics <- crop(WDPA_shp_3, neotropics)

# ------------------------------------------------------------
# 4. Merge the cropped WDPA layers
# ------------------------------------------------------------
# Combine the three cropped WDPA objects into a single layer
WDPA_merged <- rbind(
  shp1_neotropics,
  shp2_neotropics,
  shp3_neotropics
)

# Visual inspection of the merged protected areas layer
plot(WDPA_merged)

# ------------------------------------------------------------
# 5. Load the DRYFLOR and TEOW shapefiles
# ------------------------------------------------------------
# These shapefiles represent two alternative delimitations of
# seasonally dry tropical forests
dryflor <- vect("./Shapefiles/STDF/dryflor/seasonally_dryfo_dis.shp")
teow    <- vect("./Shapefiles/STDF/teow/teow.shp")

# ------------------------------------------------------------
# 6. Crop protected areas to the DRYFLOR extent
# ------------------------------------------------------------
# Keep only WDPA polygons that overlap the DRYFLOR delimitation
WDPA_dryflor <- crop(WDPA_merged, dryflor)

# Plot the resulting protected areas within DRYFLOR
plot(WDPA_dryflor)

# Save the DRYFLOR protected areas shapefile
writeVector(
  WDPA_dryflor,
  "./Shapefiles/WDPA/dryflor/WDPA_dryflor.shp"
)

# ------------------------------------------------------------
# 7. Crop protected areas to the TEOW extent
# ------------------------------------------------------------
# Keep only WDPA polygons that overlap the TEOW delimitation
WDPA_teow <- crop(WDPA_merged, teow)

# Plot the resulting protected areas within TEOW
plot(WDPA_teow)

# Save the TEOW protected areas shapefile
writeVector(
  WDPA_teow,
  "./Shapefiles/WDPA/teow/WDPA_teow.shp"
)

# ============================================================
# 8. Extract DRYFLOR occurrence records inside protected areas
# ============================================================

# Load protected areas cropped to DRYFLOR
pa_dryflor <- vect("./Shapefiles/WDPA/dryflor/WDPA_dryflor.shp")

# Load cleaned bee occurrences for DRYFLOR
dryflor_occ <- vect("./Shapefiles/STDF_bees_occ/clean_names/dryflor/dryflor_clean_occ.shp")

# Crop occurrence points to the protected areas layer
# This keeps only records located inside or overlapping the
# extent of protected areas
dryflor_occ_pa <- crop(dryflor_occ, pa_dryflor)

# Plot DRYFLOR occurrences within protected areas
plot(dryflor_occ_pa)

# Convert the spatial object to a data frame
df_dryflor_occ_pa <- as.data.frame(dryflor_occ_pa)

# Add longitude and latitude coordinates to the table
df_dryflor_occ_pa <- cbind(crds(dryflor_occ_pa), df_dryflor_occ_pa)

# Rename coordinate columns
colnames(df_dryflor_occ_pa)[c(1, 2)] <- c("longitude", "latitude")

# Reorder columns
df_dryflor_occ_pa <- df_dryflor_occ_pa[, c(3, 1, 2, 4, 5, 6, 7, 8, 9)]

head(df_dryflor_occ_pa)
nrow(df_dryflor_occ_pa)  # 2579, 13099

# Save occurrence records within DRYFLOR protected areas
write.csv(
  df_dryflor_occ_pa,
  "./Data/occ_protected_areas/dryflor/dryflor_occ_PA.csv",
  row.names = FALSE
)

# ============================================================
# 9. Extract TEOW occurrence records inside protected areas
# ============================================================

# Load protected areas cropped to TEOW
pa_teow <- vect("./Shapefiles/WDPA/teow/WDPA_teow.shp")

# Load cleaned bee occurrences for TEOW
teow_occ <- vect("./Shapefiles/STDF_bees_occ/clean_names/teow/teow_clean_occ.shp")

# Crop occurrence points to the protected areas layer
# This keeps only records located inside or overlapping the
# extent of protected areas
teow_occ_pa <- crop(teow_occ, pa_teow)

# Plot TEOW occurrences within protected areas
plot(teow_occ_pa)

# Convert the spatial object to a data frame
df_teow_occ_pa <- as.data.frame(teow_occ_pa)

# Add longitude and latitude coordinates to the table
df_teow_occ_pa <- cbind(crds(teow_occ_pa), df_teow_occ_pa)

# Rename coordinate columns
colnames(df_teow_occ_pa)[c(1, 2)] <- c("longitude", "latitude")

# Reorder columns
df_teow_occ_pa <- df_teow_occ_pa[, c(3, 1, 2, 4, 5, 6, 7, 8, 9)]

head(df_teow_occ_pa)
nrow(df_teow_occ_pa)  # 2579, 2175, 11026

# Save occurrence records within TEOW protected areas
write.csv(
  df_teow_occ_pa,
  "./Data/occ_protected_areas/teow/teow_occ_PA.csv",
  row.names = FALSE
)
