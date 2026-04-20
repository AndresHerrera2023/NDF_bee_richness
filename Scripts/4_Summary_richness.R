# ============================================================
# Script: Diversity summary and statistics for DRYFLOR and TEOW
# Project: Neotropical dry forest bees
# Author: [Your Name]
# Description:
# This script calculates diversity metrics (occurrences, species,
# genera, and proportions) at family and country levels, and
# evaluates representation within protected areas for both
# DRYFLOR and TEOW datasets.
# ============================================================
# Libraries:
library(dplyr)

setwd("./NDF_bees_project/improved/")

# ------------------------------------------------------------
# DRYFLOR
# ------------------------------------------------------------

# Occurrences from DRYFLOR:
total_occ_dryflor <- read.csv("./Data/STDF_bees_occ/dryflor/dryflor_bees_final_occ.csv")
head(total_occ_dryflor)

# Total number of occurrences:
n_total_occ_dryflor <- nrow(total_occ_dryflor)
n_total_occ_dryflor

# Total number of occurrences from GBIF:
n_total_occ_gbif <- nrow(total_occ_dryflor[total_occ_dryflor$source == 'gbif',])
n_total_occ_gbif

# Total number of occurrences from literature:
n_total_occ_literature <- nrow(total_occ_dryflor[total_occ_dryflor$source == 'literature',])
n_total_occ_literature

# Total number of unique named species:
n_unique_species <- total_occ_dryflor[total_occ_dryflor$species != "sp",]
length(unique(n_unique_species[,6]))

# Total number of unique named genera:
length(unique(total_occ_dryflor[,5]))

# Check genera assigned to multiple families:
total_occ_dryflor %>%
  group_by(genus) %>%
  summarise(n_familias = n_distinct(family)) %>%
  filter(n_familias > 1)

total_occ_dryflor %>%
  group_by(genus) %>%
  summarise(
    familias = paste(unique(family), collapse = ", "),
    n_familias = n_distinct(family)
  ) %>%
  filter(n_familias > 1)

# Total number of named occurrences:
number_named_species <- nrow(total_occ_dryflor[total_occ_dryflor$species != "sp",])
number_named_species

# Total number of "sp" or unnamed species:
number_sp <- nrow(total_occ_dryflor[total_occ_dryflor$species == "sp",])
number_sp

# Number of occurrences per family:
fam_occurrences <- total_occ_dryflor %>%
  group_by(family) %>%
  summarise(total_occurrences = length(species))
fam_occurrences

# Number of genera per family:
fam_genus <- total_occ_dryflor %>%
  group_by(family) %>%
  summarise(genus = n_distinct(genus))
fam_genus

# Number of species per family (excluding "sp"):
n_unique_species_nosp <- total_occ_dryflor[total_occ_dryflor$species != "sp",]
fam_species <- n_unique_species_nosp %>%
  group_by(family) %>%
  summarise(Species = n_distinct(species))
fam_species

# Proportion of species per family:
prop_fam <- round((fam_species[,2] / sum(fam_species[,2]) * 100), digits = 2)
cbind(fam_species[,1], prop_fam)

# Proportion of occurrences per family with ID:
sp_occ_no_sp <- total_occ_dryflor[total_occ_dryflor$species != "sp",] %>%
  group_by(family) %>%
  summarise(species = length(species))

sp_occ_sp <- total_occ_dryflor %>%
  group_by(family) %>%
  summarise(Proportion = length(species))

prop_spfam <- round((sp_occ_no_sp[,2] / sp_occ_sp[,2] * 100), digits = 2)
colnames(prop_spfam) <- "prop ID"
prop_spfam

# Summary per family:
summary_family <- cbind(fam_genus, fam_species[,2], fam_occurrences[,2], prop_spfam)
summary_family

write.csv(
  summary_family,
  "./NDF_bees_project/Data/summary_diversity/dryflor/summary_per_family_dryflor.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# Country-level summaries
# ------------------------------------------------------------

# Total occurrences per country:
occ_country <- total_occ_dryflor %>%
  group_by(countryCode) %>%
  summarise(total = length(countryCode))
occ_country

# Number of species per country:
species_country <- total_occ_dryflor[total_occ_dryflor$species != "sp",] %>%
  group_by(countryCode) %>%
  summarise(Species = n_distinct(species)) %>%
  arrange(countryCode)
species_country

# Number of genera per country:
genus_country <- total_occ_dryflor %>%
  group_by(countryCode) %>%
  summarise(genus = n_distinct(genus)) %>%
  arrange(countryCode)
genus_country

# Number of occurrences with ID per country:
occ_id <- total_occ_dryflor[total_occ_dryflor$species != "sp",] %>%
  group_by(countryCode) %>%
  summarise(total = length(countryCode))
occ_id

# Proportion of identified occurrences per country:
prop_id <- round((occ_id[,2] / occ_country[,2] * 100), digits = 2)
colnames(prop_id) <- "prop ID"
prop_id

# ------------------------------------------------------------
# Species in protected areas (DRYFLOR)
# ------------------------------------------------------------

occ_dryflor_pa <- read.csv("./Data/occ_protected_areas/dryflor/dryflor_occ_PA.csv")
occ_dryflor_pa <- occ_dryflor_pa[occ_dryflor_pa$species != "sp",]

prop_pa_dryflor <- (length(unique(occ_dryflor_pa[,6])) / length(unique(n_unique_species[,6]))) * 100
prop_pa_dryflor

occ_dryflor_pa <- occ_dryflor_pa %>%
  group_by(countryCod) %>%
  summarise(species = n_distinct(species)) %>%
  arrange(countryCod)

colnames(occ_dryflor_pa)[2] <- "species"

# Add missing countries with zero species:
GT <- data.frame(countryCod = "GT", species = 0)
JM <- data.frame(countryCod = "JM", species = 0)
LC <- data.frame(countryCod = "LC", species = 0)
PR <- data.frame(countryCod = "PR", species = 0)
SV <- data.frame(countryCod = "SV", species = 0)

occ_dryflor_pa <- rbind(
  occ_dryflor_pa[1:8,], GT,
  occ_dryflor_pa[9:10,], JM, LC,
  occ_dryflor_pa[11:13,], PR,
  occ_dryflor_pa[14,], SV,
  occ_dryflor_pa[15,]
)

# Proportion of species in protected areas:
prop_dryflor_pa <- round((occ_dryflor_pa[,2] / species_country[,2] * 100), digits = 2)
colnames(occ_dryflor_pa)[2] <- "prop_protected"
prop_dryflor_pa

# Summary per country:
summary_per_country_dryflor <- cbind(
  species_country,
  genus_country[,2],
  occ_country[,2],
  prop_id,
  prop_dryflor_pa
)

write.csv(
  summary_per_country_dryflor,
  "./Data/summary_diversity/dryflor/summary_per_country_dryflor.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# TEOW
# ------------------------------------------------------------

total_occ_teow <- read.csv("./Data/STDF_bees_occ/teow/teow_bees_final_occ.csv")
head(total_occ_teow)

# Total occurrences:
n_total_occ_teow <- nrow(total_occ_teow)
n_total_occ_teow

# Occurrences by source:
n_total_occ_gbif_teow <- nrow(total_occ_teow[total_occ_teow$source == 'gbif',])
n_total_occ_gbif_teow

n_total_occ_literature_teow <- nrow(total_occ_teow[total_occ_teow$source == 'literature',])
n_total_occ_literature_teow

# Unique species and genera:
n_unique_species_teow <- total_occ_teow[total_occ_teow$species != "sp",]
length(unique(n_unique_species_teow[,6]))

length(unique(total_occ_teow[,5]))

# Named vs unnamed:
number_named_species_teow <- nrow(total_occ_teow[total_occ_teow$species != "sp",])
number_named_species_teow

number_sp_teow <- nrow(total_occ_teow[total_occ_teow$species == "sp",])
number_sp_teow

# Family summaries:
fam_occ_teow <- total_occ_teow %>%
  group_by(family) %>%
  summarise(total_occurrences = length(species))

fam_genus_teow <- total_occ_teow %>%
  group_by(family) %>%
  summarise(genus = n_distinct(genus))

fam_species_teow <- total_occ_teow[total_occ_teow$species != "sp",] %>%
  group_by(family) %>%
  summarise(species = n_distinct(species))

# Continue unchanged logic...
