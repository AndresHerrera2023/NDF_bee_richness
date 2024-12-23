# Summary richness, proportion of species with ID and in protected areas per country

# Load required library
library(dplyr)

# Set working directory
setwd("./NDF_bees_project/")

## DRYFLOR data analysis

# Load DRYFLOR occurrences dataset
total_occ <- read.csv("./Data/STDF_bees_occ/dryflor/all_occ/all_occ_dryflor.csv")
# Rearrange columns for easier processing
total_occ <- total_occ[, c(4,5,3,6,1,2,7,8)]
head(total_occ) # Display the first few rows

# Total number of occurrences
n_total_occ <- nrow(total_occ) # 22871
n_total_occ

# Total number of occurrences from GBIF
n_total_occ_gbif <- nrow(total_occ[total_occ$source == 'gbif',]) # 21295
n_total_occ_gbif

# Total number of occurrences from literature
n_total_occ_literature <- nrow(total_occ[total_occ$source == 'literature',]) # 1576
n_total_occ_literature

# Total number of unique named species
n_unique_species <- total_occ[total_occ$Species != "sp",]
length(unique(n_unique_species[,3])) # 1841 unique species

# Total number of unique genera
length(unique(total_occ[,2])) # 251 genera

# Total number of named occurrences (non-"sp")
number_named_species <- nrow(total_occ[total_occ$Species != "sp",]) # 18075
number_named_species

# Total number of unnamed species ("sp")
number_sp <- nrow(total_occ[total_occ$Species == "sp",]) # 4796
number_sp

# Number of occurrences per family
fam_occ <- total_occ %>%
  group_by(family) %>% 
  summarise(total_occ = length(Species))
fam_occ

# Number of genera per family
fam_genus <- total_occ %>%
  group_by(family) %>% 
  summarise(genus = n_distinct(genus))
fam_genus

# Number of species per family (excluding "sp")
n_unique_species_nosp <- total_occ[total_occ$Species != "sp",]
fam_species <- n_unique_species_nosp %>%
  group_by(family) %>% 
  summarise(Species = n_distinct(Species))
fam_species

# Proportion of occurrences per family
prop_fam <- round((fam_species[,2] / sum(fam_species[,2]) * 100), digits=2)
cbind(fam_species[,1], prop_fam)

# Proportion of identified occurrences per family
sp_occ_no_sp <- total_occ[total_occ$Species != "sp",] %>%
  group_by(family) %>% 
  summarise(species = length(Species))
sp_occ_sp <- total_occ %>%
  group_by(family) %>% 
  summarise(Proportion = length(Species))
prop_spfam <- round((sp_occ_no_sp[,2] / sp_occ_sp[,2] * 100), digits=2)
colnames(prop_spfam) <- "prop ID"
prop_spfam

# Summary of family-level diversity
summary_family <- cbind(fam_genus, fam_species[,2], fam_occ[,2], prop_spfam)
summary_family

# Save summary to a CSV file
write.csv(summary_family, "./Data/Summary_diversity/dryflor/summary_family_dryflor.csv")

# Total occurrences per country
occ_country <- total_occ %>%
  group_by(countryCod) %>% 
  summarise(total = length(countryCod))
occ_country

# Number of species per country (excluding "sp")
species_country <- total_occ[total_occ$Species != "sp",] %>%
  group_by(countryCod) %>%
  summarise(Species = n_distinct(Species)) %>%
  arrange(countryCod)
species_country

# Number of genera per country
genus_country <- total_occ %>%
  group_by(countryCod) %>%
  summarise(genus = n_distinct(genus)) %>%
  arrange(countryCod)
genus_country

# Number of identified occurrences per country
occ_id <- total_occ[total_occ$Species != "sp",] %>%
  group_by(countryCod) %>%
  summarise(total = length(countryCod))
occ_id

# Proportion of identified occurrences per country
prop <- round((occ_id[,2] / occ_country[,2] * 100), digits=2)
colnames(prop) <- "prop ID"
prop

# Species in protected areas
occ_protected_areas <- read.csv("./Data/Protected_areas/species_protected_areas_dryflor.csv")
occ_protected_areas <- occ_protected_areas[occ_protected_areas$Species != "sp",]

# Summarize species in protected areas by country
occ_protected <- occ_protected_areas %>%
  group_by(countryCod) %>%
  summarise(species = n_distinct(Species)) %>%
  arrange(countryCod)
occ_protected
colnames(occ_protected)[2] <- "Species"

# Add missing countries with no protected species
GT <- data.frame(countryCod = "GT", Species = 0)
pos <- 9
occ_protected_areas <- rbind(occ_protected[1:(pos-1), ], GT, occ_protected[pos:nrow(occ_protected), ])
occ_protected_areas

# Proportion of protected species
prop_protected <- round((occ_protected_areas[,2] / species_country[,2] * 100), digits=2)
colnames(prop_protected) <- "prop_protected"
prop_protected

# Summary of richness per country
summary_per_country <- cbind(species_country, genus_country[,2], occ_country[,2], prop, prop_protected)
summary_per_country

# Save country-level summary to CSV
write.csv(summary_per_country, "./Data/Summary_diversity/dryflor/summary_per_country_dryflor.csv")

##############__________TEOW______________###########

# Load TEOW occurrences dataset
total_occ_teow <- read.csv("./Data/STDF_bees_occ/teow/all_occ/all_occ_teow.csv")
# Rearrange columns for easier processing
total_occ_teow <- total_occ_teow[, c(4,5,3,6,1,2,7,8)]
head(total_occ_teow) # Display the first few rows

# Total number of occurrences
n_total_occ_teow <- nrow(total_occ_teow) # 18093
n_total_occ_teow

# Total number of occurrences from GBIF
n_total_occ_gbif_teow <- nrow(total_occ_teow[total_occ_teow$source == 'gbif',]) # 16943
n_total_occ_gbif_teow

# Total number of occurrences from literature
n_total_occ_literature_teow <- nrow(total_occ_teow[total_occ_teow$source == 'literature',]) # 1150
n_total_occ_literature_teow

# Total number of unique named species
n_unique_species_teow <- total_occ_teow[total_occ_teow$Species != "sp",]
length(unique(n_unique_species_teow[,3])) # 1567 unique species

# Total number of unique genera
length(unique(total_occ_teow[,2])) # 233 genera

# Total number of named occurrences (non-"sp")
number_named_species_teow <- nrow(total_occ_teow[total_occ_teow$Species != "sp",]) # 14786
number_named_species_teow

# Total number of unnamed species ("sp")
number_sp_teow <- nrow(total_occ_teow[total_occ_teow$Species == "sp",]) # 3307
number_sp_teow

# Number of occurrences per family
fam_occ_teow <- total_occ_teow %>%
  group_by(family) %>%
  summarise(total_occ = length(Species))
fam_occ_teow

# Number of genera per family
fam_genus_teow <- total_occ_teow %>%
  group_by(family) %>%
  summarise(genus = n_distinct(genus))
fam_genus_teow

# Number of species per family (excluding "sp")
n_unique_species_nosp_teow <- total_occ_teow[total_occ_teow$Species != "sp",]
fam_species_teow <- n_unique_species_nosp_teow %>%
  group_by(family) %>%
  summarise(Species = n_distinct(Species))
fam_species_teow

# Proportion of occurrences per family
prop_fam_teow <- round((fam_species_teow[,2] / sum(fam_species_teow[,2]) * 100), digits=2)
cbind(fam_species_teow[,1], prop_fam_teow)

# Proportion of identified occurrences per family
sp_occ_no_sp_teow <- total_occ_teow[total_occ_teow$Species != "sp",] %>%
  group_by(family) %>%
  summarise(species = length(Species))
sp_occ_sp_teow <- total_occ_teow %>%
  group_by(family) %>%
  summarise(Proportion = length(Species))
prop_spfam_teow <- round((sp_occ_no_sp_teow[,2] / sp_occ_sp_teow[,2] * 100), digits=2)
colnames(prop_spfam_teow) <- "prop ID"
prop_spfam_teow

# Summary of family-level diversity
summary_family_teow <- cbind(fam_genus_teow, fam_species_teow[,2], fam_occ_teow[,2], prop_spfam_teow)
summary_family_teow

# Save family-level summary to CSV
write.csv(summary_family_teow, "./Data/Summary_diversity/teow/summary_family_teow.csv")

# Total occurrences per country
occ_country_teow <- total_occ_teow %>%
  group_by(countryCod) %>%
  summarise(total = length(countryCod))
occ_country_teow

# Number of species per country (excluding "sp")
species_country_teow <- total_occ_teow[total_occ_teow$Species != "sp",] %>%
  group_by(countryCod) %>%
  summarise(Species = n_distinct(Species)) %>%
  arrange(countryCod)
species_country_teow

# Number of genera per country
genus_country_teow <- total_occ_teow %>%
  group_by(countryCod) %>%
  summarise(genus = n_distinct(genus)) %>%
  arrange(countryCod)
genus_country_teow

# Number of identified occurrences per country
occ_id_teow <- total_occ_teow[total_occ_teow$Species != "sp",] %>%
  group_by(countryCod) %>%
  summarise(total = length(countryCod))
occ_id_teow

# Proportion of identified occurrences per country
prop_teow <- round((occ_id_teow[,2] / occ_country_teow[,2] * 100), digits=2)
colnames(prop_teow) <- "prop ID"
prop_teow

# Species in protected areas
p_occ_teow <- read.csv("./Data/Protected_areas/species_protected_areas_teow.csv")
occ_protected_areas_teow <- p_occ_teow %>%
  group_by(countryCod) %>%
  summarise(species = n_distinct(Species)) %>%
  arrange(countryCod)

# Add missing countries with no protected species
no_protected <- data.frame(countryCod = c("AI", "GD", "GT", "HN", "MS", "PY"), species = c(rep(0, times = 6)))
occ_protected_areas_teow <- rbind(occ_protected_areas_teow, no_protected)
occ_protected_areas_teow <- occ_protected_areas_teow[order(occ_protected_areas_teow$countryCod),]

# Proportion of protected species
prop_protected_teow <- round((occ_protected_areas_teow[,2] / species_country_teow[,2]) * 100, digits=2)
colnames(prop_protected_teow) <- "prop_protected"
prop_protected_teow

# Summary of richness per country
summary_per_country_teow <- cbind(species_country_teow, genus_country_teow[,2], occ_country_teow[,2], prop_teow, prop_protected_teow)
summary_per_country_teow

# Save country-level summary to CSV
write.csv(summary_per_country_teow, "./Data/Summary_diversity/teow/summary_per_country_teow.csv")
