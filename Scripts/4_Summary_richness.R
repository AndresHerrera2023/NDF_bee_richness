### Code: Summary richness 
### Project: Neotropical dry forest bees
### Author: Andres Herrera

#Libraries:
library(dplyr)

#Occurrences from DRYFLOR:
total_occ_dryflor <- read.csv("./NDF_bees_project/Data/STDF_bees_occ/dryflor/all/dryflor_bees_all_occ.csv")
head(total_occ_dryflor)

#Total number of occurrences:
n_total_occurrences<-nrow(total_occ_dryflor) #17875
n_total_occ

#Total number of occurrences from GBIF:
n_total_occ_gbif <-  nrow(total_occ_dryflor[total_occ_dryflor$source == 'gbif',]) #16299
n_total_occ_gbif

#Total number of occurrences from literature:
n_total_occ_literature <-  nrow(total_occ_dryflor[total_occ_dryflor$source == 'literature',]) #1576
n_total_occ_literature

#Total number of unique named species:
n_unique_species <- total_occ_dryflor[total_occ_dryflor$species != "sp",]
length(unique(n_unique_species[,6]))#species =  1848

#Total number of unique named genera:
length(unique(total_occ_dryflor[,5])) #genus = 252

#Total number of named occurrences:
number_named_species <- nrow(total_occ_dryflor[total_occ_dryflor$species != "sp",]) #13402
number_named_species

#Total number of "sp" or unnamed species:
number_sp <- nrow(total_occ_dryflor[total_occ_dryflor$species == "sp",]) #4473
number_sp

#Number of occurrences per family:
fam_occurrences<- total_occ_dryflor %>%
  group_by(family) %>% 
  summarise(total_occurrences= length(species))

fam_occ

#Number of genera per family:
fam_genus <- total_occ_dryflor %>%
  group_by(family) %>% 
  summarise(genus = n_distinct(genus))
fam_genus

#Number of species per family (no sp):
n_unique_species_nosp <- total_occ_dryflor[total_occ_dryflor$species != "sp",]
fam_species <- n_unique_species_nosp  %>%
  group_by(family) %>% 
  summarise(Species = n_distinct(species))
fam_species <- rbind(fam_species, data.frame(
  family = "Melittidae",
  Species = 0,
  stringsAsFactors = FALSE
))

prop_fam <- round((fam_species[,2] /sum(fam_species[,2]) * 100), digits=2)
cbind(fam_species[,1],prop_fam)

#Proportion of occurrences per family with ID:
sp_occ_no_sp <- total_occ_dryflor[total_occ_dryflor$species != "sp",] %>%
  group_by(family) %>% 
  summarise(species = length(species))

sp_occ_no_sp <- rbind(sp_occ_no_sp, data.frame(
  family = "Melittidae",
  species = 0,
  stringsAsFactors = FALSE
))


sp_occ_sp <- total_occ_dryflor %>%
  group_by(family) %>% 
  summarise(Proportion = length(species))

prop_spfam <- round((sp_occ_no_sp[,2] /sp_occ_sp[,2] * 100), digits=2)
colnames(prop_spfam) <- "prop ID"
prop_spfam

summary_family <-cbind(fam_genus, fam_species[,2], fam_occ[,2],prop_spfam)
summary_family

write.csv(summary_family, "Z:/Andres/NDF_bees_project/summary_diversity/dryflor/summary_per_family_dryflor.csv", row.names = F)

#Total number of occurrencesper country:
occ_country <- total_occ_dryflor %>%
  group_by(countryCode) %>% 
  summarise(total = length(countryCode))
occ_country

#Number of Species per country:
species_country <-total_occ_dryflor[total_occ_dryflor$species != "sp",] %>% 
  group_by(countryCode) %>%   
  summarise(Species = n_distinct(species))  %>%   
  arrange(countryCode)
species_country 

#Number of genera per country:
genus_country <-total_occ_dryflor %>% 
  group_by(countryCode) %>% 
  summarise(genus = n_distinct(genus))  %>%   
  arrange(countryCode)
genus_country 

#Number of occurences with ID per country:
occ_id <- total_occ_dryflor[total_occ_dryflor$species != "sp",] %>%
  group_by(countryCode) %>% 
  summarise(total = length(countryCode))
occ_id  

#Proportion ID species per country:
prop_id <- round((occ_id[,2] /occ_country[,2] * 100), digits=2)
colnames(prop_id) <- "prop ID"
prop_id

##Species in protected areas:
occ_dryflor_pa <- read.csv("Z:/Andres/NDF_bees_project/summary_diversity/dryflor/dryflor_occ_protected_areas.csv")
occ_dryflor_pa <- occ_dryflor_pa[occ_dryflor_pa$species != "sp",]

prop_pa_dryflor <- (length(unique(occ_dryflor_pa[,6]))/length(unique(n_unique_species[,6])))*100 #46.53% sp in protected areas
prop_pa_dryflor 

occ_dryflor_pa <- occ_dryflor_pa %>% 
  group_by(countryCode) %>% 
  summarise(species = n_distinct(species))  %>%   
  arrange(countryCode)
occ_dryflor_pa
colnames(occ_dryflor_pa)[2]<- "species"
occ_dryflor_pa

GT <- data.frame(countryCode = "GT", species = 0)
JM <- data.frame(countryCode = "JM", species = 0)
SV <- data.frame(countryCode = "SV", species = 0)

occ_dryflor_pa <- rbind(occ_dryflor_pa[1:8,],GT,
                             occ_dryflor_pa[9:10,], JM,
                             occ_dryflor_pa[11:16, ],SV,
                             occ_dryflor_pa[17:nrow(occ_dryflor_pa),])

View(occ_dryflor_pa)

prop_dryflor_pa <- round((occ_dryflor_pa[,2] / species_country[,2] * 100), digits=2)
colnames(occ_dryflor_pa)[2] <- "prop_protected"
prop_dryflor_pa

#Summary richness:
summary_per_country_dryflor <- cbind(species_country, genus_country[,2],occ_country[,2], prop_id, prop_dryflor_pa)
summary_per_country_dryflor

write.csv(summary_per_country_dryflor, "Z:/Andres/NDF_bees_project/summary_diversity/dryflor/summary_per_country_dryflor.csv", row.names = F)

##############__________TEOW______________###########

#Data:
total_occ_teow <- read.csv("Z:/Andres/NDF_bees_project/Data/STDF_bees_occ/teow/all/teow_bees_all_occ.csv")
head(total_occ_teow)
nrow(total_occ_teow)

#Total number of occurrences:
n_total_occ_teow <-nrow(total_occ_teow) #13595
n_total_occ_teow

#Total number of occurrences from GBIF:
n_total_occ_gbif_teow <-  nrow(total_occ_teow[total_occ_teow$source == 'gbif',]) #11938
n_total_occ_gbif_teow

#Total number of occurrences from literature:
n_total_occ_literature_teow <-  nrow(total_occ_teow[total_occ_teow$source == 'literature',]) #1657
n_total_occ_literature_teow

#Total number of unique named species:
n_unique_species_teow <- total_occ_teow[total_occ_teow$species != "sp",]
length(unique(n_unique_species_teow[,6]))#species =  1406

#Total number of unique named genera:
length(unique(total_occ_teow[,5])) #genus = 208

#Total number of named occurences:
number_named_species_teow <- nrow(total_occ_teow[total_occ_teow$species != "sp",]) #10447
number_named_species_teow

#Total number of "sp" or unnamed species:
number_sp_teow <- nrow(total_occ_teow[total_occ_teow$species == "sp",]) #3148
number_sp_teow

#Number of occurrences per family:
fam_occ_teow <- total_occ_teow %>%
  group_by(family) %>% 
  summarise(total_occurrences= length(species))

fam_occ_teow 

#Number of genus per family:
fam_genus_teow <- total_occ_teow %>%
  group_by(family) %>% 
  summarise(genus = n_distinct(genus))
fam_genus_teow

#Number of species per family (no sp):
n_unique_species_nosp <- total_occ_teow[total_occ_teow$species != "sp",]
fam_species_teow <- n_unique_species_nosp  %>%
  group_by(family) %>% 
  summarise(species = n_distinct(species))
fam_species_teow

fam_species_teow <- rbind(fam_species_teow, data.frame(
  family = "Melittidae",
  species = 0,
  stringsAsFactors = FALSE))
fam_species_teow

prop_fam_teow <- round((fam_species_teow[,2] /sum(fam_species_teow[,2]) * 100), digits=2)
prop_fam_teow
cbind(fam_species_teow[,1],prop_fam_teow)

#Proportion of occurrences per family with ID:
sp_occ_no_sp_teow <- total_occ_teow[total_occ_teow$species != "sp",] %>%
  group_by(family) %>% 
  summarise(species = length(species))
sp_occ_no_sp_teow <- rbind(sp_occ_no_sp_teow, data.frame(
  family = "Melittidae",
  species = 0,
  stringsAsFactors = FALSE))
sp_occ_no_sp_teow 

sp_occ_sp_teow <- total_occ_teow %>%
  group_by(family) %>% 
  summarise(Proportion = length(species))

prop_spfam_teow <- round((sp_occ_no_sp_teow[,2] /sp_occ_sp_teow[,2] * 100), digits=2)
colnames(prop_spfam_teow) <- "prop ID"
prop_spfam_teow

summary_family_teow <-cbind(fam_genus_teow, fam_species_teow[,2], fam_occ_teow[,2],prop_spfam_teow)
summary_family_teow

write.csv(summary_family_teow, "Z:/Andres/NDF_bees_project/summary_diversity/teow/summary_per_family_teow.csv", row.names = F)

#Total number of occurrences per country:
occ_country_teow <- total_occ_teow %>%
  group_by(countryCode) %>% 
  summarise(total = length(countryCode))
occ_country_teow
nrow(occ_country_teow)
View(occ_country_teow)

#Number of Species per country:
species_country_teow <-total_occ_teow[total_occ_teow$species != "sp",] %>% 
  group_by(countryCode) %>%   
  summarise(species = n_distinct(species))  %>%   
  arrange(countryCode)
species_country_teow 
View(species_country_teow)


GD <- data.frame(countryCode = "GD", species = 0)
species_country_teow <- rbind(species_country_teow[1:7,],GD,
                              species_country_teow[8:nrow(species_country_teow),])


nrow(species_country_teow)
View(species_country_teow)

#Number of genera per country:
genus_country_teow <-total_occ_teow %>% 
  group_by(countryCode) %>% 
  summarise(genus = n_distinct(genus))  %>%   
  arrange(countryCode)
genus_country_teow
nrow(genus_country_teow)
View(genus_country_teow)

#Number of occurrences with ID per country:
occ_id_teow <- total_occ_teow[total_occ_teow$species != "sp",] %>%
  group_by(countryCode) %>% 
  summarise(total = length(countryCode))
occ_id_teow
View(occ_id_teow)
GD <- data.frame(countryCode = "GD", total = 0)
occ_id_teow <- rbind(occ_id_teow[1:7,],GD,
                     occ_id_teow[8:nrow(occ_id_teow),])
nrow(occ_id_teow)
View(occ_id_teow)

#Proportion ID species per country:
prop_teow <- round((occ_id_teow[,2] /occ_country_teow[,2] * 100), digits=2)
colnames(prop_teow) <- "prop ID"
prop_teow

##Species in protected areas:
p_occ_teow <- read.csv("Z:/Andres/NDF_bees_project/summary_diversity/teow/teow_occ_protected_areas.csv")

prop_pa_teow <- (length(unique(p_occ_teow[,6]))/length(unique(n_unique_species_teow[,6])))*100 # 38.90
prop_pa_teow

occ_protected_areas_teow <- p_occ_teow %>% 
  group_by(countryCode) %>% 
  summarise(species = n_distinct(species))  %>%   
  arrange(countryCode)
occ_protected_areas_teow 

#Adding countries with 0 species:
GD <- data.frame(countryCode = "GD", species = 0, stringsAsFactors = FALSE)
GT <- data.frame(countryCode = "GT", species = 0, stringsAsFactors = FALSE)
HN <- data.frame(countryCode = "HN", species = 0, stringsAsFactors = FALSE)
LC <- data.frame(countryCode = "LC", species = 0, stringsAsFactors = FALSE)
MS <- data.frame(countryCode = "MS", species = 0,stringsAsFactors = FALSE)
PA <- data.frame(countryCode = "PA", species = 0,stringsAsFactors = FALSE)
TT <- data.frame(countryCode = "TT", species = 0,stringsAsFactors = FALSE)

occ_protected_areas_teow <- rbind(occ_protected_areas_teow[1:7,],GD, GT, HN,
                                  occ_protected_areas_teow[8:9,], LC, MS, 
                                  occ_protected_areas_teow[10:11,], PA, 
                                  occ_protected_areas_teow[12:14,],TT,
                                  occ_protected_areas_teow[15,])

occ_protected_areas_teow 
View(occ_protected_areas_teow)
write.csv(occ_protected_areas_teow, "Z:/Andres/NDF_bees_project/summary_diversity/teow/summary_teow_occ_protected_areas.csv", row.names = F)

prop_protected_teow <- round((occ_protected_areas_teow[,2] /species_country_teow[,2] * 100), digits=2)
colnames(prop_protected_teow) <- "prop_protected"
prop_protected_teow

#Summary richness:
summary_per_country_teow <- cbind(species_country_teow, genus_country_teow[,2],occ_country_teow[,2], prop_teow, prop_protected_teow)
summary_per_country_teow

write.csv(summary_per_country_teow, "Z:/Andres/NDF_bees_project/Summary_diversity/teow/summary_per_country_teow.csv", row.names = F)
