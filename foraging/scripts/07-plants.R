
library(vegan)
library(data.table)
library(tidyr)

## load data sources
so <- fread("input/data_source.csv")
so <- so[, .N, by = c("data_collection", "authors", "year", "included")]
so[, .N, by = "data_collection"]
so <- so[included == 1]
so$author_yr <- as.factor(paste(so$authors, so$year, sep = "_"))


## load plant data
plants <- fread("input/plants.csv")
setnames(plants, "study", "authors")

plants$author_yr <- as.factor(paste(plants$authors, plants$year, sep = "_"))

plants <- plants[type_plant != "animal_parts" & 
                 type_plant != "litter"]

plants$type_plant[plants$type_plant == "rush"] <- "other"
plants$type_plant[plants$type_plant == "liverwort"] <- "other"
plants$type_plant[plants$type_plant == "rhizome"] <- "other"
plants$type_plant[plants$type_plant == "sedge"] <- "other"

## load season data 
sub <- fread("output/clean-data.csv")

## merge plants database with restricted list of paper used for analyses
plants <- merge(plants, so[,c("author_yr", "data_collection", "included", "N")], by = "author_yr")

unique(plants$author_yr)

## get rid of NAs
plants$plant_species[plants$plant_species == ""] <- "no_species"
plants <- plants[!is.na(plants$plant_species)]

## split into genus and species
plants[, c("genus", "species") := tstrsplit(plant_species, "_", fixed=TRUE)] 

## number of unique plant species
length(unique(plants$plant_species)) - 1 ## minus 1 for the "no species" label

## number of unique plant genera
length(unique(plants$genus)) - 1 ## minus 1 for the "no species" label

## number of unique articles
length(unique(plants$author_yr))

## number of genera per plant type
plants[plant_species != "no_species"][, uniqueN(genus), by = "type_plant"]

## number plant species per article
common <- plants[plant_species != "no_species"][, .N, by = c("genus", "type_plant")]

common[, .N, by = "type_plant"]

## top plants
length(unique(plants[genus == "Salix"]$author_yr))
length(unique(plants[genus == "Salix"]$species))
length(unique(plants[genus == "Equisetum"]$author_yr))
length(unique(plants[genus == "Equisetum"]$species))
length(unique(plants[genus == "Cladonia"]$author_yr))
length(unique(plants[genus == "Cladonia"]$species))
length(unique(plants[genus == "Dryas"]$author_yr))
length(unique(plants[genus == "Dryas"]$species))
length(unique(plants[genus == "Vaccinium"]$author_yr))
length(unique(plants[genus == "Vaccinium"]$species))


## breakdown of plants by taxonony
plant_freq <- plants[, .N, by = c("type_plant")]
plant_freq$prop <- plant_freq$N/length(plants$authors)

chisq.test(plant_freq$N)

## unique number of species per plant type
plants[, uniqueN(species), by = "type_plant"]

length(unique(plants[type_plant == "moss"]$author_yr))
length(unique(plants[type_plant == "fungi"]$author_yr))
length(unique(plants[type_plant == "tree"]$author_yr))


