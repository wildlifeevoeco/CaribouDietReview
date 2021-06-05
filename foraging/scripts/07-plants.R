

library(data.table)

## load data sources
so <- fread("input/data_source.csv")
so <- so[, .N, by = c("data_collection", "authors", "year", "included")]
so[, .N, by = "data_collection"]
so <- so[included == 1]

## load plant data
plants <- fread("input/plants.csv")
setnames(plants, "study", "authors")

plants <- plants[type_plant != "animal_parts" & 
                 type_plant != "litter"]

plants$type_plant[plants$type_plant == "rush"] <- "other"
plants$type_plant[plants$type_plant == "liverwort"] <- "other"
plants$type_plant[plants$type_plant == "rhizome"] <- "other"
plants$type_plant[plants$type_plant == "sedge"] <- "other"


## merge plants database with restricted list of paper used for analyses
plants <- merge(plants, so[,c("authors", "data_collection", "included", "N")], by = "authors")

unique(plants$authors)

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
length(unique(plants$authors))

## number of genera per plant type
plants[plant_species != "no_species"][, uniqueN(genus), by = "type_plant"]

### NMDS 
plants <- plants[plant_species != "no_species"]
plants_long <- plants[, uniqueN(plant_species), by = c("authors", "plant_species")]
data_wide <- spread(plants_long, plant_species, V1)
data_wide[is.na(data_wide)] <- 0

com = data_wide[,2:ncol(data_wide)]
m_com = as.matrix(com)

library(vegan)
nmds = metaMDS(m_com, distance = "bray")
nmds
plot(nmds)


## number plant species per article
common <- plants[plant_species != "no_species"][, .N, by = c("genus")]

## top plants
length(unique(plants[genus == "Salix"]$authors))
length(unique(plants[genus == "Salix"]$species))
length(unique(plants[genus == "Equisetum"]$authors))
length(unique(plants[genus == "Equisetum"]$species))
length(unique(plants[genus == "Carex"]$authors))
length(unique(plants[genus == "Carex"]$species))
length(unique(plants[genus == "Dryas"]$authors))
length(unique(plants[genus == "Dryas"]$species))
length(unique(plants[genus == "Cetraria"]$authors))
length(unique(plants[genus == "Cetraria"]$species))


## breakdown of plants by taxonony
plant_freq <- plants[, .N, by = c("type_plant")]
plant_freq$prop <- plant_freq$N/length(plants$authors)

chisq.test(plant_freq$N)

## unique number of species per plant type
plants[, uniqueN(species), by = "type_plant"]


