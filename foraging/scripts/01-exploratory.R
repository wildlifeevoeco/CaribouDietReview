

library(data.table)

## data source
so <- fread("input/data_source.csv")
so <- so[, .N, by = c("data_collection", "authors", "year", "included")]
so[, .N, by = "data_collection"]
so <- so[included == 1]

## Input foraging data
df <- fread("input/foraging.csv")

df <- df[authors != "Denryter et al." & 
          authors != "White and Trudell" &
          authors != "McNeill et al." & 
          authors != "Thompson et al"]

############################## 
########## SEASON ############
############################## 

df <- df[!is.na(season)]

df[, mean(forbs, na.rm = T), by = "season"]

df$season[df$season == "calving"] <- "1-Calving"
df$season[df$season == "summer"] <- "2-Summer"
df$season[df$season == "rut"] <- "3-Autumn"
df$season[df$season == "fall"] <- "3-Autumn"
df$season[df$season == "winter"] <- "4-Winter"
df$season[df$season == "spring"] <- "5-Spring"

aa <- data.table(rbind(df[,mean(lichen, na.rm = T), by = "season"], 
                 df[,mean(graminoid, na.rm = T), by = "season"], 
                 df[,mean(forbs, na.rm = T), by = "season"], 
                 df[,mean(shrubs, na.rm = T), by = "season"], 
                 df[,mean(horsetail, na.rm = T), by = "season"], 
                 df[,mean(tree, na.rm = T), by = "season"], 
                 df[,mean(moss, na.rm = T), by = "season"], 
                 df[,mean(fungi, na.rm = T), by = "season"]),
                 rbind(df[,sd(lichen, na.rm = T), by = "season"], 
                 df[,sd(graminoid, na.rm = T), by = "season"], 
                 df[,sd(forbs, na.rm = T), by = "season"], 
                 df[,sd(shrubs, na.rm = T), by = "season"], 
                 df[,sd(horsetail, na.rm = T), by = "season"], 
                 df[,sd(tree, na.rm = T), by = "season"], 
                 df[,sd(moss, na.rm = T), by = "season"], 
                 df[,sd(fungi, na.rm = T), by = "season"]),
           plant = c(rep(c("lichen"), 5),
                     rep(c("graminoid"), 5), 
                     rep(c("forbs"), 5), 
                     rep(c("shrubs"), 5), 
                     rep(c("horsetail"), 5), 
                     rep(c("tree"), 5), 
                     rep(c("moss"), 5), 
                     rep(c("fungi"), 5)))

aa <- aa[,2:5]
colnames(aa) <- c("meanDiet", "season", "sdDiet", "plant")

fwrite(aa, "output/season.csv")

############################## 
########## SUBSPECIES #########
############################## 

unique(df$Subspecies)

df$Subspecies[df$Subspecies == "Peary"] <- "1-Peary"
df$Subspecies[df$Subspecies == "Greenland"] <- "2-Barren Ground"
df$Subspecies[df$Subspecies == "barren-ground"] <- "2-Barren Ground"
df$Subspecies[df$Subspecies == "mountaine"] <- "3-Mountain"
df$Subspecies[df$Subspecies == "woodland"] <- "4-Woodland"
df$Subspecies[df$Subspecies == "reindeer"] <- "5-Reindeer"
df$Subspecies[df$Subspecies == "introduced-reindeer"] <- "6-Introduced-Reindeer"

df <- df[,c("lichen", "graminoid", "shrubs", "data type", "latitude",
               "sympatric_ungulates", "aug_daily_average",
                 "Subspecies", "season","authors")]

fwrite(df, "output/clean-data.csv")

bb <- data.table(rbind(df[,mean(lichen, na.rm = T), by = "Subspecies"], 
                       df[,mean(graminoid, na.rm = T), by = "Subspecies"], 
                       df[,mean(forbs, na.rm = T), by = "Subspecies"], 
                       df[,mean(shrubs, na.rm = T), by = "Subspecies"], 
                       df[,mean(horsetail, na.rm = T), by = "Subspecies"], 
                       df[,mean(tree, na.rm = T), by = "Subspecies"], 
                       df[,mean(moss, na.rm = T), by = "Subspecies"], 
                       df[,mean(fungi, na.rm = T), by = "Subspecies"]),
                 rbind(df[,sd(lichen, na.rm = T), by = "Subspecies"], 
                       df[,sd(graminoid, na.rm = T), by = "Subspecies"], 
                       df[,sd(forbs, na.rm = T), by = "Subspecies"], 
                       df[,sd(shrubs, na.rm = T), by = "Subspecies"], 
                       df[,sd(horsetail, na.rm = T), by = "Subspecies"], 
                       df[,sd(tree, na.rm = T), by = "Subspecies"], 
                       df[,sd(moss, na.rm = T), by = "Subspecies"], 
                       df[,sd(fungi, na.rm = T), by = "Subspecies"]),
                 plant = c(rep(c("lichen"), 6),
                           rep(c("graminoid"), 6), 
                           rep(c("forbs"), 6), 
                           rep(c("shrubs"), 6), 
                           rep(c("horsetail"), 6), 
                           rep(c("tree"), 6), 
                           rep(c("moss"), 6), 
                           rep(c("fungi"), 6)))

bb <- bb[,2:5]
colnames(bb) <- c("meanDiet", "subspecies", "sdDiet", "plant")

fwrite(bb, "output/subspecies-summary.csv")

############################## 
########## LATITUDE ##########
############################## 


cc <- data.table(rbind(df[,mean(lichen, na.rm = T), by = c("caribou_herd", "latitude", "longitude", "season")], 
                         df[,mean(graminoid, na.rm = T), by = c("caribou_herd", "latitude", "longitude","season")], 
                         df[,mean(forbs, na.rm = T), by = c("caribou_herd", "latitude", "longitude","season")], 
                         df[,mean(shrubs, na.rm = T), by = c("caribou_herd", "latitude", "longitude","season")], 
                         df[,mean(horsetail, na.rm = T), by = c("caribou_herd", "latitude", "longitude","season")], 
                         df[,mean(tree, na.rm = T), by = c("caribou_herd", "latitude", "longitude","season")], 
                         df[,mean(moss, na.rm = T), by = c("caribou_herd", "latitude", "longitude","season")], 
                         df[,mean(fungi, na.rm = T), by = c("caribou_herd", "latitude", "longitude", "season")]),
                   plant = c(rep(c("lichen"),81),
                             rep(c("graminoid"), 81), 
                             rep(c("forbs"), 81), 
                             rep(c("shrubs"), 81), 
                             rep(c("horsetail"), 81), 
                             rep(c("tree"), 81), 
                             rep(c("moss"), 81), 
                             rep(c("fungi"), 81)))

colnames(cc) <- c("herd", "latitude", "longitude",  "season", "meanDiet", "plant")

fwrite(cc, file = "output/latitude.csv")
