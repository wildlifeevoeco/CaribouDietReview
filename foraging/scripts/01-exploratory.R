

library(data.table)
library(ggplot2)


df <- fread("input/foraging.csv")

df <- df[!is.na(season)]

df[, mean(forbs, na.rm = T), by = "season"]

aa <- data.table(rbind(df[,mean(lichen, na.rm = T), by = "season"], 
                 df[,mean(graminoid, na.rm = T), by = "season"], 
                 df[,mean(forbs, na.rm = T), by = "season"], 
                 df[,mean(shrubs, na.rm = T), by = "season"], 
                 df[,mean(horsetail, na.rm = T), by = "season"], 
                 df[,mean(tree, na.rm = T), by = "season"], 
                 df[,mean(moss, na.rm = T), by = "season"], 
                 df[,mean(fungi, na.rm = T), by = "season"]),
                 #df[,mean(other, na.rm = T), by = "season"]),
           plant = c(rep(c("lichen"), 6),
                     rep(c("graminoid"), 6), 
                     rep(c("forbs"), 6), 
                     rep(c("shrubs"), 6), 
                     rep(c("horsetail"), 6), 
                     rep(c("tree"), 6), 
                     rep(c("moss"), 6), 
                     rep(c("fungi"), 6)))


aa$season[aa$season == "calving"] <- "1-Calving"
aa$season[aa$season == "summer"] <- "2-Summer"
aa$season[aa$season == "rut"] <- "3-Rut"
aa$season[aa$season == "fall"] <- "4-Autumn"
aa$season[aa$season == "winter"] <- "5-winter"
aa$season[aa$season == "spring"] <- "6-spring"


ggplot(aa, 
       aes(plant,V1,fill=plant)) + 
  geom_bar(stat = "identity", na.rm=TRUE) +
  ylab("% of diet") +
  theme(#legend.position = c(0.1, 0.875),
        legend.key = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_blank(),
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black", angle = 45, hjust = 1), 
        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~season)


png("figures/fig1.png", width = 6000, height = 4500, units = "px", res = 600)
ggplot(aa, 
       aes(season,V1,fill=season)) + 
  geom_bar(stat = "identity", na.rm=TRUE) +
  ylab("% of diet") +
  theme(#legend.position = c(0.1, 0.875),
    legend.key = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    axis.title = element_text(size=18),
    axis.text.y = element_text(size=12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black", angle = 90, hjust = 1), 
    strip.text  = element_text(size = 16),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~plant)
dev.off()

unique(df$caribou_type)

bb <- data.table(rbind(df[,mean(lichen, na.rm = T), by = "Subspecies"], 
                       df[,mean(graminoid, na.rm = T), by = "Subspecies"], 
                       df[,mean(forbs, na.rm = T), by = "Subspecies"], 
                       df[,mean(shrubs, na.rm = T), by = "Subspecies"], 
                       df[,mean(horsetail, na.rm = T), by = "Subspecies"], 
                       df[,mean(tree, na.rm = T), by = "Subspecies"], 
                       df[,mean(moss, na.rm = T), by = "Subspecies"], 
                       df[,mean(fungi, na.rm = T), by = "Subspecies"]),
                 plant = c(rep(c("lichen"), 7),
                           rep(c("graminoid"), 7), 
                           rep(c("forbs"), 7), 
                           rep(c("shrubs"), 7), 
                           rep(c("horsetail"), 7), 
                           rep(c("tree"), 7), 
                           rep(c("moss"), 7), 
                           rep(c("fungi"), 7)))

ggplot(bb, 
       aes(Subspecies,V1,fill=Subspecies)) + 
  geom_bar(stat = "identity", na.rm=TRUE) +
  ylab("% of diet") +
  theme(#legend.position = c(0.1, 0.875),
    legend.key = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    axis.title = element_text(size=18),
    axis.text.y = element_text(size=12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black", angle = 90, hjust = 1), 
    strip.text  = element_text(size = 16),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~plant)

cc <- data.table(rbind(df[,mean(lichen, na.rm = T), by = c("caribou_herd", "latitude", "season")], 
                         df[,mean(graminoid, na.rm = T), by = c("caribou_herd", "latitude", "season")], 
                         df[,mean(forbs, na.rm = T), by = c("caribou_herd", "latitude", "season")], 
                         df[,mean(shrubs, na.rm = T), by = c("caribou_herd", "latitude", "season")], 
                         df[,mean(horsetail, na.rm = T), by = c("caribou_herd", "latitude", "season")], 
                         df[,mean(tree, na.rm = T), by = c("caribou_herd", "latitude", "season")], 
                         df[,mean(moss, na.rm = T), by = c("caribou_herd", "latitude", "season")], 
                         df[,mean(fungi, na.rm = T), by = c("caribou_herd", "latitude", "season")]),
                   plant = c(rep(c("lichen"),82),
                             rep(c("graminoid"), 82), 
                             rep(c("forbs"), 82), 
                             rep(c("shrubs"), 82), 
                             rep(c("horsetail"), 82), 
                             rep(c("tree"), 82), 
                             rep(c("moss"), 82), 
                             rep(c("fungi"), 82)))

ggplot(cc[plant == "shrubs"], aes(latitude, V1, color = plant), ) +
  geom_jitter(width = 1) +
  geom_smooth(method = "gam") +
  ylab("% Item in diet") + 
  theme(#legend.position = c(0.1, 0.875),
    legend.key = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size=18),
    axis.text.y = element_text(size=12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"), 
    strip.text  = element_text(size = 16),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1))
  facet_wrap(~season)
