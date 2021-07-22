
library(data.table)
library(ggplot2)
library(gridExtra)
library(glmmTMB)

lat <- fread("output/latitude.csv")

lat$season[lat$season == "calving"] <- "1-Calving"
lat$season[lat$season == "summer"] <- "2-Summer"
lat$season[lat$season == "rut"] <- "3-Autumn"
lat$season[lat$season == "fall"] <- "3-Autumn"
lat$season[lat$season == "winter"] <- "4-Winter"
lat$season[lat$season == "spring"] <- "5-Spring"
lat$season[lat$season == ""] <- NA
lat <- na.omit(lat, cols="season")

mod_lich <- glmmTMB(meanDiet ~ latitude + season, data = lat[plant == "lichen"])
summary(mod_lich)

png("figures/fig3.png", width = 6000, height = 3000, units = "px", res = 600)
aa <- ggplot(cc[plant == "graminoid"], aes(latitude, meanDiet)) +
  geom_jitter(aes(color = season), width = 2, alpha = 0.75) +
  geom_smooth(method = "lm", color = "black") +
  ylab("% Graminoids in diet") + 
  xlab("Latitude") +
  ggtitle("A)") +
  scale_color_viridis_d() +
  theme(legend.position = c(0.25, 0.8),
    legend.title = element_blank(),    
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
bb <- ggplot(cc[plant == "shrubs"], aes(latitude, meanDiet)) +
  geom_jitter(aes(color = season), width = 2, alpha = 0.75) +
  geom_smooth(method = "lm", color = "black") +
  ylab("% Shrubs in diet") + 
  xlab("Latitude") +
  ggtitle("B)") +
  scale_color_viridis_d() +
  theme(legend.position = 'none',
        legend.title = element_blank(),    
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
cc <- ggplot(cc[plant == "lichen"], aes(latitude, meanDiet)) +
  geom_jitter(aes(color = season), width = 2, alpha = 0.75) +
  geom_smooth(method = "lm", color = "black") +
  ylab("% Lichen in diet") + 
  xlab("Latitude") +
  ggtitle("C)") +
  scale_color_viridis_d() +
  theme(legend.position = 'none',
        legend.title = element_blank(),    
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
grid.arrange(aa,bb,cc,nrow = 1)

dev.off()


png("figures/fig4.png", width = 6000, height = 4500, units = "px", res = 600)
ggplot(cc[plant == "lichen"], aes(latitude, meanDiet, color = season)) +
  geom_jitter(width = 2, alpha = 0.75) +
  geom_smooth(method = "lm", se = F) +
  ylab("% Lichen in Diet") + 
  scale_color_viridis_d() +
  theme(#legend.position = c(0.1, 0.875),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size=18),
    axis.text.y = element_text(size=12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"), 
    strip.text  = element_text(size = 16),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~season)
dev.off()


png("figures/fig5.png", width = 6000, height = 4500, units = "px", res = 600)
ggplot(cc[plant == "shrubs"], aes(latitude, meanDiet, color = season)) +
  geom_jitter(width = 2, alpha = 0.75) +
  geom_smooth(method = "lm", se = F) +
  ylab("% Shrub in Diet") + 
  scale_color_viridis_d() +
  theme(#legend.position = c(0.1, 0.875),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size=18),
    axis.text.y = element_text(size=12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"), 
    strip.text  = element_text(size = 16),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~season)
dev.off()

png("figures/fig6.png", width = 6000, height = 4500, units = "px", res = 600)
ggplot(cc[plant == "graminoid"], aes(latitude, meanDiet, color = season)) +
  geom_jitter(width = 2, alpha = 0.75) +
  geom_smooth(method = "lm", se = F) +
  ylab("% Graminoid in Diet") + 
  scale_color_viridis_d() +
  theme(#legend.position = c(0.1, 0.875),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size=18),
    axis.text.y = element_text(size=12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black"), 
    strip.text  = element_text(size = 16),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~season)
dev.off()