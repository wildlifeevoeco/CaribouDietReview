
library(data.table)
library(ggplot2)

cc <- fread("output/latitude.csv")

png("figures/fig3.png", width = 6000, height = 4500, units = "px", res = 600)
ggplot(cc, aes(latitude, meanDiet, color = plant)) +
  geom_jitter(width = 2, alpha = 0.75) +
  geom_smooth(method = "lm", se = F) +
  ylab("% Diet") + 
  scale_color_viridis_d() +
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
    panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~season)
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