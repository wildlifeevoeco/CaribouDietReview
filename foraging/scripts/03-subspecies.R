
library(data.table)
library(ggplot2)

sub <- fread("output/clean-data.csv")


png("figures/fig2.png", width = 6000, height = 4000, units = "px", res = 600)
aa <- ggplot(sub, 
             aes(Subspecies, lichen, fill=Subspecies)) + 
  geom_jitter(aes(color = Subspecies), 
              shape = 16,
              position = position_jitter(0.2),
              size = 2,
              alpha = 0.6) +
  geom_boxplot(aes(fill = Subspecies), 
               alpha = 0.5,
               outlier.color = NA) + 
  #geom_bar(stat = "identity", na.rm=TRUE, color = "black", alpha = 0.75) +
  #geom_errorbar(aes(ymin = meanDiet, 
  #                  ymax = meanDiet + sdDiet), width = 0.2) +
  ylab("Percent lichen in diet") + xlab("") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ylim(0,100) +
  ggtitle("A)") +
  theme(legend.position = 'none',
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
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

bb <- ggplot(sub, 
             aes(Subspecies, graminoid, fill=Subspecies)) + 
  geom_jitter(aes(color = Subspecies), 
              shape = 16,
              position = position_jitter(0.2),
              size = 2,
              alpha = 0.6) +
  geom_boxplot(aes(fill = Subspecies), 
               alpha = 0.5,
               outlier.color = NA) + 
  #geom_bar(stat = "identity", na.rm=TRUE, color = "black", alpha = 0.75) +
  #geom_errorbar(aes(ymin = meanDiet, 
  #                  ymax = meanDiet + sdDiet), width = 0.2) +
  ylab("Percent graminoid in diet") + xlab("") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ylim(0,100) +
  ggtitle("B)") +
  theme(legend.position = 'none',
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
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

grid.arrange(aa,bb, nrow = 1)

dev.off()
