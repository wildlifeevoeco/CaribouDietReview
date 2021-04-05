

library(data.table)
library(ggplot2)
library(gridExtra)

df <- fread("input/foraging.csv")

df$Subspecies[df$Subspecies == "Peary"] <- "1-Peary"
df$Subspecies[df$Subspecies == "Greenland"] <- "2-Barren Ground"
df$Subspecies[df$Subspecies == "barren-ground"] <- "2-Barren Ground"
df$Subspecies[df$Subspecies == "mountaine"] <- "3-Mountain"
df$Subspecies[df$Subspecies == "woodland"] <- "4-Woodland"
df$Subspecies[df$Subspecies == "reindeer"] <- "5-Reindeer"
df$Subspecies[df$Subspecies == "introduced-reindeer"] <- "6-Introduced-Reindeer"

png("figures/fig6.png", width = 6000, height = 3000, units = "px", res = 600)
aa <- ggplot(df, aes(aug_daily_average, graminoid)) +
  geom_jitter(aes(color = factor(Subspecies)), 
              shape=16, 
              position=position_jitter(0.2), 
              size = 2, 
              alpha = 0.5) +
  geom_smooth(method = "lm", color = "black") +
  #scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ylim(0,100) +
  ggtitle('A)') +
  ylab("% Graminoids in diet") +
  xlab("August daily average temperature") +
  theme(
    legend.key = element_blank(),
    legend.text = element_text(size = 10, color = "black"),
    legend.title = element_blank(),
    legend.position = c(0.37,0.76),
    axis.title.y = element_text(size=12,color = 'black'),
    axis.text=element_text(size=12,color = 'black'),
    plot.title=element_text(size = 12,hjust=0),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill=NA, size=1))

bb <- ggplot(df, aes(aug_daily_average, shrubs)) +
  geom_jitter(aes(color = factor(Subspecies)), 
              shape=16, 
              position=position_jitter(0.2), 
              size = 2, 
              alpha = 0.5) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_viridis_d() +
  ylim(0,100) +
  ggtitle('B)') +
  ylab("% Shrubs in diet") +
  xlab("August daily average temperature") +
  theme(legend.position = 'none',
    axis.title.y = element_text(size=12,color = 'black'),
    axis.text=element_text(size=12,color = 'black'),
    plot.title=element_text(size = 12,hjust=0),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = 'black', fill=NA, size=1))

cc <- ggplot(df, aes(aug_daily_average, lichen)) +
      geom_jitter(aes(color = factor(Subspecies)), 
                  shape=16, 
                  position=position_jitter(0.2), 
                  size = 2, 
                  alpha = 0.5) +
      geom_smooth(method = "lm", color = "black") +
      #scale_fill_viridis_d() +
      scale_color_viridis_d() +
      ylim(0,100) +
      ggtitle('C)') +
      ylab("% Lichen in diet") +
      xlab("August daily average temperature") +
      theme(legend.position = 'none',
        axis.title.y = element_text(size=12,color = 'black'),
        axis.text=element_text(size=12,color = 'black'),
        plot.title=element_text(size = 12,hjust=0),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = 'black', fill=NA, size=1))    

grid.arrange(aa,bb,cc,nrow = 1)
dev.off()

  