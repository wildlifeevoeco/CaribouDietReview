

library(data.table)
library(ggplot2)
library(gridExtra)

sub <- fread("output/clean-data-all.csv")

sub[, .N, by = "author_yr"]


aa <- ggplot(sub, 
       aes(season, lichen, fill=season)) + 
  geom_jitter(color = "black", 
              shape = 16,
              position = position_jitter(0.2),
              size = 2,
              alpha = 0.6) +
  geom_boxplot(fill = "darkgrey", 
               alpha = 0.5,
               outlier.color = NA) + 
  ylab("Percent lichens in diet") + xlab("") +
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
             aes(season, graminoid, fill=season)) + 
  geom_jitter(color = "black", 
              shape = 16,
              position = position_jitter(0.2),
              size = 2,
              alpha = 0.6) +
  geom_boxplot(fill = "darkgrey", 
               alpha = 0.5,
               outlier.color = NA) + 
  ylab("Percent graminoids in diet") + xlab("") +
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

cc <- ggplot(sub, 
             aes(season, vascular, fill=season)) + 
  geom_jitter(color = "black", 
              shape = 16,
              position = position_jitter(0.2),
              size = 2,
              alpha = 0.6) +
  geom_boxplot(fill = "darkgrey", 
               alpha = 0.5,
               outlier.color = NA) + 
  ylab("Percent vascular plants in diet") + xlab("") +
  ylim(0,100) +
  ggtitle("C)") +
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


g <- grid.arrange(aa,bb, cc, nrow = 1)


ggsave(
  'figures/fig1.pdf',
  g,
  width = 10,
  height = 5,
  dpi = 320
)
