
library(data.table)
library(ggplot2)

bb <- fread("output/subspecies.csv")

png("figures/fig2.png", width = 6000, height = 4500, units = "px", res = 600)
ggplot(bb, 
       aes(subspecies,meanDiet,fill = subspecies)) + 
  geom_bar(stat = "identity", na.rm=TRUE, color = "black", alpha= 0.75) +
  geom_errorbar(aes(ymin = meanDiet, 
                    ymax = meanDiet + sdDiet), width = 0.2) +
  ylab("% of diet") +
  scale_fill_viridis_d() +
  theme(#legend.position = c(0.1, 0.875),
    legend.key = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    legend.title = element_blank(),
    axis.title = element_text(size=18),
    axis.text.y = element_text(size=12, color = "black"),
    axis.text.x = element_text(size = 12, color = "black", 
                               angle = 90, 
                               hjust = 1), 
    strip.text  = element_text(size = 16),
    strip.background = element_rect(colour="black", size = 1, fill = "white"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~plant)
dev.off()