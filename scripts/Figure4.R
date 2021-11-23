


library(data.table)
library(ggplot2)
library(gridExtra)

sub <- fread("output/clean-data-all.csv")

gram <- sub[,c("Npp", "graminoid")]
gram$plant <- "Graminoid"
setnames(gram, "graminoid", "diet")
lichen <- sub[,c("Npp", "lichen")]
lichen$plant <- "Lichen"
setnames(lichen, "lichen", "diet")

df <- rbind(gram, lichen)

col = c("black", "darkgrey")

png("figures/fig4.png", width = 4000, height = 4000, units = "px", res = 600)
ggplot(df, aes(Npp, diet, color = plant)) +
  geom_jitter(aes(shape = plant), 
              width = 100, 
              alpha = 0.75,
              size = 2) +
  geom_smooth(aes(color = plant), 
              method = "lm", se = F) +
  scale_color_manual(values = col) +
  xlab("Average annual net primary productivity") +
  ylab("Percentage of plant type in diet") + 
  ylim(0,100) +
  theme(legend.position = c(0.8,0.9), 
        legend.key = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"), 
        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 
dev.off()
