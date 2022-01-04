


library(data.table)
library(ggplot2)
library(visreg)
library(gridExtra)

sub <- fread("output/clean-data-all.csv")

## multiply NPP * 0.0001
sub$Npp <- sub$Npp*0.0001

gram <- readRDS("output/graminoid-model.RDS")
lichen <- readRDS("output/lichen-model.RDS")

gram <- sub[,c("Npp", "graminoid")]
gram$plant <- "Graminoid"
setnames(gram, "graminoid", "diet")
lichen <- sub[,c("Npp", "lichen")]
lichen$plant <- "Lichen"
setnames(lichen, "lichen", "diet")

df <- rbind(gram, lichen)

col = c("black", "darkgrey")

g <- ggplot(df, aes(Npp, diet, color = plant)) +
  geom_jitter(aes(shape = plant), 
              width = 0.01, 
              alpha = 0.75,
              size = 2) +
  geom_smooth(aes(color = plant), 
              method = "lm", se = F) +
  scale_color_manual(values = col) +
  ylab("Percentage of plant type in diet") + 
  labs(x = expression(Average~annual~net~primary~productivity~~(frac(kg~C, m^{2})))) +
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

ggsave(
  'figures/fig4.pdf',
  g,
  width = 7,
  height = 7,
  dpi = 320
)