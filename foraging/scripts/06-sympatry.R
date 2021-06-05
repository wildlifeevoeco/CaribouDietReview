

library(data.table)
library(ggplot2)
library(gridExtra)

sub <- fread("output/clean-data.csv")

sub <- sub[!is.na(sub$sympatric_ungulates)]

### Sympatric ungulates
sub$sympatric_ungulates[sub$sympatric_ungulates == "4"] <- ">4"
sub$sympatric_ungulates[sub$sympatric_ungulates == "5"] <- ">4"
sub$sympatric_ungulates[sub$sympatric_ungulates == "6"] <- ">4"

sub$sympatric_ungulates <- factor(sub$sympatric_ungulates, levels=c("0", "1",  "2", "3", ">4"))


png("figures/fig3.png", width = 8000, height = 4000, units = "px", res = 600)
aa <- ggplot(sub, 
             aes(sympatric_ungulates, lichen, fill=sympatric_ungulates)) + 
  geom_jitter(aes(color = sympatric_ungulates), 
              shape = 16,
              position = position_jitter(0.2),
              size = 2,
              alpha = 0.6) +
  geom_boxplot(aes(fill = sympatric_ungulates), 
               alpha = 0.5,
               outlier.color = NA) + 
  #geom_bar(stat = "identity", na.rm=TRUE, color = "black", alpha = 0.75) +
  #geom_errorbar(aes(ymin = meanDiet, 
  #                  ymax = meanDiet + sdDiet), width = 0.2) +
  ylab("Percent lichen in diet") + xlab("Number of sympatric ungulates") +
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
        axis.text.x = element_text(size = 12, color = "black",  hjust = 1), 
        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

bb <- ggplot(sub, 
             aes(sympatric_ungulates, graminoid, fill=sympatric_ungulates)) + 
  geom_jitter(aes(color = sympatric_ungulates), 
              shape = 16,
              position = position_jitter(0.2),
              size = 2,
              alpha = 0.6) +
  geom_boxplot(aes(fill = sympatric_ungulates), 
               alpha = 0.5,
               outlier.color = NA) + 
  #geom_bar(stat = "identity", na.rm=TRUE, color = "black", alpha = 0.75) +
  #geom_errorbar(aes(ymin = meanDiet, 
  #                  ymax = meanDiet + sdDiet), width = 0.2) +
  ylab("Percent graminoid in diet") + xlab("Number of sympatric ungulates") +
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
        axis.text.x = element_text(size = 12, color = "black", hjust = 1), 
        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

cc <- ggplot(sub, 
             aes(sympatric_ungulates, vascular, fill=sympatric_ungulates)) + 
  geom_jitter(aes(color = sympatric_ungulates), 
              shape = 16,
              position = position_jitter(0.2),
              size = 2,
              alpha = 0.6) +
  geom_boxplot(aes(fill = sympatric_ungulates), 
               alpha = 0.5,
               outlier.color = NA) + 
  #geom_bar(stat = "identity", na.rm=TRUE, color = "black", alpha = 0.75) +
  #geom_errorbar(aes(ymin = meanDiet, 
  #                  ymax = meanDiet + sdDiet), width = 0.2) +
  ylab("Percent vascular plants in diet") + xlab("Number of sympatric ungulates") +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  ylim(0,100) +
  ggtitle("C)") +
  theme(legend.position = 'none',
        legend.key = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_blank(),
        axis.title = element_text(size=18),
        axis.text.y = element_text(size=12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black", hjust = 1), 
        strip.text  = element_text(size = 16),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 


grid.arrange(aa,bb, cc, nrow = 1)
dev.off()
