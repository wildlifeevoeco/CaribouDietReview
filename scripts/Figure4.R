
## load packages
library(data.table)
library(ggplot2)

## load data
a1 <- readRDS("output/lichen-model.RDS")
b1 <- readRDS("output/graminoid-model.RDS")
c1 <- readRDS("output/vascular-model.RDS")

## pull out fixed effects
coefs <- data.table(var = rbind(data.table(rownames(data.frame(summary(a1)[6]$coefficients$cond[,1]))), ## lichen
                                data.table(rownames(data.frame(summary(b1)[6]$coefficients$cond[,1]))), ## graminoids
                                data.table(rownames(data.frame(summary(c1)[6]$coefficients$cond[,1])))), ## vascular plants
                    coef = rbind(data.table(summary(a1)[6]$coefficients$cond[,1]), 
                                 data.table(summary(b1)[6]$coefficients$cond[,1]), 
                                 data.table(summary(c1)[6]$coefficients$cond[,1])),
                    se = rbind(data.table(summary(a1)[6]$coefficients$cond[,2]), 
                               data.table(summary(b1)[6]$coefficients$cond[,2]), 
                               data.table(summary(c1)[6]$coefficients$cond[,2])),
                    model = c(rep("Lichen", 11), 
                              rep("Graminoids", 3),
                              rep("Vascular plants", 7)))
setnames(coefs, c("var.V1", "coef.V1", "se.V1"), c("var", "coef", "se"))

coefs

png("figures/Fig4.png", width = 5000, height = 5000, units = "px", res = 600)
ggplot(data = coefs) +
  geom_point(aes(coef, var, color = model), size = 2, 
             position = position_dodge(width = 0.4), 
             alpha = 0.5) +
  geom_errorbar(aes(coef, var, 
                    xmin = coef - se, 
                    xmax = coef + se,
                    color = model), width = 0, size = 0.75,
                position = position_dodge(width = 0.4), 
                alpha = 0.5) +  
  geom_vline(xintercept = 0, lty = 2) +
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77")) +
  #labels = c("Lichen", "Graminoids", "Vascular plants")) +
  xlab("Fixed effect coefficient estimate") + 
  ylab("") +
  scale_y_discrete(labels = c(`(Intercept)` = "Model Intercept",
                              `subspecies6-Introduced-Reindeer` = "Ecotype (Introduced Reindeer)",
                              `subspecies5-Reindeer` = "Ecotype (Reindeer)", 
                              `subspecies4-Woodland` = "Ecotype (Woodland)",
                              `subspecies3-Mountain` = "Ecotype (Mountain)", 
                              `subspecies2-Barren Ground` = "Ecotype (Barren Ground)",
                              `season5-Spring` = "Season (Spring)", 
                              `season4-Winter` = "Season (Winter)", 
                              `season3-Autumn` = "Season (Autumn)", 
                              `season2-Summer` = "Season (Summer)", 
                              `symp` = "Number of sympatric ungulates", 
                              `scale(Npp)` = "Net primary productivity",
                              `datarumen` = "Data type (Rumen)")) +
  theme(
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 14, color = 'black'),
    axis.text = element_text(size = 12, color = 'black'),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    panel.border = element_rect(colour = "black", fill=NA, size = 1)) 
dev.off()

