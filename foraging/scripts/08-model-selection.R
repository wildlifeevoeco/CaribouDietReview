
library(data.table)
library(glmmTMB)
library(broom)
library(ggplot2)

## load data
sub <- fread("output/clean-data.csv")
sub$lichen <- sub$lichen/100
sub$graminoid <- sub$graminoid/100
sub$vascular <- sub$vascular/100

## change names of some variables
setnames(sub, c("data type", "Subspecies", "sympatric_ungulates"), c("data", "subspecies", "symp"))

sub$symp[sub$symp == 5] <- 4
sub$symp[sub$symp == 6] <- 4



########################### 
######## LICHEN ##########
########################### 

## global model
a1 <- glmmTMB(lichen ~ 
                subspecies + 
                season + 
                latitude + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

summary(a1)
performance::r2(a1)

############################# 
######## GRAMINOID ##########
############################# 

## global model
b1 <- glmmTMB(graminoid ~ 
                subspecies + 
                season + 
                latitude + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)
summary(b1)
sjstats::r2(b1)

##################################### 
########## VASCULAR PLANTS ##########
##################################### 

## global model
c1 <- glmmTMB(vascular ~ 
                subspecies + 
                season + 
                latitude + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)
summary(c1)
sjstats::r2(c1)

colnames(coef(a1)$author_yr)

rownames(data.frame(summary(a1)[6]$coefficients$cond[,1]))

## pull out fixed effects
coefs <- data.table(var = rbind(data.table(rownames(data.frame(summary(a1)[6]$coefficients$cond[,1]))),
                                data.table(rownames(data.frame(summary(b1)[6]$coefficients$cond[,1]))),
                                data.table(rownames(data.frame(summary(c1)[6]$coefficients$cond[,1])))),
                    coef = rbind(data.table(summary(a1)[6]$coefficients$cond[,1]), 
                                 data.table(summary(b1)[6]$coefficients$cond[,1]), 
                                 data.table(summary(c1)[6]$coefficients$cond[,1])),
                    se = rbind(data.table(summary(a1)[6]$coefficients$cond[,2]), 
                               data.table(summary(b1)[6]$coefficients$cond[,2]), 
                               data.table(summary(c1)[6]$coefficients$cond[,2])),
                    model = c(rep("Lichen", 13), 
                              rep("Graminoids", 13), 
                              rep("Vascular plants", 13)))
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
                              `latitude` = "Latitude",
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


