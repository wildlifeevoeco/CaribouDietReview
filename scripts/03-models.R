
library(data.table)
library(glmmTMB)
library(broom)
library(MuMIn)
library(ggplot2)

## load data
sub <- fread("output/clean-data-npp2.csv")
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
                scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove subspecies
a2 <- glmmTMB(lichen ~ 
                #subspecies + 
                season + 
                scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove season
a3 <- glmmTMB(lichen ~ 
                subspecies + 
                #season + 
                scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove scale(Npp)
a4 <- glmmTMB(lichen ~ 
                subspecies + 
                season + 
                #scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove sympatry
a5 <- glmmTMB(lichen ~ 
                subspecies + 
                season + 
                scale(Npp) + 
                #symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove subspecies + season
a6 <- glmmTMB(lichen ~ 
                #subspecies + 
                #season + 
                scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove subspecies + scale(Npp)
a7 <- glmmTMB(lichen ~ 
                #subspecies + 
                season + 
                #scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)


## remove subspecies + sympatry
a8 <- glmmTMB(lichen ~ 
                #subspecies + 
                season + 
                scale(Npp) + 
                #symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove season + scale(Npp)
a9 <- glmmTMB(lichen ~ 
                subspecies + 
                #season + 
                #scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove season + scale(Npp)
a10 <- glmmTMB(lichen ~ 
                subspecies + 
                #season + 
                scale(Npp) + 
                #symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)


## remove scale(Npp) + sympatry
a11 <- glmmTMB(lichen ~ 
                 subspecies + 
                 season + 
                 #scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only subspecies
a12 <- glmmTMB(lichen ~ 
                 subspecies + 
                 #season + 
                 #scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only season
a13 <- glmmTMB(lichen ~ 
                 #subspecies + 
                 season + 
                 #scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only scale(Npp)
a14 <- glmmTMB(lichen ~ 
                 #subspecies + 
                 #season + 
                 scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only scale(Npp)
a15 <- glmmTMB(lichen ~ 
                 #subspecies + 
                 #season + 
                 #scale(Npp) + 
                 symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

lichen_mods <- MuMIn::AICc(a1, a2, a3, a4, a5, a6, a7, a8,
                           a9, a10, a11, a12, a13, a14, a15)

lichen_mods$deltaAIC <- lichen_mods$AICc - min(lichen_mods$AICc) 
lichen_mods$weights <- round(Weights(AICc(a1, a2, a3, a4, a5, a6, a7, a8,
                                    a9, a10, a11, a12, a13, a14, a15)),3)


## top model:
summary(a11)
performance::r2(a11)


saveRDS(a11, "output/lichen-model.RDS")


############################# 
######## GRAMINOID ##########
############################# 

## global model
b1 <- glmmTMB(graminoid ~ 
                subspecies + 
                season + 
                scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove subspecies
b2 <- glmmTMB(graminoid ~ 
                #subspecies + 
                season + 
                scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove season
b3 <- glmmTMB(graminoid ~ 
                subspecies + 
                #season + 
                scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove scale(Npp)
b4 <- glmmTMB(graminoid ~ 
                subspecies + 
                season + 
                #scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove sympatry
b5 <- glmmTMB(graminoid ~ 
                subspecies + 
                season + 
                scale(Npp) + 
                #symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove subspecies + season
b6 <- glmmTMB(graminoid ~ 
                #subspecies + 
                #season + 
                scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove subspecies + scale(Npp)
b7 <- glmmTMB(graminoid ~ 
                #subspecies + 
                season + 
                #scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)


## remove subspecies + sympatry
b8 <- glmmTMB(graminoid ~ 
                #subspecies + 
                season + 
                scale(Npp) + 
                #symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove season + scale(Npp)
b9 <- glmmTMB(graminoid ~ 
                subspecies + 
                #season + 
                #scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove season + scale(Npp)
b10 <- glmmTMB(graminoid ~ 
                 subspecies + 
                 #season + 
                 scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)


## remove scale(Npp) + sympatry
b11 <- glmmTMB(graminoid ~ 
                 subspecies + 
                 season + 
                 #scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only subspecies
b12 <- glmmTMB(graminoid ~ 
                 subspecies + 
                 #season + 
                 #scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only season
b13 <- glmmTMB(graminoid ~ 
                 #subspecies + 
                 season + 
                 #scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only scale(Npp)
b14 <- glmmTMB(graminoid ~ 
                 #subspecies + 
                 #season + 
                 scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only scale(Npp)
b15 <- glmmTMB(graminoid ~ 
                 #subspecies + 
                 #season + 
                 #scale(Npp) + 
                 symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

graminoid_mods <- MuMIn::AICc(b1, b2, b3, b4, b5, b6, b7, b8,
                           b9, b10, b11, b12, b13, b14, b15)

graminoid_mods$deltaAIC <- graminoid_mods$AICc - min(graminoid_mods$AICc) 
graminoid_mods$weights <- round(Weights(AICc(b1, b2, b3, b4, b5, b6, b7, b8,
                                             b9, b10, b11, b12, b13, b14, b15)),3)

## top model
summary(b15)
performance::r2(b15)


saveRDS(b15, "output/graminoid-model.RDS")


##################################### 
########## VASCULAR PLANTS ##########
##################################### 
## global model
c1 <- glmmTMB(vascular ~ 
                subspecies + 
                season + 
                scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

summary(c1)
performance::r2(c1)

## remove subspecies
c2 <- glmmTMB(vascular ~ 
                #subspecies + 
                season + 
                scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove season
c3 <- glmmTMB(vascular ~ 
                subspecies + 
                #season + 
                scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove scale(Npp)
c4 <- glmmTMB(vascular ~ 
                subspecies + 
                season + 
                #scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove sympatry
c5 <- glmmTMB(vascular ~ 
                subspecies + 
                season + 
                scale(Npp) + 
                #symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove subspecies + season
c6 <- glmmTMB(vascular ~ 
                #subspecies + 
                #season + 
                scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove subspecies + scale(Npp)
c7 <- glmmTMB(vascular ~ 
                #subspecies + 
                season + 
                #scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)


## remove subspecies + sympatry
c8 <- glmmTMB(vascular ~ 
                #subspecies + 
                season + 
                scale(Npp) + 
                #symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove season + scale(Npp)
c9 <- glmmTMB(vascular ~ 
                subspecies + 
                #season + 
                #scale(Npp) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove season + scale(Npp)
c10 <- glmmTMB(vascular ~ 
                 subspecies + 
                 #season + 
                 scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)


## remove scale(Npp) + sympatry
c11 <- glmmTMB(vascular ~ 
                 subspecies + 
                 season + 
                 #scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only subspecies
c12 <- glmmTMB(vascular ~ 
                 subspecies + 
                 #season + 
                 #scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only season
c13 <- glmmTMB(vascular ~ 
                 #subspecies + 
                 season + 
                 #scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only scale(Npp)
c14 <- glmmTMB(vascular ~ 
                 #subspecies + 
                 #season + 
                 scale(Npp) + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only scale(Npp)
c15 <- glmmTMB(vascular ~ 
                 #subspecies + 
                 #season + 
                 #scale(Npp) + 
                 symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

vascular_mods <- MuMIn::AICc(c1, c2, c3, c4, c5, c6, c7, c8,
                             c9, c10, c11, c12, c13, c14, c15)

vascular_mods$deltaAIC <- vascular_mods$AICc - min(vascular_mods$AICc) 
vascular_mods$weights <- round(Weights(AICc(c1, c2, c3, c4, c5, c6, c7, c8,
                                             c9, c10, c11, c12, c13, c14, c15)),3)

summary(c8)
performance::r2(c8)

saveRDS(c8, "output/vascular-model.RDS")

