
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

## remove subspecies
a2 <- glmmTMB(lichen ~ 
                #subspecies + 
                season + 
                latitude + 
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
                latitude + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove latitude
a4 <- glmmTMB(lichen ~ 
                subspecies + 
                season + 
                #latitude + 
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
                latitude + 
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
                latitude + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove subspecies + latitude
a7 <- glmmTMB(lichen ~ 
                #subspecies + 
                season + 
                #latitude + 
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
                latitude + 
                #symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove season + latitude
a9 <- glmmTMB(lichen ~ 
                subspecies + 
                #season + 
                #latitude + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## remove season + latitude
a10 <- glmmTMB(lichen ~ 
                subspecies + 
                #season + 
                latitude + 
                #symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)


## remove latitude + sympatry
a11 <- glmmTMB(lichen ~ 
                 subspecies + 
                 season + 
                 #latitude + 
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
                 #latitude + 
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
                 #latitude + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only latitude
a14 <- glmmTMB(lichen ~ 
                 #subspecies + 
                 #season + 
                 latitude + 
                 #symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

## only latitude
a15 <- glmmTMB(lichen ~ 
                 #subspecies + 
                 #season + 
                 #latitude + 
                 symp + 
                 data + 
                 (1|author_yr),
               ziformula=~1,
               family=beta_family,
               data = sub)

MuMIn::AICc(a1, a2, a3, a4, a5, a6, a7, a8,
     a9, a10, a11, a12, a13, a14, a15)



saveRDS(a1, "output/lichen-model.RDS")


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
saveRDS(b1, "output/graminoid-model.RDS")


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
saveRDS(c1, "output/vascular-model.RDS")

