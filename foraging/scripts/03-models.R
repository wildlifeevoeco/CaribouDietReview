
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

