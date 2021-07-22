
library(data.table)
library(glmmTMB)


## load data
sub <- fread("output/clean-data.csv")
sub$lichen <- sub$lichen/100
sub$graminoid <- sub$graminoid/100
sub$vascular <- sub$vascular/100

## change names of some variables
setnames(sub, c("data type", "Subspecies", "sympatric_ungulates"), c("data", "subspecies", "symp"))


########################### 
######## LICHEN ##########
########################### 

## global model
a1 <- glmmTMB(lichen ~ 
                subspecies + 
                season + 
                latitude + 
                symp + 
                data + (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## base model
a2 <- glmmTMB(lichen ~ 
                season + 
                latitude + 
                data + (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## subspecies
a3 <- glmmTMB(lichen ~ 
                subspecies + 
                season +
                latitude + 
                data + (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## sympatry model
a4 <- glmmTMB(lichen ~ 
                season + 
                latitude + 
                symp + 
                data + (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

aic <- AIC(a1, a2, a3, a4)

data.table(AIC = aic$AIC, 
           delta = abs(min(aic$AIC) - aic$AIC))

summary(a3)

############################# 
######## GRAMINOID ##########
############################# 

## global model
b1 <- glmmTMB(graminoid ~ 
                subspecies + 
                season + 
                latitude + 
                symp + 
                data + (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## base model
b2 <- glmmTMB(graminoid ~ 
                season + 
                latitude + 
                data + (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## subspecies
b3 <- glmmTMB(graminoid ~ 
                subspecies + 
                season +
                latitude + 
                data + (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## sympatry model
b4 <- glmmTMB(graminoid ~ 
                season + 
                latitude + 
                symp + 
                data + (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

aic <- AIC(b1, b2, b3, b4)

data.table(AIC = aic$AIC, 
           delta = abs(min(aic$AIC) - aic$AIC))

summary(b3)

############################# 
########## SHRUBS ##########
############################# 

## global model
c1 <- glmmTMB(vascular ~ 
                subspecies + 
                season + 
                scale(latitude) + 
                symp + 
                data + 
                (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## base model
c2 <- glmmTMB(vascular ~ 
                season + 
                scale(latitude) + 
                data + (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## subspecies
c3 <- glmmTMB(vascular ~ 
                subspecies + 
                season +
                scale(latitude)  + 
                data + (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

## sympatry model
c4 <- glmmTMB(vascular ~ 
                season + 
                scale(latitude) + 
                symp + 
                data + (1|author_yr),
              ziformula=~1,
              family=beta_family,
              data = sub)

aic <- AIC(c1, c2, c3, c4)

data.table(AIC = aic$AIC, 
           delta = abs(min(aic$AIC) - aic$AIC))

summary(c1)

