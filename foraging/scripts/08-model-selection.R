
library(data.table)
library(glmmTMB)


## load data
sub <- fread("output/clean-data.csv")
sub$lichen <- sub$lichen/100
sub$graminoid <- sub$graminoid/100
sub$shrubs <- sub$shrubs/100

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
                aug_daily_average +
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

## base model
a2 <- glmmTMB(lichen ~ 
                season + 
                latitude + 
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

## subspecies
a3 <- glmmTMB(lichen ~ 
                subspecies + 
                season +
                latitude + 
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

## sympatry model
a4 <- glmmTMB(lichen ~ 
                season + 
                latitude + 
                symp + 
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

## temp model
a5 <- glmmTMB(lichen ~ 
                season +  
                latitude + 
                aug_daily_average +
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

aic <- AIC(a1, a2, a3, a4, a5)

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
                aug_daily_average +
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

## base model
b2 <- glmmTMB(graminoid ~ 
                season + 
                latitude + 
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

## subspecies
b3 <- glmmTMB(graminoid ~ 
                subspecies + 
                season +
                latitude + 
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

## sympatry model
b4 <- glmmTMB(graminoid ~ 
                season + 
                latitude + 
                symp + 
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

## temp model
b5 <- glmmTMB(graminoid ~ 
                season +  
                latitude + 
                aug_daily_average +
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

aic <- AIC(b1, b2, b3, b4, b5)

data.table(AIC = aic$AIC, 
           delta = abs(min(aic$AIC) - aic$AIC))

summary(b3)

############################# 
########## SHRUBS ##########
############################# 

## global model
c1 <- glmmTMB(shrubs ~ 
                subspecies + 
                season + 
                scale(latitude) + 
                symp + 
                aug_daily_average +
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

## base model
c2 <- glmmTMB(shrubs ~ 
                season + 
                scale(latitude) + 
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

## subspecies
c3 <- glmmTMB(shrubs ~ 
                subspecies + 
                season +
                scale(latitude)  + 
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

## sympatry model
c4 <- glmmTMB(shrubs ~ 
                season + 
                scale(latitude) + 
                symp + 
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

## temp model
c5 <- glmmTMB(shrubs ~ 
                season +  
                scale(latitude) + 
                aug_daily_average +
                (1|data) + (1|authors),
              ziformula=~1,
              family=beta_family,
              data = sub)

aic <- AIC(c1, c2, c3, c4, c5)

data.table(AIC = aic$AIC, 
           delta = abs(min(aic$AIC) - aic$AIC))



