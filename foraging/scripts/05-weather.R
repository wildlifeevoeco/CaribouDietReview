

library(data.table)
library(ggplot2)


df <- fread("input/foraging.csv")

ggplot(df, aes(aug_daily_average, shrubs)) +
  geom_point() +
  geom_smooth()
ggplot(df, aes(aug_daily_average, lichen)) +
  geom_point() +
  geom_smooth()
