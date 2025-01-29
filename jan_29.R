# Clear memory 
rm(list = ls()) # CTRL/CMD + ENTER

# Load packages
library(wooldridge)
library(ggplot2)

?sleep75

ggplot(sleep75, aes(x = hrwage, y = totwrk)) +
  geom_point() +
  geom_smooth(method = 'lm', col = 'blue') + 
  labs(title = 'Substitution effect wins',
       x = 'Hourly wage',
       y = 'Minutes worked per week')

# 1/29

library(dplyr)

View(cars) # View dataset
cars %>% filter(dist <= 10)

Deltah = (150 - 120) / 120 * 100
Deltaw = (15 - 11) / 11 * 100 
sigma = Deltah / Deltaw

