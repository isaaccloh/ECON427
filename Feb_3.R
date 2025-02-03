# February 3
rm(list = ls())

install.packages('foreign')

library(foreign)
library(dplyr)

# Creating data from online dataset 
df = read.dta("https://www.princeton.edu/~otorres/Panel101.dta")

View(df)

# DiD estimation of treatment effect
df = df %>% mutate(time = ifelse(year >= 1994, 1, 0)) %>%
  mutate(treated = ifelse(country == 'E' | country == 'F' | country == 'G', 1, 0)) %>%
  mutate(did = time * treated)
