# Clear memory
rm(list = ls())

# Installing packages
install.packages('AER')

# Load packages
library(wooldridge)
library(AER)
library(ggplot2)
library(dplyr)

# View dataset
?labsup

# Create new variable totwrk = weeks * hours
df = labsup %>% mutate(totwrk = weeks * hours)

# Run regressions
reg = lm(totwrk ~ kids, data = df) # OLS 
iv_reg = ivreg(totwrk ~ kids | samesex, data = df) # IV

# Plot results
df = df %>% mutate(ols_pred = predict(reg)) %>%
  mutate(iv_pred = predict(iv_reg))

ggplot(df, aes(x = kids, y = totwrk)) +
  geom_point(color = 'black', alpha = 0.5) +
  geom_line(aes(y = ols_pred), color = 'blue') + 
  geom_line(aes(y = iv_pred), color = 'red') +
  labs(
    title = 'OLS vs IV regression',
    x = 'Number of kids',
    y = 'Hours worked per year',
    caption = 'Blue = OLS, red = IV'
  )
