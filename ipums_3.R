# Clear memory
rm(list = ls())

# Set working directory (customize this step with an appropriate folder)
setwd('C:/Users/lohi/Documents/R')

# Loading packages
library(ipumsr)
library(dplyr)
library(purrr)
library(ggplot2)
library(data.table)
library(tidyr)

# Getting list of variables
namesdf = get_sample_info(collection = 'cps') %>%
  filter(!grepl('ASEC', description)) %>%
  mutate(year = substr(description, nchar(description) - 4 + 1, nchar(description))) %>% 
  mutate(year = as.numeric(year)) %>%
  filter(year == 2021) %>% 
  pull(name)

## Extracting data from IPUMS
cps_extract_request = define_extract_micro(
  collection = 'cps',
  description = "2021 CPS Data",
  samples = namesdf,
  variables = c("YEAR", "SEX", "AGE", "RACE", "HISPAN", 
                "PAIDHOUR", "EARNWEEK", "HOURWAGE", "UHRSWORK1", 
                "EDUC", "EARNWT")
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

## Analyzing data (Exercise 1)
# Data selection
df_2 = cps_data %>% filter(AGE >= 16 & AGE <= 69) %>%
  filter(PAIDHOUR == 1 | PAIDHOUR == 2) %>%
  filter(!is.na(UHRSWORK1)) %>%
  filter(!is.na(EARNWEEK)) %>%
  filter(!is.na(HOURWAGE))

# Save the data
save(df_2, file = 'df_2.RData')

# Load data
load('df_2.RData')

# Creating variables
df = df_2 %>% filter(EARNWEEK > 0 & EARNWEEK < 9999.99) %>%
  filter(UHRSWORK1 > 0 & UHRSWORK1 < 997) %>%
  filter(HOURWAGE > 0 & HOURWAGE < 99) %>%
  ### Use mutate to create variable wage equal to EARNWEEK / UHRSWORK1 if PAIDHOUR is 1, otherwise HOURWAGE ###
  ### Mutate to create variable low_wage equal to 1 if wage is $15 or less, otherwise 0 ###
  mutate(race = case_when(
    RACE == 100 & HISPAN == 0 ~ 'white',
    RACE == 200 & HISPAN == 0 ~ 'black',
    RACE == 651 & HISPAN == 0 ~ 'asian',
    HISPAN > 1 ~ 'hispanic',
    .default = NA
  )) %>%
  ### Use case_when to create variable equal to 'lhs' if EDUC <= 60, 'hs' if it's between 70 and 73, 'lcol' if between 80 and 100, 'col' if >= 110, NA otherwise (reference example above) ###

# Figure 1
df %>% group_by(AGE, SEX) %>%
  summarize(lowwage = mean(low_wage, na.rm = TRUE)) %>% 
  mutate(SEX = as.factor(SEX)) %>%
  ggplot(aes(x = AGE, y = lowwage, group = SEX, color = SEX)) +
  geom_line() +
  ggtitle('Low wage fractions by sex') +
  ylab('Low wage fraction') +
  xlab('Age')

# Figure 2
df %>% filter(race != 5) %>% group_by(AGE, race) %>%
  summarize(lowwage = mean(low_wage, na.rm = TRUE)) %>% 
  ggplot(aes(x = AGE, y = lowwage, group = race, color = race)) +
  geom_line() +
  ggtitle('Low wage fractions by race') +
  ylab('Low wage fraction') +
  xlab('Age')

# Figure 3
df %>% group_by(AGE, educ) %>%
  filter(educ != 1) %>%
  summarize(lowwage = mean(low_wage, na.rm = TRUE)) %>% 
  ggplot(aes(x = AGE, y = lowwage, group = educ, color = educ)) +
  geom_line() +
  ggtitle('Low wage fractions by education') +
  ylab('Low wage fraction') +
  xlab('Age')
