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