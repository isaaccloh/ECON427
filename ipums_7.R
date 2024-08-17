# Clear memory
rm(list = ls())

# Set working directory (customize this step with an appropriate folder)
setwd('C:/Users/lohi/Documents/R')

# Loading packages
library(ipumsr)
library(dplyr)
library(purrr)
library(ggplot2)

# Getting list of variables
namesdf = get_sample_info(collection = 'cps') %>%
  filter(grepl('ASEC', description) & !grepl('Research', description)) %>%
  mutate(year = substr(description, nchar(description) - 4 + 1, nchar(description))) %>% 
  mutate(year = as.numeric(year)) %>%
  filter(year >= 1964) %>%
  pull(name)

## Extracting data from IPUMS
cps_extract_request = define_extract_cps(
  description = "1964-2023 CPS Data",
  samples = namesdf,
  variables = c("YEAR", "AGE", "EDUC", "INCWAGE", "ASECWT", "SEX", "WKSWORK2")
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

## Analyzing data (Exercise 1)
# Data selection
df_1 = cps_data %>% filter(AGE >= 21 & AGE <=64 & INCWAGE > 0 & INCWAGE < 99999998 
                           & WKSWORK2 > 1 & WKSWORK2 < 6 & ASECWT >= 0)

# Save the data
save(df_1, file = 'df_1.RData')

# Load data
load('df_1.RData')

df = df_1 %>% mutate()