#### Example script to get IPUMS working

# Clear memory
rm(list = ls())

# Set working directory (customize this step with an appropriate folder)
setwd('C:/Users/lohi/Documents/R')

# Installing packages (do this only once)
install.packages('ipumsr')
install.packages('dplyr')
install.packages('purrr')
install.packages('ggplot2')

# Loading packages
library(ipumsr)
library(dplyr)
library(purrr)
library(ggplot2)

# Save key in .Renviron for use across sessions (try to get your own API key here: https://account.ipums.org/api_keys)
set_ipums_api_key("...", save = TRUE)

# Note: sample code are here https://cps.ipums.org/cps-action/samples/sample_ids
?define_extract_cps

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
  variables = c("YEAR", "AGE", "EDUC", "INCWAGE", "ASECWT")
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

# Viewing data
View(cps_data)