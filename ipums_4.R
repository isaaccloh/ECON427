# Clear memory
rm(list = ls())

# Set working directory (customize this step with an appropriate folder)
setwd('C:/Users/lohi/Documents/R')

# Loading packages. Install if necessary
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
  filter(year >= 2018) %>% 
  filter(!(name %in% c('cps2022_02b', 'cps2022_05b'))) %>%
  pull(name)

## Extracting data from IPUMS
cps_extract_request = define_extract_micro(
  collection = 'cps',
  description = "2018-2025 CPS Data",
  samples = namesdf,
  variables = c("YEAR", "MONTH", "SEX", "AGE", "RACE", "HISPAN", "EMPSTAT",
                "EARNWEEK", "WTFINL", "EARNWT", "EDUC"
                ) 
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

## Analyzing data (Exercise 1)
# Data selection
df_2 = cps_data %>% filter(AGE >= 18 & AGE <=  64 & EMPSTAT >= 10) ### FILL IN THE AGE RANGES TO CONSIDER ###

# Save the data
save(df_2, file = 'df_2.RData')

# Load data
load('df_2.RData')

# Creating variables
df = df_2 %>% mutate(employedwork = ifelse(EMPSTAT == 10, 1, 0)) %>%
  ### Use the mutate and case_when commands to create variables 'race' and 'educ' using the directions in our textbook (see the previous data explorer exercise) ###
  mutate(learnwk = ifelse(EARNWEEK > 0 & EARNWEEK <= 9999.99, log(EARNWEEK), NA)) %>%
  mutate(year_month = as.integer(YEAR) + (as.integer(MONTH) -1) / 12)
           
# Figure 1
df %>% group_by(year_month) %>%
  summarize(emppop = sum(employedwork) / n()) %>%
  ggplot(aes(x = year_month, y = emppop)) + geom_line() +
  ggtitle('Employment share') +
  ylab('Employment population') +
  xlab('Year/month') +
  ylim(0,1)

# Figure 2  
df %>% group_by(year_month, SEX) %>%
  summarize(emppop = sum(employedwork) / n()) %>%
  mutate(SEX = as.factor(SEX)) %>%
  ggplot(aes(x = year_month, y = emppop, group = SEX, color = SEX)) +
  geom_line() +
  ggtitle('Employment share by sex') +
  ylab('Employment population') +
  xlab('Year/month') +
  ylim(0,1)

# Figure 3  
df %>% group_by(year_month, race) %>%
  summarize(emppop = sum(employedwork) / n()) %>%
  ggplot(aes(x = year_month, y = emppop, group = race, color = race)) +
  geom_line() +
  ggtitle('Employment share by race') +
  ylab('Employment population') +
  xlab('Year/month') +
  ylim(0,1)

# Figure 4  
### Replicate (almost verbatim) the code for Figure 3 to produce a figure showing
### the monthly trends in the employment-population ratio of the various education groups. Hint: try replacing 'race' with 'educ'

# Figure 5  
df %>% group_by(year_month) %>%
  summarize(avglwage = weighted.mean(learnwk, EARNWT, na.rm = TRUE)) %>%
  ggplot(aes(x = year_month, y = avglwage)) +
  geom_line() +
  ggtitle('Average log earnings') +
  ylab('Average log earnings') +
  xlab('Year/month') 
