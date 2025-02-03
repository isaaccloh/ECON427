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
  ### Read the textbook exercise and filter for the correct time range here (hint: "Time range to download") ###
  pull(name)

## Extracting data from IPUMS
cps_extract_request = define_extract_micro(
  collection = 'cps',
  description = "1968-2023 CPS Data",
  samples = namesdf,
  variables = c("YEAR", "SEX", "AGE", "LABFORCE", "NCHLT5", "ASECWT") ### Complete this list with the appropriate variables; note that you'll have to use all-caps ###
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

## Analyzing data (Exercise 1)
# Data selection
df_2 = cps_data %>% filter() ### Fill in age ranges and labforce values to consider. Hint: use & and LABFORCE != 0 ###

# Save the data
save(df_2, file = 'df_2.RData')

# Load data
load('df_2.RData')

# Creating variables
df = df_2 %>% mutate(labforce = ifelse()) %>% ### Use the ifelse command to set labforce equal to 1 if LABFORCE equals 2, and 0 otherwise ###
  mutate(chil5 = ifelse()) ### Use the ifelse command to set chil5 equal to 1 if NCHLT5 is >= 1, 0 otherwise ###

# Weighted LFPR over time
df %>% group_by(YEAR, SEX) %>%
  summarize(lfpr = weighted.mean(labforce, ASECWT, na.rm = TRUE)) %>% 
  mutate(SEX = as.factor(SEX)) %>%
  ggplot(aes(x = YEAR, y = lfpr, group = SEX, color = SEX)) +
  geom_line() +
  ggtitle('LFPR over time') +
  ylab('LFPR') +
  xlab('year')

# Weighted LFPR over time for females
df %>% filter(SEX == 2) %>% 
  group_by(YEAR, chil5) %>%
  summarize(lfpr = weighted.mean(labforce, ASECWT, na.rm = TRUE)) %>% 
  mutate(chil5 = as.factor(chil5)) %>%
  ggplot(aes(x = YEAR, y = lfpr, group = chil5, color = chil5)) +
  geom_line() +
  ggtitle('LFPR over time for women') +
  ylab('LFPR') +
  xlab('year')

# Weighted LFPR over time for males
df %>% filter(SEX == 1) %>% 
  group_by(YEAR, chil5) %>%
  summarize(lfpr = weighted.mean(labforce, ASECWT, na.rm = TRUE)) %>% 
  mutate(chil5 = as.factor(chil5)) %>%
  ggplot(aes(x = YEAR, y = lfpr, group = chil5, color = chil5)) +
  geom_line() +
  ggtitle('LFPR over time for men') +
  ylab('LFPR') +
  xlab('year')
