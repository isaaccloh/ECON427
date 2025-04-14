# Clear memory
rm(list = ls())

# Set working directory (customize this step with an appropriate folder)
setwd('C:/Users/lohi/Documents/R')

# Loading packages
library(ipumsr)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(broom)

# Getting list of variables
namesdf = get_sample_info(collection = 'cps') %>%
  filter(grepl('ASEC', description) & !grepl('Research', description)) %>%
  mutate(year = substr(description, nchar(description) - 4 + 1, nchar(description))) %>% 
  mutate(year = as.numeric(year)) %>%
  filter(year >= 1964) %>%
  pull(name)

## Extracting data from IPUMS
cps_extract_request = define_extract_micro(
  collection = 'cps',
  description = "1964-2025 CPS Data",
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

# Creating new variables
df = df_1 %>% mutate(educ = case_when(EDUC == 10 ~ 2.5,
                                      EDUC == 11 ~ 1,
                                      EDUC == 12 ~ 2,
                                      EDUC == 13 ~ 3,
                                      EDUC == 14 ~ 4,
                                      EDUC == 20 ~ 5.5,
                                      EDUC == 21 ~ 5,
                                      EDUC == 22 ~ 6,
                                      EDUC == 30 ~ 7.5,
                                      EDUC == 31 ~ 7,
                                      EDUC == 32 ~ 8,
                                      EDUC == 40 ~ 9,
                                      EDUC == 50 ~ 10,
                                      EDUC == 60 ~ 11,
                                      EDUC %in% c(70, 71, 72, 73) ~ 12,
                                      EDUC %in% c(80, 81) ~ 13,
                                      EDUC %in% c(90, 91, 92) ~ 14,
                                      EDUC == 100 ~ 15,
                                      EDUC %in% c(110, 111) ~ 16,
                                      EDUC %in% c(120, 121) ~ 17,
                                      EDUC %in% c(122,123) ~ 18,
                                      EDUC %in% c(124,125) ~ 20,
                                      .default = NA
                                      )) %>%
  mutate(wkswork = case_when(WKSWORK2 == 1 ~ 7,
                             WKSWORK2 == 2 ~ 20,
                             WKSWORK2 == 3 ~ 33,
                             WKSWORK2 == 4 ~ 43.5,
                             WKSWORK2 == 5 ~ 48.5,
                             WKSWORK2 == 6 ~ 51,
                             .default = NA)) %>%
  mutate(learn = log(INCWAGE / wkswork)) %>%
  mutate(exper = AGE - educ - 6) %>%
  mutate(sex = case_when(SEX == 1 ~ 'Male',
                         SEX == 2 ~ 'Female',
                         .default = NA)) %>%
  mutate(expersq = exper^2)

# Statistical analysis questions 1 and 2
results = df %>% group_by(sex) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(learn ~ educ + exper + expersq, data = .x, weights = .x$ASECWT)),
    tidied = map(model, tidy)
  ) %>%
  unnest(tidied)

educ_returns = results %>% filter(term == 'educ') %>%
  select(estimate)

# Statistical analysis questions 3-5. Note: SEX == 1 is males, 2 is females
results_year = df %>% group_by(sex, YEAR) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(learn ~ educ + exper + expersq, data = .x, weights = .x$ASECWT)),
    tidied = map(model, tidy)
  ) %>%
  unnest(tidied)

age_earnings_slope = results_year %>% select(term, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  mutate(slope = exper + (2 * 10 * expersq))

# Rates of return to schooling
age_earnings_slope %>% ggplot(aes(x = YEAR, y = educ , group = sex, color = sex)) +
  geom_line() +
  ggtitle('Returns to schooling by year') +
  xlab('Year') +
  ylab('Returns to schooling')

# Rates of return to experience
age_earnings_slope %>% ggplot(aes(x = YEAR, y = slope, group = sex, color = sex)) +
  geom_line() +
  ggtitle('Returns to experience by year') +
  xlab('Year') +
  ylab('Returns to experience') 
