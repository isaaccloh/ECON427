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
  filter(grepl('May 2004', description)) %>%
  mutate(year = substr(description, nchar(description) - 4 + 1, nchar(description))) %>% 
  mutate(year = as.numeric(year)) %>%
  pull(name)

## Extracting data from IPUMS
cps_extract_request = define_extract_micro(
  collection = 'cps',
  description = "May 2004 CPS Data",
  samples = namesdf,
  variables = c('AGE', 'PAIDHOUR', 'EARNWEEK', 'HOURWAGE', 
                'UHRSWORK1', 'EDUC', 'OCC1990', 'WSREGSHFT', 
                'EARNWT', 'WSSUPPWT')
)

submitted_extract = submit_extract(cps_extract_request)
downloadable_extract = wait_for_extract(submitted_extract)
data_files = download_extract(downloadable_extract)

cps_data = read_ipums_micro(data_files)

## Analyzing data (Exercise 1)
# Data selection
df_2 = cps_data %>% filter(PAIDHOUR %in% c(1,2) 
                           & !is.na(EARNWEEK) & !is.na(HOURWAGE) & !is.na(UHRSWORK1) 
                           & UHRSWORK1 > 0 & UHRSWORK1 < 997 
                           & EARNWEEK > 0 & EARNWEEK < 9999.99) %>%
                    filter(AGE >= 16 & AGE <= 64)
  
# Save the data
save(df_2, file = 'df_2.RData')

# Load data
load('df_2.RData')

# Creating variables
df = df_2 %>% mutate(hrwage = ifelse(PAIDHOUR == 1, EARNWEEK/UHRSWORK1, HOURWAGE)) %>% 
  mutate(ireg_wrk = ifelse(WSREGSHFT == 1, 1, 0)) %>%
  mutate(educ = case_when(
    EDUC <=60 ~ 'less than HS',
    EDUC >= 70 & EDUC <= 73 ~ 'HS',
    EDUC >= 80 & EDUC <= 100 ~ 'Some col',
    EDUC >= 110 ~ 'Col'
  )) %>%
  
  
### Create new variables educ, age_group, occupations using mutate, ifelse, and case_when commands ###
  

# Figure 1
df %>% group_by(educ) %>%
  summarize(ireg_wrk = mean(ireg_wrk, na.rm = TRUE)) %>%
  ggplot(aes(x = educ, y = ireg_wrk)) + geom_bar(stat="identity") +
  ggtitle('Irregular work by educational group') +
  ylab('Irregular work percentage') +
  xlab('Educational group') +
  ylim(0,1)

# Figure 2  
df %>% group_by(age_group) %>%
  summarize(ireg_wrk = mean(ireg_wrk, na.rm = TRUE)) %>%
  add_row(age_group = "Overall", ireg_wrk = mean(df$ireg_wrk, na.rm = TRUE))%>%
  ggplot(aes(x = age_group, y = ireg_wrk)) + geom_bar(stat="identity") +
  ggtitle('Irregular work by age group') +
  ylab('Irregular work percentage') +
  xlab('Age group') +
  ylim(0,1)

# Figure 3  
df %>% group_by(educ, ireg_wrk) %>%
  mutate(ireg_wrk = as.factor(ireg_wrk)) %>%
  summarize(avg_wage = mean(hrwage, na.rm = TRUE)) %>%
  ggplot(aes(x = educ, y = avg_wage, fill = ireg_wrk)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  ggtitle('Wage by education and irregular work') +
  ylab('Average wage') +
  xlab('Education/irregular shift?') 

# Figure 4  
df %>% group_by(occupation) %>%
  summarize(ireg_wrk = mean(ireg_wrk, na.rm = TRUE)) %>%
  add_row(occupation = "Overall", ireg_wrk = mean(df$ireg_wrk, na.rm = TRUE))%>%
  ggplot(aes(x = occupation, y = ireg_wrk)) + geom_bar(stat="identity") +
  ggtitle('Irregular work by occupation') +
  ylab('Irregular work percentage') +
  xlab('Occupation') +
  ylim(0,1)

# Figure 5  
df %>% group_by(occupation, ireg_wrk) %>%
  mutate(ireg_wrk = as.factor(ireg_wrk)) %>%
  summarize(avg_wage = mean(hrwage, na.rm = TRUE)) %>%
  ggplot(aes(x = occupation, y = avg_wage, fill = ireg_wrk)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  ggtitle('Wage by occupation and irregular work') +
  ylab('Average wage') +
  xlab('Occupation/irregular shift?') 