#### Preamble ####
# Purpose: Prepare and clean the 5 year 2018 survey data downloaded from ACS in order to predict 2020 elections outcome
# Author: Usman Sadiq and Victor ChanYoung Cho
# Data: 22 October 2020
# Contact: usman.sadiq@mail.utoronto.ca; victorchanyoung.cho@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/usman_sadiq/Desktop/sta304/PS3_new/sta304_ps3/")
raw_data <- read_dta("inputs/usa_00002.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(#region,
         #statefip,
         sex, 
         age, 
         race,
         inctot,
         #hispan,
         educ)
         #marst, 
         #bpl,
         #citizen,
         #educd,
         #labforce)
         

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)
reduced_data <-
  reduced_data %>%
  mutate(household_income = 
           case_when(
             inctot < 25000 ~ "less than 25,000",
             (25000 <= inctot & inctot < 50000) ~ "25,000 to 49,999",
             (50000 <= inctot & inctot < 100000) ~ "50,000 to 99,999",
             (100000 <= inctot & inctot < 150000) ~ "100,000 to 149,999",
             inctot >= 150000 ~ "150,000 or more"
           ))


reduced_data <-
  reduced_data %>%
  mutate(education =
           case_when(
             (educ == "n/a or no schooling" | educ == "nursery school to grade 4" 
              | educ == "grade 5, 6, 7, or 8" | educ == "grade 9" | educ == "grade 10" 
              | educ == "grade 11" | educ == "grade 12") ~ "highschool or less",
             (educ == "1 year of college" | educ == "2 years of college") ~ "associate degree",
             educ == "4 years of college" ~ "bachelors degree",
             educ == "5+ years of college" ~ "masters or above"
           ))



reduced_data <- 
  reduced_data %>%
  count(age, sex, race, household_income, education) %>%
  group_by(age, sex, race, household_income, education) 

reduced_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)") %>%
  filter(race != "three or more major races") %>%
  filter(race != "two major races")

reduced_data <-
  reduced_data %>%
  rename(race_ethnicity = race,
         gender = sex)

reduced_data$age <- as.integer(reduced_data$age)

reduced_data <-
  reduced_data %>%
  filter(age >= 18) %>%
  filter(age < 94)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/census_data.csv")



         