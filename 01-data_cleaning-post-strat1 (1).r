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
setwd("/Users/usman_sadiq/Desktop/sta304/PS3/sta304_ps3/")
raw_data <- read_dta("inputs/usa_00002.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(#region,
         #stateicp,
         sex, 
         age, 
         race,
         inctot,
         hispan,
         educ,
         #marst, 
         #bpl,
         #citizen,
         #educd,
         #labforce,
         labforce)
         

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

reduced_data <- 
  reduced_data %>%
  count(age, sex, race, hispan, inctot, educ, labforce) %>%
  group_by(age, sex, race, hispan, inctot, educ, labforce) 

reduced_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)")

reduced_data <-
  reduced_data %>%
  rename(gender = sex,
         race_ethnicity = race,
         hispanic = hispan,
         household_income = inctot,
         education = educ,
         employment = labforce)

reduced_data$age <- as.integer(reduced_data$age)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/census_data.csv")



         