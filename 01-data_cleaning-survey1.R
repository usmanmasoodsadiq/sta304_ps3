#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/Victor/Documents/sta304_ps3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625/ns20200625.dta")
# Add the labels

raw_data <- labelled::to_factor(raw_data)


raw_data 

unique(raw_data$household_income)


# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(
         vote_2020,
         gender,
         race_ethnicity,
         household_income,
         education, 
         age) %>%
  filter(vote_2020 != "I am not sure/don't know") %>% 
  filter(vote_2020 != "I would not vote") %>% 
  filter(vote_2020 != "Someone else") %>%
  na.omit() %>%
  
  
  mutate(race_ethnicity = 
    case_when(
      race_ethnicity == "White" ~ "white",
      race_ethnicity == "Black, or African American" ~ "black/african american/negro",
      race_ethnicity == "Asian (Vietnamese)" ~ "other asian or pacific islander",
      race_ethnicity == "Asian (Chinese)" ~ "chinese",
      race_ethnicity == "Asian (Korean)" ~ "other asian or pacific islander",
      race_ethnicity == "Asian (Japanese)" ~ "japanese",
      race_ethnicity == "Asian (Filipino)" ~ "other asian or pacific islander",
      race_ethnicity == "Asian (Other)" ~ "other asian or pacific islander",
      race_ethnicity == "Asian (Asian Indian)" ~ "american indian or alaska native",
      race_ethnicity == "Pacific Islander (Other)" ~ "other asian or pacific islander",
      race_ethnicity == "Pacific Islander (Samoan)" ~ "other asian or pacific islander",
      race_ethnicity == "Pacific Islander (Guamanian)" ~ "other asian or pacific islander",
      race_ethnicity == "Some other race" ~ "other race, nec",
      race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ "other asian or pacific islander",
      race_ethnicity == "American Indian or Alaska Native" ~ "american indian or alaska native"
    )
  ) %>%
  
  mutate(household_income = 
           case_when(
             (household_income == "Less than $14,999" | household_income == "$15,000 to $19,999" | 
                household_income == "$20,000 to $24,999" | household_income == '<NA>') ~ "less than $24,999",
             (household_income == "$25,000 to $29,999" | household_income == "$30,000 to $34,999" | 
                household_income == "$35,000 to $39,999"| household_income == "$40,000 to $44,999" |
                household_income == "$45,000 to $49,999") ~ "24,999 to 49,999",
             (household_income == "$50,000 to $54,999" | household_income == "$55,000 to $59,999" | 
                household_income == "$60,000 to $64,999"| household_income == "$65,000 to $69,999" |
                household_income == "$70,000 to $74,999" | household_income == "$75,000 to $79,999" |
                household_income == "$80,000 to $84,999" | household_income == "$85,000 to $89,999" |
                household_income == "$90,000 to $94,999" | household_income == "$95,000 to $99,999") ~ "50,000 to 99,999",
             (household_income == "$100,000 to $124,999" | household_income == "$125,000 to $149,999") ~ "100,000 to 149,999",
             (household_income == "$175,000 to $199,999" | household_income == "$150,000 to $174,999" | 
                household_income == "$200,000 to $249,999"| household_income == "$250,000 and above") ~ "150,000 or more"
           )
  ) %>%
  
  mutate(education = 
           case_when(
             (education == "High school graduate" | education == "Other post high school vocational training" | 
                education == "3rd Grade or less" | education == "Completed some high school" | education == "Middle School - Grades 4 - 8") ~ "highschool or less",
             (education == "Associate Degree" | education == "Completed some college, but no degree") ~ "associate degree",
             (education == "College Degree (such as B.A., B.S.)" | education == "Completed some graduate, but no degree") ~ "bachelors degree",
             (education == "Masters degree" | education == "Doctorate degree") ~ "masters or above"
           )
  ) %>%
  
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0)) %>%
  
  mutate(gender = ifelse(gender=="Male","male","female"))


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?


# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/survey_data.csv")


