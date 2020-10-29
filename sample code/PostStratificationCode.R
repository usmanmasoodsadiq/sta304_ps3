setwd("C:/Users/Sammi-Jo/Documents/OldDropBox/STA304/Friday Lectures/Week 5/Archive")

library(broom) # Helps make the regression results tidier
library(tidyverse) # Helps make programming with R easier

### Step 1: Loading in the Poll data
example_poll <- read_csv("outputs/data/example_poll.csv")

head(example_poll)

example_poll %>% 
  summarise(raw_ALP_prop = sum(supports_ALP) / nrow(example_poll))

### Step 2: "Modelling" the Poll data
model <- lm(supports_ALP ~ gender + age_group,
            data = example_poll)

broom::tidy(model)


### Step 3: Loading in the census data
census_data <- read_csv("outputs/data/census_data.csv")
head(census_data)

### Step 4: Apply the model to the poststratification data
# Here we are making predictions using our model 
# with some new data from the census, and we saving 
# the results of those predictions by adding a new 
# column to the census_data dataset called 'estimate'.
census_data$estimate <-
  model %>%
  predict(newdata = census_data)

census_data %>%
  mutate(alp_predict_prop = estimate*cell_prop_of_division_total) %>%
  group_by(state) %>%
  summarise(alp_predict = sum(alp_predict_prop))

