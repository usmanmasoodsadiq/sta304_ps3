setwd("C:/Users/Sammi-Jo/Desktop/Archive")

library(broom) # Helps make the regression results tidier
library(tidyverse) # Helps make programming with R easier
library(brms)

### Step 1: Loading in the Poll data
example_poll <- read_csv("outputs/data/example_poll.csv")

head(example_poll)

example_poll %>% 
  summarise(raw_ALP_prop = sum(supports_ALP) / nrow(example_poll))

### Step 2: "Modelling" the Poll data
model <- brm(supports_ALP ~ gender + age_group,
              data = example_poll, 
              family = bernoulli())

# model <- read_rds("outputs/model/brms_model.rds")
summary(model)


### Step 3: Loading in the census data
census_data <- read_csv("outputs/data/census_data.csv")
head(census_data)

### Step 4: Apply the model to the poststratification data
# Here we are making predictions using our model 
# with some new data from the census, and we saving 
# the results of those predictions by adding a new 
# column to the census_data dataset called 'estimate'.
post_stratified_estimates <-
  model %>%
  tidybayes::add_predicted_draws(newdata =census_data) %>%
  rename(alp_predict =.prediction) %>%
  mutate(alp_predict_prop =alp_predict*cell_prop_of_division_total) %>%
  group_by(state, .draw) %>%
  summarise(alp_predict =sum(alp_predict_prop)) %>%
  group_by(state) %>% 
  summarise(mean =mean(alp_predict),
            lower =quantile(alp_predict,0.025),
            upper =quantile(alp_predict,0.975))

post_stratified_estimates
