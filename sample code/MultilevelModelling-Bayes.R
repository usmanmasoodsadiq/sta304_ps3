setwd("C:/Users/Sammi-Jo/Desktop/Archive")

library(broom) # Helps make the regression results tidier
library(tidyverse) # Helps make programming with R easier
library(brms) # Allows for Bayesian inference

### Loading in the Poll data
example_poll <- read_csv("outputs/data/example_poll.csv")

head(example_poll)


### Multilevel Logistic Model for different states
model_states <- brm(supports_ALP ~ gender + age_group +
                       (1|state),
                       data = example_poll, 
                       family = bernoulli())

summary(model_states)