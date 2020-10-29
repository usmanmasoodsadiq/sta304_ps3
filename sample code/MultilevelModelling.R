library(tidyverse) 
library(lme4)

### Loading in Data
setwd("C:/Users/Sammi-Jo/Desktop/ProblemSet2")
gss <- read_csv("gss.csv")

#################### FREQUENTIST
## Random Intercept Model - Linear Regression
model_linear <- lmer(feelings_life ~ age + marital_status + (1|region),
            data = gss, 
            REML=F)

summary(model_linear)

## Random Intercept Model - Logistic Regression
model_logit <- glmer(as.factor(worked_last_week) ~ age +  (1|region),
                     data = gss, 
                     family=binomial)

summary(model_logit)
 


