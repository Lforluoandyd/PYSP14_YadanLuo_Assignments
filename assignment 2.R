# Load the packages that may be used

library(tidyverse) #for tidy format
library(psych) # for describe
library(lm.beta) # for lm.beta
library(gridExtra) #for general visualization
library(car)
library(lmtest)

# Read the dataset

home_sample_1 <- read.csv("https://tinyurl.com/ha-dataset1")
home_sample_1  <- home_sample_1[-c(93,150),]
View(home_sample_1)

# Data diagnostics

weight_check <- home_sample_1 %>% # visualize irregularities
  ggplot()+
  aes(x = weight)+
  geom_histogram()
IQ_check <- home_sample_1 %>%
  ggplot()+
  aes(x = IQ)+
  geom_histogram()
household_income_check <- home_sample_1 %>%
  ggplot()+
  aes(x = household_income)+
  geom_histogram()
grid.arrange(weight_check, IQ_check, household_income_check, ncol = 1)

home_sample_1 %>% filter(household_income < 0)
home_sample_1 <- home_sample_1[-109,] # ID_109 should be excluded since the household income in below 0, which does not make sense.

# Biuld backward regression model

model_3 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income,
                     data = home_sample_1)
summary(model_3)

model_3_train <- step(model_3, direction = "backward")
summary(model_3_train)
anova(model_3_train,model_3)

# Model diagnostics

# outliers

model_3_train %>% plot(which = 5) #cook's distance
model_3_train %>% plot(which = 4)

home_sample_1 %>% slice(c(103, 114, 148)) # check outliers, it seems these values are valid rather than results of error

# normality

model_3_train %>% plot(which = 2) #QQ plot
model_3_train_residulas <- enframe(residuals(model_3_train))
model_3_train_residulas %>% ggplot()+
  aes(x = value) +
  geom_histogram() #histogram of residuals
describe(model_3_train_residulas) # check skew and kurtosis, the values approximately equals 0, showing the residuals meet the requirement of normality

# linearity

model_3_train %>% residualPlots() # all p values are above 0.05, indicating the model meet the assumption of linearity

# homoscedasticity

model_3_train %>% ncvTest() 
model_3_train %>% bptest() # p value > 0.05, no homoscedasticity

# no multicollinearity

model_3_train %>% vif() # all vif values are below 3, fulfilling the requirements of no multicollinearity


# Model comparison 

backward_model <- model_3_train
theorybased_model <- model_2_rebuild

anova(model_3,backward_model)
AIC(model_3)
AIC(backward_model)
AIC(theorybased_model)


# Parameters of backward model

confint(backward_model)
lm.beta(backward_model)

# Predict

home_sample_2 <- read.csv("https://tinyurl.com/ha-dataset2")
home_sample_2 <- home_sample_2 %>% mutate(cortisol = (cortisol_saliva + cortisol_serum)/2)
RSS_backward <- sum((home_sample_2$pain - predict(backward_model, newdata = home_sample_2))^2)
RSS_theory <- sum((home_sample_2$pain - predict(theorybased_model, newdata = home_sample_2))^2)
