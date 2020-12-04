#Load the packages that may be used

library(tidyverse) #for tidy format
library(psych) # for describe
library(lm.beta) # for lm.beta
library(gridExtra) #for general visualization
library(car)
library(lmtest)

# Read the dataset

home_sample_1 <- read.csv("https://tinyurl.com/ha-dataset1")


# Data exploration

View(home_sample_1)
summary(home_sample_1) # irregular values in variable "age" & "STAI_trait" detected

age_check <- home_sample_1 %>% # visualize irregularities
  ggplot()+
  aes(x = age)+
  geom_bar()
STAI_trait_check <- home_sample_1 %>%
  ggplot()+
  aes(x = STAI_trait)+
  geom_bar()
grid.arrange(age_check, STAI_trait_check, ncol = 2)

cortisol_serum_check <- home_sample_1 %>% 
  ggplot()+
  aes(x=cortisol_serum, fill = sex)+
  geom_density(alpha = 0.3)
cortisol_saliva_check <- home_sample_1 %>% 
  ggplot()+
  aes(x=cortisol_saliva, fill = sex)+
  geom_density(alpha = 0.3)
grid.arrange(cortisol_serum_check, cortisol_saliva_check, ncol = 2) # visualize the relationship between "sex" and "cortisol"

home_sample_1 %>% filter(age >400) 
home_sample_1 %>% filter(STAI_trait < 20) 
home_sample_1  <- home_sample_1[-c(93,150),]# ID_93 should be excluded since age = 444, ID_150 should be excluded since STAI_trait = 20
 

# Build model 1

model_1 <- lm(pain ~ sex + age, data = home_sample_1)
summary(model_1)


# Build model 2

model_2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = home_sample_1)
summary(model_2)


# Primary model comparison
anova(model_1, model_2)
AIC(model_1)
AIC(model_2)


# Model diognostics

# outliers

home_sample_1 %>% # visualization
  mutate(rownum = row.names(home_sample_1)) %>% 
  ggplot()+
  aes(x = pain_cat, y = pain, label = rownum)+
  geom_label()+
  geom_smooth(method = "lm")

model_2 %>% plot(which = 5) #cook's distance
model_2 %>% plot(which = 4)

home_sample_1 %>% slice(c(68, 100, 114)) # check outliers, it seems these values are valid rather than results of error

# normality

model_2 %>% plot(which = 2) #QQ plot
model_2_residuals <- enframe(residuals(model_2))
model_2_rebuild_residuals %>% ggplot()+
  aes(x = value) +
  geom_histogram() #histogram of residuals
describe(model_2_residuals) # check skew and kurtosis, the values approximately equals 0, showing the residuals meet the requirement of normality

#linearity

model_2 %>% residualPlots() # all p values are above 0.05, indicating the model meet the assumption of linearity

# homoscedasticity

model_2 %>% plot(which = 3)
model_2 %>% ncvTest()
model_2 %>% bptest() # p values are above 0.05, indicating the model meet the assumption of homoscedasticty

# no multicollinearity

model_2 %>% vif() # there is multicollinearity between cortisol_saliva and cortisol_serum(vif > 3), which makes sense, they are both mearsurements of cortisol

home_sample_1 %>% select(STAI_trait, pain_cat, mindfulness, cortisol_saliva, cortisol_serum) %>% 
  pairs.panels(col = "blue", lm = T) # correlation coeffcient of cortisol_saliva and cortisol_serum is 0.89, which is very high

home_sample_1 <- home_sample_1 %>% 
  mutate(cortisol = (cortisol_saliva + cortisol_serum)/2) # create a new variable representing cortisol

# rebulid model 2

model_2_rebuild <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol, data = home_sample_1)
summary(model_2_rebuild)

# recheck model 2

model_2_rebuild_residuals <- enframe(residuals(model_2_rebuild))
describe(model_2_rebuild_residuals) # normality

model_2_rebuild %>% residualPlots() # linearity

model_2_rebuild %>% ncvTest() 
model_2_rebuild %>% bptest() # homoscedasticity

model_2_rebuild %>% vif() # no multicollinearity


# Final model comparison

anova(model_1,model_2_rebuild)
AIC(model_2_rebuild)


# CIs and std.beta of models

confint(model_1)
confint(model_2_rebuild)

lm.beta(model_1)
lm.beta(model_2_rebuild)
