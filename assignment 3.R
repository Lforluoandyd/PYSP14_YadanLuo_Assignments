# load packages in need

library(psych) # for describe
library(tidyverse) # for tidy format and ggplot
library(cAIC4) # for cAIC
library(r2glmm) # for r2beta
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM
library(optimx) # for optimx optimizer

# Custom function

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}


# Read the dataset

home_sample_3 <- read.csv("https://tinyurl.com/ha-dataset3")
home_sample_4 <- read.csv("https://tinyurl.com/ha-dataset4")
home_sample_3 <- home_sample_3 %>% mutate(cortisol = (cortisol_saliva + cortisol_serum)/2) # mutate a new variable representing cortisol level, which if for being consisitent with assignment 1
home_sample_4 <- home_sample_3 %>% mutate(cortisol = (cortisol_saliva + cortisol_serum)/2)
View(home_sample_3)


# Build  linear mixed model and calculate the parameters

model_int <- lmer(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol + (1|hospital), data = home_sample_3)
summary(model_int)
stdCoef.merMod(model_int)
confint(model_int)
r2beta(model_int, method = "nsj", data = home_sample_3)
r.squaredGLMM(model_int)


# Predict

RSS_int <- sum((home_sample_4$pain - predict(model_int,newdata = home_sample_4))^2)
TSS_int <- sum((home_sample_4$pain - mean(home_sample_4$pain))^2)
R2_int <- 1-(RSS_int/TSS_int)


# New mixed linear model

model_int_new <- lmer(pain ~ cortisol + (1|hospital), data = home_sample_3)
summary(model_int_new)


# Visualize fitted regression lines

home_sample_3 <- home_sample_3 %>% mutate(pred_int = predict(model_int_new))
home_sample_3 %>%
  ggplot() +
  aes(y = pain, x = cortisol, group = hospital)+
  geom_point(aes(color = hospital), size = 4) +
  geom_line(color='black', aes(y = pred_int, x = cortisol))+
  facet_wrap( ~ hospital, ncol = 5)


