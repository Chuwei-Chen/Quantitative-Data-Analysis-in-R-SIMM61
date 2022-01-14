library(psych) 
library(tidyverse)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)

#custom function
#extracted from https://stackoverflow.com/questions/25142901/standardized-coefficients-for-lmer-model
stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
}

#Load dataset
surgery_A = read_csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/surgery_data_1.csv")
view(surgery_A)
#there is a coding error in case ID_25, where the sex is coded as "woman", and need to be changed to "female"
surgery_A$sex[surgery_A$sex=="woman"]<- "female"

surgery_B = read_csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/surgery_data_2.csv")
view(surgery_B)

#Assign hospital as a grouping factor
surgery_A = surgery_A %>%
  mutate(hospital = factor(hospital))

surgery_B = surgery_B %>%
  mutate(hospital = factor(hospital))

#checking dataset: clustering in different hospitals
surgery_A %>% 		
  ggplot() +		
  aes(y = pain, x = STAI_trait) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)

STAI_plot = surgery_A %>% 		
  ggplot() +		
  aes(y = pain, x = STAI_trait, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)		

STAI_plot
#hospital 10 has the largest slope, and hospital 9 shows a parallel fitted line(maybe STAI_trait does not significantly influence in this hospital)

pain_cat_plot = surgery_A %>% 		
  ggplot() +		
  aes(y = pain, x = pain_cat, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)		

pain_cat_plot

mindfulness_plot = surgery_A %>% 		
  ggplot() +		
  aes(y = pain, x = mindfulness, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)		

mindfulness_plot

cortisol_serum_plot = surgery_A %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)		

cortisol_serum_plot

sex_plot = surgery_A %>% 		
  ggplot() +		
  aes(y = pain, x = sex, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)		

sex_plot

age_plot = surgery_A %>% 		
  ggplot() +		
  aes(y = pain, x = age, color = hospital) +		
  geom_point(size = 4) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)		

age_plot

#the above plots indicate there exist clustering in different hospitals
#meaning the hospital has an effect on intercept of the outcome, and the effect of the fixed effect predictor (slope) in the surgery A dataset

#Build a random intercept model
mod_A_int = lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1 | hospital), data = surgery_A)
summary(mod_A_int)
confint(mod_A_int)
library(sjPlot)
tab_model(mod_A_int, show.ci = 0.95, show.intercept = TRUE, show.est = TRUE, show.aic = TRUE, show.stat = TRUE, show.r2=TRUE, p.style = "stars", title = "Table 1. Random Intercept Model")

#Calculate marginal R^2 and conditional R^2for Surgery_A
r.squaredGLMM(mod_A_int)
cAIC(mod_A_int)$caic
#predict surgery_B
pred_B <- predict(mod_A_int,newdata=surgery_B,allow.new.levels=TRUE)

#Calculate R^2 for surgery_B
#Calculate RSS
RSS_B = sum((surgery_B$pain - pred_B)^2)
RSS_B
#Calculate TSS
mod_B_mean <- lm(pain ~ 1, data = surgery_B)
TSS_B = sum((surgery_B$pain - predict(mod_B_mean))^2)
TSS_B
#R^2
R2_B = 1 - (RSS_B/TSS_B)
R2_B #0.38

#Build final model
r2beta(mod_A_int, method = "nsj", data = surgery_A)#cortisol_serum is the most influencial predictor

mod_final_int = lmer(pain ~ cortisol_serum + (1 | hospital), data = surgery_A)
summary(mod_final_int)

mod_final_slope = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = surgery_A)
summary(mod_final_slope)

#visualization
surgery_A = surgery_A %>%
  mutate(pred_int = predict(mod_final_int), pred_slope = predict(mod_final_slope))

#random intercept model plot
surgery_A %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                                                       aes(y = pred_int, x = cortisol_serum)) + facet_wrap(~hospital,
                                                                                                        ncol = 2)
#random intercept and random slope model plot
surgery_A %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                                                       aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital,
                                                                                                           ncol = 2)

#comparing random intercept and random slope model
cAIC(mod_final_int)$caic #664.5371
cAIC(mod_final_slope)$caic #664.5371
anova(mod_final_int, mod_final_slope)

summary(mod_final_slope)
tab_model(mod_A_int, show.ci = 0.95, show.intercept = TRUE, show.est = TRUE, show.aic = TRUE, show.stat = TRUE, show.r2=TRUE, p.style = "stars", title = "Table 1. Random Intercept Model")
