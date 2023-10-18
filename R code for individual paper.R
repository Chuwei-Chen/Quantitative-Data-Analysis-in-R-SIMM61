#Setting work directory
getwd()
setwd("D:/Master/Assignments/SIMM61/Assignment 1-Dataset")
library(tidyverse)
library(haven)

#Import WVS Wave 7 data in sav format and look at the dataset
WVS7 <- read_sav("World_Values_Survey_Wave_7_Inverted_sav_v2_0.sav")
View(WVS7)

#Filter Chinese cases
WVS7_US <- WVS7 %>%
  filter(B_COUNTRY ==840)
View(WVS7_US)

#Inspect the independent variable
head(WVS7_CN$Q69P)
head(WVS7_CN$Q70P)
head(WVS7_CN$Q71P)
head(WVS7_CN$Q72P)
head(WVS7_CN$Q73P)
head(WVS7_CN$Q74P)
#Inspect the dependent variables
head(WVS7_CN$Q196P)
head(WVS7_CN$Q197P)
head(WVS7_CN$Q198P)
#Control variables
head(WVS7_CN$Q260)
head(WVS7_CN$Q262)
head(WVS7_CN$Q275)
head(WVS7_CN$Q260)
head(WVS7_CN$Q284)
head(WVS7_CN$Q287P)
head(WVS7_CN$Q288)

#Select variables
WVS_Selected2 <- WVS7_US %>%
  mutate(confidence_police = as.numeric(Q69P), 
         confidence_courts = as.numeric(Q70P), 
         confidence_gov = as.numeric(Q71P), 
         confidence_polpty = as.numeric(Q72P),
         confidence_parlmnt = as.numeric(Q73P), 
         confidence_civilser = as.numeric(Q74P), 
         survideo = as.numeric(Q198P), 
         surinternet = as.numeric(Q199P), 
         surcollect = as.numeric(Q200P), 
         gender = as.numeric(Q260), 
         age = as.numeric(Q262),
         edu = as.numeric(Q275),
         workgov = as.numeric(Q284),
         socclass = as.numeric(Q287P),
         houseincome = as.numeric(Q288)) %>% 
  select(confidence_police,
         confidence_courts,
         confidence_gov, 
         confidence_polpty, 
         confidence_parlmnt, 
         confidence_civilser,
         survideo,
         surinternet,
         surcollect, 
         gender,
         age,
         edu, 
         workgov,
         socclass,
         houseincome)
#Now look at the selected data
view(WVS_Selected2)

#Drop variables with missing value
sum(is.na(WVS_Selected))
sum(complete.cases(WVS_Selected)) 
WVS_Selected[complete.cases(WVS_Selected),]
finaldata2 <- na.omit(WVS_Selected2)
sum(is.na(finaldata))
view(finaldata2)

#Note that the the version of data-file I used has been inverted positive direction of scales by WVS

#Organize Variables
#First I would like to recode variable "Q284" "are you working for 1 government or public institution, 2 priviate business or industry, 3 private NGO" to "do you work for the government or not"
finaldata$workgov[finaldata$workgov == 1] <- "1" 
#work for government 
finaldata2$workgov[finaldata2$workgov == 2 | finaldata2$workgov == 3] <- "2"  
#do not work for government
table(finaldata2$workgov)

#Create Index Variables
#Create Independent Variable "Confidence_gov"(confidence for government)
finaldata2 <- finaldata2 %>% 
  mutate(indexiv = (confidence_police + confidence_courts + confidence_polpty+ 
confidence_parlmnt+confidence_civilser))

#Create Dependent Variable "surv_support", support for government surveillance
finaldata2 <- finaldata2 %>% 
  mutate(indexdv = (survideo + surinternet + surcollect))

#Using Cronbach's Alpha test to conduct reliability analysis of index
install.packages("psych")
library(psych)

#test independent variable index
test_indexiv <- data.frame(WVS7_US$Q69P, 
                           WVS7_US$Q70P, 
                           WVS7_US$Q71P, 
                           WVS7_US$Q72P, 
                           WVS7_US$Q73P,
                           WVS7_US$Q74P)

alphaindexiv <- alpha(test_indexiv)
summary(alphaindexiv)
#The raw_alpha returns 0.89, indicating a relatively good internal consistency.

#test dependent variable index
test_indexdv <- data.frame(WVS7_US$Q196P,
                           WVS7_US$Q197P,
                           WVS7_US$Q198P)
alphaindexdv <- alpha(test_indexdv)
summary(alphaindexdv)
#The raw_alpha returns 0.73, indicating a relatively good internal consistency.

#Summary Statistics of focal X and Y
# Plot focal X
ggplot(finaldata, aes(indexiv)) + 
  geom_histogram() +
  theme_classic() + 
  ggtitle("Focal Independent Variable")

# Summary stats for focal X 
summary(finaldata$indexiv)

# Plot focal Y
ggplot(finaldata, aes(indexdv)) + 
  geom_histogram() +
  theme_classic() + 
  ggtitle("Focal Dependent Variable")

# Summary stats for focal Y 
summary(finaldata$indexdv)

#Examine the bi-variate relationship between independent variable and dependent variable
mod1 <- lm(indexdv ~ indexiv, 
           data = finaldata2)
summary(mod1)

# Add Control Variables to the model
# Model with control variables
mod2 <- lm(indexdv ~ indexiv +
             gender +
             age +
             edu +
             workgov +
             socclass +
             houseincome, 
           data = finaldata2)

summary(mod2)