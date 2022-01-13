#Load Packages
library(tidyverse)
library(psych)
library(gridExtra)
library(pscl)
library(lmtest)
library(dominanceanalysis)	

#Load dataset
titanic_training =
  read_csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Home_assignment/Titanic%20-%20training%20set.csv")

#Check data
View(titanic_training)
titanic_training %>%
  summary()
describe(titanic_training)
#recode survived as categorical variable 
titanic_training = titanic_training %>% 	
  mutate(Survived = factor(recode(Survived,
                                  "0" = "No",
                                  "1" = "Yes")),
         sex = factor(recode(Sex,
                              "1" = "male",
                              "0" = "female")))

#dealing with missing value(using mean to replace NA value)
colSums(is.na(titanic_training)) #177 missing value in "Age"
titanic_training$Age[is.na(titanic_training$Age)] <- mean(titanic_training$Age,na.rm=T)
sum(is.na(titanic_training$Age))

#Exploratory Analysis visually
titanic_training %>% 	
  ggplot() +	
  aes(x = Pclass) +	
  geom_bar()

titanic_training %>% 	
  ggplot() +	
  aes(x = Sex) +	
  geom_bar()

titanic_training %>% 	
  ggplot() +	
  aes(x = SibSp) +	
  geom_bar()

titanic_training %>% 	
  ggplot() +	
  aes(x = Parch) +	
  geom_bar()

#The relationship between numbers of siblings or spouses and survival
titanic_training %>% 	
  group_by(Survived) %>% 	
  summarize(mean = mean(SibSp),	
            sd = sd(SibSp))	

titanic_training %>% 	
  ggplot() +	
  aes(y = SibSp, x = Survived) +	
  geom_violin(aes(fill = Survived)) +	
  geom_boxplot() +	
  geom_jitter(width = 0.1)

#The relationship between numbers of parents or children with survival
titanic_training %>% 	
  group_by(Survived) %>% 	
  summarize(mean = mean(Parch),	
            sd = sd(Parch))

titanic_training %>% 	
  ggplot() +	
  aes(y = Parch, x = Survived) +	
  geom_violin(aes(fill = Survived)) +	
  geom_boxplot() +	
  geom_jitter(width = 0.1)

#The relationship between age and survival
#Fill in the missing value using the mean of age
titanic_training$Age[is.na(titanic_training$Age)] <- mean(titanic_training$Age,na.rm=T)
sum(is.na(titanic_training$Age))

titanic_training %>% 	
  group_by(Survived) %>% 	
  summarize(mean = mean(Age),	
            sd = sd(Age))

titanic_training %>% 	
  ggplot() +	
  aes(y = Age, x = Survived) +	
  geom_violin(aes(fill = Survived)) +	
  geom_boxplot() +	
  geom_jitter(width = 0.1)

titanic_training %>%
  ggplot()+
  aes(x=Age,fill=Survived)+
  geom_histogram(binwidth =3)

#The relationship between class and survival
titanic_training %>% 	
  group_by(Survived, Pclass) %>% 	
  summarize(n = n()) %>% 	
  spread(Survived, n)	

titanic_training %>% 	
  ggplot() +	
  aes(x = Pclass, fill = Survived) +	
  geom_bar()

#The relationship between sex and survival
titanic_training %>% 	
  group_by(Survived, Sex) %>% 	
  summarize(n = n()) %>% 	
  spread(Survived, n)	

titanic_training %>% 	
  ggplot() +	
  aes(x = Sex, fill = Survived) +	
  geom_bar()

#The relationship between Embarked and survival
titanic_training %>% 	
  group_by(Survived, Embarked) %>% 	
  summarize(n = n()) %>% 	
  spread(Survived, n)	

titanic_training %>% 	
  ggplot() +	
  aes(x = Embarked, fill = Survived) +	
  geom_bar()

#The relationship between fare and survival
titanic_training %>% 	
  group_by(Survived) %>% 	
  summarize(mean = mean(Fare),	
            sd = sd(Fare))

titanic_training %>% 	
  ggplot() +	
  aes(y = Fare, x = Survived) +	
  geom_violin(aes(fill = Survived)) +	
  geom_boxplot() +	
  geom_jitter(width = 0.1)

#Based on the previous result, these predictors seem to have correlation: Pclass, Sex, Age, SibSp, Parch, and Fare

#Build logistic regression model
#Select variables
model_test <- titanic_training %>%
  select(Survived,
         Sex,
         Age,
         SibSp,
         Parch,
         Pclass)

mod1 = glm(Survived~., data = model_test, family = binomial)
summary(mod1)
exp(confint(mod1))

library(sjPlot)
tab_model(mod1, show.ci = 0.95, show.intercept = TRUE, show.est = TRUE, show.aic = TRUE, show.stat = TRUE, show.r2=TRUE, p.style = "stars")

#predictions for Sue and Kate with Leonardo
pred1 = data.frame(
  Age = c(5, 20),
  Sex = c("female","female"),
  SibSp = c(0, 1),
  Parch = c(2,1),
  Pclass = c(3,3)
) #with Leonardo

pred2 = data.frame(
  Age = c(5, 20),
  Sex = c("female","female"),
  SibSp = c(0, 0),
  Parch = c(1,1),
  Pclass = c(3,3)
) #Without Leonardo

#Sue and Kate with Leonardo
predict(mod1,pred1,interval="confidence")

exp(1.36)/(1+exp(1.36))
exp(0.5)/(1+exp(0.5))

#Sue and Kate without Leonardo
predict(mod1,pred2,interval="confidence")
exp(1.44)/(1+exp(1.44))
exp(0.84)/(1+exp(0.84))

#Model Performance
#McFadden R square
pR2(mod1)
# -2LL, deviance
pR2(mod1)["llh"] * -2

#Prediction Accuracy
model_test = model_test %>%
  mutate(pred_mod1 = predict(mod1)) %>%
  mutate(pred_mod1 = case_when(pred_mod1 <= 0 ~ "No",
                               pred_mod1 > 0 ~ "Yes"))
model_test = model_test %>%
  mutate(correct_prediction = case_when(pred_mod1 == Survived ~ "correct",
                                        pred_mod1 != Survived ~ "incorrect"))

model_test %>%
  group_by(correct_prediction) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

#Accuracy is 704/891=79.01%

#Comparison with null model
mod_null = glm(Survived ~ 1, family = binomial(), data = model_test)
summary(mod_null)
head(predict(mod_null))
pR2(mod_null)["llh"] * -2
#did not survive:549/891=62%
#survived: 342/891=38%
#did not survive vs survived: 549/342=1.6, log(1.6)=0.47

#percentage of survival number
model_test %>%
  group_by(Survived) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

#Crosstabs of survival and predicted values
model_test %>%
  group_by(Survived, pred_mod1) %>%
  summarize(n = n()) %>%
  spread(Survived, n)

#Correctly categorized as did not survived
model_test %>%
  filter(Survived == "No") %>%
  group_by(correct_prediction) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

#Correctly categorized as survived
model_test %>%
  filter(Survived == "Yes") %>%
  group_by(correct_prediction) %>%
  summarise(count = n()) %>%
  mutate(freq = count / sum(count))

#is the model better than null model?
lrtest(mod_null, mod1)
#the constructed model has larger value of loglikelihood, so it is better than the null model

#test added value
dominance_mod1<-dominanceAnalysis(mod1)
contributionByLevel(dominance_mod1, fit.functions="r2.m")
plot(dominance_mod1, which.graph ="conditional",fit.function = "r2.m")

averageContribution(dominance_mod1,fit.functions = "r2.m")
plot(dominance_mod1, which.graph ="general",fit.function = "r2.m") + coord_flip()