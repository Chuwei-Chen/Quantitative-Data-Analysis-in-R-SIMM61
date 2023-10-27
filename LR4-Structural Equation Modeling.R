library(lavaan)
library(semPlot)
library(semptools)
library(tidyverse)
library(CompQuadForm)
library(ICS)
library(psychTools)

#import dataset
my_data = holzinger.swineford

#specify the SEM model
modelA<-'
visual=~ t01_visperc+ t02_cubes+ t03_frmbord+t04_lozenges
verbal=~ t06_paracomp+t07_sentcomp+t09_wordmean
speed=~ t10_addition+t12_countdot+t13_sccaps

'

#fit the model
fit <- sem(modelA, data = my_data)
#visualization
semPaths(fit)

summary(fit)

#check multivariate normality
mvnorm.kur.test(my_data[,c("t01_visperc", "t02_cubes", "t03_frmbord", "t04_lozenges","t06_paracomp","t07_sentcomp","t09_wordmean","t10_addition","t12_countdot","t13_sccaps")])
mvnorm.skew.test(my_data[,c("t01_visperc", "t02_cubes", "t03_frmbord", "t04_lozenges","t06_paracomp","t07_sentcomp","t09_wordmean","t10_addition","t12_countdot","t13_sccaps")])

#solution with robust SE and test statistics
fit_MLM <- sem(modelA, data = my_data, estimator = "MLM")
summary(fit_MLM, fit.measures = T)

#task 2:build modelB in case of t10 and t12 has similar structure
modelB<-'
visual=~ t01_visperc+ t02_cubes+ t03_frmbord+t04_lozenges
verbal=~ t06_paracomp+t07_sentcomp+t09_wordmean
speed=~ t10_addition+t12_countdot+t13_sccaps
t10_addition~~t12_countdot

'

fit2 <- sem(modelB, data = my_data)

semPaths(fit2,whatLabels = "std")

summary(fit2)

#solution with robust SE and test statistics
fit2_MLM <- sem(modelB, data = my_data, estimator = "MLM")
summary(fit2_MLM, fit.measures = T)
semPaths(fit2_MLM, whatLabels = "std")

#compare model A and model B
anova(fit, fit2)

#mediation model
model_mediation = "
t13_sccaps ~ c*t01_visperc + b*t12_countdot
t12_countdot ~ a*t01_visperc

indirect := a*b

total := c + (a*b)
"
fit_mediation = sem(model_mediation, data = my_data)
semPaths(fit_mediation, fixedStyle = 1, label.scale=F, nCharNodes = 0,
         sizeMan2=5, sizeMan=15, asize=3, edge.label.cex = 1)
summary(fit_mediation)
