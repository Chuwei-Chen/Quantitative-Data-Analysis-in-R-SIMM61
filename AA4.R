library(lavaan)
library(semPlot)
library(semptools)
library(tidyverse)
library(CompQuadForm)
library(ICS)

install.packages("psychTools")
library(psychTools)
my_data = holzinger.swineford

modelA<-'
visual=~ t01_visperc+ t02_cubes+ t03_frmbord+t04_lozenges
verbal=~ t06_paracomp+t07_sentcomp+t09_wordmean
speed=~ t10_addition+t12_countdot+t13_sccaps

'
fit <- sem(modelA, data = my_data)
plot = semPaths(fit, label.scale=F, nCharNodes = 8,
                sizeMan2=3.5, sizeMan=10, asize=3, edge.color="black", residuals = F, fixedStyle = 1)
summary(fit)

#check multivariate normality
mvnorm.kur.test(my_data[,c("t01_visperc", "t02_cubes", "t03_frmbord", "t04_lozenges","t06_paracomp","t07_sentcomp","t09_wordmean","t10_addition","t12_countdot","t13_sccaps")])