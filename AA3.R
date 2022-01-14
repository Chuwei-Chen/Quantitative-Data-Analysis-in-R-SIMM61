library(GGally)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(paran) 
library(psych) 
library(car)
library(GPArotation)
library(MVN)
library(ICS) 
library(tidyverse)
library(dplyr)

#Load custom function
#extracted from Zoltan Kekecs
fviz_loadnings_with_cor <- function(mod, axes = 1, loadings_above = 0.4){	
  require(factoextra)	
  require(dplyr)	
  require(ggplot2)
  
  
  if(!is.na(as.character(mod$call$call)[1])){	
    if(as.character(mod$call$call)[1] == "PCA"){	
      contrib_and_cov = as.data.frame(rbind(mod[["var"]][["contrib"]], mod[["var"]][["cor"]]))	
      
      vars = rownames(mod[["var"]][["contrib"]])	
      attribute_type = rep(c("contribution","correlation"), each = length(vars))	
      contrib_and_cov = cbind(contrib_and_cov, attribute_type)	
      contrib_and_cov	
      
      plot_data = cbind(as.data.frame(cbind(contrib_and_cov[contrib_and_cov[,"attribute_type"] == "contribution",axes], contrib_and_cov[contrib_and_cov[,"attribute_type"] == "correlation",axes])), vars)	
      names(plot_data) = c("contribution", "correlation", "vars")	
      
      plot_data = plot_data %>% 	
        mutate(correlation = round(correlation, 2))	
      
      plot = plot_data %>% 	
        ggplot() +	
        aes(x = reorder(vars, contribution), y = contribution, gradient = correlation, label = correlation)+	
        geom_col(aes(fill = correlation)) +	
        geom_hline(yintercept = mean(plot_data$contribution), col = "red", lty = "dashed") + scale_fill_gradient2() +	
        xlab("variable") +	
        coord_flip() +	
        geom_label(color = "black", fontface = "bold", position = position_dodge(0.5))	
      
      
    }	
  } else if(!is.na(as.character(mod$Call)[1])){	
    
    if(as.character(mod$Call)[1] == "fa"){	
      loadings_table = mod$loadings %>% 	
        matrix(ncol = ncol(mod$loadings)) %>% 	
        as_tibble() %>% 	
        mutate(variable = mod$loadings %>% rownames()) %>% 	
        gather(factor, loading, -variable) %>% 	
        mutate(sign = if_else(loading >= 0, "positive", "negative"))	
      
      if(!is.null(loadings_above)){	
        loadings_table[abs(loadings_table[,"loading"]) < loadings_above,"loading"] = NA	
        loadings_table = loadings_table[!is.na(loadings_table[,"loading"]),]	
      }	
      
      if(!is.null(axes)){	
        
        loadings_table = loadings_table %>% 	
          filter(factor == paste0("V",axes))	
      }	
      
      
      plot = loadings_table %>% 	
        ggplot() +	
        aes(y = loading %>% abs(), x = reorder(variable, abs(loading)), fill = loading, label =       round(loading, 2)) +	
        geom_col(position = "dodge") +	
        scale_fill_gradient2() +	
        coord_flip() +	
        geom_label(color = "black", fill = "white", fontface = "bold", position = position_dodge(0.5)) +	
        facet_wrap(~factor) +	
        labs(y = "Loading strength", x = "Variable")	
    }	
  }	
  
  
  
  
  
  
  return(plot)	
  
}	

#load dataset
ar <- read_csv("https://raw.githubusercontent.com/kekecsz/SIMM61-Course-materials/main/Exercise_06%20-%20CFA%20and%20EFA/animalrights.csv")	
view(ar)
sum(is.na(ar))
#it returns 11 missing values, so we drop the missing value
ar <- ar %>%
  drop_na()	

ar %>% 	
  describe()

#Explore data
str(ar)

summary(ar)


#check outliers using cook's distance
mod_out <- lm(liberal ~ ., data=ar)
cooksd <- cooks.distance(mod_out)
sort(cooksd, decreasing = TRUE) %>% head()
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # draw cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")
#potential outliers: 57,34,104,111,23,113

car::outlierTest(mod_out)
#111

#or use qqPlot to check outliers
qqPlot(mod_out)
#potential outliers:57,111
install.packages('outliers')
library(outliers)
rm.outlier(ar, fill = TRUE, median = FALSE, opposite = FALSE)

#checking data
mod_allitems = lm(liberal ~ ar1 + ar2 + ar3 + ar4 + ar5 + ar6 + ar7 + ar8 + ar9 + ar10 +	
                    ar11 + ar12 + ar13 + ar14 + ar15 + ar16 + ar17 + ar18 + ar19 + ar20 +	
                    ar21 + ar22 + ar23 + ar24 + ar25 + ar26 + ar27 + ar28,	
                  data = ar)	

summary(mod_allitems)

vif(mod_allitems) #ar5 and ar13 has vif larger than 3

ar_items <- ar %>% 	
  dplyr::select(ar1:ar28)

cor = ar_items %>% 	
  cor()
cor	

#Exploratory Factor Analysis
#factorability
ar_cor <- cor(ar_items)

#KMO test
KMO(ar_cor)#all KMO index is larger than 0.6, so the data is factorable

#multivariate normality
result <- mvn(ar_items[, 1:28], mvnTest = "hz")
result$multivariateNormality

mvnorm.kur.test(na.omit(ar_items[, 1:28]))

mvnorm.skew.test(na.omit(ar_items[, 1:28]))
#these tests returns p-value lower than 0.05, which violates the assumption of normality

#paf extraction model
#choosing ideal numbers of factors
#scree test
fa.parallel(ar_cor, n.obs = nrow(ar_items), fa = "fa", fm = "pa")#which suggest we choose 4 factors
#VSS technique
nfactors(ar_cor, n.obs = nrow(ar_items))
#parallel test
paran(ar_items,graph =TRUE)

# - scree test: 5	
# - Parallel test: 2	
# - VSS: 1-2	
# - MAP: 2

EFA_mod1 <- fa(ar_cor, nfactors = 5, fm = "pa")
EFA_mod1

#communality
EFA_mod1_common <- as.data.frame(sort(EFA_mod1$communality, decreasing = TRUE))
EFA_mod1_common
mean(EFA_mod1$communality)#overall communality 0.44

#Based on the tests we decide to use 2 factors
EFA_mod2 <- fa(ar_cor, nfactors = 2, fm = "pa")
EFA_mod2

#communality
EFA_mod2_common <- as.data.frame(sort(EFA_mod1$communality, decreasing = TRUE))
EFA_mod2_common
mean(EFA_mod2$communality)#overall communality 0.34

#factor rotation
EFA_mod2$rotation
fa(ar_cor, nfactors = 2, fm = "pa", rotate = "oblimin")

EFA_mod_promax <- fa(ar_cor, nfactors = 2, fm = "pa", rotate = "promax")
fa.diagram(EFA_mod_promax)
EFA_mod_promax

EFA_mod_varimax <- fa(ar_cor, nfactors = 2, fm = "pa", rotate = "varimax")
fa.diagram(EFA_mod_varimax)
EFA_mod_varimax

fa.diagram(EFA_mod2)

fviz_loadnings_with_cor(EFA_mod2, axes = 1, loadings_above = 0.35)
fviz_loadnings_with_cor(EFA_mod2, axes = 2, loadings_above = 0.35)

#final model#based on low loadings on both factors
ar_test_fin <- subset(ar_items, select = -c(1,3,8,11,14,16,21,22))

cor_fin = ar_test_fin %>% 	
  cor()
cor_fin

ar_cor_fin <- cor(ar_test_fin)
EFA_mod_fin <- fa(ar_cor_fin, nfactors = 2, fm = "pa")
EFA_mod_fin

EFA_fin_common <- as.data.frame(sort(EFA_mod_fin$communality, decreasing = TRUE))
EFA_fin_common
mean(EFA_mod_fin$communality)#0.40

EFA_fin_promax <- fa(ar_cor_fin, nfactors = 2, fm = "pa", rotate = "promax")
fa.diagram(EFA__fin_promax)
EFA_fin_promax

EFA_fin_varimax <- fa(ar_cor_fin, nfactors = 2, fm = "pa", rotate = "varimax")
fa.diagram(EFA_fin_varimax)
EFA_fin_varimax #choose varimax method

#saving factor scores
ar_final <- subset(ar, select = -c(1,3,8,11,14,16,21,22))
factorscores = factor.scores(ar_final[,1:20], EFA_fin_varimax)$scores
ar_factorscroes = cbind(ar_final, factorscores)

#build the 2-factor linear model, 2 factors as predictors and liberal as outcome variable
mod_EFA <- lm(liberal ~ PA1+
                PA2, 
              data = ar_factorscroes)
summary(mod_EFA)