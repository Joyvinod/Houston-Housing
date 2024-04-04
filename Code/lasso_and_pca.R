rm(list = ls())
library(glmnet)
library(GGally)
library(corrplot)
library(MASS)
library(dplyr)



clean3 <- read.csv("20240314_cleaned_data.csv",stringsAsFactors=TRUE)
clean4 <- read.csv("20240314_cleaned_data.csv",stringsAsFactors= FALSE, header = TRUE)

clean4[is.na(clean4)] <- 0
newdf <- clean4 %>% select(where(is.numeric))


#base_lm <-   lm(ClosePrice~ BathsTotal + BedsTotal + SqFtTotal + CloseDate + Siding
#  + CoolSystem + CountyOrParish + HeatSystem + HOAMandatory + GeoMarketArea + NewConstruction + PoolPrivate
#  + DwellingType + Restrictions + Roof + SchoolDistrict + WaterSewer + YearBuilt + PostalCode, data = clean3)
#base_lm <-   lm(ClosePrice~. , data = clean3)

#summary(base_lm)
model_lasso <- cv.glmnet(x = as.matrix(clean3[, -22]), y = as.matrix(clean3[,22]), type.measure = "mse",
          nfolds = 10, alpha = 1, nlambda = 20, family = "gaussian", standardize = TRUE)

plot(model_lasso)
model_lasso$lambda.min
cbind(model_lasso$lambda, model_lasso$cvm, model_lasso$nzero)
coef(model_lasso, s = model_lasso$lambda.min)
lasso_lm <- lm(ClosePrice~ HOAMandatory + NewConstruction + SqFtTotal + PoolPrivate, data = clean3)
summary(lasso_lm)

PCA <- prcomp(newdf[,2:9], center = TRUE, scale.= TRUE)
summary(PCA)
PCA$rotation
screeplot(PCA, type = "lines", col = "red")
PC <- PCA$x[,1:5]
clean4PC <- cbind(PC, newdf[,1])
modelPCA <- lm(V6~., data = as.data.frame(clean4PC))
summary(modelPCA)
x = 5