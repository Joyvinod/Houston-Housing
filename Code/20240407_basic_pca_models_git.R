### 2024/04/07 regression modeling with recleaned data ###

### load recleaned data
data5 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/data5.csv")
data5 <- subset(data5, select = -X)

### Fit lm models

## Basic lm model, no PCA

# fit basic model, no cv, without CloseDate
model_basic <- lm(data = data5, formula = ClosePrice~.-CloseDate)

# basic model summary
summary(model_basic) # R squared of 0.7885, adjusted R squared of 0.7879

# look at significant coefficients
model_basic_coefs <- as.data.frame(summary(model_basic)$coefficients)

# restrict to p < 0.15
model_basic_coefs_015 <- subset(model_basic_coefs, `Pr(>|t|)` < 0.15)
model_basic_coefs_015 <- model_basic_coefs_015[order(model_basic_coefs_015$`Pr(>|t|)`), ]


## PCA on continuous variables

# select continuous variables
data5_cts <- subset(data5, select = c(BathsTotal, BedsTotal, SqFtTotal, YearBuilt))

# calculate principal components
data5_cts_pcs <- as.data.frame(prcomp(data5_cts, scale = T)$x)

# replace continuous variables in original df
data6 <- subset(data5, select = -c(BathsTotal, BedsTotal, SqFtTotal, YearBuilt))
data6 <- cbind(data5_cts_pcs, data6)

# fit basic PCA model, no cv, without CloseDate
model_pca <- lm(data = data6, formula = ClosePrice~.-CloseDate)

# PCA model summary
summary(model_pca) # R squared of 0.7885, adjusted R squared of 0.7879 (same as no PCA)

# look at significant coefficients
model_pca_coefs <- as.data.frame(summary(model_pca)$coefficients)

# restrict to p < 0.15
model_pca_coefs_015 <- subset(model_pca_coefs, `Pr(>|t|)` < 0.15)
model_pca_coefs_015 <- model_pca_coefs_015[order(model_pca_coefs_015$`Pr(>|t|)`), ]
