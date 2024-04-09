### Variable Selection on Adj R2, forward stepwise ###

## set seed
set.seed(42)

## install packages
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(caret)

## read in data with pca completed on cts variables (data6)

data6 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/data6.csv")
data6 <- subset(data6, select = -X)

## Model just on continuous variables with PCA

model_cts <- lm(data = data6, formula = ClosePrice~PC1+PC2+PC3+PC4)

summary(model_cts) # R2 = 0.5962, Adj R2 = 0.5962

## get array of factors to consider

# get factors from colnames
possible_factors <- colnames(data6)

# remove PCA columns and sales date (lookahead bias) and price (dependent var)
possible_factors <- possible_factors[possible_factors != "PC1" &
                                       possible_factors != "PC2" &
                                       possible_factors != "PC3" &
                                       possible_factors != "PC4" &
                                       possible_factors != "ClosePrice" &
                                       possible_factors != "CloseDate"]

## Forward stepwise regression on PCA model

# Adj R2 of PCA only model as starting point
current_adj_r2 <- summary(model_cts)$adj.r.squared

# starting factors
selected_factor_set <- c("PC1", "PC2", "PC3", "PC4")

# loop through factors and see if they improve Adj R2
for (i in 1:length(possible_factors)) {
  # pick factor to test from possible_factors
  new_factor <- possible_factors[i]
  
  # create a test factor set and formula for test model
  test_factor_set <- c(selected_factor_set, new_factor)
  test_factor_formula <- paste("ClosePrice~", paste(test_factor_set, collapse = "+"), collapse = "")
  
  # fit test model and save Adj R2
  test_model <- lm(data = data6, formula = test_factor_formula)
  test_adj_r2 <- summary(test_model)$adj.r.squared
  
  # if Adj R2 is improved > 1%, add factor to selected_factor_set and update current Adj R2
  if (test_adj_r2/current_adj_r2 >= 1.01) {
    selected_factor_set <- test_factor_set
    current_adj_r2 <- test_adj_r2
  }
}

# check selected factors
selected_factor_set # 11 factors
# "PC1", "PC2", "PC3", "PC4", "HOAMandatory", "NewConstruction", "Geo_West.University.Southside.Area",
# "Geo_Memorial.Villages", "Geo_River.Oaks.Area", "Geo_Tanglewood.Area", "Zip_77024"

# create dataframe with only the factors plus depedent var due to caret formula input peculiarities
data_forward_stepwise_model <- data6[c(selected_factor_set, "ClosePrice")]

## Cross-validation on model fitted with selected factors using caret
model_forward_stepwise <- train(ClosePrice ~ ., data = data_forward_stepwise_model, method = "lm",
                                trControl = trainControl(method = "cv", number = 10))

# see results
print(model_forward_stepwise)
summary(model_forward_stepwise) # Adj R2 = 0.7149


