### Incorporating GoogleTrends data to stepwise model

## set seed
set.seed(42)

## install packages
library(caret)

### Data prep
## load data
# housing data
data7 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/data7.csv")
data7 <- subset(data7, select = -X)

# GoogleTrends data
gtrends <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/gtrends.csv")
gtrends <- subset(gtrends, select = -X)

# add CloseMonth to data7 so data can be combined
data7$CloseMonth <- format(as.Date(data7$CloseDate), "%Y-%m")

# merge dfs
data8 <- merge(data7, gtrends, by = "CloseMonth")


### Fit base regression model (PCA with stepwise regression var selection)
model_base <- train(ClosePrice ~ PC1 + PC2 + PC3 + PC4 + 
                      HOAMandatory + 
                      NewConstruction + 
                      Geo_West.University.Southside.Area + 
                      Geo_Memorial.Villages + 
                      Geo_River.Oaks.Area + 
                      Geo_Tanglewood.Area + 
                      Zip_77024, 
                    data = data8, 
                    method = "lm",
                    trControl = trainControl(method = "cv", number = 10))

summary(model_base) # Adj R2 = 0.7149


### Try fitting each of the GT data columns to see if they improve model R2

# components from base model
base_factor_set <- c("ClosePrice~PC1 + PC2 + PC3 + PC4 + 
                       HOAMandatory + 
                       NewConstruction + 
                       Geo_West.University.Southside.Area + 
                       Geo_Memorial.Villages + 
                       Geo_River.Oaks.Area + 
                       Geo_Tanglewood.Area + 
                       Zip_77024")
base_adj_r2 <- summary(model_base)$adj.r.squared

# df to hold results
gt_results <- data.frame("gt_metric" = c(), 
                         "lm_coefficient" = c(), 
                         "cv10_r2" = c(),
                         "cv10_adj_r2" = c(),
                         "improves_adj_r2" = c())

# gtrends columns to check
gt_possible_factors <- colnames(gtrends)
gt_possible_factors <- gt_possible_factors[gt_possible_factors != "CloseMonth"]

# loop through gtrends columns to fit model and record R2, Adj R2, coefficient
for (i in 1:length(gt_possible_factors)) {
  # pick factor to test from possible_factors
  new_factor <- gt_possible_factors[i]
  gt_results[i, "gtmetric"] <- new_factor
  
  # create a test factor set and formula for test model
  test_factor_set <- c(base_factor_set, new_factor)
  test_factor_formula <- as.formula(paste(test_factor_set, collapse = "+"))
  
  # fit test model
  test_model <- train(test_factor_formula, data = data8, method = "lm",
                      trControl = trainControl(method = "cv", 
                                               number = 10))
  
  # save test_factor coefficient
  test_coefs <- summary(test_model)$coefficient
  test_coef <- test_coefs[new_factor, "Pr(>|t|)"]
  gt_results[i, "lm_coefficient"] <- test_coef
  
  # save R2
  test_r2 <- summary(test_model)$r.squared
  gt_results[i, "cv10_r2"] <- test_r2
  
  # save Adj R2
  test_adj_r2 <- summary(test_model)$adj.r.squared
  gt_results[i, "cv10_adj_r2"] <- test_adj_r2
  
  # save whether Adj R2 improves on base
  if(test_adj_r2 > base_adj_r2) {
    gt_results[i, "improves_adj_r2"] <- TRUE
  }
  else {
    gt_results[i, "improves_adj_r2"] <- FALSE
  }
}

# results
gt_results # Each GT column taken alone slightly improves Adj R2 except 3 month MA

### Try using all GT columns as variables

## Remove correlations with PCA
gtrends_pca <- subset(gtrends, select = -CloseMonth)
gtrends_pca <- as.data.frame(prcomp(gtrends_pca, scale = T)$x)
gtrends_pca <- cbind(gtrends$CloseMonth, gtrends_pca)
colnames(gtrends_pca) <- c("CloseMonth",
                           "gt_pc1",
                           "gt_pc2",
                           "gt_pc3",
                           "gt_pc4",
                           "gt_pc5",
                           "gt_pc6",
                           "gt_pc7",
                           "gt_pc8",
                           "gt_pc9",
                           "gt_pc10",
                           "gt_pc11",
                           "gt_pc12",
                           "gt_pc13")

## Merge with housing data
data9 <- merge(data7, gtrends_pca, by = "CloseMonth")

# fit lm model with 10-fold CV
model_gt_pca <- train(ClosePrice ~ .-CloseDate-CloseMonth,
                    data = data9, 
                    method = "lm",
                    trControl = trainControl(method = "cv", 
                                             number = 10))

# results
summary(model_gt_pca) # gt_pc9 and gt_pc13 are not significant factors

# refit with non-significant factors removed
model_gt_pca2 <- train(ClosePrice ~ .-CloseDate-CloseMonth-gt_pc9-gt_pc13,
                      data = data9, 
                      method = "lm",
                      trControl = trainControl(method = "cv", number = 10))

# results
summary(model_gt_pca2) # R2 = Adj R2 = 0.725 on all significant factors

# save dfs
write.csv(data8, "/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/data8.csv")
write.csv(data9, "/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/data9.csv")
