### Add in M2 money supply

## set seed
set.seed(42)

## install packages
library(caret)

### Data prep
## load data
# housing/google data
data9 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/data9.csv")

# drop X column and non-significant GoogleTrends columns (from "adding_gtdata" analysis)
data9 <- subset(data9, select = -c(X, gt_pc9, gt_pc13))

# M2 money supply data
data_m2 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/m2_supply_fred.csv")

## Combine dfs
# Create CloseMonth for M2 df
data_m2$CloseMonth <- format(as.Date(data_m2$DATE), "%Y-%m")
data_m2 <- subset(data_m2, select = -DATE)

# combine dfs
data10 <- merge(data9, data_m2, by = "CloseMonth", all.x = T) # left join

### Fit lm model with 10-fold CV
model_gt_m2 <- train(ClosePrice ~ .-CloseDate-CloseMonth,
                       data = data10, 
                       method = "lm",
                       trControl = trainControl(method = "cv", 
                                                number = 10))

# results
summary(model_gt_m2) # slight improvement over model_gt_pca2 with Adj R2 = 0.7258 cf 0.725

# remove non-significant factors gt_pc11 and gt_pc12 and rerun
model_gt_m2 <- train(ClosePrice ~ .-CloseDate-CloseMonth-gt_pc11-gt_pc12,
                     data = data10, 
                     method = "lm",
                     trControl = trainControl(method = "cv", 
                                              number = 10))

# results
summary(model_gt_m2) # maintains slight improvement with fewer factors

# save data
write.csv(data10, "/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/data10.csv")
