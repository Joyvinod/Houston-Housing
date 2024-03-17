### load packages
library(tidyverse)
library(TTR)
library(corrplot)
library(zoo)

### load Google Trends data

# 2004-2024_US_har houses for sale
gt1 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/Google Trends/2004-2024_US_har houses for sale.csv")

# 2004-2024_US_har houston
gt2 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/Google Trends/2004-2024_US_har houston.csv")

# 2004-2024_US_har real estate
gt3 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/Google Trends/2004-2024_US_har real estate.csv")

# 2004-2024_US_houses for sale houston
gt4 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/Google Trends/2004-2024_US_houses for sale houston.csv")

# 2004-2024_US_houston home prices
gt5 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/Google Trends/2004-2024_US_houston home prices.csv")

# 2004-2024_US_houston real estate
gt6 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/Google Trends/2004-2024_US_houston real estate.csv")

# 2004-2024_US_houston realtor
gt7 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/Google Trends/2004-2024_US_houston realtor.csv")

# load cleaned housing data
data4 <- read.csv("/Users/eswain7/Documents/Documents - ES’s MacBook Air/202401_MGT6203/Project/Upload/20240314_data4.csv")





### Combine Google Trends data into one df

# extract month from row names to column
month <- row.names(gt1)
month <- month[-1]

# create df
gtrends <- data.frame("month" = month)

# add individual time series to df
gtrends <- gtrends %>%
  mutate(gtrends$gt1 <- as.integer(gt1[-1, ])) %>%
  mutate(gtrends$gt2 <- as.integer(gt2[-1, ])) %>%
  mutate(gtrends$gt3 <- as.integer(gt3[-1, ])) %>%
  mutate(gtrends$gt4 <- as.integer(gt4[-1, ])) %>%
  mutate(gtrends$gt5 <- as.integer(gt5[-1, ])) %>%
  mutate(gtrends$gt6 <- as.integer(gt6[-1, ])) %>%
  mutate(gtrends$gt7 <- as.integer(gt7[-1, ]))

# clear up column names
colnames(gtrends) <- c("CloseMonth", "gt1", "gt2", "gt3", "gt4", "gt5", "gt6", "gt7")



# check correlations between gt data
gtrends_ind_cor <- cor(gtrends %>% select(-c(CloseMonth)))
corrplot(gtrends_ind_cor, method="number") # a lot of correlation



### sum the Google Trends data into one vector
gtrends$gt_sum <- gtrends$gt1 + 
  gtrends$gt2 + 
  gtrends$gt3 + 
  gtrends$gt4 + 
  gtrends$gt5 + 
  gtrends$gt6 + 
  gtrends$gt7
gtrends <- gtrends[, c("CloseMonth", "gt_sum")]



### calculate macd, signal, and histogram with expo moving average
macd <- as.data.frame(MACD(gtrends$gt_sum, nFast = 12, nSlow = 26, nSig = 9, maType="EMA", percent = T))
macd$hist <- macd$macd - macd$signal
macd$CloseMonth <- month
gtrends <- merge(gtrends, macd, by = "CloseMonth")



### calculate rsi w/weighted moving average
gtrends$rsi <- RSI(gtrends$gt_sum, n=14, maType = "WMA")



### calculate SMAs
gtrends$ma03 <- rollmean(gtrends$gt_sum, k = 3, fill = NA, align = "right")
gtrends$ma06 <- rollmean(gtrends$gt_sum, k = 6, fill = NA, align = "right")
gtrends$ma12 <- rollmean(gtrends$gt_sum, k = 12, fill = NA, align = "right")



### calculate lagged sums
gtrends$lag01 <- lag(gtrends$gt_sum, n = 1L) 
gtrends$lag02 <- lag(gtrends$gt_sum, n = 2L) 
gtrends$lag03 <- lag(gtrends$gt_sum, n = 3L) 
gtrends$lag06 <- lag(gtrends$gt_sum, n = 6L) 
gtrends$lag12 <- lag(gtrends$gt_sum, n = 12L) 



# remove NAs from beginning of df
gtrends <- na.omit(gtrends)
rownames(gtrends) <- NULL # reset rownames



# create column with the month in which the house closed 
# so the housing data can be combined with the monthly Google Trends data
data4$CloseMonth <- format(as.Date(data4$CloseDate), "%Y-%m")





### see if the Google Trends data is predictive of the mean closing price for
### each month of housing data
# makes assumption: similar houses were sold each month

# calculate mean closing price per month
mean_price <- data.frame("CloseMonth" = unique(data4$CloseMonth), "mean_price")

for(i in 1:63) {
  m <- mean_price[i, 1]
  rows <- data4[data4$CloseMonth == m, ]
  mean_price[i, 2] <- sum(rows$ClosePrice) / length(rows)
}



# merge mean closing price data with Google Trends data
gtrends_meanprice <- merge(gtrends, mean_price, by = "CloseMonth")
gtrends_meanprice$mean_price <- as.numeric(gtrends_meanprice$X.mean_price.)
gtrends_meanprice <- gtrends_meanprice %>% select(-X.mean_price.)



# simple leaner model mean_price ~ GT data
model_meanprice <- lm(data = gtrends_meanprice, formula = mean_price~.-CloseMonth)
summary(model_meanprice) # GT data explains ~70% of monthly average price variation



# check correlation between Google Trends predictors
gtrends_cor <- cor(gtrends_meanprice %>% select(-c(CloseMonth, mean_price)))
corrplot(gtrends_cor, method="number") # a lot of correlation
max(gtrends_cor < 1)



# since there is correlation, looking at each factor individually
factors <- colnames(gtrends_meanprice)
r2 <- data.frame("factor" = factors, "r_squared")
l <- length(factors)
dep_var_index <- grep("mean_price", colnames(gtrends_meanprice))

for (i in 1:l) {
  if (factors[i] == "CloseMonth" | factors[i] == "mean_price") {
    r2[i, 2] <- NA
  }
  else {
    df <- gtrends_meanprice[, c(i, dep_var_index)]
    model <- lm(data = df, formula = mean_price~.)
    r2[i, 2] <- summary(model)$r.squared 
  }
}

r2 # over 50% of the var in price can still be explained by single factors

# best R2 is lag02 (0.5721)



model_ma06 <- lm(data = gtrends_meanprice, formula = mean_price~ma06)
summary(model_ma06)

model_lag02 <- lm(data = gtrends_meanprice, formula = mean_price~lag02)
summary(model_lag02)



# select only GT factors with highest R2 = still too much correlation
gtrends_select <- gtrends_meanprice[c("CloseMonth", "mean_price", "macd", "signal", "ma06", "lag02")]
corrplot(cor(gtrends_select[,-c(1, 2)]), method="number")



# all factors correlated, so choosing highest R2 in lag02

gtrends_lag02 <- gtrends_meanprice[c("CloseMonth", "lag02")]

df_housing_lag02 <- merge(data4, gtrends_lag02, by="CloseMonth")






### lm models

### basic lm model for df_housing_lag02 

# baseline model with just housing data
model_lag02_baseline <- lm(data = df_housing_lag02, formula = ClosePrice ~.-CloseDate -CloseMonth -lag02)

summary(model_lag02_baseline) # R2 = .7986 Adj R2 = .797

# model with lag02 added as factor
model_lag02 <- lm(data = df_housing_lag02, formula = ClosePrice~.-CloseDate -CloseMonth)

summary(model_lag02) # R2 = .7986 Adj R2 = .797

coefs_lag02 <- data.frame(summary(model_lag02)$coef)

# summary of lag02 in the model
(summary(model_lag02)$coef)["lag02", ]

### no difference from adding GT data ###



# # adding in all of the Google Trends data (ignoring correlation of factors)
# df_housing_gtrends <- merge(data4, gtrends, by="CloseMonth")

# ### compare lm on just housing data and with GT as factors

# baseline model with just housing data
model_gtrends_baseline <- lm(data = df_housing_gtrends, formula = ClosePrice ~ .
                   -CloseMonth
                   -CloseDate
                   -gt_sum
                   -macd
                   -signal
                   -hist
                   -rsi
                   -ma03
                   -ma06
                   -ma12
                   -lag01
                   -lag02
                   -lag03
                   -lag06
                   -lag12)

summary(model_gtrends_baseline) # R2 = .7986, Adj R2 = .797

model_gtrends <- lm(data = df_housing_gtrends, formula = ClosePrice ~.
                 -CloseDate
                 -CloseMonth)

summary(model_gtrends) #R2 = .8051, Adj R2 = .8035

coefs_gtrends <- data.frame(summary(model_gtrends)$coef)



### re-modeling with best performing factors
model_gtbest <- lm(data = df_housing_gtrends, formula = ClosePrice ~ .
                   -CloseMonth
                   -CloseDate
                   #-gt_sum
                   #-macd
                   -signal
                   -hist
                   -ma03
                   #-ma06
                   -ma12
                   -lag01
                   -lag02
                   #-lag03
                   -lag06
                   -lag12)

summary(model_gtbest) # R2 = .8042, Adj R2 = .8026
coefs_gtbest <- data.frame(summary(model_gtbest)$coef)



### alternative approach of using trailing 24 months of summed GT data
### as stand alone factors

### create df with trailing 24 months summed GT data

df_trail24 <- as.data.frame(matrix(data = NA, 
                                   nrow = length(unique(data4$CloseMonth)), 
                                   ncol = 24))
colnames(df_trail24) <- seq(from = 1, to = 24, by = 1)
df_trail24$CloseMonth <- unique(data4$CloseMonth)

indicator = "gt_sum"

for (i in 1:63) {
  current_month <- df_trail24[i, "CloseMonth"]
  gt_row <- as.integer(row.names(gtrends[gtrends$CloseMonth == current_month, ]))
  begin <- gt_row - 24
  end <- gt_row - 1
  time_series <- gtrends[c(end:begin), indicator]
  df_trail24[i, 1:24] <- time_series
}

# combine with housing data
df_housing_trail24 <- merge(data4, df_trail24, by="CloseMonth")



# train lm model on trailing gt data
model_trail24 <- lm(data = df_housing_trail24, formula = ClosePrice ~ . -CloseDate -CloseMonth)

summary(model_trail24) # R2 = .8043 Adj R2 = 8027

coefs_trail24 <- data.frame(summary(model_trail24)$coef)

