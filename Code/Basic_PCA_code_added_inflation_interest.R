

##### I have taken Evans code and then added federal interest rate and inflation

### clear all
rm(list = ls())

# clear plots
dev.off()  # only if there is a plot

# clear console # Edit clear console

cat("\014")  # or use ctrl+L



### set wd to the place where the data folder is residing
getwd()
setwd("C:/Users/kakka/OneDrive/GeorgiaTech/MGT 6203/Team_60")



### 2024/04/07 regression modeling with recleaned data ###

### load recleaned data
data5 <- read.csv("data5.csv")
data5 <- subset(data5, select = -X)
head(data5)

# let us look at inflation data and clean it up 
inflation_data <- read.csv('usainflation.csv')
inflation_data <- inflation_data %>%
rename(monthly_inflation_rate = Monthly.Inflation.Rate..Seasonally.adjusted.)



# just want to keep monthly inflation rate and delete any other columns
inflation_data <- inflation_data[, -which(names(inflation_data) == "Annual.Inflation.Rate..not.Seasonally.adjusted.")]
head(inflation_data)


# let us look at federal interest rate data and clean it up
interest_rate <- read.csv('Federalinterestrate.csv')
# Rename column "old_column_name" to "new_column_name"
interest_rate <- interest_rate %>%
                  rename(monthly_interest_rate = FEDFUNDS)
head(interest_rate)


# Convert date columns to consistent format
data5$CloseDate <- as.Date(data5$CloseDate, format = "%m/%d/%Y")
interest_rate$DATE <- as.Date(interest_rate$DATE, format = "%m/%d/%Y")
inflation_data$Month <- as.Date(paste0("01-", inflation_data$Month), format = "%d-%b-%y")

# Extract month and year from date column
data5$month_year <- format(data5$CloseDate, "%b-%y")
interest_rate$month_year <- format(interest_rate$DATE, "%b-%y")
inflation_data$month_year <- format(inflation_data$Month, "%b-%y")

# getting the mean interest rate monthly instead of daily
interest_rate <- interest_rate %>%
  group_by(month_year) %>%
  summarise(mean_monthly_interest_rate = mean(monthly_interest_rate))

interest_rate


# Merge datasets based on month and year
data5 <- merge(data5, inflation_data, by = "month_year")
data5 <- merge(data5, interest_rate, by = "month_year")
# Get the current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))
# Create the HouseAge column
data5$HouseAge <- current_year - data5$YearBuilt


head(data5$HouseAge)



## I see a few extra columns like month, month_year and YearBuilt so deleting them

data5 <- data5[, !names(data5) %in% c("month_year", "Month", "YearBuilt")]
unique(names(data5))

### Fit lm models

## Basic lm model, no PCA

# fit basic model, no cv, without CloseDate
model_basic <- lm(data = data5, formula = ClosePrice~.-CloseDate)

# basic model summary
summary(model_basic) # old model R squared of 0.7885, adjusted R squared of 0.7879
                      # current model R2 = 0.7964,   0.7958

# look at significant coefficients
model_basic_coefs <- as.data.frame(summary(model_basic)$coefficients)
model_basic_coefs

# restrict to p < 0.15
model_basic_coefs_015 <- subset(model_basic_coefs, `Pr(>|t|)` < 0.15)
model_basic_coefs_015 <- model_basic_coefs_015[order(model_basic_coefs_015$`Pr(>|t|)`), ]
model_basic_coefs_015


## PCA on continuous variables

# select continuous variables
data5_cts <- subset(data5, select = c(BathsTotal, BedsTotal, SqFtTotal, HouseAge, mean_monthly_interest_rate, monthly_inflation_rate))

# calculate principal components
data5_cts_pcs <- as.data.frame(prcomp(data5_cts, scale = T)$x)

# replace continuous variables in original df
data6 <- subset(data5, select = -c(BathsTotal, BedsTotal, SqFtTotal, HouseAge, mean_monthly_interest_rate, monthly_inflation_rate))
data6 <- cbind(data5_cts_pcs, data6)

# fit basic PCA model, no cv, without CloseDate
model_pca <- lm(data = data6, formula = ClosePrice~.-CloseDate)

# PCA model summary
summary(model_pca) # old model R squared of 0.7885, adjusted R squared of 0.7879 (same as no PCA)
# new_model

# look at significant coefficients
model_pca_coefs <- as.data.frame(summary(model_pca)$coefficients)

# restrict to p < 0.15
model_pca_coefs_015 <- subset(model_pca_coefs, `Pr(>|t|)` < 0.15)
model_pca_coefs_015 <- model_pca_coefs_015[order(model_pca_coefs_015$`Pr(>|t|)`), ]
model_pca_coefs_015
