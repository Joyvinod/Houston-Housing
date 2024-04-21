

##### I have taken Evans code and then added federal interest rate and inflation rate and M2

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
#head(inflation_data)


# let us look at federal interest rate data and clean it up
interest_rate <- read.csv('Federalinterestrate_1.csv')
# Rename column "old_column_name" to "new_column_name"
interest_rate <- interest_rate %>%
                  rename(monthly_interest_rate = FEDFUNDS)
head(interest_rate)

# let us do the same with M2 money supply data
M2 <- read.csv('m2_supply_fred.csv')
#head(M2)
#nrow(M2)



# Convert date columns to consistent format
data5$CloseDate <- as.Date(data5$CloseDate, format = "%m/%d/%Y")
interest_rate$DATE <- as.Date(interest_rate$DATE, format = "%m/%d/%Y")
inflation_data$Month <- as.Date(paste0("01-", inflation_data$Month), format = "%d-%b-%y")
M2$DATE <- as.Date(M2$DATE, format = "%m/%d/%Y")

# Extract month and year from date column
data5$month_year <- format(data5$CloseDate, "%b-%y")
interest_rate$month_year <- format(interest_rate$DATE, "%b-%y")
inflation_data$month_year <- format(inflation_data$Month, "%b-%y")
M2$month_year <- format(M2$DATE, "%b-%y")

# getting the mean interest rate monthly instead of daily
interest_rate <- interest_rate %>%
  group_by(month_year) %>%
  summarise(mean_monthly_interest_rate = mean(monthly_interest_rate))

# removing the date column in M2
M2 <- subset(M2, select = -DATE)

# Merge datasets based on month and year
data5 <- merge(data5, inflation_data, by = "month_year")
data5 <- merge(data5, interest_rate, by = "month_year")
data5 <- merge(data5, M2, by = "month_year")

#head(data5)
#nrow(data5)

# Get all column names and convert them to lowercase
all_column_names <- tolower(colnames(data5))

# Filter column names starting with 'm'
m_column_names <- all_column_names[startsWith(all_column_names, "m")]

# Get unique column names
unique_m_column_names <- unique(m_column_names)
unique_m_column_names
#m_column_names



# converting year_built to houseage column
# Get the current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))
# Create the HouseAge column
data5$HouseAge <- current_year - data5$YearBuilt


head(data5$HouseAge)



## I see a few extra columns like month, month_year and YearBuilt so deleting them

data5 <- data5[, !names(data5) %in% c("month_year", "Month", "YearBuilt")]
#unique(names(data5))

### Fit lm models

## Basic lm model, no PCA

# fit basic model, no cv, without CloseDate
model_basic <- lm(data = data5, formula = ClosePrice~.-CloseDate)

# basic model summary
summary(model_basic) # old model R squared of 0.7885, adjusted R squared of 0.7879
                      # model with  interest rate and inflation added R2 = 0.7964,   0.7958
                      # current model with  M2, interest rate and inflation added - R2 = 0.7983 adjusted R2 - 0.7977

# look at significant coefficients
model_basic_coefs <- as.data.frame(summary(model_basic)$coefficients)
model_basic_coefs

# restrict to p < 0.15
model_basic_coefs_015 <- subset(model_basic_coefs, `Pr(>|t|)` < 0.15)
model_basic_coefs_015 <- model_basic_coefs_015[order(model_basic_coefs_015$`Pr(>|t|)`), ]
model_basic_coefs_015


## PCA on continuous variables

# select continuous variables
data5_cts <- subset(data5, select = c(BathsTotal, BedsTotal, SqFtTotal, HouseAge, mean_monthly_interest_rate, monthly_inflation_rate, M2SL))

# calculate principal components
data5_cts_pcs <- as.data.frame(prcomp(data5_cts, scale = T)$x)

# replace continuous variables in original df
data6 <- subset(data5, select = -c(BathsTotal, BedsTotal, SqFtTotal, HouseAge, mean_monthly_interest_rate, monthly_inflation_rate, M2SL))
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







# Assuming 'data5' contains your dataset

# Sampling 50% of the data
set.seed(123) # for reproducibility
sampled_data <- data5[sample(nrow(data5), nrow(data5) * 0.50), ]
nrow(sampled_data)

# lets remove the outliers in the sampled data 

# Assuming 'sampled_data' contains your dataset

# Calculate mean and standard deviation of ClosePrice
mean_close_price <- mean(sampled_data$ClosePrice)
sd_close_price <- sd(sampled_data$ClosePrice)

# Define the range for acceptable values
lower_bound <- mean_close_price - 2 * sd_close_price  # Adjust as needed
upper_bound <- mean_close_price + 2 * sd_close_price  # Adjust as needed

# Filter out outliers
filtered_data <- sampled_data[sampled_data$ClosePrice >= lower_bound & sampled_data$ClosePrice <= upper_bound, ]

# Check the dimensions to see how many rows were removed
rows_removed <- nrow(sampled_data) - nrow(filtered_data)




# Load ggplot2 for visualization
library(ggplot2)




# Scatter plot with custom colors and decimal formatting
plot_interest_rate <- ggplot(sampled_data, aes(x = mean_monthly_interest_rate, y = ClosePrice)) +
  geom_point(color = "#FF5733", alpha = 0.6) + # custom color for points
  labs(x = "Monthly Interest Rate", y = "Close Price") + # adding labels to axes
  #ggtitle("Close Price vs. Monthly Interest Rate") + # adding title
  theme_minimal() + # setting minimal theme for cleaner appearance
  theme( # customizing the theme
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # title size and position
    axis.title = element_text(size = 14), # axis labels size
    axis.text = element_text(size = 12), # axis tick labels size
    legend.title = element_blank(), # remove legend title
    legend.text = element_text(size = 12) # legend text size
  ) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) # Convert scientific notation to regular numbers



# Scatter plot with custom colors
plot_houseage <-ggplot(sampled_data, aes(x = HouseAge, y = ClosePrice)) +
  geom_point(color = "#FF5733", alpha = 0.6) + # custom color for points
  labs(x = "House Age", y = "Close Price") + # adding labels to axes
  #ggtitle("Close Price vs. HouseAge") + # adding title
  theme_minimal() + # setting minimal theme for cleaner appearance
  theme( # customizing the theme
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # title size and position
    axis.title = element_text(size = 14), # axis labels size
    axis.text = element_text(size = 12), # axis tick labels size
    legend.title = element_blank(), # remove legend title
    legend.text = element_text(size = 12) # legend text size
  ) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) # Convert scientific notation to regular numbers



# Scatter plot with custom colors
plot_inflation_rate <-ggplot(sampled_data, aes(x = monthly_inflation_rate, y = ClosePrice)) +
  geom_point(color = "#FF5733", alpha = 0.6) + # custom color for points
  labs(x = "Monthly Inflation Rate", y = "Close Price") + # adding labels to axes
  #ggtitle("Close Price vs. Monthly Inflation Rate") + # adding title
  theme_minimal() + # setting minimal theme for cleaner appearance
  theme( # customizing the theme
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # title size and position
    axis.title = element_text(size = 14), # axis labels size
    axis.text = element_text(size = 12), # axis tick labels size
    legend.title = element_blank(), # remove legend title
    legend.text = element_text(size = 12) # legend text size
  )  +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) # Convert scientific notation to regular numbers

#sampled_data$monthly_inflation_rate


plot_M2 <- ggplot(sampled_data, aes(x = M2SL, y = ClosePrice)) +
  geom_point(color = "#FF5733", alpha = 0.6) + # custom color for points
  labs(x = "Money Supply M2", y = "Close Price") + # adding labels to axes
  #ggtitle("Close Price vs Money Supply") + # adding title
  theme_minimal() + # setting minimal theme for cleaner appearance
  theme( # customizing the theme
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # title size and position
    axis.title = element_text(size = 14), # axis labels size
    axis.text = element_text(size = 12), # axis tick labels size
    legend.title = element_blank(), # remove legend title
    legend.text = element_text(size = 12) # legend text size
  )  +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) # Convert scientific notation to regular numbers



plot_houseage
plot_interest_rate
plot_inflation_rate
plot_M2


# Arrange plots in four quarters
library(gridExtra)
grid.arrange(plot_houseage, plot_interest_rate, plot_M2, plot_inflation_rate, ncol = 2, nrow = 2)

######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################




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
#head(inflation_data)


# let us look at federal interest rate data and clean it up
interest_rate <- read.csv('Federalinterestrate_1.csv')
# Rename column "old_column_name" to "new_column_name"
interest_rate <- interest_rate %>%
  rename(monthly_interest_rate = FEDFUNDS)
head(interest_rate)

# let us do the same with M2 money supply data
M2 <- read.csv('m2_supply_fred.csv')
#head(M2)
#nrow(M2)



# Convert date columns to consistent format
data5$CloseDate <- as.Date(data5$CloseDate, format = "%m/%d/%Y")
interest_rate$DATE <- as.Date(interest_rate$DATE, format = "%m/%d/%Y")
inflation_data$Month <- as.Date(paste0("01-", inflation_data$Month), format = "%d-%b-%y")
M2$DATE <- as.Date(M2$DATE, format = "%m/%d/%Y")

# Extract month and year from date column
data5$month_year <- format(data5$CloseDate, "%b-%y")
interest_rate$month_year <- format(interest_rate$DATE, "%b-%y")
inflation_data$month_year <- format(inflation_data$Month, "%b-%y")
M2$month_year <- format(M2$DATE, "%b-%y")

# getting the mean interest rate monthly instead of daily
interest_rate <- interest_rate %>%
  group_by(month_year) %>%
  summarise(mean_monthly_interest_rate = mean(monthly_interest_rate))

# removing the date column in M2
M2 <- subset(M2, select = -DATE)

# Merge datasets based on month and year
data5 <- merge(data5, inflation_data, by = "month_year")
data5 <- merge(data5, interest_rate, by = "month_year")
data5 <- merge(data5, M2, by = "month_year")

#head(data5)
#nrow(data5)

# Get all column names and convert them to lowercase
all_column_names <- tolower(colnames(data5))

# Filter column names starting with 'm'
m_column_names <- all_column_names[startsWith(all_column_names, "m")]

# Get unique column names
unique_m_column_names <- unique(m_column_names)
unique_m_column_names
#m_column_names



# converting year_built to houseage column
# Get the current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))
# Create the HouseAge column
data5$HouseAge <- current_year - data5$YearBuilt


head(data5$HouseAge)



## I see a few extra columns like month, month_year and YearBuilt so deleting them

data5 <- data5[, !names(data5) %in% c("month_year", "Month", "YearBuilt")]
#unique(names(data5))

### Fit lm models

## Basic lm model, no PCA

# fit basic model, no cv, without CloseDate
model_basic <- lm(data = data5, formula = ClosePrice~.-CloseDate)

# basic model summary
summary(model_basic) # old model R squared of 0.7885, adjusted R squared of 0.7879
# model with  interest rate and inflation added R2 = 0.7964,   0.7958
# current model with  M2, interest rate and inflation added - R2 = 0.7983 adjusted R2 - 0.7977

# look at significant coefficients
model_basic_coefs <- as.data.frame(summary(model_basic)$coefficients)
model_basic_coefs

# restrict to p < 0.15
model_basic_coefs_015 <- subset(model_basic_coefs, `Pr(>|t|)` < 0.15)
model_basic_coefs_015 <- model_basic_coefs_015[order(model_basic_coefs_015$`Pr(>|t|)`), ]
model_basic_coefs_015


## PCA on continuous variables

# select continuous variables 
# monthly_inflation_rate, M2SL
data5_cts <- subset(data5, select = c(BathsTotal, BedsTotal, SqFtTotal)) 
#data5_cts <- subset(data5, select = c(BathsTotal, BedsTotal, SqFtTotal,  mean_monthly_interest_rate)) # mean_monthly_interest_rate
#data5_cts <- subset(data5, select = c(BathsTotal, BedsTotal, SqFtTotal,  monthly_inflation_rate)) # monthly_inflation_rate
#data5_cts <- subset(data5, select = c(BathsTotal, BedsTotal, SqFtTotal,  M2SL)) # M2SL

# calculate principal components
data5_cts_pcs <- as.data.frame(prcomp(data5_cts, scale = T)$x)

# replace continuous variables in original df
data6 <- subset(data5, select = -c(BathsTotal, BedsTotal, SqFtTotal)) 
#data6 <- subset(data5, select = -c(BathsTotal, BedsTotal, SqFtTotal,  mean_monthly_interest_rate)) # mean_monthly_interest_rate
#data6 <- subset(data5, select = -c(BathsTotal, BedsTotal, SqFtTotal,  monthly_inflation_rate)) # monthly_inflation_rate
#data6 <- subset(data5, select = -c(BathsTotal, BedsTotal, SqFtTotal,  M2SL)) # M2SL
#data6 <- cbind(data5_cts_pcs, data6)

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




