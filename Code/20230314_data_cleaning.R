
### load Joy's data (second step of cleaning)
data2 <- read.csv("Code/Data_Cleaning_housing_data_ver2.R")

# install libraries
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)

### subset df to columns with less than 5% missing and remove rows with NAs

# set maximum number of NAs in column as 5% of observations
max_na <- nrow(data2)*0.05

# select columns with less NAs than max_na
data4 <- data2[, colSums(is.na(data2)) < max_na]

# identify columns with NAs
colnames(data4[, colSums(is.na(data4)) > 0])

# identify rows with more than one NA and remove
missing_many <- as.integer((rownames(data4[rowSums(is.na(data4)) > 0, ])))
data4 <- data4[-missing_many, ]      # removes 363 observations


### clean by column

# split baths into full and half columns
data4 <- data4 %>%
  mutate(HalfBathsTotal = (BathsTotal %% 1)*10) %>%
  mutate(BathsTotal = floor(BathsTotal)) 

# factors to factors by column
data4 <- data4 %>%
  # BathsTotal
  # BedsTotal
  # SqFtTotal
  # CloseDate
  # ClosePrice
  mutate(Siding = as.factor(Siding)) %>%
  mutate(CoolSystem = as.factor(CoolSystem)) %>%
  mutate(CountyOrParish = as.factor(CountyOrParish)) %>%
  mutate(ForLease = as.factor(ifelse(ForLease == "True", 1, 0))) %>%
  mutate(HeatSystem = as.factor(HeatSystem)) %>%
  mutate(HOAMandatory = as.factor(ifelse(HOAMandatory == "True", 1, 0))) %>%
  mutate(GeoMarketArea = as.factor(GeoMarketArea)) %>%
  mutate(NewConstruction = as.factor(ifelse(NewConstruction == "True", 1, 0))) %>%
  mutate(PoolPrivate = as.factor(ifelse(PoolPrivate == "True", 1, 0))) %>%
  mutate(PropertyType = as.factor(PropertyType)) %>%
  mutate(DwellingType = as.factor(DwellingType)) %>%
  mutate(Restrictions = as.factor(Restrictions)) %>%
  mutate(Roof = as.factor(Roof)) %>%
  mutate(SchoolDistrict = as.factor(SchoolDistrict)) %>%
  mutate(WaterSewer = as.factor(WaterSewer)) %>%
  # YearBuilt
  mutate(PostalCode = as.factor(PostalCode))
  # HalfBathsTotal

# remove lease properties
data4 <- data4[data4$ForLease == 0, ]

# remove ForLease and PropertyType since uniform values
data4 <- data4 %>% select(-c(ForLease, PropertyType))

# order by close date
data4 <- data4[order(data4$CloseDate),]
