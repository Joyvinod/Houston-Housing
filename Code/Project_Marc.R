Data1 <- read.csv('data_cleaned_stage2.csv')
head(Data1)

#load pacman

#load packages
pacman::p_load(caret, dplyr, numbers, math, PerformanceAnalytics, lubridate, xts, tidyverse, ggthemes, reshape2, GGally, corrplot, tree, rpart.plot, ggpubr)


### subset df to columns with less than 5% missing and remove rows with NAs

# set maximum number of NAs in column as 5% of observations
max_na <- nrow(Data1)*0.05

# select columns with less NAs than max_na <- data2[, colSums(is.na(data2)) < max_na]
Data2 <- Data1[, colSums(is.na(Data1)) < max_na]
# identify columns with NAs
colnames(Data2[, colSums(is.na(Data2)) > 0])

# identify rows with more than one NA and remove
missing_many <- as.integer((rownames(Data2[rowSums(is.na(Data2)) > 0, ])))
Data2 <- Data2[-missing_many, ]      # removes 363 observations
head(Data2)

# split baths into full and half columns
Data2 <- Data2 %>%
  mutate(HalfBathsTotal = (BathsTotal %% 1)*10) %>%
  mutate(BathsTotal = floor(BathsTotal)) 
head(Data2)



# factors to factors by column
Data2 <- Data2 %>%
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
Data2 <- Data2[Data2$ForLease == 0, ]


# remove ForLease and PropertyType since uniform values
Data2 <- Data2 %>% select(-c(ForLease, PropertyType))
head(Data2)

View(Data2)

### correlations
Data2_cont <- Data2 %>% 
  select(c(BathsTotal, BedsTotal, SqFtTotal, ClosePrice))

corrplot(cor(Data2_cont), method="circle", sig.level = 0.01)
cor(Data2_cont)

pairs(~BathsTotal+SqFtTotal+ClosePrice+BedsTotal,data=Data2,
      main="Scatterplot Matrix")

#Clustering for County and Close Price
library(cluster)  # For K-means clustering
library(ggplot2)  # For visualization
library(factoextra)
library(NbClust)
library(tidyverse)
library(dplyr)
library(car)
library(lubridate)
library(gridExtra)
head(Data2)
summary(Data2)
#Remove rows with year over 2024
Data2$YearBuilt[Data2$YearBuilt > 2024] <- NA
View(Data2)

#Create a scatterplot to compare house year built and close price
scatterplot(ClosePrice ~ YearBuilt, data=Data2,  xlab="Year Built", ylab="Close Price", grid=FALSE)
scatterplot(ClosePrice ~ SqFtTotal, data=Data2,  xlab="Total Square Foot", ylab="Close Price", grid=FALSE)
#Create a new column called 'Year' that extract year from the date column

# Create a vector of dates
dates <- as.Date(Data2$CloseDate)
# Extract the year from the dates
Data2$CloseYear <- year(dates)
head(Data2)
#Create a scatterplot to compare close year with close price
scatterplot(ClosePrice ~ CloseYear, data=Data2,  xlab="Close Year", ylab="Close Price", grid=FALSE)

ggplot(Data2, aes(x = CloseYear, fill = ..count..)) +
  geom_histogram(binwidth = 0.05) +
  ggtitle("Figure 2 Histogram of SalePrice") +
  ylab("Year Sold") +
  xlab("Housing Price") + 
  theme(plot.title = element_text(hjust = 0.5))

model1 <- lm(ClosePrice ~SqFtTotal, data = Data2)
summary(model1)

model2 <- lm(ClosePrice ~BathsTotal, data = Data2)
summary(model2)

model3 <- lm(ClosePrice ~YearBuilt, data = Data2)
summary(model3)

model4 <- lm(ClosePrice ~GeoMarketArea, data = Data2)
summary(model4)

model5 <- lm(ClosePrice ~ SqFtTotal + BedsTotal, data = Data2)
summary(model5)

options(repr.plot.width=7, repr.plot.height=4)
plot1 <- ggplot(Data2, aes(x=SqFtTotal, y=ClosePrice)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="blue", se=FALSE)+
  ggtitle("Close Price vs Total Square Feet") +
  theme(plot.title = element_text(hjust = 0.4))
print(plot1)

plot2 <- ggplot(Data2, aes(x=YearBuilt, y=ClosePrice)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="blue", se=FALSE)+
  ggtitle("Close Price vs Year Built") +
  theme(plot.title = element_text(hjust = 0.4))
print(plot2)

plot3 <- ggplot(Data2, aes(x=BathsTotal, y=ClosePrice)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="blue", se=FALSE)+
  ggtitle("Close Price vs Total Bathrooms") +
  theme(plot.title = element_text(hjust = 0.4))
print(plot3)

plot4 <- ggplot(Data2, aes(x=CloseYear, y=ClosePrice)) + 
  geom_point(shape=1) +  
  geom_smooth(method=lm , color="blue", se=FALSE)+
  ggtitle("Close Price vs Year Sold") +
  theme(plot.title = element_text(hjust = 0.4))
print(plot4)

grid.arrange(plot1, plot2, plot3, plot4)

Data2 <- Data2[,-which(names(Data2) == "CloseDate")]
