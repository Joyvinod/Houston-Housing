## Data recleaned 2024/04/07 with focus on dummy variables ##

### install libraries
install.packages("tidyverse", repos = "http://cran.us.r-project.org")
library(tidyverse)

install.packages("stringr", repos = "http://cran.us.r-project.org")
library(stringr)

### load Joy's data (second step of cleaning)

data2 <- read.csv(".../data_cleaned_stage2.csv")

# copy df
data4 <- data2

### remove lease properties

# remove lease properties
# unique(data2$ForLease) # "False" or "True"
data4 <- data4[data4$ForLease == "False", ]

# remove ForLease and PropertyType since uniform values
data4 <- data4 %>% select(-c(ForLease, PropertyType))

### subset df to columns with less than 5% missing and remove rows with NAs

# set maximum number of NAs in column as 5% of observations
max_na <- nrow(data4)*0.05

# select columns with less NAs than max_na
data4 <- data4[, colSums(is.na(data4)) < max_na]

# identify columns with NAs
colnames(data4[, colSums(is.na(data4)) > 0])

# remove rows with NAs in factor columns
data4 <- data4[is.na(data4$CoolSystem) == F, ]
data4 <- data4[is.na(data4$HOAMandatory) == F, ]
data4 <- data4[is.na(data4$GeoMarketArea) == F, ]
data4 <- data4[is.na(data4$PoolPrivate) == F, ]
data4 <- data4[is.na(data4$DwellingType) == F, ]
data4 <- data4[is.na(data4$Restrictions) == F, ]
data4 <- data4[is.na(data4$SchoolDistrict) == F, ]

# identify rows with more than one NA and remove to prevent imputation difficulties
missing_many <- as.integer((rownames(data4[rowSums(is.na(data4)) > 0, ])))
data4 <- data4[-missing_many, ]      # removes 714 observations

summary(data4)

### clean data - character columns

## Siding

# check remaining NAs
sum(is.na(data4$Siding)) # 0

# identify unique observations
unique(data4$Siding) # 383 variations

# # look for patterns of significance
# model_Siding = lm(data = data4, formula = ClosePrice~Siding)
# 
# # get significant coefficients
# coefs_Siding <- as.data.frame(summary(model_Siding)$coefficients)
# significant_coefs_Siding <- coefs_Siding[coefs_Siding$`Pr(>|t|)` < 0.05, ]
# 
# # get siding materials terms
# siding_materials <- paste(rownames(significant_coefs_Siding), collapse = " ")
# 
# siding_materials <- str_replace_all(siding_materials, "Siding", "")
# siding_materials <- str_replace_all(siding_materials, ",", " ")
# siding_materials <- str_replace_all(siding_materials, "  ", " ")
# 
# siding_materials <- as.data.frame(str_split(siding_materials, " "))
# siding_terms <- unique(siding_materials)
# colnames(siding_terms) <- c("term")
# siding_terms <- siding_terms[siding_terms$term != "(Intercept)" & siding_terms$term !=  "&" & siding_terms$term != "Unknown", ]

# saving execution results of code above to variable to prevent repeated fitting of large model
siding_terms <- c("Aluminum", "Asbestos", "Vinyl", "Brick", "Wood", "Other", "Stucco", "Cement", "Board", "Stone", "Synthetic")

# create dummy variables for each term
for (i in 1:length(siding_terms)) {
  siding_term <- paste(siding_terms[i])
  data4[[paste("Siding", siding_term, sep = "_")]] <- as.factor(ifelse(str_detect(data4$Siding, siding_term), 1, 0))
}

# remove original column
data4 <- subset(data4, select = -c(Siding))

## CoolSystem

# check remaining NAs
sum(is.na(data4$CoolSystem)) # 0

# unique values
unique(data4$CoolSystem) # 53 variations

# # look for patterns of significance
# model_CoolSystem = lm(data = data4, formula = ClosePrice~CoolSystem)
# 
# # get significant coefficients
# coefs_CoolSystem <- as.data.frame(summary(model_CoolSystem)$coefficients)
# significant_coefs_CoolSystem <- coefs_CoolSystem[coefs_CoolSystem$`Pr(>|t|)` < 0.05, ]
# 
# # get cooling system terms
# cooling_materials <- paste(rownames(significant_coefs_CoolSystem), collapse = ", ")
# 
# cooling_materials <- str_replace_all(cooling_materials, "\\(Intercept\\), ", "")
# cooling_materials <- str_replace_all(cooling_materials, "CoolSystem", "")
# cooling_materials <- str_replace_all(cooling_materials, "  ", " ")
# 
# cooling_materials <- as.data.frame(str_split(cooling_materials, ", "))
# cooling_terms <- unique(cooling_materials)
# colnames(cooling_terms) <- c("term")

# saving execution results of code above to variable to prevent repeated fitting of model
cooling_terms <- c("Central Electric", "Central Gas", "Zoned", "Heat Pump", "Other Cooling", "Solar Assisted", "Window Units", "No Cooling/Vent")

# create dummy variables for each term
for (i in 1:length(cooling_terms)) {
  cooling_term <- paste(cooling_terms[i])
  data4[[paste("Cooling", cooling_term, sep = "_")]] <- as.factor(ifelse(str_detect(data4$CoolSystem, cooling_term), 1, 0))
}

# remove original column
data4 <- subset(data4, select = -c(CoolSystem))

## CountyOrParish

# check remaining NAs
sum(is.na(data4$CountyOrParish)) # 0

# unique values
county_terms <- unique(data4$CountyOrParish) # 9 variations

# create dummy variables for each term
for (i in 1:length(county_terms)) {
  county_term <- paste(county_terms[i])
  data4[[paste("County", county_term, sep = "_")]] <- as.factor(ifelse(str_detect(data4$CountyOrParish, county_term), 1, 0))
}

# remove original column
data4 <- subset(data4, select = -c(CountyOrParish))

## HeatSystem

# check remaining NAs
sum(is.na(data4$HeatSystem)) # 0

# unique values
heat_terms <- unique(data4$HeatSystem) # 88 variations

# # look for patterns of significance
# model_HeatSystem = lm(data = data4, formula = ClosePrice~HeatSystem)
# 
# # get significant coefficients
# coefs_HeatSystem <- as.data.frame(summary(model_HeatSystem)$coefficients)
# significant_coefs_HeatSystem <- coefs_HeatSystem[coefs_HeatSystem$`Pr(>|t|)` < 0.05, ]
# 
# # get cooling system terms
# heating_materials <- paste(rownames(significant_coefs_HeatSystem), collapse = " ")
# 
# heating_materials <- str_replace_all(heating_materials, "(Intercept)", "")
# heating_materials <- str_replace_all(heating_materials, "HeatSystem", "")
# heating_materials <- str_replace_all(heating_materials, "\\(\\)", "")
# heating_materials <- str_replace_all(heating_materials, "  ", " ")
# 
# heating_materials <- as.data.frame(str_split(heating_materials, ", "))
# heating_terms <- unique(heating_materials)
# colnames(heating_terms) <- c("term")
# heating_terms <- str_replace_all(heating_terms$term, "Zoned ", "")
# heating_terms <- str_trim(heating_terms, side = "both")
# heating_terms <- unique(heating_terms)
# heating_terms <- heating_terms[heating_terms != "Heat Pump Central Gas" &
#                                  heating_terms != "Other Heating Central Gas" &
#                                  heating_terms != "Window Unit Central Gas"]
# heating_terms[heating_terms == "Propane Other Heating"] <- "Propane" 

# saving execution results of code above to variable to prevent repeated fitting of model
heating_terms <- c("Central Electric", "Central Gas", "Other Heating", "Solar Assisted", "Heat Pump", "Window Unit", "Space Heater",
                   "Wall Heater", "Propane", "Zoned")

# create dummy variables for each term
for (i in 1:length(heating_terms)) {
  heating_term <- paste(heating_terms[i])
  data4[[paste("Heating", heating_term, sep = "_")]] <- as.factor(ifelse(str_detect(data4$HeatSystem, heating_term), 1, 0))
}

# remove original column
data4 <- subset(data4, select = -c(HeatSystem))

## PoolPrivate

# check remaining NAs
sum(is.na(data4$PoolPrivate)) # 0

# unique values
unique(data4$PoolPrivate) # "True"/"False"

# convert to 1/0
data4$PoolPrivate <- as.factor(ifelse(data4$PoolPrivate == "True", 1, 0))

## HOAMandatory

# check remaining NAs
sum(is.na(data4$HOAMandatory)) # 0

# unique values
unique(data4$HOAMandatory) # "True"/"False"

# convert to 1/0
data4$HOAMandatory <- as.factor(ifelse(data4$HOAMandatory == "True", 1, 0))

summary(data4)

## GeoMarketArea

# check remaining NAs
sum(is.na(data4$GeoMarketArea)) # 0

# unique values
geo_terms <- unique(data4$GeoMarketArea) # 112 variations

# create dummy variables for each term
for (i in 1:length(geo_terms)) {
  geo_term <- paste(geo_terms[i])
  data4[[paste("Geo", geo_term, sep = "_")]] <- as.factor(ifelse(str_detect(data4$GeoMarketArea, geo_term), 1, 0))
}

# remove original column
data4 <- subset(data4, select = -c(GeoMarketArea))

## NewConstruction

# check remaining NAs
sum(is.na(data4$NewConstruction)) # 0

# unique values
unique(data4$NewConstruction) # "True"/"False"

# convert to 1/0
data4$NewConstruction <- as.factor(ifelse(data4$NewConstruction == "True", 1, 0))

## DwellingType

# check remaining NAs
sum(is.na(data4$DwellingType)) # 0

# unique values
unique(data4$DwellingType) # 13

# remove terms duplicative
dwelling_terms <- c("Free Standing", "Manufactured", "Duplex", "Historic", "Patio Home")

# create dummy variables for each term
for (i in 1:length(dwelling_terms)) {
  dwelling_term <- paste(dwelling_terms[i])
  data4[[paste("Dwel", dwelling_term, sep = "_")]] <- as.factor(ifelse(str_detect(data4$DwellingType, dwelling_term), 1, 0))
}

# remove original column
data4 <- subset(data4, select = -c(DwellingType))

summary(data4)

## Restrictions

# check remaining NAs
sum(is.na(data4$Restrictions)) # 0

# unique values
restriction_values <- unique(data4$Restrictions) # 64

# extract values
restriction_values <- paste(restriction_values, collapse = ", ")
restriction_values <- as.data.frame(str_split(restriction_values, ", "))
colnames(restriction_values) <- c("value")
restriction_values <- unique(restriction_values$value)

# create dummy variables for each term
for (i in 1:length(restriction_values)) {
  restriction_value <- paste(restriction_values[i])
  data4[[paste("Rest", restriction_value, sep = "_")]] <- as.factor(ifelse(str_detect(data4$Restrictions, restriction_value), 1, 0))
}

# remove original column
data4 <- subset(data4, select = -c(Restrictions))

## Roof

# check remaining NAs
sum(is.na(data4$Roof)) # 0

# unique values
roof_materials <- unique(data4$Roof) # 42

# extract values
roof_materials <- paste(roof_materials, collapse = ", ")
roof_materials <- as.data.frame(str_split(roof_materials, ", "))
colnames(roof_materials) <- c("material")
roof_materials <- unique(as.data.frame(roof_materials))
roof_materials <- roof_materials$material

# create dummy variables for each term
for (i in 1:length(roof_materials)) {
  roof_material <- paste(roof_materials[i])
  data4[[paste("Roof", roof_material, sep = "_")]] <- as.factor(ifelse(str_detect(data4$Roof, roof_material), 1, 0))
}

# remove original column
data4 <- subset(data4, select = -c(Roof))

## SchoolDistrict

# check remaining NAs
sum(is.na(data4$SchoolDistrict)) # 0

# unique values
districts <- unique(data4$SchoolDistrict) # 30

# create dummy variables for each term
for (i in 1:length(districts)) {
  district <- paste(districts[i])
  data4[[paste("Dist", district, sep = "_")]] <- as.factor(ifelse(str_detect(data4$SchoolDistrict, district), 1, 0))
}

# remove original column
data4 <- subset(data4, select = -c(SchoolDistrict))

## WaterSewer

# check remaining NAs
sum(is.na(data4$WaterSewer)) # 0

# unique values
waters <- unique(data4$WaterSewer) # 30

# extract values
waters <- paste(waters, collapse = ", ")
waters <- as.data.frame(str_split(waters, ", "))
colnames(waters) <- c("type")
waters <- unique(waters$type)

# create dummy variables for each term
for (i in 1:length(waters)) {
  water <- paste(waters[i])
  data4[[paste("WS", water, sep = "_")]] <- as.factor(ifelse(str_detect(data4$WaterSewer, water), 1, 0))
}

# remove original column
data4 <- subset(data4, select = -c(WaterSewer))

## PostalCode

# check remaining NAs
sum(is.na(data4$PostalCode)) # 0

# unique values
unique(data4$PostalCode) # 144

# extract 5 digit zips 
data4$PostalCode <- str_extract(data4$PostalCode, "77\\d{3}")

# delete rows with bad zip values (NAs)
data4 <- data4[is.na(data4$PostalCode) == F, ]

# find unique 5 digit postal codes
zips <- unique(data4$PostalCode)

# create dummy variables for each term
for (i in 1:length(zips)) {
  zip <- paste(zips[i])
  data4[[paste("Zip", zip, sep = "_")]] <- as.factor(ifelse(str_detect(data4$PostalCode, zip), 1, 0))
}

# remove original column
data4 <- subset(data4, select = -c(PostalCode))



### clean data - continuous variables

## BathsTotal

# count half baths as 0.5 instead of 0.1
data4 <- data4 %>%
  mutate(BathsTotal = (BathsTotal %% 1) * 5 + floor(BathsTotal)) 

## BedsTotal

## CloseDate
data4$CloseDate <- ymd(data4$CloseDate)

## ClosePrice

## YearBuilt

# change zeros to NAs
data4$YearBuilt[data4$YearBuilt == 0] <- NA

# drop NAs
data4 <- data4[is.na(data4$YearBuilt) == F, ]

# drop rows with clearly inaccurate/multiple build years
data4 <- data4[data4$YearBuilt <= 2024, ]

## SqFtTotal

# replace two zeros with NAs
data4$SqFtTotal[data4$SqFtTotal == 0] <- NA

# try imputation 

# find and delete singular level factors
col_names <- colnames(data4)
cols_to_delete <- c()

for (i in 1:length(col_names)) {
  col_name <- col_names[i]
  if (col_name == "SqFtTotal") {next}
  if (col_name == "CloseDate") {next}
  num_rows <- nrow(data4)
  num_zeros <- sum(data4[[col_name]] == 0)
  num_ones <- sum(data4[[col_name]] == 1)
  
  if (num_rows == num_zeros | num_rows == num_zeros) {cols_to_delete <- c(cols_to_delete, col_name)}
}

data4$"County_Brazoria" <- NULL
data4$"Geo_Alvin North" <- NULL
data4$"Dist_3 - Alvin" <- NULL

# model to predict missing SqFtTotal values
SqFtTotal_model <- lm(formula = SqFtTotal~.-ClosePrice-CloseDate, data = data4)
summary(SqFtTotal_model) #R squared of 0.7903

# array to check if any non_NAs were changed
check_array <- data4$SqFtTotal

# indices of NAs to change
na_index <- which(is.na(data4$SqFtTotal), arr.ind = T)

# loop through and impute
for (i in 1:length(na_index)) {
  ind <- na_index[i]
  data4[ind, "SqFtTotal"] <- predict(SqFtTotal_model, data4[ind, ], type = "response")
}

# change to integer
data4$SqFtTotal <- as.integer(data4$SqFtTotal)

# check changes are correct
changed_ind <- which(data4$SqFtTotal != check_array, arr.ind = T)
which(changed_ind != na_index, arr.ind = T) # 0

# order by close date
data4 <- data4[order(data4$CloseDate),]

# reset row numbers
row.names(data4) <- 1:nrow(data4)

# save as new df
data5 <- data4

# export to csv
write.csv(data5, ".../data5.csv")
