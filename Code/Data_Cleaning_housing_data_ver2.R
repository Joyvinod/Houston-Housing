### clear all
rm(list = ls())

# clear plots
dev.off()  # only if there is a plot

# clear console # Edit clear console

cat("\014")  # or use ctrl+L



### set wd to the place where the data folder is residing
getwd()
setwd("C:/Users/kakka/OneDrive/GeorgiaTech/MGT 6203/Team_60")

# Step 1: Load the data
housing_data <- read.csv("combined_file.csv")
# Handle missing values
housing_data[housing_data == ""] <- NA


# Step 2: Explore the data
head(housing_data)  # View the first few rows
summary(housing_data)  # Summary statistics
str(housing_data)  # Structure of the dataset
dim(housing_data)  # Dimensions of the dataset


# Let us check the columns and the missing values as a percent.
# Assuming 'data' is your dataset
# Calculate percentage of missing values for each column
missing_percentage <- colMeans(is.na(housing_data)) * 100

# Create a data frame with column names and their missing percentages
missing_df <- data.frame(
  Column = names(housing_data),
  Missing_Percentage = round(missing_percentage, 2)
)




# Identify columns with 30% or more missing values
columns_to_keep <- names(housing_data)[missing_percentage <= 50]
length(columns_to_keep)[]

# Remove columns with 20% or more missing values
data_cleaned_stage1 <- housing_data[, names(housing_data) %in% columns_to_keep]
colnames(data_cleaned_stage1)
#write.csv(data_cleaned_stage1, file = "data_cleaned_stage1.csv", row.names = FALSE)

# now let us select the columns from stage1 data and create a new dataset to use
columns_to_be_kept <- c(
  "Acres", 
  "BathsTotal", 
  "BedsTotal", 
  "SqFtTotal", 
  "CloseDate", 
  "ClosePrice", 
  "Connections", 
  "Siding", 
  "CoolSystem", 
  "CountyOrParish", 
  "Dishwasher", 
  "Disposal", 
  "Energy", 
  "Exterior", 
  "Floors", 
  "ForLease", 
  "GarageDesc", 
  "HeatSystem", 
  "HOAMandatory", 
  "Interior", 
  "GeoMarketArea", 
  "Microwave", 
  "NewConstruction", 
  "OvenType", 
  "PoolPrivate", 
  "PropertyType", 
  "DwellingType", 
  "Restrictions", 
  "Roof", 
  "SchoolDistrict", 
  "RangeType", 
  "WaterSewer", 
  "YearBuilt", 
  "PostalCode"
  
)


data_cleaned_stage2 <- data_cleaned_stage1[, columns_to_be_kept]
colnames(data_cleaned_stage2)

#write.csv(data_cleaned_stage2, file = "data_cleaned_stage2.csv", row.names = FALSE)


###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
###########################################################################################################################



library(tidyverse)


col_to_delete ="NA"
process_comma_separated_values <- function(data, col_name) {
  data <- data %>%
    mutate(!!sym(col_name) := str_split(!!sym(col_name), ", ")) %>%
    unnest(c(!!sym(col_name))) %>%
    count(across(everything())) %>%
    pivot_wider(names_from = !!sym(col_name), values_from = n, values_fill = 0)
  return(data)
}

delete_column <- function(data, col_name) {
  if (col_name %in% colnames(data)) {
    data <- data[, !colnames(data) %in% col_name, drop = FALSE]
    return(data)
  } else {
    #print(paste("Column", col_name, "does not exist."))
    return(data)
  }
}



# These are the columns which have comma seperated values. I have seperated the values into their own columns and it has a 1 or 0
# depending on if that value is present
concatenated_columns <- c("Connections",  "Siding" , "CoolSystem"
                          , "Energy", "Exterior", "Floors", "GarageDesc"
                          , "HeatSystem",  "DwellingType", "Roof"
                          , "Restrictions", "RangeType", "WaterSewer")


# lets name our data from stage3 to processed data
processed_data <- data_cleaned_stage2

for (col in concatenated_columns) {
  processed_data <- process_comma_separated_values(processed_data, col)
  processed_data <- delete_column(processed_data, col_to_delete)
  unique_values <- unique(trimws(unlist(strsplit(as.character(data_cleaned_stage2[[col]]), ","))), na.rm = TRUE)
  unique_values <-na.omit(unique_values)
  for (val in unique_values) {
    if(!is.na(val)) {
    old_column <- val
    new_column <- paste(col, val, sep = "_")
    processed_data <- processed_data %>%
      rename_with(~ new_column, .cols = all_of(old_column))
  }
  }
  print(unique_values)
}


data_cleaned_stage3 <- processed_data



dim(data_cleaned_stage3)
colnames(data_cleaned_stage3)
#write.csv(data_cleaned_stage3, file = "data_cleaned_stage3.csv", row.names = FALSE)
