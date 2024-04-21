# Team-60
 Team 60's group project GitHub repository for MGT 6203 (Canvas) Spring of 2024 semester.

### Title
Combining Disparate Approaches to Better Predict Future Residential Housing Prices

### Members
|Name            |Email                |
|:--------------:|:-------------------:|
|Evan Swain      |eswain7@gatech.edu.  |
|Joy Kakkanad    |jkakkanad3@gatech.edu|
|Marc Presume    |mpresume3@gatech.edu |
|Russell Dawkins |rdawkins6@gatech.edu |

### Description
The data and code in this repository was used for the analysis in the final report of Group 60

### Instructions
The table of contents is organized by the section numbers of our final report. For each section, we have left instructions to recreate the steps of our analysis.

### Table of contents
|  Readme Section | Report Section  | Title                                     |Author                      |Email                                     |
|:---------------:|:---------------:|:-----------------------------------------:|:--------------------------:|:----------------------------------------:|
|        A        |        -        | [Execution environment](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#a-execution-environment)                     |Evan Swain                  |eswain7@gatech.edu                        |
|        B        |       2.1       | [Data Collection and Characteristics](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#b-data-collection-and-characteristics-final-report-section-21)       |Evan Swain                  |eswain7@gatech.edu                        |
|        C        |      2.2.1      | [HAR Data Cleaning](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#c-har-data-cleaning-final-report-section-221)                         |Joy Kakkanad                |jkakkanad3@gatech.edu                     |
|        D        |      2.2.2      | [Google Trends Data Cleaning](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#d-google-trends-data-cleaning-final-report-section-222)               |Evan Swain                  |eswain7@gatech.edu                        |
|        E        |       2.4       | [Data Exploration re: correlation](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#e-data-exploration-re-correlation-final-report-section-24)          |Evan Swain & Russell Dawkins|eswain7@gatech.edu & rdawkins6@gatech.edu |
|        F        |    2.5.1/3.1    | [Lasso Regression](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#f-lasso-regression-final-report-sections-251-and-31)                          |Russell Dawkins             |rdawkins6@gatech.edu                      |
|        G        |    2.5.2/3.2    | [Linear Regression](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#g-linear-regression-final-report-sections-252-and-32)                         |Marc Presume                |mpresume3@gatech.edu                      |
|        H        |       3.3       | [Principal Component Analysis](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#h-principal-component-analysis-final-report-section-33)              |Russell Dawkins             |rdawkins6@gatech.edu                      |
|        I        |    2.5.3/3.4    | [PCA-RegEx-Stepwise Model](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#i-pca-regex-stepwise-model-final-report-sections-253-and-34)                  |Evan Swain                  |eswain7@gatech.edu                        |
|        J        |     2.6/3.5     | [Mean price prediction with Google Trends](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#j-mean-price-prediction-with-google-trends-final-report-sections-26-and-35)  |Evan Swain                  |eswain7@gatech.edu                        |
|        K        |     2.7/3.6.    | [Adding Google Trends data to basline model](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#k-adding-google-trends-data-to-basline-model-final-report-sections-27-and-36)|Evan Swain                  |eswain7@gatech.edu                        |
|        L        |       3.7.      | [Exploration of additional variables](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#l-exploration-of-additional-variables-final-report-section-37)       |Joy Kakkanad & Evan Swain   |jkakkanad3@gatech.edu & eswain7@gatech.edu|

### A. Execution Environment
Our code was written in the R programming language and is saved in the R script format. R 4.3.1 is required to run it. 

### B. Data Collection and Characteristics (Final Report Section 2.1)
The HAR data file was too large (> 25MB) to upload to github. This data was sourced from the realtor section of https://www.har.com/. Please contact eswain7@gatech.edu for access.  
  
The seven Google Trends time series data can be found in the code folder. This data was sourced from https://trends.google.com/trends/.  

-[time series for "HAR houses for sale"](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Data/2004-2024_US_har%20houses%20for%20sale.csv)  
-[time series for "HAR Houston"](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Data/2004-2024_US_har%20houston.csv)  
-[time series for "HAR Real Estate"](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Data/2004-2024_US_har%20real%20estate.csv)  
-[time series for "houses for sale Houston"](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Data/2004-2024_US_houses%20for%20sale%20houston.csv)  
-[time series for "Houston home prices"](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Data/2004-2024_US_houston%20home%20prices.csv)  
-[time series for "Houston Real Estate"](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Data/2004-2024_US_houston%20real%20estate.csv)  
-[time series for "Houston realtor"](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Data/2004-2024_US_houston%20realtor.csv)   
 
Data used for additional analysis can also be found in the code folder.

[Federal interest rate data](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Data/Federalinterestrate.csv)  
[M2 money supply data from FRED](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Data/m2_supply_fred.csv) - data sourced from [Federal Reserve Bank of St. Louis](https://fred.stlouisfed.org/series/M2SL)  
[US Inflation data](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Data/usainflation.csv)

### C. HAR Data Cleaning (Final Report Section 2.2.1)

### D. Google Trends Data Cleaning (Final Report Section 2.2.2)
The code for the cleaning of the Google Trends data and creation of Google Trends factors can be found [here](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Code/20240317_google_trends_analysis.R). The analysis takes the seven Google Trends time series and outputs a data frame named "gtrends." 

### E. Data Exploration re: correlation (Final Report Section 2.4)
The code for data exploration and correlation can be found [here](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/main/Code/Final%20Project_Marc.R)
### F. Lasso Regression (Final Report Sections 2.5.1 and 3.1) 

### G. Linear Regression (Final Report Sections 2.5.2 and 3.2)        
The code for the Linear Regression model can be found [here](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/main/Code/Final%20Project_Marc.R)
### H. Principal Component Analysis (Final Report Section 3.3)

### I. PCA-RegEx-Stepwise Model (Final Report Sections 2.5.3 and 3.4)
The code for the regular expression analysis can be found [here](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Code/20240407_data_cleaning_git.R). It takes the data frame "data_cleaned_stage2" from [Section C](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#c-har-data-cleaning-final-report-section-221) as input and outputs a data frame "data5."

The code for the PCA transformation can be found [here](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Code/20240407_basic_pca_models_git.R). It takes the data frame "data5" from the Regular Expression analysis above as an input and outputs a data frame "data6."

The code for the forward stepwise variable selection can be found [here](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Code/20240409_variable_selection_git.R). It takes the data frame "data6" from the PCA transformation above as an input and outputs a data frame "data7."

### J. Mean price prediction with Google Trends (Final Report Sections 2.6 and 3.5)
The code for the mean monthly price prediction analysis can be found [here](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Code/20240317_google_trends_analysis.R) at lines 122-176 as part of the larger analysis/conversion of Google Trends data.

### K. Adding Google Trends data to basline model (Final Report Sections 2.7 and 3.6)
The code for measuring prediction performance increase to the PCA-RegEx-Stepewise model by adding the Google Trends data can be found [here](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Code/20240410_adding_gtdata_git.R). It takes "data7" from [Section I](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#i-pca-regex-stepwise-model-final-report-sections-253-and-34) and "gtrends" from [Section D](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#d-google-trends-data-cleaning-final-report-section-222) as inputs and outputs a data frame "data9."

### L. Exploration of additional variables (Final Report Section 3.7)
This analysis was done in two sepearate sections of code. The analysis of adding M2 money supply data and the resulting model can be found [here](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Code/20240410_adding_m2_git.R) and takes "data9" from [Section K](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/edit/main/README.md#k-adding-google-trends-data-to-basline-model-final-report-sections-27-and-36) and the [M2 Money Supply data from FRED](https://github.gatech.edu/MGT-6203-Spring-2024-Canvas/Team-60/blob/4af1f9791ea4eafa03f35a5bf707eb1fe0726199/Data/m2_supply_fred.csv) as inputs.
