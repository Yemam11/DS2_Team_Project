# Pulling Data for Team Assignment 

# Setting libraries 
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
        
# Setting file path 
#file_path <- "/Users/zachery/Downloads/transfusion_data.xlsx"

# Reading data from excel file 
raw_data <- read_excel("transfusion_data.xlsx")
# NOTE: blank columns in excel file given numbers as columns 

colnames(raw_data)
# blank columns of DH - DK renamed as 112 - 115 

# Seeing dimensions, confirms with excel 
dim(raw_data)

# NAs in data 
anyNA(raw_data)
# Blanks in excel sheet converted to NAs 

# Checking data type 
str(raw_data)
# Need to factor some variables, some dates listed as character

# Storing data in new df 
data <- raw_data %>%
  select(-112, -113, -114, -115)
# Getting rid of blank columns 

for (col in colnames(data)) {
  if (is.character(data[[col]]) || is.factor(data[[col]])) {
  if (any(data[[col]] == "?", na.rm = TRUE)) {
    data[[col]][data[[col]] == "?"] <- NA
    }
  if (any(data[[col]] == " ", na.rm = TRUE)) {
    data[[col]][data[[col]] == " "] <- NA
    }
  }
}


# List of variables which need to be factored 
factor_columns <- c("Type", 
                   "Gender (male)", 
                   "COPD",                                        
                   "alpha1-Antitrypsin Deficiency",
                   "Cystic Fibrosis",                             
                   "Idiopathic Pulmonary Hypertension",
                   "Interstitial Lung Disease",                
                   "Pulm_Other",                                 
                   "Coronary Artery Disease",                     
                   "Hypertension",
                   "Diabetes (insulin)",                    
                   "Diabetes (diet/OHGs)",
                   "GERD/PUD",                                 
                   "Renal Failure",
                   "Stroke/CVA",                               
                   "Liver Disease",
                   "Thyroid Disease",
                   "First Lung Transplant",
                   "Redo Lung Transplant",                       
                   "DCD vs DBD",
                   "ExVIVO Lung Perfusion",
                   "Preoperative ECLS",
                   "Intraoperative ECLS",
                   "ECLS_ECMO",                                 
                   "ECLS_CPB",
                   "Protamine (Y=1 N=0)",
                   "Tranexamic Acid Used",
                   "ALIVE_30DAYS_YN",
                   "ALIVE_90DAYS_YN",                       
                   "ALIVE_12MTHS_YN",
                   "Need for reoperation for bleeding within 24h", 
                   "Massive Transfusion")

# Using a for loop to factor variables 
for (col in factor_columns) {
  data[[col]] <- factor(data[[col]])
}

# Checking the result
str(data)
# NOTE: ODDITIES IN PROTAMINE --> SHOULD BE A FACTOR OF 0/1, see levels of 25 & 400

# Converting dates 
data$`Extubation Date` <-  ymd_hms(data$`Extubation Date`)

data$DEATH_DATE <-  dmy(data$DEATH_DATE)

# FOUND REPIITION: ICU STAY DOUBLED, DIFFERENCES IN THE DATA 
ICU_data <- data %>%
  select(`Duration of ICU Stay (days)`, `Duration of ICU stay (days)`)

# Seeing where there are differences 
data$`Duration of ICU stay (days)` == data$`Duration of ICU Stay (days)`
# one has NA at 152, ROUNDING ERRORS AND DIFFERENT NUMBERS POST 152 INDICE

# DECIDING TO KEEP "Duration of ICU stay (days)" AS IT HAS LESS MISSING DATA (152+ IS NOT ALL 0s)
data <- data %>%
  select(-`Duration of ICU Stay (days)`)

# Adressing spaces in variable names 
# Getting column names 
col_name1 <- colnames(data)

# Converting to uppercase, trimming whitespace, comverting spaces into "_"s
col_name2 <- str_to_upper(col_name1)

col_name3 <- str_trim(col_name2)

col_name4 <- str_replace_all(col_name3, " ", "_")

colnames(data) <- col_name4

summary(data)

# Dealing with double of "Exturbation Date" 
# One column had dates the other also had dates as excel serial dates 

# Converting excel format serial dates into POSIXt
data$DATE_OF_EXTUBATION <- as.Date(as.numeric(data$DATE_OF_EXTUBATION), origin = "1899-12-30") 

data$DATE_OF_EXTUBATION <- as.POSIXct(data$DATE_OF_EXTUBATION)

# Comparing differences 
extubation_data <- data %>%
  select(DATE_OF_EXTUBATION, EXTUBATION_DATE)

# Using "DATE_OF_EXTUBATION" as it has less missing data, also renaming a long column name 
data <- data %>%
  select(-EXTUBATION_DATE) %>%
  rename("NEED_REOPERATION_WITHIN_24H" = NEED_FOR_REOPERATION_FOR_BLEEDING_WITHIN_24H)

# Factoring Massive transfution 
data$MASSIVE_TRANSFUSION <- factor(data$MASSIVE_TRANSFUSION, levels = c(0,1), labels = c(FALSE, TRUE))

summary(data)

#Need to go fix RBC count data, there are some NAs where there should be zeros
