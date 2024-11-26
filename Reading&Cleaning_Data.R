#' BTC1877 Team Project Main file
#' Authors: Zack Chan, Taran Bhartt, Mingwei Cui, Youssef Emam
#' Date: December 2nd, 2024

#### Loading libraries ####
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(mice)
library(ggplot2)
library(glmnet)
library(pROC)
library(tree)
library(survival)

#### Importing Data ####
     
# Ensure that the transfusion_data.xlsx file is in the working directory to run.

# Reading data from excel file 
raw_data <- read_excel("transfusion_data.xlsx")
# NOTE: blank columns in excel file given numbers as columns 


#### Data preprocessing and cleaning ####

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

#assigning blanks and '?' to NA values
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

# Replace all non binary values with NA
# data <- data %>% 
#   mutate(`Protamine (Y=1 N=0)` = case_when(
#     `Protamine (Y=1 N=0)` != 1 | `Protamine (Y=1 N=0)` != 0 ~ NA,
#     .default = `Protamine (Y=1 N=0)`
#   ))
# 
# data[["Protamine (Y=1 N=0)"]] <- droplevels(data[["Protamine (Y=1 N=0)"]])

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

#Renaming certain similar columns, which become identical if special chars are removed
data <- data %>% 
  rename("Lung1_Functional_Plt_Number" = `Lung1_Functional Plt #`,
         "Lung1_Functional_Plt_Percent" = `Lung1_Functional Plt %`,
         "Lung2_Functional_Plt_Number" = `Lung2_Functional Plt #`,
         "Lung2_Functional_Plt_Percent" = `Lung2_Functional Plt %`
         )

# Renaming a duplicate column
colnames(data)[94] <- "ICU_DISCHARGE_DATE_TIME2"

# Adressing spaces in variable names 
# Getting column names 
col_name1 <- colnames(data)

# Converting to uppercase, trimming whitespace, comverting spaces into "_"s

col_name2 <- str_to_upper(col_name1)

col_name3 <- str_trim(col_name2)

#col_name4 <- str_replace_all(col_name3, " ", "_")

col_name4 <- gsub("[^A-Za-z0-9]+", "_", col_name3)

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

#Creating a column that records whether a transfusion ocurred
# Use total 24H RBC, second last column - THIS SEEMS TO CONFLICT WITH THE OTHER RBC COLUMNS, why?

data <- data %>% 
  mutate(TRANSFUSION_GIVEN = case_when(
    TOTAL_24HR_RBC == 0 ~ 0,
    TOTAL_24HR_RBC != 0 & !is.na(TOTAL_24HR_RBC) ~ 1
  ))

data$TRANSFUSION_GIVEN <- factor(data$TRANSFUSION_GIVEN, levels = c(1,0), labels = c(TRUE, FALSE))


summary(data)
#Need to go fix RBC count data, there are some NAs where there should be zeros
# We can wait and see which variables we will actually use and that will help determine which ones to clean


#### Initial Imputation / Missing Data Processing ####

#Subsetting the data to select only the relevant columns
# THIS WILL NEED TO CHANGE
modeling_data <- data %>%
  select(TYPE:PROTAMINE_Y_1_N_0_, TRANSFUSION_GIVEN)

#Removing columns with > 30% missingness
for (name in names(modeling_data)){
  pct_missing <- sum(is.na(modeling_data[[name]]))/length(modeling_data[[name]])
  cat(name, ":", pct_missing,"\n")
  
  #drop the column if 
  if (pct_missing >= 0.3){
    modeling_data[[name]] <- NULL
  }
}

imputed_data <- mice(data = modeling_data, m = 1, seed = 123, defaultMethod = c("pmm", "logreg", "polyreg", "polyr"))
#Figure out the error - likely due to coliniearity, this will be resolved when we narrow down which variables to select
# Will also likely need to deal with colinearity of the data when imputing

#Extract the imputed dataset

imputed_data <- complete(imputed_data)

#No NAs
anyNA(imputed_data)


#### Deciding on model with highest discrimination ####

#Create/ Cross validate the models 5 times, and then test and capture AUC
# Use the model with the highest AUC


#Create data frame to hold results
results <- data.frame(trial = 1:5, lasso_AUC = 1:5, pruned_tree_AUC = 1:5, tree_AUC = 1:5)


# Create loop to repeat the process 5 times and capture the result each time
for (i in 1:5){
  
  #Set a random seed to reduce sampling bias in each iteration
  set.seed(sample(1:1000, 1))
  
  #Splitting the data into training and test (80:20 split)
  test_index <- sample(nrow(imputed_data), nrow(imputed_data)/5)
  
  testing_data <- imputed_data[test_index,]
  training_data <-imputed_data[-test_index,]
  
  #### Building lasso classifier ####
  
  #create model matrix for the features, remove intercept
  features <- model.matrix(TRANSFUSION_GIVEN~., training_data)[,-1]
  
  #create response vector
  response <- training_data$TRANSFUSION_GIVEN
  
  #identify the lambda which minimizes AUC using 5 fold cross validation
  classifier_tuning <- cv.glmnet(features, response, alpha = 1, family = "binomial", type.measure = "auc", nfolds = 5)
  
  
  #extract min lambda
  min_lambda <- classifier_tuning$lambda.min
  
  #Create a model
  classifier_model <- glmnet(features, response, family = "binomial")
  
  #create new data for predictions, remove intercept
  newdata <- model.matrix(TRANSFUSION_GIVEN~., testing_data)[,-1]
  
  #Make predictions using the trained model
  predictions <- as.numeric(predict(classifier_model, newx = newdata, s= min_lambda, type = "response"))
  
  # plot ROC
  ROC <- roc(TRANSFUSION_GIVEN ~ predictions, data=testing_data)
  
  #Save the AUC to the DF
  results[i, "lasso_AUC"] <- ROC$auc
  
  #### Building classification trees ####
  
  #create a classification tree excluding time
  classification_tree <- tree(TRANSFUSION_GIVEN ~., data = imputed_data, subset = -test_index)
  
  
  #Cross validation for pruning
  cv.classification_tree <- cv.tree(classification_tree, FUN=prune.tree, K = 5)
  
  #extract the tree with the lowest deviance
  best.size <- cv.classification_tree$size[which.min(cv.classification_tree$dev)]
  
  #make sure the tree is not a stump
  if (best.size == 1){
    best.size <- 2
  }
  
  #prune the tree to be the optimal size
  pruned_tree <- prune.misclass(classification_tree, best=best.size)
  
  
  #predict using the classifier tree
  pruned_tree_prediction = predict(pruned_tree, newdata = testing_data, type = "vector")
  
  #extract probability of event
  pruned_tree_probabilities <- pruned_tree_prediction[,2]
  
  #predict using unpruned tree
  tree_prediction = predict(classification_tree, newdata = testing_data, type = "vector")
  
  #extract probability of event
  tree_probabilities <- tree_prediction[,2]
  
  #determine ROC for pruned tree
  pruned_tree_ROC <- roc(TRANSFUSION_GIVEN ~ pruned_tree_probabilities, data = testing_data)

  
  #save results for the pruned tree
  results[i, "pruned_tree_AUC"] <- pruned_tree_ROC$auc
  
  #determine ROC for unpruned tree
  tree_ROC <- roc(TRANSFUSION_GIVEN ~ tree_probabilities, data = testing_data)
  
  #save AUC for tree
  results[i, "tree_AUC"] <- tree_ROC$auc
  
}

#pivot the data longer
results <- pivot_longer(results, cols = lasso_AUC:tree_AUC, names_to = "Model", values_to = "AUC")

# Calculate the Average AUC for each classifier
results <- results %>% 
  group_by(Model) %>% 
  summarise(average_auc = mean(AUC))

# The lasso classifier appears to be the model with the best discrimination


