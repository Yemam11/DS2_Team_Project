#### DATA FOR KAPLAN MEIER ####

library(lubridate)

# Making duplicate of data for ease of making changes
modeling_data2 <- data %>%
  select(TRANSFUSION_GIVEN, TOTAL_24HR_RBC ,PREOPERATIVE_ECLS, 
         ECLS_ECMO, ECLS_CPB, COPD, CYSTIC_FIBROSIS, PRE_HB, 
         PRE_PLATELETS, PRE_PT, PRE_INR, PRE_CREATININE, 
         PROTAMINE_Y_1_N_0_, INTRA_ALBUMIN_5_ML_, INTRA_CRYSTALLOID_ML_, 
         INTRA_CELL_SAVER_RETURNED_ML_, INTRA_CRYOPRECIPITATE, LAS_SCORE, 
         BMI, AGE, TYPE, IDIOPATHIC_PULMONARY_HYPERTENSION, HYPERTENSION, MASSIVE_TRANSFUSION, 
         RENAL_FAILURE, DIABETES_INSULIN_, GENDER_MALE_, 
         INTRA_FRESH_FROZEN_PLASMA, INTRA_PACKED_CELLS, INTRA_PLATELETS, INTRA_CRYOPRECIPITATE, 
         OR_DATE, DEATH_DATE)
         
         #PGD_AT_72HRS_GRADE_0_3_, NEED_REOPERATION_WITHIN_24H, 
         #ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN, ICU_LOS, HOSPITAL_LOS)
# selecting other mortality variables + time variables to calculate time 

# Creating factored transfusion variable
modeling_data2$TRANSFUSION_FACT <- ifelse(modeling_data$TOTAL_24HR_RBC == 0, 0,
                                          ifelse(modeling_data$TOTAL_24HR_RBC <= 3, 1,
                                                 ifelse(modeling_data$TOTAL_24HR_RBC <= 6, 2,
                                                        ifelse(modeling_data$TOTAL_24HR_RBC <= 9, 3, 4))))

modeling_data2$TRANSFUSION_FACT <- factor(modeling_data2$TRANSFUSION_FACT, 
                                          levels = c(0,1,2,3,4), 
                                          labels = c("None", "Small", "Medium", "Large", "Massive"))

summary(modeling_data2$TRANSFUSION_FACT)

# Constructing time variables for primary + secondary analysis 
# initializing time variable for secondary analysis 
modeling_data2$TIME_2 <- NA 

# converting date (currently in date form) into Posix for manipulation
modeling_data2$DEATH_DATE <- as.POSIXct(modeling_data2$DEATH_DATE)

# checking if anyone dead at exactly 1 year 
which((modeling_data2$DEATH_DATE - modeling_data2$OR_DATE) == 365)

# using for loop to calculate time till death for each individual for those who have recorded deaths
for (n in 1:nrow(modeling_data2)) {
  if (is.na(data$DEATH_DATE[n])) {
    modeling_data2$TIME_2[n] <- 365
  } else {
    modeling_data2$TIME_2[n] <- data$DEATH_DATE[n]  - data$OR_DATE[n]
  }
} 

# examining time variables 
modeling_data2$TIME_2

# as no one died at 365 days, can correctly assume all those with time = 365 were alive and censored
# Creating event variable for kaplan meier curves, creating indicator for patients w/ observed deaths 
modeling_data2$DEAD <- ifelse(modeling_data2$TIME_2 != 365, 1, 0)

# initializing time variable for primary analysis 
modeling_data2$TIME_1 <- NA

# using for loop to calculate time till death for each individual for those who have recorded deaths within 1 year
for (n in 1:nrow(modeling_data2)) {
  if (is.na(data$DEATH_DATE[n])) {
    modeling_data2$TIME_1[n] <- 365
  } else if (365 >= (data$DEATH_DATE[n]  - data$OR_DATE[n])) {
    modeling_data2$TIME_1[n] <- data$DEATH_DATE[n]  - data$OR_DATE[n]
  } else {
    modeling_data2$TIME_1[n] <- 365
  }
} 

modeling_data2$TIME_1 

#### TESTING WHICH CAN BE STRATIFIED ####

# Selecting data for patients who died 
death_data <- modeling_data2 %>%
  filter(DEAD == 1) 

# Seeing if data can be stratified on lung transplant type
summary(death_data$TYPE)
# Need to collapse single transplants

# Collapsing type variable, re-factoring
modeling_data2$TYPE <- ifelse(modeling_data2$TYPE == "Single Left Lung", "Single",
                              ifelse(modeling_data2$TYPE == "Single Right Lung", "Single", "Bilateral"))

modeling_data2$TYPE <- factor(modeling_data2$TYPE)

summary(modeling_data2$TYPE)

# Seeing if data can be stratified on life support data
summary(death_data$PREOPERATIVE_ECLS)

summary(death_data$ECLS_CPB)
# Barely any event data for pre-op life support or CPB

summary(death_data$ECLS_ECMO)
# Some data for ECMO ventilation 

# Collapsing life support data, re-factoring
modeling_data2$ECLS <- ifelse((modeling_data2$PREOPERATIVE_ECLS == "TRUE" |
                                      modeling_data2$ECLS_ECMO == "TRUE" | modeling_data2$ECLS_CPB == "TRUE"), 1, 0)

modeling_data2$ECLS <- factor(modeling_data2$ECLS, levels = c(0,1), labels = c("FALSE","TRUE"))   
# 1/T = any life support, 0/F = no life support

summary(modeling_data2$ECLS)

# Testing stratifying by transfusion
summary(death_data$MASSIVE_TRANSFUSION)
# CANNOT STRATIFY ON MASSIOVE, ONLY 1 DEAD w/ MASSIVE 

# Examining if any comobidities can be used for stratification
summary(death_data$DIABETES_INSULIN_)
# barely any to stratify 

summary(death_data$RENAL_FAILURE)
# barely any to stratify 

summary(death_data$HYPERTENSION)
# small amount of event data 

summary(death_data$IDIOPATHIC_PULMONARY_HYPERTENSION)
# no data to stratify 
# Cannot justify stratifying on solely hypertension as a comorbidity

# Testing stratifying by gender
summary(death_data$GENDER_MALE_)

# Testing stratifying by COPD
summary(death_data$COPD)

# Testing stratifying by CF
summary(death_data$CYSTIC_FIBROSIS)

# Creating variable to measure presence of lung disease, refactoring
modeling_data2$LUNG_DISEASE <- ifelse((modeling_data2$COPD == "TRUE" | modeling_data2$CYSTIC_FIBROSIS == "TRUE"), 1, 0)

modeling_data2$LUNG_DISEASE <- factor(modeling_data2$LUNG_DISEASE, levels = c(0,1), labels = c("FALSE", "TRUE"))


# Seeing transfusion data in those who died 
death_data$TOTAL_24HR_RBC

death_data$TRANSFUSION_FACT

# Stratifying based on the following patient characteristics: 
# - Type of life support given (non invasive ventilation, pre-op invasive ventilation (CPB/ECMO/ECLS), pre and/or post-op invasive ventilation (CPB/ECMO/ECLS)
# - Type of lung transplant (single vs bilateral)
# - Gender 
# - Presence of most common lung disease COPD 
# - Presence of any Lung disease, specifically COPD or CF 


#### PRIMARY KM ANALYSIS ####
# Stratifying by life support 
spprt_curves_prim <- survfit(Surv(TIME_1, DEAD =="1") ~ ECLS, data = modeling_data2)

plot(spprt_curves_prim,
     conf.int =0.95,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Kaplan-Meier Curves by ECLS Status")

legend("bottomleft",                   
       legend = c("No ECLS", "ECLS (ECMO/CPB)"), 
       col = c("blue", "red"),
       lty = 1, 
       title = "ECLS Status")           

# Stratifying by transplant type
type_curves_prim <- survfit(Surv(TIME_1, DEAD =="1") ~ TYPE, data = modeling_data2)

plot(type_curves_prim,
     conf.int = 0.95,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Kaplan-Meier Curves by Transplant Type")

legend("bottomleft",                    
       legend = levels(modeling_data2$TYPE), 
       col = c("blue", "red"),                
       lty = 1,                       
       title = "ECLS Status")

# Stratifying by gender
gndr_curves_prim <- survfit(Surv(TIME_1, DEAD =="1") ~ GENDER_MALE_, data = modeling_data2)

plot(gndr_curves_prim, 
     conf.int = 0.95,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Kaplan-Meier Curves by Gender")

legend("bottomleft",                    
       legend = c("Female", "Male"), 
       col = c("blue", "red"),                
       lty = 1,                       
       title = "Gender")

# Stratifying by COPD
copd_curves_prim <- survfit(Surv(TIME_1, DEAD =="1") ~ COPD, data = modeling_data2)

plot(copd_curves_prim,
     conf.int = 0.95,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Kaplan-Meier Curves by COPD Presence")

legend("bottomleft",                    
       legend = c("No COPD", "COPD"), 
       col = c("blue", "red"),                
       lty = 1,                       
       title = "COPD Status")

# Stratifying by presence of ANY lung disease
lng_curves_prim <- survfit(Surv(TIME_1, DEAD =="1") ~ LUNG_DISEASE, data = modeling_data2)

plot(lng_curves_prim,
     conf.int = 0.95,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Kaplan-Meier Curves by Lung Disease Presence")

legend("bottomleft",                    
       legend = c("No Lung Disease", "Any Lung Disease (COPD or CF)"), 
       col = c("blue", "red"),                
       lty = 1,                       
       title = "COPD Status")

# Stratifying by transfusion amount\
trans_curves_prim <- survfit(Surv(TIME_1, DEAD =="1") ~ TRANSFUSION_FACT, data = modeling_data2)

plot(trans_curves_prim,
     conf.int =0.95,
     col = 1:5,           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Kaplan-Meier Curves by Transfusion Amount")

legend("bottomleft",                   
       legend = levels(modeling_data2$TRANSFUSION_FACT), 
       col = 1:5,
       lty = 1, 
       title = "Transfusion Amount")           

#### SECONDARY KM ANALYSIS####
# Stratifying by life support 
spprt_curves_sec <- survfit(Surv(TIME_2, DEAD =="1") ~ ECLS, data = modeling_data2)

plot(spprt_curves_sec,
     conf.int =0.95,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Secondary Kaplan-Meier Curves by ECLS Status")

legend("bottomleft",                   
       legend = c("No ECLS", "ECLS (ECMO/CPB)"), 
       col = c("blue", "red"),
       lty = 1, 
       title = "ECLS Status")           

# Stratifying by transplant type
type_curves_sec <- survfit(Surv(TIME_2, DEAD =="1") ~ TYPE, data = modeling_data2)

plot(type_curves_sec,
     conf.int = 0.95,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Secondary Kaplan-Meier Curves by Transplant Type")

legend("bottomleft",                    
       legend = levels(modeling_data2$TYPE), 
       col = c("blue", "red"),                
       lty = 1,                       
       title = "ECLS Status")

# Stratifying by gender
gndr_curves_sec <- survfit(Surv(TIME_2, DEAD =="1") ~ GENDER_MALE_, data = modeling_data2)

plot(gndr_curves_sec, 
     conf.int = 0.95,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Secondary Kaplan-Meier Curves by Gender")

legend("bottomleft",                    
       legend = c("Female", "Male"), 
       col = c("blue", "red"),                
       lty = 1,                       
       title = "Gender")

# Stratifying by COPD
copd_curves_sec <- survfit(Surv(TIME_2, DEAD =="1") ~ COPD, data = modeling_data2)

plot(copd_curves_sec,
     conf.int = 0.95,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Secondary Kaplan-Meier Curves by COPD Presence")

legend("bottomleft",                    
       legend = c("No COPD", "COPD"), 
       col = c("blue", "red"),                
       lty = 1,                       
       title = "COPD Status")

# Stratifying by presence of ANY lung disease
lng_curves_sec <- survfit(Surv(TIME_2, DEAD =="1") ~ LUNG_DISEASE, data = modeling_data2)

plot(lng_curves_prim,
     conf.int = 0.95,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Secondary Kaplan-Meier Curves by Lung Disease Presence")

legend("bottomleft",                    
       legend = c("No Lung Disease", "Any Lung Disease (COPD or CF)"), 
       col = c("blue", "red"),                
       lty = 1,                       
       title = "COPD Status")


#### KM CURVES ASSUMPTION TEST ####
survdiff(Surv(time, status==1) ~ sex,data=melanoma)

#### COX PROPORTIONAL HAZARD MODEL DATA ####
library(survival)
library(boot)

# Running base cox-ph model to see if there are any issues 
cox_model <- coxph(Surv(TIME_1, DEAD =="1")~ ., data=modeling_data2)
# seeing convergence issues with variables 5 and 21 here, 
# specifically ECLS_CPBTRUE & IDIOPATHIC_PULMONARY_HYPERTENSIONTRUE, also need to collapse transfusion, specifically massive

# Seeing if mortality variables can converge
# Seeing if CPB data can converge or is only within 1 group
CPB_data <- modeling_data2 %>%
  select(ECLS_CPB, DEAD) %>%
  filter(DEAD == "1")

any(CPB_data$ECLS_CPB == "TRUE")
# No deaths have CPB, cant use in cox model 
# Instead use ECLS indicator made previously (any ECLS vs none)

# Seeing if IDIOPATHIC_PULMONARY_HYPERTENSION data can converge or is only within 1 group
IPH_data <- modeling_data2 %>%
  select(IDIOPATHIC_PULMONARY_HYPERTENSION, DEAD) %>%
  filter(DEAD == "1")

any(IPH_data$IDIOPATHIC_PULMONARY_HYPERTENSION == "TRUE")
# No deaths have IDIOPATHIC_PULMONARY_HYPERTENSION, cant use in cox model 

# Seeing if HYPERTENSION data can converge or is only within 1 group
hypertension_data <- modeling_data2 %>%
  select(HYPERTENSION, DEAD) %>%
  filter(DEAD == "1")

any(hypertension_data$HYPERTENSION == "TRUE")
# Some deaths have HYPERTENSION, will include in cox model 

modeling_data2$TRANSFUSION_FACT <- ifelse

test <- modeling_data2 %>%
  select(DEAD, TRANSFUSION_FACT, LAS_SCORE) %>%
  filter(TRANSFUSION_FACT == "Massive")

any(test$TRANSFUSION_FACT == "Massive")

test2 <- modeling_data2 %>%
  select(DEAD, TRANSFUSION_FACT, MASSIVE_TRANSFUSION)

any(test2$TRANSFUSION_FACT == "Massive")
# transfusion only 

# Making cox-ph model data for primary & secondary analysis, need to remove variables w/ too high missingness 
cox_data_prim <- modeling_data2 %>%
  select(-PROTAMINE_Y_1_N_0_, -LUNG_DISEASE, -OR_DATE, -DEATH_DATE, -IDIOPATHIC_PULMONARY_HYPERTENSION, -ECLS_CPB, -ECLS_ECMO, -TRANSFUSION_FACT, -TIME_2)

cox_data_sec <- modeling_data2 %>%
  select(-PROTAMINE_Y_1_N_0_, -LUNG_DISEASE, -OR_DATE, -DEATH_DATE, -IDIOPATHIC_PULMONARY_HYPERTENSION, -ECLS_CPB, -ECLS_ECMO, -TRANSFUSION_FACT, -TIME_1)

#### COX PROPORTIONAL HAZARD PRIMARY ANALYSIS ####
# Primary Analysis for cox model
cox_model_prim <- coxph(Surv(TIME_1, DEAD =="1")~ ., data=cox_data_prim)

summary(cox_model_prim)
# PREOPERATIVE_ECLSTRUE, PRE_PT, PRE_INR, INTRA_CELL_SAVER_RETURNED, INTRA_FRESH_FROZEN_PLASMA, INTRA_PACKED_CELLS, ECLSTRUE are significant 

# Secondary Analysis for cox model
cox_model_sec <- coxph(Surv(TIME_2, DEAD == "1")~ ., data=cox_data_sec)

summary(cox_model_sec)
# PRE_PT, PRE_INR, INTRA_PACKED_CELLS are significant 
# SAME PREDICTORS (BUT LESS) THAN IN PRIMARY ANALYSIS

#### COX MODEL ASSUMPTION TEST ####
# Primary Analysis 
cox.zph(cox_model_prim)
# No obvious concerns

# Secondary Analysis 
#cox.zph(cox_model_sec)

#### Classification Modelling for Patient Outcomes ####
# Using classification lasso model 

# Creating dataset for classification model on patient outcomes 
modeling_data3 <- modeling_data2 %>%
  select(-OR_DATE, -DEATH_DATE, PROTAMINE_Y_1_N_0_, -DEAD, -ECLS, -LUNG_DISEASE) %>%
  cbind(ICU_LOS = data$ICU_LOS, 
        HOSPITAL_LOS = data$HOSPITAL_LOS, 
        NEED_REOPERATION_WITHIN_24H = data$NEED_REOPERATION_WITHIN_24H)

modeling_data4 <- imputed_data %>%
  cbind(ICU_LOS = data$ICU_LOS, 
        HOSPITAL_LOS = data$HOSPITAL_LOS, 
        NEED_REOPERATION_WITHIN_24H = data$NEED_REOPERATION_WITHIN_24H,
        TIME = modeling_data2$TIME_1)


ICU_regression_data <- modeling_data3 %>%
  select(-HOSPITAL_LOS, -NEED_REOPERATION_WITHIN_24H, -TIME_1, -PROTAMINE_Y_1_N_0_)

set.seed(123)

#Splitting the data into training and test (80:20 split)
test_index_q2 <- sample(nrow(ICU_regression_data ), round(nrow(ICU_regression_data)/5))

testing_data_q2 <- ICU_regression_data[test_index_q2,]
training_data_q2 <-ICU_regression_data[-test_index_q2,]

#create model matrix for the features, remove intercept
features_q2 <- model.matrix(ICU_LOS ~., ICU_regression_data)[,-1]

#create response vector
response_q2_ICU <- ICU_regression_data$ICU_LOS

ICU_regression_tuning <- cv.glmnet(features_q2, response_q2_ICU, alpha = 1, type.measure = "mse", nfolds = 5)

Short-Term Recovery:
  ICU & Hosiptial stay 

Patient complications:
  Massive transfusion
  Need for reoperation

Long-Term Survival:
ALIVE_30DAYS_YN
ALIVE_90DAYS_YN
ALIVE_12MTHS_YN
Mortality

Potentially look at:
  POSTDAY1_HB, POSTDAY1_CREATININE for renal and blood markers 







death_data$
cox PH 
- graft rejection 
- need for re-op 
- mortality 
- PH assumptions













