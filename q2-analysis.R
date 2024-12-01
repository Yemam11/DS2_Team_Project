#### DATA FOR KAPLAN MEIER ####

library(lubridate)

# Making duplicate of data for ease of making changes
modeling_data2 <- total_imputed_data %>%
  cbind(IDIOPATHIC_PULMONARY_HYPERTENSION = data$IDIOPATHIC_PULMONARY_HYPERTENSION,
         HYPERTENSION = data$HYPERTENSION,
         MASSIVE_TRANSFUSION = data$MASSIVE_TRANSFUSION,
         RENAL_FAILURE = data$RENAL_FAILURE, 
         DIABETES_INSULIN_ = data$DIABETES_INSULIN_,
         GENDER_MALE_ = data$GENDER_MALE_, 
         INTRA_FRESH_FROZEN_PLASMA = data$INTRA_FRESH_FROZEN_PLASMA,
         INTRA_PACKED_CELLS = data$INTRA_PACKED_CELLS, 
         INTRA_PLATELETS = data$INTRA_PLATELETS, 
         OR_DATE = data$OR_DATE, 
         DEATH_DATE = data$DEATH_DATE,
         ICU_LOS = data$ICU_LOS, 
         HOSPITAL_LOS = data$HOSPITAL_LOS, 
         NEED_REOPERATION_WITHIN_24H = data$NEED_REOPERATION_WITHIN_24H, 
         INTERSTITIAL_LUNG_DISEASE = data$INTERSTITIAL_LUNG_DISEASE)
      
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

# Constructing time variables for analysis 
# initializing time variable for analysis 
modeling_data2$TIME <- NA 

# converting date (currently in date form) into Posix for manipulation
modeling_data2$DEATH_DATE <- as.POSIXct(modeling_data2$DEATH_DATE)

# checking if anyone dead at exactly 1 year 
which((modeling_data2$DEATH_DATE - modeling_data2$OR_DATE) == 365)


# using for loop to calculate time till death for each individual for those who have recorded deaths within 1 year
for (n in 1:nrow(modeling_data2)) {
  
  time <- modeling_data2$DEATH_DATE[n]  - modeling_data2$OR_DATE[n]
    
  if (is.na(modeling_data2$DEATH_DATE[n])) {
    modeling_data2$TIME[n] <- 365
  } else if (365 >= (time)) {
    modeling_data2$TIME[n] <- time
  } else {
    modeling_data2$TIME[n] <- 365
  }
} 

modeling_data2$TIME

# as no one died at 365 days, can correctly assume all those with time = 365 were alive and censored
# Creating event variable for kaplan meier curves, creating indicator for patients w/ observed deaths 
modeling_data2$DEAD <- ifelse(modeling_data2$TIME < 365, 1, 0)


#### TESTING WHICH CAN BE STRATIFIED ####

# Selecting data for patients who died 
death_data <- modeling_data2 %>%
  filter(DEAD == "1") 

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
spprt_curves_prim <- survfit(Surv(TIME, DEAD =="1") ~ ECLS, data = modeling_data2)

# Plotting KM curve stratified by ECLS 
plot(spprt_curves_prim,
     conf.int =0.95,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Kaplan-Meier Curves by ECLS Status")

# Adding legend
legend("bottomleft",                   
       legend = c("No ECLS", "ECLS (ECMO/CPB)"), 
       col = c("blue", "red"),
       lty = 1, 
       title = "ECLS Status")     

# Log-Log test 
plot(survfit(Surv(TIME, DEAD =="1") ~ ECLS, data = modeling_data2), fun = "cloglog", 
     main ="Log-Log Plot for KM Curves Stratified by ECLS", 
     xlab = "Log(Time Since Operation, Days)", 
     ylab())

title(main ="Log-Log Plot for KM Curves Stratified by ECLS")

# Doing Log-Rank test for KM curves by ECLS
survdiff(Surv(TIME, DEAD =="1") ~ ECLS, data = modeling_data2)

# Stratifying by transplant type
type_curves_prim <- survfit(Surv(TIME, DEAD =="1") ~ TYPE, data = modeling_data2)

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

survdiff(Surv(TIME, DEAD =="1") ~ TYPE, data = modeling_data2)

# Stratifying by gender
gndr_curves_prim <- survfit(Surv(TIME, DEAD =="1") ~ GENDER_MALE_, data = modeling_data2)
 
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

survdiff(Surv(TIME, DEAD =="1") ~ TYPE, data = modeling_data2)

# Stratifying by COPD
copd_curves_prim <- survfit(Surv(TIME, DEAD =="1") ~ COPD, data = modeling_data2)

plot(copd_curves_prim,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Kaplan-Meier Curves by COPD Presence")

legend("bottomleft",                    
       legend = c("No COPD", "COPD"), 
       col = c("blue", "red"),                
       lty = 1,                       
       title = "COPD Status")

survdiff(Surv(TIME, DEAD =="1") ~ COPD, data = modeling_data2)

# Stratifying by presence of ANY lung disease
lng_curves_prim <- survfit(Surv(TIME, DEAD =="1") ~ LUNG_DISEASE, data = modeling_data2)

plot(lng_curves_prim,
     col = c("blue", "red"),           
     xlab = "Time From Lung Transplant (Days)",          
     ylab = "Survival Probability", 
     main = "Kaplan-Meier Curves by Lung Disease Presence")

legend("bottomleft",                    
       legend = c("No Lung Disease", "Any Lung Disease (COPD or CF)"), 
       col = c("blue", "red"),                
       lty = 1,                       
       title = "COPD Status")

survdiff(Surv(TIME, DEAD =="1") ~ LUNG_DISEASE, data = modeling_data2)

# Stratifying by transfusion amount\
trans_curves_prim <- survfit(Surv(TIME, DEAD =="1") ~ TRANSFUSION_FACT, data = modeling_data2)

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

survdiff(Surv(TIME, DEAD =="1") ~ TRANSFUSION_FACT, data = modeling_data2)

#### KM LOG-LOG TEST ####

plot(survfit(Surv(time, status==1) ~ sex, data=melanoma), fun = "cloglog")


#### COX PROPORTIONAL HAZARD MODEL DATA ####
library(survival)
library(boot)

# Running base cox-ph model to see if there are any issues 
cox_model <- coxph(Surv(TIME, DEAD =="1")~ ., data=modeling_data2)
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

# Making cox-ph model data for primary & secondary analysis, need to remove variables w/ too high missingness 
cox_data_prim <- modeling_data2 %>%
  select(-LUNG_DISEASE, -OR_DATE, -NEED_REOPERATION_WITHIN_24H,
         -DEATH_DATE, -IDIOPATHIC_PULMONARY_HYPERTENSION, -ECLS_CPB, 
         -ECLS_ECMO, -ICU_LOS, -HOSPITAL_LOS, -TRANSFUSION_FACT, -MASSIVE_TRANSFUSION)

#### COX PROPORTIONAL HAZARD PRIMARY ANALYSIS ####
# Primary Analysis for cox model
cox_model_prim <- coxph(Surv(TIME, DEAD =="1")~ ., data=cox_data_prim)

summary(cox_model_prim)
# TOTAL_24HR_RBC, INTRA_CELL_SAVER_RETURNED_ML_, BMI, TYPE, INTRA_FRESH_FROZEN_PLASMA, INTRA_PACKED_CELLS 

#### COX MODEL ASSUMPTION TEST ####
# Primary Analysis 
cox.zph(cox_model_prim)
# No obvious concerns


# NEW


