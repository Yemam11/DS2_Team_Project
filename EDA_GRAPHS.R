
##########EDA########## 
#From data, pior to imputation 
library(ggplot2)
library(dplyr)
library(patchwork)

#Creating histograms for eahc variable, grouping by types

# TRASNFUSION GRAPHS 
#Transfusion Given
#114 given transfusion and 78 without. 
p1 <- ggplot(data = data,
       aes(x = TRANSFUSION_GIVEN)) + geom_bar() + ggtitle("Transfusion Given") + 
  labs( x = "Transfusion Status")+ geom_text(stat = "Count", aes(label = ..count..), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total 24H RBC 
p2 <- ggplot(data, aes(x = TOTAL_24HR_RBC)) +
  geom_histogram(
    binwidth = 5, 
    fill = rgb(0, 0, 1, alpha = 0.35), 
    color = "black" 
  ) +
  ggtitle("Total 24H Red Blood Cells Given") +
  xlab("Total 24H RBC") +
  ylab("Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10)
  )

# Massive transfusion 
p3 <- ggplot(data = data,
             aes(x = MASSIVE_TRANSFUSION)) + geom_bar() + ggtitle("Massive Transfusion") + 
  labs( x = "Massive Transfusion")+ 
  geom_text(stat = "Count", aes(label = ..count..), vjust = -0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  )

# MAking a combined plot for transfusion data 
combined_plot <- (p1+p2+p3) +
  plot_annotation(title = "Distribution of Transfusion Data: Transfusion Given, Total Given, & Massive Transfusion Count",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 10)))

combined_plot

# TRANSPLANT DATA
#Type of transplant 
p4 <- ggplot(data = data,
       aes(x = TYPE)) + geom_bar(fill = "blue") + ggtitle("Lung Transplants, By Type") + 
  labs( x = "Transplant Type")+ geom_text(stat = "Count", aes(label = ..count..), vjust = -0.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 

# PATIIENT DATA
#Age
p5 <- ggplot(data, aes(x = AGE)) +
  geom_histogram(
    binwidth = 5, 
    fill = rgb(0, 0, 1, alpha = 0.35), 
    color = "black" 
  ) +
  ggtitle("Recipient Age") +
  xlab("Recipient Age (Years)") +
  ylab("Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  )

#GENDER_Male 
p6 <- ggplot(data = data, aes(x = GENDER_MALE_)) +
  geom_bar(fill = "pink") +
  ggtitle("Gender (True = Male)") +
  labs(x = "Gender", y = "Count") + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) 

#BMI w/ SE
p7 <- ggplot(data, aes(x = BMI)) +
  geom_histogram(
    binwidth = 1, 
    fill = rgb(0, 0, 1, alpha = 0.35), 
    color = "black" 
  ) +
  ggtitle("Recipient BMI") +
  xlab("Recipient BMI") +
  ylab("Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  )

# Making combined graph for all patient characteristics
combined_plot2 <- (p5 + p6 + p7) +
  plot_annotation(title = "Distribution of Patient Data: Age, Gender, BMI",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

combined_plot2

# ECLS DATA
#Intraoperative ECLS, ECMO and CPB
three_data <- data %>%
  summarize(
    count_intra = sum(!is.na(INTRAOPERATIVE_ECLS) & INTRAOPERATIVE_ECLS == TRUE),
    count_ecmo = sum(!is.na(ECLS_ECMO) & ECLS_ECMO == TRUE),
    count_CPB = sum(!is.na(ECLS_CPB) & ECLS_CPB == TRUE)
  )

# Making long data to count
summary_data_long <- three_data %>%
  pivot_longer(cols = c(count_CPB, count_ecmo, count_intra),
               names_to = "ECLS_Type",
               values_to = "Count")

# Changing label names
summary_data_long[,1] <- c("CPB", "ECMO", "Any ECLS")

ggplot(summary_data_long, aes(x = ECLS_Type, y = Count)) +
  geom_bar(stat = "identity", position = "dodge", fill = "brown") + 
  labs(x = "ECLS Type", y = "Count", title = "Types of ECLS Received") +
  geom_text(aes(label = c("CPB", "ECMO", "ECLS")), vjust = -0.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  )

# CORMORBIDITY DATA 
#Comorbidities :Renal disease, COPD, Cystic fibrosis, IPH, hypertension, and diabetes
comor_data <- data %>%
  summarize(
    COPD = sum(!is.na(COPD) & COPD == TRUE),
    Hypertension = sum(!is.na(HYPERTENSION) & HYPERTENSION == TRUE),
    IPH = sum(!is.na(IDIOPATHIC_PULMONARY_HYPERTENSION) & IDIOPATHIC_PULMONARY_HYPERTENSION == TRUE),
    Cystic_Fibrosis = sum(!is.na(CYSTIC_FIBROSIS) & CYSTIC_FIBROSIS == TRUE),
    Renal_Failure = sum(!is.na(RENAL_FAILURE) & RENAL_FAILURE == TRUE), 
    Diabetes = sum(!is.na(DIABETES_INSULIN_) & DIABETES_INSULIN_== TRUE), 
    Diabetes_Diet = sum(!is.na(DIABETES_DIET_OHGS_) & DIABETES_DIET_OHGS_== TRUE)
    
  )

# Making long data to count
summary_data_long <- comor_data %>%
  pivot_longer(cols = c(COPD, Hypertension,IPH, Cystic_Fibrosis, Renal_Failure, Diabetes, Diabetes_Diet),
               names_to = "Comorbidities",
               values_to = "Count")

# Changing label names
summary_data_long[,1] <- c("COPD", "Hypertension", "IPH", "Cystic Fibrosis", "Renal Failure", 
                           "Diabetes (Insulin)", "Diabetes (Diet/OHG)")

ggplot(summary_data_long, aes(x = Comorbidities, y = Count)) +
  geom_bar(stat = "identity", position = "dodge", fill = "lightgreen") +
  labs(x = "Comorbidities", y = "Count", title = "Count of Recipient Comorbidities") +
  geom_text(aes(label = Count), vjust = -0.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# LAS SCORES 
#LAS Score 
hist(data$LAS_SCORE, col = "plum4", main = "Histogram of Recipient LAS Score", xlab = "LAS Score", breaks = 14)

# PRE-OP BLOOD DATA

# Setting graphical parameters
par(mfrow = c(1,3))

#Pre-HB
hist(data$PRE_HB, col = "plum2", main = "Preoperative Hemoglobin", xlab = "Hemoglobin level", breaks = 14)

#Pre_INR
hist(data$PRE_INR, col = "hotpink4", main = " Pre-op IN Ratio",xlim = c(0,4), xlab = "International Normalization Ratio")

#Pre_Creatinine
hist(data$PRE_CREATININE, col = "hotpink4", main = "Preoperative Creatine Levels",
 xlab = "Preoperative Creatine")

# Resetting graphical parameters
par(mfrow = c(1,1))

# INTA-OP DATA
# Fresh frozen plasma 
p8 <- ggplot(data, aes(x = INTRA_FRESH_FROZEN_PLASMA)) +
  geom_histogram(
    binwidth = 5, 
    fill = rgb(0, 0, 1, alpha = 0.35), 
    color = "black" 
  ) +
  ggtitle("Fresh Frozen Plasma") +
  xlab("Intra Fresh Frozen Plasma") +
  ylab("Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  )

p8

# Intra packed cells 
p9 <- ggplot(data, aes(x = INTRA_PACKED_CELLS)) +
  geom_histogram(
    binwidth = 5, 
    fill = rgb(0, 0, 1, alpha = 0.35), 
    color = "black" 
  ) +
  ggtitle("Packed Cells") +
  xlab("Intra Packed Cells") +
  ylab("Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  )

p9

# Intra packed cells 
p10 <- ggplot(data, aes(x = INTRA_PLATELETS)) +
  geom_histogram(
    binwidth = 3, 
    fill = rgb(0, 0, 1, alpha = 0.35), 
    color = "black" 
  ) +
  ggtitle("Intra Platelets") +
  xlab("Intra Platelet Count") +
  ylab("Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  )

p10

# Intra albumin 
p11 <- ggplot(data, aes(x = INTRA_ALBUMIN_5_ML_)) +
  geom_histogram(
    binwidth = 300, 
    fill = rgb(0, 0, 1, alpha = 0.35), 
    color = "black" 
  ) +
  ggtitle("Intra Albumin") +
  xlab("Intra Albumin, 5mL") +
  ylab("Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  )

p11

# Intra crystalloid 
p12 <- ggplot(data, aes(x = INTRA_CRYSTALLOID_ML_)) +
  geom_histogram(
    binwidth = 300, 
    fill = rgb(0, 0, 1, alpha = 0.35), 
    color = "black" 
  ) +
  ggtitle("Intra Crystalloid") +
  xlab("Intra Crystalloid (mL)") +
  ylab("Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  )

p12

# Intra cryoprecipitate
p13 <- ggplot(data, aes(x = INTRA_CRYOPRECIPITATE)) +
  geom_histogram(
    binwidth = 10, 
    fill = rgb(0, 0, 1, alpha = 0.35), 
    color = "black" 
  ) +
  ggtitle("Intra Cryoprecipitate") +
  xlab("Intra Cryoprecipitate") +
  ylab("Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  )

p13

# Intra cell saver 
p14 <- ggplot(data, aes(x = INTRA_CELL_SAVER_RETURNED_ML_)) +
  geom_histogram(
    binwidth = 500, 
    fill = rgb(0, 0, 1, alpha = 0.35), 
    color = "black" 
  ) +
  ggtitle("Intra Cell Saver Returned") +
  xlab("Intra Cell Saver Returned (mL)") +
  ylab("Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5) # Center the title
  )

p14

# Combining intra plots together
combined_plot3 <- (p8+p9+p10+p11+p12+p13+p14) +
  plot_annotation(title = "Distribution of Intraoperative Blood Compostion Variables",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

combined_plot3 





