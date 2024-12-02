

par(mfrow=c(1,1))

ICU_reg_data <- modeling_data2 %>%
  select(-HOSPITAL_LOS, NEED_REOPERATION_WITHIN_24H, -ECLS, -LUNG_DISEASE, -TRANSFUSION_FACT,
         -OR_DATE, -DEATH_DATE, -DEAD, -TIME)

ICU_reg_data$TYPE <- total_imputed_data$TYPE

#Splitting the data into training and test (80:20 split)
test_index_icu <- sample(nrow(ICU_reg_data), round(nrow(ICU_reg_data)/5))

testing_data_icu <- ICU_reg_data[test_index_icu,]
training_data_icu <-ICU_reg_data[-test_index_icu,]


#Create data frame to hold results
results_regression_icu <- data.frame(trial = 1:10, lasso_MSE = 1:10)

coefficients_regression_icu <- data.frame(feature = c("Intercept", names(ICU_reg_data)[c(-1, -ncol(ICU_reg_data))], "TypeLeft", "TypeRight"))


#repeatedly train with different data to assess the impact of sampling bias
for(i in 1:10){
  set.seed(sample(1:1000, 1))
  
  #Splitting the data into training and test (80:20 split)
  test_index_icu <- sample(nrow(ICU_reg_data), round(nrow(ICU_reg_data)/5))
  
  testing_data_icu <- ICU_reg_data[test_index_icu,]
  training_data_icu <-ICU_reg_data[-test_index_icu,]
  
  #create model matrix for the features, remove intercept
  features_icu <- model.matrix(ICU_LOS ~., training_data_icu)[,-1]
  
  #create response vector
  response_icu <- training_data_icu$ICU_LOS
  
  #identify the lambda which minimizes AUC using 5 fold cross validation
  regression_tuning_icu <- cv.glmnet(features_icu, response_icu, alpha = 1, type.measure = "mse", nfolds = 5)
  
  #extract min lambda
  min_lambda_regression_icu <- regression_tuning_icu$lambda.min
  
  #extract the coefficients for the min lambda
  coef.glmnet(regression_tuning_icu, s = min_lambda_regression_icu)
  
  #create a model for predictions
  regression_model_icu <- glmnet(features_icu, response_icu, lambda = min_lambda_regression_icu)
  
  #create new data for predictions using the testing  set, remove intercept
  newdata_icu <- model.matrix(ICU_LOS~., testing_data_icu)[,-1]
  
  #Make predictions using the trained model
  predictions_icu <- as.numeric(predict(regression_model_icu, newx = newdata_icu, s = min_lambda_regression_icu, type = "response"))
  
  #save coefficients
  coefficients_regression_icu <- cbind(coefficients_regression_icu, as.vector(as.matrix(coef(regression_model_icu))))
  names(coefficients_regression_icu)[length(coefficients_regression_icu)] <- i
  
  #calculate MSE
  MSE <- mean((testing_data_icu$ICU_LOS - predictions_icu)^2)
  results_regression[i, "lasso_MSE"] <- MSE
}

#find out how many times each coefficient is non-zero
resilient_coefficients_regression_icu <- coefficients_regression_icu %>% 
  mutate(nonZero = rowSums(across(-feature, ~ . != 0)))

#Graph findings
ggplot(resilient_coefficients_regression_icu, mapping = aes(y = feature, x = nonZero))+
  geom_col()

#Create a final model to extract coefficients
testing_data_icu <- ICU_reg_data
features_icu <- model.matrix(ICU_LOS ~., training_data_icu)[,-1]
response_icu <- training_data_icu$ICU_LOS

#identify the lambda which minimizes AUC using 5 fold cross validation
regression_tuning_icu <- cv.glmnet(features_icu, response_icu, alpha = 1, type.measure = "mse", nfolds = 5)

#extract min lambda
min_lambda_regression_icu <- regression_tuning_icu$lambda.min

#refit model
regression_model_icu <- glmnet(features_icu, response_icu, lambda = min_lambda_regression_icu)

#extract the coefficients for the min lambda
coef.glmnet(regression_model_icu, s = min_lambda_regression)


# Creating coefficient plot
regression_model_icu <- glmnet(features_icu, response_icu, alpha = 1)
plot(regression_model_icu, xvar = "lambda", lwd = 2, col = colors)
abline(h = 0, col = "black", lty = 2, lwd = 2)
title(main = "Lasso Regression Regularization Plot",
      line = 3)

#Create a legend
legend("topright",
       legend = coef_names,
       col = colors,
       cex = 0.5,
       lwd = 2)


