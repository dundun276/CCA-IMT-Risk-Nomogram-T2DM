#########################
##### Split Dataset #####
#########################
library(glmnet) # version 4.1-8
library(rms)    # version 6.8-0
library(dplyr)  # version 1.1.4
library(caret)  # version 7.0-1

# Exclude ID and Name columns from the analysis data
analysis_data <- complete_data[, 2:50]
View(analysis_data)

# Split the dataset
set.seed(12345)
index_0.7 <- createDataPartition(analysis_data$CCA_thicken, p = 0.7, list = FALSE)
train_data <- analysis_data[ index_0.7, ]     # 564 samples
test_data  <- analysis_data[-index_0.7, ]     # 240 samples
table(train_data$CCA_thicken)
table(test_data$CCA_thicken)

# Save training and testing datasets
write.csv(train_data, "CCA_train_data.csv", row.names = FALSE)
write.csv(test_data, "CCA_test_data.csv", row.names = FALSE)



########################
### LASSO Regression ###
########################
# Filter numeric and categorical variables
numeric_vars <- train_data %>% select_if(is.numeric)
categorical_vars <- train_data %>% select_if(is.factor)

# Standardize numeric variables
preProcValues <- preProcess(numeric_vars, method = c("center", "scale", "YeoJohnson", "nzv"))
normalized_numeric_vars <- predict(preProcValues, numeric_vars)

# Combine standardized numeric variables with original categorical variables
normalize_analysis_data <- bind_cols(normalized_numeric_vars, categorical_vars)
summary(normalize_analysis_data)

# Prepare independent and dependent variables
x_data <- normalize_analysis_data[, -49]  # Independent variables
y_data <- normalize_analysis_data[, 49]   # Dependent variable

# LASSO path plot
fit_lasso <- glmnet(as.matrix(x_data), y_data, alpha = 1, family = "binomial")
plot(fit_lasso, xvar = "lambda", label = FALSE)
print(fit_lasso)

# Convert categorical variables to dummy variables
x_numeric <- model.matrix(~ . - 1, data = x_data)  # Convert to numeric matrix
y_numeric <- as.numeric(y_data)  # Convert dependent variable to numeric

# Cross-validation plot
fit_cv <- cv.glmnet(x_numeric, y_numeric, alpha = 1, family = "binomial")
plot(fit_cv)
abline(v = log(fit_cv$lambda.min), lty = 2, col = "blue")  # Minimum lambda
abline(v = log(fit_cv$lambda.1se), lty = 2, col = "red")   # 1 standard error lambda

# Add legend to the cross-validation plot
legend("topleft", 
       legend = c("lambda.min", "lambda.1se"), 
       col = c("blue", "red"), 
       lty = 2, 
       bty = "n")

# Extract coefficients corresponding to the minimum lambda
Coefficients <- coef(fit_lasso, s = fit_cv$lambda.min)

# Output results
Active.Index <- which(Coefficients != 0)
cat("Optimal lambda value:", fit_cv$lambda.min, "\n")
row.names(Coefficients)[Active.Index]
# [1] "(Intercept)"                "Age"                        "Disease Duration"          
# [4] "BMI"                        "CA19-9"                     "HbA1c"                     
# [7] "HDL"                        "LDL"                        "GFR"                       
# [10] "UMA"                        "MON ratio"                  "PLT"                       
# [13] "LY"                         "LAP"                        "TyG index"                 
# [16] "Hypertension"               "Diabetic Nephropathy"       "Smoke"                     
# [19] "Drink"                      "Exercise"                   "Family History of Diabetes"
# [22] "Central Obesity"            "Poor Lipid Control"  



###########################################
## Stepwise Backward Logistic Regression ##
###########################################
library(dplyr)      # version 1.1.4
library(broom)      # version 1.0.7
library(forestplot) # version 3.1.6
library(MASS)       # version 7.3-58.2

# Fit stepwise backward logistic regression
formula_CCA <- CCA_thicken ~ Age + `Disease Duration` + BMI + `CA19-9` + HbA1c + HDL + LDL + GFR + UMA +
  `MON ratio` +  PLT + LY + LAP +  `Diabetic Nephropathy` + Smoke + Drink + Exercise +
  `Family History of Diabetes` +  `Central Obesity` + `Poor Lipid Control` 
fit_CCA <- glm(formula_CCA, data = train_data, family = binomial())
backward_model <- stepAIC(fit_CCA, direction = "backward")

# Extract results from stepwise backward logistic regression
results_CCA <- tidy(backward_model, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR = round(exp(estimate), 3),
    CI_95_Lower = round(exp(conf.low), 3),
    CI_95_Upper = round(exp(conf.high), 3),
    p_value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
  # ) %>%
  # select(
  #   Predictor = term,
  #   Beta = estimate,
  #   OR,
  #   CI_95_Lower,
  #   CI_95_Upper,
  #   p_value
  # )

# Save results
print(results_CCA)
write.csv(results_CCA, "backward_logistic_regression_results_CCA.csv", row.names = FALSE)

# Retain variables with p < 0.05
p_values <- summary(backward_model)$coefficients[, 4]
significant_vars <- names(p_values[p_values < 0.05 & names(p_values) != "(Intercept)"])
significant_vars <- unique(gsub("\\d+", "", significant_vars))
print(significant_vars)
# [1] "Age"      "BMI"      "HDL"      "LDL"      "LAP"      "Smoke"    "Exercise"