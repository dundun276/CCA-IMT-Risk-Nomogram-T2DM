############################
## Model Interpretability ##
###########################3
library(rmda)    # version 1.6
library(xgboost) # version 1.7.8.1
library(shapviz) # version 0.9.7
library(ggplot2) # version 3.5.1

# Decision curve analysis
perform_dca <- function(data, predicted_probs, formula_str, cohort_name) {
  dca_results <- decision_curve(
    as.formula(formula_str),
    data = predicted_probs,
    thresholds = seq(0, 1, by = 0.01)
    )
  par(cex=0.9)
  plot_decision_curve(dca_results, curve.names = cohort_name,
                      col = "#DC0000B2", confidence.intervals = FALSE, 
                      cost.benefit.axis = FALSE, cost.benefits = FALSE,
                      legend.position = "topright")
  par(cex=1.0)
  return(dca_results)
  }

#================#
#== DCA curves ==#
#================#
# Training set DCA curve
predicted_probs_train_all <- data.frame(
  CCA_thicken = selected_train_data$CCA_thicken,
  glm_model = CCA_predicted_train
  )

for (feature in features) {
  fit_single <- glm(as.formula(paste("CCA_thicken ~", feature)), 
                    data = selected_train_data, family = binomial())
  predicted_probs_train_all[[feature]] <- predict(fit_single, newdata = selected_train_data, type = "response")
  single_models[[feature]] <- fit_single
  }

formula_str_train <- paste("CCA_thicken ~ glm_model +", paste(features, collapse = " + "))
dca_results_train <- perform_dca(selected_train_data, predicted_probs_train_all, 
                                 formula_str_train, "Training Cohort")

# Validation set DCA curve
predicted_probs_test_all <- data.frame(
  CCA_thicken = selected_test_data$CCA_thicken,
  glm_model = CCA_predicted_test
  )

for (feature in features) {
  predicted_probs_test_all[[feature]] <- predict(single_models[[feature]], 
                                                 newdata = selected_test_data, type = "response")
  }

formula_str_test <- paste("CCA_thicken ~ glm_model +", paste(features, collapse = " + "))
dca_results_test <- perform_dca(selected_test_data, predicted_probs_test_all, 
                                formula_str_test, "Validation Cohort")

#=================#
#= SHAP Analysis =#
#=================#
# Data preprocessing
x_xgboost <- selected_train_data[, c("Age", "BMI", "HDL", "LDL", "LAP", "Smoke", "Exercise")]
names(x_xgboost)[3] <- "HDL-C"
names(x_xgboost)[4] <- "LDL-C"
x_xgboost[] <- lapply(x_xgboost, function(x) as.numeric(gsub("^\\s+|\\s+$", "", x))) # Remove spaces and convert to numeric
x_xgboost <- as.matrix(x_xgboost)
str(x_xgboost) # Check if it is a numeric matrix

y_xgboost <- selected_train_data$CCA_thicken
str(y_xgboost) # Check if it is numeric

# XGBoost model parameters
params <- list(
  objective = "binary:logistic",
  max_depth = 4,                 # Maximum depth of trees
  eta = 0.1,                     # Learning rate
  nthread = 4,                   # Number of threads
  eval_metric = "logloss",       # Use logloss as the evaluation metric
  subsample = 0.8,               # Subsample ratio
  colsample_bytree = 0.8,        # Feature sampling ratio
  gamma = 0.1,                   # Minimum loss reduction for node splitting
  min_child_weight = 1           # Minimum weight of leaf nodes
  )

# Train XGBoost model
model_xgboost <- xgboost(
  data = x_xgboost, 
  label = y_xgboost, 
  params = params, 
  nrounds = 150  # Number of iterations
  )

# Calculate SHAP values using the XGBoost model
shap_values <- shapviz(model_xgboost, x_xgboost)

# Visualize SHAP values
custom_colors <- c("#1f77b4", "#1f77b4")
sv_force(shap_values, fill_colors = custom_colors)

sv_importance(shap_values, kind = "bar", fill = "#0080FF", show_numbers = TRUE) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 14))

sv_dependence(shap_values,
              v = c("Age", "BMI", "Smoke", "Exercise", "LAP", "LDL-C", "HDL-C")) + theme_bw()

# Loop to plot SHAP interaction plots for each variable
shap_interaction <- shapviz(model_xgboost, x_xgboost, interactions = TRUE)
variables <- c("Age", "BMI", "HDL-C", "LDL-C", "LAP", "Smoke", "Exercise")
for (var in variables) {
  p <- sv_dependence(shap_interaction, v = var, color_var = "auto") +
    theme_bw() +
    ggtitle(paste("SHAP Interaction Plot for", var))
  print(p)
}