######################################
## Model Development and Validation ##
######################################
library(rms)              # version 6.8-0
library(ResourceSelection)# version 0.3-6
library(data.table)       # version 1.16.4
library(pROC)             # version 1.18.5

# Data preprocessing function
preprocess_data <- function(data) {
  selected_data <- data[, c("Age","BMI","HDL","LDL","LAP","Smoke","Exercise","CCA_thicken")]
  selected_data$CCA_thicken <- as.numeric(selected_data$CCA_thicken) - 1
  return(selected_data)
  }

# Model fitting and nomogram generation
fit_and_plot_nomogram <- function(data) {
  dd <- datadist(data)
  options(datadist = "dd")
  
  label(data$HDL) <- "HDL-C"
  label(data$LDL) <- "LDL-C"
  
  fit_model <- lrm(CCA_thicken ~ Age + BMI + HDL + LDL + LAP + Smoke + Exercise, 
                   data = data, x = TRUE, y = TRUE)
  nomogram_model <- nomogram(fit_model, fun = plogis, funlabel = "Probability of Event")
  
  par(mar = c(5, 4, 4, 2) + 0.1)
  plot(nomogram_model, xfrac = 0.15, cex.axis = 1, cex.var = 1.1)
  
  return(fit_model)
  }

# Calibration plot and Hosmer-Lemeshow test
plot_calibration <- function(predicted, actual) {
  cal <- val.prob(predicted, actual, logistic.cal = TRUE, xlab = "Predicted Probability", 
                  ylab = "Actual Probability", statloc = FALSE, riskdist = c("predicted", "calibrated"), 
                  cex = 1, mkh = 0.02, connect.group = FALSE, connect.smooth = TRUE, 
                  g.group = 4, evaluate = 100, nmin = 0, legendloc = c(0.6, 0.4))
  
  hl_test <- hoslem.test(actual, predicted, g = 10)
  
  text(x = 0.03, y = 0.9, labels = paste("\nHosmer-Lemeshow p-value:", round(hl_test$p.value, 3), 
                                         "\nBrier Score:", round(cal["Brier"], 3), 
                                         "\nIntercept:", round(cal["Intercept"], 3), 
                                         "\nSlope:", round(cal["Slope"], 3)), adj = 0, cex = 0.9)
  
  return(cal)
  }

# 1000 bootstrap calibrations
bootstrap_calibration <- function(data, title) {
  fit_bootstrap <- lrm(CCA_thicken ~ Age + BMI + HDL + LDL + LAP + Smoke + Exercise, 
                       data = data, x = TRUE, y = TRUE)
  bootstrap_cal <- calibrate(fit_bootstrap, method = "boot", B = 1000, data = data, response = "CCA_thicken")
  
  plot(bootstrap_cal, xlim = c(0, 1), ylim = c(0, 1), xlab = "Predicted Probability", 
       ylab = "Observed Probability", cex.lab = 1.2, cex.axis = 1, cex.main = 1.2, cex.sub = 0.8, 
       legend = FALSE, main = title)
  
  lines(bootstrap_cal[, c("predy", "calibrated.corrected")], type = 'l', lwd = 3, pch = 16, col = "#90EE90")
  lines(bootstrap_cal[, c("predy", "calibrated.orig")], type = "l", pch = 16, lwd = 3, col = "#F88379")
  abline(0, 1, lty = 2, lwd = 2, col = "black")
  
  legend(0.6, 0.35, c("Apparent", "Bias-corrected", "Ideal"), 
         lty = c(1, 1, 2), lwd = c(3, 3, 2), col = c("#F88379", "#90EE90", "black"), bty = "n")
  
  return(bootstrap_cal)
  }

# Plot ROC curves
plot_roc_curves <- function(roc_curves, auc_values, title = "", features, colors, line_types) {
  plot(roc_curves[["model"]], col = "#1f77b4", lwd = 2, 
       main = title, xlab = "Specificity", ylab = "Sensitivity", 
       xlim = c(1, 0), ylim = c(0, 1), las = 1, xaxs = "i", yaxs = "i", 
       cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
  abline(h = seq(0.2, 0.8, by = 0.2), lty = 2, col = "gray")
  abline(v = seq(0.2, 0.8, by = 0.2), lty = 2, col = "gray")
  
  for (i in seq_along(features)) {
    lines(roc_curves[[features[i]]], col = colors[i], lwd = 2, lty = line_types[i])
  }
  
  names(auc_values)[4]="HDL-C"
  names(auc_values)[8]="LDL-C"
  legend("bottomright", 
         legend = c(paste("Model AUC:", round(auc_values["model"], 4)), 
                    paste(names(auc_values)[-1], "AUC:", round(auc_values[-1], 4))),
         col = c("#1f77b4", colors), lwd = 2, lty = c(1, line_types), cex = 0.5, bty = "n")
  }

#==============================#
#== Training Set Calibration ==#
#==============================#
selected_train_data <- preprocess_data(train_data)
fit_nomo <- fit_and_plot_nomogram(selected_train_data)
CCA_predicted_train <- predict(fit_nomo, data = selected_train_data, type = "fitted")
CCA_cal_train <- plot_calibration(CCA_predicted_train, selected_train_data$CCA_thicken)
CCA_bootstrap1000_train <- bootstrap_calibration(selected_train_data, "Training Set Bootstrap Calibration")

#==============================#
#= Validation Set Calibration =#
#==============================#
selected_test_data <- preprocess_data(test_data)
setDT(selected_test_data)
CCA_predicted_test <- predict(fit_nomo, newdata = selected_test_data, type = "fitted")
CCA_cal_test <- plot_calibration(CCA_predicted_test, selected_test_data$CCA_thicken)
CCA_bootstrap1000_test <- bootstrap_calibration(selected_test_data, "Validation Set Bootstrap Calibration")

#===================#
#==== ROC curves ===#
#===================#
# Set variables, colors, and line types
features <- c("Age","Smoke","HDL","Exercise","BMI","LAP","LDL")
colors <- c("#377eb8", "#f781bf", "#e377c2","#4daf4a" , "#2ca02c","#984ea3", "#9467bd")
line_types <- c(2, 3, 2, 3, 2, 3, 2, 3)

# Training set ROC curves
single_models <- list()
roc_curve_train_model <- roc(selected_train_data$CCA_thicken, CCA_predicted_train)
auc_value_train_model <- auc(roc_curve_train_model)
roc_curves_train <- list(model = roc_curve_train_model)
auc_values_train <- c(model = auc_value_train_model)

for (feature in features) {
  fit_single <- glm(as.formula(paste("CCA_thicken ~", feature)), 
                    data = selected_train_data, family = binomial())
  single_models[[feature]]  <- fit_single
  predicted_probs_single <- predict(fit_single, newdata = selected_train_data, type = "response")
  roc_curve_single <- roc(selected_train_data$CCA_thicken, predicted_probs_single)
  roc_curves_train[[feature]] <- roc_curve_single
  auc_values_train[feature] <- auc(roc_curve_single)
  }

plot_roc_curves(roc_curves_train, auc_values_train, "ROC Curves of Training Cohort",
                features, colors, line_types)

# Validation set ROC curves
roc_curve_test_model <- roc(selected_test_data$CCA_thicken, CCA_predicted_test)
auc_value_test_model <- auc(roc_curve_test_model)
roc_curves_test <- list(model = roc_curve_test_model)
auc_values_test <- c(model = auc_value_test_model)

for (feature in features) {
  predicted_probs_single <- predict(single_models[[feature]], 
                                    newdata = selected_test_data, type = "response")
  roc_curve_single <- roc(selected_test_data$CCA_thicken, predicted_probs_single)
  roc_curves_test[[feature]] <- roc_curve_single
  auc_values_test[feature] <- auc(roc_curve_single)
  }

plot_roc_curves(roc_curves_test, auc_values_test, "ROC Curves of Validation Cohort",
                features, colors, line_types)