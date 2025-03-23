#######################
### Data Imputation ###
#######################
library(dplyr)  # version 1.1.4
library(VIM)    # version 6.2.2
library(mice)   # version 3.17.0

# Load data
original_data <- data[,c(1:50,55)]

# Define columns to be factorized
factor_cols_binary <- c("Hypertension", "Diabetic Nephropathy", "Smoke", "Drink", "Exercise", 
                        "Family History of Diabetes", "Central Obesity", 
                        "Poor Glycemic Control", "Poor Lipid Control", "CCA_thicken")
factor_cols_ternary <- "Regular Medication"
factor_cols_gender <- "Gender"
factor_cols_bmi <- "BMI Classification"

# Data transformation and factorization
original_data <- original_data %>%
  mutate(
    across(1, as.character),   # Convert column 1 to character type
    across(3:51, as.numeric),  # Convert columns 3 to 51 to numeric type
    across(all_of(factor_cols_binary), ~ factor(., levels = c(0, 1))),
    across(all_of(factor_cols_ternary), ~ factor(., levels = c(0, 1, 2))),
    across(all_of(factor_cols_gender), ~ factor(., levels = c(1, 2))),
    across(all_of(factor_cols_bmi), ~ factor(., levels = c(1, 2, 3, 4)))
    )

# Check data structure
str(original_data)

# Visualize the percentage of missing values
aggr(original_data, prop = TRUE, numbers = TRUE)

# Calculate the missing value rate for each column
na_rates <- original_data %>%
  summarise(across(everything(), ~ mean(is.na(.))))
print(na_rates*100)

# Perform multiple imputation using mice
method <- c(
  ID = "",                     # "ID"
  Name = "",                   # "Name"
  Gender = "",                 # "Gender"
  Age = "",                    # "Age"
  Hypertension = "logreg",     # "Hypertension"
  Disease_Duration = "",       # "Disease Duration"
  Regular_Medication = "polr", # "Regular Medication"
  Diabetic_Nephropathy = "",   # "Diabetic Nephropathy"
  WHR = "",                    # "Waist-to-Hip Ratio"
  WC = "",                     # "Waist circumference"
  HC = "",                     # "Hip circumference"
  BMI = "pmm",                 # "BMI"
  Smoke = "logreg",            # "Smoke"
  Drink = "logreg",            # "Drink"
  Exercise = "logreg",         # "Exercise"
  Family_History_Diabetes = "logreg",  # "Family History of Diabetes"
  CEA = "pmm",                 # "CEA"
  CA19_9 = "pmm",              # "CA19-9"
  FCP = "pmm",                 # "FCP"
  FCP_2H = "pmm",              # "2H FCP"
  FBG = "pmm",                 # "FBG"
  HbA1c = "pmm",               # "HbA1c"
  PBG_2H = "pmm",              # "2H PBG"
  TC = "pmm",                  # "TC"
  TG = "pmm",                  # "TG"
  HDL = "pmm",                 # "HDL"
  LDL = "pmm",                 # "LDL"
  Ca = "pmm",                  # "Ca"
  GER = "pmm",                 # "GER"
  SUA = "pmm",                 # "SUA"
  UMA = "pmm",                 # "UMA"
  UCr = "pmm",                 # "UCr"
  UACR = "pmm",                # "UACR"
  WBC = "pmm",                 # "WBC"
  MON = "pmm",                 # "MON"
  MON_Ratio = "pmm",           # "MON ratio"
  NEU = "pmm",                 # "NEU"
  PLT = "pmm",                 # "PLT"
  LY = "",                     # "LY"
  NLR = "pmm",                 # "NLR"
  PLR = "pmm",                 # "PLR"
  Central_Obesity = "",        # "Central Obesity"
  BMI_Classification = "",     # "BMI Classification"
  Poor_Glycemic_Control = "",  # "Poor Glycemic Control"
  Poor_Lipid_Control = "",     # "Poor Lipid Control"
  LAP = "",                    # "LAP"
  NonHDL = "",                 # "NonHDL"
  NHHR = "",                   # "NHHR"
  TyG_Index = "",              # "TyG index"
  AIP = "",                    # "AIP"
  CCA_thicken = ""             # "CCA_thicken"
  )

imp <- mice(original_data, 
            method = method,   # Imputation methods
            m = 5,             # Generate 5 imputed datasets
            maxit = 10,        # Maximum number of iterations
            print = TRUE,      # Print imputation process
            seed = 123,        # Random seed
            trace = FALSE)     # Do not show progress bar

# Fit a model on the imputed data
fit <- with(imp, glm(CCA_thicken ~ ., data = original_data, family = binomial()))

# Summarize results
pooled <- pool(fit)
summary(pooled)

# Extract the third imputed dataset
result_mice <- complete(imp, action = 3)

# Check missing values and their locations
colSums(is.na(result_mice))
sapply(result_mice, function(x) which(is.na(x)))

# Extract complete data
complete_rows <- complete.cases(result_mice)
complete_data <- result_mice[complete_rows, ]
complete_data <- complete_data[, -2]
colSums(is.na(complete_data))
View(complete_data)
write.csv(complete_data, "CCA_mice_complete_data.csv", row.names = FALSE)