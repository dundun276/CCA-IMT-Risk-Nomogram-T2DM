#########################
###### Forest Plot ######
#########################
library(dplyr)      # version 1.1.4
library(broom)      # version 1.0.7
library(forestplot) # version 3.1.6

# Fit the final model
fit_final <- glm(as.formula(paste("CCA_thicken ~", paste(significant_vars, collapse = "+"))), 
                   data = train_data, family = binomial())
summary(fit_final)

# Extract the final model variable table
table_data_final <- tidy(fit_final, conf.int = TRUE) %>%
  mutate(
    Variable = c("Intercept", "Age [years]", "BMI [kg/m^2]", "HDL-C [mmol/L]", "LDL-C [mmol/L]",
                 "LAP", "Smoke=Yes", "Exercise=Yes"),
    Beta = round(estimate,3),
    OR = round(exp(estimate),2),
    lower_CI=round(exp(conf.low), 2),
    upper_CI=round(exp(conf.high), 2),
    OR_CI_95 = paste0(OR, "(", paste0(round(exp(conf.low), 2), "-", round(exp(conf.high), 2)), ")" ),
    p_value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
    ) %>%
  select(
    Variable,
    Beta,
    OR,
    lower_CI,
    upper_CI,
    OR_CI_95,
    p_value
    )

# Add a header to table_data_final
table_data_final[1,3:6] <- NA
new_row <- data.frame(
  Variable = "Variable",
  Beta = "¦Â",
  OR = NA,
  lower_CI = NA,
  upper_CI = NA,
  OR_CI_95 = "OR(95%CI)",
  p_value = "p value",
  stringsAsFactors = FALSE
  )

# Combine the header and table_data_final
table_data_final <- table_data_final %>% mutate(across(c(Beta, OR, lower_CI, upper_CI), as.character))
table_data_final <- bind_rows(new_row, table_data_final)

# Convert data types in table_data_final
table_data_final <- table_data_final %>%
  mutate(
    Beta = ifelse(row_number() == 1, Beta, as.numeric(Beta)),  # First row is the header, do not convert
    OR = ifelse(row_number() == 1, OR, as.numeric(OR)),
    lower_CI = ifelse(row_number() == 1, lower_CI, as.numeric(lower_CI)),
    upper_CI = ifelse(row_number() == 1, upper_CI, as.numeric(upper_CI))
    )

# Draw the forest plot for the final model
forestplot(
  labeltext = as.matrix(table_data_final[, c(1:2, 6:7)]),  # Variable names
  mean = table_data_final$OR,         # Estimates (e.g., OR values)
  lower = table_data_final$lower_CI,  # Lower confidence interval
  upper = table_data_final$upper_CI,  # Upper confidence interval
  is.summary = c(T, F, F, F, F, F, F, F, F),  # Rows to bold
  graph.pos = 3,                      # Graph position, placed in the third column
  xlab = "The estimates",             # X-axis label
  zero = 1,                           # Reference line position (usually 1)
  colgap = unit(3, 'mm'),             # Column spacing
  lineheight = unit(6, 'mm'),         # Row height
  line.margin = 0.08,                 # Row spacing
  col = fpColors(box = '#458B00',     # Box color
                 summary = '#8B008B', # Summary color
                 lines = 'black',     # Confidence interval line color
                 zero = '#7AC5CD'),   # Reference line color
  txt_gp = fpTxtGp(ticks = gpar(cex = 0.8),                   # Tick label font size
                   xlab = gpar(cex = 0.9, fontface = "bold"), # X-axis label font size
                   cex = 0.8),                                # Other text font size
  xticks = c(0, 2, 4, 6, 8, 10),
  
  lwd.xaxis = 2,           # X-axis tick line width
  lwd.zero = 2.5,          # Reference line width
  lwd.ci = 2,              # Confidence interval line width
  lty.ci = "solid",        # Confidence interval line type
  boxsize = 0.2            # Box size
)
grid.text("Multiple logistic regression forest plot", 
          x = unit(0.5, "npc"),                    # Horizontally centered
          y = unit(0.7, "npc"),                    # Adjust y value to lower the title height
          gp = gpar(cex = 1.2, fontface = "bold")) # Title font size and bold
