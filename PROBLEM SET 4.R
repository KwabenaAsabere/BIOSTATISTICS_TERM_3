library(tidyverse)
framData = read_csv("FraminghamPS4bin.csv")

library(tidyverse)
library(MASS) # for negative binomials
library(ggplot2)
library(survival)
framData = read_csv("FraminghamPS4bin.csv")
# Define a function to calculate summaries and IR for a given variable
calculate_IR_summary <- function(data, grouping_variable) {
  summary <- data %>%
    group_by({{grouping_variable}}) %>%
    summarise(D_sum = sum(D, na.rm = TRUE),
              Y_sum = sum(Y, na.rm = TRUE),
              .groups = 'drop') %>%
    mutate(IR_per_100k = (D_sum / Y_sum) * 100000) # Scale the IR to per 100,000
  return(summary)
}
#
gender_summary <- calculate_IR_summary(framData, gender)
cursmoke_summary <- calculate_IR_summary(framData, cursmoke)
diabetes_summary <- calculate_IR_summary(framData, diabetes)
bpmeds_summary <- calculate_IR_summary(framData, bpmeds)
bmicat_summary <- calculate_IR_summary(framData, bmicat)
agecat_summary <- calculate_IR_summary(framData, agecat)
tbin_summary <- calculate_IR_summary(framData, tbin)
# Print the outputs for verification
print(gender_summary)
print(cursmoke_summary)
print(diabetes_summary)
print(bpmeds_summary)
print(bmicat_summary)
print(agecat_summary)
print(tbin_summary)
# Poisson
model1 = glm(D ~ gender, offset = log(Y), data = framData,
             family=poisson(link="log"))
summary(model1)
AIC(model1)
# Checking fit
# Pearson chi-square goodness-of-fit test (like poisgof in Stata)
X2 = sum(residuals(model5, type = "pearson")^2); X2
df = model5$df.residual; df

pval = round(1 - pchisq(X2, df), 3); pval
# Negative binomial regression
neg_model1 = glm.nb(D ~ gender + offset(log(Y)), data=framData)
summary(neg_model1)
# Models
model1 = glm(D ~ gender, offset = log(Y), data = framData,
             family=poisson(link="log"))
summary(model1)
model5 = glm(D ~ gender + agecat + cursmoke + bmicat + diabetes + bpmeds + tbin,
             offset = log(Y), data = framData,
             family=poisson(link="log"))
summary(model5)
neg_model1 = glm.nb(D ~ gender + offset(log(Y)), data=framData)
summary(neg_model1)
neg_model5 = glm.nb(D ~ gender + agecat + cursmoke + bmicat +
                      diabetes + bpmeds + tbin + offset(log(Y)), data=framData)
summary(neg_model5)
# Sequence of models
model_details <- list(
  model1 = "D ~ gender",
  model2 = "D ~ gender + agecat",
  model3 = "D ~ gender + agecat + cursmoke + bmicat",
  model4 = "D ~ gender + agecat + cursmoke + bmicat + diabetes + bpmeds",
  model5 = "D ~ gender + agecat + cursmoke + bmicat + diabetes + bpmeds + tbin"
)
# Initialize a dataframe to store results
results <- data.frame(Model = character(), AIC = numeric(), X2_df = numeric(), Pval = numeric())
# Loop through each model, fit it, and calculate statistics
for(i in seq_along(model_details)) {
  formula_str <- model_details[[i]]
  model <- glm(as.formula(formula_str), offset = log(Y),
               data = framData, family = poisson(link = "log"))
  # Calculate AIC
  aic_value <- AIC(model)
  # Pearson chi-square goodness-of-fit
  X2 <- sum(residuals(model, type = "pearson")^2)
  df <- model$df.residual
  pval <- round(1 - pchisq(X2, df), 3)
  # Append results
  results <- rbind(results, c(i, aic_value, X2/df, pval))
}
# Rename columns of the results dataframe
names(results) <- c("Model", "AIC", "X2/df", "Pval")
# Print the results table
print(results)
# Initialize a dataframe to store results
neg_results <- data.frame(Model = integer(), AIC = numeric(), Alpha = numeric())
# Correctly specify the offset within the formula
for(i in seq_along(model_details)) {
  formula_str <- paste(model_details[[i]], "+ offset(log(Y))") # Correct placement of the offset
  model <- glm.nb(as.formula(formula_str), data = framData)
  # Calculate AIC
  aic_value <- AIC(model)
  # Calculate alpha (over-dispersion parameter) from theta
  theta <- model$theta
  alpha <- 1 / theta
  # Append results
  neg_results <- rbind(neg_results, data.frame(Model = i, AIC = aic_value, Alpha = alpha))
}
# Output the neg_results
print(neg_results)
# Summarize data by time bins (tbin)
time_bin_summary <- framData %>%
  group_by(tbin, agecat) %>%
  summarise(
    Deaths = sum(D, na.rm = TRUE),
    Person_Days = sum(Y, na.rm = TRUE),
    Death_Rate_Per_Person_Day = sum(D, na.rm = TRUE) / sum(Y, na.rm = TRUE),
    .groups = 'drop'
  )
# View the summarized table
print(time_bin_summary)
# Step 1: Duplicate rows where tbin is 0 and update Probability to 1 for duplicates
duplicated_rows <- time_bin_summary %>%
  filter(tbin == 0) %>%
  mutate(Probability = 1)
# Bind the duplicated rows with original data
adjusted_time_bin_summary <- bind_rows(duplicated_rows, time_bin_summary)
# Step 2: Update Midtbin based on the increment rule
adjusted_time_bin_summary <- adjusted_time_bin_summary %>%
  arrange(agecat, tbin) %>%
  group_by(agecat) %>%
  mutate(Midtbin = if_else((((row_number() - 1) * 5) - 2.5) < 0, 0, (((row_number() - 1) * 5) - 2.5))) %>%
  ungroup()
# Print the adjusted table to verify changes
print(adjusted_time_bin_summary)
# Ensure 'agecat' is a factor for coloring
adjusted_time_bin_summary$agecat <- factor(adjusted_time_bin_summary$agecat)
# Plotting
ggplot(adjusted_time_bin_summary, aes(x = Midtbin, y = Survival, group = agecat, color = agecat)) +
  geom_line() + # Draw lines
  labs(
    x = "Midpoint of Time Bin (Years)",
    y = "Survival Probability",
    color = "Age Category"
  ) +
  theme_minimal() +
  scale_color_discrete(name = "Age Category") # Customize color legend title
