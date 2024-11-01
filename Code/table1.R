#### Load packages ####
library(tidyverse)
library(stringi)
library(stringr)
library(ggplot2)
library(ggthemes)
library(haven)
library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)

#### Code ####
# Import datas
data1 <- read_dta("Datas/table1-soil-climate.dta")
data2 <- read_dta("Datas/table1-population-and-census.dta")

# Define the variables for data1
vars1 <- c("lon", "lat", "alt", "rain", "soilblack", "soilalkaline", 
                      "soilalluvialriver", "soilred", "aquifermorethan150", 
                      "aquifer100and150", "phstrongalkali", "phslightalkali", 
                      "phneutral", "phslightacid", "topsoil25and50cm", 
                      "topsoil50and100cm", "topsoilmorethan300cm")

# Define the variables for data2
vars2 <- c("poverty", "literacyrate", "populationsize", "populationdensity", 
                      "fractionsc", "permanenthouse", "pervilelec", 
                      "pervilroad", "pervilhealth", "pervilschool", "ger")



## Column of mean for Landlord areas
# Calculate means for data1 and data2 where op == 1
means_data1_1 <- data1 %>%
  filter(op == 1) %>%
  select(all_of(vars1)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Data1 (op = 1)")

means_data2_1 <- data2 %>%
  filter(op == 1) %>%
  select(all_of(vars2)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Data2 (op = 1)")

# Combine the results into one data frame
combined_means_1 <- bind_rows(means_data1_1, means_data2_1)

# Drop the Group column
combined_means_1 <- combined_means_1 %>%
  select(-Group) %>%
  mutate(Mean = round(Mean, 2))

# View the combined means
print(combined_means_1)



## Column of mean for Non-Landlord areas
# Calculate means for data1 and data2 where op == 0
means_data1_0 <- data1 %>%
  filter(op == 0) %>%
  select(all_of(vars1)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Data1 (op = 0)")

means_data2_0 <- data2 %>%
  filter(op == 0) %>%
  select(all_of(vars2)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Data2 (op = 0)")

# Combine the results into one data frame
combined_means_0 <- bind_rows(means_data1_0, means_data2_0)

# Drop the Group column
combined_means_0 <- combined_means_0 %>%
  select(-Group) %>%
  mutate(Mean = round(Mean, 2))

# View the combined means
print(combined_means_0)



## Column for Robust Standard Errors
# Initialize lists to store results
results <- list()

# Fit models for vars1
for (var in vars1) {
  model <- lm(as.formula(paste(var, "~ op")), data = data1)
  robust_result <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  # Store the robust standard error
  results[[var]] <- data.frame(Variable = var,
                                Robust_SE = robust_result[2, 2])
}

# Combine results for vars1
results_data1 <- do.call(rbind, results)

# Initialize lists to store results for vars2
results <- list()

# Fit models for vars2
for (var in vars2) {
  model <- lm(as.formula(paste(var, "~ op")), data = data2)
  robust_result <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  # Store the robust standard error
  results[[var]] <- data.frame(Variable = var,
                                Robust_SE = robust_result[2, 2])
}

# Combine results for vars2
results_data2 <- do.call(rbind, results)

# Combine all results into one data frame
combined_results <- bind_rows(results_data1, results_data2)

# Drop the Coefficient column
combined_results <- combined_results %>%
  mutate(Robust_SE = round(Robust_SE, 2))

# Display the combined results
print(combined_results)




# Merge the data frames
final_table <- combined_means_1 %>%
  full_join(combined_means_0, by = "Variable") %>%
  full_join(combined_results, by = "Variable")

# View the final table
print(final_table)



## Compute the full table
# Creating the stargazer output
stargazer(final_table, type = "text", title = "Combined Means and Robust Standard Errors",
          summary = FALSE, 
          out = "Output/table1_reproduction.tex")



# Clear
rm()



###### --> RESTE À MODIFIER LE NOM DES VARIABLES ET LE NOM DES COLUMNS (ET LES PTITS TRUCS QUI RESTENT SI ON VEUT)