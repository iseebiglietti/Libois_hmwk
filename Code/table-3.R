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
library(clubSandwich)
library(stargazer)

#### Code ####
# Import datas
data_table3 <- read_dta("Datas/table3.dta")
data_pradhan <- read_dta("Datas/pradhan-election-table3.dta")



# Define the variables for datas
vars_table3 <- c("highparty1", "highparty12")



## Column of mean for Landlord areas
# Calculate means for our datas where op == 1
means_data3_1 <- data_table3 %>%
  filter(op == 1) %>%
  select(all_of(vars_table3)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Data3 (op = 1)")

means_pradhan_1 <- data_pradhan %>%
  filter(op == 1) %>%
  select(pradhanhighcaste) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Pradhan (op = 1)")

# Combine the results into one data frame
combined_means_1 <- bind_rows(means_data3_1, means_pradhan_1)

# Drop the Group column
combined_means_1 <- combined_means_1 %>%
  select(-Group) %>%
  mutate(Mean = round(Mean, 2))

# View the combined means
print(combined_means_1)



## Column of mean for Non-Landlord areas
# Calculate means for our datas where op == 0
means_data3_0 <- data_table3 %>%
  filter(op == 0) %>%
  select(all_of(vars_table3)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Data3 (op = 0)")

means_pradhan_0 <- data_pradhan %>%
  filter(op == 0) %>%
  select(pradhanhighcaste) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Pradhan (op = 0)")

# Combine the results into one data frame
combined_means_0 <- bind_rows(means_data3_0, means_pradhan_0)

# Drop the Group column
combined_means_0 <- combined_means_0 %>%
  select(-Group) %>%
  mutate(Mean = round(Mean, 2))

# View the combined means
print(combined_means_0)
















## Column for Robust Standard Errors
# Initialize lists to store results
results <- list()

# Fit models for vars_table3
for (var in vars_table3) {
  model <- lm(as.formula(paste(var, "~ op")), data = data_table3)
  robust_result <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  # Store the robust standard error
  results[[var]] <- data.frame(Variable = var,
                                Robust_SE = robust_result[2, 2])
}

# Combine results for vars_table3
results_table3 <- do.call(rbind, results)

# Initialize lists to store results
#results <- list()

# Regression for the fraction of votes cast by high and middle caste parties
  #model_frhm <- lm(frhm ~ op, data = data_table3)
  #robust_result <- coeftest(model_frhm, vcov = vcovCL(model_frhm, cluster = ~district)) 
  # Store the robust standard error
  #results[[var]] <- data.frame(Variable = var,
                                #Robust_SE = robust_result[2, 2])

# Combine results for frhm
#results_frhm <- do.call(rbind, results)

# Initialize lists to store results
results <- list()

# Regression with clustered robust se
  model_pradhan <- lm(pradhanhighcaste ~ op, data = data_pradhan %>% filter(reserv == 0))
  robust_result <- coeftest(model_pradhan, vcov = vcovCL(model_pradhan, cluster = ~district))
  # Store the robust standard error
  results[[var]] <- data.frame(Variable = var,
                                Robust_SE = robust_result[2, 2])

# Combine results for pradhan
results_pradhan <- do.call(rbind, results)

# Combine all results into one data frame
combined_results <- bind_rows(results_table3, results_pradhan, results_frhm)

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
          out = "Output/table3_reproduction.tex")

#Clear
rm()


###### --> LES STANDARD ERRORS NE SONT PAS BONNES... PAS FORCÉMENT LES BONS RESULTATS ! BIZARRE TABLE.
###### --> RESTE À MODIFIER LE NOM DES VARIABLES ET LE NOM DES COLUMNS (ET LES PTITS TRUCS QUI RESTENT SI ON VEUT)