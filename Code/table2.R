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
data_teachers <- read_dta("Datas/teachercharacterisitcs-table2.dta")
data_villages <- read_dta("Datas/villagecharacterisitcs-table2.dta")
data_students <- read_dta("Datas/studentcharacterisitcs-table2.dta")



# Define the variables for datas
vars_teachers <- c("tcaste3", "tcaste2", "tcaste", "edu", "dissch", "tgen")
vars_villages <- c("elec", "phone", "gpdistroad", "gpsl", "density", "lit", "frsc", "frobc")
vars_students <- c("studentobccaste", "studentsccaste", "studenthighcaste", 
                   "fatherprimaryedu", "motherprimaryedu", "studentgender")



## Column of mean for Landlord areas
# Calculate means for our 3 datas where op == 1
means_teachers_1 <- data_teachers %>%
  filter(op == 1) %>%
  select(all_of(vars_teachers)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Teachers (op = 1)")

means_villages_1 <- data_villages %>%
  filter(op == 1) %>%
  select(all_of(vars_villages)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Villages (op = 1)")

means_students_1 <- data_students %>%
  filter(op == 1) %>%
  select(all_of(vars_students)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Students (op = 1)")

# Combine the results into one data frame
combined_means_1 <- bind_rows(means_teachers_1, means_villages_1, means_students_1)

# Drop the Group column
combined_means_1 <- combined_means_1 %>%
  select(-Group) %>%
  mutate(Mean = round(Mean, 2))

# View the combined means
print(combined_means_1)



## Column of mean for Non-Landlord areas
# Calculate means for our 3 datas where op == 0
means_teachers_0 <- data_teachers %>%
  filter(op == 0) %>%
  select(all_of(vars_teachers)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Teachers (op = 0)")

means_villages_0 <- data_villages %>%
  filter(op == 0) %>%
  select(all_of(vars_villages)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Villages (op = 0)")

means_students_0 <- data_students %>%
  filter(op == 0) %>%
  select(all_of(vars_students)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Mean") %>%
  mutate(Group = "Students (op = 0)")

# Combine the results into one data frame
combined_means_0 <- bind_rows(means_teachers_0, means_villages_0, means_students_0)

# Drop the Group column
combined_means_0 <- combined_means_0 %>%
  select(-Group) %>%
  mutate(Mean = round(Mean, 2))

# View the combined means
print(combined_means_0)



## Column for Robust Standard Errors
# Initialize lists to store results
results <- list()

# Fit models for vars_teachers
for (var in vars_teachers) {
  model <- lm(as.formula(paste(var, "~ op")), data = data_teachers)
  robust_result <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  # Store the robust standard error
  results[[var]] <- data.frame(Variable = var,
                                Robust_SE = robust_result[2, 2])
}

# Combine results for vars_teachers
results_teachers <- do.call(rbind, results)

# Initialize lists to store results
results <- list()

# Fit models for vars_villages
for (var in vars_villages) {
  model <- lm(as.formula(paste(var, "~ op")), data = data_villages)
  robust_result <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  # Store the robust standard error
  results[[var]] <- data.frame(Variable = var,
                                Robust_SE = robust_result[2, 2])
}

# Combine results for vars_villages
results_villages <- do.call(rbind, results)

# Initialize lists to store results
results <- list()

# Fit models for vars_students
for (var in vars_students) {
  model <- lm(as.formula(paste(var, "~ op")), data = data_students)
  robust_result <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  # Store the robust standard error
  results[[var]] <- data.frame(Variable = var,
                                Robust_SE = robust_result[2, 2])
}

# Combine results for vars_students
results_students <- do.call(rbind, results)

# Combine all results into one data frame
combined_results <- bind_rows(results_teachers, results_villages, results_students)

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
          out = "Output/table2_reproduction.tex")

#Clear
rm()


###### --> LES STANDARD ERRORS NE SONT PAS BONNES... 
###### --> RESTE À MODIFIER LE NOM DES VARIABLES ET LE NOM DES COLUMNS (ET LES PTITS TRUCS QUI RESTENT SI ON VEUT)