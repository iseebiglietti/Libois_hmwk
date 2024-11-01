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
data_table4 <- read_dta("Datas/table4.dta")
data_healthcommittee <- read_dta("Datas/table4-health-committee.dta")




# Define all regression models for each outcome with appropriate controls
# Models for Village Governance Metrics (Village Meetings, VEC, PTA)

# Model 1: Village Meetings
model_village1 <- lm(gsmeet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain, data = data_table4)
model_village2 <- lm(gsmeet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density, data = data_table4)
model_village3 <- lm(gsmeet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density + elec + phone + gpdistro + lit, data = data_table4)

# Model 2: VEC Meetings
model_vec1 <- lm(ssmeet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain, data = data_table4)
model_vec2 <- lm(ssmeet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density, data = data_table4)
model_vec3 <- lm(ssmeet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density + elec + phone + gpdistro + lit, data = data_table4)

# Model 3: PTA Meetings
model_pta1 <- lm(ptameet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain, data = data_table4)
model_pta2 <- lm(ptameet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density, data = data_table4)
model_pta3 <- lm(ptameet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density + elec + phone + gpdistro + lit, data = data_table4)

# Models for Health Committee Metrics (using data_healthcommittee dataset)
model_health1 <- lm(healthcommittee ~ oudh + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain, data = data_healthcommittee)
model_health2 <- lm(healthcommittee ~ oudh + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density, data = data_healthcommittee)
model_health3 <- lm(healthcommittee ~ oudh + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density + elec + phone + gpdistro + lit, data = data_healthcommittee)

# List of all models for stargazer table
models <- list(
  model_village1, model_village2, model_village3,
  model_vec1, model_vec2, model_vec3,
  model_pta1, model_pta2, model_pta3,
  model_health1, model_health2, model_health3
)

# Compute clustered standard errors by district for each model
se_list <- lapply(models, function(model) {
  sqrt(diag(vcovCL(model, cluster = ~district)))
})

# Display the table using stargazer
stargazer(models,
          type = "text",                 # Change to "html" or "latex" for other outputs
          se = se_list,                  # Supply the list of clustered SEs
          dep.var.labels = c("Village Meetings", "VEC Meetings", "PTA Meetings", "Health Committee Meetings"),
          column.labels = c("Basic Controls", "GP Population Controls", "Other GP Characteristics"),
          covariate.labels = c("Landlord District", "Longitude", "Latitude", "Altitude", "Rainfall",
                               "Total Population", "Density", "SC Population Fraction", "OBC Population Fraction",
                               "Electricity", "Phone Access", "Literacy Rate", "Distance to Road"),
          omit.stat = c("f", "ser"),     # Omit F-statistic and standard error of regression for cleaner look
          column.sep.width = "2pt",      # Adjust space between columns
          title = "Table 4: Village Governance in Landlord and Nonlandlord Districts",
          align = TRUE                   # Center align the table
)


### --> PB AVEC DISTRICT LIGNE 56 + AVEC gpdistro DANS LES 9 PREMIÈRES REGRESSIONS