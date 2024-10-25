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

#### Code ####
# Import data 1
data_table4 <- read_dta("Datas/table4.dta")

# Regressions for villages (gsmeet)
model1 <- lm(gsmeet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain, data = data_table4)
coeftest(model1, vcov = vcovCL(model1, cluster = ~district))

model2 <- lm(gsmeet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density, data = data_table4)
coeftest(model2, vcov = vcovCL(model2, cluster = ~district))

model3 <- lm(gsmeet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density + elec + phone + gpdistro + lit, data = data_table4)
coeftest(model3, vcov = vcovCL(model3, cluster = ~district))

# Regressions for school committee (ssmeet)
model4 <- lm(ssmeet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain, data = data_table4)
coeftest(model4, vcov = vcovCL(model4, cluster = ~district))

model5 <- lm(ssmeet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density, data = data_table4)
coeftest(model5, vcov = vcovCL(model5, cluster = ~district))

model6 <- lm(ssmeet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density + elec + phone + gpdistro + lit, data = data_table4)
coeftest(model6, vcov = vcovCL(model6, cluster = ~district))

# Regressions for PTA (ptameet)
model7 <- lm(ptameet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain, data = data_table4)
coeftest(model7, vcov = vcovCL(model7, cluster = ~district))

model8 <- lm(ptameet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density, data = data_table4)
coeftest(model8, vcov = vcovCL(model8, cluster = ~district))

model9 <- lm(ptameet ~ op + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density + elec + phone + gpdistro + lit, data = data_table4)
coeftest(model9, vcov = vcovCL(model9, cluster = ~district))

# Clear
rm()




# Import data 2
data_healthcommittee <- read_dta("Datas/table4-health-committee.dta")

# Regressions for health committee (healthcommittee)
model10 <- lm(healthcommittee ~ oudh + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain, data = data_healthcommittee)
coeftest(model10, vcov = vcovCL(model10, cluster = ~district))

model11 <- lm(healthcommittee ~ oudh + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density, data = data_healthcommittee)
coeftest(model11, vcov = vcovCL(model11, cluster = ~district))

model12 <- lm(healthcommittee ~ oudh + dms01 + dms02 + dms03 + dms04 + dmts1 + dmts2 + dmts3 + dmts4 + lon + lat + alt + rain + frsc + frobc + totpop + density + elec + phone + gpdistro + lit, data = data_healthcommittee)
coeftest(model12, vcov = vcovCL(model12, cluster = ~district))

# Clear
rm()