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
data_table3 <- read_dta("Datas/table3.dta")

# Summary if op == 1
summary(data_table3 %>% filter(op == 1) %>% 
          select(highparty1, highparty12))

# Summary if op == 0
summary(data_table3 %>% filter(op == 0) %>% 
          select(highparty1, highparty12))

# Regression with clustered robust se
vars_table3 <- c("highparty1", "highparty12")

for (var in vars_table3) {
  model <- lm(as.formula(paste(var, "~ op")), data = data_table3)
  coeftest(model, vcov = vcovCL(model, cluster = ~district)) 
}

# Regression for the fraction of votes cast by high and middle caste parties
model_frhm <- lm(frhm ~ op, data = data_table3)
coeftest(model_frhm, vcov = vcovCL(model_frhm, cluster = ~district)) 

# Clear
rm()




# Import data 2
data_pradhan <- read_dta("Datas/pradhan-election-table3.dta")

# Summary if op == 1 et reserv == 0
summary(data_pradhan %>% filter(op == 1, reserv == 0) %>% 
          select(pradhanhighcaste))

# Summary if op == 0 et reserv == 0
summary(data_pradhan %>% filter(op == 0, reserv == 0) %>% 
          select(pradhanhighcaste))

# Regression with clustered robust se
model_pradhan <- lm(pradhanhighcaste ~ op, data = data_pradhan %>% filter(reserv == 0))
coeftest(model_pradhan, vcov = vcovCL(model_pradhan, cluster = ~district))

# Clear
rm()