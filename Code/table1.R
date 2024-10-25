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

#### Code ####
# Import data 1
data1 <- read_dta("Datas/table1-soil-climate.dta")

# Summary if `op == 1`
summary(data1 %>% filter(op == 1) %>% 
          select(lon, lat, alt, rain, soilblack, soilalkaline, 
                 soilalluvialriver, soilred, aquifermorethan150, 
                 aquifer100and150, phstrongalkali, phslightalkali, 
                 phneutral, phslightacid, topsoil25and50cm, topsoil50and100cm, 
                 topsoilmorethan300cm))

# Summary if `op == 0`
summary(data1 %>% filter(op == 0) %>% 
          select(lon, lat, alt, rain, soilblack, soilalkaline, 
                 soilalluvialriver, soilred, aquifermorethan150, 
                 aquifer100and150, phstrongalkali, phslightalkali, 
                 phneutral, phslightacid, topsoil25and50cm, topsoil50and100cm, 
                 topsoilmorethan300cm))

# Regressions with robust standard errors
vars1 <- c("lon", "lat", "alt", "rain", "soilblack", "soilalkaline", 
           "soilalluvialriver", "soilred", "aquifermorethan150", 
           "aquifer100and150", "phstrongalkali", "phslightalkali", 
           "phneutral", "phslightacid", "topsoil25and50cm", 
           "topsoil50and100cm", "topsoilmorethan300cm")

for (var in vars1) {
  model <- lm(as.formula(paste(var, "~ op")), data = data1)
  coeftest(model, vcov = vcovHC(model, type = "HC1"))
}

# Clear
rm()




# Import data 2
data2 <- read_dta("Datas/table1-population-and-census.dta")

# Summary if `op == 1`
summary(data2 %>% filter(op == 1) %>% 
          select(poverty, literacyrate, populationsize, populationdensity, 
                 fractionsc, permanenthouse, pervilelec, pervilroad, 
                 pervilhealth, pervilschool, ger))

# Summary if `op == 0`
summary(data2 %>% filter(op == 0) %>% 
          select(poverty, literacyrate, populationsize, populationdensity, 
                 fractionsc, permanenthouse, pervilelec, pervilroad, 
                 pervilhealth, pervilschool, ger))

# Regressions with robust standard errors
vars2 <- c("poverty", "literacyrate", "populationsize", "populationdensity", 
           "fractionsc", "pervilelec", "pervilroad", "pervilhealth", 
           "pervilschool", "permanenthouse", "ger")

for (var in vars2) {
  model <- lm(as.formula(paste(var, "~ op")), data = data2)
  coeftest(model, vcov = vcovHC(model, type = "HC1"))
}

# Clear
rm()
