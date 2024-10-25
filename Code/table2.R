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
data_teachers <- read_dta("Datas/teachercharacterisitcs-table2.dta")

# Summary if op == 1
summary(data_teachers %>% filter(op == 1) %>% 
          select(tcaste3, tcaste2, tcaste, edu, dissch, tgen))

# Summary if op == 0
summary(data_teachers %>% filter(op == 0) %>% 
          select(tcaste3, tcaste2, tcaste, edu, dissch, tgen))

# Regression with clustered robust se
vars_teachers <- c("tcaste3", "tcaste2", "tcaste", "edu", "dissch", "tgen")

for (var in vars_teachers) {
  model <- lm(as.formula(paste(var, "~ op")), data = data_teachers)
  coeftest(model, vcov = vcovCL(model, cluster = ~district))
}

# Clear
rm()




# Import data 2
data_villages <- read_dta("Datas/villagecharacterisitcs-table2.dta")

# Summary if op == 1
summary(data_villages %>% filter(op == 1) %>% 
          select(elec, phone, gpdistroad, gpsl, density, lit, frsc, frobc))

# Summary if op == 0
summary(data_villages %>% filter(op == 0) %>% 
          select(elec, phone, gpdistroad, gpsl, density, lit, frsc, frobc))

# Regression with clustered robust se
vars_villages <- c("elec", "phone", "gpdistroad", "gpsl", "density", "lit", "frsc", "frobc")

for (var in vars_villages) {
  model <- lm(as.formula(paste(var, "~ op")), data = data_villages)
  coeftest(model, vcov = vcovCL(model, cluster = ~district))
}

# Clear
rm()




# Import data 3
data_students <- read_dta("Datas/studentcharacterisitcs-table2.dta")

# Summary if op == 1
summary(data_students %>% filter(op == 1) %>% 
          select(studenthighcaste, studentobccaste, studentsccaste, 
                 fatherprimaryedu, motherprimaryedu, studentgender))

# Summary if op == 0
summary(data_students %>% filter(op == 0) %>% 
          select(studenthighcaste, studentobccaste, studentsccaste, 
                 fatherprimaryedu, motherprimaryedu, studentgender))

# Regression with clustered robust se
vars_students <- c("studentobccaste", "studentsccaste", "studenthighcaste", 
                   "fatherprimaryedu", "motherprimaryedu", "studentgender")

for (var in vars_students) {
  model <- lm(as.formula(paste(var, "~ op")), data = data_students)
  coeftest(model, vcov = vcovCL(model, cluster = ~district))
}

# Clear
rm()