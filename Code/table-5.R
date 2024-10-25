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
data_teacher <- read_dta("Datas/table-5-teacher.dta")

# Regressions with soil and climate as control
model1 <- lm(tattendance ~ op + dms01 + dmts4 + lon + lat + alt + rain, data = data_teacher)
coeftest(model1, vcov = vcovCL(model1, cluster = ~district))

model2 <- lm(tactivity ~ op + dms01 + dmts4 + lon + lat + alt + rain, data = data_teacher)
coeftest(model2, vcov = vcovCL(model2, cluster = ~district))

# Regressions with soil, climate controls and population variables
model3 <- lm(tattendance ~ frsc + frobc + totpop + op + density + dms01 + dmts4 + lon + lat + alt + rain, data = data_teacher)
coeftest(model3, vcov = vcovCL(model3, cluster = ~district))

model4 <- lm(tactivity ~ frsc + frobc + totpop + op + density + dms01 + dmts4 + lon + lat + alt + rain, data = data_teacher)
coeftest(model4, vcov = vcovCL(model4, cluster = ~district))

# Regressions with soil, climate controls, population variables and teacher, gp characteristics
model5 <- lm(tattendance ~ tcaste + tcaste2 + tgen + edu + toenr + dissch + frsc + frobc + totpop + op + density + lit + index + elec + phone + gpdistro + dms01 + dmts4 + lon + lat + alt + rain, data = data_teacher)
coeftest(model5, vcov = vcovCL(model5, cluster = ~district))

model6 <- lm(tactivity ~ tcaste + tcaste2 + tgen + edu + toenr + dissch + frsc + frobc + totpop + op + density + lit + index + elec + phone + gpdistro + dms01 + dmts4 + lon + lat + alt + rain, data = data_teacher)
coeftest(model6, vcov = vcovCL(model6, cluster = ~district))

# Clear
rm()




# Import data 2
data_stipend <- read_dta("Datas/table-5-stipend.dta")

### Regressions for stipend received by SC students
# with soil and climate as control
model7 <- lm(scholar ~ oudh + dms01 + dmts4 + lon + lat + alt + rain, data = data_stipend, subset = caste2 == 0)
coeftest(model7, vcov = vcovCL(model7, cluster = ~district))

# with soil, climate controls and population variables
model8 <- lm(scholar ~ oudh + frsc + frobc + density + totpop + dms01 + dmts4 + lon + lat + alt + rain, data = data_stipend, subset = caste2 == 0)
coeftest(model8, vcov = vcovCL(model8, cluster = ~district))

# with soil, climate controls, population variables and gp characteristics
model9 <- lm(scholar ~ oudh + frsc + frobc + density + totpop + lit + elec + phone + gpdistro + dms01 + dmts4 + lon + lat + alt + rain, data = data_stipend, subset = caste2 == 0)
coeftest(model9, vcov = vcovCL(model9, cluster = ~district))

# Clear
rm()




# Import data 3
data_infra <- read_dta("Datas/table-5-infrastructure.dta")

# Regressions with soil and climate as control
model10 <- lm(index ~ op + dms01 + dmts4 + lon + lat + alt + rain, data = data_infra)
coeftest(model10, vcov = vcovCL(model10, cluster = ~district))

# Regressions with soil, climate controls and population variables
model11 <- lm(index ~ frsc + frobc + totpop + op + density + dms01 + dmts4 + lon + lat + alt + rain, data = data_infra)
coeftest(model11, vcov = vcovCL(model11, cluster = ~district))

# Regressions with soil, climate controls, population variables and gp characteristics
model12 <- lm(index ~ frsc + frobc + totpop + op + density + lit + elec + phone + gpdistro + dms01 + dmts4 + lon + lat + alt + rain, data = data_infra)
coeftest(model12, vcov = vcovCL(model12, cluster = ~district))

# Clear
rm()




# Import data 4
data_score <- read_dta("Datas/table-5-student-score-attendance.dta")

### Regressions for students' standardized score and attendance
# with soil and climate as control
model13 <- lm(meanscore ~ op + dms01 + dmts4 + lon + lat + alt + rain, data = data_score)
coeftest(model13, vcov = vcovCL(model13, cluster = ~district))

model14 <- lm(studentattendance ~ op + dms01 + dmts4 + lon + lat + alt + rain, data = data_score)
coeftest(model14, vcov = vcovCL(model14, cluster = ~district))

# with soil, climate controls and population variables
model15 <- lm(meanscore ~ frsc + frobc + totpop + op + density + dms01 + dmts4 + lon + lat + alt + rain, data = data_score)
coeftest(model15, vcov = vcovCL(model15, cluster = ~district))

model16 <- lm(studentattendance ~ frsc + frobc + totpop + op + density + dms01 + dmts4 + lon + lat + alt + rain, data = data_score)
coeftest(model16, vcov = vcovCL(model16, cluster = ~district))

# with soil, climate controls, population variables and gp characteristics
model17 <- lm(meanscore ~ scaste + scaste2 + sgen + me2 + me3 + fe2 + fe3 + tepupr + frsc + frobc + totpop + op + density + elec + phone + gpdistro + index + lit + dms01 + dmts4 + lon + lat + alt + rain, data = data_score)
coeftest(model17, vcov = vcovCL(model17, cluster = ~district))

model18 <- lm(studentattendance ~ scaste + scaste2 + sgen + me2 + me3 + fe2 + fe3 + tepupr + frsc + frobc + totpop + op + density + elec + phone + gpdistro + index + lit + dms01 + dmts4 + lon + lat + alt + rain, data = data_score)
coeftest(model18, vcov = vcovCL(model18, cluster = ~district))

# Clear
rm()