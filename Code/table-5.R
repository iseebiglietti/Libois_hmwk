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

# Import Data
data_teacher <- read_dta("Datas/table-5-teacher.dta")
data_stipend <- read_dta("Datas/table-5-stipend.dta")
data_infra <- read_dta("Datas/table-5-infrastructure.dta")
data_score <- read_dta("Datas/table-5-student-score-attendance.dta")

# Define all regression models as specified in the code
# Teacher attendance and activity regressions
model1 <- lm(tattendance ~ op + dms01 + dmts4 + lon + lat + alt + rain, data = data_teacher)
model2 <- lm(tactivity ~ op + dms01 + dmts4 + lon + lat + alt + rain, data = data_teacher)
model3 <- lm(tattendance ~ frsc + frobc + totpop + op + density + dms01 + dmts4 + lon + lat + alt + rain, data = data_teacher)
model4 <- lm(tactivity ~ frsc + frobc + totpop + op + density + dms01 + dmts4 + lon + lat + alt + rain, data = data_teacher)
model5 <- lm(tattendance ~ tcaste + tcaste2 + tgen + edu + toenr + dissch + frsc + frobc + totpop + op + density + lit + index + elec + phone + gpdistro + dms01 + dmts4 + lon + lat + alt + rain, data = data_teacher)
model6 <- lm(tactivity ~ tcaste + tcaste2 + tgen + edu + toenr + dissch + frsc + frobc + totpop + op + density + lit + index + elec + phone + gpdistro + dms01 + dmts4 + lon + lat + alt + rain, data = data_teacher)

# Stipend received by SC students
model7 <- lm(scholar ~ oudh + dms01 + dmts4 + lon + lat + alt + rain, data = data_stipend, subset = caste2 == 0)
model8 <- lm(scholar ~ oudh + frsc + frobc + density + totpop + dms01 + dmts4 + lon + lat + alt + rain, data = data_stipend, subset = caste2 == 0)
model9 <- lm(scholar ~ oudh + frsc + frobc + density + totpop + lit + elec + phone + gpdistro + dms01 + dmts4 + lon + lat + alt + rain, data = data_stipend, subset = caste2 == 0)

# School Infrastructure Index regressions
model10 <- lm(index ~ op + dms01 + dmts4 + lon + lat + alt + rain, data = data_infra)
model11 <- lm(index ~ frsc + frobc + totpop + op + density + dms01 + dmts4 + lon + lat + alt + rain, data = data_infra)
model12 <- lm(index ~ frsc + frobc + totpop + op + density + lit + elec + phone + gpdistro + dms01 + dmts4 + lon + lat + alt + rain, data = data_infra)

# Student standardized score and attendance regressions
model13 <- lm(meanscore ~ op + dms01 + dmts4 + lon + lat + alt + rain, data = data_score)
model14 <- lm(studentattendance ~ op + dms01 + dmts4 + lon + lat + alt + rain, data = data_score)
model15 <- lm(meanscore ~ frsc + frobc + totpop + op + density + dms01 + dmts4 + lon + lat + alt + rain, data = data_score)
model16 <- lm(studentattendance ~ frsc + frobc + totpop + op + density + dms01 + dmts4 + lon + lat + alt + rain, data = data_score)
model17 <- lm(meanscore ~ scaste + scaste2 + sgen + me2 + me3 + fe2 + fe3 + tepupr + frsc + frobc + totpop + op + density + elec + phone + gpdistro + index + lit + dms01 + dmts4 + lon + lat + alt + rain, data = data_score)
model18 <- lm(studentattendance ~ scaste + scaste2 + sgen + me2 + me3 + fe2 + fe3 + tepupr + frsc + frobc + totpop + op + density + elec + phone + gpdistro + index + lit + dms01 + dmts4 + lon + lat + alt + rain, data = data_score)

# Collect all models into a list for stargazer
models <- list(model1, model2, model3, model4, model5, model6,
               model7, model8, model9,
               model10, model11, model12,
               model13, model14, model15, model16, model17, model18)

# Compute clustered standard errors by district for each model
se_list <- lapply(models, function(model) {
  sqrt(diag(vcovCL(model, cluster = ~district)))
})

# Display the table using stargazer
stargazer(models,
          type = "text",                 # Change to "html" or "latex" for other outputs
          se = se_list,                  # Supply the list of clustered SEs
          dep.var.labels = c("Teacher Attendance", "Teacher Activity", "Stipend Received", "School Infrastructure", "Student Score", "Student Attendance"),
          column.labels = c("Soil/Climate", "Population", "GP Characteristics"),
          covariate.labels = c("Landlord District", "Longitude", "Latitude", "Altitude", "Rainfall",
                               "Total Population", "Density", "SC Population Fraction", "OBC Population Fraction",
                               "Electricity", "Phone Access", "Literacy Rate", "Distance to Road",
                               "Teacher Caste", "Teacher Gender", "Teacher Education", "Student Caste", "Student Gender",
                               "Enrollment", "Distance to School"),
          omit.stat = c("f", "ser"),     # Omit F-statistic and standard error of regression for cleaner look
          column.sep.width = "2pt",      # Adjust space between columns
          title = "Table 5: Impact of Landlord Districts on Teacher, Stipend, Infrastructure, and Student Outcomes",
          align = TRUE                   # Center align the table
)
### --> PB AVEC DISTRICT LIGNE 54 + AVEC gpdistro DANS LES REGRESSIONS