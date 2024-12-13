#### Load packages ####
library(haven)
library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)

# Import Data
teacher <- read_dta("Datas/table-5-teacher.dta")
stipend <- read_dta("Datas/table-5-stipend.dta") %>%
  filter(caste2 == 0)
infra <- read_dta("Datas/table-5-infrastructure.dta")
score <- read_dta("Datas/table-5-student-score-attendance.dta")

# Functions
generate_formula <- function(dependent_var, main_var, control_vars) {
  as.formula(
    paste(
      dependent_var,
      "~", main_var,
      "+", paste(control_vars, collapse = " + ")
    )
  )
}

reg_teacher <- function(dependent_var, main_var, control_vars) {
  formula <- generate_formula(dependent_var, main_var, control_vars)
  model <- lm(formula, data = teacher)
  robust_se <- coeftest(model, vcov = vcovCL, cluster = ~districtid)
  return(robust_se)
}

reg_stipend <- function(dependent_var, main_var, control_vars) {
  formula <- generate_formula(dependent_var, main_var, control_vars)
  model <- lm(formula, data = stipend)
  robust_se <- coeftest(model, vcov = vcovCL, cluster = ~districtid)
  return(robust_se)
}

reg_infra <- function(dependent_var, main_var, control_vars) {
  formula <- generate_formula(dependent_var, main_var, control_vars)
  model <- lm(formula, data = infra)
  robust_se <- coeftest(model, vcov = vcovCL, cluster = ~districtid)
  return(robust_se)
}

reg_score <- function(dependent_var, main_var, control_vars) {
  formula <- generate_formula(dependent_var, main_var, control_vars)
  model <- lm(formula, data = score)
  robust_se <- coeftest(model, vcov = vcovCL, cluster = ~districtid)
  return(robust_se)
}

basic_controls <- c(
  "dms01", "dms02", "dms03", "dms04",
  "dms05", "dms06", "dms07", "dms08",
  "dms09", "dms10", "dms11", "dms12",
  "dms13", "dms14", "dms15", "dms16",
  "dms17", "dms18", "dms19", "dms20",
  "dms21", "dmaq3", "dmaq2", "dmaq1",
  "dmph4", "dmph5", "dmph6", "dmph7",
  "dmph8", "dmts1", "dmts2", "dmts3",
  "dmts4", "lon", "lat", "alt", "rain"
)

additional_controls_2 <- c("frsc", "frobc", "totpop", "density")
additional_controls_3 <- c(
  "elec", "phone", "gpdistroad", "lit",
  "tcaste", "tcaste2", "tgen", "edu", "toenr", "dissch",
  "lit", "index", "elec", "phone", "gpdistroad"
)
additional_controls_4 <- c("lit", "elec", "phone", "gpdistro")
additional_controls_5 <- c("lit", "elec", "phone", "gpdistroad")
additional_controls_6 <- c(
  "scaste", "scaste2", "sgen",
  "me2", "me3", "fe2", "fe3", "tepupr", "index"
)



### Teachers
# Soil and climate as controls
teacher1 <- reg_teacher("tattendance", "op", basic_controls)
teacher2 <- reg_teacher("tactivity", "op", basic_controls)
# oil, climate controls and population variables
teacher3 <- reg_teacher(
  "tattendance",
  "op",
  c(basic_controls, additional_controls_2)
)
teacher4 <- reg_teacher(
  "tactivity",
  "op",
  c(basic_controls, additional_controls_2)
)
# soil, climate controls, population variables and teacher, gp characteristics
teacher5 <- reg_teacher(
  "tattendance",
  "op",
  c(basic_controls, additional_controls_2, additional_controls_3)
)
teacher6 <- reg_teacher(
  "tactivity",
  "op",
  c(basic_controls, additional_controls_2, additional_controls_3)
)
### Stipend
# soil and climate as control
stipend1 <- reg_stipend("scholar", "oudh", basic_controls)
stipend2 <- reg_stipend(
  "scholar",
  "oudh",
  c(basic_controls, additional_controls_2)
)
stipend3 <- reg_stipend(
  "scholar",
  "oudh",
  c(basic_controls, additional_controls_2, additional_controls_4)
)
### Infrastructure
# soil and climate as controls
infra1 <- reg_infra("index", "op", basic_controls)
infra2 <- reg_infra(
  "index",
  "op",
  c(basic_controls, additional_controls_2)
)
infra3 <- reg_infra(
  "index",
  "op",
  c(basic_controls, additional_controls_2, additional_controls_5)
)

### Score and attendance
# soil and climate as controls
meanscore1 <- reg_score("meanscore", "op", basic_controls)
meanscore2 <- reg_score(
  "meanscore",
  "op",
  c(basic_controls, additional_controls_2)
)
meanscore3 <- reg_score(
  "meanscore",
  "op",
  c(
    basic_controls, additional_controls_2,
    additional_controls_5, additional_controls_6)
)


attendance1 <- reg_score("studentattendance", "op", basic_controls)
attendance2 <- reg_score(
  "studentattendance",
  "op",
  c(basic_controls, additional_controls_2)
)
attendance3 <- reg_score(
  "studentattendance",
  "op",
  c(
    basic_controls, additional_controls_2,
    additional_controls_5, additional_controls_6)
)


stargazer(models,
  type = "html", # Change to "html" or "latex" for other outputs
  se = se_list, # Supply the list of clustered SEs
  dep.var.labels = c("Teacher Attendance", "Teacher Activity", "Stipend Received", "School Infrastructure", "Student Score", "Student Attendance"),
  column.labels = c("Soil/Climate", "Population", "GP Characteristics"),
  covariate.labels = c(
    "Landlord District", "Longitude", "Latitude", "Altitude", "Rainfall",
    "Total Population", "Density", "SC Population Fraction", "OBC Population Fraction",
    "Electricity", "Phone Access", "Literacy Rate", "Distance to Road",
    "Teacher Caste", "Teacher Gender", "Teacher Education", "Student Caste", "Student Gender",
    "Enrollment", "Distance to School"
  ),
  omit.stat = c("f", "ser"), # Omit F-statistic and standard error of regression for cleaner look
  column.sep.width = "2pt", # Adjust space between columns
  title = "Table 5: Impact of Landlord Districts on Teacher, Stipend, Infrastructure, and Student Outcomes",
  align = TRUE, # Center align the table
  out = "Output/table5_reproduction.html"
)
