#### Load packages ####
library(haven)
library(dplyr)
library(lmtest)
library(sandwich)
library(car)
library(stargazer)
library(multcomp)


#### Code ####
# Import Data
teacher <- read_dta("Datas/table-6and7-teacher.dta")
student <- read_dta("Datas/table-6and7-student.dta")
stipend <- read_dta("Datas/table-6-stipend.dta")
infra <- read_dta("Datas/table-6and7-school-infrastructure.dta")

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

reg_student <- function(dependent_var, main_var, control_vars) {
  formula <- generate_formula(dependent_var, main_var, control_vars)
  model <- lm(formula, data = student)
  robust_se <- coeftest(model, vcov = vcovCL, cluster = ~districtid)
  return(robust_se)
}
basic_controls <- c(
  "hr", "un", "rb", "pr", "sl", "fz",
  "gn", "hrop", "unop", "rbop", "prop",
  "slop", "fzop", "gnop"
)

### Table 6
# attendance
attendance <- reg_teacher("tattendance", "op", basic_controls)


# Ajuster le modèle de régression
attendance <- lm(
  tattendance ~ hr + un + rb + pr + sl + fz + gn + op +
    hrop + unop + rbop + prop + slop + fzop + gnop,
  data = teacher
)
# Calculer la matrice de variance-covariance par cluster (par district ici)
clustered_se <- vcovCL(attendance, cluster = ~districtid)

# Définir la combinaison linéaire
lin_comb1 <- c("op + 0.11 * hrop + 0.099 * unop + 0.076 * rbop + 0.21 * prop + 0.22 * slop + 0.13 * fzop + 0.09 * gnop = 0")
# Tester la combinaison linéaire des coefficients
test_lincom <- glht(attendance, linfct = lin_comb1, vcov = clustered_se)
summary(test_lincom)

# Activity
activity <- lm(
  tactivity ~ hr + un + rb + pr + sl + fz + gn + op +
    hrop + unop + rbop + prop + slop + fzop + gnop,
  data = teacher
)
# Calculer la matrice de variance-covariance par cluster (par district ici)
clustered_se <- vcovCL(activity, cluster = ~districtid)
# Tester la combinaison linéaire des coefficients
test_lincom <- glht(activity, linfct = lin_comb1, vcov = clustered_se)
summary(test_lincom)

### Table 7
teacher1 <- teacher %>% filter(op == 1)


attendance <- lm(
  tattendance ~ hr + un + rb + pr + sl + fz + gn +
    slopperm + perm,
  data = teacher1
)

# Calculer la matrice de variance-covariance par cluster (par district ici)
clustered_se <- vcovCL(attendance, cluster = ~districtid)

# Définir la combinaison linéaire
lin_comb2 <- c("perm+slopperm*.27 = 0")
# Tester la combinaison linéaire des coefficients
test_lincom <- glht(attendance, linfct = lin_comb2, vcov = clustered_se)
summary(test_lincom)


































# Display Table 6 using stargazer
stargazer(models_table6,
  type = "html",
  se = se_table6,
  dep.var.labels = c("Teacher Attendance", "Teacher Activity", "Student Scores", "Student Attendance", "Scholarship Receipt", "Infrastructure Index"),
  covariate.labels = c("Historical Control Variables", "Oudh and Landlord District Interactions", "Permanent Landlord District Effects"),
  omit.stat = c("f", "ser"),
  title = "Table 6: Historical Effects on Teacher, Student, Scholarship, and Infrastructure Outcomes",
  align = TRUE,
  out = "Output/table6_reproduction.html"
)

# Display Table 7 using stargazer
stargazer(models_table7,
  type = "html",
  se = se_table7,
  dep.var.labels = c("Teacher Attendance", "Teacher Activity", "Student Scores", "Student Attendance", "Infrastructure Index"),
  covariate.labels = c("Historical Control Variables", "Landlord/Oudh and Permanent Effects"),
  omit.stat = c("f", "ser"),
  title = "Table 7: Effects within Landlord Districts on Teacher, Student, and Infrastructure Outcomes",
  align = TRUE,
  out = "Output/table7_reproduction.html"
)

### --> PB AVEC DISTRICT (COMME D'HAB), + DANS LES HYPOTHESES : "Erreur dans linearHypothesis.lm(model3, "perm + 0.27*slopperm") : there are aliased coefficients in the model."
