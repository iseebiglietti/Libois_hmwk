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
library(car)
library(stargazer)

#### Code ####
# Import Data
data_teacher <- read_dta("Datas/table-6and7-teacher.dta")
data_student <- read_dta("Datas/table-6and7-student.dta")
data_stipend <- read_dta("Datas/table-6-stipend.dta")
data_infra <- read_dta("Datas/table-6and7-school-infrastructure.dta")

### Table 6: Regressions across different outcomes (Teacher Attendance, Student Scores, etc.)
# Models for Teacher Attendance and Activity (Table 6)
model1 <- lm(tattendance ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop, data = data_teacher)
model2 <- lm(tactivity ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop, data = data_teacher)

# Models for Student Scores and Attendance (Table 6)
model5 <- lm(meanscore ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop, data = data_student)
model6 <- lm(sattendance ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop, data = data_student)

# Model for Scholarship Receipt (Table 6, SC students only)
model9 <- lm(scholar ~ hr + un + rb + pr + sl + fz + gn + oudh + hro + uno + rbo + pro + slo + fzo + gno, data = data_stipend, subset = caste2 == 0)

# Model for School Infrastructure Index (Table 6)
model10 <- lm(index ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop, data = data_infra)

### Table 7: Regressions focusing on landlord districts
# Teacher Attendance and Activity (Table 7)
model3 <- lm(tattendance ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop + slopperm + perm, data = data_teacher, subset = op == 1)
model4 <- lm(tactivity ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop + slopperm + perm, data = data_teacher, subset = op == 1)

# Student Scores and Attendance (Table 7)
model7 <- lm(meanscore ~ kh + hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop + propperm + perm, data = data_student, subset = op == 1)
model8 <- lm(sattendance ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop + propperm + perm, data = data_student, subset = op == 1)

# School Infrastructure Index (Table 7)
model11 <- lm(index ~ kh + hr + un + rb + pr + sl + fz + gn + perm + slopperm, data = data_infra, subset = op == 1)

# Compile all models for stargazer table
models_table6 <- list(model1, model2, model5, model6, model9, model10)
models_table7 <- list(model3, model4, model7, model8, model11)

# Compute clustered standard errors by district for each model in Tables 6 and 7
se_table6 <- lapply(models_table6, function(model) {
  sqrt(diag(vcovCL(model, cluster = ~district)))
})

se_table7 <- lapply(models_table7, function(model) {
  sqrt(diag(vcovCL(model, cluster = ~district)))
})

# Conduct linear hypothesis tests and capture results for Table 6 and Table 7
# (Optional) Format results as notes or separate objects if needed
hypotheses_table6 <- list(
  linearHypothesis(model1, "op + 0.11*hrop + 0.099*unop + 0.076*rbop + 0.21*prop + 0.22*slop + 0.13*fzop + 0.09*gnop"),
  linearHypothesis(model2, "op + 0.11*hrop + 0.099*unop + 0.076*rbop + 0.21*prop + 0.22*slop + 0.13*fzop + 0.09*gnop"),
  linearHypothesis(model5, "op + 0.11*hrop + 0.11*unop + 0.07*rbop + 0.19*prop + 0.19*slop + 0.12*fzop + 0.14*gnop"),
  linearHypothesis(model6, "op + 0.11*hrop + 0.11*unop + 0.07*rbop + 0.19*prop + 0.19*slop + 0.12*fzop + 0.14*gnop"),
  linearHypothesis(model9, "oudh + 0.13*hro + 0.17*uno + 0.08*rbo + 0.18*pro + 0.08*slo + 0.14*fzo + 0.13*gno"),
  linearHypothesis(model10, "op + 0.11*hrop + 0.11*unop + 0.08*rbop + 0.19*prop + 0.19*slop + 0.11*fzop + 0.11*gnop")
)

hypotheses_table7 <- list(
  linearHypothesis(model3, "perm + 0.27*slopperm"),
  linearHypothesis(model4, "perm + 0.27*slopperm"),
  linearHypothesis(model7, "perm + 0.18*propperm"),
  linearHypothesis(model8, "perm + 0.18*propperm"),
  linearHypothesis(model11, "perm + 0.27*slopperm")
)

# Display Table 6 using stargazer
stargazer(models_table6,
          type = "text",
          se = se_table6,
          dep.var.labels = c("Teacher Attendance", "Teacher Activity", "Student Scores", "Student Attendance", "Scholarship Receipt", "Infrastructure Index"),
          covariate.labels = c("Historical Control Variables", "Oudh and Landlord District Interactions", "Permanent Landlord District Effects"),
          omit.stat = c("f", "ser"),
          title = "Table 6: Historical Effects on Teacher, Student, Scholarship, and Infrastructure Outcomes",
          align = TRUE
)

# Display Table 7 using stargazer
stargazer(models_table7,
          type = "text",
          se = se_table7,
          dep.var.labels = c("Teacher Attendance", "Teacher Activity", "Student Scores", "Student Attendance", "Infrastructure Index"),
          covariate.labels = c("Historical Control Variables", "Landlord/Oudh and Permanent Effects"),
          omit.stat = c("f", "ser"),
          title = "Table 7: Effects within Landlord Districts on Teacher, Student, and Infrastructure Outcomes",
          align = TRUE
)

### --> PB AVEC DISTRICT (COMME D'HAB), + DANS LES HYPOTHESES : "Erreur dans linearHypothesis.lm(model3, "perm + 0.27*slopperm") : there are aliased coefficients in the model."