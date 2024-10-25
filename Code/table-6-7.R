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

#### Code ####
# Import data 1
data_teacher <- read_dta("Datas/table-6and7-teacher.dta")

### Table 6 
# Regression on Teachers' attendance (tattendance)
model1 <- lm(tattendance ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop, data = data_teacher)
coeftest(model1, vcov = vcovCL(model1, cluster = ~district))

linearHypothesis(model1, c("op + 0.11*hrop + 0.099*unop + 0.076*rbop + 0.21*prop + 0.22*slop + 0.13*fzop + 0.09*gnop"))

# Regression on Teachers' activity (tactivity)
model2 <- lm(tactivity ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop, data = data_teacher)
coeftest(model2, vcov = vcovCL(model2, cluster = ~district))

linearHypothesis(model2, c("op + 0.11*hrop + 0.099*unop + 0.076*rbop + 0.21*prop + 0.22*slop + 0.13*fzop + 0.09*gnop"))

### Table 7
# Only taking the landlord districts and looking at difference between permanent and oudh districts
model3 <- lm(tattendance ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop + slopperm + perm, data = data_teacher, subset = op == 1)
coeftest(model3, vcov = vcovCL(model3, cluster = ~district))

linearHypothesis(model3, c("perm + 0.27*slopperm"))

model4 <- lm(tactivity ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop + slopperm + perm, data = data_teacher, subset = op == 1)
coeftest(model4, vcov = vcovCL(model4, cluster = ~district))

linearHypothesis(model4, c("perm + 0.27*slopperm"))

# Clear
rm()




# Import data 2
data_student <- read_dta("Datas/table-6and7-student.dta")

### Table 6 
# Regression on students' scores (meanscore)
model5 <- lm(meanscore ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop, data = data_student)
coeftest(model5, vcov = vcovCL(model5, cluster = ~district))

linearHypothesis(model5, c("op + 0.11*hrop + 0.11*unop + 0.07*rbop + 0.19*prop + 0.19*slop + 0.12*fzop + 0.14*gnop"))

# Regression on students' attendance (sattendance)
model6 <- lm(sattendance ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop, data = data_student)
coeftest(model6, vcov = vcovCL(model6, cluster = ~district))

linearHypothesis(model6, c("op + 0.11*hrop + 0.11*unop + 0.07*rbop + 0.19*prop + 0.19*slop + 0.12*fzop + 0.14*gnop"))

### Table 7 
# Only taking the landlord districts and looking at difference between permanent and oudh districts
model7 <- lm(meanscore ~ kh + hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop + propperm + perm, data = data_student, subset = op == 1)
coeftest(model7, vcov = vcovCL(model7, cluster = ~district))

linearHypothesis(model7, c("perm + 0.18*propperm"))

model8 <- lm(sattendance ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop + propperm + perm, data = data_student, subset = op == 1)
coeftest(model8, vcov = vcovCL(model8, cluster = ~district))

linearHypothesis(model8, c("perm + 0.18*propperm"))

# Clear
rm()




# Import data 3
data_stipend <- read_dta("Datas/table-6-stipend.dta")

# Regression on scholarships (scholar)
model9 <- lm(scholar ~ hr + un + rb + pr + sl + fz + gn + oudh + hro + uno + rbo + pro + slo + fzo + gno, data = data_stipend, subset = caste2 == 0)
coeftest(model9, vcov = vcovCL(model9, cluster = ~district))

linearHypothesis(model9, c("oudh + 0.13*hro + 0.17*uno + 0.08*rbo + 0.18*pro + 0.08*slo + 0.14*fzo + 0.13*gno"))

# Clear
rm()




# Import data 4
data_infra <- read_dta("Datas/table-6and7-school-infrastructure.dta")

### Table 6 
# Regression on infrastructure index (index)
model10 <- lm(index ~ hr + un + rb + pr + sl + fz + gn + op + hrop + unop + rbop + prop + slop + fzop + gnop, data = data_infra)
coeftest(model10, vcov = vcovCL(model10, cluster = ~district))

linearHypothesis(model10, c("op + 0.11*hrop + 0.11*unop + 0.08*rbop + 0.19*prop + 0.19*slop + 0.11*fzop + 0.11*gnop"))

### Table 7 
# Only taking the landlord districts and looking at difference between permanent and oudh districts
model11 <- lm(index ~ kh + hr + un + rb + pr + sl + fz + gn + perm + slopperm, data = data_infra, subset = op == 1)
coeftest(model11, vcov = vcovCL(model11, cluster = ~district))

linearHypothesis(model11, c("perm + 0.27*slopperm"))

# Clear
rm()