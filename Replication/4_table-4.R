#### Load packages ####
library(haven)
library(dplyr)
library(lmtest)
library(sandwich)
library(clubSandwich)
library(stargazer)

library(MASS)       # Pour LAD
library(quantreg)   # Pour LAD
library(robustbase) # Pour M-, S-, et MM-estimations

#### Code ####
# Import datas
table4 <- as.data.frame(read_dta("Datas/table4.dta"))
hc <- as.data.frame(read_dta("Datas/table4-health-committee.dta"))


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

reg_table4 <- function(formula) {
  model <- lm(formula, data = table4)
  robust_se <- coeftest(model, vcov = vcovCL, cluster = ~districtid)
  return(robust_se)
}

reg_hc <- function(formula) {
  model <- lm(formula, data = hc)
  robust_se <- coeftest(model, vcov = vcovCL, cluster = ~districtid)
  return(robust_se)
}

reg_lad <- function(formula, data) {
  model <- rq(formula, data = data, tau = 0.5) # tau = 0.5 pour la médiane
  return(summary(model))
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
  "frsc", "frobc", "totpop",
  "density", "elec", "phone", "gpdistroad", "lit"
)



fgsmeet1 <- generate_formula("gsmeet", "op", basic_controls)
fgsmeet2 <- generate_formula(
  "gsmeet",
  "op", c(basic_controls, additional_controls_2)
)
fgsmeet3 <- generate_formula(
  "gsmeet", "op",
  c(basic_controls, additional_controls_3)
)

fssmeet1 <- generate_formula("ssmeet", "op", basic_controls)
fssmeet2 <- generate_formula(
  "ssmeet",
  "op", c(basic_controls, additional_controls_2)
)
fssmeet3 <- generate_formula(
  "ssmeet", "op",
  c(basic_controls, additional_controls_3)
)

fptameet1 <- generate_formula("ptameet", "op", basic_controls)
fptameet2 <- generate_formula(
  "ptameet",
  "op", c(basic_controls, additional_controls_2)
)
fptameet3 <- generate_formula(
  "ptameet", "op",
  c(basic_controls, additional_controls_3)
)



gsmeet1 <- reg_table4(fgsmeet1)
gsmeet2 <- reg_table4(fgsmeet2)
gsmeet3 <- reg_table4(fgsmeet3)

ssmeet1 <- reg_table4(fssmeet1)
ssmeet2 <- reg_table4(fssmeet2)
ssmeet3 <- reg_table4(fssmeet3)

ptameet1 <- reg_table4(fptameet1)
ptameet2 <- reg_table4(fptameet2)
ptameet3 <- reg_table4(fptameet3)

additional_controls_3 <- c(
  "frsc", "frobc", "totpop",
  "density", "elec", "phone", "gpdistro", "lit"
)

fhealthcommittee1 <- generate_formula("healthcommittee", "oudh", basic_controls)
fhealthcommittee2 <- generate_formula(
  "healthcommittee", "oudh",
  c(basic_controls, additional_controls_2)
)
fhealthcommittee3 <- generate_formula(
  "healthcommittee", "oudh",
  c(basic_controls, additional_controls_3)
)

healthcommittee1 <- reg_hc(fhealthcommittee1)
healthcommittee2 <- reg_hc(fhealthcommittee2)
healthcommittee3 <- reg_hc(fhealthcommittee3)