#### Load packages ####
library(haven)
library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)

#### Code ####
# Import datas
soil_climate <- read_dta("Datas/table1-soil-climate.dta")
pop <- read_dta("Datas/table1-population-and-census.dta")

# Define the variables for soil_climate
vars_soil_climate <- c(
  "lon", "lat", "alt", "rain", "soilblack", "soilalkaline",
  "soilalluvialriver", "soilred", "aquifermorethan150",
  "aquifer100and150", "phstrongalkali", "phslightalkali",
  "phneutral", "phslightacid", "topsoil25and50cm",
  "topsoil50and100cm", "topsoilmorethan300cm"
)

# Define the variables for pop
vars_pop <- c(
  "poverty", "literacyrate", "populationsize", "populationdensity",
  "fractionsc", "permanenthouse", "pervilelec",
  "pervilroad", "pervilhealth", "pervilschool", "ger"
)


regression <- function(vars, data) {
  results <- list()

  for (var in vars) {
    model <- lm(as.formula(paste(var, "~ op")), data = data)
    robust_result <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
    # Store the robust standard error
    results[[var]] <- data.frame(
      Landlord = robust_result[1, 1] + robust_result[2, 1],
      Nonlandlord = robust_result[1, 1],
      Robust_SE = robust_result[2, 2]
    )
  }

  # Combine results for vars
  results <- do.call(rbind, results)

  return(results)
}

sum_soil_climate <- regression(vars_soil_climate, soil_climate)
sum_pop <- regression(vars_pop, pop)

sum <- bind_rows(sum_soil_climate, sum_pop)

## Compute the full table
# Creating the stargazer output
stargazer(sum,
  type = "latex",
  title = "Combined Means and Robust Standard Errors",
  summary = FALSE,
  out = "Output/table1_reproduction.tex",
  label = "tab1"
)
# Clear
rm()
