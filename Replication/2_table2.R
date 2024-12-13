# Charger les bibliothèques nécessaires
library(haven) # pour lire le fichier .dta
library(lmtest)
library(sandwich)
library(clubSandwich) # pour générer les SE robustes
library(dplyr)
library(stargazer)

# Charger les données
villages <- read_dta("Datas/villagecharacterisitcs-table2.dta")
students <- read_dta("Datas/studentcharacterisitcs-table2.dta")
teachers <- read_dta("Datas/teachercharacterisitcs-table2.dta")


# Specify all variables in all datasets

vars_teachers <- c("tcaste3", "tcaste2", "tcaste", "edu", "dissch", "tgen")
vars_students <- c("studenthighcaste", "studentobccaste", "studentsccaste", "fatherprimaryedu", "motherprimaryedu", "studentgender")
vars_villages <- c("elec", "phone", "gpdistroad", "gpsl", "density", "lit", "frsc", "frobc")


clustered_regression <- function(vars, data) {
  results <- list()

  for (var in vars) {
    model <- lm(as.formula(paste(var, "~ op")), data = data)
    robust_result <- coeftest(model, vcov = vcovCL, cluster = ~districtid)
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

sum_villages <- clustered_regression(vars_villages, villages)
sum_teachers <- clustered_regression(vars_teachers, teachers)
sum_students <- clustered_regression(vars_students, students)

sum <- bind_rows(sum_villages, sum_teachers, sum_students)

## Compute the full table
# Creating the stargazer output
stargazer(sum,
  type = "latex",
  title = "Combined Means and Robust Clustered Standard Errors",
  summary = FALSE,
  out = "Output/table2_reproduction.tex",
  label = "tab2"
)


# Clear
rm()
