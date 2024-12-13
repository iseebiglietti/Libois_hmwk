#Libraries
library(haven)
library(dplyr)
library(lmtest)
library(sandwich)
library(clubSandwich)
library(stargazer)

#### Code ####
# Import datas
table_3 <- read_dta("Datas/table3.dta")
pradhan <- read_dta("Datas/pradhan-election-table3.dta") %>%
  filter(reserv == 0)

# Functions
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
# Define the variables for datas
vars3 <- c("highparty1", "highparty12")
vars_pradhan <- c("pradhanhighcaste")

sum_table3 <- clustered_regression(vars3, table_3)
sum_pradhan <- clustered_regression(vars_pradhan, pradhan)

sum <- bind_rows(sum_table3, sum_pradhan)
## Compute the full table
# Creating the stargazer output
stargazer(sum,
  type = "latex", title = "Combined Means and Clustered Robust Standard Errors",
  summary = FALSE,
  out = "Output/table3_reproduction.tex",
  label = "tab3"
)

# Clear
rm()

