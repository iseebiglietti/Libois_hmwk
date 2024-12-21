# Charger les bibliothèques nécessaires
library(dplyr)
library(moments)
library(stargazer)
library(haven)

table4 <- as.data.frame(read_dta("Datas/table4.dta"))
hc <- as.data.frame(read_dta("Datas/table4-health-committee.dta"))
teacher <- as.data.frame(read_dta("Datas/table-5-teacher.dta")) %>%
  mutate(across(where(is.character), ~ as.numeric(.)))
stipend <- as.data.frame(read_dta("Datas/table-5-stipend.dta")) %>%
  mutate(across(where(is.character), ~ as.numeric(.)))
infra <- as.data.frame(read_dta("Datas/table-5-infrastructure.dta"))
score <- as.data.frame(read_dta("Datas/table-5-student-score-attendance.dta")) %>%
  mutate(across(where(is.character), ~ as.numeric(.)))



# Génère le résumé des statistiques
# Génère le résumé des statistiques avec deux chiffres significatifs
stats <- function(data) {
  summary <- data %>%
    reframe(
      var = colnames(.),
      median = sapply(., function(x) signif(median(x, na.rm = TRUE), 2)),
      mean = sapply(., function(x) signif(mean(x, na.rm = TRUE), 2)),
      min = sapply(., function(x) signif(min(x, na.rm = TRUE), 2)),
      max = sapply(., function(x) signif(max(x, na.rm = TRUE), 2)),
      sd = sapply(., function(x) signif(sd(x, na.rm = TRUE), 2)),
      iqr = sapply(., function(x) signif(IQR(x, na.rm = TRUE), 2)),
      skewness = sapply(., function(x) signif(skewness(x, na.rm = TRUE), 2)),
      kurtosis = sapply(., function(x) signif(kurtosis(x, na.rm = TRUE), 2)),
      non_missing_pct = sapply(., function(x) signif(mean(!is.na(x)), 2))
    )
  return(summary)
}

# Appliquer la fonction sur votre dataframe
stats_table4 <- stats(table4)
stats_hc <- stats(hc)
stats_teacher <- stats(teacher)
stats_stipend <- stats(stipend)
stats_infra <- stats(infra)
stats_score <- stats(score)

result <- bind_rows(
  stats_table4, stats_hc, stats_teacher, stats_stipend,
  stats_infra, stats_score
) %>%
  distinct(var, .keep_all = TRUE)

# Créer une table dense avec stargazer
stargazer(stats_table4, type = "html", summary = FALSE, rownames = FALSE, digits = 2, title = "Robust Stats Table 4", out = "summary_statistics_table4.html")
stargazer(stats_hc, type = "html", summary = FALSE, rownames = FALSE, digits = 2, title = "Robust Health Committee", out = "summary_statistics_hc.html")
stargazer(stats_teacher, type = "html", summary = FALSE, rownames = FALSE, digits = 2, title = "Robust Teacher", out = "summary_statistics_teacher.html")
stargazer(stats_stipend, type = "html", summary = FALSE, rownames = FALSE, digits = 2, title = "Robust Stipend", out = "summary_statistics_stipend.html")
stargazer(stats_infra, type = "html", summary = FALSE, rownames = FALSE, digits = 2, title = "Robust Infra", out = "summary_statistics_infra.html")
stargazer(stats_score, type = "html", summary = FALSE, rownames = FALSE, digits = 2, title = "Robust Score", out = "summary_statistics_score.html")
stargazer(result, type = "html", summary = FALSE, rownames = FALSE, digits = 2, title = "Robust Statistics", out = "summary_statistics.html")
