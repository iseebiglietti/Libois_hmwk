#### Load packages ###
library(haven)
library(dplyr)
library(lmtest)
library(sandwich)
library(clubSandwich)
library(stargazer)
#Robust stuff

library(MASS)       # Pour LAD
library(quantreg)   # Pour LAD
library(robustbase) # Pour M-, S-, et MM-estimations
#VIF stuff
library(car) # Pour la fonction vif()

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


# Fonction pour détecter et retirer les colonnes parfaitement collinéaires
detect_and_remove_collinear <- function(data, formula, remove) {
  if (remove) {
    data <- data %>% na.omit()
    message("Les lignes contenant des NA ont été supprimées.")
  }
  # Générer la matrice de conception à partir de la formule
  design_matrix <- model.matrix(formula, data = data)

  # Vérifier le rang de la matrice
  qr_decomp <- qr(design_matrix)
  rank <- qr_decomp$rank

  # Identifier les colonnes linéairement dépendantes
  if (ncol(design_matrix) > rank) {
    # Colonnes à exclure (colonnages QR, pivotement)
    collinear_cols <- setdiff(seq_len(ncol(design_matrix)), qr_decomp$pivot[1:rank])

    # Obtenir les noms des colonnes problématiques
    collinear_names <- colnames(design_matrix)[collinear_cols]

    # Retirer les colonnes problématiques du dataset
    cleaned_data <- data %>% dplyr::select(-all_of(collinear_names))

    # Mettre à jour la formule pour exclure les colonnes problématiques
    cleaned_formula <- update(formula, paste(".~.-", paste(collinear_names, collapse = " - ")))

    # Afficher un message des colonnes retirées
    message("Colonnes retirées pour collinéarité parfaite : ", paste(collinear_names, collapse = ", "))

    return(list(data = cleaned_data, formula = cleaned_formula))
  } else {
    message("Aucune collinéarité parfaite détectée.")
    return(list(data = data, formula = formula))  # Pas de changement
  }
}


reg_cleaned_ols <- function(formula, data) {
  model <- lm(formula, data = data)
  return(model)
}

reg_ols <- function(formula, data){
  result <- detect_and_remove_collinear(data, formula, FALSE)
  cleaned_table <- result$data
  cleaned_formula <- result$formula
  return(reg_cleaned_ols(cleaned_formula, cleaned_table))
}

reg_cleaned_lad <- function(formula, data) {
  model <- rq(formula, data = data, tau = 0.5) # tau = 0.5 pour la médiane
  return(summary(model))
}

reg_lad <- function(formula, data){
  result <- detect_and_remove_collinear(data, formula, FALSE)
  cleaned_table <- result$data
  cleaned_formula <- result$formula
  return(reg_cleaned_lad(cleaned_formula, cleaned_table))
}

reg_cleaned_lts <- function(formula, data) {
  model <- ltsReg(formula, data = data)
  return(summary(model))
}

reg_lts <- function(formula, data){
  result <- detect_and_remove_collinear(data, formula, TRUE)
  cleaned_table <- result$data
  cleaned_formula <- result$formula
  return(reg_cleaned_lts(cleaned_formula, cleaned_table))
}

reg_cleaned_m <- function(formula, data) {
  model <- rlm(formula, data = data, maxit = 1000)
  return(summary(model))
}

reg_m <- function(formula, data){
  result <- detect_and_remove_collinear(data, formula, FALSE)
  cleaned_table <- result$data
  cleaned_formula <- result$formula
  return(reg_cleaned_m(cleaned_formula, cleaned_table))
}

reg_cleaned_mm <- function(formula, data) {
  model <- lmrob(formula, data = data, maxit.scale = 1000)
  return(summary(model))
}

reg_mm <- function(formula, data){
  result <- detect_and_remove_collinear(data, formula, TRUE)
  cleaned_table <- result$data
  cleaned_formula <- result$formula
  return(reg_cleaned_mm(cleaned_formula, cleaned_table))
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
additional_controls_4 <- c(
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
  c(basic_controls, additional_controls_4)
)

#Robust regressions
#VIF study, I guess this interesting because it seems that columns are highly correlated
# Ajuster un modèle de régression linéaire
lm_model <- reg_ols(fgsmeet1, table4) # Utilisation de toutes les variables prédictives pour prédire fgsmeet1

# Calculer les VIF pour chaque prédicteur
vif_results <- vif(lm_model)

# Afficher les résultats du VIF
print(vif_results)

# Interpréter les résultats
# Si un VIF est supérieur à 5 (ou 10), cela indique une collinéarité élevée avec d'autres variables




# 1. LAD regressions
lad_gsmeet1 <- reg_lad(fgsmeet1, table4)
lad_gsmeet2 <- reg_lad(fgsmeet2, table4)
lad_gsmeet3 <- reg_lad(fgsmeet3, table4)

lad_ssmeet1 <- reg_lad(fssmeet1, table4)
lad_ssmeet2 <- reg_lad(fssmeet2, table4)
lad_ssmeet3 <- reg_lad(fssmeet3, table4)

lad_ptameet1 <- reg_lad(fptameet1, table4)
lad_ptameet2 <- reg_lad(fptameet2, table4)
lad_ptameet3 <- reg_lad(fptameet3, table4)

lad_healthcommittee1 <- reg_lad(fhealthcommittee1, hc)
lad_healthcommittee2 <- reg_lad(fhealthcommittee2, hc)
lad_healthcommittee3 <- reg_lad(fhealthcommittee3, hc)

#2.LTS models
lts_gsmeet1 <- reg_lts(fgsmeet1, table4)
lts_gsmeet2 <- reg_lts(fgsmeet2, table4)
lts_gsmeet3 <- reg_lts(fgsmeet3, table4)

lts_ssmeet1 <- reg_lts(fssmeet1, table4)
lts_ssmeet2 <- reg_lts(fssmeet2, table4)
lts_ssmeet3 <- reg_lts(fssmeet3, table4)

lts_ptameet1 <- reg_lts(fptameet1, table4)
lts_ptameet2 <- reg_lts(fptameet2, table4)
lts_ptameet3 <- reg_lts(fptameet3, table4)

lts_healthcommittee1 <- reg_lts(fhealthcommittee1, hc)
lts_healthcommittee2 <- reg_lts(fhealthcommittee2, hc)
lts_healthcommittee3 <- reg_lts(fhealthcommittee3, hc)


#3 M-Estimator
m_gsmeet1 <- reg_m(fgsmeet1, table4)
m_gsmeet2 <- reg_m(fgsmeet2, table4)
m_gsmeet3 <- reg_m(fgsmeet3, table4)

m_ssmeet1 <- reg_m(fssmeet1, table4)
m_ssmeet2 <- reg_m(fssmeet2, table4)
m_ssmeet3 <- reg_m(fssmeet3, table4)

m_ptameet1 <- reg_m(fptameet1, table4)
m_ptameet2 <- reg_m(fptameet2, table4)
m_ptameet3 <- reg_m(fptameet3, table4)

m_healthcommittee1 <- reg_m(fhealthcommittee1, hc)
m_healthcommittee2 <- reg_m(fhealthcommittee2, hc)
m_healthcommittee3 <- reg_m(fhealthcommittee3, hc)

#MM-Estimation
mm_model <- reg_mm(fgsmeet1,table4)

