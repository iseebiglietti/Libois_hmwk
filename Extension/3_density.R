# Charger les bibliothèques nécessaires
library(dplyr)
library(moments)
library(stargazer)
library(haven)
library(ggplot2)
library(tidyr)

table4 <- as.data.frame(read_dta("Datas/table4.dta"))
hc <- as.data.frame(read_dta("Datas/table4-health-committee.dta"))
teacher <- as.data.frame(read_dta("Datas/table-5-teacher.dta")) %>%
  mutate(across(where(is.character), ~ as.numeric(.)))
stipend <- as.data.frame(read_dta("Datas/table-5-stipend.dta")) %>%
  mutate(across(where(is.character), ~ as.numeric(.)))
infra <- as.data.frame(read_dta("Datas/table-5-infrastructure.dta"))
score <- as.data.frame(read_dta("Datas/table-5-student-score-attendance.dta")) %>%
  mutate(across(where(is.character), ~ as.numeric(.)))



# Préparer les données en format long pour ggplot
df_long <- table4 %>%
  select(alt, density, frobc, frsc, lat, lit, rain, totpop) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Créer les graphiques de densité
ggplot(df_long, aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ variable, scales = "free", ncol = 3) + # Organiser en une grille avec 3 colonnes
  labs(title = "Densités des variables continues Table 4", x = "Valeur", y = "Densité") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Style des titres des facettes
    plot.title = element_text(hjust = 0.5, face = "bold") # Centrer le titre
  )

ggsave("OUTPUT/DENSITY/plot_table4_density.png", plot = last_plot(), width = 8, height = 8, dpi = 300)


# Préparer les données en format long pour ggplot
df_long <- table4 %>%
  select(gsmeet, ptameet, ssmeet, elec, phone, op) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Créer les graphiques en barres
ggplot(df_long, aes(x = factor(value))) +
  geom_bar(fill = "blue", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free", ncol = 3) + # Organiser en grille avec 3 colonnes
  labs(title = "Distribution des variables discrètes Table 4", x = "Valeur", y = "Fréquence") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Style des titres des facettes
    plot.title = element_text(hjust = 0.5, face = "bold") # Centrer le titre
  )

ggsave("OUTPUT/DENSITY/plot_table4_bar.png", plot = last_plot(), width = 8, height = 8, dpi = 300)

# Préparer les données en format long pour ggplot
df_long <- hc %>%
  select(oudh, healthcommittee) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Créer les graphiques en barres
ggplot(df_long, aes(x = factor(value))) +
  geom_bar(fill = "blue", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free", ncol = 3) + # Organiser en grille avec 3 colonnes
  labs(title = "Distribution des variables discrètes Table HC", x = "Valeur", y = "Fréquence") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Style des titres des facettes
    plot.title = element_text(hjust = 0.5, face = "bold") # Centrer le titre
  )

ggsave("OUTPUT/DENSITY/plot_hc_density.png", plot = last_plot(), width = 8, height = 4, dpi = 300)

# Préparer les données en format long pour ggplot
df_long <- teacher %>%
  select(edu, tcaste, tcaste2, tgen, index) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Créer les graphiques en barres
ggplot(df_long, aes(x = factor(value))) +
  geom_bar(fill = "blue", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free", ncol = 3) + # Organiser en grille avec 3 colonnes
  labs(title = "Distribution des variables discrètes", x = "Valeur", y = "Fréquence") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Style des titres des facettes
    plot.title = element_text(hjust = 0.5, face = "bold") # Centrer le titre
  )

# Préparer les données en format long pour ggplot
df_long <- teacher %>%
  select(dissch, tactivity, tattendance, toenr) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Créer les graphiques de densité
ggplot(df_long, aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ variable, scales = "free", ncol = 3) + # Organiser en une grille avec 3 colonnes
  labs(title = "Densités des variables continues", x = "Valeur", y = "Densité") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Style des titres des facettes
    plot.title = element_text(hjust = 0.5, face = "bold") # Centrer le titre
  )

# Préparer les données en format long pour ggplot
df_long <- stipend %>%
  select(hhno, kid) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Créer les graphiques en barres
ggplot(df_long, aes(x = factor(value))) +
  geom_bar(fill = "blue", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free", ncol = 3) + # Organiser en grille avec 3 colonnes
  labs(title = "Distribution des variables discrètes", x = "Valeur", y = "Fréquence") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Style des titres des facettes
    plot.title = element_text(hjust = 0.5, face = "bold") # Centrer le titre
  )

# Préparer les données en format long pour ggplot
df_long <- stipend %>%
  select(scholar) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Créer les graphiques de densité
ggplot(df_long, aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ variable, scales = "free", ncol = 3) + # Organiser en une grille avec 3 colonnes
  labs(title = "Densités des variables continues", x = "Valeur", y = "Densité") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Style des titres des facettes
    plot.title = element_text(hjust = 0.5, face = "bold") # Centrer le titre
  )


# Préparer les données en format long pour ggplot
df_long <- score %>%
  select(me2, me3, fe2, fe3) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Créer les graphiques en barres
ggplot(df_long, aes(x = factor(value))) +
  geom_bar(fill = "blue", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free", ncol = 3) + # Organiser en grille avec 3 colonnes
  labs(title = "Distribution des variables discrètes", x = "Valeur", y = "Fréquence") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Style des titres des facettes
    plot.title = element_text(hjust = 0.5, face = "bold") # Centrer le titre
  )


# Préparer les données en format long pour ggplot
df_long <- score %>%
  select(meanscore, score, studentattendance, tepupr) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Créer les graphiques de densité
ggplot(df_long, aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ variable, scales = "free", ncol = 3) + # Organiser en une grille avec 3 colonnes
  labs(title = "Densités des variables continues", x = "Valeur", y = "Densité") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Style des titres des facettes
    plot.title = element_text(hjust = 0.5, face = "bold") # Centrer le titre
  )
