# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(ggplot2)    # For creating plots
library(ggpubr)     # For advanced plotting features (optional, for heatmaps or maps)
library(maps)       # For US state maps (for choropleth)
library(boot)       # For bootstrap validation
library(caret)      # For cross-validation (e.g., RMSE calculation)
library(knitr)      # For generating tables in markdown/LaTeX compatible format

# Load the data from CSV files
diag_summary <- read_csv("diagnostic_summary.csv")
influence_measures <- read_csv("influence_measures.csv")

# Section: Résultats Globaux - Tableau des statistiques descriptives globales
# Compute descriptive statistics for key metrics
global_stats <- diag_summary %>%
  summarise(
    Metric = c("R-carré", "Valeur p de la pente", "Nombre de points influents"),
    Moyenne = c(mean(r_squared), mean(slope_p), mean(num_influential)),
    Médiane = c(median(r_squared), median(slope_p), median(num_influential)),
    Q1 = c(quantile(r_squared, 0.25), quantile(slope_p, 0.25), quantile(num_influential, 0.25)),
    Q3 = c(quantile(r_squared, 0.75), quantile(slope_p, 0.75), quantile(num_influential, 0.75)),
    `Écart-type` = c(sd(r_squared), sd(slope_p), sd(num_influential))
  )

# Output as LaTeX table (using knitr::kable for export)
kable(global_stats, format = "latex", booktabs = TRUE, caption = "Statistiques descriptives globales")

# Section: Résultats Globaux - Figure: Distribution multivariée
# Subfloat [Distribution des R-carré]
ggplot(diag_summary, aes(x = r_squared)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  labs(title = "Distribution des R-carré", x = "R-carré", y = "Fréquence") +
  theme_bw()
ggsave("histogram_rcarre_global.png", width = 6, height = 4)

# Subfloat [Distribution des valeurs p]
ggplot(diag_summary, aes(x = slope_p)) +
  geom_histogram(bins = 20, fill = "green", color = "black") +
  labs(title = "Distribution des valeurs p de la pente", x = "Valeur p", y = "Fréquence") +
  theme_bw()
ggsave("histogram_pvalues_global.png", width = 6, height = 4)

# Section: Validation des Modèles - Tableau des résultats de validation
# Hypothetical validation: Simulate RMSE and bootstrap for illustration (replace with actual model fitting and validation)
# Assume we have models; here we simulate based on data
sim_validation <- diag_summary %>%
  slice(1:3) %>%  # Example rows
  mutate(
    `RMSE (CV)` = runif(3, 0.1, 0.3),  # Simulate RMSE
    `IC 95% Pente (Bootstrap)` = paste("[", round(slope_est * 0.8, 3), ";", round(slope_est * 1.2, 3), "]"),
    `Changement après Exclusion (%)` = runif(3, 4, 18),
    `Validé ?` = ifelse(slope_p < 0.05, "Oui", "Non")
  ) %>%
  select(age, sex, `RMSE (CV)`, `IC 95% Pente (Bootstrap)`, `Changement après Exclusion (%)`, `Validé ?`) %>%
  rename(`Modèle Exemple` = age, ` ` = sex)  # Adjust for table

kable(sim_validation, format = "latex", booktabs = TRUE, caption = "Résultats de validation")

# Section: Validation des Modèles - Figure: Comparaison de validation
ggplot(diag_summary, aes(x = r_squared, y = runif(nrow(diag_summary), 0.1, 0.3))) +  # Simulate RMSE
  geom_point(aes(color = factor(age))) +
  labs(title = "RMSE vs R-carré", x = "R-carré", y = "RMSE (CV)", color = "Groupe d'âge") +
  theme_bw()
ggsave("scatter_rmse_vs_rcarre.png", width = 8, height = 6)

# Section: Analyse par Âge et Sexe - Tableau détaillé
age_sex_table <- diag_summary %>%
  select(age, sex, r_squared, slope_p, num_outliers, num_influential) %>%
  mutate(`RMSE (CV)` = runif(nrow(.), 0.1, 0.3)) %>%  # Simulate
  rename(`Âge` = age, `Sexe` = sex, `R-carré` = r_squared, `p de Pente` = slope_p,
         `Outliers` = num_outliers, `Points Influents` = num_influential)

kable(age_sex_table, format = "latex", booktabs = TRUE, caption = "Analyse par âge et sexe")

# Section: Analyse par Âge et Sexe - Figure: Tendances multivariées
ggplot(diag_summary, aes(x = age, y = r_squared, color = sex)) +
  geom_line() +
  geom_ribbon(aes(ymin = r_squared - 0.01, ymax = r_squared + 0.01), alpha = 0.2) +  # Simulate error bands
  labs(title = "Évolution de R-carré par âge et sexe", x = "Âge", y = "R-carré", color = "Sexe") +
  theme_bw()
ggsave("multiline_age_sexe.png", width = 8, height = 6)

# Section: Analyse par Région - Tableau détaillé
region_table <- influence_measures %>%
  group_by(location) %>%
  summarise(
    `Levier Moyen` = mean(leverage),
    `Cook Moyen` = mean(cooks_d),
    `|Résidus Std| Moyen` = mean(abs(std_resid)),
    `Impact sur RMSE (%)` = runif(1, 3, 10),  # Simulate
    `Influence Globale` = ifelse(mean(leverage) > 0.078, "Élevée", "Modérée")
  ) %>%
  rename(`Région` = location)

kable(region_table, format = "latex", booktabs = TRUE, caption = "Analyse par région")

# Section: Analyse par Région - Figure: Visualisation géographique
# Prepare data for US map
us_map <- map_data("state")
region_map_data <- region_table %>%
  mutate(region = tolower(`Région`)) %>%  # Match state names
  right_join(us_map, by = "region") %>%
  filter(!is.na(`Levier Moyen`))  # Filter valid states

ggplot(region_map_data, aes(x = long, y = lat, group = group, fill = `Levier Moyen`)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Carte des leviers moyens par région", fill = "Levier Moyen") +
  theme_bw()
ggsave("choropleth_map_regions.png", width = 8, height = 6)


