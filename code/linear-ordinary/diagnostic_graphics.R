# Load necessary libraries (assuming previous loads; add if needed)
library(tidyverse)
library(ggrepel)
library(viridis)
library(ggplot2)
library(usmap)
# Reload updated CSVs (assuming main() has been run previously)
ds <- read_csv(fs::path(here::here("data", "clean", "diagnostic_summary"), ext = "csv"))
inf <- read_csv(fs::path(here::here("data", "clean", "influence_measures"), ext = "csv"))
# Replace NA num_influential with 0
ds <- ds %>% mutate(num_influential = replace_na(num_influential, 0))
# Clamp slope p-values and compute -log10(p)
ds <- ds %>%
  mutate(
    slope_p_clamped = pmax(slope_p, 1e-12),
    neglog10_p = -log10(slope_p_clamped),
    slope_positive = slope_est > 0
  )
# Compute global summaries (for reference)
mean_r2 <- mean(ds$r_squared, na.rm = TRUE)
mean_slope_p <- mean(ds$slope_p, na.rm = TRUE)
mean_num_infl <- mean(ds$num_influential, na.rm = TRUE)
mean_rmse_cv <- mean(ds$rmse_cv, na.rm = TRUE)
pct_viol_norm <- mean(ds$shapiro_p < 0.05, na.rm = TRUE) * 100
cor_age_r2 <- cor(ds$age, ds$r_squared, use = "complete.obs")
# Figure: Distribution du R-carré (one graphic)
p_global_r2 <- ggplot(ds, aes(x = r_squared)) +
  geom_histogram(bins = 20, fill = "blue", alpha = 0.7) +
  geom_density(aes(y = after_stat(count)), color = "darkblue") +
  labs(title = "Distribution du R-carré", x = "R-carré", y = "Nombre") +
  theme_bw()
ggsave("fig_metrics_global_r2.png", p_global_r2, width = 6, height = 4)
# Figure: Distribution des valeurs p de la pente (one graphic)
p_global_p <- ggplot(ds, aes(x = slope_p)) +
  geom_histogram(bins = 20, fill = "red", alpha = 0.7) +
  geom_density(aes(y = after_stat(count)), color = "darkred") +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "black") +
  labs(title = "Distribution des valeurs p de la pente", x = "Valeur p", y = "Nombre") +
  theme_bw()
ggsave("fig_metrics_global_p.png", p_global_p, width = 6, height = 4)
# Figure: RMSE (CV) vs R-carré, with color by age and shape by sex (one graphic)
ds <- ds %>% mutate(sex_shape = ifelse(sex == "Male", "Homme", "Femme"))  # Translate sex for legend
p_val_comp <- ggplot(ds, aes(x = r_squared, y = rmse_cv, color = factor(age), shape = sex_shape)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = ifelse(slope_p < 0.05, paste(age, sex_shape, sep = "-"), "")), size = 3) +
  scale_shape_manual(values = c("Homme" = 17, "Femme" = 16)) +  # Triangle for Male, Circle for Female
  labs(title = "RMSE (CV) vs R-carré", x = "R-carré", y = "RMSE (CV à 10 plis)", color = "Âge", shape = "Sexe") +
  theme_bw()
ggsave("fig_validation_comparison.png", p_val_comp, width = 8, height = 6)
# Split trends into separate graphics
trends_data <- ds %>%
  pivot_longer(cols = c(r_squared, rmse_cv, num_influential), names_to = "metric", values_to = "value") %>%
  group_by(age, sex, metric) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), 
            sd_value = sd(value, na.rm = TRUE), 
            n_val = n(), .groups = "drop") %>%
  mutate(se = ifelse(n_val > 1, sd_value / sqrt(n_val), 0))
# Figure: Tendances du R-carré par âge et sexe (separate)
p_trends_r2 <- ggplot(filter(trends_data, metric == "r_squared"), aes(x = age, y = mean_value, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_value - se, ymax = mean_value + se, fill = sex), alpha = 0.2, color = NA) +
  labs(title = "Tendances du R-carré par âge et sexe", x = "Âge", y = "R-carré moyen", color = "Sexe", fill = "Sexe") +
  theme_bw()
ggsave("fig_trends_r_squared.png", p_trends_r2, width = 8, height = 5)
# Figure: Tendances du RMSE (CV) par âge et sexe (separate)
p_trends_rmse <- ggplot(filter(trends_data, metric == "rmse_cv"), aes(x = age, y = mean_value, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_value - se, ymax = mean_value + se, fill = sex), alpha = 0.2, color = NA) +
  labs(title = "Tendances du RMSE (CV) par âge et sexe", x = "Âge", y = "RMSE (CV) moyen", color = "Sexe", fill = "Sexe") +
  theme_bw()
ggsave("fig_trends_rmse_cv.png", p_trends_rmse, width = 8, height = 5)
# Figure: Tendances du nombre de points influents par âge et sexe (separate)
p_trends_infl <- ggplot(filter(trends_data, metric == "num_influential"), aes(x = age, y = mean_value, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean_value - se, ymax = mean_value + se, fill = sex), alpha = 0.2, color = NA) +
  labs(title = "Tendances du nombre de points influents par âge et sexe", x = "Âge", y = "Nombre moyen de points influents", color = "Sexe", fill = "Sexe") +
  theme_bw()
ggsave("fig_trends_num_influential.png", p_trends_infl, width = 8, height = 5)
# Figure: Distribution du nombre de valeurs aberrantes par âge et sexe (histogram/bar for outliers)
p_outliers <- ggplot(ds, aes(x = factor(age), y = num_outliers, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution du nombre de valeurs aberrantes par âge et sexe", x = "Âge", y = "Nombre de valeurs aberrantes", fill = "Sexe") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave("fig_outliers_distribution.png", p_outliers, width = 10, height = 6)
# Figure: Boîtes à moustaches des valeurs de levier par âge, faceté par sexe (leverage points)


p_leverage <- inf |> mutate(sex = ifelse(sex == "Male", "mâle", "femelle")) |> ggplot(aes(x = factor(age), y = leverage, fill = sex)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(inf$leverage_threshold, na.rm = TRUE), linetype = "dashed", color = "red") +
  facet_wrap(~ sex) +
  labs(title = "Boîtes à moustaches des valeurs de levier par âge et sexe", x = "Âge", y = "Valeur de levier", fill = "Sexe") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "")
ggsave("fig_leverage_boxplot.png", p_leverage, width = 10, height = 6)
# Figure: Boîtes à moustaches des distances de Cook par âge, faceté par sexe (influential points)
p_cooks <- inf |> mutate(sex = ifelse(sex == "Male", "mâle", "femelle")) |> ggplot(aes(x = factor(age), y = cooks_d, fill = sex)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(inf$cooks_threshold, na.rm = TRUE), linetype = "dashed", color = "red") +
  facet_wrap(~ sex) +
  labs(title = "Boîtes à moustaches des distances de Cook par âge et sexe", x = "Âge", y = "Distance de Cook", fill = "Sexe") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "")
ggsave("fig_cooks_distance_boxplot.png", p_cooks, width = 10, height = 6)
# Figure: Distribution géographique des leviers moyens (choropleth map, already in French)
agg_inf <- inf %>%
  group_by(location) %>%
  summarise(
    mean_leverage = mean(leverage, na.rm = TRUE),
    mean_cooks = mean(cooks_d, na.rm = TRUE),
    mean_std_resid = mean(abs(std_resid), na.rm = TRUE)
  ) %>%
  rename(state = location) %>%
  mutate(influence_global = ifelse(mean_cooks > 0.05, "Élevé", ifelse(mean_cooks > 0.02, "Modéré", "Faible")))
p_map <- plot_usmap(data = agg_inf, regions = "states", values = "mean_leverage", color = "black") +
  scale_fill_viridis_c(option = "plasma", name = "Levier moyen") +
  labs(title = "Distribution géographique des leviers moyens") +
  theme_bw() +
  theme(legend.position = "right")
ggsave("fig_regions_map_improved.png", p_map, width = 8, height = 5)
# Additional original plots translated to French
p1 <- ggplot(ds, aes(x = r_squared)) +
  geom_histogram(bins = 12, color = "black", alpha = 0.8) +
  geom_vline(xintercept = mean_r2, linetype = "dashed") +
  labs(title = "Distribution du R-carré", x = "R-carré", y = "Nombre de modèles âge-sexe") +
  theme_bw(base_size = 13)
ggsave("plot_r2_histogram.png", p1, width = 8, height = 4.5)
heat <- ds %>% group_by(sex, age) %>% summarize(r_squared = mean(r_squared, na.rm = TRUE), .groups = "drop")
p2 <- ggplot(heat, aes(x = factor(age), y = sex, fill = r_squared)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +
  labs(title = "Carte de chaleur : R-carré moyen par âge et sexe", x = "Âge", y = "Sexe", fill = "R-carré") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave("plot_r2_heatmap.png", p2, width = 10, height = 2.8)
pct_positive_slopes <- mean(ds$slope_est > 0, na.rm = TRUE) * 100
p3 <- ggplot(ds, aes(x = slope_est)) +
  geom_histogram(bins = 15, color = "black", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = sprintf("Distribution des estimations de pente (%.1f%% positives)", pct_positive_slopes), 
       x = "Estimation de pente (Δ prévalence / unité arsenic)", y = "Nombre de modèles âge-sexe") +
  theme_bw(base_size = 13)
ggsave("plot_slope_histogram.png", p3, width = 8, height = 4.5)
ds <- ds %>% mutate(sig_flag = if_else(slope_p < 0.05, "p < 0,05", "p >= 0,05"))
p4 <- ggplot(ds, aes(x = slope_est, y = neglog10_p)) +
  geom_point(aes(size = num_influential, shape = sig_flag), alpha = 0.5, color="purple") +
  scale_shape_manual(values = c(17, 16)) +
  scale_size_continuous(range = c(2, 8)) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  geom_text_repel(data = filter(ds, slope_p < 0.05), aes(label = paste0(age, "-", sex)), size = 3) +
  labs(title = "Pente vs signification", x = "Estimation de pente (Δ prévalence / unité arsenic)", 
       y = expression(-log[10](p["pente"])), size = "Nombre d'observations\ninfluentes", shape = "Signification") +
  theme_bw(base_size = 13)
ggsave("plot_slope_vs_significance.png", p4, width = 9, height = 6)