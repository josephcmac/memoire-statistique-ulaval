# plots_weak_but_informative.R
# Required packages: tidyverse, ggrepel, viridis (install if needed)
# install.packages(c("tidyverse", "ggrepel", "viridis"))

library(tidyverse)
library(ggrepel)
library(viridis)

# --- Load data --------------------------------------------------------------
ds_path <- "diagnostic_summary.csv"
inf_path <- "influence_measures.csv"

ds <- read_csv(ds_path, show_col_types = FALSE)
inf <- read_csv(inf_path, show_col_types = FALSE)

# --- Basic column safety & derived columns -----------------------------------
# Ensure age is integer
if ("age" %in% names(ds)) ds <- ds %>% mutate(age = as.integer(age))
if ("age" %in% names(inf)) inf <- inf %>% mutate(age = as.integer(age))

# Ensure key columns exist
required_ds_cols <- c("age", "sex", "r_squared", "slope_est", "slope_p", "num_influential")
missing_ds <- setdiff(required_ds_cols, names(ds))
if (length(missing_ds) > 0) {
  message("Warning: diagnostic_summary.csv missing columns: ", paste(missing_ds, collapse = ", "))
}

# Replace NA num_influential with 0 for plotting
if (!"num_influential" %in% names(ds)) {
  ds <- ds %>% mutate(num_influential = 0)
} else {
  ds <- ds %>% mutate(num_influential = replace_na(num_influential, 0))
}

# Clamp slope p-values to avoid -Inf when p == 0; compute -log10(p)
ds <- ds %>%
  mutate(
    slope_p_clamped = pmax(slope_p, 1e-12),
    neglog10_p = -log10(slope_p_clamped),
    slope_positive = slope_est > 0
  )

# Basic summary numbers printed to console
mean_r2 <- mean(ds$r_squared, na.rm = TRUE)
pct_r2_lt_0_05 <- mean(ds$r_squared < 0.05, na.rm = TRUE) * 100
pct_positive_slopes <- mean(ds$slope_est > 0, na.rm = TRUE) * 100
n_significant <- sum(ds$slope_p < 0.05, na.rm = TRUE)

cat(sprintf("Models: %d rows\n", nrow(ds)))
cat(sprintf("Mean R² = %.3f  |  %.1f%% of models have R² < 0.05\n", mean_r2, pct_r2_lt_0_05))
cat(sprintf("%.1f%% of slopes are positive  |  %d significant slopes (p < 0.05)\n\n",
            pct_positive_slopes, n_significant))

# --- Plot 1: Histogram of R-squared -----------------------------------------
p1 <- ggplot(ds, aes(x = r_squared)) +
  geom_histogram(bins = 12, color = "black", alpha = 0.8) +
  geom_vline(xintercept = mean_r2, linetype = "dashed") +
  labs(
    x = expression(R^2),
    y = "Count of age-sex models",
    title = sprintf("Distribution of R-squared (mean = %.3f; %.1f%% < 0.05)", mean_r2, pct_r2_lt_0_05)
  ) +
  theme_bw(base_size = 13)

ggsave("plot_r2_histogram.png", p1, width = 8, height = 4.5, dpi = 300)

# --- Plot 2: Heatmap of mean R-squared by age and sex ------------------------
# compute mean R^2 per age-sex (in case of duplicates)
heat <- ds %>%
  group_by(sex, age) %>%
  summarize(r_squared = mean(r_squared, na.rm = TRUE), .groups = "drop") %>%
  drop_na(age)

# ensure ages are ordered
heat <- heat %>% mutate(age = as.integer(age))

p2 <- ggplot(heat, aes(x = factor(age, levels = sort(unique(age))), y = sex, fill = r_squared)) +
  geom_tile(color = NA) +
  scale_fill_viridis_c(option = "magma", na.value = "grey90") +
  labs(x = "Age", y = "Sex", fill = expression(R^2),
       title = "Heatmap: mean R-squared by age (columns) and sex (rows)") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("plot_r2_heatmap.png", p2, width = 10, height = 2.8, dpi = 300)

# --- Plot 3: Histogram of slope estimates (vertical line at zero) ------------
p3 <- ggplot(ds, aes(x = slope_est)) +
  geom_histogram(bins = 15, color = "black", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Slope estimate (Δ prevalence / unit arsenic)",
       y = "Count of age-sex models",
       title = sprintf("Distribution of slope estimates (%.1f%% positive)", pct_positive_slopes)) +
  theme_bw(base_size = 13)

ggsave("plot_slope_histogram.png", p3, width = 8, height = 4.5, dpi = 300)

# --- Plot 4: Scatter slope vs -log10(p) sized by num_influential -------------
# Prepare shape mapping: significant vs not
ds <- ds %>%
  mutate(sig_flag = if_else(slope_p < 0.05, "p < 0.05", "p >= 0.05"))

p4 <- ggplot(ds, aes(x = slope_est, y = neglog10_p)) +
  geom_point(aes(size = num_influential, shape = sig_flag), alpha = 0.8) +
  scale_shape_manual(values = c("p < 0.05" = 17, "p >= 0.05" = 16)) +
  scale_size_continuous(range = c(2, 8)) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "gray30") +
  labs(x = "Slope estimate (Δ prevalence / unit arsenic)",
       y = expression(-log[10](p["slope"])),
       size = "Number of influential\nobservations",
       shape = "Significance",
       title = sprintf("Slope vs significance (n significant = %d)", n_significant)) +
  theme_bw(base_size = 13)

# Annotate only the significant models with age-sex labels (avoid clutter)
sig_labels <- ds %>% filter(slope_p < 0.05 & !is.na(age) & !is.na(sex))
if (nrow(sig_labels) > 0) {
  p4 <- p4 + geom_text_repel(
    data = sig_labels,
    aes(label = paste0(age, "-", sex)),
    size = 3,
    nudge_x = 0.01,
    segment.size = 0.3
  )
}

ggsave("plot_slope_vs_significance.png", p4, width = 9, height = 6, dpi = 300)

# --- Print file names and final message -------------------------------------
cat("Saved 4 plots:\n")
cat(" - plot_r2_histogram.png\n")
cat(" - plot_r2_heatmap.png\n")
cat(" - plot_slope_histogram.png\n")
cat(" - plot_slope_vs_significance.png\n\n")

cat("Notes / interpretation hints:\n")
cat(" - Histogram of R^2 shows many models with near-zero explanatory power (weak).\n")
cat(" - Heatmap localizes stronger fits to particular ages/sexes (informative pockets).\n")
cat(" - Slope histogram shows consistent positive directionality across strata (informative).\n")
cat(" - Scatter shows which models are statistically persuasive and which are influenced by many observations.\n")

