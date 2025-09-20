# Load necessary libraries
library(lmtest)    # For Breusch-Pagan, Durbin-Watson, and RESET tests
library(car)       # For outlierTest, Cook's distance, etc.
library(broom)     # For tidy model augmentation
library(tidyverse) # For data manipulation
library(fs)        # For file path utilities
library(here)      # For project-relative paths

# Load data with error handling
tryCatch({
  df_gbd <- read_csv(fs::path(here::here("data", "clean", "IHME-GBD_2021_CLEAN_prevalence.csv")))
}, error = function(e) {
  stop("Error loading GBD data: ", e$message)
})

tryCatch({
  df_epa <- read_csv(fs::path(here::here("data", "clean", "arsenic_pm2.5_locations.csv"))) |> 
    select(location, arsenic = mu)
}, error = function(e) {
  stop("Error loading EPA data: ", e$message)
})

# Extract unique ages and sexes, sorted for consistency
age_list <- sort(unique(df_gbd$age))
sex_list <- unique(df_gbd$sex)

# Initialize lists to collect results
diag_list <- list()
influence_list <- list()

# Loop over each combination of age and sex with progress indication
for (age_val in age_list) {
  for (sex_val in sex_list) {
    key <- paste(age_val, sex_val, sep = "_")
    cat("Processing:", key, "\n")
    
    # Filter and summarize df_gbd for the current age and sex
    df_temp <- df_gbd %>%
      filter(age == age_val, sex == sex_val) %>%
      group_by(location) %>%
      summarise(
        mu = mean(mu, na.rm = TRUE),
        sigma = sqrt(sum(sigma^2, na.rm = TRUE)) / n(),
        .groups = "drop"
      ) %>%
      filter(!is.na(mu) & !is.na(sigma))
    
    # Check if sufficient data is available
    if (nrow(df_temp) < 2) {
      cat("Skipping", key, ": Insufficient data\n")
      next
    }
    
    # Merge with df_epa and prepare regression data
    df_reg <- merge(df_temp, df_epa, by = "location") %>%
      rename(y = mu, x = arsenic) %>%
      select(location, x, y) %>%
      drop_na()
    
    # Check for minimum observations required for lm
    if (nrow(df_reg) < 3) {
      cat("Skipping", key, ": Too few observations for regression\n")
      next
    }
    
    # Fit the linear regression model with tryCatch for robustness
    model <- tryCatch({
      lm(y ~ x, data = df_reg)
    }, error = function(e) {
      cat("Error fitting model for", key, ":", e$message, "\n")
      NULL
    })
    
    if (is.null(model)) next
    
    # Get model summary
    sum_model <- summary(model)
    
    # Augment model for diagnostics
    aug_df <- broom::augment(model, data = df_reg)
    
    # Extract influence measures including DFFITS
    inf_measures <- influence.measures(model)
    infmat <- inf_measures$infmat
    
    # Perform diagnostic tests with tryCatch
    reset_test <- tryCatch(lmtest::resettest(model), error = function(e) list(p.value = NA))
    shapiro_test <- tryCatch(shapiro.test(residuals(model)), error = function(e) list(p.value = NA))
    bp_test <- tryCatch(lmtest::bptest(model), error = function(e) list(p.value = NA))
    dw_test <- tryCatch(lmtest::dwtest(model), error = function(e) list(statistic = NA, p.value = NA))
    outlier_test <- tryCatch(car::outlierTest(model), error = function(e) list(bonf.p = NA))
    
    # Calculate influential points
    cooks_dist <- cooks.distance(model)
    leverage_threshold <- 2 * (length(coef(model)) / nrow(df_reg))  # Common threshold for leverage
    cooks_threshold <- 4 / nrow(df_reg)
    influential <- which(cooks_dist > cooks_threshold | aug_df$.hat > leverage_threshold)
    
    # Collect diagnostic information into a tibble
    diag_row <- tibble(
      age = age_val,
      sex = sex_val,
      n_obs = nrow(df_reg),
      r_squared = sum_model$r.squared,
      adj_r_squared = sum_model$adj.r.squared,
      f_stat = sum_model$fstatistic[1],
      f_pvalue = pf(sum_model$fstatistic[1], sum_model$fstatistic[2], sum_model$fstatistic[3], lower.tail = FALSE),
      intercept_est = coef(model)[1],
      intercept_p = sum_model$coefficients[1, 4],
      slope_est = coef(model)[2],
      slope_p = sum_model$coefficients[2, 4],
      reset_p = reset_test$p.value,
      shapiro_p = shapiro_test$p.value,
      bp_p = bp_test$p.value,
      dw_stat = dw_test$statistic,
      dw_p = dw_test$p.value,
      num_outliers = if (all(is.na(outlier_test$bonf.p))) 0 else sum(outlier_test$bonf.p < 0.05, na.rm = TRUE),
      num_influential = length(influential)
    )
    
    # Append to diagnostic list
    diag_list[[key]] <- diag_row
    
    # Collect influence measures, including additional metrics
    inf_df <- aug_df %>%
      select(location, leverage = .hat, cooks_d = .cooksd, std_resid = .std.resid) %>%
      mutate(
        dffits = infmat[, "dffit"],
        age = age_val,
        sex = sex_val,
        is_influential = row_number() %in% influential,
        leverage_threshold = leverage_threshold,
        cooks_threshold = cooks_threshold
      )
    
    # Append to influence list
    influence_list[[key]] <- inf_df
  }
}

# Combine into final dataframes
diag_df <- bind_rows(diag_list)
influence_df <- bind_rows(influence_list)

# Output the dataframes (for inspection or saving)
print(diag_df)
print(influence_df)

# Optionally save to CSV for persistence
write_csv(diag_df, "diagnostic_summary.csv")
write_csv(influence_df, "influence_measures.csv")
