# Load necessary libraries
library(lmtest)    # For Breusch-Pagan, Durbin-Watson, and RESET tests
library(car)       # For outlierTest, Cook's distance, etc.
library(broom)     # For tidy model augmentation
library(tidyverse) # For data manipulation
library(fs)        # For file path utilities
library(here)      # For project-relative paths

process_comb <- function(df_gbd, df_epa, age_val, sex_val) {
  
  prepare_df_temp <- function(df) {
    df %>%
      dplyr::filter(age == age_val, sex == sex_val) %>%
      dplyr::group_by(location) %>%
      dplyr::summarise(
        mu = mean(mu, na.rm = TRUE),
        sigma = sqrt(sum(sigma^2, na.rm = TRUE)) / n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(mu) & !is.na(sigma))
  }
  
  prepare_df_reg <- function(df_temp) {
    merge(df_temp, df_epa, by = "location") %>%
      dplyr::rename(y = mu, x = arsenic) %>%
      dplyr::select(location, x, y) %>%
      tidyr::drop_na()
  }
  
  fit_model <- function(df_reg) {
    tryCatch(lm(y ~ x, data = df_reg), error = function(e) NULL)
  }
  
  compute_tests <- function(model) {
    list(
      reset_test = tryCatch(lmtest::resettest(model), error = function(e) list(p.value = NA)),
      shapiro_test = tryCatch(shapiro.test(residuals(model)), error = function(e) list(p.value = NA)),
      bp_test = tryCatch(lmtest::bptest(model), error = function(e) list(p.value = NA)),
      dw_test = tryCatch(lmtest::dwtest(model), error = function(e) list(statistic = NA, p.value = NA)),
      outlier_test = tryCatch(car::outlierTest(model), error = function(e) list(bonf.p = NA))
    )
  }
  
  compute_influential <- function(model, aug_df, n_obs) {
    cooks_dist <- cooks.distance(model)
    leverage_threshold <- 2 * (length(coef(model)) / n_obs)
    cooks_threshold <- 4 / n_obs
    influential <- which(cooks_dist > cooks_threshold | aug_df$.hat > leverage_threshold)
    list(
      influential = influential,
      leverage_threshold = leverage_threshold,
      cooks_threshold = cooks_threshold
    )
  }
  
  create_diag_row <- function(model, sum_model, tests, num_outliers, num_influential) {
    tibble::tibble(
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
      reset_p = tests$reset_test$p.value,
      shapiro_p = tests$shapiro_test$p.value,
      bp_p = tests$bp_test$p.value,
      dw_stat = tests$dw_test$statistic,
      dw_p = tests$dw_test$p.value,
      num_outliers = num_outliers,
      num_influential = num_influential
    )
  }
  
  create_inf_df <- function(aug_df, infmat, influential, leverage_threshold, cooks_threshold) {
    aug_df %>%
      dplyr::select(location, leverage = .hat, cooks_d = .cooksd, std_resid = .std.resid) %>%
      dplyr::mutate(
        dffits = infmat[, "dffit"],
        age = age_val,
        sex = sex_val,
        is_influential = row_number() %in% influential,
        leverage_threshold = leverage_threshold,
        cooks_threshold = cooks_threshold
      )
  }
  
  null_result <- list(diag = NULL, inf = NULL)
  
  df_temp <- prepare_df_temp(df_gbd)
  if (nrow(df_temp) < 2) return(null_result)
  
  df_reg <- prepare_df_reg(df_temp)
  if (nrow(df_reg) < 3) return(null_result)
  
  model <- fit_model(df_reg)
  if (is.null(model)) return(null_result)
  
  sum_model <- summary(model)
  aug_df <- broom::augment(model, data = df_reg)
  inf_measures <- influence.measures(model)
  infmat <- inf_measures$infmat
  tests <- compute_tests(model)
  
  influential_measures <- compute_influential(model, aug_df, nrow(df_reg))
  influential <- influential_measures$influential
  leverage_threshold <- influential_measures$leverage_threshold
  cooks_threshold <- influential_measures$cooks_threshold
  
  num_outliers <- if (all(is.na(tests$outlier_test$bonf.p))) 0 else sum(tests$outlier_test$bonf.p < 0.05, na.rm = TRUE)
  num_influential <- length(influential)
  
  diag_row <- create_diag_row(model, sum_model, tests, num_outliers, num_influential)
  
  inf_df <- create_inf_df(aug_df, infmat, influential, leverage_threshold, cooks_threshold)
  
  list(diag = diag_row, inf = inf_df)
}

process_aux <- function(results) {
  list(
    diag = results %>%
      purrr::map("diag") %>%
      purrr::compact() %>%
      dplyr::bind_rows(),
    influence = results %>%
      purrr::map("inf") %>%
      purrr::compact() %>%
      dplyr::bind_rows()
  )
}

process <- function(df_gbd, df_epa, age_list, sex_list) {
  process_aux(
    with(expand.grid(age = age_list, sex = sex_list), 
         purrr::map2(age, sex, function(x,y) 
           process_comb(df_gbd=df_gbd, df_epa=df_epa, age_val=x, sex_val=y)))
    )
}

output <- function(df) {
  c(
    write_csv(df$diag, fs::path(here::here("data", "clean", "diagnostic_summary"),ext = "csv")),
    write_csv(df$influence, fs::path(here::here("data", "clean", "influence_measures"),ext = "csv")) 
  )
}

main <- function() {
  process(
    df_gbd = read_csv(fs::path(here::here("data", "clean", "IHME-GBD_2021_CLEAN_prevalence.csv"))) |>
      mutate(age = as.integer(age)), 
    df_epa = read_csv(fs::path(here::here("data", "clean", "arsenic_pm2.5_locations.csv"))) |> 
      rename(arsenic := mu) |>
      select(location, arsenic),
    age_list = 5*(1:19)-3,
    sex_list = c("Male", "Female")
  ) |> 
    output()
}

main()
