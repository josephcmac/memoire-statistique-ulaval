library(magrittr)
library(dplyr)
library(mblm)
library(readr)
library(fs)
library(here)

extract_stats_aux <- function(sm, ci, response) {
  data.frame(
    coef_name = rownames(sm),
    estimate = sm[,1],
    mad_se = sm[,2],
    t_value = sm[,3],
    p_value = sm[,4],
    ci_low = ci[,1],
    ci_high = ci[,2],
    response = response
  )
}


extract_stats <- function(model, response) {
  extract_stats_aux(
    sm = summary(model)$coefficients,
    ci = confint(model),
    response = response
  )
}

process_age_sex <- function(df, age, sex) {
  rbind(
    extract_stats(
      model = mblm(
        formula = mu ~ arsenic, 
        data = df),
      response = "mu"
    ),
    extract_stats(
      model = mblm(
        formula = log_sigma ~ arsenic, 
        data = df |> 
          mutate(log_sigma = log10(sigma)) |>
          select(-sigma)
      ), 
      response = "log_sigma"
    )
  ) %>%
  mutate(age = age, sex = sex, n_obs = nrow(df))
}

compute_current <- function(df_gbd_age_sex, df_epa, age, sex) {
    merge(df_gbd_age_sex, df_epa) |>
    process_age_sex(age, sex)
}

process <- function(df_gbd, df_epa, combos) {
  Reduce(
    f = function(previous, comb) {
      rbind(previous, compute_current(
        df_gbd_age_sex = df_gbd %>%
          filter(age == as.numeric(comb$age), sex == comb$sex) %>%
          group_by(location) %>%
          summarise(
            mu = mean(mu),
            sigma = sqrt(sum(sigma*sigma))/n()
          ),
        df_epa = df_epa,
        age = as.numeric(comb$age),
        sex = comb$sex
        ))
    },
    x = apply(combos, 1, as.list),
    init = list()
  )
}


main <- function() {
  process(
    df_gbd = read_csv(fs::path(here::here("data", "clean", "IHME-GBD_2021_CLEAN_prevalence"), ext = "csv")),
    df_epa = read_csv(fs::path(here::here("data", "clean", "arsenic_pm2.5_locations"), ext = "csv")) |> 
      select(location, mu) |> 
      rename(arsenic = mu),
    combos = expand.grid(age = 5*(1:19) - 3, sex = c("Male", "Female"))
  ) %>%
    select(age, sex, response, coef_name, estimate, mad_se, t_value, p_value, ci_low, ci_high, n_obs) |>
  write_csv(fs::path(here::here("data", "clean", "arsenic_regression_stats"), ext="csv"))
}

main()
