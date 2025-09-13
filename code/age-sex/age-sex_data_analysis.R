library(tidyverse)
library(here)
library(fs)

inverse_logit_per_100k <- function(logit_value) {
  1e5 / (1 + 10^(-logit_value))
}

create_plot <- function(df, country, sex0, period, L_min, L_max) {
  df |>
    arrange(age) |>
    ggplot() +
    geom_line(aes(age, inverse_logit_per_100k(mu)), color = "black") +
    geom_point(aes(age, inverse_logit_per_100k(mu)), color = "black") +
    geom_errorbar(aes(age, inverse_logit_per_100k(mu),
                      ymin = inverse_logit_per_100k(ci_low),
                      ymax = inverse_logit_per_100k(ci_high)),
                  color = "gray", width = 0.2) +
    ylim(inverse_logit_per_100k(L_min), inverse_logit_per_100k(L_max)) +
    labs(
      title = "Taux d'incidence des MRC par âge",
      subtitle = paste0("Pays : ", country, "; Sexe : ", ifelse(sex0 == "Male", "mâle", "femelle"), "; Période : ", period),
      caption = "Source : EPA & IHME; Visualisation : José Manuel Rodríguez Caballero",
      x = "Tranche d'âge (± 2 ans)", y = "Nouveaux cas de MRC pour 100 000 personnes"
    ) +
    theme_bw()
}

summarise_per_age <- function(df_sex, q=qnorm(0.975)) {
  df_sex |>
    mutate(age = as.numeric(age)) |>
    group_by(age) |>
    summarise(
      mu = mean(mu),
      sigma = sqrt(sum(sigma^2)) / n(),
      .groups = "drop"
    ) |>
    mutate(
      ci_low = mu - q * sigma,
      ci_high = mu + q * sigma
    )
}


process_data_3 <- function(df_male, df_female, L_min, L_max, country = "US", start_year = 1990, end_year = 2019) {
  list(
    "Male" = create_plot(df = df_male, country = country, sex0 = "Male", period = paste(start_year, "-", end_year), L_min = L_min, L_max = L_max),
    "Female" = create_plot(df = df_female, country = country, sex0 = "Female", period = paste(start_year, "-", end_year), L_min = L_min, L_max = L_max)
  )
}
  

process_data_2 <- function(df_male, df_female, country = "US", start_year = 1990, end_year = 2019) {
  process_data_3(
    df_male=df_male, 
    df_female=df_female, 
    L_min=min(c(df_male$ci_low, df_female$ci_low)), 
    L_max=max(c(df_male$ci_high, df_female$ci_high)), 
    country = "US", start_year = 1990, end_year = 2019)
}
  

process_data_1 <- function(df_filtered, country = "US", start_year = 1990, end_year = 2019) {
  process_data_2(
    df_male = df_filtered |> filter(sex == "Male") |> summarise_per_age(),
    df_female = df_filtered |> filter(sex == "Female") |> summarise_per_age()
  )
}

process_data <- function(df, country = "US", start_year = 1990, end_year = 2019) {
  process_data_1(df_filtered = df |>
                   filter(year >= start_year & year <= end_year, country == !!country))
}

save_plots <- function(plot_list) {
  sapply(names(plot_list), function(name) {
    ggsave(
      filename = fs::path(here::here("text", "figures", "age-sex_data_analysis"), name, ext = "png"),
      plot = plot_list[[name]]
    )
  })
}

main <- function(start_year = 1990, end_year = 2019) {
  process_data(
    df = readr::read_csv(here::here("data", "clean", "IHME-GBD_2021_CLEAN_incidence.csv")),
    start_year = start_year, 
    end_year = end_year) |>
    save_plots()
}

main()