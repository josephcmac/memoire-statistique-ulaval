library(magrittr)
library(dplyr)
library(ggplot2)

create_graphics <- function(df, country, sex0, period, L_min, L_max) {
  df |> filter(sex == sex0, response == "mu", coef_name == "arsenic") |>
    select(age, estimate, ci_low, ci_high ) |>
    ggplot() +
    geom_hline(yintercept = 0, color = "red", linetype = "dotted") +
    geom_line(aes(age, estimate)) +
    geom_segment(aes(x=age, y=ci_low, xend=age, yend=ci_high), color="gray") +
    geom_point(aes(age, estimate)) +
    ylim( min(L_min, 0), max(L_max, 0) ) +
    labs(
      title = expression("Pente de l'effet de l'arsenic " ~ PM[2.5] ~ " par tranche d'âge"),
      subtitle = paste0("Pays : ", country, "; Sexe : ", ifelse(sex0 == "Male", "mâle", "femelle"), "; Période : ", period),
      caption = "Source : EPA & IHME; Visualisation : José Manuel Rodríguez Caballero",
      x = "Tranche d'âge (± 2 ans)", y = "Pente") +
    theme_bw()
}


process <- function(df, L_min, L_max) {
  list(
    "Male" = create_graphics(df, country = "US", sex0="Male", period="1990-2019", L_min, L_max),
    "Female" = create_graphics(df, country = "US", sex0="Female", period="1990-2019", L_min, L_max)
  )
}

save_graphics <- function(l) {
  sapply(names(l), function(l_name)
    ggsave(
      filename = fs::path(here::here("text", "figures", "arsenic_data_analysis", l_name), ext = "png"),
      plot = l[[l_name]]
      )
    )
}

main <- function() {
  readr::read_csv(fs::path(here::here("data", "clean", "arsenic_regression_stats"), ext = "csv")) %>%
    process(
      df = .,
      L_min = . |> 
        filter(response == "mu", coef_name == "arsenic") |>
        pull("ci_low"),
      L_max = . |> 
        filter(response == "mu", coef_name == "arsenic") |>
        pull("ci_high")
    ) |>
    save_graphics()
}

main()

