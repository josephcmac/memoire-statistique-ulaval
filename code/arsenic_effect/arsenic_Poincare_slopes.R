library(magrittr)
library(dplyr)
library(ggplot2)

color_age <- function(n) {
  rep(c("red", "blue"), length.out=n )
}


create_graphics <- function(df, sex0, period) {
  df |> filter(
    sex == sex0, 
    response == "mu", 
    coef_name == "arsenic"
    ) |>
    select(age, Estimate, MAD) |>
    mutate(sigma = 1.4826*MAD) %>%
    mutate(
      Estimate_next = lead(Estimate), 
      sigma_next = lead(sigma),
      color = color_age(n())
      ) |>
    ggplot() +
    geom_segment(aes(x = Estimate, y = sigma, xend = Estimate_next, yend = sigma_next, color=color),
                 arrow = arrow(length = unit(0.02, "npc"))) +
    geom_point(aes(Estimate, sigma)) +
    xlim(0, 0.04) +
    ylim(0, 0.1) +
    labs(
      title = "Modèles des pentes triées par tranche d'âge",
      subtitle = paste0("Sexe : ", ifelse(sex0=="Male", "mâle", "femelle"),"; Période : ",period),
      x = "μ", y = "σ",
      caption = "Source : EPA & IHME; Visualisation : José Manuel Rodríguez Caballero"
    ) +
    theme_bw() +
    theme(legend.position = "")
}


process <- function(df, period) {
  list(
    "Male" = create_graphics(df, sex0="Male", period),
    "Female" = create_graphics(df, sex0="Female", period)
  )
}

save_graphics <- function(l) {
  sapply(names(l), function(l_name)
    ggsave(
      filename = fs::path(here::here("text", "figures", "arsenic_Poincare_slopes", l_name), ext = "png"),
      plot = l[[l_name]]
      )
    )
}

main <- function() {
  readr::read_csv(fs::path(here::here("data", "clean", "arsenic_regression_stats"), ext = "csv")) %>%
    process(period="1990-2019") |>
    save_graphics()
}

main()

