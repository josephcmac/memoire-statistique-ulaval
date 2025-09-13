library(tidyverse)
library(fs)
library(here)

color_age <- function(n) {
  if (n %% 2 == 0) {
    rep(c("red", "blue"), floor(n/2) )
  } else {
    c(rep(c("red", "blue"), floor(n/2) ), "red")
  }
}

create_plot <- function(df, country, period) {
  df |>
    mutate(color = color_age(nrow(df))) |>
    mutate(mu_end = lead(mu), sigma_end = lead(sigma)) |>
    mutate(ci_lower = mu - 1.96 * sigma, ci_upper = mu + 1.96 * sigma) |>
    ggplot() +
    geom_point(aes(mu, sigma)) +
    geom_vline(xintercept = 0, linetype="dashed", color="gray") +
    geom_segment(data = . %>% filter(!is.na(mu_end) & !is.na(sigma_end)),
                 aes(x = mu, y = sigma, xend = mu_end, yend = sigma_end, 
                     color = color),
                 arrow = arrow(length = unit(0.2, "cm"))) +
    geom_segment(aes(x = ci_lower, y = sigma, xend = ci_upper, yend = sigma),
                 color = "black") +
    labs(
      title = "Demi-plan de Poincaré",
      subtitle = paste0("Pays : ", country, "; Période : ", period),
      x = "µ", y = "𝛔",
      caption = "Source : GBD-2021\nVisualisation : José Manuel Rodríguez Caballero") +
    theme_bw() +
    theme(legend.position = "", panel.grid = element_blank())
}

generate_graphics <- function(df) {
  list(
    "US" = df |>
      filter(country == "US") |>
      select(-country,-n_sample) |>
      create_plot(country="US", period="1990-2019"),
    "UK" = df |>
      filter(country == "UK") |>
      select(-country,-n_sample) |>
      create_plot(country="UK", period="1990-2019"),
    "NO" = df |>
      filter(country == "NO") |>
      select(-country,-n_sample) |>
      create_plot(country="NO", period="1990-2019"),
    "IT" = df |>
      filter(country == "IT") |>
      select(-country,-n_sample) |>
      create_plot(country="IT", period="1990-2019")
  )
}

save_graphics <- function(l) {
  map(names(l), function(key) 
    ggsave(path(here("text","figures","sex", "sex_Poincare", key),  
                ext = "png"), 
           l[[key]], width = 6, height = 6))
}

main <- function() {
  read_csv(file=path(here("data", "clean", "gbd-2021-sex-age"),  
                     ext = "csv")) |>
    generate_graphics() |>
    save_graphics()
}

main()



