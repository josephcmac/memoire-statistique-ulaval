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

create_plot <- function(df, x_min, x_max, y_min, y_max,
                        country, sex, period) {
  df |>
    mutate(ci_lower = mu - 1.96 * sigma, ci_upper = mu + 1.96 * sigma) |>
    mutate(color = color_age(nrow(df))) |>
    mutate(mu_end = lead(mu), sigma_end = lead(sigma)) |>
  ggplot() +
    geom_point(aes(mu, sigma)) +
    geom_segment(aes(x = ci_lower, y = sigma, xend = ci_upper, yend = sigma),
                 color = "black") +
    geom_segment(data = . %>% filter(!is.na(mu_end) & !is.na(sigma_end)),
                 aes(x = mu, y = sigma, xend = mu_end, yend = sigma_end, color = color),
                 arrow = arrow(length = unit(0.2, "cm"))) +
    labs(
      title = "Demi-plan de Poincaré",
      subtitle = paste0("Pays : ", country, "; Sexe : ", sex, "; Période : ", period),
      x = "µ", y = "𝛔",
      caption = "Source : GBD-2021\nVisualisation : José Manuel Rodríguez Caballero") +
    theme_bw() +
    xlim(x_min, x_max) +
    ylim(y_min, y_max) +
    theme(legend.position = "")
}

generate_graphics <- function(df) {
  list(
    "US-Male" = df |>
      filter(country == "US", sex == "Male") |>
      select(-country,-sex,-n_sample) |>
      create_plot(x_min=-3,x_max=-1,y_min=0,y_max=0.01,
                  country="US", sex="mâle", period="1990-2019"),
    "US-Female" = df |>
      filter(country == "US", sex == "Female") |>
      select(-country,-sex,-n_sample) |>
      create_plot(x_min=-3,x_max=-1,y_min=0,y_max=0.01,
                  country="US", sex="femelle", period="1990-2019"),
    "UK-Male" = df |>
      filter(country == "UK", sex == "Male") |>
      select(-country,-sex,-n_sample) |>
      create_plot(x_min=-3,x_max=-1,y_min=0,y_max=0.03,
                  country="UK", sex="mâle", period="1990-2019"),
    "UK-Female" = df |>
      filter(country == "UK", sex == "Female") |>
      select(-country,-sex,-n_sample) |>
      create_plot(x_min=-3,x_max=-1,y_min=0,y_max=0.03,
                  country="UK", sex="femelle", period="1990-2019"),
    "NO-Male" = df |>
      filter(country == "NO", sex == "Male") |>
      select(-country,-sex,-n_sample) |>
      create_plot(x_min=-3,x_max=-1,y_min=0,y_max=0.02,
                  country="NO", sex="mâle", period="1990-2019"),
    "NO-Female" = df |>
      filter(country == "NO", sex == "Female") |>
      select(-country,-sex,-n_sample) |>
      create_plot(x_min=-3,x_max=-1,y_min=0,y_max=0.02,
                  country="NO", sex="femelle", period="1990-2019"),
    "IT-Male" = df |>
      filter(country == "IT", sex == "Male") |>
      select(-country,-sex,-n_sample) |>
      create_plot(x_min=-3,x_max=-1,y_min=0,y_max=0.03,
                  country="IT", sex="mâle", period="1990-2019"),
    "IT-Female" = df |>
      filter(country == "IT", sex == "Female") |>
      select(-country,-sex,-n_sample) |>
      create_plot(x_min=-3,x_max=-1,y_min=0,y_max=0.03,
                  country="IT", sex="femelle", period="1990-2019")
  )
}

save_graphics <- function(l) {
  map(names(l), function(key) 
    ggsave(path(here("text","figures","age-sex_Poincare", key),  
                ext = "png"), 
           l[[key]], width = 6, height = 6))
}

main <- function() {
  read_csv(file=path(here("data", "clean", "IHME-GBD_2021_CLEAN_incidence_bootstrapping"),  
                     ext = "csv")) |>
    generate_graphics() |>
    save_graphics()
}

main()



