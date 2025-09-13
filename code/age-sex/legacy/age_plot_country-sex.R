library(tidyverse)
library(fs)
library(here)

inverse_logit_per_100k <- function(logit_value) {
  1e5 / (1 + 10^(-logit_value))
}

create_table_aux <- function(df, u, v) {
  df |>
    mutate(
      val = inverse_logit_per_100k(mu),
      lower = inverse_logit_per_100k(mu + u*sigma), 
      upper = inverse_logit_per_100k(mu + v*sigma)
    ) |>
    select(-mu,-sigma)
}

create_table <- function(df, country0, sex0, alpha) {
  create_table_aux(
    df =  df |>
      filter(country == country0, sex == sex0) |>
      select(age, mu, sigma), 
    u = qnorm(alpha/2), 
    v = qnorm(1-alpha/2)
    )
}

create_extended_table_sex <- function(df, country_list, sex1, alpha) {
  Reduce(f = rbind,
         x = map(country_list, function(x) 
           create_table(df=df, country0 = x, sex0 = sex1, alpha = alpha) |>
             mutate(country=x, sex=sex1)
         ),
         init = data.frame()
  )
}

create_extended_table <- function(df, country_list, sex_list, alpha) {
  Reduce(f = rbind,
         x = map(sex_list, function(x) 
           create_extended_table_sex(df=df, country_list=country_list, sex1 = x, 
                                     alpha = alpha) 
           ),
         init = data.frame()
  )
}

create_graphics_sex <- function(df, sex_label, period_label, y_min, y_max, dodge_width=1) {
  df |> 
    rename(pays := country) |>
    ggplot(aes(x = age, y = val, color = pays)) +
    geom_line(linewidth=0.1) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                  position = position_dodge(width = dodge_width)) +
    labs(
      title = "95% CI des taux d'incidence des MRC par tranche d'âge",
      subtitle = paste0("Sexe : ", sex_label,"; Période : ", period_label),
      x = "Âge (± 2 ans)", y = "Nouveaux cas pour 100 000",
      caption = "Source : GBD-2021\nVisualisation : José Manuel Rodríguez Caballero") +
    ylim(y_min, y_max) +
    theme_bw()
}

create_all_graphics <- function(df) {
  list(
    "Male" = df |> filter(sex == "Male") |>
      create_graphics_sex(
        sex_label = "Mâle",
        period_label = "1990-2019",
        y_min=0, y_max=6000
      ),
   "Female" = df |> filter(sex == "Female") |>
      create_graphics_sex(
        sex_label = "Femelle",
        period_label = "1990-2019",
        y_min=0, y_max=6000
      )
  )
}


save_graphics <- function(l) {
  map(names(l), function(n) 
    ggsave(
      filename = path(here("text", "figures", "age", "age_plot_country-sex",
                                n), ext = "png"),
      plot = l[[n]]
      )
    )
}

main <- function() {
  read_csv(file = path(here("data", "clean", "gbd-2021-age-sex"),
                       ext = "csv")) |>
    mutate(age = as.integer(age)) |>
    create_extended_table(
      country_list = c("US", "UK", "NO", "IT"),
      sex_list = c("Male", "Female"),
      alpha = 0.05
    ) |> 
    create_all_graphics() |>
    save_graphics()  
}


main()

