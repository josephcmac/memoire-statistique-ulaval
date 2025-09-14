library(tidyverse)
library(fs)
library(here)

create_table_aux <- function(df, u, v) {
  df |>
    mutate(
      val = mu,
      lower = mu + u*sigma, 
      upper = mu + v*sigma
    ) |>
    select(-mu,-sigma)
}

create_table <- function(df, country0, alpha) {
  create_table_aux(
    df = df |>
      filter(country == country0) |>
      select(age, mu, sigma), 
    u = qnorm(alpha/2), 
    v = qnorm(1-alpha/2)
    )
}

create_extended_table <- function(df, country_list, alpha) {
  Reduce(f = rbind,
         x = map(country_list, function(x) 
           create_table(df=df, country0 = x, alpha = alpha) |>
             mutate(country=x)
         ),
         init = data.frame()
  )
}


create_graphics_sex <- function(df, period_label, dodge_width=2) {
  df |> 
    rename(pays := country) |>
    ggplot(aes(x = age, y = val, color = pays)) +
    geom_line(linewidth=0.1) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, 
                  position = position_dodge(width = dodge_width)) +
    geom_hline(yintercept = 0, color="black", linetype="dashed") +
    labs(
      title = "g de Cohen par tranche d'âge (95 % CI)",
      subtitle = paste0("Période : ", period_label),
      x = "Âge (± 2 ans)", y = "Nouveaux cas pour 100 000",
      caption = "Source : GBD-2021\nVisualisation : José Manuel Rodríguez Caballero") +
    theme_bw()
}

create_all_graphics <- function(df) {
  list(
    "all" = df |>
      create_graphics_sex(
        period_label = "1990-2019"
        )
  )
}


save_graphics <- function(l) {
  map(names(l), function(n) 
    ggsave(
      filename = path(here("text", "figures", "sex-age_plot_country-sex",
                                n), ext = "png"),
      plot = l[[n]]
      )
    )
}

main <- function() {
  read_csv(file = path(here("data", "clean", "IHME-GBD_2021_CLEAN_incidence_g_Cohen"),
                       ext = "csv")) |>
    mutate(age = as.integer(age)) |>
    create_extended_table(
      country_list = c("US", "UK", "NO", "IT"),
      alpha = 0.05
    ) |> 
    create_all_graphics() |>
    save_graphics()  
}


main()

