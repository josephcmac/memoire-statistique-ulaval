library(magrittr)
library(dplyr)
library(ggplot2)

french_sex <- function(sex) {
  ifelse(sex == "Male", "mâle", "femelle")
}

color_arrow <- function(n) {
  rep(c("red", "blue"), length.out = n)
}

create_graphics <- function(df, g, age, sex, q=1.959964) {
  df |>
    mutate(r = floor((rank(arsenic) - 1) / g)) |>
    group_by(r) |>
    summarise(
      mu = mean(mu),
      sigma = sqrt(sum(sigma^2)) / n()
    ) |>
    arrange(r) |>
    mutate(
      mu_next = lead(mu),
      sigma_next = lead(sigma),
      color = color_arrow(n())
    ) |>
    ggplot() +
    geom_segment(
      aes(x = mu, y = sigma, xend = mu_next, yend = sigma_next, color = color),
      arrow = arrow(length = unit(0.3, "cm"))
    ) +
    geom_segment(
      aes(
        x = mu - q * sigma,
        y = sigma,
        xend = mu + q * sigma,
        yend = sigma
      ), color = "gray"
    ) +
    geom_point(aes(mu, sigma)) +
    labs(
      title = expression("MRC ordonnées par l'exposition à l'Arsenic " ~ PM[2.5]),
      subtitle = ifelse(g == 1,
                        paste0("Granularité : 1 élément par groupe; Âge : ", age, "(± 2 ans); Sexe : ", french_sex(sex)),
                        paste0("Granularité : ", g, " éléments par groupe; Âge : ", age, " (± 2 ans); Sexe : ", french_sex(sex))),
      caption = "Source : EPA & IHME; Visualisation : José Manuel Rodríguez Caballero",
      x = "μ", y = "σ"
    ) +
    theme_bw() +
    theme(legend.position = "none")
}

process_age_sex <- function(df, age, sex) {
  l <- list(
    create_graphics(df, 1, age, sex),
    create_graphics(df, 3, age, sex),
    create_graphics(df, 17, age, sex)
  )
  names(l) <- c(paste0("G1","-",age,"-",sex),
                paste0("G3","-",age,"-",sex),
                paste0("G17","-",age,"-",sex) )
  l
}

process <- function(df_epa, df_gbd) {
  Reduce(
    f = function(previous, l) {
      c(previous,
        process_age_sex(
          df = df_gbd |>
            filter(age == as.numeric(l$Var1), sex == l$Var2) |>
            group_by(location) |>
            summarise(
              mu = mean(mu),
              sigma = sqrt(sum(sigma*sigma))/n()
            ) |>
            merge(df_epa |> select(location, mu) |>
                    rename(arsenic := mu),
                  by = "location"),
          age = l$Var1,
          sex = l$Var2
        )
      )
    },
    x = expand.grid(5*(1:19) - 3, c("Male", "Female")) |>
      apply(1, as.list),
    init = list()
  )
}


save_graphics_list <- function(l) {
  lapply(names(l), function(l_name) {
    ggsave(
      filename = fs::path(here::here(
        "text", "figures", "arsenic_path", l_name
      ), ext = "png"),
      plot = l[[l_name]]
    )
  })
}

main <- function() {
  process(
    df_epa = readr::read_csv(file = fs::path(here::here("data", "clean",
                                       "arsenic_pm2.5_locations"),ext="csv")), 
    df_gbd = readr::read_csv(file = fs::path(here::here("data", "clean", 
                                "IHME-GBD_2021_CLEAN_prevalence"), ext="csv"))
    ) |> save_graphics_list()
}

main()
