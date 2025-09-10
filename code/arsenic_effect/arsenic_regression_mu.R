library(magrittr)
library(dplyr)
library(ggplot2)

french_sex <- function(sex) {
  ifelse(sex == "Male", "mâle", "femelle")
}


sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

create_graphics <- function(df, age, sex) {
  df |>
    ggplot() +
    geom_point(aes(arsenic, mu)) +
    geom_smooth(aes(arsenic, mu), method=sen, color="red", formula=y~x) +
    labs(
      title = expression("Régression robuste de la moyenne MRC selon l'Arsenic " ~ PM[2.5]),
      subtitle = paste0("Âge : ", age, " (± 2 ans); Sexe : ", french_sex(sex)),
      caption = "Source : EPA & IHME; Visualisation : José Manuel Rodríguez Caballero",
      x = "μ (arsenic)", y = "μ (MRC)"
    ) +
    theme_bw() +
    theme(legend.position = "none")
}

process_age_sex <- function(df, age, sex) {
  l <- list(
    create_graphics(df, age, sex)
  )
  names(l) <- c(paste0("mu","-",age,"-",sex) )
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
        "text", "figures", "arsenic_regression_mu", l_name
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
