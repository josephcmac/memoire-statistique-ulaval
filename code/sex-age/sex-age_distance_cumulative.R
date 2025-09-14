library(tidyverse)

Fisher_Rao <- function(mu1, sigma1, mu2, sigma2, sqrt2 = 1.41421356237) {
  sqrt2 * acosh(1 + ((mu2 - mu1)^2 + 2 * (sigma2 - sigma1)^2) / (4 * sigma1 * sigma2))
}

Kullback_Leibler <- function(mu1, sigma1, mu2, sigma2) {
  log(sigma2 / sigma1) + (sigma1^2 + (mu1 - mu2)^2) / (2 * sigma2^2) - 0.5
}

create_df <- function(age, d) {
  data.frame(
    age = age,
    u = cumsum(d)
  )
}

process <- function(df, f) {
  create_df(
    age = tail(df,-1)$age,
    d = sapply(2:nrow(df), function(i) 
      f(
        mu1=df$mu[i-1], sigma1=df$sigma[i-1],
        mu2=df$mu[i], sigma2=df$sigma[i]
        )
      )
  )
}

process_country <- function(df, f) {
  process(
    df = df |> filter(country == "US") |>
      arrange(age) |> select(age, mu, sigma),
    f = f
  )
}

plot_df <- function(u, title, y_lab) {
  ggplot(u) +
    geom_point(aes(age, u), color="orange") +
    geom_line(aes(age, u), color="orange") +
    labs(
      title = title,
      subtitle = "Période : 1990-2019",
      x = "Tranche d'âge (± 2 ans)",
      y = y_lab,
      caption = "Source : EPA & IHME; Visualisation : José Manuel Rodríguez Caballero"
    ) +
    theme_bw()  
}

main_aux <- function(f, title, y_lab, file_name) {
  process_country(
    df = readr::read_csv(fs::path(here::here("data", "clean", 
                "IHME-GBD_2021_CLEAN_incidence_g_Cohen"), ext = "csv")), 
    f = f
    ) |>
    plot_df(
      title = title,
      y_lab = y_lab
    ) %>%
    ggsave(
      filename = fs::path(here::here("text", "figures", "sex-age_distance_cumulative", file_name), ext = "png"), 
      plot = .
    )
}


main <- function() {
  c(
    main_aux(
      f = Fisher_Rao,
      title = "Distance de Fisher-Rao cumulée entre âges consécutives", 
      y_lab = "Distance de Fisher-Rao cumulée",
      file_name = "Fisher-Rao"
    ),
    main_aux(
      f = Kullback_Leibler,
      title = "Divergence de Kullback-Leibler cumulée entre âges consécutives",
      y_lab = "Divergence de Kullback-Leibler cumulée",
      file_name = "Kullback-Leibler"
    ) 
  )
}

main()


