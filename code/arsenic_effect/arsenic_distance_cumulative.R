library(tidyverse)

Fisher_Rao <- function(mu1, sigma1, mu2, sigma2, sqrt2 = 1.41421356237) {
  sqrt2 * acosh(1 + ((mu2 - mu1)^2 + 2 * (sigma2 - sigma1)^2) / (4 * sigma1 * sigma2))
}

Kullback_Leibler <- function(mu1, sigma1, mu2, sigma2) {
  log(sigma2 / sigma1) + (sigma1^2 + (mu1 - mu2)^2) / (2 * sigma2^2) - 0.5
}

create_df <- function(age, d_male, d_female) {
  data.frame(
    age = age,
    u_male = cumsum(d_male),
    u_female = cumsum(d_female)
  )
}

process <- function(df_male, df_female, f) {
  create_df(
    age = tail(df_male,-1)$age,
    d_male = sapply(2:nrow(df_male), function(i) 
      f(
        mu1=df_male$mu[i-1], sigma1=df_male$sigma[i-1],
        mu2=df_male$mu[i], sigma2=df_male$sigma[i]
        )
      ),
    d_female = sapply(2:nrow(df_female), function(i) 
      f(
        mu1=df_female$mu[i-1], sigma1=df_female$sigma[i-1],
        mu2=df_female$mu[i], sigma2=df_female$sigma[i]
        )
      )
  )
}

process_sex <- function(df, f) {
  process(
    df_male = df |> filter(sex == "Male", response == "mu", 
                           coef_name == "arsenic") |>
      select(age, Estimate, MAD) |>
      mutate(MAD = 1.4826*MAD) |>
      rename(mu := Estimate, sigma := MAD),
    df_female = df |> filter(sex == "Female", response == "mu", 
                             coef_name == "arsenic") |>
      select(age, Estimate, MAD) |>
      mutate(MAD = 1.4826*MAD) |>
      rename(mu := Estimate, sigma := MAD),
    f = f
  )
}

plot_df <- function(u, title, y_lab) {
  ggplot(u) +
    geom_point(aes(age, u_male), color="blue") +
    geom_line(aes(age, u_male), color="blue") +
    geom_point(aes(age, u_female), color="pink") +
    geom_line(aes(age, u_female), color="pink") +
    labs(
      title = title,
      subtitle = "Mâles : bleu; Femelles : rose",
      x = "Tranche d'âge (± 2 ans)",
      y = y_lab,
      caption = "Source : EPA & IHME; Visualisation : José Manuel Rodríguez Caballero"
    ) +
    theme_bw()  
}

main_aux <- function(f, title, y_lab, file_name) {
  process_sex(
    df = readr::read_csv(fs::path(here::here("data", "clean", 
                              "arsenic_regression_stats"), ext = "csv")), 
    f = f
    ) |>
    plot_df(
      title = title,
      y_lab = y_lab
    ) %>%
    ggsave(
      filename = fs::path(here::here("text", "figures", "arsenic_distance_cumulative", file_name), ext = "png"), 
      plot = .
    )
}


main <- function() {
  c(
    main_aux(
      f = Fisher_Rao,
      title = "Distance de Fisher-Rao cumulée entre pentes consécutives", 
      y_lab = "Distance de Fisher-Rao cumulée",
      file_name = "Fisher-Rao"
    ),
    main_aux(
      f = Kullback_Leibler,
      title = "Divergence de Kullback-Leibler cumulée entre pentes consécutives",
      y_lab = "Divergence de Kullback-Leibler cumulée",
      file_name = "Kullback-Leibler"
    ) 
  )
}

main()