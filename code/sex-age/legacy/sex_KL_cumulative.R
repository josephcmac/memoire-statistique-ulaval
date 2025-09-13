library(tidyverse)
library(fs)
library(here)

KL_norm <- function(mu1, sigma1, mu2, sigma2) {
  log(sigma2 / sigma1) + (sigma1^2 + (mu1 - mu2)^2) / (2 * sigma2^2) - 0.5
}

KL_divergences_aux <- function(df) {
  df <- df |> arrange(age)
  data.frame(
    age = head(df$age, -1),
    kl_div = KL_norm(
      mu1 = head(df$mu, -1),
      sigma1 = head(df$sigma, -1),
      mu2 = tail(df$mu, -1),
      sigma2 = tail(df$sigma, -1)
    ) |> cumsum()
  )
}

KL_divergences <- function(df) {
  df |>
    arrange(age) |>
    KL_divergences_aux()
}

KL_countries <- function(df, country_list) {
  Reduce(f = rbind,
         x = lapply(country_list, function(country0) {
           df_sub <- df |>
             filter(country == country0) |>
             select(age, mu, sigma)
           div_df <- KL_divergences(df_sub)
           div_df$country <- country0
           div_df
         }),
         init = data.frame()
  )
}

process_data <- function(country_list) {
  read_csv(file = path(here("data", "clean", "gbd-2021-sex-age"), ext = "csv")) |>
    KL_countries(country_list)
}

create_plot <- function(plot_data, countries, period) {
  ggplot(plot_data, aes(x = age, y = kl_div, color = country)) +
    geom_line() +
    theme_bw() +
    labs(
      title = "Divergences KL cumulées jusqu'à une tranche d'âge donnée",
      subtitle = paste0("Pays : ", paste(countries, collapse = ", "), "; Période : ", period),
      x = "Âge",
      y = "Divergence KL cumulées",
      color = "Pays",
      caption = "Source : GBD-2021\nVisualisation : José Manuel Rodríguez Caballero"
    )
}

main <- function(country_list = c("US", "UK", "NO", "IT")) {
  processed <- process_data(country_list)
  period <- "1990-2019"
  p <- create_plot(processed, country_list, period)
  filename <- path(here::here("text", "figures", "sex", "sex_KL_cumulative", "sex_KL_cumulative_all"), ext = "png")
  ggsave(filename = filename, plot = p)
}

main()
