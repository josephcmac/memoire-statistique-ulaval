library(tidyverse)
library(fs)
library(here)

Fisher_Rao <- function(mu1, sigma1, mu2, sigma2) {
  sqrt(2) * acosh(1 + ((mu2 - mu1)^2 + 2 * (sigma2 - sigma1)^2) / (4 * sigma1 * sigma2))
}

Fisher_Rao_distances_aux <- function(df) {
  df <- df |> arrange(age)
  data.frame(
    age = head(df$age, -1),
    fr_dist = Fisher_Rao(
      mu1 = head(df$mu, -1),
      sigma1 = head(df$sigma, -1),
      mu2 = tail(df$mu, -1),
      sigma2 = tail(df$sigma, -1)
    ) |> cumsum()
  )
}

Fisher_Rao_distances <- function(df) {
  df |>
    arrange(age) |>
    Fisher_Rao_distances_aux()
}

Fisher_Rao_countries <- function(df, country_list) {
  Reduce(f = rbind,
         x = lapply(country_list, function(country0) {
           df_sub <- df |>
             filter(country == country0) |>
             select(age, mu, sigma)
           dist_df <- Fisher_Rao_distances(df_sub)
           dist_df$country <- country0
           dist_df
         }),
         init = data.frame()
  )
}

process_data <- function(country_list) {
  read_csv(file = path(here("data", "clean", "gbd-2021-sex-age"), ext = "csv")) |>
    Fisher_Rao_countries(country_list)
}

create_plot <- function(plot_data, countries, period) {
  ggplot(plot_data, aes(x = age, y = fr_dist, color = country)) +
    geom_line() +
    theme_bw() +
    ylim(0,100) +
    labs(
      title = "Distances cumulées de Fisher-Rao jusqu'à une tranche d'âge donnée",
      subtitle = paste0("Pays : ", paste(countries, collapse = ", "), "; Période : ", period),
      x = "Âge",
      y = "Distance cumulées de Fisher-Rao",
      color = "Pays",
      caption = "Source : GBD-2021\nVisualisation : José Manuel Rodríguez Caballero"
    )
}

main <- function(country_list = c("US", "UK", "NO", "IT")) {
  processed <- process_data(country_list)
  period <- "1990-2019"
  p <- create_plot(processed, country_list, period)
  filename <- path(here::here("text", "figures", "sex", "sex_FR_cumulative", "sex_FR_cumulative_all"), ext = "png")
  ggsave(filename = filename, plot = p)
}

main()