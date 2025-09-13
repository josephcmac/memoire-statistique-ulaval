library(tidyverse)
library(fs)
library(here)

KL_div <- function(mu1, sigma1, mu2, sigma2) {
  log(sigma2 / sigma1) + (sigma1^2 + (mu1 - mu2)^2) / (2 * sigma2^2) - 0.5
}

KL_divergences_aux <- function(df) {
  df <- df |> arrange(age)
  data.frame(
    age = head(df$age, -1),
    kl_div = KL_div(
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
           rbind(
             {
               df_sub <- df |>
                 filter(country == country0, sex == "Male") |>
                 select(age, mu, sigma)
               div_df <- KL_divergences(df_sub)
               div_df$country <- country0
               div_df$sex <- "Male"
               div_df
             },
             {
               df_sub <- df |>
                 filter(country == country0, sex == "Female") |>
                 select(age, mu, sigma)
               div_df <- KL_divergences(df_sub)
               div_df$country <- country0
               div_df$sex <- "Female"
               div_df
             }
           )
         }),
         init = data.frame()
  )
}


process_data <- function(country_list) {
  read_csv(file = path(here("data", "clean", "IHME-GBD_2021_CLEAN_incidence_bootstrapping"), ext = "csv")) |>
    KL_countries(country_list) |>
    mutate(sex = ifelse(sex == "Male", "mâles", "femelles"))
}

create_plot <- function(plot_data, cntry, period) {
  ggplot(plot_data, aes(x = age, y = kl_div, color = sex)) +
    geom_line() +
    theme_bw() +
    labs(
      title = "Divergences KL cumulées jusqu'à une tranche d'âge donnée",
      subtitle = paste0("Pays : ",cntry, "; Période : ", period),
      x = "Âge",
      y = "Divergence KL cumulées",
      color = "Sexe",
      caption = "Source : GBD-2021\nVisualisation : José Manuel Rodríguez Caballero"
    )
}

save_country_plot <- function(cntry, processed_data, period) {
  plot_data <- processed_data |> filter(country == cntry)
  p <- create_plot(plot_data, cntry, period)
  filename <- fs::path(here::here("text", "figures", "age-sex_KL_cumulative",
                              paste0("age-sex_KL_cumulative_", cntry)), ext = "png")
  ggsave(filename = filename, plot = p)
}

main <- function(country_list = c("US", "UK", "NO", "IT")) {
  processed <- process_data(country_list)
  countries <- unique(processed$country)
  walk(countries, ~ save_country_plot(.x, processed, period="1990-2019"))
}

main()