library(tidyverse)
library(fs)
library(here)
library(xtable)

KL_norm <- function(mu1, sigma1, mu2, sigma2) {
  log(sigma2 / sigma1) + (sigma1^2 + (mu1 - mu2)^2) / (2 * sigma2^2) - 0.5
}

KL_df <- function(df) {
  KL_norm(
    mu1 = head(df$mu, -1),
    sigma1 = head(df$sigma, -1),
    mu2 = tail(df$mu, -1),
    sigma2 = tail(df$sigma, -1)
  ) |>
    sum()
}

KL_countries <- function(df, country_list) {
  Reduce(f = rbind,
         x = lapply(country_list, function(country0)
           data.frame(
             country = country0,
             KL = df |>
               filter(country == country0) |>
               select(-country, -n_sample) |>
               KL_df()
           )
         ),
         init = data.frame()
  )
}



main <- function(country_list = c("US", "UK", "NO", "IT")) {
  read_csv(file=path(here("data", "clean", "gbd-2021-sex-age"),  
                     ext = "csv")) |>
    KL_countries(country_list) |>
    xtable() |>
    print.xtable(file=fs::path(here::here("text","tables", "sex", "sex_KL_total",
                                          "sex_KL_total_raw"), ext = "tex"),
                 include.rownames=FALSE)  
}

main()
