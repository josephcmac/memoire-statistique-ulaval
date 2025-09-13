library(tidyverse)
library(fs)
library(here)
library(xtable)

Fisher_Rao <- function(mu1, sigma1, mu2, sigma2) {
  sqrt(2)*acosh(1 + ((mu2 - mu1)^2 + 2*(sigma2 - sigma1)^2)/(4*sigma1*sigma2))
}

Fisher_Rao_df <- function(df) {
  Fisher_Rao(
    mu1 = head(df$mu,-1),
    sigma1 = head(df$sigma,-1),
    mu2 = tail(df$mu,-1),
    sigma2 = tail(df$sigma,-1)
  ) |>
    sum()
}

Fisher_Rao_countries <- function(df, country_list) {
  Reduce(f=rbind,
         x=lapply(country_list, function(country0)
           data.frame(
             country = country0,
             FR = df |> 
               filter(country == country0) |>
               select(-country,-n_sample) |>
               Fisher_Rao_df()
           )  
         ),
         init=data.frame()
  )
}



main <- function(country_list = c("US", "UK", "NO", "IT")) {
  read_csv(file=path(here("data", "clean", "gbd-2021-sex-age"),  
                     ext = "csv")) |>
    Fisher_Rao_countries(country_list) |>
    xtable() |>
    print.xtable(file=fs::path(here::here("text","tables", "sex", "sex_FR_total",
                                          "sex_FR_total_raw"), ext = "tex"),
                 include.rownames=FALSE)  
}

main()
