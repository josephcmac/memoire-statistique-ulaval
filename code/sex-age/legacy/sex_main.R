library(tidyverse)
library(fs)
library(here)

boot_mean <- function(l) {
  l[sample( 1:length(l), size=length(l), replace=TRUE )] |>
    mean()
}

sample_mean_aux <- function(df, seed) {
  withr::with_seed(
    seed = seed,
    code =  df |>
      mutate(I = rnorm(n(), mu, sigma)) |>
      select(-mu, -sigma) |>
      pivot_wider(
        names_from = sex, 
        values_from = I, 
        names_prefix = "") |>
      mutate(g = 0.5*sign(Male - Female) ) |>
      select(-Male, -Female) |>
      group_by(age) |>
      summarise(g = boot_mean(g)) |>
      pull("g")
  )
}

sample_mean <- function(df, seed, n_sample) {
  withr::with_seed(
    seed=seed,
    code = sample(1:(10*n_sample), size = n_sample) |> 
      sapply(function(seed1) sample_mean_aux(df, seed = seed1)) |> 
      t() |> as.data.frame()
  )
}

estimate_params <- function(x, country, n_sample, age_list) {
  data.frame(
    country = country,
    age = age_list,
    mu = colMeans(x),
    sigma = apply(x, MARGIN=2, FUN=sd),
    n_sample = n_sample
  )
}

create_df_sex <- function(df, country, n_sample, age_list) {
  df |>
    sample_mean(seed=123, n_sample=n_sample) |>
    estimate_params(country = country, n_sample = n_sample, age_list = age_list)
}

create_df_country <- function(df, country_list, n_sample, age_list)  {
  Reduce(
    f = rbind,
    x = map(country_list, function(country0)
      df |>
        filter(country == country0) |>
        select(-country) |>
        create_df_sex(country=country0, n_sample=n_sample, age_list=age_list)
      ),
    init = data.frame()
  )
}

main <- function(year0=1990, year1=2019) {
  read_csv(file=path(here("data", "clean", "gbd-2021-clean"),  ext = "csv")) |>
    filter(year >= year0, year <= year1) |>
    create_df_country(country_list=c("US", "UK", "NO", "IT"), n_sample=100000,
                      age_list=seq(2,92,5)) |>
    write_csv(file=path(here("data", "clean", "gbd-2021-sex-age"),
                        ext = "csv"))
}

main()

