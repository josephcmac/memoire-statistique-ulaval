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
      group_by(age) |>
      summarise(value = boot_mean(I)) |>
      pull("value")
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

estimate_params <- function(x, sex, country, n_sample, age_list) {
  data.frame(
    country = country,
    sex = sex,
    age = age_list,
    mu = colMeans(x),
    sigma = apply(x, MARGIN=2, FUN=sd),
    n_sample = n_sample
  )
}

create_df_sex <- function(df, country, sex0, n_sample, age_list) {
  df |>
    select(age, mu, sigma) |>
    sample_mean(seed=123, n_sample) |>
    estimate_params(sex = sex0, country = country, n_sample = n_sample, age_list)
}

create_df_country <- function(df, country_list, n_sample, age_list)  {
  Reduce(f = rbind,
         x = map(country_list, function(country0)
           rbind(
             df |>
               filter(sex == "Male", country == country0) |>
               create_df_sex(country=country0, sex0="Male", n_sample, age_list),
             df |>
               filter(sex == "Female", country == country0) |>
               create_df_sex(country=country0, sex0="Female", n_sample, age_list)
           )),
         init=data.frame()
  )
}

main <- function(year0=1990, year1=2019) {
  read_csv(file=path(here("data", "clean", "IHME-GBD_2021_CLEAN_incidence"),  ext = "csv")) |>
    filter(year >= year0, year <= year1) |>
    create_df_country(country_list=c("US", "UK", "NO", "IT"), n_sample=1000,
                      age_list=seq(2,92,5)) |>
    write_csv(file=path(here("data", "clean", "IHME-GBD_2021_CLEAN_incidence_bootstrapping"),
                        ext = "csv"))
}

main()

