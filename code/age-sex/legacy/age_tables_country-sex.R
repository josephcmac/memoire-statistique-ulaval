library(tidyverse)
library(fs)
library(here)
library(xtable)

inverse_logit_per_100k <- function(logit_value) {
  1e5 / (1 + 10^(-logit_value))
}

create_table_aux <- function(df, u, v) {
  df |>
    mutate(
      val = inverse_logit_per_100k(mu),
      lower = inverse_logit_per_100k(mu + u*sigma), 
      upper = inverse_logit_per_100k(mu + v*sigma)
    ) |>
    select(-mu,-sigma)
}

create_table <- function(df, country0, sex0, alpha) {
  create_table_aux(
    df =  df |>
      filter(country == country0, sex == sex0) |>
      select(age, mu, sigma), 
    u = qnorm(alpha/2), 
    v = qnorm(1-alpha/2)
    )
}

create_all_tables <- function(df) {
  list(
    "US-Male" = create_table(df=df, country0 = "US", sex0 = "Male", 
                             alpha = 0.05),
    "US-Female" = create_table(df=df, country0 = "US", sex0 = "Female", 
                               alpha = 0.05),
    "UK-Male" = create_table(df=df, country0 = "UK", sex0 = "Male", 
                             alpha = 0.05),
    "UK-Female" = create_table(df=df, country0 = "UK", sex0 = "Female", 
                               alpha = 0.05),
    "NO-Male" = create_table(df=df, country0 = "NO", sex0 = "Male", 
                             alpha = 0.05),
    "NO-Female" = create_table(df=df, country0 = "NO", sex0 = "Female", 
                               alpha = 0.05),
    "IT-Male" = create_table(df=df, country0 = "IT", sex0 = "Male", 
                             alpha = 0.05),
    "IT-Female" = create_table(df=df, country0 = "IT", sex0 = "Female", 
                               alpha = 0.05)
  )
}


save_tables <- function(l) {
  lapply(names(l), function(l_name) {
    print.xtable(
        x=l[[l_name]]|>
          xtable(),
        file = path(here("text","tables", "age", "age_tables_country-sex", 
                         paste0(l_name, "_raw")), ext = "tex"),
        include.rownames = FALSE
        )
  })
}

main <- function() {
  read_csv(file = path(here("data", "clean", "gbd-2021-age-sex"),
                     ext = "csv")) |>
    mutate(age = as.integer(age)) |>
    create_all_tables() |>
    save_tables()
}

main()

