library(tidyverse)
library(fs)
library(here)
library(xtable)

create_table_aux <- function(df, u, v) {
  df |>
    mutate(
      val = mu,
      lower = mu + u*sigma, 
      upper = mu + v*sigma
    ) |>
    select(-mu,-sigma)
}

create_table <- function(df, country0, alpha) {
  create_table_aux(
    df =  df |>
      filter(country == country0) |>
      select(age, mu, sigma), 
    u = qnorm(alpha/2), 
    v = qnorm(1-alpha/2)
    )
}

create_all_tables <- function(df) {
  list(
    "US" = create_table(df=df, country0 = "US", alpha = 0.05),
    "UK" = create_table(df=df, country0 = "UK", alpha = 0.05),
    "NO" = create_table(df=df, country0 = "NO", alpha = 0.05),
    "IT" = create_table(df=df, country0 = "IT", alpha = 0.05)
  )
}


save_tables <- function(l) {
  lapply(names(l), function(l_name) {
    print.xtable(
        x=l[[l_name]]|>
          xtable(),
        file = path(here("text","tables", "sex", "sex_tables_country-age", 
                         paste0(l_name, "_raw")), ext = "tex"),
        include.rownames = FALSE
        )
  })
}

main <- function() {
  read_csv(file = path(here("data", "clean", "gbd-2021-sex-age"),
                     ext = "csv")) |>
    mutate(age = as.integer(age)) |>
    create_all_tables() |>
    save_tables()
}

main()

