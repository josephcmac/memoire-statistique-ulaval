library(tidyverse)
library(fs)
library(here)
library(xtable)

signs_of <- function(l) {
  sign(diff(l))  
}

sum_row <- function(df) {
  row.names(df) <- "Total"
  df
}

add_Sums <- function(tab, age_groups) {
  tab %>%
  colSums() %>%
  t() %>%
  as.data.frame() %>%
  sum_row() %>%
  rbind(tab,  .) %>%
    setNames(age_groups)
}

table_of <- function(df, country_list, age_groups, variable) {
  sapply(country_list, function(country0)
    df |>
      filter(country == country0) |>
      select(-country,-n_sample) |>
      pull(variable) |>
      signs_of()  
  ) |> t()  |> as.data.frame() |> add_Sums(age_groups)
}

create_all_tables <- function(df, country_list, age_groups) {
  list(
    "mu" = table_of(df=df, country_list=country_list, 
                         age_groups=age_groups, variable="mu"),
    "sigma" = table_of(df=df, country_list=country_list, 
                            age_groups=age_groups, variable="sigma")
  )
}

save_tables <- function(l) {
  lapply(names(l), function(l_name) {
    print.xtable(
      x=l[[l_name]] |> xtable(digits = 0),
      file = path(here("text", "tables", "sex", "sex_path", 
                       paste0(l_name,"_raw")), ext = "tex"),
      include.rownames = TRUE,
    )
  })
}


main <- function() {
  read_csv(file=path(here("data", "clean", "gbd-2021-sex-age"),  
                     ext = "csv")) |>
    create_all_tables(
      country_list = c("US", "UK", "NO", "IT"),
      age_groups = 5*(2:19)-3
    ) |>
    save_tables()
}

main()
