logit_per_100k <- function(x) {
  log10(x) - log10(1e5 - x)
}

number_from_age <- function(age) {
  c(
    "<5 years" = 2,
    "5-9 years" = 7,
    "10-14 years" = 12,
    "15-19 years" = 17,
    "20-24 years" = 22,
    "25-29 years" = 27,
    "30-34 years" = 32,
    "35-39 years" = 37,
    "40-44 years" = 42,
    "45-49 years" = 47,
    "50-54 years" = 52,
    "55-59 years" = 57,
    "60-64 years" = 62,
    "65-69 years" = 67,
    "70-74 years" = 72,
    "75-79 years" = 77,
    "80-84 years" = 82,
    "85-89 years" = 87,
    "90-94 years" = 92
  )[age]
}

get_mu_sigma_aux <- function(df, k=0.255106728462327014828) {
  df |> dplyr::mutate(
    mu = 0.5*(Upper + Lower),
    sigma = k*(Upper - Lower)
  ) |>
    dplyr::select(-Lower, -Upper)
}

get_mu_sigma <- function(df) {
  df |> 
    dplyr::mutate(
      Lower = logit_per_100k(lower),
      Upper = logit_per_100k(upper)
  ) |>
    dplyr::select(-lower, -upper) |>
    get_mu_sigma_aux()
}

data_cleaning <- function(df, country) {
  df |>
    dplyr::select(-val) |>
    dplyr::mutate(
      country = country,
      age = as.integer(number_from_age(age)),
      year = as.integer(year)
  ) |>
    get_mu_sigma() |>
    dplyr::select(country, location, year, age, sex, mu, sigma)
}

read_file <- function(filename, measure, country) {
  readr::read_csv(fs::path(here::here("data", "raw", "IHME-GBD_2021", 
              measure, country, filename, filename),
                           ext = "csv"))
}

read_country_clean <- function(files, measure, country) {
  purrr::map_dfr(
    .x = files,
    .f = function(file) 
      read_file(filename = file,
                measure = measure,
                country = country) |> 
      data_cleaning(country=country)
  )
}

main <- function(country_list, output_name) {
  purrr::map_dfr(
    .x = names(country_list),
    .f = function(country_name) 
      read_country_clean( 
        files=country_list[[country_name]],
        measure = "prevalence",
        country=country_name 
      )
  ) |>
    readr::write_csv(
      file = fs::path(here::here("data", "clean", output_name), ext="csv")
      )
}

main(
  country_list = list(
    "US" = c("IHME-GBD_2021_DATA-8d7180df-1",
             "IHME-GBD_2021_DATA-032c907b-1",
             "IHME-GBD_2021_DATA-77da5abb-1",
             "IHME-GBD_2021_DATA-86bd88fa-1", 
             "IHME-GBD_2021_DATA-43538444-1",
             "IHME-GBD_2021_DATA-a2e0c894-1",
             "IHME-GBD_2021_DATA-ef3a720a-1")
  ),
  output_name = "IHME-GBD_2021_CLEAN_prevalence"
)
