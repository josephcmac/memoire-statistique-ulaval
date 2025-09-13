logit_per_100k <- function(x) {
  log10(x) - log10(1e5 - x)
}

age_valid <- c("<5 years",
               "5-9 years",
               "10-14 years",
               "15-19 years",
               "20-24 years",
               "25-29 years",
               "30-34 years",
               "35-39 years",
               "40-44 years",
               "45-49 years",
               "50-54 years",
               "55-59 years",
               "60-64 years",
               "65-69 years",
               "70-74 years",
               "75-79 years",
               "80-84 years",
               "85-89 years",
               "90-94 years")

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
    dplyr::filter(age %in% age_valid) |>
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

remove_duplicated <- function(df) {
  df[!duplicated(df |> dplyr::select("country","location","year","age","sex")), ]
}

main <- function(country_list, output_name, measure) {
  purrr::map_dfr(
    .x = names(country_list),
    .f = function(country_name) 
      read_country_clean( 
        files=country_list[[country_name]],
        measure = measure,
        country=country_name 
      )
  ) |>
  remove_duplicated() |>
    readr::write_csv(
      file = fs::path(here::here("data", "clean", output_name), ext="csv")
      )
}

main(
  country_list = list(
    "US" = c(
      "IHME-GBD_2021_DATA-71218e0f-1",
      "IHME-GBD_2021_DATA-fa78d47f-1",
      "IHME-GBD_2021_DATA-cf8618a5-1",
      "IHME-GBD_2021_DATA-1d3bcc6b-1",
      "IHME-GBD_2021_DATA-88f8cc90-1",
      "IHME-GBD_2021_DATA-5a753dd9-1",
      "IHME-GBD_2021_DATA-2ca4dae5-1",
      "IHME-GBD_2021_DATA-549e457b-1",
      "IHME-GBD_2021_DATA-989bddbf-1",
      "IHME-GBD_2021_DATA-2105cfb1-1",
      "IHME-GBD_2021_DATA-437141d7-1",
      "IHME-GBD_2021_DATA-a6a3aac0-1",
      "IHME-GBD_2021_DATA-bc6c5104-1"
    ),
    "UK" = c("IHME-GBD_2021_DATA-b8f037ff-1"),
    "NO" = c("IHME-GBD_2021_DATA-024182b4-1", 
             "IHME-GBD_2021_DATA-98171be8-1", 
             "IHME-GBD_2021_DATA-ff447d3a-1"),
    "IT" = c("IHME-GBD_2021_DATA-4511f405-1")
  ),
  output_name = "IHME-GBD_2021_CLEAN_incidence",
  measure = "incidence"
)
