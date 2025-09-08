LOCATIONS <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
               "Colorado", "Connecticut", "Delaware", "District of Columbia",
               "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
               "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
               "Massachusetts", "Michigan", "Minnesota", "Mississippi",
               "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
               "New Jersey", "New Mexico", "New York", "North Carolina",
               "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
               "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
               "Texas", "Utah", "Vermont", "Virginia", "Washington",
               "West Virginia", "Wisconsin", "Wyoming")


EXCLUDE_REGIONS <- c("Puerto Rico", "Virgin Islands", "Country Of Mexico")

correct_DC <- function(x) {
  ifelse(x == "District Of Columbia", "District of Columbia", x)
}
read_year <- function(year, exclude_regions = EXCLUDE_REGIONS) {
  
  readr::read_csv(fs::path(here::here("data", "raw", "daily_HAPS",
                                  paste0("daily_HAPS_", year)), ext="csv")) |>
    janitor::clean_names() |>
    dplyr::filter(
      units_of_measure == "Micrograms/cubic meter (LC)",
      sample_duration == "24 HOUR"
    ) |>
    dplyr::select("state_name", "x1st_max_value") |>
    dplyr::filter(!(state_name %in% exclude_regions)) |>
    dplyr::mutate(
      state_name = correct_DC(state_name)
    ) |>
    dplyr::rename(location := state_name, value := x1st_max_value)
}

read_all <- function(year_start, year_end) {
  purrr::map_dfr(year_start:year_end, read_year)
}

bootstrapping <- function(df, seed) {
  withr::with_seed(
    seed = seed,
    code = df[sample(1:nrow(df), size=nrow(df), replace = TRUE),]
  )
}

sample_df <- function(df, seed) {
  code = df |>
    bootstrapping(seed) |>
    dplyr::group_by(location) |>
    dplyr::summarise(value = log(mean(value)))
}

bootstrapping_estimation <- function(M, B, l_states = LOCATIONS) {
  data.frame(
    location = l_states,
    mu = apply(M, 1, function(x) mean(x)),
    sigma = apply(M, 1, function(x) sd(x)),
    B = B,
    shapiro_W = apply(M, 1, function(x) shapiro.test(x) |> 
                        getElement("statistic")),
    shapiro_p = apply(M, 1, function(x) shapiro.test(x) |> 
                        getElement("p.value"))
  )
}

process <- function(df, B, seed=123) {
  withr::with_seed(
    seed=seed,
    code = sapply(sample(1:(100*B), size=B, replace = FALSE),
                  function(b) sample_df(df, seed = b) |>
                    dplyr::pull("value"))
  )
}

main <- function(B = 1000) {
  process(
    df = read_all(1990, 2019),
    B = B
  ) |>
    bootstrapping_estimation(B = B) |>
    readr::write_csv(
      file = fs::path(here::here("data", "clean", "arsenic_pm2.5_locations"), 
                      ext = "csv")
    )
}

main()
