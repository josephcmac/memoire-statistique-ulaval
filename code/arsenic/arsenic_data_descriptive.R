df <- readr::read_csv(fs::path(here::here("data", "clean", "arsenic_pm2.5_locations"), 
                         ext = "csv"))
summary(df)

df |>
  dplyr::arrange( desc(mu) ) |>
  print(n=51)

sum(df$shapiro_p < 0.05)
sum(df$shapiro_p < 0.05/nrow(df))
