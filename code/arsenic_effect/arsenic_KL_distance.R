library(tidyverse)

Kullback_Leibler <- function(mu1, sigma1, mu2, sigma2) {
  log(sigma2 / sigma1) + (sigma1^2 + (mu1 - mu2)^2) / (2 * sigma2^2) - 0.5
}

df <- readr::read_csv(fs::path(here::here("data", "clean", "arsenic_regression_stats"), ext = "csv"))

df_male <- df |> filter(sex == "Male", response == "mu", coef_name == "arsenic") |>
  select(age, Estimate, MAD) |>
  mutate(MAD = 1.4826*MAD) |>
  rename(mu := Estimate, sigma := MAD)

df_female <- df |> filter(sex == "Female", response == "mu", coef_name == "arsenic") |>
  select(age, Estimate, MAD) |>
  mutate(MAD = 1.4826*MAD) |>
  rename(mu := Estimate, sigma := MAD)


d_male <- sapply(2:nrow(df_male), function(i) 
  Kullback_Leibler(
    mu1=df_male$mu[i-1], sigma1=df_male$sigma[i-1],
    mu2=df_male$mu[i], sigma2=df_male$sigma[i]))

d_female <- sapply(2:nrow(df_female), function(i) 
  Kullback_Leibler(
    mu1=df_female$mu[i-1], sigma1=df_female$sigma[i-1],
    mu2=df_female$mu[i], sigma2=df_female$sigma[i]))



u <- data.frame(
  age = tail(df_male,-1)$age,
  u_male = cumsum(d_male),
  u_female = cumsum(d_female)
)



ggplot(u) +
  geom_point(aes(age, u_male), color="red") +
  geom_line(aes(age, u_male), color="red") +
  geom_point(aes(age, u_female), color="blue") +
  geom_line(aes(age, u_female), color="blue") +
  labs(
    title = "Divergence de Kullback-Leibler cumulée entre pentes consécutives",
    subtitle = "Mâles : rouge; Femelles : bleu",
    x = "Tranche d'âge (± 2 ans)",
    y = "Divergence de Kullback-Leibler cumulée",
    caption = "Source : EPA & IHME; Visualisation : José Manuel Rodríguez Caballero"
  ) +
  theme_bw()
