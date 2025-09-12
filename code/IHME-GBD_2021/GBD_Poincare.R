library(tidyverse)

main_aux2 <- function(df, age0, sex0) {
    ggplot(df) +
    geom_point(aes(mu, sigma)) +
    geom_label(aes(mu, sigma, label=location)) +
    labs(
      title = expression(""~Logit[10]~" (prévalence des MRC), Période : 1990-2019"),
      subtitle = paste0("Âge : ", age0, "; Sexe : ", ifelse(sex0 == "Male", "mâle", "femelle")),
      caption = "Source : EPA; Visualisation : José Manuel Rodríguez Caballero",
      x = "μ", y = "σ"
    ) +
    theme_bw()
}

main_aux <- function() {
  read_csv(fs::path(here::here("data", "clean", "IHME-GBD_2021_CLEAN_prevalence"),
                    ext="csv")) %>%
    group_by(age, sex, location) %>%
    summarise(
      mu = mean(mu),
      sigma = sqrt(sum( sigma*sigma ))/n()
    ) %>%
    filter(age == 47, sex == "Male") %>%
    main_aux2(df=., age0 = 47, sex0 = "Male")
}

main <- function() {
  png(fs::path(here::here("text", "figures", "GBD_Poincare", "GBD_Poincare"),
               ext = "png"), width = 1000, height = 600)
  print(main_aux())
  dev.off()
}

main()