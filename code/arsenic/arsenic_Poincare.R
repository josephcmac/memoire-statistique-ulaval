library(tidyverse)

main_aux2 <- function(df, B) {
    ggplot(df) +
    geom_point(aes(mu, sigma)) +
    geom_label(aes(mu, sigma, label=location)) +
    labs(
      title = expression("Logarithme de l'Arsenic "~PM[2*","*5]~" moyen par État (1990-2019)"),
      subtitle = paste0("Nombre d'échantillons bootstrap : B = ", B),
      caption = "Source : EPA; Visualisation : José Manuel Rodríguez Caballero",
      x = "μ", y = "σ"
    ) +
    theme_bw()
}

main_aux <- function() {
  read_csv(fs::path(here::here("data", "clean", "arsenic_pm2.5_locations"),
                    ext="csv")) %>%
    main_aux2(df=., B = pull(., "B")|>first() )
}

main <- function() {
  png(fs::path(here::here("text", "figures", "arsenic_Poincare", "arsenic_Poincare"),
               ext = "png"), width = 1000, height = 600)
  print(main_aux())
  dev.off()
}

main()