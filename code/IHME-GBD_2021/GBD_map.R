library(tidyverse)
library(usmap)
library(here)

main_aux <- function(age0, sex0) {
  read_csv(fs::path(here::here("data", "clean", "IHME-GBD_2021_CLEAN_prevalence"),ext="csv")) %>%
    group_by(age, sex, location) |>
    summarise(mu = mean(mu)) |>
    filter(age == age0, sex == sex0) |>
    rename(state = location) %>%
    plot_usmap(data = ., values = "mu", include = c(state.abb, "DC")) +
    scale_fill_viridis_c(
      name = expression(atop(Logit[10]~"(prévalance des MRC)")),
      option = "plasma",
      n.breaks = 6
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = grid::unit(8, "cm"),
        barheight = grid::unit(0.5, "cm"),
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5),
      plot.caption = element_text(size = 8),
      legend.title = element_text(size = 10, lineheight = 1.2, hjust = 0.5),
      legend.text = element_text(size = 9)
    ) +
    labs(
      title = expression(Logit[10]~"(prévalance des MRC), Période : 1990–2019"),
      subtitle = paste0("Âge : ", age0, "; Sexe : ", ifelse(sex0 == "Male", "mâle", "femelle")),
      caption = "Source : EPA; Visualisation : José Manuel Rodríguez Caballero"
    )
}

main <- function() {
  out_file <- here::here("text", "figures", "GBD_map", "GBD_map.png")
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = out_file,
    plot = main_aux(age0 = 47, sex = "Male"),
    width = 7, 
    height = 4.5,
    dpi = 300
  )
}

main()
