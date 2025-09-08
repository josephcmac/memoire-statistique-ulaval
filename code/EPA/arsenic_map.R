library(tidyverse)
library(usmap)
library(here)

main_aux <- function() {
  read_csv(here::here("data", "clean", "arsenic_pm2.5_locations.csv")) %>%
    rename(state = location) %>%
    plot_usmap(data = ., values = "mu", include = c(state.abb, "DC")) +
    scale_fill_viridis_c(
      name = expression(atop(Log[10]~"( Arsenic "~PM[2,5]~"("*mu*" g/ m"^3*") )")),
      option = "plasma",
      n.breaks = 6   # 5–7 bins usually works well
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
      title = expression("Logarithme de l'Arsenic "~PM[2.5]~" moyen par État (1990–2019)"),
      caption = "Source : EPA; Visualisation : José Manuel Rodríguez Caballero"
    )
}

main <- function() {
  out_file <- here::here("text", "figures", "arsenic_map", "arsenic_map.png")
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = out_file,
    plot = main_aux(),
    width = 7,   # inches
    height = 4.5,
    dpi = 300
  )
}

main()
