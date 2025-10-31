data2plot <- data2plot %>%
  mutate(
    level = if_else(
      level == "National Average",
      " ",
      level
    )
  )

# 1. Crear fila adicional para la etiqueta "National Average"
label_df <- tibble(
  label_html = "<b><i><span style='color:#575796;'>National Average</span></i></b>",
  x = 2,  # posición horizontal (ajusta según tu escala)
  y = 2.5   # la colocamos arriba del panel
)

ggplot2::ggplot(
  data2plot, 
  ggplot2::aes(x = value * 100, y = level, fill = color)) +
  ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE), width = 0.9, na.rm = TRUE) +
  ggplot2::geom_text(
    ggplot2::aes(x = 101, label = label_value),
    family = "inter", fontface = "bold", color = "#575796",
    hjust = 0, size = 3.5, na.rm = TRUE
  ) +
  ggplot2::facet_grid(
    rows = ggplot2::vars(grouping),
    scales = "free", space = "free_y", switch = "y"
  ) +
  ggplot2::scale_fill_manual(values = c("primary" = "#575796", "secondary" = "#e5e8e8")) +
  ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, 115), position = "top") +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    # clave: estiliza el lado derecho (por el switch = "y")
    axis.text.y = ggtext::element_markdown(size = 15, hjust = 1,
                                           family = "inter", face = "plain"),
    # opcional: oculta el lado izquierdo si aparece
    #axis.text.y.left  = ggplot2::element_blank()
  ) +
  geom_richtext(
    data = label_df,
    aes(x = x, y = y, label = label_html),
    inherit.aes = FALSE,
    fill = NA, label.color = NA, # sin fondo ni borde
    vjust = 1.5, hjust = 1,
    size = 5,
    family = "inter"
  )
