## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            OECD Ireland LNS Report - Bar Viz
##
## Author(s):         Natalia Rodriguez   (nrodriguez@worldjusticeproject.org)
##                    Santiago Pardo      (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot_by_group <- function(data_frame,
                          group_cfg  = full_group_cfg,
                          levels_cfg = levels_map
) {
  force(group_cfg); force(levels_cfg)
  
  # Mapa clave -> etiqueta de levels_cfg (fuente común)
  key2levels_name <- params$full_group_cfg
  
  # 1) Preparación de datos
  facet_order <- unname(group_cfg)  
  facet_order2 <- unique(c(" ", facet_order))
  
  data2plot <- data_frame %>%
    dplyr::mutate(
      primary = mean,
      secondary = 1 - mean,
      grouping = case_when(
        grouping == "Overall" ~ " ",
        TRUE ~ grouping
        ),
      level = case_when(
        grouping == " " ~ "National Average",
        TRUE ~ level
        )) %>%
    tidyr::pivot_longer(c(primary, secondary), 
                        names_to = "color", 
                        values_to = "value") %>%
    dplyr::mutate(
      grouping = dplyr::recode(grouping, !!!group_cfg),  # recodifica con group_cfg (no global)
      grouping = factor(grouping, levels = facet_order2),
      label_value = dplyr::if_else(
        color == "primary",
        dplyr::if_else(
          is.na(value),
          "NA",
          paste0(round(value * 100, 0), "%")
        ),
        NA_character_
      )
    ) 
  
  # Grupos realmente presentes en los datos (tras recodificación)
  groups_present <- stats::na.omit(unique(as.character(data2plot$grouping)))
  
  # 2) Niveles dinámicos por grupo
  #    - Usa levels_cfg cuando exista la entrada
  #    - Para NUTS/Region, usa niveles detectados en los datos
  #    - Solo arma entradas para grupos presentes
  levels_all <- character(0)
  
  for (k in names(group_cfg)) {
    disp_lab <- group_cfg[[k]]                      # etiqueta mostrada (p. ej., "Gender")
    if (!disp_lab %in% groups_present) next        # si ese facet no aparece en datos, omite
    
    if (k == "NUTS") {
      # Region desde datos
      levels_region <- data2plot %>%
        dplyr::filter(grouping == disp_lab) %>%
        dplyr::distinct(level) %>%
        dplyr::arrange(level) %>%
        dplyr::pull(level)
      if (length(levels_region)) {
        levels_all <- c(levels_all, paste(disp_lab, levels_region, sep = " | "))
      }
      next
    }
    
    # Para el resto, busca el nombre de niveles en levels_cfg
    lv_key <- key2levels_name[[k]]   # p.ej. "Income"
    if (is.null(lv_key)) next
    
    if (!lv_key %in% names(levels_cfg)) {
      warning("No se encontró levels_cfg[['", lv_key, "']] para la clave '", k, "'. Se omite en levels_all.")
      next
    }
    
    # Orden deseado desde levels_cfg
    lv_vals_ordered <- levels_cfg[[lv_key]]
    
    # Niveles realmente presentes en los datos para este grouping
    present_vals <- data2plot %>%
      dplyr::filter(grouping == disp_lab) %>%
      dplyr::distinct(level) %>%
      dplyr::pull(level)
    
    # Usar intersección, respetando el orden de levels_cfg
    lv_use <- intersect(lv_vals_ordered, present_vals)
    
    if (length(lv_use)) {
      levels_all <- c(levels_all, paste(disp_lab, lv_use, sep = " | "))
    }
  }
  
  # Si por alguna razón levels_all queda vacío, reconstituimos de lo observado
  if (length(levels_all) == 0) {
    levels_all <- data2plot %>%
      dplyr::mutate(y_key = paste(grouping, level, sep = " | ")) %>%
      dplyr::pull(y_key) %>%
      unique()
  }
  
  levels_all <- c(" ", levels_all)
  
  data2plot <- data2plot %>%
    dplyr::mutate(
      y_id  = paste(grouping, level, sep = " | "),
      y_lab = sprintf("<b>%s</b>", level),
      y_id  = if_else(grouping == " ", " ", y_id),
      y_id  = factor(y_id, levels = levels_all),
      # sin markup en la etiqueta final, solo texto plano (ggtext lo formatea)
      y_lab = dplyr::if_else(grouping == " ", " ", level)
    )
  
  first_group <- levels(forcats::fct_inorder(data2plot$grouping))[1]
  
  label_df <- tibble(
    label_html = "<span style='color:#575796;font-weight:700;font-style:italic;'><b><i>National Average</i></b></span>",
    x = 0,   # posición en X 
    y = 1.75,   # arriba del panel
    grouping = first_group  # <-- clave: coincide con la faceta
  )
  
  
  p <- ggplot2::ggplot(data2plot, ggplot2::aes(x = value * 100, y = y_id, fill = color)) +
    ggplot2::geom_col(position = ggplot2::position_stack(reverse = TRUE), width = 0.9, na.rm = TRUE) +
    ggplot2::geom_text(
      ggplot2::aes(x = 101, label = label_value),
      family = "inter", fontface = "bold", color = "#575796",
      hjust = 0, size = 5, na.rm = TRUE
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(grouping),
      scales = "free", space = "free_y", switch = "y"
    ) +
    #ggplot2::scale_y_discrete(labels = labels_map) +
    ggplot2::scale_fill_manual(values = c("primary" = "#575796", "secondary" = "#e5e8e8")) +
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, 115), position = "top") +
    scale_y_discrete(
      labels = function(x) sub("^.* \\| ", "", x)  # Remueve "Grouping | " del inicio
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = ggtext::element_markdown(size = 14,
                                     hjust = 1,
                                     family = "inter",
                                     face = "plain"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.spacing = grid::unit(12, "mm"),
      strip.text.y.left = element_text(angle = 0,
                                       size = 16,
                                       color = "#575796",
                                       hjust = 1,
                                       vjust = 1,
                                       family = "inter",
                                       face = "bold",
                                       margin = margin(-20,-35,0,55)),
      strip.switch.pad.grid = grid::unit(-35, "mm"),
      strip.clip = "off",
      legend.position = "none"
    ) +
    geom_richtext(
      data = label_df,
      aes(x = x, y = y, label = label_html),
      inherit.aes = FALSE,
      fill = NA, label.color = NA, # sin fondo ni borde
      vjust = 1.5, hjust = 1,
      size = 5.623357,
      family = "inter" 
    );p
  
  return(p)
}
