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
  key2levels_name <- c(
    "Overall"          = "National Average",
    "gender"           = "Gender",
    "age_group"        = "Age Group",
    "edu_level"        = "Education Level",
    "ethnic_majority"  = "Ethnicity",
    "income"           = "Income",
    "emp_status"       = "Employment Status",
    "nationality"      = "Nationality",
    "marital_status"   = "Marital Status",
    "level_impact"     = "Impact Level",
    "cooccurence_group"= "Co-occurrent Problems",
    "disability"       = "Disability",
    "NUTS"             = "Region"
  )
  
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
      label_value = dplyr::if_else(color == "primary", 
                                   paste0(round(value * 100, 0), "%"), 
                                   NA_character_)
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
    lv_key <- key2levels_name[[k]]
    if (is.null(lv_key)) next  # sin mapeo conocido, omite silenciosamente
    
    if (!lv_key %in% names(levels_cfg)) {
      # Si falta en levels_cfg, lo omitimos con advertencia suave
      warning("No se encontró levels_cfg[['", lv_key, "']] para la clave '", k, "'. Se omite en levels_all.")
      next
    }
    
    lv_vals <- levels_cfg[[lv_key]]
    if (length(lv_vals)) {
      levels_all <- c(levels_all, paste(disp_lab, lv_vals, sep = " | "))
    }
  }
  
  # Si por alguna razón levels_all queda vacío, lo reconstituimos de lo observado en datos
  if (length(levels_all) == 0) {
    levels_all <- data2plot %>%
      dplyr::mutate(y_key = paste(grouping, level, sep = " | ")) %>%
      dplyr::pull(y_key) %>%
      unique()
  }
  
  levels_all <- unique(c("<span style='color:#575796;'><b><i>{level}</i></b></span>", levels_all))
  
  data2plot <- data2plot %>%
    dplyr::mutate(
      y_id  = paste(grouping, level, sep = " | "),
      y_lab = sprintf("<b>%s</b>", level),
      y_id  = factor(y_id, levels = levels_all),
      y_lab = if_else(grouping == " ", " ",level)
    )
  
  first_group <- levels(forcats::fct_inorder(data2plot$grouping))[1]
  
  label_df <- tibble(
    label_html = "<span style='color:#575796;font-weight:700;font-style:italic;'><b><i>National Average</i></b></span>",
    x = 0,   # posición en X (tu escala va 0–115, ponlo donde quieras)
    y = 2,   # arriba del panel
    grouping = first_group  # <-- clave: coincide con la faceta
  )
  
  # # Mapa único id -> etiqueta HTML
  # labels_map <- data2plot %>%
  #   dplyr::distinct(y_id, y_lab) %>%
  #   { setNames(.$y_lab, .$y_id) }
  # 
  
  
  p <- ggplot2::ggplot(data2plot, ggplot2::aes(x = value * 100, y = y_lab, fill = color)) +
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
    #ggplot2::scale_y_discrete(labels = labels_map) +
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
      axis.text.y = ggtext::element_markdown(size = 10,
                                     hjust = 1,
                                     family = "inter",
                                     face = "plain"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.spacing = grid::unit(12, "mm"),
      strip.text.y.left = element_text(angle = 0,
                                       size = 10,
                                       color = "#575796",
                                       hjust = 0,
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
      size = 3.5,
      family = "inter", 
      face = "bold"
    );p
  
  return(p)
}
