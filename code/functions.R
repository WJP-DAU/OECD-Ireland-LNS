## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            OECD Ireland LNS Report - functions
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
## 1.  Required functions                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ----------------------------------------
# Helper interno: etiquetar por sufijo numérico usando labels_vec
# - category: nombres de columnas
# - labels_vec: vector de labels indexado (1..K)
# - regex_suffix: regex para extraer el número al final o tras "_"
# ----------------------------------------

.label_from_suffix <- function(category, labels_vec,
                               regex_suffix = "(?<=_)\\d+(?=(_[A-Za-z].*)?$)") {
  suff_idx <- stringr::str_extract(category, regex_suffix)
  
  # Fallback si no encontró nada con el regex "inteligente"
  if (any(is.na(suff_idx))) {
    suff_idx2 <- stringr::str_extract(category, "\\d+")
    # Reemplaza donde estaba NA por el fallback
    suff_idx[is.na(suff_idx)] <- suff_idx2[is.na(suff_idx)]
  }
  
  # Si aún quedan NA, informar exactamente cuáles columnas fallaron
  if (any(is.na(suff_idx))) {
    bad <- category[is.na(suff_idx)]
    stop(
      "No pude extraer sufijos numéricos de: ",
      paste0(bad, collapse = ", "),
      ". Provee labels_map o ajusta el regex_suffix."
    )
  }
  
  idx <- as.integer(suff_idx)
  if (max(idx, na.rm = TRUE) > length(labels_vec)) {
    stop("labels_vec no alcanza para el mayor índice encontrado (", max(idx, na.rm = TRUE), ").")
  }
  labels_vec[idx]
}

# ---------------------------
# Helper para filtrar grupos
# ---------------------------
select_groups <- function(cfg, ...) {
  keys <- c(...)
  cfg[intersect(keys, names(cfg))]
}

# ----------------------------------------
# Helper para definir una "medida/plot"
# (id, expresión de valor, y preset de grupos)
# ----------------------------------------
add_measure <- function(id, value, groups_preset) {
  list(id = id, value = enquo(value), groups_preset = groups_preset)
}

summarize_by_vars <- function(data,
                              value,
                              group_cfg,   
                              include_overall = TRUE) {
  val <- rlang::ensym(value)
  
  # Deriva los grupos desde group_cfg (fuente única) y valida que existan en data
  groups <- setdiff(names(group_cfg), "Overall")
  groups <- intersect(groups, names(data))
  grp_syms <- rlang::syms(groups)
  
  # Overall
  overall_n <- sum(!is.na(rlang::eval_tidy(val, data)))
  overall_mean <- if (overall_n > 0) mean(rlang::eval_tidy(val, data), na.rm = TRUE) else NA_real_
  
  # Por grupo
  out <- purrr::map_dfr(grp_syms, function(g) {
    data %>%
      dplyr::filter(!is.na(!!g)) %>%
      dplyr::group_by(!!g) %>%
      dplyr::summarise(
        mean = mean(!!val, na.rm = TRUE),
        n    = sum(!is.na(!!val)),
        .groups = "drop"
      ) %>%
      dplyr::rename(level = !!g) %>%
      dplyr::mutate(
        grouping     = rlang::as_string(g),
        overall_mean = overall_mean,
        overall_n    = overall_n,
        diff_overall = mean - overall_mean,
        .before = 1
      )
  })
  
  overall_row <- tibble::tibble(
    grouping     = "Overall",
    level        = "All",
    mean         = overall_mean,
    n            = overall_n,
    overall_mean = overall_mean,
    overall_n    = overall_n,
    diff_overall = 0
  )
  
  result <- if (include_overall) dplyr::bind_rows(overall_row, out) else out
  
  # Chequeo
  check <- purrr::map_dfr(grp_syms, function(g) {
    data %>%
      dplyr::filter(!is.na(!!g)) %>%
      dplyr::group_by(!!g) %>%
      dplyr::summarise(test_mean = mean(!!val, na.rm = TRUE), .groups = "drop") %>%
      dplyr::rename(level = !!g) %>%
      dplyr::mutate(grouping = rlang::as_string(g), .before = 1)
  })
  
  comp <- result %>%
    dplyr::filter(grouping != "Overall") %>%
    dplyr::select(grouping, level, mean) %>%
    dplyr::left_join(check, by = c("grouping", "level")) %>%
    dplyr::mutate(final = mean - test_mean)
  
  tol <- 1e-9
  ok <- all(abs(tidyr::replace_na(comp$final, Inf)) < tol)
  print(if (ok) "DONE ✅" else "MISMATCH ❌")
  
  return(result)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2. Group bars functions                                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

compute_groupbars_tables <- function(data, params) {
  stopifnot(is.list(params), !is.null(params$measures))
  
  tbls <- map(params$measures, function(m) {
    summarize_by_vars(
      data      = data,
      value     = !!m$value,
      group_cfg = params$groups_presets[[m$groups_preset]]
    )
  })
  
  names(tbls) <- vapply(params$measures, function(m) m$id, FUN.VALUE = character(1))
  tbls
}


# ===================================
# GRÁFICAS (plot + save)
#     Input: tables (lista), params (solo config),
#            out_dir, file_ext, use_outcome_dir,
#            default sizes y overrides, y selección de medidas a graficar
#     Output: lista de ggplots guardados en disco
# ===================================
render_groupbars_plots <- function(
    tables,
    params,
    out_dir,
    file_ext = "svg",
    use_outcome_dir = FALSE,
    measures_to_plot = NULL,          # c("prevalence","timeliness", ...) o NULL = todas
    default_width = 300,
    default_height = 350,
    size_overrides = list()           # p.ej. list(prevalence = list(width=300,height=280))
) {
  stopifnot(is.list(params), !is.null(params$measures))
  
  # Resolver carpeta de salida
  out_dir <- normalizePath(out_dir, mustWork = FALSE)
  if (use_outcome_dir) out_dir <- file.path(out_dir, "outcome")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Qué ids graficar
  ids_all <- names(tables)
  ids <- if (is.null(measures_to_plot)) ids_all else intersect(measures_to_plot, ids_all)
  
  # lookups
  file_for_id <- function(id) file.path(out_dir, paste0(id, ".", file_ext))
  size_for_id <- function(id) {
    if (id %in% names(size_overrides)) {
      w <- size_overrides[[id]]$width  %||% default_width
      h <- size_overrides[[id]]$height %||% default_height
      c(w, h)
    } else c(default_width, default_height)
  }
  # helper: encontrar preset del id
  preset_for_id <- function(id) {
    keep(params$measures, ~ .x$id == id)[[1]]$groups_preset
  }
  
  plots <- map(ids, function(id) {
    df_sum <- tables[[id]]
    preset <- preset_for_id(id)
    group_cfg <- params$groups_presets[[preset]]
    
    p <- plot_by_group(
      data_frame = df_sum,
      group_cfg  = group_cfg,
      levels_cfg = params$levels_map
    )
    
    sz <- size_for_id(id)
    ggplot2::ggsave(
      plot     = p,
      filename = file_for_id(id),
      width    = sz[1],
      height   = sz[2],
      units    = "mm",
      scale    = 0.75
    )
    p
  })
  
  names(plots) <- ids
  plots
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3. Multiresponse bars functions                                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

compute_bars_tables <- function(data, params) {
  stopifnot(is.list(params), !is.null(params$blocks))
  
  out <- map(params$blocks, function(b) {
    stopifnot(all(b$cols %in% names(data)))
    
    # 1) agregación (promedio * 100)
    agg <- data %>%
      summarize(across(all_of(b$cols), ~ mean(.x, na.rm = TRUE) * 100)) %>%
      pivot_longer(cols = everything(), names_to = "category", values_to = "values")
    
    # 2) etiquetas
    if (!is.null(b$labels_map_id)) {
      lbl_map <- params$labels_map_lib[[b$labels_map_id]]
      if (is.null(lbl_map)) stop("labels_map_id '", b$labels_map_id, "' no existe en labels_map_lib.")
      agg <- agg %>%
        mutate(category_label = dplyr::recode(category, !!!lbl_map, .default = category))
    } else if (!is.null(b$labels_vec_id)) {
      lbl_vec <- params$labels_vec_lib[[b$labels_vec_id]]
      if (is.null(lbl_vec)) stop("labels_vec_id '", b$labels_vec_id, "' no existe en labels_vec_lib.")
      agg <- agg %>%
        mutate(category_label = .label_from_suffix(category, lbl_vec))
    } else {
      stop("Debes definir labels_map_id o labels_vec_id para el bloque '", b$id, "'.")
    }
    
    # 3) data final para graficar
    df_plot <- agg %>%
      arrange(desc(values)) %>%
      mutate(
        labels   = paste0(format(round(values, 0), nsmall = 0), "%"),
        lab_pos  = values,
        color    = "standard",
        order_no = row_number()
      ) %>%
      select(category = category_label,
             values2plot = values,
             lab_pos, labels, color, order_no)
    
    # 4) top_n si aplica
    if (is.finite(b$top_n)) {
      df_plot <- df_plot %>% slice_head(n = b$top_n)
    }
    
    df_plot
    
  })
  
  names(out) <- vapply(params$blocks, `[[`, character(1), "id")
  out
  
}

# ===================================
# GRÁFICAS (plot + save)
#     - Input: tables (lista), params (no usado aquí salvo consistencia),
#              out_dir, file_ext, default sizes y overrides, selección de ids
#     - Output: lista de ggplots guardados
# ===================================
render_bars_plots <- function(
    tables,
    params,
    out_dir,
    file_ext = "svg",
    default_width = 300,
    default_height = 350,
    size_overrides = list(),          # p.ej., list(drm_contacted = list(width=300,height=350))
    ids_to_plot = NULL,               # c("drm_contacted","adviser") o NULL = todas
    scale = 0.75
) {
  out_dir <- normalizePath(out_dir, mustWork = FALSE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  ids_all <- names(tables)
  ids <- if (is.null(ids_to_plot)) ids_all else intersect(ids_to_plot, ids_all)
  
  file_for_id <- function(id) file.path(out_dir, paste0(id, ".", file_ext))
  size_for_id <- function(id) {
    if (id %in% names(size_overrides)) {
      w <- size_overrides[[id]]$width  %||% default_width
      h <- size_overrides[[id]]$height %||% default_height
      c(w, h)
    } else c(default_width, default_height)
  }
  
  plots <- map(ids, function(id) {
    data_plot <- tables[[id]]
    
    p <- WJPr::wjp_bars(
      data      = data_plot,
      target    = "values2plot",
      grouping  = "category",
      order     = "order_no",
      direction = "horizontal",
      colors    = "color",
      cvec      = c("standard" = "#575796")
    ) +
      geom_text(
        mapping = aes(y = lab_pos + 7, label = labels),
        size     = 5,
        color    = "#1a1a1a",
        family   = "inter",
        fontface = "bold.italic"
      ) +
      theme(
        axis.text.x = element_text(size = 18, family = "inter", face = "plain", color = "#1a1a1a"),
        axis.text.y = element_text(size = 18, hjust = 0, family = "inter", face = "plain", color = "#1a1a1a")
      )
    
    sz <- size_for_id(id)
    ggsave(
      filename = file_for_id(id),
      plot     = p,
      width    = sz[1],
      height   = sz[2],
      units    = "mm",
      scale    = scale
    )
    p
  })
  
  names(plots) <- ids
  plots
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4. Heatmap functions                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tables_drm <- function(data) {
  
  data %>%
    summarise(
      across(
        starts_with("drm"),
        ~ mean(.x, na.rm = TRUE)
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to  = "variable",
      values_to = "value"
    ) %>%
    # excluimos drm_11 si no se quiere en el heatmap
    filter(!str_detect(variable, "drm_11")) %>%
    mutate(
      category = case_when(
        str_detect(variable, "efficiency") ~ "Efficient\nprocess",
        str_detect(variable, "fairness")   ~ "Fair\nprocess",
        str_detect(variable, "affordable") ~ "Affordable\nprocess",
        str_detect(variable, "duration")   ~ "Timely\nprocess",
        str_detect(variable, "helpful")    ~ "Helpful\nmechanism"
      ),
      actor = case_when(
        str_detect(variable, "drm_1") ~ "Court",
        str_detect(variable, "drm_2") ~ "Tribunal",
        str_detect(variable, "drm_3") ~ "Ombudsman",
        str_detect(variable, "drm_4") ~ "Police",
        str_detect(variable, "drm_5") ~ "Formal\nmediation",
        str_detect(variable, "drm_6") ~ "Lawyer",
        str_detect(variable, "drm_7") ~ "Government\ndepartment",
        str_detect(variable, "drm_8") ~ "Community\nleader",
        str_detect(variable, "drm_9") ~ "Other DRM"
      )
    ) %>%
    # ordenamos factores (categorías y actores)
    mutate(
      category = factor(
        category,
        levels = c(
          "Efficient\nprocess",
          "Fair\nprocess",
          "Affordable\nprocess",
          "Timely\nprocess",
          "Helpful\nmechanism"
        )
      ),
      actor = factor(
        actor,
        levels = c(
          "Other DRM",
          "Community\nleader",
          "Government\ndepartment",
          "Lawyer",
          "Formal\nmediation",
          "Police",
          "Ombudsman",
          "Tribunal",
          "Court"
        )
      )
    )
}

plot_drm_heatmap <- function(DRM_results,
                             outfile = file.path(path2SP, "output/drm_heatmap.svg"),
                             width  = 300,
                             height = 250) {
  
  # 1) Rango dinámico (excluyendo NA)
  min_val <- min(DRM_results$value, na.rm = TRUE)
  max_val <- max(DRM_results$value, na.rm = TRUE)
  
  # 2) Función interna para el color del texto (contraste)
  text_color <- function(val) {
    # punto medio del rango observado
    mid <- min_val + (max_val - min_val) / 2
    ifelse(val > mid, "white", "#1a1a1a")
  }
  
  # 3) Construir el heatmap
  p_drm_heatmap <- ggplot(
    DRM_results,
    aes(x = category, y = actor, fill = value)
  ) +
    geom_tile(color = "white", linewidth = 0.8) +
    
    # Texto dentro de cada celda con contraste dinámico
    geom_text(
      aes(
        label = scales::percent(value, accuracy = 1),
        color = text_color(value)
      ),
      size = 4,
      fontface = "bold",
      family = "inter"
    ) +
    scale_color_identity() +   # permite usar los colores tal cual vienen de text_color()
    
    # Gradiente dinámico basado en min/max observados
    scale_fill_gradient(
      name   = "Share",
      limits = c(min_val, max_val),
      labels = scales::percent_format(accuracy = 1),
      low    = "#E3E4F5",    # tono claro de la marca
      high   = "#575796"     # color de marca
    ) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.position = "none",  # sin leyenda
      axis.text.x.top = element_text(
        angle  = 0,
        hjust  = 0.5,
        family = "inter",
        face   = "bold",
        size   = 12
      ),
      axis.text.y = element_text(
        family = "inter",
        face   = "bold",
        size   = 12,
        hjust  = 0
      ),
      legend.title = element_text(
        family = "inter",
        face   = "bold"
      ),
      legend.text = element_text(
        family = "inter"
      ),
      panel.grid = element_blank()
    )
  

  ggsave(
    plot    = p_drm_heatmap,
    filename = outfile,
    device  = "svg",
    width   = width,
    height  = height,
    units   = "mm"
  )
  
  return(p_drm_heatmap)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. Co-ocurrence plot                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot_coocurrence_bars <- function(tables,
                                  params,
                                  filename    = file.path(path2SP, "output/drm_heatmap.svg"),
                                  facet_order = NULL,
                                  width_mm    = 300,
                                  height_mm   = 280) {
  
  # 1. Facet order y labels
  if (is.null(facet_order)) {
    facet_order <- names(params[["full_group_cfg"]])
  }
  facet_labels <- params[["full_group_cfg"]]  # named vector: internal_code → pretty label
  
  # 2. Data para el plot
  co_ocurr <- tables[["ndisputes"]] %>%
    select(grouping, level, value = mean) %>%
    mutate(
      grouping = factor(grouping, levels = facet_order)
    )
  
  # 3. Máximo global para la barra de fondo
  max_val <- max(co_ocurr$value, na.rm = TRUE)
  
  # 4. Plot
  p <- ggplot(co_ocurr,
              aes(x = level, y = value)) +
    
    # Background bar (gris)
    geom_col(
      aes(y = max_val),
      fill  = "#E6E8E6",
      width = 0.9
    ) +
    
    # Main bar (morado)
    geom_col(
      fill  = "#4F4A8C",
      width = 0.9
    ) +
    
    # Texto
    geom_text(
      aes(label = round(value, 1)),
      family   = "inter",
      fontface = "bold",
      color    = "#575796",
      hjust    = 0,
      size     = 5
    ) +
    
    coord_flip(clip = "off") +
    
    facet_grid(
      rows    = vars(grouping),
      scales  = "free",
      space   = "free_y",
      switch  = "y",
      labeller = labeller(grouping = facet_labels)
    ) +
    
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    
    theme_minimal() +
    theme(
      strip.placement       = "outside",
      strip.background      = element_blank(),
      axis.title.x          = element_blank(),
      axis.title.y          = element_blank(),
      axis.text.x           = element_blank(),
      axis.text.y           = ggtext::element_markdown(
        size = 16, hjust = 1, family = "inter"
      ),
      panel.grid.major.y    = element_blank(),
      panel.grid.major.x    = element_blank(),
      panel.grid.minor.x    = element_blank(),
      panel.spacing         = unit(12, "mm"),
      strip.text.y.left     = element_text(
        angle  = 0,
        size   = 16,
        color  = "#575796",
        hjust  = 1,
        vjust  = 1,
        family = "inter",
        face   = "bold",
        margin = margin(-20, -35, 0, 55)
      ),
      strip.switch.pad.grid = unit(-35, "mm"),
      strip.clip            = "off",
      legend.position       = "none"
    )
  
  # 6. Save
  ggsave(
    filename = filename,
    plot     = p,
    device   = "svg",
    width    = width_mm,
    height   = height_mm,
    units    = "mm"
  )
  
  return(p)
}

