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
  overall_mean <- if (overall_mean == 1) overall_n / 2402 else overall_mean

  # Por grupo
  out <- purrr::map_dfr(grp_syms, function(g) {
    data %>%
      dplyr::filter(!is.na(!!g)) %>%
      ungroup() %>%
      dplyr::group_by(!!g) %>%
      dplyr::summarise(
        mean = mean(!!val, na.rm = TRUE),
        n = sum(!is.na(!!val)),
        .groups = "drop"
      ) %>%
      dplyr::rename(level = !!g) %>%
      dplyr::mutate(
        grouping = rlang::as_string(g),
        overall_mean = overall_mean,
        overall_n = overall_n,
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
    dplyr::mutate(
      # Treat NaN == NaN as a match (both are NaN because n=0, which is expected)
      final = dplyr::case_when(
        is.nan(mean) & is.nan(test_mean) ~ 0,
        TRUE ~ mean - test_mean
      )
    )

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
    ) %>%
      mutate(
        mean = if_else(n < 30, NA_real_, mean),
        overall_mean = if_else(overall_n < 30, NA_real_, overall_mean),
        diff_overall = if_else(n < 30, NA_real_, diff_overall)
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
  measures_to_plot = NULL, # c("prevalence","timeliness", ...) o NULL = todas
  default_width = 300,
  default_height = 350,
  size_overrides = list() # p.ej. list(prevalence = list(width=300,height=280))
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
      w <- size_overrides[[id]]$width %||% default_width
      h <- size_overrides[[id]]$height %||% default_height
      c(w, h)
    } else {
      c(default_width, default_height)
    }
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

    # ---- 0) número de observaciones por categoría (columna) ----
    n_obs_df <- data %>%
      summarize(across(all_of(b$cols), ~ sum(!is.na(.x)))) %>%
      pivot_longer(cols = everything(), names_to = "category", values_to = "n_obs")

    # ---- 1) agregación (promedio * 100) ----
    agg <- data %>%
      summarize(across(all_of(b$cols), ~ mean(.x, na.rm = TRUE) * 100)) %>%
      pivot_longer(cols = everything(), names_to = "category", values_to = "values") %>%
      left_join(n_obs_df, by = "category") # <<< añadir n_obs por categoría

    # ---- 2) etiquetas ----
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

    # ---- 3) data final para graficar ----
    df_plot <- agg %>%
      arrange(desc(values)) %>%
      mutate(
        labels   = paste0(format(round(values, 0), nsmall = 0), "%"),
        lab_pos  = values,
        color    = "standard",
        order_no = row_number()
      ) %>%
      select(
        category = category_label,
        values2plot = values,
        lab_pos, labels, color, order_no, n_obs
      ) %>%
      mutate(
        values2plot = if_else(n_obs < 30, NA_real_, values2plot),
        lab_pos = if_else(n_obs < 30, NA_real_, lab_pos),
        labels = if_else(n_obs < 30, NA_character_, labels),
        order_no = if_else(n_obs < 30, NA_integer_, order_no)
      )

    # ---- 4) top_n si aplica ----
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
  size_overrides = list(), # p.ej., list(drm_contacted = list(width=300,height=350))
  ids_to_plot = NULL, # c("drm_contacted","adviser") o NULL = todas
  scale = 0.75
) {
  out_dir <- normalizePath(out_dir, mustWork = FALSE)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  ids_all <- names(tables)
  ids <- if (is.null(ids_to_plot)) ids_all else intersect(ids_to_plot, ids_all)

  file_for_id <- function(id) file.path(out_dir, paste0(id, ".", file_ext))
  size_for_id <- function(id) {
    if (id %in% names(size_overrides)) {
      w <- size_overrides[[id]]$width %||% default_width
      h <- size_overrides[[id]]$height %||% default_height
      c(w, h)
    } else {
      c(default_width, default_height)
    }
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
        size = 5,
        color = "#1a1a1a",
        family = "inter",
        fontface = "bold.italic"
      ) +
      theme(
        axis.text.x = element_text(size = 18, family = "inter", face = "plain", color = "#1a1a1a"),
        axis.text.y = element_text(size = 15, hjust = 0, family = "inter", face = "plain", color = "#1a1a1a")
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
  # 1) número de observaciones por variable (no NA)
  n_obs_df <- data %>%
    summarise(
      across(
        starts_with("drm"),
        ~ sum(!is.na(.x))
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to  = "variable",
      values_to = "n_obs"
    )

  # 2) medias por variable
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
    # unimos n_obs por variable
    left_join(n_obs_df, by = "variable") %>%
    # excluimos drm_11 si no se quiere en el heatmap
    filter(
      !str_detect(variable, "drm_11"),
      !str_detect(variable, "drm_res")
    ) %>%
    mutate(
      category = case_when(
        str_detect(variable, "efficiency") ~ "Efficient\nprocess",
        str_detect(variable, "fairness") ~ "Fair\nprocess",
        str_detect(variable, "affordable") ~ "Affordable\nprocess",
        str_detect(variable, "duration") ~ "Timely\nprocess",
        str_detect(variable, "helpful") ~ "Helpful\nmechanism"
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
                             outfile = file.path(path2SP, "analysis/output/drm_heatmap.svg"),
                             width = 300,
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
    scale_color_identity() + # permite usar los colores tal cual vienen de text_color()

    # Gradiente dinámico basado en min/max observados
    scale_fill_gradient(
      name   = "Share",
      limits = c(min_val, max_val),
      labels = scales::percent_format(accuracy = 1),
      low    = "#E3E4F5", # tono claro de la marca
      high   = "#575796" # color de marca
    ) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.position = "none", # sin leyenda
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
    plot = p_drm_heatmap,
    filename = outfile,
    device = "svg",
    width = width,
    height = height,
    units = "mm"
  )

  return(p_drm_heatmap)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.1 Heatmap function (second section DRMs)                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tables_drm2 <- function(data) {
  data %>%
    summarise(
      across(
        starts_with("drm_res"),
        ~ mean(.x, na.rm = TRUE)
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to  = "variable",
      values_to = "value"
    ) %>%
    # excluimos drm_11 si no se quiere en el heatmap
    filter(!str_detect(variable, "drm_res_11")) %>%
    mutate(
      category = case_when(
        str_detect(variable, "outcome") ~ "Written\noutcome",
        str_detect(variable, "appeal") ~ "Appeal\nrights",
        str_detect(variable, "rep") ~ "Lawyer\nrepresentation",
        str_detect(variable, "oth") ~ "Other adviser\nrepresentation",
      ),
      actor = case_when(
        str_detect(variable, "drm_res_1") ~ "Court",
        str_detect(variable, "drm_res_2") ~ "Tribunal",
        str_detect(variable, "drm_res_3") ~ "Ombudsman",
        str_detect(variable, "drm_res_4") ~ "Police",
        str_detect(variable, "drm_res_5") ~ "Formal\nmediation",
        str_detect(variable, "drm_res_6") ~ "Lawyer",
        str_detect(variable, "drm_res_7") ~ "Government\ndepartment",
        str_detect(variable, "drm_res_8") ~ "Community\nleader",
        str_detect(variable, "drm_res_9") ~ "Other DRM"
      )
    ) %>%
    # ordenamos factores (categorías y actores)
    mutate(
      category = factor(
        category,
        levels = c(
          "Written\noutcome",
          "Appeal\nrights",
          "Lawyer\nrepresentation",
          "Other adviser\nrepresentation"
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

plot_drm_heatmap2 <- function(DRM_results2,
                              outfile = file.path(path2SP, "analysis/output/drm2_heatmap.svg"),
                              width = 300,
                              height = 250) {
  # 1) Rango dinámico (excluyendo NA)
  min_val2 <- min(DRM_results2$value, na.rm = TRUE)
  max_val2 <- max(DRM_results2$value, na.rm = TRUE)

  # 2) Función interna para el color del texto (contraste)
  text_color <- function(val) {
    # punto medio del rango observado
    mid <- min_val2 + (max_val2 - min_val2) / 2
    ifelse(val > mid, "white", "#1a1a1a")
  }

  # 3) Construir el heatmap
  p_drm_heatmap2 <- ggplot(
    DRM_results2,
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
    scale_color_identity() + # permite usar los colores tal cual vienen de text_color()

    # Gradiente dinámico basado en min/max observados
    scale_fill_gradient(
      name   = "Share",
      limits = c(min_val2, max_val2),
      labels = scales::percent_format(accuracy = 1),
      low    = "#E3E4F5", # tono claro de la marca
      high   = "#575796" # color de marca
    ) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.position = "none", # sin leyenda
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
    plot = p_drm_heatmap2,
    filename = outfile,
    device = "svg",
    width = width,
    height = height,
    units = "mm"
  )

  return(p_drm_heatmap2)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. Panel plot — multiple indicators × multiple groupings                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Combines several groupbars tables into a single faceted panel grid.
# Columns = indicators, rows = disaggregation groups.
#
# Arguments:
#   tables           — named list from compute_groupbars_tables()
#   indicator_ids    — character vector of table IDs to include (must exist in tables)
#   indicator_labels — named vector id -> display label; if NULL, IDs are used as-is
#   params           — full params object (uses full_group_cfg and levels_map)
#   group_filters    — character vector of grouping keys from full_group_cfg,
#                      e.g. c("gender", "age_group", "income").
#                      Pass "Overall" to show national-average-only row.
#   include_overall  — if TRUE, always prepend a national average row above the groups
#   filename         — full output path (e.g. file.path(path2SP, "output/panel.svg"))
#   width_mm / height_mm / scale — passed to ggsave()

plot_panel_indicators <- function(
  tables,
  indicator_ids,
  indicator_labels = NULL,
  params,
  group_filters = "Overall",
  include_overall = TRUE,
  filename,
  width_mm = 300,
  height_mm = NULL,
  scale = 0.75,
  left_margin_mm = 40
) {
  missing_ids <- setdiff(indicator_ids, names(tables))
  if (length(missing_ids)) {
    stop("IDs not found in tables: ", paste(missing_ids, collapse = ", "))
  }

  if (is.null(indicator_labels)) {
    indicator_labels <- setNames(indicator_ids, indicator_ids)
  }

  # Separate "Overall" from real group keys
  group_filters_clean <- setdiff(group_filters, "Overall")
  show_overall <- include_overall || "Overall" %in% group_filters
  groupings_to_keep <- c(if (show_overall) "Overall", group_filters_clean)

  # Display labels and row order for facet strips (left side)
  overall_strip <- " "
  group_strip_labels <- purrr::map_chr(
    group_filters_clean,
    ~ params$full_group_cfg[[.x]] %||% .x
  )
  row_order <- c(if (show_overall) overall_strip, group_strip_labels)

  # Bind selected tables and filter
  combined <- purrr::map_dfr(indicator_ids, function(id) {
    tables[[id]] %>% dplyr::mutate(indicator = id)
  }) %>%
    dplyr::filter(grouping %in% groupings_to_keep) %>%
    dplyr::mutate(
      indicator_label = dplyr::recode(indicator, !!!indicator_labels),
      indicator_label = factor(indicator_label,
        levels = unname(indicator_labels[indicator_ids])
      ),
      level = dplyr::if_else(grouping == "Overall", "National Average", level),
      grouping_disp = dplyr::if_else(grouping == "Overall", overall_strip, grouping),
      grouping_disp = dplyr::recode(grouping_disp, !!!params$full_group_cfg),
      grouping_disp = factor(grouping_disp, levels = row_order)
    )

  # Build composite y_id factor levels.
  # National Average (" ") is placed at position 1 (bottom of the global y-axis), but its
  # facet row is first in row_order so it renders at the TOP of the panel grid.
  # Each group's levels are added in REVERSE levels_map order so the first levels_map entry
  # lands at the highest position within that section (= top of the group's facet panel).
  key2levels_name <- params$full_group_cfg
  levels_cfg <- params$levels_map
  groups_present <- stats::na.omit(unique(as.character(combined$grouping_disp)))
  levels_all <- character(0)

  for (k in names(key2levels_name)) {
    if (k == "Overall" || !k %in% group_filters_clean) next
    disp_lab <- key2levels_name[[k]]
    if (!disp_lab %in% groups_present) next

    if (k == "NUTS") {
      levels_region <- combined %>%
        dplyr::filter(grouping == k) %>%
        dplyr::distinct(level) %>%
        dplyr::arrange(level) %>%
        dplyr::pull(level)
      if (length(levels_region)) {
        levels_all <- c(levels_all, paste(disp_lab, rev(levels_region), sep = " | "))
      }
      next
    }

    ordered_lvs <- levels_cfg[[disp_lab]]
    present_vals <- combined %>%
      dplyr::filter(grouping == k) %>%
      dplyr::distinct(level) %>%
      dplyr::pull(level)
    lv_use <- if (!is.null(ordered_lvs)) intersect(ordered_lvs, present_vals) else present_vals
    if (length(lv_use)) {
      levels_all <- c(levels_all, paste(disp_lab, rev(lv_use), sep = " | "))
    }
  }

  # "National Average" goes first = position 1 = bottom in ggplot y-axis;
  # the " " facet row is first in row_order, so it renders at the TOP of the grid.
  # Using the plain string (not overall_strip " ") lets scale_y_discrete show the label.
  levels_all <- c("National Average", levels_all)

  # Auto-size height.
  # space = "free_y" gives the single-bar National Average panel only 1/N of the panel
  # area, which is very small when N is large.  Adding 3 virtual bars to the count
  # ensures the top row always has enough room without distorting the other rows.
  # The extra overhead (100 mm) covers plot margins, panel spacings, and the indicator
  # label annotations that sit above the National Average bar (y = 1.75, clip = "off").
  if (is.null(height_mm)) {
    n_panels <- length(row_order)
    height_mm <- (length(levels_all) + 3L) * 4 + max(0L, n_panels - 1L) * 8 + 100
  }

  # Attach composite y_id and pivot to long format
  combined <- combined %>%
    dplyr::mutate(
      y_id = paste(as.character(grouping_disp), level, sep = " | "),
      y_id = dplyr::if_else(as.character(grouping_disp) == overall_strip, "National Average", y_id),
      y_id = factor(y_id, levels = levels_all),
      primary = mean,
      secondary = 1 - mean
    ) %>%
    tidyr::pivot_longer(c(primary, secondary),
      names_to  = "color",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      label_value = dplyr::if_else(
        color == "primary",
        dplyr::if_else(is.na(value), "N/A",
          paste0(round(value * 100, 0), "%")
        ),
        NA_character_
      )
    )

  # Indicator labels — one per column, placed above the National Average bar.
  # y = 1.75 sits above position 1 ("National Average") in the " " facet row;
  # coord_cartesian(clip = "off") keeps it visible outside the panel boundary.
  col_label_df <- tibble::tibble(
    label = unname(indicator_labels[indicator_ids]),
    x = 50,
    y = 2.5,
    grouping_disp = factor(overall_strip, levels = row_order),
    indicator_label = factor(
      unname(indicator_labels[indicator_ids]),
      levels = unname(indicator_labels[indicator_ids])
    )
  )

  # "National Average" label — mirrors col_label_df structure exactly
  # (factor facet vars + numeric y) so geom_text places it correctly.
  na_label_df <- tibble::tibble(
    x = -2,
    y = 1,
    grouping_disp = factor(overall_strip, levels = row_order),
    indicator_label = factor(
      unname(indicator_labels[indicator_ids[1]]),
      levels = unname(indicator_labels[indicator_ids])
    ),
    label = "National Average"
  )

  # Group strip labels — y = count of levels in that panel = top bar position.
  group_label_df <- purrr::map_dfr(group_filters_clean, function(k) {
    disp_lab <- params$full_group_cfg[[k]] %||% k
    if (!disp_lab %in% groups_present) {
      return(NULL)
    }
    group_y_ids <- levels_all[startsWith(levels_all, paste0(disp_lab, " | "))]
    if (!length(group_y_ids)) {
      return(NULL)
    }
    tibble::tibble(
      grouping_disp = factor(disp_lab, levels = row_order),
      indicator_label = factor(
        unname(indicator_labels[indicator_ids[1]]),
        levels = unname(indicator_labels[indicator_ids])
      ),
      y = length(group_y_ids) + 0.5,
      x = -10,
      label = disp_lab
    )
  })

  p <- ggplot2::ggplot(
    combined,
    ggplot2::aes(x = value * 100, y = y_id, fill = color)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_stack(reverse = TRUE),
      width    = 0.7,
      na.rm    = TRUE
    ) +
    # Value labels
    ggplot2::geom_text(
      ggplot2::aes(x = 101, label = label_value),
      family = "inter",
      fontface = "bold",
      color = "#575796",
      hjust = -0.1,
      size = 4,
      na.rm = TRUE
    ) +
    # Variable label
    ggplot2::geom_text(
      data = col_label_df,
      ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      color = "#575796",
      fontface = "bold",
      family = "inter",
      size = 5,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.9,
      na.rm = TRUE
    ) +
    # "National Average" label
    ggplot2::geom_text(
      data = na_label_df,
      ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 1,
      vjust = 0.5,
      color = "#575796",
      family = "inter",
      fontface = "bold",
      size = 4.5,
      na.rm = TRUE
    ) +
    # Group label annotations
    ggplot2::geom_text(
      data = group_label_df,
      ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 1,
      vjust = 0,
      color = "#575796",
      family = "inter",
      fontface = "bold",
      size = 4.5,
      na.rm = TRUE
    ) +
    ggplot2::facet_grid(
      rows   = ggplot2::vars(grouping_disp),
      cols   = ggplot2::vars(indicator_label),
      scales = "free_y",
      space  = "free_y",
      switch = "y"
    ) +
    ggplot2::scale_fill_manual(
      values = c("primary" = "#575796", "secondary" = "#e5e8e8")
    ) +
    ggplot2::scale_x_continuous(
      expand   = c(0, 0),
      limits   = c(0, 115),
      position = "top",
      oob      = scales::oob_keep
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) {
        lbl <- sub("^.* \\| ", "", x)
        dplyr::if_else(lbl == "National Average", "", lbl)
      }
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.text.y.left = element_blank(),
      strip.switch.pad.grid = grid::unit(0, "mm"),
      strip.clip = "off",
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        size   = 12,
        hjust  = 1,
        family = "inter",
        face   = "plain",
        color  = "#1a1a1a"
      ),
      panel.grid = element_blank(),
      panel.spacing = grid::unit(6, "mm"),
      legend.position = "none",
      plot.margin = margin(20, 5, 5, left_margin_mm, "mm")
    )

  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(
    filename = filename,
    plot     = p,
    width    = width_mm,
    height   = height_mm,
    units    = "mm",
    scale    = scale
  )

  return(p)
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 6. Co-ocurrence plot                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot_coocurrence_bars <- function(tables,
                                  params,
                                  filename = file.path(path2SP, "analysis/output/co_ocurrence.svg"),
                                  facet_order = NULL,
                                  width_mm = 300,
                                  height_mm = 280) {
  levels_cfg <- params$levels_map
  # Mapa clave -> etiqueta de levels_cfg (fuente común)
  key2levels_name <- params$full_group_cfg


  facet_order <- c(
    "Overall", "age_group", "disability", "edu_level", "gender", "income", "NUTS",
    "level_impact", "cooccurence_group", "category"
  )

  facet_order2 <- unique(c(" ", facet_order))

  group_cfg <- params$full_group_cfg

  # 1. Facet order y labels
  if (is.null(facet_order)) {
    facet_order <- names(params[["full_group_cfg"]])
  }
  facet_labels <- params[["full_group_cfg"]] # named vector: internal_code → pretty label

  co_ocurr <- tables[["ndisputes"]] %>%
    dplyr::mutate(
      primary = mean,
      secondary = 6.6 - mean,
      grouping = case_when(
        grouping == "Overall" ~ " ",
        TRUE ~ grouping
      ),
      level = case_when(
        grouping == " " ~ "National Average",
        TRUE ~ level
      )
    ) %>%
    tidyr::pivot_longer(c(primary, secondary),
      names_to = "color",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      grouping = dplyr::recode(grouping, !!!group_cfg),
      # recodifica con group_cfg (no global)
      # grouping = factor(grouping, levels = facet_order2)),
      label_value = dplyr::if_else(color == "primary",
        paste0(round(value, 1)),
        NA_character_
      )
    )

  # Grupos realmente presentes en los datos (tras recodificación)
  groups_present <- stats::na.omit(unique(as.character(co_ocurr$grouping)))

  # 2) Niveles dinámicos por grupo
  #    - Usa levels_cfg cuando exista la entrada
  #    - Para NUTS/Region, usa niveles detectados en los datos
  #    - Solo arma entradas para grupos presentes
  levels_all <- character(0)

  for (k in names(group_cfg)) {
    disp_lab <- group_cfg[[k]] # etiqueta mostrada (p. ej., "Gender")
    if (!disp_lab %in% groups_present) next # si ese facet no aparece en datos, omite

    if (k == "NUTS") {
      # Region desde datos
      levels_region <- co_ocurr %>%
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
    lv_key <- key2levels_name[[k]] # p.ej. "Income"
    if (is.null(lv_key)) next

    if (!lv_key %in% names(levels_cfg)) {
      warning("No se encontró levels_cfg[['", lv_key, "']] para la clave '", k, "'. Se omite en levels_all.")
      next
    }

    # Orden deseado desde levels_cfg
    lv_vals_ordered <- levels_cfg[[lv_key]]

    # Niveles realmente presentes en los datos para este grouping
    present_vals <- co_ocurr %>%
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
    levels_all <- co_ocurr %>%
      dplyr::mutate(y_key = paste(grouping, level, sep = " | ")) %>%
      dplyr::pull(y_key) %>%
      unique()
  }

  levels_all <- c(" ", levels_all)

  data2plot <- co_ocurr %>%
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
    x = 0, # posición en X
    y = 1.75, # arriba del panel
    grouping = first_group # <-- clave: coincide con la faceta
  )

  # 3. Máximo global para la barra de fondo
  max_val <- max(data2plot$value, na.rm = TRUE)

  # 4. Plot
  p <- ggplot(
    data2plot,
    aes(y = y_id, x = value, fill = color)
  ) +
    geom_col(position = ggplot2::position_stack(reverse = TRUE), width = 0.9, na.rm = TRUE) +

    # Texto
    geom_text(
      aes(
        x = 6.7,
        label = label_value
      ),
      family = "inter",
      fontface = "bold",
      color = "#575796",
      hjust = 0,
      size = 5
    ) +
    coord_cartesian(clip = "off") +
    ggplot2::facet_grid(
      rows = ggplot2::vars(grouping),
      scales = "free", space = "free_y", switch = "y"
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 7.25), position = "top") +
    ggplot2::scale_fill_manual(values = c("primary" = "#575796", "secondary" = "#e5e8e8")) +
    scale_y_discrete(
      labels = function(x) sub("^.* \\| ", "", x) # Remueve "Grouping | " del inicio
    ) +
    theme_minimal() +
    theme(
      strip.placement = "outside",
      strip.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = ggtext::element_markdown(
        size = 16, hjust = 1, family = "inter"
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.spacing = unit(12, "mm"),
      strip.text.y.left = element_text(
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
    )
  p

  # 6. Save
  ggsave(
    filename = filename,
    plot = p,
    device = "svg",
    width = width_mm,
    height = height_mm,
    units = "mm",
    scale = 0.75
  )

  return(p)
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 7. Co-occurrence heat table (conditional)
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot_cooccurrence_table <- function(
  data,
  filename = file.path(path2SP, "analysis/output/co-ocurrence_table.svg"),
  width_mm = 370,
  height_mm = 235
) {
  problem_cols <- c(
    "problem_cat_land", "problem_cat_neighbors", "problem_cat_housing", "problem_cat_family",
    "problem_cat_injury", "problem_cat_citizen", "problem_cat_gov", "problem_cat_public",
    "problem_cat_products", "problem_cat_services", "problem_cat_money", "problem_cat_employment"
  )

  problem_labels <- c(
    problem_cat_land = "Land",
    problem_cat_neighbors = "Neighbors",
    problem_cat_housing = "Housing",
    problem_cat_family = "Family",
    problem_cat_injury = "Injury",
    problem_cat_citizen = "Citizenship",
    problem_cat_gov = "Government\nbenefits",
    problem_cat_public = "Public services",
    problem_cat_products = "Products",
    problem_cat_services = "Services",
    problem_cat_money = "Money/debt",
    problem_cat_employment = "Employment"
  )

  missing_cols <- setdiff(problem_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns for co-occurrence heat table: ", paste(missing_cols, collapse = ", "))
  }

  problems_df <- data %>%
    dplyr::select(all_of(problem_cols)) %>%
    dplyr::mutate(dplyr::across(everything(), ~ dplyr::if_else(.x == 1, 1, 0, missing = 0)))

  base_n <- colSums(problems_df == 1, na.rm = TRUE)

  joint_counts <- matrix(
    0,
    nrow = length(problem_cols),
    ncol = length(problem_cols),
    dimnames = list(problem_cols, problem_cols)
  )

  for (i in seq_along(problem_cols)) {
    for (j in seq_along(problem_cols)) {
      joint_counts[i, j] <- sum(
        problems_df[[problem_cols[i]]] == 1 & problems_df[[problem_cols[j]]] == 1,
        na.rm = TRUE
      )
    }
  }

  conditional_mat <- matrix(
    NA_real_,
    nrow = length(problem_cols),
    ncol = length(problem_cols),
    dimnames = list(problem_cols, problem_cols)
  )

  for (i in seq_along(problem_cols)) {
    for (j in seq_along(problem_cols)) {
      denom <- base_n[i]
      conditional_mat[i, j] <- if (denom > 0) joint_counts[i, j] / denom else NA_real_
    }
  }

  # Suppress low-base rows to keep consistency with n<30 rule used in the report
  low_base_rows <- names(base_n[base_n < 30])
  if (length(low_base_rows) > 0) {
    conditional_mat[low_base_rows, ] <- NA_real_
  }

  cond_long <- as.data.frame(as.table(conditional_mat), stringsAsFactors = FALSE) %>%
    dplyr::rename(row_problem = Var1, col_problem = Var2, value = Freq) %>%
    dplyr::mutate(
      is_diagonal = row_problem == col_problem,
      label = dplyr::if_else(
        is.na(value),
        "N/A",
        scales::percent(value, accuracy = 1)
      ),
      label = dplyr::if_else(is_diagonal, "", label),
      row_problem = dplyr::recode(row_problem, !!!problem_labels),
      col_problem = dplyr::recode(col_problem, !!!problem_labels)
    )

  row_order <- base_n %>%
    sort(decreasing = TRUE) %>%
    names() %>%
    dplyr::recode(!!!problem_labels)

  cond_long <- cond_long %>%
    dplyr::mutate(
      row_problem = factor(row_problem, levels = rev(row_order)),
      col_problem = factor(col_problem, levels = row_order)
    )

  min_val <- min(cond_long$value, na.rm = TRUE)
  max_val <- max(cond_long$value, na.rm = TRUE)
  if (!is.finite(min_val) || !is.finite(max_val) || min_val == max_val) {
    min_val <- 0
    max_val <- 1
  }

  tile_base <- cond_long %>% dplyr::filter(!is_diagonal)
  tile_diag <- cond_long %>% dplyr::filter(is_diagonal)

  p <- ggplot2::ggplot(cond_long, ggplot2::aes(x = col_problem, y = row_problem)) +
    ggplot2::geom_tile(
      data = tile_base,
      ggplot2::aes(fill = value),
      color = "white",
      linewidth = 0.8
    ) +
    ggplot2::geom_tile(
      data = tile_diag,
      fill = "white",
      color = "white",
      linewidth = 1
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      family = "inter",
      size = 3,
      fontface = "bold",
      lineheight = 0.9,
      color = "#1a1a1a",
      na.rm = TRUE
    ) +
    ggplot2::scale_fill_gradient(
      low = "#E3E4F5",
      high = "#575796",
      limits = c(min_val, max_val),
      labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      x = "Also had this problem (column)",
      y = "Given this problem (row)",
      caption = "How to read: Choose a row problem, then move across columns. Each cell shows the % who also had the column problem."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x.top = ggplot2::element_text(
        family = "inter",
        face = "bold",
        size = 9,
        lineheight = 0.95,
        color = "#1a1a1a"
      ),
      axis.text.y = ggplot2::element_text(family = "inter", face = "bold", size = 10, color = "#1a1a1a"),
      plot.caption = ggplot2::element_text(family = "inter", size = 12, color = "#1a1a1a", lineheight = 1.2, hjust = 0),
      plot.caption.position = "plot",
      axis.title.x = ggplot2::element_text(family = "inter", face = "bold", size = 10, color = "#1a1a1a", margin = ggplot2::margin(t = 18)),
      axis.title.x.top = ggplot2::element_text(family = "inter", face = "bold", size = 10, color = "#1a1a1a", margin = ggplot2::margin(b = 14)),
      axis.title.y = ggplot2::element_text(family = "inter", face = "bold", size = 10, color = "#1a1a1a", margin = ggplot2::margin(r = 18)),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(8, 18, 12, 8)
    )

  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(
    filename = filename,
    plot = p,
    width = width_mm,
    height = height_mm,
    units = "mm"
  )

  return(p)
}
