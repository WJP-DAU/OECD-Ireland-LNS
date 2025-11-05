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
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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

# ===================================
# 2) ETAPA: TABLAS (compute summaries)
#     Input: data (data_subset.df), params
#     Output: lista nombrada de data frames
# ===================================
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
# 3) ETAPA: GRÁFICAS (plot + save)
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

