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

summarize_by_vars <- function(data,
                              value,
                              group_cfg = group_labels,   # <- NEW: usa este mismo objeto
                              include_overall = TRUE) {
  val <- rlang::ensym(value)
  
  # Deriva los grupos desde group_labels (fuente única) y valida que existan en data
  groups <- setdiff(names(group_labels), "Overall")
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
