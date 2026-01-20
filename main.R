## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            OECD Ireland LNS Report - Settings
##
## Author(s):         Natalia Rodriguez   (nrodriguez@worldjusticeproject.org)
##                    Santiago Pardo             (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
## Creation date:     October 14th, 2025
##
## This version:      November 21st, 2025
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required modules                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("code/settings.R")
source("code/data_wrangling.R")
source("code/params.R")
source("code/functions.R")
source("code/bars_group.R")
source("code/sankey_rep.R")
source("code/sankey_drm.R")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Loading data                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

master_data <- read_dta(
  file.path(
    path2SP,
    "data/ireland_lns_2025_final.dta")
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Data wrangling                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---------------------------
# Parseo de argumentos estilo GNU
# ---------------------------

args <- commandArgs(trailingOnly = TRUE)

# Función auxiliar para parsear argumentos tipo --param=valor
parse_arg <- function(name, default = NULL) {
  pattern <- paste0("^--", name, "=")
  match <- args[grepl(pattern, args)]
  
  if (length(match) == 0) {
    return(default)  # valor por defecto si no se pasa
  }
  
  value <- sub(pattern, "", match)
  return(value)
}

# Leer argumento high_impact
high_impact_raw <- parse_arg("high_impact", default = "FALSE")

# Convertir a logico
high_impact <- as.logical(high_impact_raw)

cat(">> high_impact =", high_impact, "\n")

if (isTRUE(high_impact)) {
  
  data_subset.df <- wrangle_ireland_lns(master_data) %>%
    filter(AJE_impact %in% c(3, 4, 5))
  
} else {
  
  data_subset.df <- wrangle_ireland_lns(master_data)
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Data for plots                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## =========================================================
## Group Bars
## =========================================================

params <- groupbars_params()

tables <- compute_groupbars_tables(data_subset.df, params)

plots <- render_groupbars_plots(
  tables          = tables,
  params          = params,
  out_dir         = file.path(path2SP, "output"),  
  file_ext        = "svg",
  use_outcome_dir = FALSE,                         # TRUE => guarda en output/outcome/
  measures_to_plot = NULL,                         
  default_width   = 300,
  default_height  = 475,
  size_overrides  = list(
    prevalence = list(width = 300, height = 475)
  )
)

## =========================================================
## Co-occurrence
## =========================================================

# ------------------------------------------------------------

plot_coocurrence_bars(
  tables      = tables,
  params      = params,
  filename    = file.path(path2SP,"output/co_ocurrence.svg"),
  facet_order = facet_order, 
  height_mm = 475, 
  width_mm = 300
)

## =========================================================
## Multi Response Bars
## =========================================================

# ------------------------------------------------------------

multi_response_bars_params <- build_bars_params()

multi_response_bars_tables <- compute_bars_tables(
  data   = data_subset.df,   
  params = multi_response_bars_params
)

multi_response_bars_plots <- render_bars_plots(
  tables         = multi_response_bars_tables,
  params         = multi_response_bars_params,
  out_dir        = file.path(path2SP, "output"),
  file_ext       = "svg",
  default_width  = 300,
  default_height = 350,
  size_overrides = list(
    prevalence_categories = list(width = 250, height = 250)
  ),
  ids_to_plot = NULL
)

## =========================================================
## Heat-map DRM
## =========================================================

# ------------------------------------------------------------

data2drm <- tables_drm(data_subset.df)%>%
  mutate(
    value = if_else(n_obs < 30, NA_real_, value)  
    )

drm_process <- list(
  drm_process = data2drm
)


p <- plot_drm_heatmap(data2drm)


### Heatmap 2 - second section of DRMs
data2drm2 <- tables_drm2(data_subset.df)

drm_process2 <- list(
  drm_process2 = data2drm2
)


p <- plot_drm_heatmap2(data2drm2)



## =========================================================
## Sankey Advice & Representation
## =========================================================

# ------------------------------------------------------------

psk_ad <- plot_sankey_advice(data = data_subset.df)

ggsave(
  psk_ad,
  filename = file.path(path2SP, "output/sankey_advice_representation.svg"),
  width    = 400,
  height   = 225, 
  units = "mm", scale = 0.75
)

## =========================================================
## Sankey DRM
## =========================================================

# ------------------------------------------------------------

psk_drm <- plot_sankey_drm(data = data_subset.df)

ggsave(
  psk_drm,
  filename = file.path(path2SP, "output/sankey_drm.svg"),
  width    = 400,
  height   = 225, 
  units = "mm", scale = 0.75
)

## =========================================================
## Network graph co-ocurrence
## =========================================================

data2plot <- data_subset.df %>%
  select(respondentid, tidyselect::matches("^problem_cat_.*[^0-9]$")) %>%
  rename(
    `Land`                 = problem_cat_land,
    `Citizenship`          = problem_cat_citizen,
    `Housing`              = problem_cat_housing,
    `Employment`           = problem_cat_employment,
    `Neighbors`            = problem_cat_neighbors,
    `Family`               = problem_cat_family,
    `Injury`               = problem_cat_injury,
    `Government\nBenefits` = problem_cat_gov,
    `Public\nServices`     = problem_cat_public,
    `Products`             = problem_cat_products,
    `Services`             = problem_cat_services,
    `Money &\nDebt`        = problem_cat_money
  )

network_chart <- network_graph(data = data2plot)

ggsave(
  network_chart,
  filename = file.path(path2SP, "output/network_graph.svg"),
  width    = 17,
  height   = 17, 
  bg = "white"
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 5. Tables                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tables_outline <- c(tables, multi_response_bars_tables, drm_process)
openxlsx::write.xlsx(tables_outline, 
                     file.path(path2SP, "tables_outline.xlsx"))
print("Tables outline saved to 'tables_outline.xlsx'")

