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
source("code/functions.R")
source("code/bars_group.R")
source("code/data_wrangling.R")
source("code/params.R")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  Loading data                                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

master_data <- read_dta(
  file.path(path2SP,"data/ireland_lns_2025_final.dta")
)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Data wrangling (later to re-factor)                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- wrangle_ireland_lns(master_data) 

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Data for plots                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
  default_height  = 350,
  size_overrides  = list(
    prevalence = list(width = 300, height = 280)
  )
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

tables_outline <- c(tables, multi_response_bars_tables)
openxlsx::write.xlsx(tables_outline, "tables_outline.xlsx")

