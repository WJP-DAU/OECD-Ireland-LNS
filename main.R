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
## 3.  Data wrangling (later to re-factor)                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_subset.df <- wrangle_ireland_lns(master_data) 

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


## =========================================================
## Co-occurrence
## =========================================================

# ------------------------------------------------------------

# 1a. Extract desired facet order - This comes from params.R
facet_order <- c("Overall", "age_group", "disability", "edu_level", "gender", "income", "NUTS",
                 "level_impact", "cooccurence_group", "category")
  #names(params[["full_group_cfg"]]) 


# 1b. Facet labels (pretty names for strips)
facet_labels <- params[["full_group_cfg"]]   # named vector: internal_code -> label

# 2. Prepare data and apply the facet order
# Assuming:
# co_ocurr has: grouping, level, value, x, y, label_html
# max_val <- max(co_ocurr$value, na.rm = TRUE)

# Assuming:
# co_ocurr has: grouping, level, value, x, y, label_html
# max_val <- max(co_ocurr$value, na.rm = TRUE)

# 3. Plot
plot_ocurr <- ggplot(co_ocurr, 
                     aes(x = level, y = value)) +   # factor() forces fixed bar width
  
  # full-width background bar
  geom_col(
    aes(y = max_val),
    fill  = "#E6E8E6",
    width = 0.9
  ) +
  
  # main purple bar
  geom_col(
    fill  = "#4F4A8C",
    width = 0.9
  ) +
  
  geom_text(
    aes(label = round(value, 1)),
    family   = "inter",
    fontface = "bold",
    color    = "#575796",
    hjust    = 0,
    size     = 5
  ) +
  
  coord_flip(clip = "off") +
  
  ggplot2::facet_grid(
    rows    = ggplot2::vars(grouping),
    scales  = "free",
    space   = "free_y",
    switch  = "y",
    # 👉 use custom labels for each facet
    labeller = ggplot2::labeller(
      grouping = facet_labels
    )
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  
  theme_minimal() +
  theme(
    strip.placement       = "outside",
    strip.background      = element_blank(),
    axis.title.x          = element_blank(),
    axis.title.y          = element_blank(),
    axis.text.x           = element_blank(),
    axis.text.y           = ggtext::element_markdown(
      size   = 16,
      hjust  = 1,
      family = "inter",
      face   = "plain"
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


ggsave(
  file.path(path2SP, "output/co_ocurrence.svg", fsep = "/"),
  device = "svg",
  width  = 300,
  height = 280,
  units  = "mm"
)



