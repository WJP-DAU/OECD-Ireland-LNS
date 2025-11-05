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

openxlsx::write.xlsx(tables, "tables_outline.xlsx")

## =========================================================
## Bars
## =========================================================

reason_labels <- c(
  "Issue not\nimportant enough",
  "Issue resolved\nbefore action",
  "Confident could\nresolve alone",
  "Confident could\nresolve with help",
  "Up to the other\nparty to act",
  "Person caused\nthe issue",
  "Did not know\nwhere to go",
  "Could not obtain\nlegal assistance",
  "Too expensive\nor feared cost",
  "Too far or\nhard to reach",
  "Too inconvenient",
  "Did not trust\nauthorities",
  "Did not think\nthey could help",
  "Afraid of\nconsequences",
  "Did not know\nit was possible"
)

data2plot <- data_subset.df %>%
  summarize(
    across(
      starts_with("AJR_noresol_reason_"),
      ~mean(.x, na.rm = T)
    )
  ) %>%
  pivot_longer(cols = everything(), 
               names_to = "category", 
               values_to = "values") %>%
  drop_na() %>%
  mutate(
    reason_number = str_extract(category, "\\d+"),
    reason_label  = reason_labels[as.numeric(reason_number)]
  ) %>%
  drop_na(reason_label) %>%
  mutate(
    values2plot = values*100
  ) %>%
  arrange(-values2plot) %>%
  mutate(
    order_no = row_number(),
    lab_pos  = values2plot+7,
    color    = "standard",
    labels = paste0(
      format(
        round(values2plot, 0),
        nsmall = 0
      ), "%"
    )
  ) %>%
  filter(
    order_no < 11
  )

p <- WJPr::wjp_bars(
  data      = data2plot,
  target    = "values2plot",
  grouping  = "reason_label",
  order     = "order_no", 
  direction = "horizontal",
  colors    = "color",
  cvec      = c("standard" = "#575796")
) +
  geom_text(
    aes(
      y = data2plot$lab_pos,
      label = labels
    ),
    size     = 5,
    color    = "#1a1a1a",
    family   = "inter",
    fontface = "bold.italic"
  ) +
  theme(
    axis.text.x = element_text(
      size   = 16,
      # hjust  = 0,
      family = "inter",
      face   = "plain",
      color  = "#1a1a1a"
    ),
    axis.text.y = element_text(
      size   = 16,
      hjust  = 0,
      family = "inter",
      face   = "plain",
      color  = "#1a1a1a"
    ),
  )

ggsave(
  plot = p,
  paste0(path2SP, "output/noresol_reasons.svg"), 
  width  = 300, 
  height = 350,
  units  = "mm",
  scale  = 0.75 
)

# ------------------------------------------------------------
# 1) Vector de labels corto (fuera de la función; edítalo a tu gusto)
#    Debe estar indexado por posición: 1 = motivo 1, 2 = motivo 2, etc.
reason_labels_short <- c(
  "Issue not\nimportant enough",
  "Issue resolved\nbefore action",
  "Confident could\nresolve alone",
  "Confident could\nresolve with help",
  "Up to the other\nparty to act",
  "Person caused\nthe issue",
  "Did not know\nwhere to go",
  "Could not obtain\nlegal assistance",
  "Too expensive\nor feared cost",
  "Too far or\nhard to reach",
  "Too inconvenient",
  "Did not trust\nauthorities",
  "Did not think\nthey could help",
  "Afraid of\nconsequences",
  "Did not know\nit was possible"
)
# ------------------------------------------------------------

# ------------------------------------------------------------
# 2) Función genérica para múltiples respuesta (0/1 por columna)
#    - data: data.frame con columnas binarias (0/1)
#    - cols_to_plot: vector de nombres de columnas a incluir
#    - labels_vec: vector de labels indexado por número (1,2,3,...) — opcional
#    - labels_map: named character vector: names = columnas, values = label — opcional
#    - name, country, w, h: para guardar el SVG
# 
#  Reglas de etiquetado:
#   - Si labels_map está provisto, se usa directamente (columna -> etiqueta).
#   - Si NO hay labels_map, se intenta extraer el sufijo numérico de cada columna
#     y usar labels_vec[ sufijo ].
# ------------------------------------------------------------
gen_multiresponse_bars <- function(data,
                                   cols_to_plot,
                                   labels_vec = NULL,
                                   labels_map = NULL,
                                   name,
                                   country,
                                   w, h) {
  stopifnot(all(cols_to_plot %in% names(data)))
  
  # 1) Estimar prevalencias (promedios de 0/1 * 100)
  agg <- data %>%
    summarize(
      across(all_of(cols_to_plot), ~ mean(.x, na.rm = TRUE) * 100)
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "category",
      values_to = "values"
    )
  
  # 2) Construir etiqueta a mostrar
  if (!is.null(labels_map)) {
    # Mapa nombre de columna -> etiqueta
    agg <- agg %>%
      mutate(category_label = dplyr::recode(category, !!!labels_map, .default = category))
  } else {
    # Usar sufijo numérico para indexar labels_vec
    if (is.null(labels_vec)) {
      stop("Debes proveer labels_vec (indexado por número) o labels_map (nombrado por columna).")
    }
    suff_idx <- str_extract(agg$category, "(?<=_)\\d+")
    if (any(is.na(suff_idx))) {
      stop("No pude extraer sufijos numéricos de alguna(s) columna(s). Usa labels_map.")
    }
    suff_idx <- as.integer(suff_idx)
    if (max(suff_idx, na.rm = TRUE) > length(labels_vec)) {
      stop("labels_vec no alcanza para el mayor sufijo encontrado en las columnas.")
    }
    agg <- agg %>%
      mutate(category_label = labels_vec[suff_idx])
  }
  
  # 3) Preparar data para wjp_bars
  data_plot <- agg %>%
    arrange(desc(values)) %>%
    mutate(
      labels   = paste0(format(round(values, 0), nsmall = 0), "%"),
      lab_pos  = values,
      color    = "standard",
      order_no = row_number()
    ) %>%
    select(category_label, values2plot = values, lab_pos, labels, color, order_no) %>%
    rename(category = category_label) %>%
    filter(
      order_no < 13
    )
  
  # 4) Graficar
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
    );p
  
  # 5) Guardar
  #dir.create(paste0("output/{country}"), recursive = TRUE, showWarnings = FALSE)
  ggsave(
    filename = paste0(path2SP,"output/",name,".svg"),
    plot     = p,
    width    = w,
    height   = h,
    units    = "mm",
    scale    = 0.75
  )
  
  return(p)
}

cols_drm <- paste0("AJR_drm_",1:11,"_bin")

drm_labels_short <- c(
  "Court",
  "Tribunal",
  "Ombudsman",
  "Police or\nlaw enforcement",
  "Mediation or\nconciliation service",
  "Lawyer or\nlaw office staff",
  "Gov. department\nor local council",
  "Community leader\nor person of standing",
  "Other dispute\nresolution service",
  "Other person\n(friend, family,\netc.)",
  "Other professional\nor organisation"
)

gen_multiresponse_bars(
  data         = data_subset.df,
  cols_to_plot = cols_drm,
  labels_vec   = drm_labels_short,  # usa el sufijo numérico
  name         = "drm_contacted",
  country      = "Ireland",
  w = 300, h = 350
)


cols_adviser <- paste0("AJD_adviser_",1:17)

adviser_labels_short <- c(
    "Court",
    "Tribunal",
    "Ombudsman",
    "Police or\nlaw enforcement",
    "Family\nMediation Service",
    "Other mediation/\ndispute resolution\nservice",
    "Private solicitor\nor law office",
    "Private barrister\nor chambers",
    "Legal Aid Board\nLaw Centre",
    "Other law centre\n(e.g. FLAC)",
    "Gov. department\nor local council",
    "Non-legal professional\nor organisation",
    "Community leader\nor person of standing",
    "Online search\n(e.g. Google)",
    "Social media\n(e.g. Facebook, X,\nTikTok, Reddit)",
    "AI tools\n(e.g. ChatGPT,\nGemini)",
    "Other person\n(friend, family,\netc.)"
  )

gen_multiresponse_bars(
  data         = data_subset.df,
  cols_to_plot = cols_adviser,
  labels_vec   = adviser_labels_short,  # usa el sufijo numérico
  name         = "adviser",
  country      = "Ireland",
  w = 300, h = 350
)


cols_barriers <- paste0("AJD_noadvice_reason_",1:17)

help_barriers_short <- c(
  "No real\ndisagreement\nor problem",
  "Other person/\norganisation\nwas right",
  "Issue resolved\nby itself",
  "Got enough help\nfrom others",
  "Did not feel\nhelp was needed",
  "Issue not\nserious enough",
  "Concerned about\ntime required",
  "Concerned about\ncost",
  "Hard to find\nsomeone nearby",
  "Too stressful",
  "Feared harming\nrelationship",
  "Scared to take\naction or ask\nfor help",
  "Did not know\nwhere/how to\nfind advice",
  "Thought help\nwould not\nchange outcome",
  "Tried before —\nwas not helpful",
  "Told not to\nget advice or\ninformation",
  "Other reason"
)

gen_multiresponse_bars(
  data         = data_subset.df,
  cols_to_plot = cols_barriers,
  labels_vec   = help_barriers_short,  # usa el sufijo numérico
  name         = "help_barriers",
  country      = "Ireland",
  w = 300, h = 350
)

cols_status <- paste0("AJR_status_cur_",1:7)

resolution_status_labels_short <- c(
  "Formal legal\nprocess ongoing",
  "Other dispute\nresolution ongoing",
  "Seeking advice/\nsupport to resolve",
  "Waiting for\nother party to act",
  "No recent action —\nintend to resolve",
  "No recent\naction taken",
  "Other"
)

gen_multiresponse_bars(
  data         = data_subset.df,
  cols_to_plot = cols_status,
  labels_vec   = resolution_status_labels_short,  # usa el sufijo numérico
  name         = "status",
  country      = "Ireland",
  w = 300, h = 350
)

cols_problems <- c(
  "problem_cat_land",
  "problem_cat_neighbors",
  "problem_cat_housing",
  "problem_cat_family",
  "problem_cat_injury",
  "problem_cat_citizen",
  "problem_cat_gov",
  "problem_cat_public",
  "problem_cat_products",
  "problem_cat_services",
  "problem_cat_money",
  "problem_cat_employment"
)

problems_labels_short <- c(
  "problem_cat_land" = "Land",
  "problem_cat_neighbors" = "Neighbours",
  "problem_cat_housing"="Housing",
  "problem_cat_family" = "Family/relationship",
  "problem_cat_injury"= "Injury",
  "problem_cat_citizen"="Citizen or migration",
  "problem_cat_gov"="Goverment benefits \nand payments",
  "problem_cat_public"="Public services",
  "problem_cat_products"="Products",
  "problem_cat_services"="Services",
  "problem_cat_money"="Money/debt",
  "problem_cat_employment"="Employment"
)

gen_multiresponse_bars(
  data         = data_subset.df,
  cols_to_plot = cols_problems,
  labels_map = problems_labels_short,
  #labels_vec   = problems_labels_short,  # usa el sufijo numérico
  name         = "prevalence_categories",
  country      = "Ireland",
  w = 250, h = 250
)

