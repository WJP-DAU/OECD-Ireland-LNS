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
source("code/network_graph.R")

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

params <- groupbars_params()

tables <- compute_groupbars_tables(data_subset.df, params)


## =========================================================
## Panel Bars
## =========================================================

## Top advisers by problem type 
plot_panel_indicators(                                                                                                                                                   
  tables           = tables,
  indicator_ids    = c("AJD_adviser_14", "AJD_adviser_17",                                                                                                                 
                       "AJD_adviser_12", "AJD_adviser_15",
                       "AJD_adviser_7"),
  indicator_labels = c(                                                                                                                                                  
    AJD_adviser_14 = "Online search",
    AJD_adviser_17 = "Other person",                                                                                                                       
    AJD_adviser_12 = "Non-legal\nprofessional or\norganisation",
    AJD_adviser_15 = "Social media",  
    AJD_adviser_7  = "Private solicitor\nor law office"
  ),
  params          = params,                                                                                                                                              
  group_filters    = c("Overall", "level_impact", "cooccurence_group", "category"),                                                                                              
  include_overall = TRUE,
  filename        = file.path(path2SP, "analysis/output/advisers_grouped_prob.svg"),
  left_margin_mm   = 5,    # minimal space needed    
  scale    = 1
)


#Reasons for not seeking prof help - grouped
plot_panel_indicators(                                                                                                                                                   
  tables           = tables,
  indicator_ids    = c("reason_no_need", "reason_legalcap", "reason_process",                                                                                                                 
                       "reason_interpersonal","reason_other", "reason_prev"),
  indicator_labels = c(
    reason_no_need       = "No need/\nlow severity",
    reason_legalcap      = "Legal Capability\nBarriers",
    reason_process       = "Process barriers\n(cost/time/access)",
    reason_interpersonal = "Interpersonal\nBarriers",
    reason_other         = "Other reason",
    reason_prev          = "Previous\nexperiences"
  ),
  params          = params,                                                                                                                                              
  group_filters   = c("Overall", "gender", "age_group", "edu_level",
                      "income", "NUTS", "disability", "ethnic_majority"),                                                                                                
  include_overall = TRUE,
  filename        = file.path(path2SP, "analysis/output/reasons_grouped_demos.svg"),
  width_mm         = 400, 
  left_margin_mm   = 5,    # minimal space needed     
  scale    = 1
)

plot_panel_indicators(                                                                                                                             
  tables           = tables,                              
  indicator_ids    = c("reason_no_need", "reason_legalcap", "reason_process",                                                                                                                 
                       "reason_interpersonal","reason_other", "reason_prev"),
  indicator_labels = c(
    reason_no_need       = "No need/\nlow severity",
    reason_legalcap      = "Legal Capability\nBarriers",
    reason_process       = "Process barriers\n(cost/time/access)",
    reason_interpersonal = "Interpersonal\nBarriers",
    reason_other         = "Other reason",
    reason_prev          = "Previous\nexperiences"
  ),                  
  params           = params,                                                                                                                       
  group_filters    = c("Overall", "level_impact", "cooccurence_group", "category"),
  
  include_overall  = TRUE,                                                                                                                         
  filename         = file.path(path2SP, "analysis/output/reasons_grouped_prob.svg"),
  width_mm         = 400, 
  left_margin_mm   = 5,    # minimal space needed   
  scale            = 1  
) 


#Reasons for not seeking ANY help - grouped

plot_panel_indicators(                                                                                                                             
  tables           = tables,                              
  indicator_ids    = c("no_need_action", "legalcap_action", "process_action", "interpersonal_action", 
                       "bigger_action", "other_action"),
  indicator_labels = c(no_need_action       = "No need/\nwas not needed",
                       legalcap_action      = "Legal Capability\nBarriers", 
                       process_action       = "Process barriers\n(cost/time)",  
                       bigger_action        = "Had bigger\nproblems",
                       interpersonal_action = "Interpersonal\nBarriers",
                       other_action         = "Other reason"
                       ),                                              
  params           = params,                                                                                                                       
  group_filters    = c(  "Overall", "gender", "age_group", "edu_level", "income", 
                         "NUTS", "disability", "ethnic_majority"),
  
  include_overall  = TRUE,                                                                                                                         
  filename         = file.path(path2SP, "analysis/output/no_help_demo.svg"),
  width_mm         = 400,
  left_margin_mm   = 5,    # minimal space needed    
  scale            = 1  
)       

plot_panel_indicators(                                                                                                                             
  tables           = tables,                              
  indicator_ids    = c("no_need_action", "legalcap_action", "process_action", "interpersonal_action", 
                       "bigger_action", "other_action"),
  indicator_labels = c(no_need_action       = "No need/\nwas not needed",
                       legalcap_action      = "Legal Capability\nBarriers", 
                       process_action       = "Process barriers\n(cost/time)",  
                       bigger_action        = "Had bigger\nproblems",
                       interpersonal_action = "Interpersonal\nBarriers",
                       other_action         = "Other reason"
  ),                                            
  params           = params,                                                                                                                       
  group_filters    = c("Overall", "level_impact", "cooccurence_group"),
  
  include_overall  = TRUE,                                                                                                                         
  filename         = file.path(path2SP, "analysis/output/no_help_prob.svg"),
  width_mm         = 400,
  left_margin_mm   = 5,    # minimal space needed    
  scale            = 1  
)     


#Contacted DRMs - grouped 
plot_panel_indicators(                                                                                                                             
  tables           = tables,                              
  indicator_ids    = c("AJR_drm_6_bin",
                       "AJR_drm_7_bin",
                       "AJR_drm_4_bin",
                       "AJR_drm_11_bin",
                       "AJR_drm_8_bin"
  ),
  indicator_labels = c(AJR_drm_6_bin  = "Lawyer or\nlaw office staff", 
                       AJR_drm_7_bin  = "Gov. department\nor local council",
                       AJR_drm_4_bin  = "Police or\nlaw enforcement",
                       AJR_drm_11_bin = "Other professional\nor organisation",
                       AJR_drm_8_bin  = "Community leader\nor person of\nstanding"
  ),   
  params           = params,                                                                                                                       
  group_filters    = c("Overall", "level_impact", "cooccurence_group", "category"),
  include_overall  = TRUE,                                                                                                                         
  filename         = file.path(path2SP, "analysis/output/drm1_prob.svg"),
  left_margin_mm   = 5,    # minimal space needed 
  width_mm         = 400, 
  height_mm        = 476                                                                                                                           
)

plot_panel_indicators(                                                                                                                             
  tables           = tables,                              
  indicator_ids    = c("AJR_drm_1_bin", 
                       "AJR_drm_10_bin", 
                       "AJR_drm_9_bin",
                       "AJR_drm_5_bin",
                       "AJR_drm_2_bin",
                       "AJR_drm_3_bin"
  ),
  indicator_labels = c(AJR_drm_1_bin  = "Court", 
                       AJR_drm_10_bin = "Other person\n(friend, family,\netc.)",
                       AJR_drm_9_bin  = "Other dispute\nresolution\nservice",
                       AJR_drm_5_bin  = "Mediation or\nconciliation\nservice",
                       AJR_drm_2_bin  = "Tribunal",
                       AJR_drm_3_bin  = "Ombudsman"
  ),                                             
  params           = params,                                                                                                                       
  group_filters    = c("Overall", "level_impact", "cooccurence_group", "category"),
  include_overall  = TRUE,                                                                                                                         
  filename         = file.path(path2SP, "analysis/output/drm2_prob.svg"),
  left_margin_mm   = 5,    # minimal space needed 
  width_mm         = 400, 
  height_mm        = 476                                                                                                                           
)


#No DRMs reasons - grouped

plot_panel_indicators(                                                                                                                             
  tables           = tables,                              
  indicator_ids    = c("no_need_drm", "legalcap_drm", "process_drm", "legal_assis_drm", "trust_drm", 
                       "interpersonal_drm", "other_drmr"),
  indicator_labels = c(no_need_drm       = "Did not\nneed a DRM", 
                       legalcap_drm      = "Legal Capability\nBarriers",
                       legal_assis_drm   = "Could not\nobtain legal\nassistance",
                       process_drm       = "Process barriers\n(cost/distance/\nconvenience)",
                       trust_drm         = "Lack of trust\nof authorities",
                       interpersonal_drm = "Interpersonal\nbarriers",
                       other_drmr        = "Other reason"
                       ),                                             
  params           = params,                                                                                                                       
  group_filters    = c(  "Overall", "gender", "age_group", "edu_level", "income", 
                         "NUTS", "disability", "ethnic_majority"),
  
  include_overall  = TRUE,                                                                                                                         
  filename         = file.path(path2SP, "analysis/output/no_drm_demo.svg"),
  left_margin_mm   = 5,    # minimal space needed   
  width_mm         = 430,
  height_mm        = 400   
)

plot_panel_indicators(                                                                                                                             
  tables           = tables,                              
  indicator_ids    = c("no_need_drm", "legalcap_drm", "process_drm", "legal_assis_drm", "trust_drm", 
                       "interpersonal_drm", "other_drmr"),
  indicator_labels = c(no_need_drm       = "Did not\nneed a DRM", 
                       legalcap_drm      = "Legal Capability\nBarriers",
                       legal_assis_drm   = "Could not\nobtain legal\nassistance",
                       process_drm       = "Process barriers\n(cost/distance/\nconvenience)",
                       trust_drm         = "Lack of trust\nof authorities",
                       interpersonal_drm = "Interpersonal\nbarriers",
                       other_drmr        = "Other reason"
  ),                                          
  params           = params,                                                                                                                       
  group_filters    = c("Overall", "level_impact", "cooccurence_group", "category"),
  include_overall  = TRUE,                                                                                                                         
  filename         = file.path(path2SP, "analysis/output/no_drm_prob.svg"),
  left_margin_mm   = 5,    # minimal space needed   
  width_mm         = 430,  
  height_mm        = 476                                                                                                                           
)  

  
#Levels of impact grouped 
plot_panel_indicators(                                                                                                                             
  tables           = tables,                              
  indicator_ids    = c("impact_1", "impact_2", "impact_3", "impact_4", "impact_5"),
  indicator_labels = c(impact_1 = "None at all", impact_2 = "Slight", impact_3 = "Moderate", 
                       impact_4 = "High", impact_5 = "Severe"),                                              
  params           = params,                                                                                                                       
  group_filters    = c(  "Overall", "gender", "age_group", "edu_level", "income", 
                         "NUTS", "disability", "ethnic_majority"),

  include_overall  = TRUE,                                                                                                                         
  filename         = file.path(path2SP, "analysis/output/impact_demo.svg"),
  left_margin_mm   = 5,    # minimal space needed    
  width_mm         = 400,                                                                                                                          
  height_mm        = 476                                                                                                                           
)                                                                                                                                                  


#Hardships grouped
plot_panel_indicators(
  tables           = tables,
  indicator_ids    = c( 
                        "hardship_2_bin_p",
                        "hardship_8_bin_p",
                        "hardship_10_bin_p",
                        "hardship_9_bin_p",
                        "hardship_15_bin_p",
                        "hardship_16_bin_p"
                        ),
  indicator_labels = c(hardship_2_bin_p  = "Stress or\nemotional\nstrain", 
                       hardship_8_bin_p  = "Loss of\nmoney", 
                       hardship_10_bin_p = "Deterioration\nin mental\nhealth", 
                       hardship_9_bin_p  = "Loss of\nconfidence",
                       hardship_15_bin_p = "Finding everyday\nactivities\ndifficult", 
                       hardship_16_bin_p = "Less trust\nin public\ninstitutions"
                       ),    
  params           = params,
  
  group_filters    = c("Overall", "category", "cooccurence_group"),
  include_overall  = TRUE,
  filename         = file.path(path2SP, "analysis/output/hardship1_prob.svg"),
  left_margin_mm   = 5,    # minimal space needed    
  width_mm         = 420,  
  height_mm        = 400
)

plot_panel_indicators(
  tables           = tables,
  indicator_ids    = c( 
                        "hardship_11_bin_p",
                        "hardship_1_bin_p",
                        "hardship_3_bin_p",
                        "hardship_14_bin_p",
                        "hardship_4_bin_p",
                        "hardship_6_bin_p"
                        ),
  indicator_labels = c(hardship_11_bin_p  = "Feeling\nisolated", 
                       hardship_1_bin_p   = "Health\ndifficulties\nor injury", 
                       hardship_3_bin_p   = "Damage to\nrelationships", 
                       hardship_14_bin_p  = "Disruption of\npublic services",
                       hardship_4_bin_p   = "Being threatened\nor feeling\nunsafe",
                       hardship_6_bin_p   = "Changes to\nhousing\nsituation"
                       ),  
  params           = params,
  group_filters    = c("Overall", "category", "cooccurence_group"),
  include_overall  = TRUE,
  filename         = file.path(path2SP, "analysis/output/hardship2_prob.svg"),
  left_margin_mm   = 5,    # minimal space needed    
  width_mm         = 420,  
  height_mm        = 250
)

plot_panel_indicators(
  tables           = tables,
  indicator_ids    = c( 
                        "hardship_5_bin_p",
                        "hardship_13_bin_p",
                        "hardship_7_bin_p",
                        "hardship_12_bin_p"
                        ),
  indicator_labels = c(hardship_5_bin_p   = "Damage to property",
                       hardship_13_bin_p  = "Disruption to\neducation", 
                       hardship_7_bin_p   = "Loss of employment",
                       hardship_12_bin_p  = "Use of substances"
                       ),  
  params           = params,
  group_filters    = c("Overall", "category", "cooccurence_group"),
  include_overall  = TRUE,
  filename         = file.path(path2SP, "analysis/output/hardship3_prob.svg"),
  left_margin_mm   = 5,    # minimal space needed    
  width_mm         = 420,  
  height_mm        = 250
)


#Affordable mechanisms - grouped 
plot_panel_indicators(
  tables           = tables,
  indicator_ids    = c( 
                        "drm_1_d_affordable_bin",
                        "drm_2_d_affordable_bin",
                        "drm_3_d_affordable_bin",
                        "drm_4_d_affordable_bin",
                        "drm_5_d_affordable_bin"
  ),
  indicator_labels = c(drm_1_d_affordable_bin  = "Court",
                       drm_2_d_affordable_bin  = "Tribunal", 
                       drm_3_d_affordable_bin  = "Ombudsman",
                       drm_4_d_affordable_bin  = "Police or\nlaw enforcement",
                       drm_5_d_affordable_bin  = "Mediation or\nconciliation service"
                         ),  
  params           = params,
  group_filters    = c(  "Overall", "gender", "age_group", "edu_level", "income", 
                         "NUTS", "disability", "ethnic_majority"),
  include_overall  = TRUE,
  filename         = file.path(path2SP, "analysis/output/drm_afford1_demo.svg"),
  width_mm         = 400,  
  height_mm        = 250
)

plot_panel_indicators(
  tables           = tables,
  indicator_ids    = c( 
                        "drm_6_d_affordable_bin",
                        "drm_7_d_affordable_bin",
                        "drm_8_d_affordable_bin",
                        "drm_9_d_affordable_bin",
                        "drm_11_d_affordable_bin"
                        ),
  indicator_labels = c(drm_6_d_affordable_bin  = "Lawyer or\nlaw office staff",
                       drm_7_d_affordable_bin  = "Gov. department\nor local council", 
                       drm_8_d_affordable_bin  = "Community leader\nor person of standing",
                       drm_9_d_affordable_bin  = "Other dispute\nresolution service",
                       drm_11_d_affordable_bin  = "Other professional\nor organisation"
  ),  
  params           = params,
  group_filters    = c(  "Overall", "gender", "age_group", "edu_level", "income", 
                         "NUTS", "disability", "ethnic_majority"),
  include_overall  = TRUE,
  filename         = file.path(path2SP, "analysis/output/drm_afford2_demo.svg"),
  width_mm         = 400,  
  height_mm        = 250
)


#Legal cap - grouped

#Access2rep
plot_panel_indicators(                                                                                                                                                   
  tables           = tables,
  indicator_ids    = c("access2rep2", "reason_no_need", "reason_legalcap", "reason_external"),
  indicator_labels = c(                                                                                                                                                  
    access2rep2     = "Access to Appropriate\nAssistance and\nRepresentation",
    reason_no_need  = "Did not need\n the assistance",
    reason_legalcap = "Legal Capability\nBarriers",
    reason_external = "External\nbarriers"
  ),
  params          = params,                                                                                                                                              
  group_filters    = c("Overall",
                       "rights",
                       "info",
                       "help",
                       "fair_cap"),                                                                                              
  include_overall = TRUE,
  filename        = file.path(path2SP, "analysis/output/legal_cap_access2rep.svg"),
  width_mm         = 400,                                                                                                                          
  height_mm        = 250,                                 
  scale            = 1,
  left_margin_mm   = 65    # reserve more space in the SVG
)

#Access2DRM
plot_panel_indicators(                                                                                                                                                   
  tables           = tables,
  indicator_ids    = c("access2DRM2", "no_need_drm", "legalcap_drm", "external_bar_drm"),
  indicator_labels = c(                                                                                                                                                  
    access2DRM2      = "Access to DRM\n(SDG 16.3.3)",
    no_need_drm      = "Did not\nneed a DRM", 
    legalcap_drm     = "Legal Capability\nBarriers",
    external_bar_drm = "External\nBarriers"
    
  ),
  params          = params,                                                                                                                                              
  group_filters    = c("Overall",
                       "rights",
                       "info",
                       "help",
                       "fair_cap"),                                                                                              
  include_overall = TRUE,
  filename        = file.path(path2SP, "analysis/output/legal_cap_drm.svg"),
  width_mm         = 400,                                                                                                                          
  height_mm        = 250,                                 
  scale            = 1,
  left_margin_mm   = 65    # reserve more space in the SVG
)


#Access to justice
plot_panel_indicators(                                                                                                                                                   
  tables           = tables,
  indicator_ids    = c("access2info", "access2rep", "access2DRM", "fairness", "outcome_done"),
  indicator_labels = c(
    access2info    = "Awareness of\nSources of\nInformation",
    access2rep     = "Access to Appropriate\nAssistance and\nRepresentation",
    access2DRM     = "Access to DRM\n(SDG 16.3.3)",
    fairness       = "Fair Outcome",
    outcome_done   = "Finalized resolution\nprocess"
  ),
  params          = params,                                                                                                                                              
  group_filters    = c(  "Overall", "gender", "age_group", "edu_level", "income", 
                         "NUTS", "disability", "ethnic_majority"),                                                                                             
  include_overall = TRUE,
  filename        = file.path(path2SP, "analysis/output/a2j.svg"),
  width_mm         = 400,                                                                                                                          
  scale            = 1,
  left_margin_mm   = 65    # reserve more space in the SVG
)


## =========================================================
## Group Bars
## =========================================================


plots <- render_groupbars_plots(
  tables          = tables,
  params          = params,
  out_dir         = file.path(path2SP, "analysis/output"),  
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
  out_dir        = file.path(path2SP, "analysis/output"),
  file_ext       = "svg",
  default_width  = 300,
  default_height = 350,
  size_overrides = list(
    prevalence_categories = list(width = 250, height = 250)
  ),
  ids_to_plot = NULL
)


## =========================================================
## Co-occurrence
## =========================================================

# ------------------------------------------------------------

plot_coocurrence_bars(
  tables      = tables,
  params      = params,
  filename    = file.path(path2SP,"analysis/output/co_ocurrence.svg"),
  facet_order = facet_order, 
  height_mm = 475, 
  width_mm = 300
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
  filename = file.path(path2SP, "analysis/output/sankey_advice_representation.svg"),
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
  filename = file.path(path2SP, "analysis/output/sankey_drm.svg"),
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
  filename = file.path(path2SP, "analysis/output/network_graph.svg"),
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

