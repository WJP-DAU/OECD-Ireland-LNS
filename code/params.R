# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# OECD Ireland LNS Report - Data Wrangling
# Authors: Natalia Rodriguez, Santiago Pardo
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# =============================================================================
# 1) ETAPA: PARÁMETROS (solo config)
#    Output: params (cfgs, presets, measures)
# =============================================================================

groupbars_params <- function() {
  
  full_group_cfg <- c(
    "Overall"           = "National Average",
    "gender"            = "Gender",
    "age_group"         = "Age Group",
    "edu_level"         = "Education Level",
    "income"            = "Income",
    "NUTS"              = "Region",
    "level_impact"      = "Impact Level",
    "cooccurence_group" = "Co-occurrent Problems",
    "disability"        = "Disability"
  )
  
  levels_map <- list(
    "National Average"      = "All",
    "Gender"                = c("Female", "Male"),
    "Age Group"             = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-100"),
    "Education Level"       = c("Lower Education", "Higher Education"),
    "Income"                = c("< €30k a year","€30k – €70 a year","€70k – €120k a year","> €120k a year"),
    "Impact Level"          = c("High impact", "Low impact"),
    "Co-occurrent Problems" = c("1 problem","2-3 problems","4-5 problems","5 or more problems"),
    "Disability"            = c("With disability", "Without disability")
  )
  
  groups_presets <- list(
    basic    = select_groups(full_group_cfg, "Overall","gender","age_group","edu_level","income","NUTS","disability"),
    extended = select_groups(full_group_cfg, "Overall","gender","age_group","edu_level","income","NUTS","level_impact","cooccurence_group","disability")
  )
  
  measures <- list(
    add_measure("prevalence",    prevalence,    "basic"),
    add_measure("timeliness",    timeliness,    "extended"),
    add_measure("contacted_DRM", contacted_drm, "extended"),
    add_measure("access2DRM",    access2drm,    "extended"),
    add_measure("access2info",   access2info,   "extended"),
    add_measure("access2rep",    access2rep,    "extended"),
    add_measure("fairness",      fair,          "extended"),
    add_measure("outcome_done",  outcome_done,  "extended")
  )
  
  list(
    full_group_cfg = full_group_cfg,
    levels_map     = levels_map,
    groups_presets = groups_presets,
    measures       = measures
  )
}
