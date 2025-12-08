# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# OECD Ireland LNS Report - Data Wrangling
# Authors: Natalia Rodriguez, Santiago Pardo
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# =============================================================================
# 1) ETAPA: PARÁMETROS (solo config)
#    Output: params (cfgs, presets, measures)
# =============================================================================

####### Grouped bars with disaggregations #######

groupbars_params <- function() {
  
  full_group_cfg <- c(
    "Overall"           = "National Average",
    "gender"            = "Gender",
    "age_group"         = "Age Group",
    "edu_level"         = "Education Level",
    "income"            = "Income",
    "NUTS"              = "Region",
    "level_impact"      = "Impact Level",
    "cooccurence_group" = "Co-occurrence",
    "disability"        = "Disability",
    "category"          = "Problem Type",
    "ethnic_majority"   = "Ethnicity"
  )
  
  levels_map <- list(
    "National Average"      = "All",
    "Gender"                = c("Female", 
                                "Male"),
    "Age Group"             = c("18-24", 
                                "25-34", 
                                "35-44", 
                                "45-54", 
                                "55-64", 
                                "65-100"),
    "Education Level"       = c("Lower Education", 
                                "Higher Education"),
    "Income"                = c("< €30k a year",
                                "€30k – €70 a year",
                                "€70k – €120k a year",
                                "> €120k a year"),
    "Impact Level"          = c("High impact", 
                                "Low impact"),
    "Co-occurrence" = c("1 problem",
                                "2-3 problems",
                                "4-5 problems",
                                "5 or more problems"),
    "Disability"            = c("With disability", 
                                "Without disability"),
    "Problem Type"      = c("Land", 
                                "Neighbors", 
                                "Housing", 
                                "Family/relationship", 
                                "Injury", 
                                "Citizenship",
                                "Government benefits", 
                                "Public services", 
                                "Products", 
                                "Services",
                                "Money/debt", 
                                "Employment"),
    "Ethnicity"         = c("White Irish",
                            "Other Ethnicity")
  )
  
  groups_presets <- list(
    basic    = select_groups(
      full_group_cfg, 
      "Overall",
      "gender",
      "age_group",
      "edu_level",
      "income",
      "NUTS",
      "disability",
      "ethnic_majority"),
    extended = select_groups(
      full_group_cfg, 
      "Overall",
      "gender",
      "age_group",
      "edu_level",
      "income",
      "NUTS",
      "level_impact",
      "cooccurence_group",
      "disability",
      "category",
      "ethnic_majority")
  )
  
  # Here you can add a new variable 
  
  measures <- list(
    add_measure("prevalence",    prevalence,    "basic"),
    add_measure("ndisputes",     ndisputes,     "basic"),
    add_measure("timeliness",    timeliness,    "extended"),
    add_measure("contacted_DRM", contacted_drm, "extended"),
    add_measure("access2DRM",    access2drm,    "extended"),
    add_measure("access2info",   access2info,   "extended"),
    add_measure("access2rep",    access2rep,    "extended"),
    add_measure("fairness",      fair,          "extended"),
    add_measure("outcome_done",  outcome_done,  "extended"),
    add_measure("impact",        impact,        "basic"),
    add_measure("had_hardship",  had_hardship,  "basic"),
    add_measure("legal_rights",  legal_rights,  "basic"),
    add_measure("access2info2",  access2info,   "basic"),
    add_measure("expert_help",   expert_help,   "basic"),
    add_measure("fair_outcome",  fair_outcome,  "basic")
  )
  
  list(
    full_group_cfg = full_group_cfg,
    levels_map     = levels_map,
    groups_presets = groups_presets,
    measures       = measures
  )
}


####### Bars for multiresponse #######

build_bars_params <- function() {
  
  add_mr_block <- function(id, cols, labels_vec_id = NULL, labels_map_id = NULL, top_n = Inf) {
    list(
      id             = id,
      cols           = cols,
      labels_vec_id  = labels_vec_id,
      labels_map_id  = labels_map_id,
      top_n          = top_n
    )
  }
  
  # --- Biblioteca de labels: VECTORES por índice (1..K) ---
  labels_vec_lib <- list(
    # Asesores (AJD_adviser_1 : _17)
    adviser = c(
      "Court",
      "Tribunal",
      "Ombudsman",
      "Police or\nlaw enforcement",
      "Family\nMediation Service",
      "Other mediation/dispute\nresolution service",
      "Private solicitor\nor law office",
      "Private barrister\nor chambers",
      "Legal Aid Board\nLaw Centre",
      "Other law centre\n(e.g. FLAC)",
      "Gov. department\nor local council",
      "Non-legal professional\nor organisation",
      "Community leader\nor person of standing",
      "Online search",
      "Social media",
      "AI tools",
      "Other person"
    ),
    
    #Advisers - help (adviser_help_1_bin : _17)
    help_adviser = c(
      "Court",
      "Tribunal",
      "Ombudsman",
      "Police or\nlaw enforcement",
      "Family\nMediation Service",
      "Other mediation/dispute\nresolution service",
      "Private solicitor\nor law office",
      "Private barrister\nor chambers",
      "Legal Aid Board\nLaw Centre",
      "Other law centre\n(e.g. FLAC)",
      "Gov. department\nor local council",
      "Non-legal professional\nor organisation",
      "Community leader\nor person of standing",
      "Online search",
      "Social media",
      "AI tools",
      "Other person"
    ),
    
    # Razones para no buscar ayuda prof (AJD_noadvice_reason_1:17)
    no_prof_help = c(
      "No real disaggreement/\nproblem",
      "Other person\n/org was right",
      "Issue resolved\nby itself",
      "Got all the help\nfrom others",
      "Did not feel\nhelp was needed",
      "Issue not\nserious enough",
      "Concerned about\ntime required",
      "Concerned about\ncost",
      "Too difficult to\nfind help nearby",
      "Too stressful",
      "Feared harming\nrelationship with\nthe other party",
      "Scared to take\naction/ask for help",
      "Did not know where/how\nto find advice",
      "Thought help would not\nchange the outcome",
      "Tried seeking help\nbefore and it\nwasn't helpful",
      "Someone told me\nnot to get advice",
      "Other reason"
    ),
    
    #Non-seekers intentions (AJR_action_1:4)
    intention = c(
      "Haven't done/\ndidn't do anything and\ndon't expect to",
      "Nothing yet but expect to",
      "Have done something",
      "No, as it has resolved\nitself or is no longer\nan issue"
    ),
    
    #Reasons for not seeking ANY help (AJR_noaction_1:13)
    no_help = c(
      "It wasn’t very\nimportant",
      "It was resolved\nquickly",
      "Concerned about\ntime required",
      "Too stressful",
      "Concerned about\ncost",
      "It would damage\nthe relationship",
      "Uncomfortable/\nashamed to\nask for help",
      "It would make\nno difference",
      "Had bigger issues",
      "Were at fault",
      "Didn’t know\nwhat to do",
      "Didn’t need advice",
      "Other reason"
    ),
    
    # DRM contactados (cols AJR_drm_1_bin : AJR_drm_11_bin)
    drm = c(
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
    ),
    
    reason_no_resol = c(
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
    ),

    # Estado resolución actual (AJR_status_cur_1 : _7)
    status = c(
      "Formal legal\nprocess ongoing",
      "Other dispute\nresolution ongoing",
      "Seeking advice/\nsupport to resolve",
      "Waiting for\nother party to act",
      "No recent action —\nintend to resolve",
      "No recent\naction taken",
      "Other"
    ),
    
    #Resolution description (AJR_settle_resol_1:11)
    description_resol = c(
      "A court judgment",
      "A tribunal decision",
      "A decision by\nanother type of\nauthority",
      "Another type of\ndispute resolution",
      "Exchanges\nbetween lawyers",
      "Intervention\nof a third person",
      "Reached an agreement\nwithout any assistance\nof a third person",
      "Got what they\nwanted without\nhaving to do anything",
      "Made changes to\navoid the issue",
      "Gave up trying\nto do anything",
      "Some other way"
    ),
    
    #Types of hardships
    hardships = c(
      "Health difficulties\nor injury",
      "Stress or\nemotional strain",
      "Damage to\nrelationships",
      "Being threatened or\nfeeling unsafe",
      "Damage to property",
      "Changes to housing\nsituation",
      "Loss of employment",
      "Loss of money",
      "Loss of confidence",
      "Deterioration in\nmental health",
      "Feeling isolated",
      "Use of substances",
      "Disruption to\neducation",
      "Disruption of public\nservices",
      "Finding everyday\nactivities difficult",
      "Less trust in\npublic institutions"
    )
  )
  
  # --- Biblioteca de labels: MAPS nombrados (columna -> etiqueta) ---
  labels_map_lib <- list(
    # Categorías de problema (ya venían nombradas)
    problems = c(
      "problem_cat_land"        = "Land",
      "problem_cat_neighbors"   = "Neighbours",
      "problem_cat_housing"     = "Housing",
      "problem_cat_family"      = "Family/relationship",
      "problem_cat_injury"      = "Injury",
      "problem_cat_citizen"     = "Citizen or migration",
      "problem_cat_gov"         = "Goverment benefits \nand payments",
      "problem_cat_public"      = "Public services",
      "problem_cat_products"    = "Products",
      "problem_cat_services"    = "Services",
      "problem_cat_money"       = "Money/debt",
      "problem_cat_employment"  = "Employment"
    )
  )
  
  # --- Definición de bloques a producir (puedes agregar/editar libremente) ---
  blocks <- list(
    add_mr_block(
      id = "adviser",
      cols = paste0("AJD_adviser_", 1:17),
      labels_vec_id = "adviser",
      top_n = Inf
    ),
    add_mr_block(
      id = "help_adviser",
      cols = paste0("adviser_help_", 1:17,"_bin"),
      labels_vec_id = "help_adviser",
      top_n = Inf
    ),
    add_mr_block(
      id = "no_prof_help",
      cols = paste0("AJD_noadvice_reason_", 1:17),
      labels_vec_id = "no_prof_help",
      top_n = Inf
    ),
    add_mr_block(
      id = "intention",
      cols = paste0("AJR_action_", 1:4),
      labels_vec_id = "intention",
      top_n = Inf
    ),
    add_mr_block(
      id = "no_help",
      cols = paste0("AJR_noaction_", 1:13),
      labels_vec_id = "no_help",
      top_n = Inf
    ),
    add_mr_block(
      id = "drm",
      cols = paste0("AJR_drm_", 1:11,"_bin"),
      labels_vec_id = "drm",
      top_n = Inf
    ),
    add_mr_block(
      id = "no_resol_reasons",
      cols = paste0("AJR_noresol_reason_", 1:15),
      labels_vec_id = "reason_no_resol",
      top_n = Inf
    ),
    add_mr_block(
      id = "status",
      cols = paste0("AJR_status_cur_", 1:7),
      labels_vec_id = "status",
      top_n = Inf
    ),
    add_mr_block(
      id = "description_resol",
      cols = paste0("AJR_settle_resol_", 1:11),
      labels_vec_id = "description_resol",
      top_n = Inf
    ),
    add_mr_block(
      id = "hardships",
      cols = paste0("hardship_", 1:16,"_bin"),
      labels_vec_id = "hardships",
      top_n = Inf
    ),
    
    add_mr_block(
      id = "prevalence_categories",
      cols = c(
        "problem_cat_land","problem_cat_neighbors","problem_cat_housing","problem_cat_family",
        "problem_cat_injury","problem_cat_citizen","problem_cat_gov","problem_cat_public",
        "problem_cat_products","problem_cat_services","problem_cat_money","problem_cat_employment"
      ),
      labels_map_id = "problems",
      top_n = Inf
    )
  )
  
  list(
    labels_vec_lib = labels_vec_lib,
    labels_map_lib = labels_map_lib,
    blocks         = blocks
  )
}
