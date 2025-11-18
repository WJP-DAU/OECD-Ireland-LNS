# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# OECD Ireland LNS Report - Data Wrangling
# Authors: Natalia Rodriguez, Santiago Pardo
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# -----------------------------------------------------------------------------
# Wrapper principal
#   - Receive: master_data (data.frame)
#   - Return: data_subset.df
# -----------------------------------------------------------------------------
wrangle_ireland_lns <- function(master_data) {
  
  # ===========================================================================
  # 1) Create main vars
  # ===========================================================================
  data_subset.df <- master_data %>%
    mutate(
      # Prevalence
      prevalence = had_dispute,
      
      # Problem categories (ALL MENTIONED PROBLEMS)
      problem_cat_land       = if_else(AJP_A1 == 1, 1, 0, missing = 0),
      problem_cat_neighbors  = if_else((AJP_B1 == 1 | AJP_B2 == 1 | AJP_B3 == 1), 1, 0, missing = 0),
      problem_cat_housing    = if_else((AJP_C1 == 1 | AJP_C2 == 1 | AJP_C3 == 1 |
                                          AJP_D1 == 1 | AJP_D2 == 1 | AJP_D3 == 1 |
                                          AJP_E1 == 1 | AJP_E2 == 1 | AJP_E3 == 1 | AJP_E4 == 1 |
                                          AJP_E5 == 1 | AJP_E6 == 1 | AJP_E7 == 1), 1, 0, missing = 0),
      problem_cat_family     = if_else((AJP_F1 == 1 | AJP_F2 == 1 | AJP_F3 == 1 |
                                          AJP_F4 == 1 | AJP_F5 == 1 | AJP_F6 == 1), 1, 0, missing = 0),
      problem_cat_injury     = if_else((AJP_H1 == 1 | AJP_H2 == 1 | AJP_H3 == 1 |
                                          AJP_H4 == 1 | AJP_H5 == 1), 1, 0, missing = 0),
      problem_cat_citizen    = if_else((AJP_G1 == 1 | AJP_G2 == 1 | AJP_G3 == 1 |
                                          AJP_G4 == 1 | AJP_G5 == 1), 1, 0, missing = 0),
      problem_cat_gov        = if_else((AJP_I1 == 1 | AJP_I2 == 1 | AJP_I3 == 1 |
                                          AJP_I4 == 1 | AJP_I5 == 1 | AJP_I6 == 1), 1, 0, missing = 0),
      problem_cat_public     = if_else((AJP_J1 == 1 | AJP_J2 == 1 | AJP_J3 == 1 | AJP_J4 == 1), 1, 0, missing = 0),
      problem_cat_products   = if_else((AJP_K1 == 1 | AJP_K2 == 1), 1, 0, missing = 0),
      problem_cat_services   = if_else((AJP_L1 == 1 | AJP_L2 == 1), 1, 0, missing = 0),
      problem_cat_money      = if_else((AJP_M1 == 1 | AJP_M2 == 1 | AJP_M3 == 1 |
                                          AJP_M4 == 1 | AJP_M5 == 1), 1, 0, missing = 0),
      problem_cat_employment = if_else((AJP_N1 == 1 | AJP_N2 == 1 | AJP_N3 == 1 | AJP_N4 == 1), 1, 0, missing = 0),
      
      # Co-occurence
      cooccurence_group = case_when(
        ndisputes == 0 ~ NA_character_,
        ndisputes == 1 ~ "1 problem",
        ndisputes <= 3 ~ "2-3 problems",
        ndisputes <= 5 ~ "4-5 problems",
        ndisputes >= 6 ~ "5 or more problems"
      ),
      
      # Selected problem category
      category = case_when(
        AJP_cat_selected == 1  ~ "Land",
        AJP_cat_selected == 2  ~ "Neighbors",
        AJP_cat_selected == 3  ~ "Housing",
        AJP_cat_selected == 4  ~ "Family/ relationship",
        AJP_cat_selected == 5  ~ "Injury",
        AJP_cat_selected == 6  ~ "Citizenship or migration",
        AJP_cat_selected == 7  ~ "Government benefits and payments",
        AJP_cat_selected == 8  ~ "Public services",
        AJP_cat_selected == 9  ~ "Products",
        AJP_cat_selected == 10 ~ "Services",
        AJP_cat_selected == 11 ~ "Money/ debt",
        AJP_cat_selected == 12 ~ "Employment"
      ),
      
      # Access to appropriate information and advice
      access2info = case_when(
        had_dispute == 0 ~ NA_real_,
        AJE_infosource_done %in% c(1,2) | AJE_infosource_ongoing %in% c(1,2) ~ 1,
        AJE_infosource_done %in% c(3,4) | AJE_infosource_ongoing %in% c(3,4) ~ 0
      ),
      
      # Contacted an adviser (ALL)
      contact_adviser = case_when(
        had_dispute == 0 ~ NA_real_,
        AJD_adviser_1 == 1 | AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 |
          AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1 |
          AJD_adviser_9 == 1 | AJD_adviser_10 == 1 | AJD_adviser_11 == 1 | AJD_adviser_12 == 1 |
          AJD_adviser_13 == 1 | AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 |
          AJD_adviser_17 == 1 ~ 1,
        AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
          AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
          AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
          AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 &
          AJD_adviser_17 == 2 ~ 0
      ),
      
      # Appropriate / non-appropriate adviser
      appropriate_adviser = case_when(
        had_dispute == 0 ~ NA_real_,
        AJD_adviser_1 == 1 | AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 |
          AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1 |
          AJD_adviser_9 == 1 | AJD_adviser_10 == 1 | AJD_adviser_11 == 1 | AJD_adviser_12 == 1 |
          AJD_adviser_13 == 1 ~ 1,
        AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
          AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
          AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
          AJD_adviser_13 == 2 ~ 0
      ),
      no_appropriate_adviser = case_when(
        had_dispute == 0 ~ NA_real_,
        (AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1) &
          (AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
             AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
             AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
             AJD_adviser_13 == 2) ~ 1,
        (AJD_adviser_1 == 1 | AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 |
           AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1 |
           AJD_adviser_9 == 1 | AJD_adviser_10 == 1 | AJD_adviser_11 == 1 | AJD_adviser_12 == 1 |
           AJD_adviser_13 == 1) ~ 0,
        had_dispute == 1 ~ 0
      ),
      
      # Did not / needed assistance (ALL & professional)
      no_need_assistance = case_when(
        (AJR_noaction_1 == 1 | AJR_noaction_2 == 1 | AJR_noaction_6 == 1 | AJR_noaction_10 == 1 | AJR_noaction_12 == 1) &
          (AJR_noaction_3 == 0 & AJR_noaction_4 == 0 & AJR_noaction_5 == 0 & AJR_noaction_7 == 0 &
             AJR_noaction_8 == 0 & AJR_noaction_9 == 0 & AJR_noaction_11 == 0 & AJR_noaction_13 == 0) ~ 1,
        AJR_action == 4 ~ 1,
        had_dispute == 1 ~ 0
      ),
      needed_assistance = case_when(
        (AJR_noaction_3 == 1 | AJR_noaction_4 == 1 | AJR_noaction_5 == 1 | AJR_noaction_7 == 1 |
           AJR_noaction_8 == 1 | AJR_noaction_9 == 1 | AJR_noaction_11 == 1 | AJR_noaction_13 == 1) ~ 1,
        had_dispute == 1 ~ 0
      ),
      no_need_assistance_prof = case_when(
        (AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
           AJD_adviser_13 == 2) &
          (AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1) &
          (AJD_noadvice_reason_1 == 1 | AJD_noadvice_reason_2 == 1 | AJD_noadvice_reason_3 == 1 |
             AJD_noadvice_reason_4 == 1 | AJD_noadvice_reason_5 == 1 | AJD_noadvice_reason_6 == 1) &
          (AJD_noadvice_reason_7 == 0 & AJD_noadvice_reason_8 == 0 & AJD_noadvice_reason_9 == 0 &
             AJD_noadvice_reason_10 == 0 & AJD_noadvice_reason_11 == 0 & AJD_noadvice_reason_12 == 0 &
             AJD_noadvice_reason_13 == 0 & AJD_noadvice_reason_14 == 0 & AJD_noadvice_reason_15 == 0 &
             AJD_noadvice_reason_16 == 0) ~ 1,
        had_dispute == 1 ~ 0
      ),
      needed_assistance_prof = case_when(
        (AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
           AJD_adviser_13 == 2) &
          (AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1) &
          (AJD_noadvice_reason_7 == 1 | AJD_noadvice_reason_8 == 1 | AJD_noadvice_reason_9 == 1 |
             AJD_noadvice_reason_10 == 1 | AJD_noadvice_reason_11 == 1 | AJD_noadvice_reason_12 == 1 |
             AJD_noadvice_reason_13 == 1 | AJD_noadvice_reason_14 == 1 | AJD_noadvice_reason_15 == 1 |
             AJD_noadvice_reason_16 == 1) ~ 1,
        had_dispute == 1 ~ 0
      ),
      unknown = case_when(
        (AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
           AJD_adviser_13 == 2) &
          (AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1) &
          (AJD_noadvice_reason_1 == 0 & AJD_noadvice_reason_2 == 0 & AJD_noadvice_reason_3 == 0 &
             AJD_noadvice_reason_4 == 0 & AJD_noadvice_reason_5 == 0 & AJD_noadvice_reason_6 == 0 &
             AJD_noadvice_reason_7 == 0 & AJD_noadvice_reason_8 == 0 & AJD_noadvice_reason_9 == 0 &
             AJD_noadvice_reason_10 == 0 & AJD_noadvice_reason_11 == 0 & AJD_noadvice_reason_12 == 0 &
             AJD_noadvice_reason_13 == 0 & AJD_noadvice_reason_14 == 0 & AJD_noadvice_reason_15 == 0 &
             AJD_noadvice_reason_16 == 0) &
          AJD_noadvice_reason_17 == 1 ~ 1,
        AJR_action == 3 ~ 1,
        had_dispute == 1 ~ 0
      ),
      
      # Access to assistance & representation (compuesto)
      access2rep = case_when(
        needed_assistance == 1 ~ 0,
        needed_assistance_prof == 1 ~ 0,
        unknown == 1 ~ 0,
        no_need_assistance_prof == 1 ~ 1,
        no_need_assistance == 1 ~ 1,
        appropriate_adviser == 1 ~ 1
      ),
      
      # Helpfulness by adviser (bin)
      across(
        c(AJD_adviser_help_1, AJD_adviser_help_2, AJD_adviser_help_3, AJD_adviser_help_4,
          AJD_adviser_help_5, AJD_adviser_help_6, AJD_adviser_help_7, AJD_adviser_help_8,
          AJD_adviser_help_9, AJD_adviser_help_10, AJD_adviser_help_11, AJD_adviser_help_12,
          AJD_adviser_help_13, AJD_adviser_help_14, AJD_adviser_help_15, AJD_adviser_help_16,
          AJD_adviser_help_17),
        \(x) case_when(
          had_dispute == 0 ~ NA_real_,
          x %in% c(1,2) ~ 0,
          x %in% c(3,4) ~ 1,
          x %in% c(5,6) ~ NA_real_
        ),
        .names = "{str_remove(.col, 'AJD_')}_bin"
      ),
      
      # Barreras (prof help)
      reason_no_need = case_when(
        AJD_noadvice_reason_1 == 1 | AJD_noadvice_reason_2 == 1 | AJD_noadvice_reason_3 == 1 |
          AJD_noadvice_reason_5 == 1 | AJD_noadvice_reason_6 == 1 ~ 1,
        (AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
           AJD_adviser_13 == 2) ~ 0
      ),
      reason_had_help = case_when(
        AJD_noadvice_reason_4 == 1 | AJD_noadvice_reason_7 == 1 | AJD_noadvice_reason_8 == 1 |
          AJD_noadvice_reason_9 == 1 ~ 1,
        (AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
           AJD_adviser_13 == 2) ~ 0
      ),
      reason_info_bar = case_when(
        AJD_noadvice_reason_13 == 1 ~ 1,
        (AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
           AJD_adviser_13 == 2) ~ 0
      ),
      reason_relation_bar = case_when(
        AJD_noadvice_reason_11 == 1 | AJD_noadvice_reason_12 == 1 ~ 1,
        (AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
           AJD_adviser_13 == 2) ~ 0
      ),
      reason_psycho_bar = case_when(
        AJD_noadvice_reason_10 == 1 | AJD_noadvice_reason_14 == 1 ~ 1,
        (AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
           AJD_adviser_13 == 2) ~ 0
      ),
      reason_prior = case_when(
        AJD_noadvice_reason_15 == 1 ~ 1,
        (AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
           AJD_adviser_13 == 2) ~ 0
      ),
      reason_social = case_when(
        AJD_noadvice_reason_16 == 1 ~ 1,
        (AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
           AJD_adviser_13 == 2) ~ 0
      ),
      reason_other = case_when(
        AJD_noadvice_reason_17 == 1 ~ 1,
        (AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
           AJD_adviser_13 == 2) ~ 0
      ),
      
      # Non-seekers’ intention (quienes NO acudieron a ningún advisor)
      AJR_action_1 = case_when(
        AJR_action == 1 ~ 1,
        AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
          AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
          AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
          AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 &
          AJD_adviser_17 == 2 ~ 0
      ),
      AJR_action_2 = case_when(
        AJR_action == 2 ~ 1,
        AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
          AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
          AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
          AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 &
          AJD_adviser_17 == 2 ~ 0
      ),
      AJR_action_3 = case_when(
        AJR_action == 3 ~ 1,
        AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
          AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
          AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
          AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 &
          AJD_adviser_17 == 2 ~ 0
      ),
      AJR_action_4 = case_when(
        AJR_action == 4 ~ 1,
        AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 &
          AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
          AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 &
          AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 &
          AJD_adviser_17 == 2 ~ 0
      ),
      
      # Contacted a DRM
      contacted_drm = case_when(
        had_dispute == 0 ~ NA_real_,
        (AJR_drm_1_bin == 1 | AJR_drm_2_bin == 1 | AJR_drm_3_bin == 1 | AJR_drm_4_bin == 1 |
           AJR_drm_5_bin == 1 | AJR_drm_6_bin == 1 | AJR_drm_7_bin == 1 | AJR_drm_8_bin == 1 |
           AJR_drm_9_bin == 1 | AJR_drm_11_bin == 1) ~ 1,
        (AJR_drm_1_bin == 0 & AJR_drm_2_bin == 0 & AJR_drm_3_bin == 0 & AJR_drm_4_bin == 0 &
           AJR_drm_5_bin == 0 & AJR_drm_6_bin == 0 & AJR_drm_7_bin == 0 & AJR_drm_8_bin == 0 &
           AJR_drm_9_bin == 0 & AJR_drm_11_bin == 0) ~ 0,
        had_dispute == 1 ~ 0
      ),
      
      # Access to DRM (SDG 16.3.3) y derivados
      access2drm = case_when(
        contacted_drm == 1 ~ 1,
        contacted_drm == 0 &
          (AJR_noresol_reason_7 == 1 | AJR_noresol_reason_8 == 1 | AJR_noresol_reason_9 == 1 |
             AJR_noresol_reason_10 == 1 | AJR_noresol_reason_11 == 1 | AJR_noresol_reason_12 == 1 |
             AJR_noresol_reason_13 == 1 | AJR_noresol_reason_14 == 1 | AJR_noresol_reason_15 == 1) ~ 0
      ),
      needed_drm = case_when(
        had_dispute == 0 ~ NA_real_,
        (AJR_noresol_reason_7 == 1 | AJR_noresol_reason_8 == 1 | AJR_noresol_reason_9 == 1 |
           AJR_noresol_reason_10 == 1 | AJR_noresol_reason_11 == 1 | AJR_noresol_reason_12 == 1 |
           AJR_noresol_reason_13 == 1 | AJR_noresol_reason_14 == 1 | AJR_noresol_reason_15 == 1) ~ 1,
        (AJR_drm_1_bin == 0 & AJR_drm_2_bin == 0 & AJR_drm_3_bin == 0 & AJR_drm_4_bin == 0 &
           AJR_drm_5_bin == 0 & AJR_drm_6_bin == 0 & AJR_drm_7_bin == 0 & AJR_drm_8_bin == 0 &
           AJR_drm_9_bin == 0 & AJR_drm_11_bin == 0) ~ 0,
        (AJR_noresol_reason_7 == 0 & AJR_noresol_reason_8 == 0 & AJR_noresol_reason_9 == 0 &
           AJR_noresol_reason_10 == 0 & AJR_noresol_reason_11 == 0 & AJR_noresol_reason_12 == 0 &
           AJR_noresol_reason_13 == 0 & AJR_noresol_reason_14 == 0 & AJR_noresol_reason_15 == 0) ~ 0
      ),
      no_need_drm = case_when(
        had_dispute == 0 ~ NA_real_,
        (AJR_noresol_reason_1 == 1 | AJR_noresol_reason_2 == 1 | AJR_noresol_reason_3 == 1 |
           AJR_noresol_reason_4 == 1 | AJR_noresol_reason_5 == 1 | AJR_noresol_reason_6 == 1) &
          (AJR_noresol_reason_7 == 0 & AJR_noresol_reason_8 == 0 & AJR_noresol_reason_9 == 0 &
             AJR_noresol_reason_10 == 0 & AJR_noresol_reason_11 == 0 & AJR_noresol_reason_12 == 0 &
             AJR_noresol_reason_13 == 0 & AJR_noresol_reason_14 == 0 & AJR_noresol_reason_15 == 0) ~ 1,
        (AJR_noresol_reason_7 == 1 | AJR_noresol_reason_8 == 1 | AJR_noresol_reason_9 == 1 |
           AJR_noresol_reason_10 == 1 | AJR_noresol_reason_11 == 1 | AJR_noresol_reason_12 == 1 |
           AJR_noresol_reason_13 == 1 | AJR_noresol_reason_14 == 1 | AJR_noresol_reason_15 == 1) ~ 0,
        AJR_noresol_reason_16 == 1 ~ 0,
        (AJR_noresol_reason_1 == 0 & AJR_noresol_reason_2 == 0 & AJR_noresol_reason_3 == 0 &
           AJR_noresol_reason_4 == 0 & AJR_noresol_reason_5 == 0 & AJR_noresol_reason_6 == 0) ~ 0,
        (AJR_drm_1_bin == 0 & AJR_drm_2_bin == 0 & AJR_drm_3_bin == 0 & AJR_drm_4_bin == 0 &
           AJR_drm_5_bin == 0 & AJR_drm_6_bin == 0 & AJR_drm_7_bin == 0 & AJR_drm_8_bin == 0 &
           AJR_drm_9_bin == 0 & AJR_drm_11_bin == 0) ~ 0
      ),
      unknown_drm = case_when(
        had_dispute == 0 ~ NA_real_,
        AJR_noresol_reason_16 == 1 &
          (AJR_noresol_reason_7 == 0 & AJR_noresol_reason_8 == 0 & AJR_noresol_reason_9 == 0 &
             AJR_noresol_reason_10 == 0 & AJR_noresol_reason_11 == 0 & AJR_noresol_reason_12 == 0 &
             AJR_noresol_reason_13 == 0 & AJR_noresol_reason_14 == 0 & AJR_noresol_reason_15 == 0) ~ 1,
        (AJR_noresol_reason_1 == 1 | AJR_noresol_reason_2 == 1 | AJR_noresol_reason_3 == 1 |
           AJR_noresol_reason_4 == 1 | AJR_noresol_reason_5 == 1 | AJR_noresol_reason_6 == 1 |
           AJR_noresol_reason_7 == 1 | AJR_noresol_reason_8 == 1 | AJR_noresol_reason_9 == 1 |
           AJR_noresol_reason_10 == 1 | AJR_noresol_reason_11 == 1 | AJR_noresol_reason_12 == 1 |
           AJR_noresol_reason_13 == 1 | AJR_noresol_reason_14 == 1 | AJR_noresol_reason_15 == 1) ~ 0,
        (AJR_drm_1_bin == 0 & AJR_drm_2_bin == 0 & AJR_drm_3_bin == 0 & AJR_drm_4_bin == 0 &
           AJR_drm_5_bin == 0 & AJR_drm_6_bin == 0 & AJR_drm_7_bin == 0 & AJR_drm_8_bin == 0 &
           AJR_drm_9_bin == 0 & AJR_drm_11_bin == 0) ~ 0
      ),
      
      # Eficiency/Fairness/Affordability/Duration/Helpfulness by mechanism
      across(
        c(AJR_drm_1_c, AJR_drm_2_c, AJR_drm_3_c, AJR_drm_4_c, AJR_drm_5_c, AJR_drm_6_c,
          AJR_drm_7_c, AJR_drm_8_c, AJR_drm_9_c, AJR_drm_11_c),
        \(x) case_when(
          had_dispute == 0 ~ NA_real_,
          x %in% c(1,2) ~ 1,
          x %in% c(3,4) ~ 0,
          x %in% c(5,6) ~ NA_real_
        ),
        .names = "{str_remove(.col, 'AJR_')}_bin"
      ),
      across(
        c(AJR_drm_1_f, AJR_drm_2_f, AJR_drm_3_f, AJR_drm_4_f, AJR_drm_5_f, AJR_drm_6_f,
          AJR_drm_7_f, AJR_drm_8_f, AJR_drm_9_f, AJR_drm_11_f),
        \(x) case_when(
          had_dispute == 0 ~ NA_real_,
          x %in% c(1,2) ~ 1,
          x %in% c(3,4) ~ 0,
          x %in% c(5,6) ~ NA_real_
        ),
        .names = "{str_remove(.col, 'AJR_')}_bin"
      ),
      across(
        c(AJR_drm_1_d, AJR_drm_2_d, AJR_drm_3_d, AJR_drm_4_d, AJR_drm_5_d, AJR_drm_6_d,
          AJR_drm_7_d, AJR_drm_8_d, AJR_drm_9_d, AJR_drm_11_d),
        \(x) case_when(
          had_dispute == 0 ~ NA_real_,
          x %in% c(1,2) ~ 1,
          x %in% c(3,4) ~ 0,
          x %in% c(5,6) ~ NA_real_
        ),
        .names = "{str_remove(.col, 'AJR_')}_bin"
      ),
      across(
        c(AJR_drm_1_e, AJR_drm_2_e, AJR_drm_3_e, AJR_drm_4_e, AJR_drm_5_e, AJR_drm_6_e,
          AJR_drm_7_e, AJR_drm_8_e, AJR_drm_9_e, AJR_drm_11_e),
        \(x) case_when(
          had_dispute == 0 ~ NA_real_,
          x %in% c(1,2) ~ 1,
          x %in% c(3,4) ~ 0,
          x %in% c(5,6) ~ NA_real_
        ),
        .names = "{str_remove(.col, 'AJR_')}_bin"
      ),
      across(
        c(AJR_drm_1_h, AJR_drm_2_h, AJR_drm_3_h, AJR_drm_4_h, AJR_drm_5_h, AJR_drm_6_h,
          AJR_drm_7_h, AJR_drm_8_h, AJR_drm_9_h, AJR_drm_11_h),
        \(x) case_when(
          had_dispute == 0 ~ NA_real_,
          x %in% c(1,2) ~ 1,
          x %in% c(3,4) ~ 0,
          x %in% c(5,6) ~ NA_real_
        ),
        .names = "{str_remove(.col, 'AJR_')}_bin"
      ),
      
      # Issue resolution
      AJR_settle_resol_1 = if_else(AJR_settle_resol==1,1,0,missing=NA),
      AJR_settle_resol_2 = if_else(AJR_settle_resol==2,1,0,missing=NA),
      AJR_settle_resol_3 = if_else(AJR_settle_resol==3,1,0,missing=NA),
      AJR_settle_resol_4 = if_else(AJR_settle_resol==4,1,0,missing=NA),
      AJR_settle_resol_5 = if_else(AJR_settle_resol==5,1,0,missing=NA),
      AJR_settle_resol_6 = if_else(AJR_settle_resol==6,1,0,missing=NA),
      AJR_settle_resol_7 = if_else(AJR_settle_resol==7,1,0,missing=NA),
      AJR_settle_resol_8 = if_else(AJR_settle_resol==8,1,0,missing=NA),
      AJR_settle_resol_9 = if_else(AJR_settle_resol==9,1,0,missing=NA),
      AJR_settle_resol_10 = if_else(AJR_settle_resol==10,1,0,missing=NA),
      AJR_settle_resol_11 = if_else(AJR_settle_resol==11,1,0,missing=NA),
  
      # Timeliness
      start_date = make_date(AJR_year_start, AJR_month_start, 1),
      end_date   = make_date(AJR_year_end,   AJR_month_end,   1),
      months_diff = if_else(!is.na(start_date) & !is.na(end_date),
                            interval(start_date, end_date) %/% months(1),
                            as.integer(NA)),
      timeliness = case_when(
        had_dispute == 0 ~ NA_real_,
        AJR_status %in% c(1,4,5) ~ NA_real_,
        months_diff <= 12 ~ 1,
        months_diff > 12 ~ 0
      ),
      
      # Fairness of the process (global)
      fair = case_when(
        had_dispute == 0 ~ NA_real_,
        AJR_fair == 1 ~ 1,
        AJR_fair == 2 ~ 0
      ),
      
      # Outcome
      outcome_done = case_when(
        AJR_status == 1 ~ NA_real_,
        AJR_status == 2 ~ 0,
        AJR_status == 3 ~ 1,
        AJR_status %in% c(4,5) ~ NA_real_
      ),
      
      # Severity of impact
      impact = case_when(
        AJE_impact %in% c(1,2) ~ 0,
        AJE_impact %in% c(3,4,5) ~ 1
      ),
      
      # Hardships (bin)
      across(
        c(AJE_hardship_1, AJE_hardship_2, AJE_hardship_3, AJE_hardship_4, AJE_hardship_5, AJE_hardship_6,
          AJE_hardship_7, AJE_hardship_8, AJE_hardship_9, AJE_hardship_10, AJE_hardship_11, AJE_hardship_12,
          AJE_hardship_13, AJE_hardship_14, AJE_hardship_15, AJE_hardship_16),
        \(x) case_when(
          had_dispute == 0 ~ NA_real_,
          x == 1 ~ 1,
          x == 2 ~ 0,
          x == 3 ~ NA_real_
        ),
        .names = "{str_remove(.col, 'AJE_')}_bin"
      ),
      
      #Hardship - overall
      had_hardship = case_when(
        had_dispute == 0 ~ NA_real_,
        hardship_1_bin ==1 | hardship_2_bin ==1 | hardship_3_bin ==1 | 
        hardship_4_bin ==1 | hardship_5_bin ==1 | hardship_6_bin ==1 | 
        hardship_7_bin ==1 | hardship_8_bin ==1 | hardship_9_bin ==1 | 
        hardship_10_bin ==1 | hardship_11_bin ==1 | hardship_12_bin ==1 | 
        hardship_13_bin ==1 | hardship_14_bin ==1 | hardship_15_bin ==1 | 
        hardship_16_bin ==1 ~ 1,
        hardship_1_bin ==0 & hardship_2_bin ==0 & hardship_3_bin ==0 & 
        hardship_4_bin ==0 & hardship_5_bin ==0 & hardship_6_bin ==0 & 
        hardship_7_bin ==0 & hardship_8_bin ==0 & hardship_9_bin ==0 & 
        hardship_10_bin ==0 & hardship_11_bin ==0 & hardship_12_bin ==0 & 
        hardship_13_bin ==0 & hardship_14_bin ==0 & hardship_15_bin ==0 & 
        hardship_16_bin ==0 ~ 0,
        had_dispute == 1 ~ 0
      ),

      # Legal capability
      legal_rights = case_when(
        AJE_legalrights_done %in% c(1,2) | AJE_legalrights_ongoing %in% c(1,2) ~ 1,
        AJE_legalrights_done %in% c(3,4) | AJE_legalrights_ongoing %in% c(3,4) ~ 0
      ),
      expert_help = case_when(
        AJE_advice_done %in% c(1,2) | AJE_advice_ongoing %in% c(1,2) ~ 1,
        AJE_advice_done %in% c(3,4) | AJE_advice_ongoing %in% c(3,4) ~ 0
      ),
      fair_outcome = case_when(
        AJE_fairoutcome_done %in% c(1,2) | AJE_fairoutcome_ongoing %in% c(1,2) ~ 1,
        AJE_fairoutcome_done %in% c(3,4) | AJE_fairoutcome_ongoing %in% c(3,4) ~ 0
      ),
      
      # Cambiar AJD_adviser_* a binario 0/1
      across(
        starts_with("AJD_adviser_"),
        ~ case_when(.x == 1 ~ 1,
                    .x == 2 ~ 0)
      )
    )
  
  # ===========================================================================
  # 2) Sociodemográficos
  # ===========================================================================
  data_subset.df <- data_subset.df %>%
    mutate(
      gender = case_when(
        gend == 1 ~ "Male",
        gend == 2 ~ "Female"
      ),
      age_group = case_when(
        age == 1 | (age >= 18 & age <= 24)  ~ "18-24",
        age == 2 | (age >= 25 & age <= 34)  ~ "25-34",
        age == 3 | (age >= 35 & age <= 44)  ~ "35-44",
        age == 4 | (age >= 45 & age <= 54)  ~ "45-54",
        age == 5 | (age >= 55 & age <= 64)  ~ "55-64",
        age == 6 | (age >= 65 & age <= 100) ~ "65-100"
      ),
      edu_level = case_when(
        edu_2 == 1 ~ "Lower Education",
        edu_2 == 2 ~ "Higher Education"
      ),
      ethnic_majority = case_when(
        ethni == 1 ~ "Ethnic majority",
        ethni %in% c(2,3,4,5,6,7,8,9,10,11,12,13,14) ~ "Ethnic minority"
      ),
      disability = case_when(
        disability %in% c(1,2) ~ "With disability",
        disability == 3 ~ "Without disability"
      ),
      income = case_when(
        income == 1 ~ "< €30k a year",
        income == 2 ~ "€30k – €70 a year",
        income == 3 ~ "€70k – €120k a year",
        income == 4 ~ "> €120k a year"
      ),
      income = factor(income, c("< €30k a year",
                                "€30k – €70 a year",
                                "€70k – €120k a year",
                                "> €120k a year")),
      emp_status = case_when(
        emp %in% c(1,2) ~ "Employed",
        emp %in% c(3,4,5,6,7,8) ~ "Not employed"
      ),
      nationality = case_when(
        nation == 1 ~ "National",
        nation == 2 ~ "Foreigner"
      ),
      marital_status = case_when(
        marital %in% c(2,3) ~ "Married",
        marital %in% c(1,4,5,6) ~ "Not married"
      ),
      NUTS = case_when(
        region == 1 ~ "Dublin",
        region == 2 ~ "Leinster",
        region == 3 ~ "Munster",
        region == 4 ~ "Ulster/Connacht"
      ),
      level_impact = case_when(
        impact == 1 ~ "High impact",
        impact == 0 ~ "Low impact"
      ),
      cooccurence_group = as.character(cooccurence_group)
    )
  
  # ============================================================================
  # 3) Special wrangling - NONE (all the wrangling was done in 2)
  # ============================================================================
  
  return(data_subset.df)
  
}
