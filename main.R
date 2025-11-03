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


data_subset <- master_data %>%
  mutate(
    
    #Prevalence
    prevalence = had_dispute,
    
    
    #Problem categories (ALL MENTIONED PROBLEMS)
    problem_cat_land = if_else(
      AJP_A1 == 1
      , 1 , 0, 0),
    problem_cat_neighbors = if_else(
      (AJP_B1 == 1 | AJP_B2 == 1 | AJP_B3 == 1)
      , 1 , 0, 0),
    problem_cat_housing = if_else(
      (AJP_C1 == 1 | AJP_C2 == 1 | AJP_C3 == 1 | AJP_D1 == 1 | AJP_D2 == 1 | AJP_D3 == 1 | 
         AJP_E1 == 1 | AJP_E2 == 1 | AJP_E3 == 1 | AJP_E4 == 1 | AJP_E5 == 1 | AJP_E6 == 1 | 
         AJP_E7 == 1 )
      , 1 , 0, 0),
    problem_cat_family = if_else(
      (AJP_F1 == 1 | AJP_F2 == 1 | AJP_F3 == 1 | AJP_F4 == 1 | AJP_F5 == 1 | AJP_F6 == 1)
      , 1 , 0, 0),
    problem_cat_injury = if_else(
      (AJP_H1 == 1 | AJP_H2 == 1 | AJP_H3 == 1 | AJP_H4 == 1 | AJP_H5 == 1)
      , 1 , 0, 0),
    problem_cat_citizen = if_else(
      (AJP_G1 == 1 | AJP_G2 == 1 | AJP_G3 == 1 | AJP_G4 == 1 | AJP_G5 == 1)
      , 1 , 0, 0),
    problem_cat_gov = if_else(
      (AJP_I1 == 1 | AJP_I2 == 1 | AJP_I3 == 1 | AJP_I4 == 1 | AJP_I5 == 1 | AJP_I6 == 1)
      , 1 , 0, 0),
    problem_cat_public = if_else(
      (AJP_J1 == 1 | AJP_J2 == 1 | AJP_J3 == 1 | AJP_J4 == 1)
      , 1 , 0, 0),
    problem_cat_products = if_else(
      (AJP_K1 == 1 | AJP_K2 == 1)
      , 1 , 0, 0),
    problem_cat_services = if_else(
      (AJP_L1 == 1 | AJP_L2 == 1)
      , 1 , 0, 0),
    problem_cat_money = if_else(
      (AJP_M1 == 1 | AJP_M2 == 1 | AJP_M3 == 1 | AJP_M4 == 1 | AJP_M5 == 1)
      , 1 , 0, 0),
    problem_cat_employment = if_else(
      (AJP_N1 == 1 | AJP_N2 == 1 | AJP_N3 == 1 | AJP_N4 == 1)
      , 1 , 0, 0),

    #Co-occurence
    cooccurence_group = case_when(
      ndisputes == 0 ~ NA_character_,
      ndisputes == 1 ~ "1 problem",
      ndisputes <= 3 ~ "2-3 problems",
      ndisputes <= 5 ~ "4-5 problems",
      ndisputes >= 6 ~ "5 or more problems"
    ),
    
    #Problem category (SELECTED PROBLEM)
    category = case_when(
      AJP_cat_selected == 1 ~ "Land",
      AJP_cat_selected == 2 ~ "Neighbors",
      AJP_cat_selected == 3 ~ "Housing",
      AJP_cat_selected == 4 ~ "Family/ relationship",
      AJP_cat_selected == 5 ~ "Injury",
      AJP_cat_selected == 6 ~ "Citizenship or migration",
      AJP_cat_selected == 7 ~ "Government benefits and payments",
      AJP_cat_selected == 8 ~ "Public services",
      AJP_cat_selected == 9 ~ "Products",
      AJP_cat_selected == 10 ~ "Services",
      AJP_cat_selected == 11 ~ "Money/ debt",
      AJP_cat_selected == 12 ~ "Employment",
    ),
    
    #Access to appropriate information and advice
    access2info = case_when(
      had_dispute == 0   ~ NA_real_,
      AJE_infosource_done == 1 | AJE_infosource_done ==2 | 
      AJE_infosource_ongoing == 1 | AJE_infosource_ongoing ==2  ~ 1,
      AJE_infosource_done == 3 | AJE_infosource_done ==4 | 
      AJE_infosource_ongoing == 3 | AJE_infosource_ongoing ==4  ~ 0,    
    ), 
    
    #Contacted an adviser (ALL)
    contact_adviser = case_when(
      had_dispute == 0   ~ NA_real_,
      AJD_adviser_1 == 1 | AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
        AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1 |
        AJD_adviser_9 == 1 | AJD_adviser_10 == 1 | AJD_adviser_11 == 1 | AJD_adviser_12 == 1 |
        AJD_adviser_13 == 1 | AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 |
        AJD_adviser_17 == 1 ~ 1,
      AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2  & AJD_adviser_4 == 2 & 
        AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & 
        AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 & 
        AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & 
        AJD_adviser_17 == 2 ~ 0
    ),
    
    #Appropriate advisor
    appropriate_adviser = case_when(
      had_dispute == 0   ~ NA_real_,
      AJD_adviser_1 ==1 | AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
        AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1 |
        AJD_adviser_9 == 1 | AJD_adviser_10 == 1 | AJD_adviser_11 == 1 | AJD_adviser_12 ==1 |
        AJD_adviser_13 == 1 ~ 1,
      AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
        AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
        AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
        AJD_adviser_13 == 2 ~ 0
    ),
    
    #Non-appropriate advisor
    no_appropriate_adviser = case_when(
      had_dispute == 0   ~ NA_real_,
      (AJD_adviser_14==1 | AJD_adviser_15==1 | AJD_adviser_16==1 | AJD_adviser_17==1) 
      & 
       (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
           AJD_adviser_13 == 2) ~ 1,
      (AJD_adviser_1 ==1 | AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
         AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1 |
         AJD_adviser_9 == 1 | AJD_adviser_10 == 1 | AJD_adviser_11 == 1 | AJD_adviser_12 ==1 |
         AJD_adviser_13 == 1) ~ 0,
      had_dispute == 1 ~ 0
    ),
    
    #Did not need assistance (ALL advisers)
    no_need_assistance = case_when(
      (AJR_noaction_1 == 1 | AJR_noaction_2 ==1 | AJR_noaction_6 ==1 | AJR_noaction_10 ==1 
       | AJR_noaction_12 ==1) 
      &
        (AJR_noaction_3 ==0 & AJR_noaction_4 ==0 &  AJR_noaction_5 ==0 &  AJR_noaction_7 ==0 & 
           AJR_noaction_8 ==0 &  AJR_noaction_9 ==0 &  AJR_noaction_11 ==0 & AJR_noaction_13 ==0) ~ 1,
      AJR_action ==4 ~ 1 ,
      had_dispute == 1 ~ 0
    ),
    
    #Needed but did not have access to an adviser
    needed_assistance = case_when(
      (AJR_noaction_3 ==1 | AJR_noaction_4 ==1 | AJR_noaction_5 ==1 | AJR_noaction_7 ==1 | 
         AJR_noaction_8 ==1 | AJR_noaction_9 ==1 | AJR_noaction_11 ==1 | AJR_noaction_13 ==1) ~ 1,
      had_dispute == 1 ~ 0
    ),
    
    #Did not need assistance - prof advisers
    no_need_assistance_prof = case_when(
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2) 
      &
        (AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1 )
      &
        (AJD_noadvice_reason_1 ==1 | AJD_noadvice_reason_2==1 | AJD_noadvice_reason_3==1 | 
           AJD_noadvice_reason_4==1 | AJD_noadvice_reason_5==1 | AJD_noadvice_reason_6==1 ) &
        (AJD_noadvice_reason_7== 0 & AJD_noadvice_reason_8== 0 & AJD_noadvice_reason_9== 0 & 
           AJD_noadvice_reason_10== 0 & AJD_noadvice_reason_11== 0 & AJD_noadvice_reason_12== 0 & 
           AJD_noadvice_reason_13== 0 & AJD_noadvice_reason_14== 0 &  AJD_noadvice_reason_15== 0 & 
           AJD_noadvice_reason_16 == 0) ~ 1,
      had_dispute == 1 ~ 0
    ),
    
    #Needed assistance - prof advisers
    needed_assistance_prof = case_when(
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & 
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & 
         AJD_adviser_13 == 2) 
      & 
        (AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1 ) 
      &
        (AJD_noadvice_reason_7== 1 | AJD_noadvice_reason_8== 1 | AJD_noadvice_reason_9== 1 | 
           AJD_noadvice_reason_10== 1 | AJD_noadvice_reason_11== 1 | AJD_noadvice_reason_12== 1 | 
           AJD_noadvice_reason_13== 1 | AJD_noadvice_reason_14== 1 |  AJD_noadvice_reason_15== 1 | 
           AJD_noadvice_reason_16 == 1 ) ~ 1,
      had_dispute == 1 ~ 0
    ),
    
    #Unknown 
    unknown = case_when(
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & 
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & 
         AJD_adviser_13 == 2) 
      & 
        (AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1 ) 
      &
        (AJD_noadvice_reason_1 ==0 & AJD_noadvice_reason_2==0 &  AJD_noadvice_reason_3==0 &  
           AJD_noadvice_reason_4==0 &  AJD_noadvice_reason_5==0 &  AJD_noadvice_reason_6==0 & 
           AJD_noadvice_reason_7== 0 & AJD_noadvice_reason_8== 0 & AJD_noadvice_reason_9== 0 & 
           AJD_noadvice_reason_10== 0 & AJD_noadvice_reason_11== 0 & AJD_noadvice_reason_12== 0 & 
           AJD_noadvice_reason_13== 0 & AJD_noadvice_reason_14== 0 &  AJD_noadvice_reason_15== 0 & 
           AJD_noadvice_reason_16 == 0 ) 
      &
        AJD_noadvice_reason_17 == 1 ~1,
      AJR_action ==3 ~ 1,
      had_dispute == 1 ~ 0
    ),
    
    #Access to appropriate/professional assistance and representation
    access2rep = case_when(
      needed_assistance == 1 ~ 0,
      needed_assistance_prof == 1 ~ 0,
      unknown == 1 ~ 0,
      no_need_assistance_prof ==1 ~ 1,
      no_need_assistance == 1 ~ 1,
      appropriate_adviser == 1 ~ 1
    ),
    
    
    #Helpfulness by adviser
    across(
      c(AJD_adviser_help_1, AJD_adviser_help_2, AJD_adviser_help_3, AJD_adviser_help_4, 
        AJD_adviser_help_5, AJD_adviser_help_6, AJD_adviser_help_7, AJD_adviser_help_8, 
        AJD_adviser_help_9, AJD_adviser_help_10, AJD_adviser_help_11, AJD_adviser_help_12, 
        AJD_adviser_help_13, AJD_adviser_help_14, AJD_adviser_help_15, AJD_adviser_help_16, 
        AJD_adviser_help_17),
      \(x) case_when(
        had_dispute == 0   ~ NA_real_,
        (x == 1 | x == 2) ~ 0,
        (x == 3 | x == 4) ~ 1,
        (x == 5 | x == 6) ~ NA_real_,
      ),
      .names = "{str_remove(.col, 'AJD_')}_bin"
    ),
    
    #Barriers in the Access to Assistance and Representation (for professional help)
    reason_no_need = case_when(
      (AJD_noadvice_reason_1 == 1 | AJD_noadvice_reason_2 == 1 | AJD_noadvice_reason_3 == 1 | 
        AJD_noadvice_reason_5 == 1 | AJD_noadvice_reason_6 == 1) ~ 1,
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
           AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
           AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
           AJD_adviser_13 == 2) ~ 0,
    ),
    reason_had_help = case_when(
      (AJD_noadvice_reason_4 ==1 | AJD_noadvice_reason_7 ==1 | AJD_noadvice_reason_8 ==1 | 
         AJD_noadvice_reason_9 ==1) ~ 1,
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2) ~ 0, 
    ),
    reason_info_bar = case_when(
      AJD_noadvice_reason_13 == 1 ~ 1,
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2) ~ 0, 
    ),
    reason_relation_bar = case_when(
      (AJD_noadvice_reason_11 == 1 | AJD_noadvice_reason_12 == 1) ~ 1,
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2) ~ 0, 
    ),
    reason_psycho_bar = case_when(
      (AJD_noadvice_reason_10== 1 | AJD_noadvice_reason_14 == 1) ~ 1,
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2) ~ 0, 
    ),
    reason_prior = case_when(
      AJD_noadvice_reason_15 == 1 ~ 1,
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2) ~ 0,
    ),
    reason_social = case_when(
      AJD_noadvice_reason_16==1 ~ 1,
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2) ~ 0, 
      ),
    reason_other = case_when(
      AJD_noadvice_reason_17==1 ~ 1,
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2) ~ 0, 
    ),
    
    #Non-seekers’ intention (People that did not access ANY advisor)
    AJR_action_1 = case_when(
      AJR_action==1 ~ 1,
      AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2  & AJD_adviser_4 == 2 & 
        AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & 
        AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 & 
        AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & 
        AJD_adviser_17 == 2 ~ 0
    ),
    AJR_action_2 = case_when(
      AJR_action==2 ~ 1,
      AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2  & AJD_adviser_4 == 2 & 
        AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & 
        AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 & 
        AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & 
        AJD_adviser_17 == 2 ~ 0
    ),
    AJR_action_3 = case_when(
      AJR_action==3 ~ 1,
      AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2  & AJD_adviser_4 == 2 & 
        AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & 
        AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 & 
        AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & 
        AJD_adviser_17 == 2 ~ 0
    ),
    AJR_action_4 = case_when(
      AJR_action==4 ~ 1,
      AJD_adviser_1 == 2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2  & AJD_adviser_4 == 2 & 
        AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & 
        AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 == 2 & 
        AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & 
        AJD_adviser_17 == 2 ~ 0
    ),
    
    #Reasons for not seeking help (NO to ALL advisers)
    #These are variables AJR_noaction_1-AJR_noaction_13. No need to do extra calculations 
    
    #Contacted a DRM
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
    
    #Access to a dispute resolution mechanism (SDG 16.3.3)
    access2drm = case_when(
      contacted_drm ==1 ~ 1 ,
      contacted_drm ==0  
      &
      (AJR_noresol_reason_7 == 1 | AJR_noresol_reason_8 ==1 | AJR_noresol_reason_9 == 1  | 
      AJR_noresol_reason_10 == 1 | AJR_noresol_reason_11 ==1 | AJR_noresol_reason_12 ==1 | 
      AJR_noresol_reason_13 ==1 | AJR_noresol_reason_14 == 1 | AJR_noresol_reason_15 == 1 ) ~ 0
    ),
    
    #Needed but didn't have access to a DRM
    needed_drm = case_when(
      had_dispute == 0 ~ NA_real_,
      (AJR_noresol_reason_7 == 1 | AJR_noresol_reason_8 ==1 | AJR_noresol_reason_9 == 1  | 
         AJR_noresol_reason_10 == 1 | AJR_noresol_reason_11 ==1 | AJR_noresol_reason_12 ==1 | 
         AJR_noresol_reason_13 ==1 | AJR_noresol_reason_14 == 1 | AJR_noresol_reason_15 == 1 ) ~ 1,
      (AJR_drm_1_bin == 0 & AJR_drm_2_bin == 0 & AJR_drm_3_bin == 0 & AJR_drm_4_bin == 0 & 
         AJR_drm_5_bin == 0 & AJR_drm_6_bin == 0 & AJR_drm_7_bin == 0 & AJR_drm_8_bin == 0 & 
         AJR_drm_9_bin == 0 & AJR_drm_11_bin == 0) ~ 0,
      (AJR_noresol_reason_7 == 0  & AJR_noresol_reason_8 == 0 & AJR_noresol_reason_9 == 0  & 
         AJR_noresol_reason_10 == 0 & AJR_noresol_reason_11 ==0 & AJR_noresol_reason_12 ==0  & 
         AJR_noresol_reason_13 ==0 & AJR_noresol_reason_14 == 0 & AJR_noresol_reason_15 == 0 ) ~ 0
    ),
    
    #Did not need to turn to a DRM
    no_need_drm = case_when(
      had_dispute == 0 ~ NA_real_,
      (AJR_noresol_reason_1 == 1 | AJR_noresol_reason_2== 1 | AJR_noresol_reason_3== 1 | 
         AJR_noresol_reason_4== 1 | AJR_noresol_reason_5== 1 | AJR_noresol_reason_6== 1) 
      &
      (AJR_noresol_reason_7 == 0  & AJR_noresol_reason_8 == 0 & AJR_noresol_reason_9 == 0  & 
         AJR_noresol_reason_10 == 0 & AJR_noresol_reason_11 ==0 & AJR_noresol_reason_12 ==0  & 
         AJR_noresol_reason_13 ==0 & AJR_noresol_reason_14 == 0 & AJR_noresol_reason_15 == 0 ) ~ 1,
    
      (AJR_noresol_reason_7 == 1 | AJR_noresol_reason_8 ==1 | AJR_noresol_reason_9 == 1  | 
         AJR_noresol_reason_10 == 1 | AJR_noresol_reason_11 ==1 | AJR_noresol_reason_12 ==1 | 
         AJR_noresol_reason_13 ==1 | AJR_noresol_reason_14 == 1 | AJR_noresol_reason_15 == 1 ) ~ 0,
      
      AJR_noresol_reason_16 == 1 ~ 0,
      
      (AJR_noresol_reason_1 == 0 & AJR_noresol_reason_2== 0 & AJR_noresol_reason_3== 0 & 
         AJR_noresol_reason_4== 0 & AJR_noresol_reason_5== 0 & AJR_noresol_reason_6== 0) ~ 0,
      
      (AJR_drm_1_bin == 0 & AJR_drm_2_bin == 0 & AJR_drm_3_bin == 0 & AJR_drm_4_bin == 0 & 
         AJR_drm_5_bin == 0 & AJR_drm_6_bin == 0 & AJR_drm_7_bin == 0 & AJR_drm_8_bin == 0 & 
         AJR_drm_9_bin == 0 & AJR_drm_11_bin == 0) ~ 0
    ),
    
    #Unknown
    unknown_drm = case_when(
      had_dispute == 0 ~ NA_real_,
      AJR_noresol_reason_16 ==1 & 
      (AJR_noresol_reason_7 == 0  & AJR_noresol_reason_8 == 0 & AJR_noresol_reason_9 == 0 & 
        AJR_noresol_reason_10 == 0 & AJR_noresol_reason_11 ==0 & AJR_noresol_reason_12 ==0 & 
        AJR_noresol_reason_13 ==0 & AJR_noresol_reason_14 == 0 & AJR_noresol_reason_15 == 0 ) ~ 1,
      
      (AJR_noresol_reason_1 == 1 | AJR_noresol_reason_2== 1 | AJR_noresol_reason_3== 1 | 
         AJR_noresol_reason_4== 1 | AJR_noresol_reason_5== 1 | AJR_noresol_reason_6== 1 |
         AJR_noresol_reason_7 == 1 | AJR_noresol_reason_8 ==1 | AJR_noresol_reason_9 == 1  | 
         AJR_noresol_reason_10 == 1 | AJR_noresol_reason_11 ==1 | AJR_noresol_reason_12 ==1 | 
         AJR_noresol_reason_13 ==1 | AJR_noresol_reason_14 == 1 | AJR_noresol_reason_15 == 1) ~ 0,
      
      (AJR_drm_1_bin == 0 & AJR_drm_2_bin == 0 & AJR_drm_3_bin == 0 & AJR_drm_4_bin == 0 & 
         AJR_drm_5_bin == 0 & AJR_drm_6_bin == 0 & AJR_drm_7_bin == 0 & AJR_drm_8_bin == 0 & 
         AJR_drm_9_bin == 0 & AJR_drm_11_bin == 0) ~ 0
    ),
    
    #Reasons for not accessing a DRM
    #These are variables AJR_noresol_reason_1- AJR_noresol_reason_16. No calculations needed

    # Efficiency by each mechanism 
    across(
      c(AJR_drm_1_c, AJR_drm_2_c, AJR_drm_3_c, AJR_drm_4_c, AJR_drm_5_c, AJR_drm_6_c, AJR_drm_7_c, 
        AJR_drm_8_c, AJR_drm_9_c, AJR_drm_11_c),
      \(x) case_when(
        had_dispute == 0   ~ NA_real_,
        (x == 1 | x == 2) ~ 1,
        (x == 3 | x == 4) ~ 0,
        (x == 5 | x == 6) ~ NA_real_,
      ),
      .names = "{str_remove(.col, 'AJR_')}_bin"
    ),
    
    # Fairness of the dispute resolution process by each mechanism 
    across(
      c(AJR_drm_1_f, AJR_drm_2_f, AJR_drm_3_f, AJR_drm_4_f, AJR_drm_5_f, AJR_drm_6_f, AJR_drm_7_f, 
        AJR_drm_8_f, AJR_drm_9_f, AJR_drm_11_f),
      \(x) case_when(
        had_dispute == 0   ~ NA_real_,
        (x == 1 | x == 2) ~ 1,
        (x == 3 | x == 4) ~ 0,
        (x == 5 | x == 6) ~ NA_real_,
      ),
      .names = "{str_remove(.col, 'AJR_')}_bin"
    ),
  
    # Affordability by each mechanism 
    across(
      c(AJR_drm_1_d, AJR_drm_2_d, AJR_drm_3_d, AJR_drm_4_d, AJR_drm_5_d, AJR_drm_6_d, AJR_drm_7_d, 
        AJR_drm_8_d, AJR_drm_9_d, AJR_drm_11_d),
      \(x) case_when(
        had_dispute == 0   ~ NA_real_,
        (x == 1 | x == 2) ~ 1,
        (x == 3 | x == 4) ~ 0,
        (x == 5 | x == 6) ~ NA_real_,
      ),
      .names = "{str_remove(.col, 'AJR_')}_bin"
    ),
    
    # Duration by each mechanism
    across(
      c(AJR_drm_1_e, AJR_drm_2_e, AJR_drm_3_e, AJR_drm_4_e, AJR_drm_5_e, AJR_drm_6_e, AJR_drm_7_e, 
        AJR_drm_8_e, AJR_drm_9_e, AJR_drm_11_e),
      \(x) case_when(
        had_dispute == 0   ~ NA_real_,
        (x == 1 | x == 2) ~ 1,
        (x == 3 | x == 4) ~ 0,
        (x == 5 | x == 6) ~ NA_real_,
      ),
      .names = "{str_remove(.col, 'AJR_')}_bin"
    ),
    
    # Helpfulness by each mechanism   
    across(
      c(AJR_drm_1_h, AJR_drm_2_h, AJR_drm_3_h, AJR_drm_4_h, AJR_drm_5_h, AJR_drm_6_h, AJR_drm_7_h, 
        AJR_drm_8_h, AJR_drm_9_h, AJR_drm_11_h),
      \(x) case_when(
        had_dispute == 0   ~ NA_real_,
        (x == 1 | x == 2) ~ 1,
        (x == 3 | x == 4) ~ 0,
        (x == 5 | x == 6) ~ NA_real_,
      ),
      .names = "{str_remove(.col, 'AJR_')}_bin"
    ),   
    
    
    #Timeliness of the process
    start_date = make_date(AJR_year_start, AJR_month_start, 1),
    end_date   = make_date(AJR_year_end, AJR_month_end, 1),
    months_diff = interval(start_date, end_date) %/% months(1),
    
    timeliness = case_when(
      had_dispute == 0   ~ NA_real_,
      AJR_status ==1 | AJR_status == 4 | AJR_status == 5 ~ NA_real_,
      months_diff<= 12 ~ 1,
      months_diff> 12 ~ 0
    ),    
 
    #Fairness of the process
    fair = case_when(
      had_dispute == 0   ~ NA_real_,
      AJR_fair == 1 ~ 1,
      AJR_fair == 2 ~ 0
      ),
    
    # Outcome
    outcome_done = case_when(
      AJR_status == 1 ~ NA_real_,
      AJR_status == 2 ~ 0 ,
      AJR_status == 3 ~ 1 ,
      AJR_status == 4 ~ NA_real_,
      AJR_status == 5 ~ NA_real_
    ),
    
    #Severity of impact
    impact = case_when(
      (AJE_impact == 1 | AJE_impact == 2) ~ 0,
      (AJE_impact == 3 | AJE_impact == 4| AJE_impact == 5) ~ 1
    ),
    
    #Hardships
    across(
      c(AJE_hardship_1, AJE_hardship_2, AJE_hardship_3, AJE_hardship_4, AJE_hardship_5, AJE_hardship_6,
        AJE_hardship_7, AJE_hardship_8, AJE_hardship_9, AJE_hardship_10, AJE_hardship_11, AJE_hardship_12,
        AJE_hardship_13, AJE_hardship_14, AJE_hardship_15, AJE_hardship_16),
      \(x) case_when(
        had_dispute == 0   ~ NA_real_,
        (x == 1 ) ~ 1,
        (x == 2) ~ 0,
        (x == 3) ~ NA_real_,
      ),
      .names = "{str_remove(.col, 'AJE_')}_bin"
    ),  
  
    #Legal capability
    legal_rights = case_when(
      (AJE_legalrights_done == 1 | AJE_legalrights_done == 2 | AJE_legalrights_ongoing == 1 | 
         AJE_legalrights_ongoing == 2 ) ~ 1,
      (AJE_legalrights_done == 3 | AJE_legalrights_done == 4 | AJE_legalrights_ongoing == 3 | 
          AJE_legalrights_ongoing == 4 ) ~ 0
    ),
    
    expert_help = case_when(
      (AJE_advice_done == 1 | AJE_advice_done == 2 | 
         AJE_advice_ongoing == 1 | AJE_advice_ongoing == 2) ~ 1,
      (AJE_advice_done == 3 | AJE_advice_done == 4 | 
         AJE_advice_ongoing == 3 | AJE_advice_ongoing == 4) ~ 0,
    ),
    
    fair_outcome = case_when(
      (AJE_fairoutcome_done == 1 | AJE_fairoutcome_done==2 | 
         AJE_fairoutcome_ongoing ==1 | AJE_fairoutcome_ongoing ==2) ~ 1,
      (AJE_fairoutcome_done == 3 | AJE_fairoutcome_done==4 | 
         AJE_fairoutcome_ongoing ==3 | AJE_fairoutcome_ongoing ==4) ~ 0,
    ),
  
  # Changing AJE_adviser to binary
    across(
      starts_with(paste0("AJD_adviser_",1:17)),
      ~case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0
      )
    )
  
  )

### Demographics
    
data_subset.df <- data_subset %>%
  mutate(
    
    gender = case_when(
      gend == 1 ~ "Male",
      gend == 2 ~ "Female"
    ),
    #gender = factor(gender, levels = c("Female", "Male")),
    
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
  #edu_level = factor(edu_level, levels = c("No Higher Education", "Higher Education")),
  
  ethnic_majority = case_when(
    ethni == 1 ~ "Ethnic majority",
    ethni %in% c(2,3,4,5,6,7,8,9,10,11,12,13,14) ~ "Ethnic minority"
  ),
  
  disability = case_when(
    (disability == 1 | disability == 2) ~ "With disability",
     disability == 3 ~ "Without disability"
  ),
  
  income = case_when(
    income == 1 ~ "> Ç30k a year",
    income == 2 ~ "Ç30k–Ç70 a year",
    income == 3 ~ "Ç70k–Ç120k a year",
    income == 4 ~ "< Ç120k a year"
  ),
  
  emp_status = case_when(
    emp %in% c(1,2) ~ "Employed",
    emp %in% c(3,4,5,6,7,8) ~ "Not employed",
  ),
  
  nationality = case_when(
    nation == 1 ~ "National",
    nation == 2 ~ "Foreigner",
  ),
  #nationality = factor(nationality, levels = c("Foreigner", "National")),
  
  marital_status = case_when(
    marital %in% c(2,3) ~ "Married",
    marital %in% c(1,4,5,6) ~ "Not married"
  ),
  #marital_status = factor(marital_status, levels = c("Married", "Not married")),
  
  NUTS = 
    case_when(
      region == 1 ~ "Dublin",
      region == 2 ~ "Leinster",
      region == 3 ~ "Munster",
      region == 4 ~ "Ulster"
    ),
  
  level_impact = 
    case_when(
      impact == 1 ~ "High impact",
      impact == 0 ~ "Low impact"
    ),
  cooccurence_group = as.character(cooccurence_group)
)



# Special wrangling

# 1) Fix the name typo and keep values in 0–1 (means already are proportions)
data_drm <- data_subset.df %>%
  mutate(
    affordable_court             = case_when(AJR_drm_1_d %in% c(1,2) ~ 1,
                                             AJR_drm_1_d %in% c(3,4) ~ 0,
                                             TRUE ~ NA_real_),
    affordable_tribunal          = case_when(AJR_drm_2_d %in% c(1,2) ~ 1,
                                             AJR_drm_2_d %in% c(3,4) ~ 0,
                                             TRUE ~ NA_real_),
    affordable_ombudsman         = case_when(AJR_drm_3_d %in% c(1,2) ~ 1,
                                             AJR_drm_3_d %in% c(3,4) ~ 0,
                                             TRUE ~ NA_real_),
    affordable_police            = case_when(AJR_drm_4_d %in% c(1,2) ~ 1,
                                             AJR_drm_4_d %in% c(3,4) ~ 0,
                                             TRUE ~ NA_real_),
    affordable_mediation         = case_when(AJR_drm_5_d %in% c(1,2) ~ 1,
                                             AJR_drm_5_d %in% c(3,4) ~ 0,
                                             TRUE ~ NA_real_),
    affordable_lawyer            = case_when(AJR_drm_6_d %in% c(1,2) ~ 1,
                                             AJR_drm_6_d %in% c(3,4) ~ 0,
                                             TRUE ~ NA_real_),
    affordable_gov_department    = case_when(AJR_drm_7_d %in% c(1,2) ~ 1,
                                             AJR_drm_7_d %in% c(3,4) ~ 0,
                                             TRUE ~ NA_real_),
    affordable_community_leader  = case_when(AJR_drm_8_d %in% c(1,2) ~ 1,
                                             AJR_drm_8_d %in% c(3,4) ~ 0,
                                             TRUE ~ NA_real_),
    
    fair_court             = case_when(AJR_drm_1_f %in% c(1,2) ~ 1,
                                       AJR_drm_1_f %in% c(3,4) ~ 0,
                                       TRUE ~ NA_real_),
    fair_tribunal          = case_when(AJR_drm_2_f %in% c(1,2) ~ 1,
                                       AJR_drm_2_f %in% c(3,4) ~ 0,
                                       TRUE ~ NA_real_),
    fair_ombudsman         = case_when(AJR_drm_3_f %in% c(1,2) ~ 1,
                                       AJR_drm_3_f %in% c(3,4) ~ 0,
                                       TRUE ~ NA_real_),
    fair_police            = case_when(AJR_drm_4_f %in% c(1,2) ~ 1,
                                       AJR_drm_4_f %in% c(3,4) ~ 0,
                                       TRUE ~ NA_real_),
    fair_mediation         = case_when(AJR_drm_5_f %in% c(1,2) ~ 1,
                                       AJR_drm_5_f %in% c(3,4) ~ 0,
                                       TRUE ~ NA_real_),
    fair_lawyer            = case_when(AJR_drm_6_f %in% c(1,2) ~ 1,
                                       AJR_drm_6_f %in% c(3,4) ~ 0,
                                       TRUE ~ NA_real_),
    fair_gov_department    = case_when(AJR_drm_7_f %in% c(1,2) ~ 1,
                                       AJR_drm_7_f %in% c(3,4) ~ 0,
                                       TRUE ~ NA_real_),
    fair_community_leader  = case_when(AJR_drm_8_f %in% c(1,2) ~ 1,
                                       AJR_drm_8_f %in% c(3,4) ~ 0,
                                       TRUE ~ NA_real_)
  ) 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Data for plots                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## =========================================================
## Group bars
## =========================================================

# Helper para seleccionar solo algunos grupos por gráfico
select_groups <- function(cfg, ...) {
  keys <- c(...)
  cfg[intersect(keys, names(cfg))]
}

## =========================================================
## Helper general para correr cada bloque (compute → plot → save)
## =========================================================
# Config (única)
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
  "Income"                = c("> Ç30k a year",
                              "Ç30k–Ç70 a year",
                              "Ç70k–Ç120k a year",
                              "< Ç120k a year"),
  "Impact Level"          = c("High impact", "Low impact"),
  "Co-occurrent Problems" = c("1 problem","2-3 problems","4-5 problems","5 or more problems"),
  "Disability"            = c("With disability", "Without disability")
)

run_block <- function(data, 
                      value, 
                      group_cfg, 
                      levels_cfg, 
                      outfile, 
                      width = 300, height = 280) {
  val <- rlang::enexpr(value)
  df_sum <- summarize_by_vars(data = data, 
                              value = !!val, 
                              group_cfg = group_cfg)
  p <- plot_by_group(data_frame = df_sum, 
                     group_cfg = group_cfg, 
                     levels_cfg = levels_cfg)
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(plot = p, 
                  filename = outfile, 
                  width = width, 
                  height = height, 
                  units = "mm", 
                  scale = 0.75)
  return(p)
}

# Prevalence
grp_prev <- select_groups(full_group_cfg, 
                          "Overall",
                          "gender",
                          "age_group",
                          "edu_level",
                          "income",
                          "NUTS",
                          "disability")

plot1 <- run_block(data = data_subset.df, 
                   value = "prevalence", 
                   group_cfg = grp_prev, 
                   levels_cfg = levels_map, 
                   paste0(path2SP, 
                          "output/prevalence.svg"))

grp_selected <- select_groups(full_group_cfg,
                              "Overall",
                              "gender",
                              "age_group",
                              "edu_level",
                              "income",
                              "NUTS",
                              "level_impact",
                              "cooccurence_group",
                              "disability"
)

# Timeliness (< 1 year)

plot2 <- run_block(
  data_subset.df, 
  timeliness, 
  grp_selected, 
  levels_map, 
  paste0(path2SP, "output/timeliness.svg"), 
  width = 300, height = 350
  )

# Contacted DRM

plot3 <- run_block(
  data_subset.df, 
  contacted_drm, 
  grp_selected, 
  levels_map, 
  paste0(path2SP, "output/contacted_DRM.svg"),
  width = 300, height = 350
  )

# Access to DRM
plot4 <- run_block(
  data_subset.df, 
  access2drm, 
  grp_selected, 
  levels_map, 
  paste0(path2SP, "output/access2DRM.svg"),
  width = 300, height = 350
  )

# Access to info
plot5 <- run_block(
  data_subset.df, 
  access2info, 
  grp_selected, 
  levels_map, 
  paste0(path2SP, "output/access2info.svg"),
  width = 300, height = 350
  )

# Access to representation
plot6 <- run_block(
  data_subset.df, 
  access2rep, 
  grp_selected, 
  levels_map, 
  paste0(path2SP, "output/access2rep.svg"),
  width = 300, height = 350
  )

# Fairness
plot7 <- run_block(
  data_subset.df, 
  fair, 
  grp_selected, 
  levels_map, 
  paste0(path2SP, "output/fairness.svg"),
  width = 300, height = 350
  )

# Outcome
plot8 <- run_block(
  data_subset.df, 
  outcome_done, 
  grp_selected, 
  levels_map, 
  paste0(path2SP, "output/outcome_done.svg"),
  width = 300, height = 350
  )

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

