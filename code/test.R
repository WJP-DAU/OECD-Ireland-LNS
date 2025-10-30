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

#source("code/settings.R")
#source("code/functions.R")
#source("code/bars_group.R")

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
      , 1 , 0),
    problem_cat_neighbors = if_else(
      (AJP_B1 == 1 | AJP_B2 == 1 | AJP_B3 == 1)
      , 1 , 0),
    problem_cat_housing = if_else(
      (AJP_C1 == 1 | AJP_C2 == 1 | AJP_C3 == 1 | AJP_D1 == 1 | AJP_D2 == 1 | AJP_D3 == 1 | 
         AJP_E1 == 1 | AJP_E2 == 1 | AJP_E3 == 1 | AJP_E4 == 1 | AJP_E5 == 1 | AJP_E6 == 1 | 
         AJP_E7 == 1 )
      , 1 , 0),
    problem_cat_family = if_else(
      (AJP_F1 == 1 | AJP_F2 == 1 | AJP_F3 == 1 | AJP_F4 == 1 | AJP_F5 == 1 | AJP_F6 == 1)
      , 1 , 0),
    problem_cat_injury = if_else(
      (AJP_H1 == 1 | AJP_H2 == 1 | AJP_H3 == 1 | AJP_H4 == 1 | AJP_H5 == 1)
      , 1 , 0),
    problem_cat_citizen = if_else(
      (AJP_G1 == 1 | AJP_G2 == 1 | AJP_G3 == 1 | AJP_G4 == 1 | AJP_G5 == 1)
      , 1 , 0),
    problem_cat_gov = if_else(
      (AJP_I1 == 1 | AJP_I2 == 1 | AJP_I3 == 1 | AJP_I4 == 1 | AJP_I5 == 1 | AJP_I6 == 1)
      , 1 , 0),
    problem_cat_public = if_else(
      (AJP_J1 == 1 | AJP_J2 == 1 | AJP_J3 == 1 | AJP_J4 == 1)
      , 1 , 0),
    problem_cat_products = if_else(
      (AJP_K1 == 1 | AJP_K2 == 1)
      , 1 , 0),
    problem_cat_services = if_else(
      (AJP_L1 == 1 | AJP_L2 == 1)
      , 1 , 0),
    problem_cat_money = if_else(
      (AJP_M1 == 1 | AJP_M2 == 1 | AJP_M3 == 1 | AJP_M4 == 1 | AJP_M5 == 1)
      , 1 , 0),
    problem_cat_employment = if_else(
      (AJP_N1 == 1 | AJP_N2 == 1 | AJP_N3 == 1 | AJP_N4 == 1)
      , 1 , 0),
    
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
    
    #Access to appropriate/professional assistance and representation
    access2rep = case_when(
      had_dispute == 0   ~ NA_real_,
      
      #YES to any Professional adviser
      AJD_adviser_1 ==1 | AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
        AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1 |
        AJD_adviser_9 == 1 | AJD_adviser_10 == 1 | AJD_adviser_11 == 1 | AJD_adviser_12 ==1 |
        AJD_adviser_13 == 1 ~ 1,
      
      #No advisers (ALL) - reasons: opting out 
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 &
         AJD_adviser_17 == 2) 
      & 
        (AJR_noaction_1 == 1 | AJR_noaction_2 ==1 | AJR_noaction_6 ==1 | AJR_noaction_10 ==1 
         | AJR_noaction_12 ==1) ~ 1,
      
      #No prof advisers - reasons: opting out
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2) 
      &
        (AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1 )
      &
        (AJD_noadvice_reason_1 ==1 | AJD_noadvice_reason_2==1 | AJD_noadvice_reason_3==1 | 
           AJD_noadvice_reason_4==1 | AJD_noadvice_reason_5==1 | AJD_noadvice_reason_6==1 ) ~ 1,
      
      # No action (No, as it has resolved itself or is no longer an issue) as opting out
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 &
         AJD_adviser_17 == 2) 
      & 
        AJR_action == 4 ~ 1,
      
      # Needed access and did not get it - ALL advisers (reasons for no action = barriers)
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 &
         AJD_adviser_17 == 2)
      &
        (AJR_noaction_3 ==1 | AJR_noaction_4 ==1 | AJR_noaction_5 ==1 | AJR_noaction_7 ==1 | 
           AJR_noaction_8 ==1 | AJR_noaction_9 ==1 | AJR_noaction_11 ==1 | AJR_noaction_13 ==1) ~ 0,
      
      #Needed access and did not get it - prof advisers (reasons for not going to a prof adviser = barriers)
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
           AJD_noadvice_reason_16 == 1 ) ~ 0,
      
      #Unknown reasons - prof advisers (reasons for no contact a prof adviser = other reason)
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & 
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & 
         AJD_adviser_13 == 2) 
      &
        (AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1 ) 
      &
        AJD_noadvice_reason_17 == 1 ~ 0, 
      
      #Unknown reasons (AJR_action == 3, "Have done something")
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
         AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
         AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 &
         AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 &
         AJD_adviser_17 == 2) 
      & 
        AJR_action == 3 ~ 1
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
    
    #Unknown reason 
    unknown = case_when(
      AJR_action ==3 ~ 1,
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
    
    #Unknown prof help
    unknown_prof = case_when(
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
    had_dispute == 1 ~ 0
    )

  )

table(data_subset$access2rep)
table(data_subset$contact_adviser)
table(data_subset$appropriate_adviser)
table(data_subset$no_appropriate_adviser)

table(data_subset$no_need_assistance_prof)
table(data_subset$needed_assistance_prof)
table(data_subset$unknown_prof)

table(data_subset$no_need_assistance)
table(data_subset$needed_assistance)
table(data_subset$unknown)


