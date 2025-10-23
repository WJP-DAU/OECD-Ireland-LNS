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
    
    #Problem category
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
    
    #Categories as unique vars
    problem_cat_land       = if_else(AJP_cat_selected== 1, 1 , 0),
    problem_cat_neighbors  = if_else(AJP_cat_selected== 2, 1 , 0),
    problem_cat_housing    = if_else(AJP_cat_selected== 3, 1 , 0),
    problem_cat_family     = if_else(AJP_cat_selected== 4, 1 , 0),
    problem_cat_injury     = if_else(AJP_cat_selected== 5, 1 , 0),
    problem_cat_citizen    = if_else(AJP_cat_selected== 6, 1 , 0),
    problem_cat_gov        = if_else(AJP_cat_selected== 7, 1 , 0),
    problem_cat_public     = if_else(AJP_cat_selected== 8, 1 , 0),
    problem_cat_products   = if_else(AJP_cat_selected== 9, 1 , 0),
    problem_cat_services   = if_else(AJP_cat_selected== 10, 1 , 0),
    problem_cat_money      = if_else(AJP_cat_selected== 11, 1 , 0),
    problem_cat_employment = if_else(AJP_cat_selected== 12, 1 , 0),

    #Co-occurence
    cooccurence_group = case_when(
      ndisputes == 0 ~ NA_character_,
      ndisputes == 1 ~ "1 problem",
      ndisputes <= 3 ~ "2-3 problems",
      ndisputes <= 5 ~ "4-5 problems",
      ndisputes >= 6 ~ "5 or more problems"
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
    approp_adv = case_when(
      had_dispute == 0   ~ NA_real_,
      AJD_adviser_1 ==1 | AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
        AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1 |
        AJD_adviser_9 == 1 | AJD_adviser_10 == 1 | AJD_adviser_11 == 1 | AJD_adviser_12 ==1 |
        AJD_adviser_13 == 1 ~ 1,
      AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
        AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
        AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 | AJD_adviser_12 ==2 |
        AJD_adviser_13 == 2 ~ 0
    ),
    
    #Access to appropriate/professional assistance and representation
    access2rep = case_when(
      had_dispute == 0   ~ NA_real_,
      AJD_adviser_1 ==1 | AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
      AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1 |
      AJD_adviser_9 == 1 | AJD_adviser_10 == 1 | AJD_adviser_11 == 1 | AJD_adviser_12 ==1 |
      AJD_adviser_13 == 1 ~ 1,
    
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
      AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
      AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 | AJD_adviser_12 ==2 |
      AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 &
      AJD_adviser_17 == 2) 
      & 
      (AJR_noaction_1 == 1 | AJR_noaction_2 ==1 | AJR_noaction_6 ==1 | AJR_noaction_7 ==1 | 
      AJR_noaction_9 ==1 | AJR_noaction_10 ==1 | AJR_noaction_12 ==1) ~ 1,
      
      (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
      AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
      AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 | AJD_adviser_12 ==2 |
      AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 &
      AJD_adviser_17 == 2) 
      &
      (AJR_noaction_3 ==1 | AJR_noaction_4 ==1 | AJR_noaction_5 ==1 | AJR_noaction_8 ==1 | 
      AJR_noaction_11 ==1 | AJR_noaction_13 ==1) ~ 0
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
    
    #Non-seekers’ intention (People that did not access ANY advisor)
    
    #Contacted a DRM
    contacted_drm = case_when(
      AJR_drm_1_bin == 1 | AJR_drm_2_bin == 1 | AJR_drm_3_bin == 1 | AJR_drm_4_bin == 1 | 
      AJR_drm_5_bin == 1 | AJR_drm_6_bin == 1 | AJR_drm_7_bin == 1 | AJR_drm_8_bin == 1 | 
      AJR_drm_9_bin == 1 | AJR_drm_11_bin == 1 ~ 1,
      
      AJR_drm_1_bin == 0 & AJR_drm_2_bin == 0 & AJR_drm_3_bin == 0 & AJR_drm_4_bin == 1 & 
        AJR_drm_5_bin == 0 & AJR_drm_6_bin == 0 & AJR_drm_7_bin == 0 & AJR_drm_8_bin == 0 & 
        AJR_drm_9_bin == 0 & AJR_drm_11_bin == 0 ~ 0
    ),
    
    #Barriers of access to a DRM
    
    
    #Access to a dispute resolution mechanism
    access2drm = case_when(
      had_dispute == 0   ~ NA_real_,
      AJR_drm_1_bin == 1 | AJR_drm_2_bin == 1 | AJR_drm_3_bin == 1 | AJR_drm_4_bin == 1 | 
      AJR_drm_5_bin == 1 | AJR_drm_6_bin == 1 | AJR_drm_7_bin == 1 | AJR_drm_8_bin == 1 | 
      AJR_drm_9_bin == 1 | AJR_drm_11_bin == 1 ~ 1 ,
      
      (AJR_drm_1_bin == 0 & AJR_drm_2_bin == 0 & AJR_drm_3_bin == 0 & AJR_drm_4_bin == 0 & 
      AJR_drm_5_bin == 0 & AJR_drm_6_bin == 0 & AJR_drm_7_bin == 0 & AJR_drm_8_bin == 0 & 
      AJR_drm_9_bin == 0 & AJR_drm_11_bin == 0 ) 
      &
      (AJR_noresol_reason_7 == 1 | AJR_noresol_reason_8 ==1 | AJR_noresol_reason_9 == 1  | 
      AJR_noresol_reason_10 == 1 | AJR_noresol_reason_11 ==1 | AJR_noresol_reason_12 ==1 | 
      AJR_noresol_reason_13 ==1 | AJR_noresol_reason_14 == 1 | AJR_noresol_reason_15 == 1 ) ~ 0
    ),
    
    #Reasons for not accessing a DRM
    
    
    
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
    
    # Fairness by each mechanism 
    
    # Efficiency by each mechanism 
    
    # Affordability by each mechanism 
    
    # Duration by each mechanism 
    
    # Fairness by each mechanism 
    
    # Helpfulness by each mechanism 
    
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
    
    #Type of impact    
    
    #Legal capability
    legal_rights = case_when(
      (AJE_legalrights_done == 1 | AJE_legalrights_done == 2 | AJE_legalrights_ongoing == 1 | 
         AJE_legalrights_ongoing == 2 ) ~ 1,
      (AJE_legalrights_done == 3 | AJE_legalrights_done == 4 | AJE_legalrights_ongoing == 3 | 
          AJE_legalrights_ongoing == 4 ) ~ 0
    ),
    
    expert_help = case_when(
      (AJE_advice_done == 1 | AJE_advice_done == 2 | AJE_advice_ongoing == 1 | AJE_advice_ongoing == 2) ~ 1,
      (AJE_advice_done == 3 | AJE_advice_done == 4 | AJE_advice_ongoing == 3 | AJE_advice_ongoing == 4) ~ 0
    )
  )

    
### Demographics
    
demographics <- master_data %>%
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
    (disability == 1 | disability == 2) ~ 1,
     disability == 3 ~ 0
  ),
  
  income = case_when(
    income == 1 ~ "Less than Ç30,000 a year",
    income == 2 ~ "Between Ç30,000 and Ç70,000 a year",
    income == 3 ~ "Between Ç70,000 and Ç120,000 a year",
    income == 4 ~ "More than Ç120,000 a year"
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
  )
  #marital_status = factor(marital_status, levels = c("Married", "Not married")),
)


View(data_subset[, c("adviser_help_1_bin", "AJD_adviser_help_1")])


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 4.  Data for plots                                                                                       ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1. Prevalence of justice problems

plot1 <- data_subset %>%
  summarize(
    prevalence=mean(prevalence,na.rm=TRUE)
  )

# 2. Prevalence of Justice Problems in Ireland, by Problem Category 

plot2 <- data_subset %>%
  summarise(
    total_sample = n(),
    problem_cat_land        = sum(problem_cat_land, na.rm = TRUE)/total_sample,
    problem_cat_neighbors   = sum(problem_cat_neighbors, na.rm = TRUE)/total_sample,
    problem_cat_housing     = sum(problem_cat_housing, na.rm = TRUE)/total_sample,
    problem_cat_family      = sum(problem_cat_family, na.rm = TRUE)/total_sample,
    problem_cat_injury      = sum(problem_cat_injury, na.rm = TRUE)/total_sample,
    problem_cat_citizen     = sum(problem_cat_citizen, na.rm = TRUE)/total_sample,
    problem_cat_gov         = sum(problem_cat_gov, na.rm = TRUE)/total_sample,
    problem_cat_public      = sum(problem_cat_public, na.rm = TRUE)/total_sample,
    problem_cat_products    = sum(problem_cat_products, na.rm = TRUE)/total_sample,
    problem_cat_services    = sum(problem_cat_services, na.rm = TRUE)/total_sample,
    problem_cat_money       = sum(problem_cat_money, na.rm = TRUE)/total_sample,
    problem_cat_employment  = sum(problem_cat_employment, na.rm = TRUE)/total_sample,
  )

# 3 .Co-occurrence of Non-Trivial Justice Problems in Ireland 

plot3 <- data_subset %>%
  summarise(
    coocurrence=mean(ndisputes,na.rm=TRUE)
  )
  

# 4. Dispute Resolution Processes that Finalized in Less than One Year, in Ireland 

# 5. Pathways to Accessing Dispute Resolution Mechanisms (DRM) in Ireland 
  
