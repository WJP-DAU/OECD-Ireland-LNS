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
## 2.  Loading data                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

master_data <- read_dta(
  file.path(path2SP,"data/ireland_lns_2025_final.dta")
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Data wrangling (later to re-factor)                                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


data_subset <- master_data %>%
  mutate(
    prevalence = had_dispute,
    
    access2info = case_when(
      had_dispute == 0   ~ NA_real_,
      AJE_infosource == 1 | AJE_infosource ==2 ~ 1,
      AJE_infosource == 3 | AJE_infosource ==4 ~ 0
    ), 
    
    access2rep = case_when(
      had_dispute == 0   ~ NA_real_,
      AJD_adviser_1 ==1 | AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | 
      AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1 |
      AJD_adviser_9 == 1 | AJD_adviser_10 == 1 | AJD_adviser_11 == 1 | AJD_adviser_12 ==1 |
      AJD_adviser_13 == 1 ~ 1,
      
      AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & 
      AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 &
      AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 | AJD_adviser_12 ==2 |
      AJD_adviser_13 == 2 ~ 0,
      
      AJD_noadvice_reason_1 == 1 | AJD_noadvice_reason_2 ==1 | AJD_noadvice_reason_3 ==1 | 
      AJD_noadvice_reason_4 == 1 | AJD_noadvice_reason_5 == 1 | AJD_noadvice_reason_6 == 1 |
      AJD_noadvice_reason_11 == 1 ~ 1,
      
      AJD_noadvice_reason_7 == 1 | AJD_noadvice_reason_8 == 1 | AJD_noadvice_reason_9 == 1 |
      AJD_noadvice_reason_10 == 1 | AJD_noadvice_reason_12 == 1 | AJD_noadvice_reason_13 == 1 | 
      AJD_noadvice_reason_14 == 1 | AJD_noadvice_reason_15 == 1 | AJD_noadvice_reason_16 == 1 | 
      AJD_noadvice_reason_17 == 1  ~ 0,
      AJD_noadvice_reason_18 == 1 | AJD_noadvice_reason_19 == 1 ~ NA_real_ 
    ),
    
    access2drm = case_when(
      had_dispute == 0   ~ NA_real_,
      AJR_drm_1_bin == 1 | AJR_drm_2_bin == 1 | AJR_drm_3_bin == 1 | AJR_drm_4_bin == 1 | 
      AJR_drm_5_bin == 1 | AJR_drm_6_bin == 1 | AJR_drm_7_bin == 1 | AJR_drm_8_bin == 1 | 
      AJR_drm_9_bin == 1 | AJR_drm_11_bin == 1 ~ 1 ,
      
      AJR_drm_1_bin == 0 & AJR_drm_2_bin == 0 & AJR_drm_3_bin == 0 & AJR_drm_4_bin == 0 & 
      AJR_drm_5_bin == 0 & AJR_drm_6_bin == 0 & AJR_drm_7_bin == 0 & AJR_drm_8_bin == 0 & 
      AJR_drm_9_bin == 0 & AJR_drm_11_bin == 0 ~ 0 ,
      
    ),
    
    #barriers_drm = case_when() ??????
    
    fair = case_when(
      had_dispute == 0   ~ NA_real_,
      AJR_fair == 1 ~ 1,
      AJR_fair == 2 ~ 0
      ),
    
    start_date = make_date(AJR_year_start, AJR_month_start, 1),
    end_date   = make_date(AJR_year_end, AJR_month_end, 1),
    months_diff = interval(start_date, end_date) %/% months(1),
    
    timeliness = case_when(
      had_dispute == 0   ~ NA_real_,
      AJR_status ==1 | AJR_status == 4 | AJR_status == 5 ~ NA_real_,
      months_diff<= 12 ~ 1,
      months_diff> 12 ~ 0,
    ),
    
    outcome_done = case_when(
      AJR_status == 1 ~ NA_real_,
      AJR_status == 2 ~ 0 ,
      AJR_status == 3 ~ 1 ,
      AJR_status == 4 ~ NA_real_,
      AJR_status == 5 ~ NA_real_
    )
  )



  
  
