/*=================================================================================================================
Project:		OECD - LNS Ireland
Routine:		Renaming
Author(s):		Natalia Rodriguez 	(nrodriguez@worldjusticeproject.org)
Dependencies:  	World Justice Project
Creation Date:	October, 2025

Description:
Do-file that renames all questions.

=================================================================================================================*/

cls

/*=================================================================================================================
					PROBLEM SELECTION
=================================================================================================================*/

*--------  Renaming problems
tokenize A B C D E F G H I J K L M N
forvalues i=1/9 {
	rename q23_2_0`i'_* AJP_``i''*
}
tokenize A B C D E F G H I J K L M N
forvalues i=10/14 {
	rename q23_2_`i'_* AJP_``i''*
}

*-------- Frequencies

tokenize A B C D E F G H I J K L M N
forvalues i=1/9 {
	rename q23_3_0`i'_* AJP_``i''*_freq
}
tokenize A B C D E F G H I J K L M N
forvalues i=10/14 {
	rename q23_3_`i'_* AJP_``i''*_freq
}

*-------- Rankings (for multiple problems within the category)

tokenize A B C D E F G H I J K L M N
forvalues i=1/9 {
	rename q23_4_0`i'_* AJP_``i''*_rank
}
tokenize A B C D E F G H I J K L M N
forvalues i=10/14 {
	rename q23_4_`i'_* AJP_``i''*_rank
}

rename q23_2_catselected AJP_cat_selected
rename q23_4_recentissue_selected AJP_problem

/*=================================================================================================================
					STATUS
=================================================================================================================*/

*-------- Status of problem
rename q24 AJR_status

*-------- Status description for current problems
forvalues i=1/9 {
	rename q24_1_`i' AJR_status_cur_`i'
}

/*=================================================================================================================
					ASSISTANCE
=================================================================================================================*/

*-------- Advisers
forvalues i=1/9 {
	rename q25_1_0`i' AJD_adviser_`i'
}
forvalues i=10/17 {
	rename q25_1_`i' AJD_adviser_`i'
}

*-------- Advisers (binary)
forvalues i=1/9 {
	rename q25_1_0`i'_bin AJD_adviser_`i'_bin
}
forvalues i=10/17 {
	rename q25_1_`i'_bin AJD_adviser_`i'_bin
}

*-------- Advisers (help)
forvalues i=1/9 {
	rename q25_7_0`i' AJD_adviser_help_`i'
}
forvalues i=10/17 {
	rename q25_7_`i' AJD_adviser_help_`i'
}

*-------- Reasons for not seeking assistance
forvalues i=1/9 {
	rename q25_3_0`i' AJD_noadvice_reason_`i'
}
forvalues i=10/19 {
	rename q25_3_`i' AJD_noadvice_reason_`i'
}


/*=================================================================================================================
					NO ACTION TAKEN
=================================================================================================================*/

*-------- Taking action
rename q25_5 AJR_action

*-------- Reasons for not taking action
forvalues i=1/9 {
	rename q25_6_0`i' AJR_noaction_`i'
}
forvalues i=10/13 {
	rename q25_6_`i' AJR_noaction_`i'
}

/*=================================================================================================================
					RESOLUTION
=================================================================================================================*/

*-------- Mechanisms
forvalues i=1/9 {
	rename q26_0`i' AJR_drm_`i'
}
forvalues i=10/11 {
	rename q26_`i' AJR_drm_`i'
}

*-------- Binary mechanisms
forvalues i=1/9 {
	rename q26_0`i'_bin AJR_drm_`i'_bin
}
forvalues i=10/11 {
	rename q26_`i'_bin AJR_drm_`i'_bin
}

*-------- Reasons for not seeking help
forvalues i=1/9 {
	rename q27_0`i' AJR_noresol_reason_`i'
}
forvalues i=10/18 {
	rename q27_`i' AJR_noresol_reason_`i'
}

/*=================================================================================================================
					SATISFACTION
=================================================================================================================*/

*-------- Resolution
rename q28 AJR_settle_resol

*-------- Evaluation of each mechanism
forvalues i=1/9 {
	rename q29_1_it0`i'_* AJR_drm_`i'_*
}

rename q29_1_it11_* AJR_drm_11_*

*-------- Evaluation of each mechanism
forvalues i=1/9 {
	rename q29_2_it0`i'_* AJR_drm_res_`i'_*
}

rename q29_2_it11_* AJR_drm_res_11_*

rename q30 AJR_fair

rename q31_1 AJE_legalrights
rename q31_2 AJE_infosource
rename q31_3 AJE_advice
rename q31_4 AJE_fairoutcome

/*=================================================================================================================
					IMPACT
=================================================================================================================*/	

rename q32_1 AJE_impact


forvalues i=1/9 {
	rename q32_0`i' AJE_hardship_`i'
}
forvalues i=10/16 {
	rename q32_`i' AJE_hardship_`i'
}
	
	
/*=================================================================================================================
					DEMOGRAPHICS
=================================================================================================================*/	

rename q11 gend
rename q03 age_cat
rename q18 edu
rename education edu_2
rename q17 emp
rename employment emp_2
rename qsocialgrade work
rename socialgrade work_2
rename county county
rename region region
rename q23_1_1 tenant
rename q23_1_2 landlord
rename q23_1_3 home_owner

rename q15 disability
rename q14 ethni
rename q06 nation
rename q10 years_ireland
rename q05 marital
rename q19_1 A1_cat
rename q19_2 A1_children_cat
rename q19_3 children_home
rename q20 income
rename q03actual age
rename q10actual year_ireland

rename q19_1actual A1
rename q19_2actual A1_children
rename Q17_oth emp_other

rename Q24_1_oth AJR_status_cur_oth

rename Q24_2_month AJR_month_start
rename Q24_2_year AJR_year_start

rename Q24_3_month AJR_month_end
rename Q24_3_year AJR_year_end


rename Q25_3_oth AJD_noadvice_reason_oth
rename Q25_4 AJD_noadvice_reason_other_org

rename Q25_5_3 AJR_action_other
rename Q25_6_oth AJR_noaction_oth
rename Q27_oth AJR_noresol_reason_oth
rename Q28_oth AJR_settle_resol_other
rename Q34_1 AJR_org_1
rename Q34_2 AJR_org_2
rename Q34_3 AJR_org_3
rename Q14_4 ethni_white_other
rename Q14_7 ethni_black_other
rename Q14_11 ethni_asian_other
rename Q14_13 ethni_mixed_other
rename Q14_14 ethni_other
