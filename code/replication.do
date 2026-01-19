/*=================================================================================================================
Project:		OECD - LNS Ireland
Routine:		Replication Report
Author(s):		Natalia Rodriguez 	(nrodriguez@worldjusticeproject.org)
Dependencies:  	World Justice Project
Creation Date:	October, 2025

Description:
Master dofile for the replication of the OECD-LNS for Ireland.

=================================================================================================================*/

clear all
cls

/*=================================================================================================================
					Pre-settings
=================================================================================================================*/

*----- Required packages:
* NONE

*----- Defining paths to SharePoint & your local Git Repo copy:

*-------- (a) Natalia Rodriguez:
if (inlist("`c(username)'", "nrodriguez")) {
	global path2SP "C:\Users\nrodriguez\OneDrive - World Justice Project\Programmatic\Data Analytics\6. Country Reports\OECD-Ireland-LNS"
}

*-------- (b) Any other user: PLEASE INPUT YOUR PERSONAL PATH TO THE SHAREPOINT DIRECTORY:
else {
	global path2SP ""
}

*-------- Defining path to Data and DoFiles:
*global path2data "${path2SP}/Merged Files/Merged Final Files"
global path2dos  "${path2SP}/code"

	
/*=================================================================================================================
					1. Data Import
=================================================================================================================*/

*----- Opening CLEAN dataset

use "${path2SP}\data\ireland_lns_2025_final.dta", clear


/*=====================================================================================================================================
					2. Recoding variables
=====================================================================================================================================*/

*----- Types of problems by category (ALL problems mentioned)

	* 1. Land *
	gen land=0
	foreach x in AJP_A1 {
		replace land=1 if `x'==1
		}

	* 2. Neighbours *
	gen neighbors=0
	foreach x in AJP_B1 AJP_B2 AJP_B3 {
		replace neighbors=1 if `x'==1
		}

	* 3. Housing * 
	gen housing=0
	foreach x in AJP_C1 AJP_C2 AJP_C3 AJP_D1 AJP_D2 AJP_D3 AJP_E1 AJP_E2 AJP_E3 AJP_E4 AJP_E5 AJP_E6 AJP_E7 {
		replace housing=1 if `x'==1
		}

	*4. Family/relationship *
	gen family=0
	foreach x in AJP_F1 AJP_F2 AJP_F3 AJP_F4 AJP_F5 AJP_F6 {
		replace family=1 if `x'==1
	}

	* 5. Injury
	gen injury=0
	foreach x in AJP_H1 AJP_H2 AJP_H3 AJP_H4 AJP_H5 {
		replace injury=1 if `x'==1
	}

	* 6. Citizenship or migration
	gen citizenship=0
	foreach x in AJP_G1 AJP_G2 AJP_G3 AJP_G4 AJP_G5 {
		replace citizenship=1 if `x'==1
	}

	* 7. Government benefits and payments 
	gen gov=0
	foreach x in AJP_I1 AJP_I2 AJP_I3 AJP_I4 AJP_I5 AJP_I6 {
		replace gov=1 if `x'==1
	}

	*8.	Public services
	gen public_services=0
	foreach x in AJP_J1 AJP_J2 AJP_J3 AJP_J4 {
		replace public_services=1 if `x'==1
	}

	*9.	Products
	gen products=0
	foreach x in AJP_K1 AJP_K2 { 
		replace products=1 if `x'==1
	}

	*10.	Services
	gen services=0
	foreach x in AJP_L1 AJP_L2 {
		replace services=1 if `x'==1
	}

	*11.	Money/debt
	gen MoneyDebt=0
	foreach x in AJP_M1 AJP_M2 AJP_M3 AJP_M4 AJP_M5 {
		replace MoneyDebt=1 if `x'==1
	}

	*12. Employment
	gen employment=0
	foreach x in AJP_N1 AJP_N2 AJP_N3 AJP_N4 {
		replace employment=1 if `x'==1
	}

	
*----- Co-occuring problems
gen cooccurence_group = .
replace cooccurence_group = 1 if ndisputes == 1 
replace cooccurence_group = 2 if ndisputes >1 & ndisputes <= 3 
replace cooccurence_group = 3 if ndisputes >3 & ndisputes <= 5 
replace cooccurence_group = 4 if ndisputes >= 6 


*----- Access to appropriate information and advice 	
gen access2info = .
replace access2info = 0 if AJE_infosource_done == 3 | AJE_infosource_done ==4 | AJE_infosource_ongoing == 3 | AJE_infosource_ongoing ==4
replace access2info = 1 if AJE_infosource_done == 1 | AJE_infosource_done ==2 | AJE_infosource_ongoing == 1 | AJE_infosource_ongoing ==2
replace access2info=. if had_dispute==0	  


*----- Contacted an advisor (ALL ADVISERS)
egen contact_adviser_n = rowtotal(AJD_adviser_1_bin AJD_adviser_2_bin AJD_adviser_3_bin AJD_adviser_4_bin AJD_adviser_5_bin AJD_adviser_6_bin AJD_adviser_7_bin AJD_adviser_8_bin AJD_adviser_9_bin AJD_adviser_10_bin AJD_adviser_11_bin AJD_adviser_12_bin AJD_adviser_13_bin AJD_adviser_14_bin AJD_adviser_15_bin AJD_adviser_16_bin AJD_adviser_17_bin)

gen contact_adviser = contact_adviser_n>0
replace contact_adviser = . if had_dispute==0

gen no_contact_adviser = .
replace no_contact_adviser = 1 if contact_adviser==0
replace no_contact_adviser = 0 if contact_adviser==1


*----- Access to appropriate assistance and representation (indicator)
gen access2rep = .

//** ACCESS

*Went to a professional adviser
replace access2rep = 1 if AJD_adviser_1 ==1 | AJD_adviser_2 == 1 | AJD_adviser_3 == 1 | AJD_adviser_4 == 1 | AJD_adviser_5 == 1 | AJD_adviser_6 == 1 | AJD_adviser_7 == 1 | AJD_adviser_8 == 1 | AJD_adviser_9 == 1 | AJD_adviser_10 == 1 | AJD_adviser_11 == 1 | AJD_adviser_12 ==1 | AJD_adviser_13 == 1

*Did not need the access - ALL advisers (reasons for no action = opting out)
replace access2rep = 1 if ///
(AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & AJD_adviser_17 == 2) & ///
(AJR_noaction_1 == 1 | AJR_noaction_2 ==1 | AJR_noaction_6 ==1 | AJR_noaction_10 ==1 | AJR_noaction_12 ==1) & ///
(AJR_noaction_3 ==0 & AJR_noaction_4 ==0 &  AJR_noaction_5 ==0 &  AJR_noaction_7 ==0 & AJR_noaction_8 ==0 &  AJR_noaction_9 ==0 &  AJR_noaction_11 ==0 & AJR_noaction_13 ==0 )

*Did not need the access - non-appropriate advisers (reasons for not going to a prof adviser = opting out)
replace access2rep = 1 if (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2) & ///
(AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1 ) & ///
(AJD_noadvice_reason_1 ==1 | AJD_noadvice_reason_2==1 | AJD_noadvice_reason_3==1 | AJD_noadvice_reason_4==1 | AJD_noadvice_reason_5==1 | AJD_noadvice_reason_6==1 ) & ///
(AJD_noadvice_reason_7== 0 & AJD_noadvice_reason_8== 0 & AJD_noadvice_reason_9== 0 & AJD_noadvice_reason_10== 0 & AJD_noadvice_reason_11== 0 & AJD_noadvice_reason_12== 0 & AJD_noadvice_reason_13== 0 & AJD_noadvice_reason_14== 0 &  AJD_noadvice_reason_15== 0 & AJD_noadvice_reason_16 == 0)

*Categorization of AJR_action == 4 (No, as it has resolved itself or is no longer an issue) as "Opting out"
replace access2rep = 1 if (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & AJD_adviser_17 == 2) & AJR_action == 4


//** NO ACCESS

*Unknown reasons (Did not go to a professional adviser, but went to other advisers. In the reasons for no contact a prof adviser: Other reason )
replace access2rep = 0 if (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2) & ///
(AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1 ) & ///
(AJD_noadvice_reason_1 ==0 & AJD_noadvice_reason_2==0 &  AJD_noadvice_reason_3==0 &  AJD_noadvice_reason_4==0 &  AJD_noadvice_reason_5==0 &  AJD_noadvice_reason_6==0 & AJD_noadvice_reason_7== 0 & AJD_noadvice_reason_8== 0 & AJD_noadvice_reason_9== 0 & AJD_noadvice_reason_10== 0 & AJD_noadvice_reason_11== 0 & AJD_noadvice_reason_12== 0 & AJD_noadvice_reason_13== 0 & AJD_noadvice_reason_14== 0 &  AJD_noadvice_reason_15== 0 & AJD_noadvice_reason_16 == 0 ) & ///
AJD_noadvice_reason_17 == 1

*Unknown reasons (AJR_action == 3, "Have done something")
replace access2rep = 0 if (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & AJD_adviser_17 == 2) & AJR_action == 3

*Needed access and did not get it - ALL advisers (reasons for no action = barriers)
replace access2rep = 0 if ///
(AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & AJD_adviser_17 == 2) & ///
(AJR_noaction_3 ==1 | AJR_noaction_4 ==1 | AJR_noaction_5 ==1 | AJR_noaction_7 ==1 | AJR_noaction_8 ==1 | AJR_noaction_9 ==1 | AJR_noaction_11 ==1 | AJR_noaction_13 ==1)

*Needed access and did not get it - professional advisers (reasons for not going to a professional adviser and needing access)
replace access2rep = 0 if (AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1 ) & ///
(AJD_noadvice_reason_7== 1 | AJD_noadvice_reason_8== 1 | AJD_noadvice_reason_9== 1 | AJD_noadvice_reason_10== 1 | AJD_noadvice_reason_11== 1 | AJD_noadvice_reason_12== 1 | AJD_noadvice_reason_13== 1 | AJD_noadvice_reason_14== 1 |  AJD_noadvice_reason_15== 1 | AJD_noadvice_reason_16 == 1 )


*Make missing people with no disputes
replace access2rep = . if had_dispute==0


*----- Professional adviser
egen appropriate_adviser_n = rowtotal(AJD_adviser_1_bin AJD_adviser_2_bin AJD_adviser_3_bin AJD_adviser_4_bin AJD_adviser_5_bin AJD_adviser_6_bin AJD_adviser_7_bin AJD_adviser_8_bin AJD_adviser_9_bin AJD_adviser_10_bin AJD_adviser_11_bin AJD_adviser_12_bin AJD_adviser_13_bin)

gen appropriate_adviser = appropriate_adviser_n >0
*replace appropriate_adviser = . if contact_adviser ==0
replace appropriate_adviser = . if had_dispute==0


*----- Non-professional advisor
gen no_appropriate_adviser = .
replace no_appropriate_adviser = 0 if had_dispute==1
replace no_appropriate_adviser = 1 if (AJD_adviser_14==1 | AJD_adviser_15==1 | AJD_adviser_16==1 | AJD_adviser_17==1) & appropriate_adviser==0


*----- NON-Professional advisers breakdown

*Did not need assistance : NO to ALL prof advisers (YES to a non-professional), opted out from going to a prof adviser
gen no_need_assistance_prof = .
replace no_need_assistance_prof = 0 if had_dispute == 1

replace no_need_assistance_prof = 1 if (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2) & ///
(AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1 ) & ///
(AJD_noadvice_reason_1 ==1 | AJD_noadvice_reason_2==1 | AJD_noadvice_reason_3==1 | AJD_noadvice_reason_4==1 | AJD_noadvice_reason_5==1 | AJD_noadvice_reason_6==1 ) & ///
(AJD_noadvice_reason_7== 0 & AJD_noadvice_reason_8== 0 & AJD_noadvice_reason_9== 0 & AJD_noadvice_reason_10== 0 & AJD_noadvice_reason_11== 0 & AJD_noadvice_reason_12== 0 & AJD_noadvice_reason_13== 0 & AJD_noadvice_reason_14== 0 &  AJD_noadvice_reason_15== 0 & AJD_noadvice_reason_16 == 0)

*Needed access and did not go to a professional adviser: NO to ALL prof advisers (YES to a non-professional), reasons = barriers
gen needed_assistance_prof = . 
replace needed_assistance_prof = 0 if had_dispute == 1

replace needed_assistance_prof = 1 if (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2) & ///
(AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1 ) & ///
(AJD_noadvice_reason_7== 1 | AJD_noadvice_reason_8== 1 | AJD_noadvice_reason_9== 1 | AJD_noadvice_reason_10== 1 | AJD_noadvice_reason_11== 1 | AJD_noadvice_reason_12== 1 | AJD_noadvice_reason_13== 1 | AJD_noadvice_reason_14== 1 |  AJD_noadvice_reason_15== 1 | AJD_noadvice_reason_16 == 1)

*Unknown reason for prof assistance (but yes to other non-prof assistance)
gen unknown_prof = . 
replace unknown_prof = 0 if had_dispute == 1

replace unknown_prof = 1 if (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2) & ///
(AJD_adviser_14 == 1 | AJD_adviser_15 == 1 | AJD_adviser_16 == 1 | AJD_adviser_17 == 1 ) & ///
(AJD_noadvice_reason_1 ==0 & AJD_noadvice_reason_2==0 &  AJD_noadvice_reason_3==0 &  AJD_noadvice_reason_4==0 &  AJD_noadvice_reason_5==0 &  AJD_noadvice_reason_6==0 & AJD_noadvice_reason_7== 0 & AJD_noadvice_reason_8== 0 & AJD_noadvice_reason_9== 0 & AJD_noadvice_reason_10== 0 & AJD_noadvice_reason_11== 0 & AJD_noadvice_reason_12== 0 & AJD_noadvice_reason_13== 0 & AJD_noadvice_reason_14== 0 &  AJD_noadvice_reason_15== 0 & AJD_noadvice_reason_16 == 0 ) & ///
AJD_noadvice_reason_17 == 1


*----- NO to CONTACTED an adviser breakdown

**Did not need assistance: NO to ALL advisers (reasons for no action = opting out)
gen no_need_assistance =.
replace no_need_assistance = 0 if had_dispute == 1

replace no_need_assistance = 1 if (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & AJD_adviser_17 == 2) & ///
(AJR_noaction_1 == 1 | AJR_noaction_2 ==1 | AJR_noaction_6 ==1 | AJR_noaction_10 ==1 | AJR_noaction_12 ==1) & ///
(AJR_noaction_3 ==0 & AJR_noaction_4 ==0 &  AJR_noaction_5 ==0 &  AJR_noaction_7 ==0 & AJR_noaction_8 ==0 &  AJR_noaction_9 ==0 &  AJR_noaction_11 ==0 & AJR_noaction_13 ==0)

replace no_need_assistance = 1 if (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & AJD_adviser_17 == 2) & AJR_action ==4 

**Needed access and did not get it : NO to ALL advisers (reasons for no action = barriers)
gen needed_assistance = .
replace needed_assistance = 0 if had_dispute == 1

replace needed_assistance = 1 if (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & AJD_adviser_17 == 2) & ///
(AJR_noaction_3 ==1 | AJR_noaction_4 ==1 | AJR_noaction_5 ==1 | AJR_noaction_7 ==1 | AJR_noaction_8 ==1 | AJR_noaction_9 ==1 | AJR_noaction_11 ==1 | AJR_noaction_13 ==1 )

**Unknown reasons for no contact
gen unknown = . 
replace unknown = 0 if had_dispute == 1

*NO to ALL advisers AND AJR_action = No, as it has resolved itself or is no longer an issue)
replace unknown = 1 if (AJD_adviser_1 ==2 & AJD_adviser_2 == 2 & AJD_adviser_3 == 2 & AJD_adviser_4 == 2 & AJD_adviser_5 == 2 & AJD_adviser_6 == 2 & AJD_adviser_7 == 2 & AJD_adviser_8 == 2 & AJD_adviser_9 == 2 & AJD_adviser_10 == 2 & AJD_adviser_11 == 2 & AJD_adviser_12 ==2 & AJD_adviser_13 == 2 & AJD_adviser_14 == 2 & AJD_adviser_15 == 2 & AJD_adviser_16 == 2 & AJD_adviser_17 == 2) & AJR_action ==3


*----- Helpfulness by adviser
forvalues i=1/17{
	gen adviser_help_`i'=AJD_adviser_help_`i'
	recode adviser_help_`i' (1 2 = 0) (3 4 = 1) (5 6 = .)
}

*----- Barriers to accessing professional help (Reasons)
/*
gen reason_no_need = 0 if appropriate_adviser==0
replace reason_no_need =1 if AJD_noadvice_reason_1 ==1 | AJD_noadvice_reason_2 ==1 | AJD_noadvice_reason_3 ==1 | AJD_noadvice_reason_5 ==1 | AJD_noadvice_reason_6 ==1

gen reason_had_help = 0 if appropriate_adviser==0
replace reason_had_help =1 if AJD_noadvice_reason_4 ==1 | AJD_noadvice_reason_7 ==1 | AJD_noadvice_reason_8 ==1 | AJD_noadvice_reason_9 ==1

gen reason_info_bar = 0 if appropriate_adviser==0
replace reason_info_bar = 1 if AJD_noadvice_reason_13 ==1 

gen reason_relation_bar = 0 if appropriate_adviser==0 
replace reason_relation_bar =1 if AJD_noadvice_reason_11 ==1 | AJD_noadvice_reason_12 ==1

gen reason_psycho_bar = 0 if appropriate_adviser==0
replace reason_psycho_bar =1 if AJD_noadvice_reason_10==1 | AJD_noadvice_reason_14 ==1

gen reason_prior = 0 if appropriate_adviser==0
replace reason_prior = 1 if AJD_noadvice_reason_15==1

gen reason_social = 0 if appropriate_adviser==0
replace reason_social = 1 if AJD_noadvice_reason_16==1

gen reason_other = 0 if appropriate_adviser==0
replace reason_other = 1 if AJD_noadvice_reason_17==1
*/

*----- Non-seekers' actions/intentions
tab AJR_action, g(AJR_action_)


*----- Reasons for not seeking help (NO to ALL advisers)
*These are variables AJR_noaction_1-AJR_noaction_13. No need to do extra calculations 
	

*----- Contacted a DRM

egen contacted_drm_n = rowtotal(AJR_drm_1_bin AJR_drm_2_bin AJR_drm_3_bin AJR_drm_4_bin AJR_drm_5_bin AJR_drm_6_bin AJR_drm_7_bin AJR_drm_8_bin AJR_drm_9_bin AJR_drm_11_bin)

gen contacted_drm = contacted_drm_n>0
replace contacted_drm = . if had_dispute==0

	
*----- Access to a DRM
gen access2drm =.
replace access2drm = 0 if contacted_drm==0 & ///
(AJR_noresol_reason_7 == 1 | AJR_noresol_reason_8 ==1 | AJR_noresol_reason_9 == 1  | AJR_noresol_reason_10 == 1 | AJR_noresol_reason_11 ==1 | AJR_noresol_reason_12 ==1 | AJR_noresol_reason_13 ==1 | AJR_noresol_reason_14 == 1 | AJR_noresol_reason_15 == 1 )

replace access2drm = 1 if contacted_drm==1

*----- Needed but didn't have access to a DRM
gen needed_drm = . 
replace needed_drm = 0 if contacted_drm==0
replace needed_drm = 1 if (AJR_noresol_reason_7 == 1 | AJR_noresol_reason_8 ==1 | AJR_noresol_reason_9 == 1  | AJR_noresol_reason_10 == 1 | AJR_noresol_reason_11 ==1 | AJR_noresol_reason_12 ==1 | AJR_noresol_reason_13 ==1 | AJR_noresol_reason_14 == 1 | AJR_noresol_reason_15 == 1 ) 


*----- Did not need to turn to a DRM
gen no_need_drm = .
replace no_need_drm = 0 if contacted_drm==0
replace no_need_drm = 1 if (AJR_noresol_reason_1 == 1 | AJR_noresol_reason_2== 1 | AJR_noresol_reason_3== 1 | AJR_noresol_reason_4== 1 | AJR_noresol_reason_5== 1 | AJR_noresol_reason_6== 1) & ///
(AJR_noresol_reason_7 == 0  & AJR_noresol_reason_8 == 0 & AJR_noresol_reason_9 == 0  & AJR_noresol_reason_10 == 0 & AJR_noresol_reason_11 ==0 & AJR_noresol_reason_12 ==0  & AJR_noresol_reason_13 ==0 & AJR_noresol_reason_14 == 0 & AJR_noresol_reason_15 == 0 )


*----- Unknown reason DRMs
gen unknown_drm = .
replace unknown_drm = 0 if contacted_drm==0
replace unknown_drm = 1 if AJR_noresol_reason_16 ==1 & (AJR_noresol_reason_7 == 0  & AJR_noresol_reason_8 == 0 & AJR_noresol_reason_9 == 0  & AJR_noresol_reason_10 == 0 & AJR_noresol_reason_11 ==0 & AJR_noresol_reason_12 ==0  & AJR_noresol_reason_13 ==0 & AJR_noresol_reason_14 == 0 & AJR_noresol_reason_15 == 0 )

	
*----- Reasons for not accessing DRMs	
*These are AJR_noresol_reason_1- AJR_noresol_reason_16. No further calculations needed


*----- Fixing the denominator for the contacted mechanisms
forvalues i=1/11 {
	gen drm_`i' = .
	replace drm_`i' = 0 if contacted_drm==0 & (AJR_noresol_reason_7 == 1 | AJR_noresol_reason_8 ==1 | AJR_noresol_reason_9 == 1  | AJR_noresol_reason_10 == 1 | AJR_noresol_reason_11 ==1 | AJR_noresol_reason_12 ==1 | AJR_noresol_reason_13 ==1 | AJR_noresol_reason_14 == 1 | AJR_noresol_reason_15 == 1 )
	replace drm_`i' = 1 if AJR_drm_`i'==1
}


*----- Efficiency by each mechanism
forvalues i=1/9{
	gen drm_`i'_eff=AJR_drm_`i'_c
	recode drm_`i'_eff (1 2 = 1) (3 4 = 0) (5 6 = .)
}

gen drm_11_eff = AJR_drm_11_c
recode drm_11_eff (1 2 = 1) (3 4 = 0) (5 6 = .)


*----- Fairness of the dispute resolution process by each mechanism 
forvalues i=1/9{
	gen drm_`i'_fair=AJR_drm_`i'_f
	recode drm_`i'_fair (1 2 = 1) (3 4 = 0) (5 6 = .)
}

gen drm_11_fair = AJR_drm_11_f
recode drm_11_fair (1 2 = 1) (3 4 = 0) (5 6 = .)


*----- Affordability by each mechanism 
forvalues i=1/9{
	gen drm_`i'_aff=AJR_drm_`i'_d
	recode drm_`i'_aff (1 2 = 1) (3 4 = 0) (5 6 = .)
}

gen drm_11_aff = AJR_drm_11_d
recode drm_11_aff (1 2 = 1) (3 4 = 0) (5 6 = .)


*----- Duration by each mechanism
forvalues i=1/9{
	gen drm_`i'_dur=AJR_drm_`i'_e
	recode drm_`i'_dur (1 2 = 1) (3 4 = 0) (5 6 = .)
}

gen drm_11_dur = AJR_drm_11_e
recode drm_11_dur (1 2 = 1) (3 4 = 0) (5 6 = .)


*----- Helpfulness by each mechanism   
forvalues i=1/9{
	gen drm_`i'_help=AJR_drm_`i'_h
	recode drm_`i'_help (1 2 = 1) (3 4 = 0) (5 6 = .)
}

gen drm_11_help = AJR_drm_11_h
recode drm_11_help(1 2 = 1) (3 4 = 0) (5 6 = .)


*----- Procedural Fairness in the Resolution

*Outcome
forvalues i=1/9 {
	gen drm_res_`i'_outcome = AJR_drm_res_`i'_a
	recode drm_res_`i'_outcome (1 = 1) (2 = 0) (3 4 = .)
}

gen drm_res_11_outcome = AJR_drm_res_11_a
recode drm_res_11_outcome (1 = 1) (2 = 0) (3 4 = .)

*Appeal
forvalues i=1/9 {
	gen drm_res_`i'_ap = AJR_drm_res_`i'_b
	recode drm_res_`i'_ap (1 = 1) (2 = 0) (3 4 = .)
}

gen drm_res_11_ap = AJR_drm_res_11_b
recode drm_res_11_ap (1 = 1) (2 = 0) (3 4 = .)

*Lawyer representation
forvalues i=1/9 {
	gen drm_res_`i'_rep = AJR_drm_res_`i'_c
	recode drm_res_`i'_rep (1 = 1) (2 = 0) (3 4 = .)
}

gen drm_res_11_rep = AJR_drm_res_11_c
recode drm_res_11_rep (1 = 1) (2 = 0) (3 4 = .)

*Other representation
forvalues i=1/9 {
	gen drm_res_`i'_oth = AJR_drm_res_`i'_d
	recode drm_res_`i'_oth (1 = 1) (2 = 0) (3 4 = .)
}

gen drm_res_11_oth = AJR_drm_res_11_d
recode drm_res_11_oth (1 = 1) (2 = 0) (3 4 = .)


*----- Issue resolution
tab AJR_settle_resol, g(AJR_settle_resol_)


*----- Timeliness of the process

* Create monthly date variables
gen start_date = ym(AJR_year_start, AJR_month_start)
gen end_date   = ym(AJR_year_end, AJR_month_end)

* Format as readable monthly dates
format start_date end_date %tm

* Calculate difference in months
gen diff_months = end_date - start_date

gen timeliness = .
replace timeliness = 1 if diff_months<=12 & diff_months!=.
replace timeliness = 0 if diff_months>12 & diff_months!=.


*----- Fairness of the process
gen fair_process = AJR_fair
recode fair_process (2=0) 
 

*----- Outcome
gen outcome = AJR_status
recode outcome (4 5 = .)
tab outcome, g(outcome_)

gen outcome_done = AJR_status
recode outcome_done (1 = 0) (2 3 = 1) (4 5 = .)

*----- Level of impact
gen level_impact = .
replace level_impact = 0 if AJE_impact == 1 | AJE_impact == 2
replace level_impact = 1 if AJE_impact == 3 | AJE_impact == 4 | AJE_impact ==5 
	

*----- Hardships
forvalues i=1/16 {
	gen hardship_`i' = AJE_hardship_`i'
	recode hardship_`i' (2 = 0) (3 = .)
}

egen nhardships = rowtotal(hardship_1 hardship_2 hardship_3 hardship_4 hardship_5 hardship_6 hardship_7 hardship_8 hardship_9 hardship_10 hardship_11 hardship_12 hardship_13 hardship_14 hardship_15 hardship_16)

gen had_hardship = nhardships>0
replace had_hardship = . if had_dispute==0


*----- Legal capability

gen legal_rights = AJE_legalrights_done
replace legal_rights = AJE_legalrights_ongoing if legal_rights==.

gen infosource = AJE_infosource_done
replace infosource = AJE_infosource_ongoing if infosource==.

gen expert_help = AJE_advice_done
replace expert_help = AJE_advice_ongoing if expert_help ==.

gen fair_outcome = AJE_fairoutcome_done
replace fair_outcome = AJE_fairoutcome_ongoing if fair_outcome ==. 


foreach v in legal_rights infosource expert_help fair_outcome {
	recode `v' (1 2 =1 ) (3 4 = 0)
}


*----- Prevalence of disputes - HIGH IMPACT

gen had_dispute_hi=had_dispute
replace had_dispute_hi=0 if level_impact==0

gen had_dispute_hi_n = had_dispute_hi


*------ Demographics

*Gender
gen gend2=gend
recode gend2 (3 =.)

*Age
gen age_g=.
replace age_g=1 if age>=18 & age<=24 & age!=.
replace age_g=2 if age>=25 & age<=34 & age!=.
replace age_g=3 if age>=35 & age<=44 & age!=.
replace age_g=4 if age>=45 & age<=54 & age!=.
replace age_g=5 if age>=55 & age<=64 & age!=.
replace age_g=6 if age>=65 & age!=.

*Edu 
*edu_2

*Ethnicity
gen ethnic_majority = ""
replace ethnic_majority = "Ethnic majority" if ethni == 1 
replace ethnic_majority = "Ethnic minority" if ethni>1 & ethni<15 & ethni!=.

*Disability
gen disability2= ""
replace disability2 = "With disability" if disability == 1 | disability == 2
replace disability2 = "Without disability" if disability == 3 
  
*Income
gen income2 = income
recode income2 (5=.)
  

	
/*=====================================================================================================================================
					3. Export data
=====================================================================================================================================*/

*----- Run global.do (creates global for all indicators and counts)
do "${path2dos}\globals.do"



*----- Country level  

preserve
collapse (mean) $a2j (count) $a2j_n 

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication.xlsx", replace firstrow(varl) sheet("Overall")
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Overall") modify
putexcel A2:IG2, overwri nformat("0%") 
putexcel B2:B10, overwri nformat("0")
putexcel HF2:HF10, overwri nformat("0")
putexcel A1:IG1, overwri bold hcenter txtwrap

restore


*----- Gender

preserve

collapse (mean) $a2j (count) $a2j_n  , by(gend2)

drop if gend2==.

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

label define gend2 1 "Male" 2 "Female"
label values gend2 gend2

export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(varl) sheet("Gender") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Gender") modify
putexcel B2:IH210, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Age group

preserve

collapse (mean) $a2j (count) $a2j_n , by(age_g)

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

label define age 1 "18-24" 2 "25-34" 3 "35-44" 4 "45-54" 5 "55-64" 6 "+65"
label values age_g age	

export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(varl) sheet("Age") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Age") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Education level 

preserve

collapse (mean) $a2j (count) $a2j_n , by(edu_2)

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"


export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(varl) sheet("Edu") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Edu") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Income 

preserve

collapse (mean) $a2j (count) $a2j_n , by(income2)

drop if income2==.

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

label define income2 1 "< €30k a year" 2 "€30k – €70 a year" 3 "€70k – €120k a year" 4 "> €120k a year"
label values income2 income2  

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(varl) sheet("Income") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Income") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Region 

preserve

collapse (mean) $a2j (count) $a2j_n , by(region)

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(varl) sheet("Region") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Region") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Disability

preserve

collapse (mean) $a2j (count) $a2j_n , by(disability2)

drop if disability2==""

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(varl) sheet("Disability") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Disability") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Impact level
 
preserve

collapse (mean) $a2j (count) $a2j_n , by(level_impact)

drop if level_impact==.

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

label define level_impact 0 "Low impact" 1 "High impact"
label values level_impact level_impact

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(varl) sheet("Level of impact") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Level of impact") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Co-occurrent problems

preserve

collapse (mean) $a2j (count) $a2j_n , by(cooccurence_group)

drop if cooccurence_group==.

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

label define cooccurence_group 1 "1 problem" 2 "2-3 problems" 3 "4-5 problems" 4 "5 or more problems"
label values cooccurence_group cooccurence_group

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(varl) sheet("Co-occurrance") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Co-occurrance") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Ethnicity

preserve

collapse (mean) $a2j (count) $a2j_n , by(ethnic_majority)

drop if ethnic_majority==""

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(varl) sheet("Ethnicity") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Ethnicity") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


/*=====================================================================================================================================
					4. Sub-sample (high impact problems)
=====================================================================================================================================*/


*----- Country level  

preserve

keep if level_impact==1

collapse (mean) $a2j (count) $a2j_n

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication_high.xlsx", replace firstrow(varl) sheet("Overall")
putexcel set "${path2SP}\data\reports_replication_high.xlsx", sheet("Overall") modify
putexcel A2:IG2, overwri nformat("0%") 
putexcel B2:B10, overwri nformat("0")
putexcel HF2:HF10, overwri nformat("0")
putexcel A1:IG1, overwri bold hcenter txtwrap

restore


*----- Gender

preserve

keep if level_impact==1

collapse (mean) $a2j (count) $a2j_n , by(gend2)

drop if gend2==.

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

label define gend2 1 "Male" 2 "Female"
label values gend2 gend2

export excel using "${path2SP}\data\reports_replication_high.xlsx", firstrow(varl) sheet("Gender") 
putexcel set "${path2SP}\data\reports_replication_high.xlsx", sheet("Gender") modify
putexcel B2:IH210, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Age group

preserve

keep if level_impact==1

collapse (mean) $a2j (count) $a2j_n , by(age_g)

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

label define age 1 "18-24" 2 "25-34" 3 "35-44" 4 "45-54" 5 "55-64" 6 "+65"
label values age_g age	

export excel using "${path2SP}\data\reports_replication_high.xlsx", firstrow(varl) sheet("Age") 
putexcel set "${path2SP}\data\reports_replication_high.xlsx", sheet("Age") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Education level 

preserve

keep if level_impact==1

collapse (mean) $a2j (count) $a2j_n , by(edu_2)

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"


export excel using "${path2SP}\data\reports_replication_high.xlsx", firstrow(varl) sheet("Edu") 
putexcel set "${path2SP}\data\reports_replication_high.xlsx", sheet("Edu") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Income 

preserve

keep if level_impact==1

collapse (mean) $a2j (count) $a2j_n , by(income2)

drop if income2==.

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

label define income2 1 "< €30k a year" 2 "€30k – €70 a year" 3 "€70k – €120k a year" 4 "> €120k a year"
label values income2 income2  

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication_high.xlsx", firstrow(varl) sheet("Income") 
putexcel set "${path2SP}\data\reports_replication_high.xlsx", sheet("Income") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Region 

preserve

keep if level_impact==1

collapse (mean) $a2j (count) $a2j_n , by(region)

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication_high.xlsx", firstrow(varl) sheet("Region") 
putexcel set "${path2SP}\data\reports_replication_high.xlsx", sheet("Region") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Disability

preserve

keep if level_impact==1

collapse (mean) $a2j (count) $a2j_n , by(disability2)

drop if disability2==""

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication_high.xlsx", firstrow(varl) sheet("Disability") 
putexcel set "${path2SP}\data\reports_replication_high.xlsx", sheet("Disability") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Impact level
 
preserve

keep if level_impact==1

collapse (mean) $a2j (count) $a2j_n , by(level_impact)

drop if level_impact==.

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

label define level_impact 0 "Low impact" 1 "High impact"
label values level_impact level_impact

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication_high.xlsx", firstrow(varl) sheet("Level of impact") 
putexcel set "${path2SP}\data\reports_replication_high.xlsx", sheet("Level of impact") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Co-occurrent problems

preserve

keep if level_impact==1

collapse (mean) $a2j (count) $a2j_n , by(cooccurence_group)

drop if cooccurence_group==.

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

label define cooccurence_group 1 "1 problem" 2 "2-3 problems" 3 "4-5 problems" 4 "5 or more problems"
label values cooccurence_group cooccurence_group

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication_high.xlsx", firstrow(varl) sheet("Co-occurrance") 
putexcel set "${path2SP}\data\reports_replication_high.xlsx", sheet("Co-occurrance") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore


*----- Ethnicity

preserve

keep if level_impact==1

collapse (mean) $a2j (count) $a2j_n , by(ethnic_majority)

drop if ethnic_majority==""

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

export excel using "${path2SP}\data\reports_replication_high.xlsx", firstrow(varl) sheet("Ethnicity") 
putexcel set "${path2SP}\data\reports_replication_high.xlsx", sheet("Ethnicity") modify
putexcel B2:IH10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel HG2:HG10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore



*----- Prevalence HIGH IMPACT

*----- Country level  

preserve

collapse (mean) had_dispute_hi
//by(gend2 age_g edu_2 income2 region disability2 level_impact cooccurence_group ethnic_majority)
*replace 

*do "${path2dos}\labels.do"

export excel using "${path2SP}\data\prevalence_high.xlsx", firstrow(varl) sheet("Overall") replace
putexcel set "${path2SP}\data\prevalence_high.xlsx", sheet("Overall") modify
putexcel A2:IH10, overwri nformat("0%") 
putexcel B2:B10, overwri nformat("0")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore

*----- Gender

preserve

collapse (mean) had_dispute_hi , by(gend2)

drop if gend2==.

//by(gend2 age_g edu_2 income2 region disability2 level_impact cooccurence_group ethnic_majority)
*replace 

*do "${path2dos}\labels.do"
label define gend2 1 "Male" 2 "Female"
label values gend2 gend2

export excel using "${path2SP}\data\prevalence_high.xlsx", firstrow(varl) sheet("Gender")
putexcel set "${path2SP}\data\prevalence_high.xlsx", sheet("Gender") modify
putexcel B2:B10, overwri nformat("0%") 
putexcel A1:IH1, overwri bold hcenter txtwrap

restore

*----- Age group

preserve

collapse (mean) had_dispute_hi, by(age_g)


//by(gend2 age_g edu_2 income2 region disability2 level_impact cooccurence_group ethnic_majority)
*replace 

*do "${path2dos}\labels.do"
label define age 1 "18-24" 2 "25-34" 3 "35-44" 4 "45-54" 5 "55-64" 6 "+65"
label values age_g age	

export excel using "${path2SP}\data\prevalence_high.xlsx", firstrow(varl) sheet("Age")
putexcel set "${path2SP}\data\prevalence_high.xlsx", sheet("Age") modify 
putexcel B2:B10, overwri nformat("0%")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore

*----- Education level 

preserve

collapse (mean) had_dispute_hi , by(edu_2)

//by(gend2 age_g edu_2 income2 region disability2 level_impact cooccurence_group ethnic_majority)
*replace 

*do "${path2dos}\labels.do"

export excel using "${path2SP}\data\prevalence_high.xlsx", firstrow(varl) sheet("Edu")
putexcel set "${path2SP}\data\prevalence_high.xlsx", sheet("Edu") modify
putexcel B2:B10, overwri nformat("0%")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore

*----- Income 

preserve

collapse (mean) had_dispute_hi , by(income2)

drop if income2==.

//by(gend2 age_g edu_2 income2 region disability2 level_impact cooccurence_group ethnic_majority)
*replace 

*do "${path2dos}\labels.do"
label define income2 1 "< €30k a year" 2 "€30k – €70 a year" 3 "€70k – €120k a year" 4 "> €120k a year"
label values income2 income2  

export excel using "${path2SP}\data\prevalence_high.xlsx", firstrow(varl) sheet("Income")
putexcel set "${path2SP}\data\prevalence_high.xlsx", sheet("Income") modify
putexcel B2:B10, overwri nformat("0%")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore

*----- Region 

preserve

collapse (mean) had_dispute_hi , by(region)


//by(gend2 age_g edu_2 income2 region disability2 level_impact cooccurence_group ethnic_majority)
*replace 

*do "${path2dos}\labels.do"

export excel using "${path2SP}\data\prevalence_high.xlsx", firstrow(varl) sheet("Region")
putexcel set "${path2SP}\data\prevalence_high.xlsx", sheet("Region") modify
putexcel B2:B10, overwri nformat("0%")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore

*----- Disability

preserve

collapse (mean) had_dispute_hi, by(disability2)

drop if disability2==""


//by(gend2 age_g edu_2 income2 region disability2 level_impact cooccurence_group ethnic_majority)
*replace 

*do "${path2dos}\labels.do"

export excel using "${path2SP}\data\prevalence_high.xlsx", firstrow(varl) sheet("Disability")
putexcel set "${path2SP}\data\prevalence_high.xlsx", sheet("Disability") modify
putexcel B2:B10, overwri nformat("0%")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore

*----- Co-occurrent problems

preserve

collapse (mean) had_dispute_hi , by(cooccurence_group)

drop if cooccurence_group==.

//by(gend2 age_g edu_2 income2 region disability2 level_impact cooccurence_group ethnic_majority)
*replace 

*do "${path2dos}\labels.do"

export excel using "${path2SP}\data\prevalence_high.xlsx", firstrow(varl) sheet("Co-occurrance")
putexcel set "${path2SP}\data\prevalence_high.xlsx", sheet("Co-occurrance") modify
putexcel B2:B10, overwri nformat("0%")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore

*----- Ethnicity

preserve

collapse (mean) had_dispute_hi , by(ethnic_majority)

drop if ethnic_majority==""

//by(gend2 age_g edu_2 income2 region disability2 level_impact cooccurence_group ethnic_majority)
*replace 

*do "${path2dos}\labels.do"

export excel using "${path2SP}\data\prevalence_high.xlsx", firstrow(varl) sheet("Ethnicity")
putexcel set "${path2SP}\data\prevalence_high.xlsx", sheet("Ethnicity") modify
putexcel B2:B10, overwri nformat("0%")
putexcel A1:IH1, overwri bold hcenter txtwrap

restore



*----- Breakdown by problems

gen level_impact2=level_impact
gen level_impact2_n=level_impact2

global prob "AJD_noadvice_reason_1 AJD_noadvice_reason_2 AJD_noadvice_reason_3 AJD_noadvice_reason_4 AJD_noadvice_reason_5 AJD_noadvice_reason_6 AJD_noadvice_reason_7 AJD_noadvice_reason_8 AJD_noadvice_reason_9 AJD_noadvice_reason_10 AJD_noadvice_reason_11 AJD_noadvice_reason_12 AJD_noadvice_reason_13 AJD_noadvice_reason_14 AJD_noadvice_reason_15 AJD_noadvice_reason_16 AJD_noadvice_reason_17 drm_1 drm_2 drm_3 drm_4 drm_5 drm_6 drm_7 drm_8 drm_9 drm_10 drm_11 level_impact2 hardship_1 hardship_2 hardship_3 hardship_4 hardship_5 hardship_6 hardship_7 hardship_8 hardship_9 hardship_10 hardship_11 hardship_12 hardship_13 hardship_14 hardship_15 hardship_16 legal_rights infosource expert_help fair_outcome"

preserve
collapse (mean) $a2j level_impact2 (count) $a2j_n level_impact2_n, by(AJP_cat_selected)

*Removing low counts: Less than 30
foreach v in $a2j {
	replace `v' = . if `v'_n<30
}

drop $a2j_n

do "${path2dos}\labels.do"

keep AJP_cat_selected $prob

drop if AJP_cat_selected==.

export excel using "${path2SP}\data\problems_breakdown.xlsx", replace firstrow(varl) sheet("Overall")
putexcel set "${path2SP}\data\problems_breakdown.xlsx", sheet("Overall") modify
putexcel A2:AX13, overwri nformat("0%") 
putexcel A1:AX1, overwri bold hcenter txtwrap

restore


