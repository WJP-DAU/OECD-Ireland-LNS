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
global path2dos  "${path2SP}/"

	
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


*----- Procedural Fairness in the Resolution PENDING


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

#delimit ;
global a2j "
had_dispute 
ndisputes
land neighbors housing family injury citizenship gov public_services products services MoneyDebt employment
access2info
access2rep 
contact_adviser no_contact_adviser
appropriate_adviser no_appropriate_adviser
AJD_adviser_1_bin AJD_adviser_2_bin AJD_adviser_3_bin AJD_adviser_4_bin AJD_adviser_5_bin AJD_adviser_6_bin AJD_adviser_7_bin AJD_adviser_8_bin AJD_adviser_9_bin AJD_adviser_10_bin AJD_adviser_11_bin AJD_adviser_12_bin AJD_adviser_13_bin AJD_adviser_14_bin AJD_adviser_15_bin AJD_adviser_16_bin AJD_adviser_17_bin
adviser_help_1 adviser_help_2 adviser_help_3 adviser_help_4 adviser_help_5 adviser_help_6 adviser_help_7 adviser_help_8 adviser_help_9 adviser_help_10 adviser_help_11 adviser_help_12 adviser_help_13 adviser_help_14 adviser_help_15 adviser_help_16 adviser_help_17
reason_no_need reason_had_help reason_info_bar reason_relation_bar reason_psycho_bar reason_prior reason_social reason_other
AJD_noadvice_reason_1 AJD_noadvice_reason_2 AJD_noadvice_reason_3 AJD_noadvice_reason_4 AJD_noadvice_reason_5 AJD_noadvice_reason_6 AJD_noadvice_reason_7 AJD_noadvice_reason_8 AJD_noadvice_reason_9 AJD_noadvice_reason_10 AJD_noadvice_reason_11 AJD_noadvice_reason_12 AJD_noadvice_reason_13 AJD_noadvice_reason_14 AJD_noadvice_reason_15 AJD_noadvice_reason_16 AJD_noadvice_reason_17
AJR_action_1 AJR_action_2 AJR_action_3 AJR_action_4
AJR_noaction_1 AJR_noaction_2 AJR_noaction_3 AJR_noaction_4 AJR_noaction_5 AJR_noaction_6 AJR_noaction_7 AJR_noaction_8 AJR_noaction_9 AJR_noaction_10 AJR_noaction_11 AJR_noaction_12 AJR_noaction_13
access2drm
contacted_drm
needed_drm
no_need_drm
unknown_drm
AJR_drm_1_bin AJR_drm_2_bin AJR_drm_3_bin AJR_drm_4_bin AJR_drm_5_bin AJR_drm_6_bin AJR_drm_7_bin AJR_drm_8_bin AJR_drm_9_bin AJR_drm_10_bin AJR_drm_11_bin
AJR_noresol_reason_1 AJR_noresol_reason_2 AJR_noresol_reason_3 AJR_noresol_reason_4 AJR_noresol_reason_5 AJR_noresol_reason_6 AJR_noresol_reason_7 AJR_noresol_reason_8 AJR_noresol_reason_9 AJR_noresol_reason_10 AJR_noresol_reason_11 AJR_noresol_reason_12 AJR_noresol_reason_13 AJR_noresol_reason_14 AJR_noresol_reason_15 AJR_noresol_reason_16
drm_1_eff drm_2_eff drm_3_eff drm_4_eff drm_5_eff drm_6_eff drm_7_eff drm_8_eff drm_9_eff drm_11_eff 
drm_1_fair drm_2_fair drm_3_fair drm_4_fair drm_5_fair drm_6_fair drm_7_fair drm_8_fair drm_9_fair drm_11_fair 
drm_1_aff drm_2_aff drm_3_aff drm_4_aff drm_5_aff drm_6_aff drm_7_aff drm_8_aff drm_9_aff drm_11_aff 
drm_1_dur drm_2_dur drm_3_dur drm_4_dur drm_5_dur drm_6_dur drm_7_dur drm_8_dur drm_9_dur drm_11_dur 
drm_1_help drm_2_help drm_3_help drm_4_help drm_5_help drm_6_help drm_7_help drm_8_help drm_9_help drm_11_help
diff_months timeliness 
fair_process outcome_1 outcome_2 outcome_3 outcome_done
had_hardship
hardship_1 hardship_2 hardship_3 hardship_4 hardship_5 hardship_6 hardship_7 hardship_8 hardship_9 hardship_10 hardship_11 hardship_12 hardship_13 hardship_14 hardship_15 hardship_16
legal_rights infosource expert_help fair_outcome
" ;
;
#delimit cr

* Total sample counts - Country
preserve

gen cont=1

collapse (sum) cont

save "${path2SP}\data\counts_a2j_path.dta", replace

restore


*----- Country level  

preserve
collapse (mean) $a2j

export excel using "${path2SP}\data\reports_replication.xlsx", replace firstrow(var) sheet("Overall")
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Overall") modify
putexcel A2:GY2, overwri nformat("0%") 
putexcel B2:B10, overwri nformat("0")
putexcel FW2:FW10, overwri nformat("0")
putexcel A1:GY1, overwri bold hcenter txtwrap

restore


*----- Gender
preserve

collapse (mean) $a2j , by(gend2)

drop if gend2==.

label define gend2 1 "Male" 2 "Female"
label values gend2 gend2

export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(var) sheet("Gender") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Gender") modify
putexcel B2:GY210, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel FX2:FX10, overwri nformat("0")
putexcel A1:GY1, overwri bold hcenter txtwrap

restore


*----- Age group
preserve

collapse (mean) $a2j, by(age_g)

label define age 1 "18-24" 2 "25-34" 3 "35-44" 4 "45-54" 5 "55-64" 6 "+65"
label values age_g age	

export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(var) sheet("Age") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Age") modify
putexcel B2:GY10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel FX2:FX10, overwri nformat("0")
putexcel A1:GY1, overwri bold hcenter txtwrap

restore


*----- Education level 
preserve

collapse (mean) $a2j, by(edu_2)


export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(var) sheet("Edu") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Edu") modify
putexcel B2:GY10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel FX2:FX10, overwri nformat("0")
putexcel A1:GY1, overwri bold hcenter txtwrap

restore


*----- Income 
preserve

collapse (mean) $a2j, by(income2)


export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(var) sheet("Income") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Income") modify
putexcel B2:GY10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel FX2:FX10, overwri nformat("0")
putexcel A1:GY1, overwri bold hcenter txtwrap

restore


*----- Region 
preserve

collapse (mean) $a2j, by(region)


export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(var) sheet("Region") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Region") modify
putexcel B2:GY10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel FX2:FX10, overwri nformat("0")
putexcel A1:GY1, overwri bold hcenter txtwrap

restore


*----- Disability
preserve

collapse (mean) $a2j, by(disability2)


export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(var) sheet("Disability") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Disability") modify
putexcel B2:GY10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel FX2:FX10, overwri nformat("0")
putexcel A1:GY1, overwri bold hcenter txtwrap

restore


*----- Impact level 
preserve

collapse (mean) $a2j, by(level_impact)


export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(var) sheet("Level of impact") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Level of impact") modify
putexcel B2:GY10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel FX2:FX10, overwri nformat("0")
putexcel A1:GY1, overwri bold hcenter txtwrap

restore


*----- Co-occurrent problems
preserve

collapse (mean) $a2j, by(cooccurence_group)


export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(var) sheet("Co-occurrance") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Co-occurrance") modify
putexcel B2:GY10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel FX2:FX10, overwri nformat("0")
putexcel A1:GY1, overwri bold hcenter txtwrap

restore


*----- Ethnicity
preserve

collapse (mean) $a2j, by(ethnic_majority)


export excel using "${path2SP}\data\reports_replication.xlsx", firstrow(var) sheet("Ethnicity") 
putexcel set "${path2SP}\data\reports_replication.xlsx", sheet("Ethnicity") modify
putexcel B2:GY10, overwri nformat("0%") 
putexcel C2:C10, overwri nformat("0")
putexcel FX2:FX10, overwri nformat("0")
putexcel A1:GY1, overwri bold hcenter txtwrap

restore


