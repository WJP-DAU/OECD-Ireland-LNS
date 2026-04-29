/*=================================================================================================================
Project:		OECD - LNS Ireland
Routine:		Survey cleaning routine
Author(s):		Natalia Rodriguez 	(nrodriguez@worldjusticeproject.org)
Dependencies:  	World Justice Project
Creation Date:	October, 2025

Description:
Master dofile for the cleaning of the OECD-LNS for Ireland.

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
global path2dos  "${path2SP}/cleaning/routines"

	
/*=================================================================================================================
					1. Data Import
=================================================================================================================*/

*----- Importing the original dataset
import spss using "${path2SP}\data\original\OECD-LEGAL_IncludingVerbatims.sav"

save "${path2SP}\data\ireland_lns_2025_raw.dta", replace

*use "${path2SP}\data\ireland_lns_2025_raw.dta", clear

/*=================================================================================================================
					2. Country wrangling
=================================================================================================================*/
/*
*----- Include standard labels:
cls
do "${path2dos}/variable_labels.do"
*/

*----- Creating NUTS 3 variable
gen NUTS_3=""

replace NUTS_3="Border" if county==5 | county==21 | county==12 | county==2 | county==18

replace NUTS_3="West" if county==7 | county==16 | county==20

replace NUTS_3="Mid-west" if county==3 | county==22 | county==13

replace NUTS_3="South East" if county==23 | county==10 | county==1 | county==25

replace NUTS_3="South-West" if county==4 | county==8

replace NUTS_3="Dublin" if county==6

replace NUTS_3="Mid-East" if county==26 | county==9 | county==17 | county==15

replace NUTS_3="Midlands" if county==14 | county==24 | county==19 | county==11


/*=================================================================================================================
					3. Routing
=================================================================================================================*/

*----- READ: Check for non-zero values

cls
do "${path2dos}/routing.do"

	
/*=================================================================================================================
					4. Renaming
=================================================================================================================*/

cls
do "${path2dos}/renaming.do"


/*=================================================================================================================
					5. Value labels
=================================================================================================================*/
 /*PENDING*/

/*=================================================================================================================
					6. Final dataset
=================================================================================================================*/

save "${path2SP}\data\ireland_lns_2025_final.dta", replace









