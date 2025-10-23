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









