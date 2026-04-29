/*=================================================================================================================
Project:		OECD - LNS Ireland
Routine:		Routing
Author(s):		Natalia Rodriguez 	(nrodriguez@worldjusticeproject.org)
Dependencies:  	World Justice Project
Creation Date:	October, 2025

Description:
Do-file that reviews the routing for all questions.

=================================================================================================================*/

*-------- q23_2 -> q23_3
/* We're checking that the problems mentioned in q23_2 have a frequency associated (q23_3) */

tokenize 01_1 02_1 02_2 02_3 03_1 03_2 03_3 04_1 04_2 04_3 05_1 05_2 05_3 05_4 05_5 05_6 05_7 06_1 06_2 06_3 06_4 06_5 06_6 07_1 07_2 07_3 07_4 07_5 08_1 08_2 08_3 08_4 08_5 09_1 09_2 09_3 09_4 09_5 09_6 10_1 10_2 10_3 10_4 11_1 11_2 12_1 12_2 13_1 13_2 13_3 13_4 13_5 14_1 14_2 14_3 14_4
forvalues i=1/56 {
	di as result "`: variable label q23_3_``i'' '"
	
	* Skip
	cap: inspect q23_3_``i'' if q23_2_``i'' != 1
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)

	* Non-skip
	qui count if q23_3_``i'' == . & q23_2_``i'' == 1
	di as text "NO-SKIP - obs: " as error r(N)
	
}

//1.	Land related issues

* Skip
cap: inspect q23_3_01_1 if q23_2_01_1 != 1
di "SKIP - obs: " r(N) "; values: " r(N_unique)

* Non-skip
qui count if q23_3_01_1 == . & q23_2_01_1 == 1
di "NO-SKIP - obs: " r(N)


//2.	Issues with neighbours

forvalues i=1/3 {
	di as text "`: variable label q23_3_02_`i' '"
	
	* Skip
	cap: inspect q23_3_02_`i' if q23_2_02_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_02_`i' == . & q23_2_02_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}


//3.	Issues as a social housing or private tenant 

*These problems only appear if the respondent mentioned "Yes" in q23_1_1
forvalues i=1/3 {
	di as text "`: variable label q23_2_03_`i' '"
	
	* Skip
	cap: inspect q23_2_03_`i' if q23_1_1!= 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_2_03_`i' == . & q23_1_1 == 1
	di as result "NO-SKIP - obs: " r(N)
	
}

* For the frequency
forvalues i=1/3 {
	di as text "`: variable label q23_3_03_`i' '"
	
	* Skip
	cap: inspect q23_3_03_`i' if q23_2_03_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_03_`i' == . & q23_2_03_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}


//4.	Issues as a private landlord

*These problems only appear if the respondent mentioned "Yes" in q23_1_2
forvalues i=1/3 {
	di as text "`: variable label q23_2_04_`i' '"
	
	* Skip
	cap: inspect q23_2_04_`i' if q23_1_2!= 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_2_04_`i' == . & q23_1_2 == 1
	di as result "NO-SKIP - obs: " r(N)
	
}

*For the frequency
forvalues i=1/3 {
	di as text "`: variable label q23_3_04_`i' '"
	
	* Skip
	cap: inspect q23_3_04_`i' if q23_2_04_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_04_`i' == . & q23_2_04_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}


//5.	Other issues with your home or any housing you own or owned

*These problems only appear if the respondent mentioned "Yes" in q23_1_3
forvalues i=1/7 {
	di as text "`: variable label q23_2_05_`i' '"
	
	* Skip
	cap: inspect q23_2_05_`i' if q23_1_3!= 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_2_05_`i' == . & q23_1_3 == 1
	di as result "NO-SKIP - obs: " r(N)
	
}

*For the frequency
forvalues i=1/7 {
	di as text "`: variable label q23_3_05_`i' '"
	
	* Skip
	cap: inspect q23_3_05_`i' if q23_2_05_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_05_`i' == . & q23_2_05_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}

//6.	Family or relationship issues

forvalues i=1/6 {
	di as text "`: variable label q23_3_06_`i' '"
	
	* Skip
	cap: inspect q23_3_06_`i' if q23_2_06_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_06_`i' == . & q23_2_06_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}


//7.	Citizenship or migration issues

forvalues i=1/5 {
	di as text "`: variable label q23_3_07_`i' '"
	
	* Skip
	cap: inspect q23_3_07_`i' if q23_2_07_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_07_`i' == . & q23_2_07_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}


//8.	Injury or illness related issues 

forvalues i=1/5 {
	di as text "`: variable label q23_3_08_`i' '"
	
	* Skip
	cap: inspect q23_3_08_`i' if q23_2_08_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_08_`i' == . & q23_2_08_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}


//9.	Issues relating to eligibility for / payment of government benefits and payments

forvalues i=1/6 {
	di as text "`: variable label q23_3_09_`i' '"
	
	* Skip
	cap: inspect q23_3_09_`i' if q23_2_09_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_09_`i' == . & q23_2_09_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}


//10.	Issues accessing public services

forvalues i=1/4 {
	di as text "`: variable label q23_3_10_`i' '"
	
	* Skip
	cap: inspect q23_3_10_`i' if q23_2_10_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_10_`i' == . & q23_2_10_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}


//11.	Issues with products bought in shops or online 

forvalues i=1/2 {
	di as text "`: variable label q23_3_11_`i' '"
	
	* Skip
	cap: inspect q23_3_11_`i' if q23_2_11_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_11_`i' == . & q23_2_11_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}


//12.	Issues with services (excluding utilities), such as mobile phone plan, food-delivery, taxis/ride-share, beauty treatments, and professional services such as dentists, lawyers and accountants (but excluding builders/tradespeople)

forvalues i=1/2 {
	di as text "`: variable label q23_3_12_`i' '"
	
	* Skip
	cap: inspect q23_3_12_`i' if q23_2_12_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_12_`i' == . & q23_2_12_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}


//13.	Money or debt issues 

forvalues i=1/5 {
	di as text "`: variable label q23_3_13_`i' '"
	
	* Skip
	cap: inspect q23_3_13_`i' if q23_2_13_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_13_`i' == . & q23_2_13_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}


//14.	Issues related to your employment

forvalues i=1/4 {
	di as text "`: variable label q23_3_14_`i' '"
	
	* Skip
	cap: inspect q23_3_14_`i' if q23_2_14_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_3_14_`i' == . & q23_2_14_`i' == 1
	di as result "NO-SKIP - obs: " r(N)
	
}


*-------- Creating number of disputes 

*Creating binary varibles for each problem
foreach v in q23_2_01_1 q23_2_02_1 q23_2_02_2 q23_2_02_3 q23_2_03_1 q23_2_03_2 q23_2_03_3 q23_2_04_1 q23_2_04_2 q23_2_04_3 q23_2_05_1 q23_2_05_2 q23_2_05_3 q23_2_05_4 q23_2_05_5 q23_2_05_6 q23_2_05_7 q23_2_06_1 q23_2_06_2 q23_2_06_3 q23_2_06_4 q23_2_06_5 q23_2_06_6 q23_2_07_1 q23_2_07_2 q23_2_07_3 q23_2_07_4 q23_2_07_5 q23_2_08_1 q23_2_08_2 q23_2_08_3 q23_2_08_4 q23_2_08_5 q23_2_09_1 q23_2_09_2 q23_2_09_3 q23_2_09_4 q23_2_09_5 q23_2_09_6 q23_2_10_1 q23_2_10_2 q23_2_10_3 q23_2_10_4 q23_2_11_1 q23_2_11_2 q23_2_12_1 q23_2_12_2 q23_2_13_1 q23_2_13_2 q23_2_13_3 q23_2_13_4 q23_2_13_5 q23_2_14_1 q23_2_14_2 q23_2_14_3 q23_2_14_4 {
	gen `v'_bin = `v'
	recode `v'_bin (2 =0) (3 = .)
}

egen ndisputes=rowtotal(q23_2_01_1_bin q23_2_02_1_bin q23_2_02_2_bin q23_2_02_3_bin q23_2_03_1_bin q23_2_03_2_bin q23_2_03_3_bin q23_2_04_1_bin q23_2_04_2_bin q23_2_04_3_bin q23_2_05_1_bin q23_2_05_2_bin q23_2_05_3_bin q23_2_05_4_bin q23_2_05_5_bin q23_2_05_6_bin q23_2_05_7_bin q23_2_06_1_bin q23_2_06_2_bin q23_2_06_3_bin q23_2_06_4_bin q23_2_06_5_bin q23_2_06_6_bin q23_2_07_1_bin q23_2_07_2_bin q23_2_07_3_bin q23_2_07_4_bin q23_2_07_5_bin q23_2_08_1_bin q23_2_08_2_bin q23_2_08_3_bin q23_2_08_4_bin q23_2_08_5_bin q23_2_09_1_bin q23_2_09_2_bin q23_2_09_3_bin q23_2_09_4_bin q23_2_09_5_bin q23_2_09_6_bin q23_2_10_1_bin q23_2_10_2_bin q23_2_10_3_bin q23_2_10_4_bin q23_2_11_1_bin q23_2_11_2_bin q23_2_12_1_bin q23_2_12_2_bin q23_2_13_1_bin q23_2_13_2_bin q23_2_13_3_bin q23_2_13_4_bin q23_2_13_5_bin q23_2_14_1_bin q23_2_14_2_bin q23_2_14_3_bin q23_2_14_4_bin)

gen had_dispute=ndisputes>0


*-------- Check that the people that mentioned problems have a problem selected 

di as result "Testing that respondents with no problem selected in fact didn't mention any legal problem"
count if ndisputes==0 & q23_4_recentissue_selected!=. 
if r(N) > 0 {
	di as error "Number of observations with a problem selected but no mentioned problems: " r(N)
}

di as result "Testing that respondents mentioned problems and do have a problem selected"
count if ndisputes>0 & q23_4_recentissue_selected==.
if r(N) > 0 {
	di as error "Number of observations with mentioned problems but no problem selected: " r(N)
}


*-------- Creating categories 

forvalues i=1/12 {
	gen problem_cat_`i'=0
}

*1.	Land
replace problem_cat_1=1 if q23_2_01_1==1 

*2.	Neighbours
replace problem_cat_2=1 if q23_2_02_1==1 | q23_2_02_2==1 | q23_2_02_3==1

*3.	Housing (if Yes at any of 3,4,5)
replace problem_cat_3=1 if q23_2_03_1==1 | q23_2_03_2==1 | q23_2_03_3==1 | q23_2_04_1==1 | q23_2_04_2==1 | q23_2_04_3==1 | q23_2_05_1==1 | q23_2_05_2==1 | q23_2_05_3==1 | q23_2_05_4==1 | q23_2_05_5==1 | q23_2_05_6==1 | q23_2_05_7==1

*4.	Family/relationship
replace problem_cat_4=1 if q23_2_06_1==1 | q23_2_06_2==1 | q23_2_06_3==1 | q23_2_06_4==1 | q23_2_06_5==1 | q23_2_06_6==1

*5.	Injury
replace problem_cat_5=1 if q23_2_08_1==1 | q23_2_08_2==1 | q23_2_08_3==1 | q23_2_08_4==1 | q23_2_08_5==1

*6.	Citizenship or migration
replace problem_cat_6=1 if q23_2_07_1==1 | q23_2_07_2==1 | q23_2_07_3==1 | q23_2_07_4==1 | q23_2_07_5==1

*7.	Government benefits and payments
replace problem_cat_7=1 if q23_2_09_1==1 | q23_2_09_2==1 | q23_2_09_3==1 | q23_2_09_4==1 | q23_2_09_5==1 | q23_2_09_6==1

*8.	Public services
replace problem_cat_8=1 if q23_2_10_1==1 | q23_2_10_2==1 | q23_2_10_3==1 | q23_2_10_4==1

*9.	Products
replace problem_cat_9=1 if q23_2_11_1==1 | q23_2_11_2==1

*10.	Services
replace problem_cat_10=1 if q23_2_12_1==1 | q23_2_12_2==1

*11.	Money/debt
replace problem_cat_11=1 if q23_2_13_1==1 | q23_2_13_2==1 | q23_2_13_3==1 | q23_2_13_4==1 | q23_2_13_5==1

*12.	Employment
replace problem_cat_12=1 if q23_2_14_1==1 | q23_2_14_2==1 | q23_2_14_3==1 | q23_2_14_4==1


*-------- Check that the category selected was mentioned

gen none=0
forvalues i=1/12 {
	replace none=none+1 if q23_2_catselected==`i' & problem_cat_`i'!=1
}

di as result "Testing that the caterogy selected was mentioned"
inspect none 
if r(N_pos) > 0 {
	di as error "Number of observations with a category selected that was not mentioned: " r(N_pos)
}

drop none

*--------  Check that the problem selected was mentioned
gen none=0

tokenize q23_2_01_1 q23_2_02_1 q23_2_02_2 q23_2_02_3 q23_2_03_1 q23_2_03_2 q23_2_03_3 q23_2_04_1 q23_2_04_2 q23_2_04_3 q23_2_05_1 q23_2_05_2 q23_2_05_3 q23_2_05_4 q23_2_05_5 q23_2_05_6 q23_2_05_7 q23_2_06_1 q23_2_06_2 q23_2_06_3 q23_2_06_4 q23_2_06_5 q23_2_06_6 q23_2_07_1 q23_2_07_2 q23_2_07_3 q23_2_07_4 q23_2_07_5 q23_2_08_1 q23_2_08_2 q23_2_08_3 q23_2_08_4 q23_2_08_5 q23_2_09_1 q23_2_09_2 q23_2_09_3 q23_2_09_4 q23_2_09_5 q23_2_09_6 q23_2_10_1 q23_2_10_2 q23_2_10_3 q23_2_10_4 q23_2_11_1 q23_2_11_2 q23_2_12_1 q23_2_12_2 q23_2_13_1 q23_2_13_2 q23_2_13_3 q23_2_13_4 q23_2_13_5 q23_2_14_1 q23_2_14_2 q23_2_14_3 q23_2_14_4
forvalues i=1/56 {
	di as text "``i''"
	replace none=none+1 if q23_4_recentissue_selected==`i' & ``i''!=1
}

di as result "Testing that the problem selected was mentioned"
inspect none 
if r(N_pos) > 0 {
	di as error "Number of observations with a problem selected that was not mentioned: " r(N_pos)
}

drop none


*-------- Calculate the # of problems mentioned within a each category

*1.	Land related issues
*q23_2_01_1 is the only problem in this category

*2.	Neighbours
egen n_problem_cat_2=rowtotal(q23_2_02_1_bin q23_2_02_2_bin q23_2_02_3_bin)

*3.	Housing (if Yes at any of 3,4,5)
egen n_problem_cat_3= rowtotal(q23_2_04_1_bin q23_2_04_2_bin q23_2_04_3_bin q23_2_05_1_bin q23_2_05_2_bin q23_2_05_3_bin q23_2_05_4_bin q23_2_05_5_bin q23_2_05_6_bin q23_2_05_7_bin)

*4.	Family/relationship
egen n_problem_cat_4 = rowtotal(q23_2_06_1_bin q23_2_06_2_bin q23_2_06_3_bin q23_2_06_4_bin q23_2_06_5_bin q23_2_06_6_bin)

*5.	Injury
egen n_problem_cat_5= rowtotal(q23_2_08_1_bin q23_2_08_2_bin q23_2_08_3_bin q23_2_08_4_bin q23_2_08_5_bin)

*6.	Citizenship or migration
egen n_problem_cat_6= rowtotal(q23_2_07_1_bin q23_2_07_2_bin q23_2_07_3_bin q23_2_07_4_bin q23_2_07_5_bin)

*7.	Government benefits and payments
egen n_problem_cat_7= rowtotal(q23_2_09_1_bin q23_2_09_2_bin q23_2_09_3_bin q23_2_09_4_bin q23_2_09_5_bin q23_2_09_6_bin)

*8.	Public services
egen n_problem_cat_8= rowtotal(q23_2_10_1_bin q23_2_10_2_bin q23_2_10_3_bin q23_2_10_4_bin)

*9.	Products
egen n_problem_cat_9= rowtotal(q23_2_11_1_bin q23_2_11_2_bin)

*10.	Services
egen n_problem_cat_10= rowtotal(q23_2_12_1_bin q23_2_12_2_bin)

*11.	Money/debt
egen n_problem_cat_11= rowtotal(q23_2_13_1_bin q23_2_13_2_bin q23_2_13_3_bin q23_2_13_4_bin q23_2_13_5_bin)

*12.	Employment
egen n_problem_cat_12= rowtotal(q23_2_14_1_bin q23_2_14_2_bin q23_2_14_3_bin q23_2_14_4_bin)


*-------- q23_3 -> q23_4
/* We're checking that the problems mentioned have a ranking (when more than 1 problem was mentioned in a category) */



*---1.	Land

*q23_2_01_1 is the only problem in this category


*---2.	Issues with neighbours

forvalues i=1/3 {
	di as text "`: variable label q23_4_02_`i' '"
	
	* Skip
	cap: inspect q23_4_02_`i' if q23_2_02_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_02_`i' == . & q23_2_02_`i' == 1 & n_problem_cat_2>1 & q23_2_catselected==2
	di as result "NO-SKIP - obs: " r(N)
	
}


*---3.	Housing (if Yes at any of 3,4,5)

*3.	Issues as a social housing or private tenant 

forvalues i=1/3 {
	di as text "`: variable label q23_4_03_`i' '"
	
	* Skip
	cap: inspect q23_4_03_`i' if q23_2_03_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_03_`i' == . & q23_2_03_`i' == 1 & n_problem_cat_3>1 & q23_2_catselected==3
	di as result "NO-SKIP - obs: " r(N)
	
}


*4.	Issues as a private landlord

forvalues i=1/3 {
	di as text "`: variable label q23_4_04_`i' '"
	
	* Skip
	cap: inspect q23_4_04_`i' if q23_2_04_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_04_`i' == . & q23_2_04_`i' == 1 & n_problem_cat_3>1 & q23_2_catselected==3
	di as result "NO-SKIP - obs: " r(N)
	
}


*5.	Other issues with your home or any housing you own or owned

forvalues i=1/7 {
	di as text "`: variable label q23_4_05_`i' '"
	
	* Skip
	cap: inspect q23_4_05_`i' if q23_2_05_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_05_`i' == . & q23_2_05_`i' == 1 & n_problem_cat_3>1 & q23_2_catselected==3
	di as result "NO-SKIP - obs: " r(N)
	
}

*---4.	Family or relationship issues

forvalues i=1/6 {
	di as text "`: variable label q23_4_06_`i' '"
	
	* Skip
	cap: inspect q23_4_06_`i' if q23_2_06_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_06_`i' == . & q23_2_06_`i' == 1 & n_problem_cat_4>1 & q23_2_catselected==4
	di as result "NO-SKIP - obs: " r(N)
	
}

*---5.	Injury or illness related issues 

forvalues i=1/5 {
	di as text "`: variable label q23_4_08_`i' '"
	
	* Skip
	cap: inspect q23_4_08_`i' if q23_2_08_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_08_`i' == . & q23_2_08_`i' == 1 & n_problem_cat_5>1 & q23_2_catselected==5
	di as result "NO-SKIP - obs: " r(N)
	
}

*---6.	Citizenship or migration

forvalues i=1/5 {
	di as text "`: variable label q23_4_07_`i' '"
	
	* Skip
	cap: inspect q23_4_07_`i' if q23_2_07_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_07_`i' == . & q23_2_07_`i' == 1 & n_problem_cat_6>1 & q23_2_catselected==6
	di as result "NO-SKIP - obs: " r(N)
	
}


*---7.	Government benefits and payments

forvalues i=1/6 {
	di as text "`: variable label q23_4_09_`i' '"
	
	* Skip
	cap: inspect q23_4_09_`i' if q23_2_09_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_09_`i' == . & q23_2_09_`i' == 1 & n_problem_cat_7>1 & q23_2_catselected==7
	di as result "NO-SKIP - obs: " r(N)
	
}


*---8.	Public services

forvalues i=1/4 {
	di as text "`: variable label q23_4_10_`i' '"
	
	* Skip
	cap: inspect q23_4_10_`i' if q23_2_10_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_10_`i' == . & q23_2_10_`i' == 1 & n_problem_cat_8>1 & q23_2_catselected==8
	di as result "NO-SKIP - obs: " r(N)
	
}


*---9.	Products

forvalues i=1/2 {
	di as text "`: variable label q23_4_11_`i' '"
	
	* Skip
	cap: inspect q23_4_11_`i' if q23_2_11_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_11_`i' == . & q23_2_11_`i' == 1 & n_problem_cat_9>1 & q23_2_catselected==9
	di as result "NO-SKIP - obs: " r(N)
	
}


*---10.	Services

forvalues i=1/2 {
	di as text "`: variable label q23_4_12_`i' '"
	
	* Skip
	cap: inspect q23_4_12_`i' if q23_2_12_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_12_`i' == . & q23_2_12_`i' == 1 & n_problem_cat_10>1 & q23_2_catselected==10
	di as result "NO-SKIP - obs: " r(N)
	
}


*---11.	Money/debt

forvalues i=1/5 {
	di as text "`: variable label q23_4_13_`i' '"
	
	* Skip
	cap: inspect q23_4_13_`i' if q23_2_13_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_13_`i' == . & q23_2_13_`i' == 1 & n_problem_cat_11>1 & q23_2_catselected==11
	di as result "NO-SKIP - obs: " r(N)
	
}


*---12.	Employment

forvalues i=1/4 {
	di as text "`: variable label q23_4_14_`i' '"
	
	* Skip
	cap: inspect q23_4_14_`i' if q23_2_14_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_14_`i' == . & q23_2_14_`i' == 1 & n_problem_cat_12>1 & q23_2_catselected==12
	di as result "NO-SKIP - obs: " r(N)
	
}


*--------  Check that if more than one problem was selected within the category, the problem selected is the second most recent problem mentioned

*Renaming binary problems
tokenize 01_1 02_1 02_2 02_3 03_1 03_2 03_3 04_1 04_2 04_3 05_1 05_2 05_3 05_4 05_5 05_6 05_7 06_1 06_2 06_3 06_4 06_5 06_6 07_1 07_2 07_3 07_4 07_5 08_1 08_2 08_3 08_4 08_5 09_1 09_2 09_3 09_4 09_5 09_6 10_1 10_2 10_3 10_4 11_1 11_2 12_1 12_2 13_1 13_2 13_3 13_4 13_5 14_1 14_2 14_3 14_4
forvalues i=1/56 {
	rename q23_2_``i''_bin AJP_`i'_bin
}



gen prob=0

gen problem_selected=""

tokenize q23_2_01_1 q23_2_02_1 q23_2_02_2 q23_2_02_3 q23_2_03_1 q23_2_03_2 q23_2_03_3 q23_2_04_1 q23_2_04_2 q23_2_04_3 q23_2_05_1 q23_2_05_2 q23_2_05_3 q23_2_05_4 q23_2_05_5 q23_2_05_6 q23_2_05_7 q23_2_06_1 q23_2_06_2 q23_2_06_3 q23_2_06_4 q23_2_06_5 q23_2_06_6 q23_2_07_1 q23_2_07_2 q23_2_07_3 q23_2_07_4 q23_2_07_5 q23_2_08_1 q23_2_08_2 q23_2_08_3 q23_2_08_4 q23_2_08_5 q23_2_09_1 q23_2_09_2 q23_2_09_3 q23_2_09_4 q23_2_09_5 q23_2_09_6 q23_2_10_1 q23_2_10_2 q23_2_10_3 q23_2_10_4 q23_2_11_1 q23_2_11_2 q23_2_12_1 q23_2_12_2 q23_2_13_1 q23_2_13_2 q23_2_13_3 q23_2_13_4 q23_2_13_5 q23_2_14_1 q23_2_14_2 q23_2_14_3 q23_2_14_4

forvalues i=1/56 {
	replace problem_selected="``i''" if q23_4_recentissue_selected==`i'
}

foreach v in q23_2_01_1 q23_2_02_1 q23_2_02_2 q23_2_02_3 q23_2_03_1 q23_2_03_2 q23_2_03_3 q23_2_04_1 q23_2_04_2 q23_2_04_3 q23_2_05_1 q23_2_05_2 q23_2_05_3 q23_2_05_4 q23_2_05_5 q23_2_05_6 q23_2_05_7 q23_2_06_1 q23_2_06_2 q23_2_06_3 q23_2_06_4 q23_2_06_5 q23_2_06_6 q23_2_07_1 q23_2_07_2 q23_2_07_3 q23_2_07_4 q23_2_07_5 q23_2_08_1 q23_2_08_2 q23_2_08_3 q23_2_08_4 q23_2_08_5 q23_2_09_1 q23_2_09_2 q23_2_09_3 q23_2_09_4 q23_2_09_5 q23_2_09_6 q23_2_10_1 q23_2_10_2 q23_2_10_3 q23_2_10_4 q23_2_11_1 q23_2_11_2 q23_2_12_1 q23_2_12_2 q23_2_13_1 q23_2_13_2 q23_2_13_3 q23_2_13_4 q23_2_13_5 q23_2_14_1 q23_2_14_2 q23_2_14_3 q23_2_14_4 {
	di as text "`v'"
	replace prob=prob+1 if problem_selected=="`v'" & ``i''!=1
}


forvalues i=1/56 {
	di as text "``i''"
	replace prob=prob+1 if q23_4_recentissue_selected==`i' & ``i''!=1
}

di as result "Testing that the problem selected was mentioned"
inspect prob 
if r(N_pos) > 0 {
	di as error "Number of observations with a problem selected that was not mentioned: " r(N_pos)
}

drop prob



*q23_2_catselected q23_4_recentissue_selected

/*
1.	Land
2.	Neighbours
3.	Housing (if Yes at any of 3,4,5)
4.	Family/relationship
5.	Injury
6.	Citizenship or migration
7.	Government benefits and payments
8.	Public services
9.	Products
10.	Services
11.	Money/debt
12.	Employment
*/





