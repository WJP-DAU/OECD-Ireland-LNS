/*=================================================================================================================
Project:		OECD - LNS Ireland
Routine:		Routing
Author(s):		Natalia Rodriguez 	(nrodriguez@worldjusticeproject.org)
Dependencies:  	World Justice Project
Creation Date:	October, 2025

Description:
Do-file that reviews the routing for all questions.

=================================================================================================================*/

/*=================================================================================================================
					PROBLEM SELECTION
=================================================================================================================*/

cls


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
egen n_problem_cat_1=rowtotal(q23_2_01_1_bin)

*2.	Neighbours
egen n_problem_cat_2=rowtotal(q23_2_02_1_bin q23_2_02_2_bin q23_2_02_3_bin)

*3.	Housing (if Yes at any of 3,4,5)
egen n_problem_cat_3= rowtotal(q23_2_03_1_bin q23_2_03_2_bin q23_2_03_3_bin q23_2_04_1_bin q23_2_04_2_bin q23_2_04_3_bin q23_2_05_1_bin q23_2_05_2_bin q23_2_05_3_bin q23_2_05_4_bin q23_2_05_5_bin q23_2_05_6_bin q23_2_05_7_bin)

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
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_02_`i' if q23_2_02_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)
	
	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_02_`i' if n_problem_cat_2==1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)
	
	* Skip (if they selected the category)
	cap: inspect q23_4_02_`i' if q23_2_catselected!=2
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_02_`i' == . & q23_2_02_`i' == 1 & n_problem_cat_2>1 & q23_2_catselected==2
	di as result "NO-SKIP - obs: " r(N)	
}


*---3.	Housing (if Yes at any of 3,4,5)

*3.	Issues as a social housing or private tenant 

forvalues i=1/3 {
	di as text "`: variable label q23_4_03_`i' '"
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_03_`i' if q23_2_03_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)
	
	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_03_`i' if n_problem_cat_3 == 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Skip (if they selected the category)
	cap: inspect q23_4_03_`i' if q23_2_catselected!= 3
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)
	
	* Non-skip
	qui count if q23_4_03_`i' == . & q23_2_03_`i' == 1 & n_problem_cat_3>1 & q23_2_catselected==3
	di as result "NO-SKIP - obs: " r(N)
}


*4.	Issues as a private landlord

forvalues i=1/3 {
	di as text "`: variable label q23_4_04_`i' '"
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_04_`i' if q23_2_04_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)
	
	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_04_`i' if n_problem_cat_3 == 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)
	
	* Skip (if they selected the category)
	cap: inspect q23_4_04_`i' if q23_2_catselected != 3
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Non-skip
	qui count if q23_4_04_`i' == . & q23_2_04_`i' == 1 & n_problem_cat_3>1 & q23_2_catselected==3
	di as result "NO-SKIP - obs: " r(N)
}


*5.	Other issues with your home or any housing you own or owned

forvalues i=1/7 {
	di as text "`: variable label q23_4_05_`i' '"
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_05_`i' if q23_2_05_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_05_`i' if n_problem_cat_3 == 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	
	
	* Skip (if they selected the category)
	cap: inspect q23_4_05_`i' if q23_2_catselected != 3
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)		
	
	* Non-skip
	qui count if q23_4_05_`i' == . & q23_2_05_`i' == 1 & n_problem_cat_3>1 & q23_2_catselected==3
	di as result "NO-SKIP - obs: " r(N)
	
}

*---4.	Family or relationship issues

forvalues i=1/6 {
	di as text "`: variable label q23_4_06_`i' '"
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_06_`i' if q23_2_06_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)
	
	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_06_`i' if n_problem_cat_4 == 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)
	
	* Skip (if they selected the category)
	cap: inspect q23_4_06_`i' if q23_2_catselected != 4
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	
	
	* Non-skip
	qui count if q23_4_06_`i' == . & q23_2_06_`i' == 1 & n_problem_cat_4>1 & q23_2_catselected==4
	di as result "NO-SKIP - obs: " r(N)
	
}

*---5.	Injury or illness related issues 

forvalues i=1/5 {
	di as text "`: variable label q23_4_08_`i' '"
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_08_`i' if q23_2_08_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_08_`i' if n_problem_cat_5 == 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	
	
	* Skip (if they selected the category)
	cap: inspect q23_4_08_`i' if q23_2_catselected != 5
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	
		
	* Non-skip
	qui count if q23_4_08_`i' == . & q23_2_08_`i' == 1 & n_problem_cat_5>1 & q23_2_catselected==5
	di as result "NO-SKIP - obs: " r(N)
}

*---6.	Citizenship or migration

forvalues i=1/5 {
	di as text "`: variable label q23_4_07_`i' '"
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_07_`i' if q23_2_07_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)
	
	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_07_`i' if n_problem_cat_6 == 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	

	* Skip (if they selected the category)
	cap: inspect q23_4_07_`i' if q23_2_catselected != 6
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	
	
	* Non-skip
	qui count if q23_4_07_`i' == . & q23_2_07_`i' == 1 & n_problem_cat_6>1 & q23_2_catselected==6
	di as result "NO-SKIP - obs: " r(N)	
}


*---7.	Government benefits and payments

forvalues i=1/6 {
	di as text "`: variable label q23_4_09_`i' '"
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_09_`i' if q23_2_09_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_09_`i' if n_problem_cat_7 == 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	

	* Skip (if they selected the category)
	cap: inspect q23_4_09_`i' if q23_2_catselected != 7
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	
	
	* Non-skip
	qui count if q23_4_09_`i' == . & q23_2_09_`i' == 1 & n_problem_cat_7>1 & q23_2_catselected==7
	di as result "NO-SKIP - obs: " r(N)
}


*---8.	Public services

forvalues i=1/4 {
	di as text "`: variable label q23_4_10_`i' '"
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_10_`i' if q23_2_10_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_10_`i' if n_problem_cat_8 == 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	

	* Skip (if they selected the category)
	cap: inspect q23_4_10_`i' if q23_2_catselected != 8
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)		
	
	* Non-skip
	qui count if q23_4_10_`i' == . & q23_2_10_`i' == 1 & n_problem_cat_8>1 & q23_2_catselected==8
	di as result "NO-SKIP - obs: " r(N)
}


*---9.	Products

forvalues i=1/2 {
	di as text "`: variable label q23_4_11_`i' '"
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_11_`i' if q23_2_11_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)
	
	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_11_`i' if n_problem_cat_9 == 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Skip (if they selected the category)
	cap: inspect q23_4_11_`i' if q23_2_catselected != 9
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	
	
	* Non-skip
	qui count if q23_4_11_`i' == . & q23_2_11_`i' == 1 & n_problem_cat_9>1 & q23_2_catselected==9
	di as result "NO-SKIP - obs: " r(N)
}


*---10.	Services

forvalues i=1/2 {
	di as text "`: variable label q23_4_12_`i' '"
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_12_`i' if q23_2_12_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)
	
	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_12_`i' if n_problem_cat_10 == 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	

	* Skip (if they selected the category)
	cap: inspect q23_4_12_`i' if q23_2_catselected != 10
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	
	
	* Non-skip
	qui count if q23_4_12_`i' == . & q23_2_12_`i' == 1 & n_problem_cat_10>1 & q23_2_catselected==10
	di as result "NO-SKIP - obs: " r(N)
}


*---11.	Money/debt

forvalues i=1/5 {
	di as text "`: variable label q23_4_13_`i' '"
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_13_`i' if q23_2_13_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)
	
	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_13_`i' if n_problem_cat_11 == 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	

	* Skip (if they selected the category)
	cap: inspect q23_4_13_`i' if q23_2_catselected != 11
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)		
	
	* Non-skip
	qui count if q23_4_13_`i' == . & q23_2_13_`i' == 1 & n_problem_cat_11>1 & q23_2_catselected==11
	di as result "NO-SKIP - obs: " r(N)
}


*---12.	Employment

forvalues i=1/4 {
	di as text "`: variable label q23_4_14_`i' '"
	
	* Skip (if they mentioned the problem)
	cap: inspect q23_4_14_`i' if q23_2_14_`i' != 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)

	* Skip (if they mentioned more than one problem in the category)
	cap: inspect q23_4_14_`i' if n_problem_cat_12 == 1
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)	
	
	* Skip (if they selected the category)
	cap: inspect q23_4_14_`i' if q23_2_catselected != 12
	di as result "SKIP - obs: " r(N) "; values: " r(N_unique)		
	
	* Non-skip
	qui count if q23_4_14_`i' == . & q23_2_14_`i' == 1 & n_problem_cat_12>1 & q23_2_catselected==12
	di as result "NO-SKIP - obs: " r(N)	
}


*--------  Check that if more than one problem was selected within the category, the problem selected is the second most recent problem mentioned

*Renaming binary problems
tokenize A B C D E F G H I J K L M N
forvalues i=1/9 {
	rename q23_2_0`i'_*_bin AJP_``i''*_bin
}
tokenize A B C D E F G H I J K L M N
forvalues i=10/14 {
	rename q23_2_`i'_*_bin AJP_``i''*_bin
}

gen prob=0

tokenize 01_1 02_1 02_2 02_3 03_1 03_2 03_3 04_1 04_2 04_3 05_1 05_2 05_3 05_4 05_5 05_6 05_7 06_1 06_2 06_3 06_4 06_5 06_6 07_1 07_2 07_3 07_4 07_5 08_1 08_2 08_3 08_4 08_5 09_1 09_2 09_3 09_4 09_5 09_6 10_1 10_2 10_3 10_4 11_1 11_2 12_1 12_2 13_1 13_2 13_3 13_4 13_5 14_1 14_2 14_3 14_4
forvalues i=1/56 {
	di as text "q23_4_``i'' "
	replace prob=prob+1 if q23_4_recentissue_selected==`i' & q23_4_``i''!=2
}

forvalues i=1/12 {
	display as result "q23_2_catselected = `i'"
	replace prob=0 if q23_2_catselected!=`i'
	replace prob=0 if q23_2_catselected==`i' & n_problem_cat_`i'==1
}

di as result "Testing that the problem selected was was the second most recent (for multiple problems mentioned in the category)"
inspect prob 
if r(N_pos) > 0 {
	di as error "Number of observations with a problem selected that was not mentioned: " r(N_pos)
}

drop prob


/*=================================================================================================================
					STATUS
=================================================================================================================*/

*-------- had_dispute -> q24
foreach v in q24 {
	di as result "`: variable label `v' '"

	* Skip
	cap: inspect `v' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
		
	* Non-skip
	qui count if `v' == . & had_dispute!=0
	di as text "NO-SKIP - obs: " as error r(N)
}


*-------- q24 ==1, 4, 5 -> q24_1
foreach v in q24_1_1 q24_1_2 q24_1_3 q24_1_4 q24_1_5 q24_1_6 q24_1_7 q24_1_8 q24_1_9 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect `v' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
	
	* Skip
	di as input "Specific routing check"
	cap: inspect `v' if (q24 != 1 & q24 != 4 & q24 != 5)
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)
	
	* Non-skip
	qui count if `v' == . & (q24 == 1 | q24 == 4 | q24 == 5)
	di as text "NO-SKIP - obs: " as error r(N)
}

*-------- q24 ==2,3 -> q24_3
foreach v in Q24_3_month Q24_3_year  {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect `v' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)		
	
	* Skip
	di as input "Specific routing check"
	cap: inspect `v' if (q24 != 2 & q24 != 3 )
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)
	
	* Non-skip
	qui count if `v' == . & (q24 == 2 | q24 == 3 ) & Q24_3_DKNA!=1
	di as text "NO-SKIP - obs: " as error r(N)	
}
foreach v in Q24_3_DKNA  {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect `v' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)			
	
	* Skip "Specific routing check"
	cap: inspect `v' if (q24 != 2 & q24 != 3 ) & (Q24_3_month!=. & Q24_3_year!=.)
	di as text "SKIP - obs: " as error r(N) as text "; values: " r(N_unique)
	
	* Non-skip
	qui count if `v' == . & (q24 == 2 | q24 == 3 ) & (Q24_3_month==. & Q24_3_year==.)
	di as text "NO-SKIP - obs: " as error r(N)	
}


/*=================================================================================================================
					ASSISTANCE
=================================================================================================================*/


*-------- had_dispute -> q25_1
foreach v in q25_1_01 q25_1_02 q25_1_03 q25_1_04 q25_1_05 q25_1_06 q25_1_07 q25_1_08 q25_1_09 q25_1_10 q25_1_11 q25_1_12 q25_1_13 q25_1_14 q25_1_15 q25_1_16 q25_1_17 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect `v' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
	
	* Non-skip
	qui count if `v' == . & had_dispute!=0
	di as text "NO-SKIP - obs: " as error r(N)
}

*-------- q25_1 == 1 -> q25_7
forvalues i=1/9 {
	display as result "`: variable label q25_7_0`i' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect q25_7_0`i' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
	
	* Skip
	di as input "Specific routing check"
	cap: inspect q25_7_0`i' if q25_1_0`i'!=1
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)
	
	* Non-skip
	qui count if q25_7_0`i' == . & q25_1_0`i'==1
	di as text "NO-SKIP - obs: " as error r(N)
}
forvalues i=10/17 {
	display as result "`: variable label q25_7_`i' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect q25_7_`i' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
	
	* Skip
	di as input "Specific routing check"
	cap: inspect q25_7_`i' if q25_1_`i'!=1
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)
	
	* Non-skip
	qui count if q25_7_`i' == . & q25_1_`i'==1
	di as text "NO-SKIP - obs: " as error r(N)
}


*-------- q25_1 -> q25_3

* Generating binary variables for each adviser
foreach v in q25_1_01 q25_1_02 q25_1_03 q25_1_04 q25_1_05 q25_1_06 q25_1_07 q25_1_08 q25_1_09 q25_1_10 q25_1_11 q25_1_12 q25_1_13 q25_1_14 q25_1_15 q25_1_16 q25_1_17 {
	gen `v'_bin = `v'
	recode `v'_bin (2 =0) (3 = .)
}

*Dummy for assistance (ALL)
egen assistance_1=rowtotal(q25_1_01_bin q25_1_02_bin q25_1_03_bin q25_1_04_bin q25_1_05_bin q25_1_06_bin q25_1_07_bin q25_1_08_bin q25_1_09_bin q25_1_10_bin q25_1_11_bin q25_1_12_bin q25_1_13_bin q25_1_14_bin q25_1_15_bin q25_1_16_bin q25_1_17_bin)

gen assistance=assistance_1>0

*Dummy for professional assistance
egen prof_assistance_1=rowtotal(q25_1_01_bin q25_1_02_bin q25_1_03_bin q25_1_04_bin q25_1_05_bin q25_1_06_bin q25_1_07_bin q25_1_08_bin q25_1_09_bin q25_1_10_bin q25_1_11_bin q25_1_12_bin q25_1_13_bin)

gen prof_assistance=prof_assistance_1>0

*Dummy for non-professional assistance
egen no_prof_assistance_1=rowtotal(q25_1_14_bin q25_1_15_bin q25_1_16_bin q25_1_17_bin)

gen no_prof_assistance=no_prof_assistance_1>0


foreach v in q25_3_01 q25_3_02 q25_3_03 q25_3_04 q25_3_05 q25_3_06 q25_3_07 q25_3_08 q25_3_09 q25_3_10 q25_3_11 q25_3_12 q25_3_13 q25_3_14 q25_3_15 q25_3_16 q25_3_17 q25_3_18 q25_3_19 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect `v' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
	
	* Skip
	di as input "Specific routing check"
	cap: inspect `v' if prof_assistance==1 
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	

	* Non-skip
	di as input "Specific routing check"
	qui count if `v' == . & prof_assistance==0 & had_dispute==1
	di as text "NO-SKIP - obs: " as error r(N)
	
	* Non-skip
	di as input "Specific routing check"
	qui count if `v' == . & assistance==0 & had_dispute==1
	di as text "NO-SKIP - obs: " as error r(N)
}
*tab q25_3_01 no_prof_assistance


*-------- q25_3_04==1 -> q25_4
foreach v in Q25_4 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect `v' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	

	* Skip
	di as input "Specific routing check"
	cap: inspect `v' if q25_3_04 !=1
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
		
	
	* Non-skip
	qui count if `v' == "" & q25_3_04==1
	di as text "NO-SKIP - obs: " as error r(N)
}


/*=================================================================================================================
					NO ACTION TAKEN
=================================================================================================================*/

*-------- q25_1 = none -> q25_5
foreach v in q25_5 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect `v' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
		
	* Skip
	di as input "Specific routing check"
	cap: inspect `v' if assistance ==1
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
		
	* Non-skip
	qui count if `v' == . & assistance==0 & had_dispute==1
	di as text "NO-SKIP - obs: " as error r(N)
}


*-------- q25_5 ==1,2 -> q25_6
foreach v in q25_6_01 q25_6_02 q25_6_03 q25_6_04 q25_6_05 q25_6_06 q25_6_07 q25_6_08 q25_6_09 q25_6_10 q25_6_11 q25_6_12 q25_6_13 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect `v' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
		
	* Skip
	di as input "Specific routing check"
	cap: inspect `v' if q25_5 >2 & q25_5 !=.
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
		
	* Non-skip
	qui count if `v' == . & (q25_5==1 | q25_5==2)
	di as text "NO-SKIP - obs: " as error r(N)
}


/*=================================================================================================================
					RESOLUTION
=================================================================================================================*/

*-------- had_dispute -> q26
foreach v in q26_01 q26_02 q26_03 q26_04 q26_05 q26_06 q26_07 q26_08 q26_09 q26_10 q26_11 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect `v' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
		
	* Non-skip
	qui count if `v' == . & had_dispute==1
	di as text "NO-SKIP - obs: " as error r(N)
}

*Binary DRMs
foreach v in q26_01 q26_02 q26_03 q26_04 q26_05 q26_06 q26_07 q26_08 q26_09 q26_10 q26_11 {
	gen `v'_bin = `v'
	recode `v'_bin (2 = 0) (3 = .)
}

*Dummy for DMRs

egen resolution_1 = rowtotal(q26_01_bin q26_02_bin q26_03_bin q26_04_bin q26_05_bin q26_06_bin q26_07_bin q26_08_bin q26_09_bin q26_10_bin q26_11_bin)
gen resolution=resolution>0

*Dummy for professional DMRs
egen prof_resolution_1 = rowtotal(q26_01_bin q26_02_bin q26_03_bin q26_04_bin q26_05_bin q26_06_bin q26_07_bin q26_08_bin q26_09_bin q26_11_bin)
gen prof_resolution=prof_resolution_1 >0

*-------- q26= none (except 10) -> q27
foreach v in q27_01 q27_02 q27_03 q27_04 q27_05 q27_06 q27_07 q27_08 q27_09 q27_10 q27_11 q27_12 q27_13 q27_14 q27_15 q27_16 q27_17 q27_18 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Specific routing check"
	cap: inspect `v' if prof_resolution==1
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
	
	* Non-skip
	qui count if `v' == . & prof_resolution==0 & had_dispute==1
	di as text "NO-SKIP - obs: " as error r(N)
}

*-------- q24 ==3 -> q27_02
foreach v in q27_02 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Specific routing check"
	cap: inspect `v' if q24!=3 & q24!=. & prof_resolution==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
	replace `v'=. if q24!=3 & q24!=. & prof_resolution==0
	
	* Non-skip
	qui count if `v' == . & q24==3 & prof_resolution==0 & had_dispute==1
	di as text "NO-SKIP - obs: " as error r(N)
}

*-------- Assistance == Yes -> q27_04
foreach v in q27_04 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Specific routing check"
	cap: inspect `v' if assistance==0 & prof_resolution==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
	replace `v'=. if assistance==0 & prof_resolution==0
	
	* Non-skip
	qui count if `v' == . & assistance==1 & prof_resolution==0
	di as text "NO-SKIP - obs: " as error r(N)
}


/*=================================================================================================================
					SATISFACTION
=================================================================================================================*/

*-------- q24 ==2,3  -> q28
foreach v in q28 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Specific routing check"
	cap: inspect `v' if inlist(q24,1,4,5)
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
	
	* Non-skip
	qui count if `v' == . & (q24==2 | q24==3 )
	di as text "NO-SKIP - obs: " as error r(N)
}


*-------- q26 == 1 -> q29_1
forvalues i=1/9 {
	foreach x in a b c d e f g h i {
		di as result "`: variable label q29_1_it0`i'_`x' '"
		
		* Skip
		di as input "Specific routing check"
		cap: inspect q29_1_it0`i'_`x' if q26_0`i'!=1
		di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
		
		* Non-skip
		qui count if q29_1_it0`i'_`x' == . & q26_0`i'==1
		di as text "NO-SKIP - obs: " as error r(N)
	}
}
foreach x in a b c d e f g h i {
		di as result "`: variable label q29_1_it11_`x' '"
		
		* Skip
		di as input "Specific routing check"
		cap: inspect q29_1_it11_`x' if q26_11!=1
		di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
		
		* Non-skip
		qui count if q29_1_it11_`x' == . & q26_11==1
		di as text "NO-SKIP - obs: " as error r(N)
	}

*-------- q26 == 1 -> q29_2
forvalues i=1/9 {
	foreach x in a b c d {
		di as result "`: variable label q29_2_it0`i'_`x' '"
		
		* Skip
		di as input "Specific routing check"
		cap: inspect q29_2_it0`i'_`x' if q26_0`i'!=1
		di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
		
		* Non-skip
		qui count if q29_2_it0`i'_`x' == . & q26_0`i'==1
		di as text "NO-SKIP - obs: " as error r(N)
	}
}
foreach x in a b c d {
		di as result "`: variable label q29_1_it11_`x' '"
		
		* Skip
		di as input "Specific routing check"
		cap: inspect q29_2_it11_`x' if q26_11!=1
		di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
		
		* Non-skip
		qui count if q29_2_it11_`x' == . & q26_11==1
		di as text "NO-SKIP - obs: " as error r(N)
	}


*-------- q24 ==2,3  -> q30
foreach v in q30 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Specific routing check"
	cap: inspect `v' if inlist(q24,1,4,5)
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
	
	* Non-skip
	qui count if `v' == . & (q24==2 | q24==3 )
	di as text "NO-SKIP - obs: " as error r(N)
}	
	
	
*-------- had_dispute -> q31
foreach v in q31_1 q31_2 q31_3 q31_4 q32_1 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect `v' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
	
	* Non-skip
	qui count if `v' == . & had_dispute==1
	di as text "NO-SKIP - obs: " as error r(N)
}	
	
	
/*=================================================================================================================
					IMPACT
=================================================================================================================*/	
	
foreach v in q32_1 q32_01 q32_02 q32_03 q32_04 q32_05 q32_06 q32_07 q32_08 q32_09 q32_10 q32_11 q32_12 q32_13 q32_14 q32_15 q32_16 {
	di as result "`: variable label `v' '"

	* Skip
	di as input "Had dispute check"
	cap: inspect `v' if had_dispute==0
	di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	
	
	* Non-skip
	qui count if `v' == . & had_dispute==1
	di as text "NO-SKIP - obs: " as error r(N)
}	


/*=================================================================================================================
					DEMOGRAPHICS
=================================================================================================================*/		

* Skip
cap: inspect q19_3 if q19_2actual==0 & q19_2actual!=.
di as text "SKIP - obs: " as error r(N) as text "; values: " as error r(N_unique)	

* Non-skip
qui count if q19_3 == . & q19_2actual>=1 
di as text "NO-SKIP - obs: " as error r(N)

	
	
	
	
********************

/*
*Creating the problem selected
gen prob_selected=""
tokenize A1 B1 B2 B3 C1 C2 C3 D1 D2 D3 E1 E2 E3 E4 E5 E6 E7 F1 F2 F3 F4 F5 F6 G1 G2 G3 G4 G5 H1 H2 H3 H4 H5 I1 I2 I3 I4 I5 I6 J1 J2 J3 J4 K1 K2 L1 L2 M1 M2 M3 M4 M5 N1 N2 N3 N4 
forvalues i=1/56 {
	replace prob_selected="``i''" if q23_4_recentissue_selected==`i'
}

*Renaming rankings
tokenize A B C D E F G H I J K L M N
forvalues i=1/9 {
	rename q23_4_0`i'_* AJP_``i''*_freq
}
tokenize A B C D E F G H I J K L M N
forvalues i=10/14 {
	rename q23_4_`i'_* AJP_``i''*_freq
}

tokenize A1 B1 B2 B3 C1 C2 C3 D1 D2 D3 E1 E2 E3 E4 E5 E6 E7 F1 F2 F3 F4 F5 F6 G1 G2 G3 G4 G5 H1 H2 H3 H4 H5 I1 I2 I3 I4 I5 I6 J1 J2 J3 J4 K1 K2 L1 L2 M1 M2 M3 M4 M5 N1 N2 N3 N4 
forvalues i=1/12 {
	display as input "q23_2_catselected `i'"
	forvalues x=1/56 {
		display as text "AJP_``x''"
		count if q23_2_catselected==`i' & n_problem_cat_`i'>1 & prob_selected=="``x''" & AJP_``x''_freq!=2	
	}
	}

count if q23_2_catselected==2 & n_problem_cat_2>1 & q23_4_recentissue_selected==2 & q23_4_02_1!=2	
	
*/

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





