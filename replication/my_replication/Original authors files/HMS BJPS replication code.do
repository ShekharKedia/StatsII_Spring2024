

****************************************************
***** REPLICATION CODE FOR: ************************
***** Personal Economic Shocks and Public **********
***** Opposition to Unauthorized Immigration *******
***** Dan Hopkins, Yotam Margalit, Omer Solodoch ***
****************************************************
*** Note: replicators must install the estout and coefplot packages  

clear all
*local directory "ISCAP"
use "data\HMS BJPS replication data.dta"

***************************
***** Table 1 *************
***************************
** Oct '12 - Jan '13
***************************
sum ppeducat_4 female6 black6 hisp6 white6 union0 pre_party1 AGE INCOME if wave==6 & dep_d <. & pre_party1<., sep(100)
***************************
** Oct 2020 
***************************
sum ppeducat_4 female6 black6 hisp6 white6 union0 pre_party1 AGE INCOME if wave==15 & dep_d <. & pre_party1<., sep(100)


***************************
***** Table 2 *************
***************************
foreach w of numlist 7 8 10 11 13 14 15 { 
areg dep_d UNEMPLOYED RETIRED DISABLED OTHER_EMP i.ppeducat union0 white6 male AGE pre_party1 INCOME  if wave==`w', absorb(state) cl(state)
}
areg dep_d UNEMPLOYED RETIRED DISABLED OTHER_EMP i.ppeducat union0 white6 male AGE pre_party1 INCOME, absorb(state) cl(state)
gen samp1011=e(sample)

areg dep_d UNEMPLOYED RETIRED DISABLED OTHER_EMP i.ppeducat union0 white6 male AGE pre_party1 INCOME i.Year if samp1011==1, absorb(mno) cl(state)


***************************
***** Table 3 *************
***************************
label var LOST_JOB "Lost job"
label var income_shock2 "Income drop"
label var FOUND_JOB "Found job"

xtreg dep_d LOST_JOB income_shock2 FOUND_JOB logINCOME RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(state)
gen samp=e(sample)

estimates clear
eststo: xtreg dep_d LOST_JOB  RETIRED DISABLED OTHER_EMP i.ppeducat logINCOME if samp==1, cl(state)
estadd local indfe "no", replace
estadd local yearfe "no", replace
eststo: xtreg dep_d LOST_JOB  RETIRED DISABLED OTHER_EMP i.ppeducat logINCOME if samp==1, fe cl(state)
estadd local indfe "yes", replace
estadd local yearfe "no", replace
eststo: xtreg dep_d LOST_JOB  RETIRED DISABLED OTHER_EMP i.ppeducat logINCOME i.Year if samp==1, fe cl(state)
estadd local indfe "yes", replace
estadd local yearfe "yes", replace
eststo: xtreg dep_d LOST_JOB income_shock2 RETIRED DISABLED OTHER_EMP i.ppeducat logINCOME i.Year if samp==1, fe cl(state)
estadd local indfe "yes", replace
estadd local yearfe "yes", replace
eststo: xtreg dep_d LOST_JOB income_shock2 FOUND_JOB logINCOME RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(state)
estadd local indfe "yes", replace
estadd local yearfe "yes", replace

esttab est1 est2 est3 est4 est5,  ///
 keep(LOST_JOB income_shock2 FOUND_JOB)  ///
 order(LOST_JOB income_shock2 FOUND_JOB) ///
 se(3) b(3) replace star(* 0.10 ** 0.05) booktabs label  nonotes nogaps nodepvars   ///
  mtitles("" "" "" "" "" "")    ///
 s(indfe yearfe N r2, fmt(0 0 0 3) label("Individual FE" "Year FE" "Observations" "R-squared")) 


***************************
***** Table 4 *************
***************************
local inter low_educ union0 white6 male  copcimm10M Unemployment_rateM
local cov pre_pid7 AGE  INCOME FOUND_JOB RETIRED DISABLED OTHER_EMP  i.Year

areg dep_d LOST_JOB `inter' `cov', absorb(countyfips) cl(mno)
areg dep_d LOST_JOB##low_educ `inter' `cov', absorb(countyfips) cl(mno)
areg dep_d LOST_JOB##union0 `inter' `cov', absorb(countyfips) cl(mno)
areg dep_d LOST_JOB##copcimm10M `inter' `cov', absorb(countyfips) cl(mno)
areg dep_d LOST_JOB##Unemployment_rateM `inter' `cov', absorb(countyfips) cl(mno)
areg dep_d LOST_JOB##white6 `inter' `cov', absorb(countyfips) cl(mno)
areg dep_d LOST_JOB##male `inter' `cov', absorb(countyfips) cl(mno)

***************************
***** Figure 1 ************
***************************
set scheme sj
grstyle init
grstyle set plain, horizontal grid

lab var income_shock2 "Income drop"
lab var income_shock_l2 "Income drop t+1" 
lab var income_shock_l3 "Income drop t+2" 
lab var income_shock_l4 "Income drop t+3" 
 
areg dep_d income_shock2 income_shock_l2 income_shock_l3 income_shock_l4 long_unemp RETIRED DISABLED OTHER_EMP FOUND_JOB union0 black6 hisp6 AGE male i.pre_party  i.ppeducat i.Year, absorb(state) cl(state)
***** panel (a) **********
coefplot, keep(income_shock2 income_shock_l2 income_shock_l3 income_shock_l4) yline(0, lwidth(thin) lpattern(dash)) ///
msymbol(O) mcolor(maroon) msize(large) ciopts(lcolor(maroon maroon)) ///
yline(-0.2 -0.1 0.1 0.2 0.3 0.4, lwidth(thin) lcolor(gs8%30)) ///
xline(1 1.5 2 2.5 3 3.5 4, lwidth(thin) lcolor(gs8%30)) ///
ysc(r(-0.2 0.4)) ylabel(-0.2(0.1)0.4) vertical ///
levels(95 90) ytitle(Effect of income drop on Pr(support for deportation))  

lab var LOST_JOB "Lost job"
lab var LOST_JOB2 "Lost job t+1" 
lab var LOST_JOB3 "Lost job t+2" 
lab var LOST_JOB4 "Lost job t+3" 

areg dep_d LOST_JOB LOST_JOB2 LOST_JOB3 LOST_JOB4 long_unemp RETIRED DISABLED OTHER_EMP FOUND_JOB union0 black6 hisp6 AGE male i.pre_party  INCOME i.ppeducat i.Year, absorb(state) cl(state)
***** panel (b) **********
coefplot, keep(LOST_JOB LOST_JOB2 LOST_JOB3 LOST_JOB4) yline(0, lwidth(thin) lpattern(dash)) ///
msymbol(O) mcolor(maroon) msize(large) ciopts(lcolor(maroon maroon)) ///
yline(-0.2 -0.1 0.1 0.2 0.3 0.4, lwidth(thin) lcolor(gs8%30)) ///
ysc(r(-0.2 0.4)) ylabel(-0.2(0.1)0.4) vertical ///
xline(1 1.5 2 2.5 3 3.5 4, lwidth(thin) lcolor(gs8%30)) ///
levels(95 90) ytitle(Effect of job loss on Pr(support for deportation))  



*************************************
*** online appendix
*************************************

*************************************
*** Table SM-2
*************************************
sum path dep_d UNEMPLOYED LOST_JOB FOUND_JOB income_shock2 ///
reg_unemp_shock_20 INCOME Unemployment_rate_  RETIRED union0 ///
ppeducat_1 ppeducat_2 ppeducat_3 ppeducat_4 female6 AGE ///
white6 black6 hisp6 if dep_d !=. &  LOST_JOB!=.& ppeducat !=. & samp==1, sep(100)

*************************************
*** Table SM-3
*************************************
mean LOST_JOB income_shock2  if samp==1
mean LOST_JOB income_shock2  if samp==1 & white==0 & male==0
mean LOST_JOB income_shock2  if samp==1 & white==0 & male==1
mean LOST_JOB income_shock2  if samp==1 & white==1 & male==0
mean LOST_JOB income_shock2  if samp==1 & white==1 & male==1



*************************************
*** Table SM-4 attrition
*************************************
estimates clear
eststo: xtreg dropout lag_dep_d lag_LOST_JOB lag_income_shock2 lag_loweduc lag_logINCOME lag_retired lag_disabled lag_other_emps i.Year, fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg dropout lag_dep_d lag_LOST_JOB lag_dv_LOSTJOB lag_income_shock2 lag_loweduc lag_logINCOME lag_retired lag_disabled lag_other_emps i.Year, fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg dropout lag_dep_d lag_LOST_JOB lag_income_shock2 lag_dv_incomeshock2 lag_loweduc lag_logINCOME lag_retired lag_disabled lag_other_emps i.Year, fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg dropout lag_dep_d lag_LOST_JOB lag_income_shock2 lag_loweduc lag_dv_lowed lag_logINCOME lag_retired lag_disabled lag_other_emps i.Year, fe cl(mno)
estadd local fixedef "yes", replace


esttab est1 est2 est3 est4,  ///
 drop(*Year)  ///
 order(lag_dep_d lag_LOST_JOB lag_income_shock2 lag_loweduc lag_dv_LOSTJOB lag_dv_incomeshock2 lag_dv_lowed lag_logINCOME lag_retired lag_disabled lag_other_emps) ///
 se(3) b(3) replace star(* 0.10 ** 0.05) label booktabs compress nonotes nogaps nodepvars mtitles("" "" "" "") ///
 s(fixedef N r2, fmt(0 0 3) label("Individual-year FE" "Observations" "R-squared"))
***************************************************************

*************************************
*** Table SM-5 (Table 2, full specification)
*************************************
foreach w of numlist 7 8 10 11 13 14 15 { 
areg dep_d UNEMPLOYED RETIRED DISABLED OTHER_EMP i.ppeducat union0 white6 male AGE pre_party1 INCOME  if wave==`w', absorb(state) cl(state)
}
areg dep_d UNEMPLOYED RETIRED DISABLED OTHER_EMP i.ppeducat union0 white6 male AGE pre_party1 INCOME, absorb(state) cl(state)

areg dep_d UNEMPLOYED RETIRED DISABLED OTHER_EMP i.ppeducat union0 white6 male AGE pre_party1 INCOME i.Year if samp1011==1, absorb(mno) cl(state)

*************************************
*** Table SM-6 logit/oprobit
*************************************
oprobit path LOST_JOB income_shock2  logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat union0 white6 male if samp==1
logit dep_d LOST_JOB income_shock2  logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat union0 white6 male if samp==1

*************************************
*** Table SM-7 drop "don't know" responses
*************************************
** col. 6
xtreg deport_d  LOST_JOB income_shock2  logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(state)
gen sample_dk=e(sample)
** return to cols. 1-5
xtreg deport_d LOST_JOB  RETIRED DISABLED OTHER_EMP i.ppeducat if sample_dk==1, cl(state)
xtreg deport_d LOST_JOB  RETIRED DISABLED OTHER_EMP i.ppeducat if sample_dk==1, fe cl(state)
xtreg deport_d LOST_JOB  RETIRED DISABLED OTHER_EMP i.ppeducat i.Year if sample_dk==1, fe cl(state)
xtreg deport_d LOST_JOB income_shock2 RETIRED DISABLED OTHER_EMP i.ppeducat i.Year if sample_dk==1, fe cl(state)
xtreg deport_d LOST_JOB income_shock2  logINCOME RETIRED DISABLED OTHER_EMP i.ppeducat i.Year if sample_dk==1, fe cl(state)
** col. 7
xtreg dep_d  LOST_JOB income_shock2  logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(state)

*************************************
*** Table SM-8 alternative measures for an income drop
*************************************
lab var income_shock "Income drop: 1 category"
lab var income_shock2 "Income drop: 2 categories"
lab var income_shock3 "Income drop: 3 categories"
lab var income_shock4 "Income drop: 4 categories"

sum income_shock income_shock2 income_shock3 income_shock4 if samp ==1
estimates clear
eststo: xtreg dep_d LOST_JOB income_shock   logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(state)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB income_shock2   logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(state)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB income_shock3   logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(state)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB income_shock4   logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(state)
estadd local fixedef "yes", replace

esttab est1 est2 est3 est4,  ///
 keep(LOST_JOB income_shock income_shock2 income_shock3 income_shock4)  ///
 order(LOST_JOB income_shock income_shock2 income_shock3 income_shock4) ///
 se(3) b(3) replace star(* 0.10 ** 0.05) label booktabs compress nonotes nogaps nodepvars mtitles("" "" "" "") ///
 s(fixedef  N r2, fmt(0 0 3) label("Individual-year FE" "Observations" "R-squared"))

*************************************
*** Table SM-9 Effect heterogeneity controlling for individual fixed effects
*************************************
estimates clear
eststo: xtreg dep_d LOST_JOB income_shock2 logINCOME  FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB LOST_JOB_lskilled income_shock2 logINCOME  FOUND_JOB RETIRED DISABLED OTHER_EMP i.Year, fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB LOST_JOB_union income_shock2 logINCOME  FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB LOST_JOB_copcimm10M income_shock2 logINCOME  FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(countyfips)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB LOST_JOB_unemprateM income_shock2 logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(countyfips)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB LOST_JOB_white income_shock2 logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB LOST_JOB_male income_shock2 logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB LOST_JOB_white_male income_shock2 logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(mno)
estadd local fixedef "yes", replace


esttab est1 est2 est3 est4 est5 est6 est7 est8,  ///
 keep(LOST_JOB LOST_JOB_lskilled LOST_JOB_union LOST_JOB_copcimm10M LOST_JOB_unemprateM LOST_JOB_white LOST_JOB_male LOST_JOB_white_male)  ///
 order(LOST_JOB LOST_JOB_lskilled LOST_JOB_union LOST_JOB_copcimm10M LOST_JOB_unemprateM LOST_JOB_white LOST_JOB_male LOST_JOB_white_male) ///
 se(3) b(3) replace star(* 0.10 ** 0.05) label booktabs compress nonotes nogaps nodepvars mtitles("" "" "" "" "" "" "" "") ///
 s(fixedef N r2, fmt(0 0 3) label("Individual-year FE" "Observations" "R-squared"))
 
*************************************
*** Table SM-10  Interactions - robustness
*************************************
local inter low_educ union0 white6 male Unemployment_rateM
local cov pre_pid7 AGE  INCOME FOUND_JOB RETIRED DISABLED OTHER_EMP  i.Year

areg dep_d LOST_JOB `inter' copcimm10M `cov', absorb(countyfips) cl(mno)
foreach immvar in copcimm10M copcimm10Q4 copcimm00M copcimm00Q4 copcimm0010M copcimm0010Q4 {
areg dep_d LOST_JOB##`immvar' `inter' `cov', absorb(countyfips) cl(mno)
}
areg dep_d LOST_JOB##c.copcimm10 `inter' `cov', absorb(countyfips) cl(mno)

local inter low_educ union0 white6 male copcimm10M
local cov pre_pid7 AGE  INCOME FOUND_JOB RETIRED DISABLED OTHER_EMP  i.Year
foreach unempvar in Unemployment_rateQ4  {
areg dep_d LOST_JOB##`unempvar' `inter' `cov', absorb(countyfips) cl(mno)
}

local inter low_educ union0 white6 male copcimm10M
local cov pre_pid7 AGE  INCOME FOUND_JOB RETIRED DISABLED OTHER_EMP  i.Year
areg dep_d LOST_JOB##c.Unemployment_rate_ `inter' `cov', absorb(countyfips) cl(mno)
areg dep_d LOST_JOB##c.l_Unemployment_rate_ `inter' `cov', absorb(countyfips) cl(mno)

*************************************
*** Table SM-11   controlling for county-level exposure to trade shocks
*************************************

local inter low_educ union0 white6 male  copcimm10M Unemployment_rateM
local cov pre_pid7 AGE  INCOME FOUND_JOB RETIRED DISABLED OTHER_EMP l_shind_manuf_cbp l_task_outsource d_imp_usch_pd_0008 i.Year 

estimates clear
eststo: areg dep_d LOST_JOB `inter' `cov', absorb(countyfips) cl(mno)
estadd local fixedef "yes", replace
eststo: areg dep_d LOST_JOB LOST_JOB_lskilled `inter' `cov', absorb(countyfips) cl(mno)
estadd local fixedef "yes", replace
eststo: areg dep_d LOST_JOB LOST_JOB_union `inter' `cov', absorb(countyfips) cl(mno)
estadd local fixedef "yes", replace
eststo: areg dep_d LOST_JOB LOST_JOB_copcimm10M `inter' `cov', absorb(countyfips) cl(cty_fips)
estadd local fixedef "yes", replace
eststo: areg dep_d LOST_JOB LOST_JOB_unemprateM `inter' `cov', absorb(countyfips) cl(cty_fips)
estadd local fixedef "yes", replace
eststo: areg dep_d LOST_JOB LOST_JOB_white `inter' `cov', absorb(countyfips) cl(mno)
estadd local fixedef "yes", replace
eststo: areg dep_d LOST_JOB LOST_JOB_male `inter' `cov', absorb(countyfips) cl(mno)
estadd local fixedef "yes", replace

** export full table
esttab est1 est2 est3 est4 est5 est6 est7,  ///
 keep(LOST_JOB LOST_JOB_lskilled LOST_JOB_union LOST_JOB_copcimm10M LOST_JOB_unemprateM LOST_JOB_white LOST_JOB_male l_shind_manuf_cbp l_task_outsource d_imp_usch_pd_0008)  ///
 order(LOST_JOB LOST_JOB_lskilled LOST_JOB_union LOST_JOB_copcimm10M LOST_JOB_unemprateM LOST_JOB_white LOST_JOB_male l_shind_manuf_cbp l_task_outsource d_imp_usch_pd_0008) ///
 se(3) b(3) replace star(* 0.10 ** 0.05) label booktabs compress nonotes nogaps nodepvars mtitles("" "" "" "" "" "" "") ///
 s(fixedef N r2, fmt(0 0 3) label("County-year FE" "Observations" "R-squared"))

 

*************************************
*** Table SM-12   trade exposure interactions
*************************************
local inter low_educ union0 white6 male  copcimm10M Unemployment_rateM l_shind_manuf_cbp l_task_outsource
local cov pre_pid7 AGE  INCOME FOUND_JOB RETIRED DISABLED OTHER_EMP  i.Year

estimates clear
eststo: areg dep_d LOST_JOB LOST_JOB_manufact `inter' `cov' if white_male==1, absorb(cty_fips) cl(cty_fips)
estadd local fixedef "yes", replace
estadd local indfe "no", replace
eststo: areg dep_d LOST_JOB LOST_JOB_China `inter' `cov' if white_male==1, absorb(cty_fips) cl(cty_fips)
estadd local fixedef "yes", replace
estadd local indfe "no", replace
eststo: areg dep_d LOST_JOB LOST_JOB_offshore `inter' `cov' if white_male==1, absorb(cty_fips) cl(cty_fips)
estadd local fixedef "yes", replace
estadd local indfe "no", replace
eststo: areg dep_d LOST_JOB LOST_JOB_manufact `inter' `cov' if white_male==1, absorb(mno) cl(cty_fips)
estadd local fixedef "no", replace
estadd local indfe "yes", replace
eststo: areg dep_d LOST_JOB LOST_JOB_China `inter' `cov' if white_male==1, absorb(mno) cl(cty_fips)
estadd local fixedef "no", replace
estadd local indfe "yes", replace
eststo: areg dep_d LOST_JOB LOST_JOB_offshore `inter' `cov' if white_male==1, absorb(mno) cl(cty_fips)
estadd local fixedef "no", replace
estadd local indfe "yes", replace

** export full table
esttab est1 est2 est3 est4 est5 est6,  ///
 keep(LOST_JOB LOST_JOB_manufact LOST_JOB_China LOST_JOB_offshore)  ///
 order(LOST_JOB LOST_JOB_manufact LOST_JOB_China LOST_JOB_offshore) ///
 se(3) b(3) replace star(* 0.10 ** 0.05) label booktabs compress nonotes nogaps nodepvars mtitles("" "" "" "" "" "") ///
 s(fixedef indfe N r2, fmt(0 0 0 3) label("County-year FE" "Individual-year FE" "Observations" "R-squared"))


*************************************
*** Figure SM-1   
*************************************
xtreg dep_d LOST_JOB FOUND_JOB RETIRED DISABLED OTHER_EMP income_shock2  logINCOME i.ppeducat i.Year, fe cl(state)
est store jobloss_all
xtreg dep_d LOST_JOB FOUND_JOB RETIRED DISABLED OTHER_EMP income_shock2  logINCOME i.ppeducat i.Year if white6==0, fe cl(state)
est store jobloss_nonwht
xtreg dep_d LOST_JOB FOUND_JOB RETIRED DISABLED OTHER_EMP income_shock2  logINCOME i.ppeducat i.Year if white6==1, fe cl(state)
est store jobloss_wht
xtreg dep_d LOST_JOB FOUND_JOB RETIRED DISABLED OTHER_EMP income_shock2  logINCOME i.ppeducat i.Year if male==0, fe cl(state)
est store jobloss_fm
xtreg dep_d LOST_JOB FOUND_JOB RETIRED DISABLED OTHER_EMP income_shock2  logINCOME i.ppeducat i.Year if male==1, fe cl(state)
est store jobloss_ml
xtreg dep_d LOST_JOB FOUND_JOB RETIRED DISABLED OTHER_EMP income_shock2  logINCOME i.ppeducat i.Year if white_male ==0, fe cl(state)
est store jobloss_nonwhtfm 
xtreg dep_d LOST_JOB FOUND_JOB RETIRED DISABLED OTHER_EMP income_shock2  logINCOME i.ppeducat i.Year if white_male ==1, fe cl(state)
est store jobloss_whtml 


xtreg dep_d LOST_JOB FOUND_JOB RETIRED DISABLED OTHER_EMP income_shock2  logINCOME i.ppeducat i.Year if white6==0 & male==1, fe cl(state)
est store jobloss_nonwhtml
xtreg dep_d LOST_JOB FOUND_JOB RETIRED DISABLED OTHER_EMP income_shock2  logINCOME i.ppeducat i.Year if white6==1 & male==0, fe cl(state)
est store jobloss_whtfml
xtreg dep_d LOST_JOB FOUND_JOB RETIRED DISABLED OTHER_EMP income_shock2  logINCOME i.ppeducat i.Year if white6==0 & male==0, fe cl(state)
est store jobloss_nonwhtfml

**panel (a)
coefplot (jobloss_wht, offset(0.2) label(White) msymbol(S) mcolor(maroon) mfcolor(white) ciopts(lcolor(maroon maroon))) (jobloss_nonwht, offset(0.15) label(Non-white) msymbol(S) mcolor(gs8) mfcolor(white) ciopts(lcolor(gs8 gs8))) ///
(jobloss_ml, offset(0) label(Male) msymbol(D) mcolor(maroon) mfcolor(white) ciopts(lcolor(maroon maroon))) (jobloss_fm, offset(-0.05) label(Female) msymbol(D) mcolor(gs8) mfcolor(white) ciopts(lcolor(gs8 gs8))) (jobloss_whtml, offset(-0.2) label(White male) msymbol(O) mcolor(maroon) mfcolor(white) ciopts(lcolor(maroon maroon))) (jobloss_nonwhtfm, offset(-0.25) label(Non white-male) msymbol(O)  mcolor(gs8) mfcolor(white) ciopts(lcolor(gs8 gs8))) (jobloss_all, offset(0.3) label(Full sample) msymbol(O) mcolor(black) mfcolor(white) ciopts(lcolor(black black))), ///
keep(LOST_JOB) xline(0, lwidth(thin) lpattern(dash)) ///
xline(-0.3 -0.2 -0.1 0.1 0.2 0.3 0.4, lwidth(thin) lcolor(gs8%30)) ///
xsc(r(-0.3 0.4)) xlabel(-0.3(0.1)0.4) ///
levels(95 90) xtitle(Effect of job loss on Pr(support for deportation))  ///
legend(nobox region(lstyle(none)) rows(2))

**panel (b)
coefplot (jobloss_all, offset(0.3) label(Full sample) mcolor(black) mfcolor(white) ciopts(lcolor(black black))) (jobloss_whtml, offset(0.2) label(White males) msymbol(O) mcolor(maroon) mfcolor(white) ciopts(lcolor(maroon maroon))) (jobloss_whtfml, offset(0.1) label(White females) msymbol(D) mcolor(maroon) mfcolor(white) ciopts(lcolor(maroon maroon))) (jobloss_nonwhtml, offset(0) label(Non-white males) msymbol(S) mcolor(maroon) mfcolor(white) ciopts(lcolor(maroon maroon))) (jobloss_nonwhtfml, offset(-0.1) label(Non-white females) msymbol(T) mcolor(maroon) mfcolor(white) ciopts(lcolor(maroon maroon))), ///
keep(LOST_JOB) xline(0, lwidth(thin) lpattern(dash)) ///
xline(-0.3 -0.2 -0.1 0.1 0.2 0.3 0.4, lwidth(thin) lcolor(gs8%30)) ///
xsc(r(-0.3 0.4)) xlabel(-0.3(0.1)0.4) ///
levels(95 90) xtitle(Effect of job loss on Pr(support for deportation))  ///
legend(nobox region(lstyle(none)) rows(2))


*************************************
*** Table SM-14 - effect endurance, FE & RE models
*************************************
xtreg dep_d LOST_JOB  RETIRED DISABLED OTHER_EMP  i.ppeducat, fe cl(state)
xtreg dep_d LOST_JOB LOST_JOB2 RETIRED DISABLED OTHER_EMP  i.ppeducat, fe cl(state)

xtreg dep_d LOST_JOB  RETIRED DISABLED OTHER_EMP  i.ppeducat, re cl(state)
xtreg dep_d LOST_JOB LOST_JOB2 RETIRED DISABLED OTHER_EMP  i.ppeducat, re cl(state)


*************************************
*** Table SM-15 - border fence item
*************************************
estimates clear
eststo: xtreg buildwall_for LOST_JOB income_shock2   FOUND_JOB RETIRED DISABLED OTHER_EMP i.Year if white6<., fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg buildwall_for LOST_JOB income_shock2   FOUND_JOB RETIRED DISABLED OTHER_EMP i.Year if white6==1, fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg buildwall_for LOST_JOB LOST_JOB_male income_shock2 FOUND_JOB RETIRED DISABLED OTHER_EMP i.Year if white6==1, fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg buildwall_for LOST_JOB LOST_JOB_male income_shock2 income_shock2_male FOUND_JOB RETIRED DISABLED OTHER_EMP i.Year if white6==1, fe cl(mno)
estadd local fixedef "yes", replace

esttab est1 est2 est3 est4,  ///
 keep(LOST_JOB income_shock2 LOST_JOB_male income_shock2_male)  ///
 order(LOST_JOB income_shock2 LOST_JOB_male income_shock2_male) ///
 se(3) b(3) replace star(* 0.10 ** 0.05) label booktabs compress nonotes nogaps nodepvars mtitles("Full sample" "Whites" "Whites" "Whites") ///
 s(fixedef N r2, fmt(0 0 3) label("Individual-year FE" "Observations" "R-squared"))

*************************************
*** Table SM-16 - anti-Latino Prejudice
*************************************
lab var sthrdhis_cold "Negative stereotypes toward Hispanics"
lab var sthrdwht_warm "Positive stereotypes toward Whites"
lab var sthrdblk_cold "Negative stereotypes toward Blacks"
lab var sthrdwhthis_cold "Explicit Anti−Latino Prejudice"
lab var sthrdwhtblk_cold "Explicit Anti-Black Prejudice"
lab var whprejudice_relative "Relative Anti−Latino Prejudice"

lab var FOUND_JOB "Found job"
lab var RETIRED "Retired"
lab var DISABLED "Disabled"
lab var OTHER_EMP "Other employment status"

estimates clear
eststo: xtreg whprejudice_relative  LOST_JOB income_shock2 FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat  if white6==1 & sthrdwhthis_cold<., fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg whprejudice_relative  LOST_JOB LOST_JOB_male income_shock2 FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat  if white6==1 & sthrdwhthis_cold<., fe cl(mno)
estadd local fixedef "yes", replace

eststo: xtreg sthrdwhthis_cold  LOST_JOB income_shock2 FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat  if white6==1, fe cl(mno)
estadd local fixedef "yes", replace
eststo: xtreg sthrdwhthis_cold  LOST_JOB LOST_JOB_male income_shock2 FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat  if white6==1, fe cl(mno)
estadd local fixedef "yes", replace

esttab est1 est2 est3 est4,   ///
 keep(LOST_JOB LOST_JOB_male income_shock2 FOUND_JOB RETIRED DISABLED OTHER_EMP *ppeducat)  ///
 order(LOST_JOB LOST_JOB_male income_shock2 FOUND_JOB RETIRED DISABLED OTHER_EMP *ppeducat) ///
 se(3) b(3) replace star(* 0.10 ** 0.05) label booktabs compress nonotes nogaps nodepvars mtitles("Relative Anti−Latino Prejudice" "" "Explicit Anti−Latino Prejudice" "") ///
 s(fixedef N r2, fmt(0 0 3) label("Individual FE" "Observations" "R-squared"))

xtreg sthrdwhthis_cold  LOST_JOB LOST_JOB_male income_shock2 FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat  if white6==1, fe cl(mno)
**white males:
lincom LOST_JOB + LOST_JOB_male 
**white females:
lincom LOST_JOB  

*************************************
*** Table SM-17 - post-2016
*************************************
estimates clear
eststo: xtreg dep_d LOST_JOB income_shock2 logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat, fe cl(state)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB income_shock2 logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat i.Year, fe cl(state)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB post2016 income_shock2 logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat, fe cl(state)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB LOST_JOB_post2016 post2016 income_shock2 logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat, fe cl(state)
estadd local fixedef "yes", replace
eststo: xtreg dep_d LOST_JOB income_shock2 income_shock2_post2016 post2016 logINCOME FOUND_JOB RETIRED DISABLED OTHER_EMP i.ppeducat, fe cl(state)
estadd local fixedef "yes", replace

esttab est1 est2 est3 est4 est5,  ///
 keep(LOST_JOB income_shock2 LOST_JOB_post2016 income_shock2_post2016 post2016 *Year)  ///
 order(LOST_JOB income_shock2 LOST_JOB_post2016 income_shock2_post2016 post2016 *Year) ///
 se(3) b(3) replace star(* 0.10 ** 0.05) label booktabs compress nonotes nogaps nodepvars mtitles("" "" "" "" "") ///
 s(fixedef N r2, fmt(0 0 3) label("Individual FE" "Observations" "R-squared"))







