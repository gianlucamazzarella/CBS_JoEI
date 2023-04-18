*===================================================================
*				4 - MAIN ESTIMATES
*===================================================================
/* In this file: we produce all the main estimates, and we store them
	in an excel file ready to be exported. 
	The main estimates include:
 1. Table 1
 2. Figure 2
 3. Figure 3
 4. Table A.3
 5. Table A.4
 6. Table A.5
 7. Table A.6
 */
*===================================================================

* ==============================================================================
* 1 - IGE income 
* ==============================================================================

*-------------------------------------------------------------------------------
* 1.1 - By year -- Figure 2(b) and Table A.5
*------------------------------------------------------------------------------

use ${our_dir}/data/pop_ginc, clear

rename female_s female
	
local start = 1970
local N = 1989 - `start' + 1
	

matrix res = J(`N',7,.)

local r = 0
set seed 123456
qui forvalues y = `start'(1)1989 {
	noi: di `y'
	local r = `r' + 1
	matrix res[`r',1] = `y'
	
	bootstrap r(rho), reps(200): corr ginc_3y_s ginc_3y_p if female == 0 & yob_s == `y' & ginc_3y_s!=. & ginc_3y_p!=.
	matrix res[`r',2] = _b[_bs_1]
	matrix res[`r',3] = _se[_bs_1]
	matrix res[`r',4] = e(N)
		
	bootstrap r(rho), reps(200): corr ginc_3y_s ginc_3y_m if female == 1 & yob_s == `y' & ginc_3y_s!=. & ginc_3y_m!=.
	matrix res[`r',5] = _b[_bs_1]
	matrix res[`r',6] = _se[_bs_1]
	matrix res[`r',7] = e(N)
		
}

**** exporting
preserve
	svmat res
	keep res*
	drop if res1 == .
	rename res1 year
	rename res2 beta_pat
	rename res3 se_pat
	rename res4 num_pat

	rename res5 beta_mat
	rename res6 se_mat
	rename res7 num_mat

	compress
	save "${our_dir}/output/ige_by_yob_ginc", replace
restore
*-------------------------------------------------------------------------------
* 1.2 - Pooling -- Table 1 - Rows 3 and 4 + Table A.3
*------------------------------------------------------------------------------

use ${our_dir}/data/pop_ginc, clear

keep if yob_s >= 1970
keep rinc* ginc* full_ginc* female_s

matrix ginc3y= J(1,6,.)
matrix ginc5y= J(1,6,.)
matrix rinc3y= J(1,6,.)
matrix rinc5y= J(1,6,.)
matrix full_ginc3y= J(1,6,.)
matrix full_ginc5y= J(1,6,.)

foreach k in ginc rinc full_ginc{
	foreach j in 3 5 {
		set seed 123456
		bootstrap r(rho), reps(200): corr `k'_`j'y_s `k'_`j'y_p if female_s==0 & `k'_`j'y_s!=. & `k'_`j'y_p!=.
		matrix `k'`j'y[1,1] = _b[_bs_1]
		matrix `k'`j'y[1,2] = _se[_bs_1]
		matrix `k'`j'y[1,3] = e(N)

		bootstrap r(rho), reps(200): corr `k'_`j'y_s `k'_`j'y_m if female_s==1 & `k'_`j'y_s!=. & `k'_`j'y_m!=.
		matrix `k'`j'y[1,4] = _b[_bs_1]
		matrix `k'`j'y[1,5] = _se[_bs_1]
		matrix `k'`j'y[1,6] = e(N)
	}
	
}

**** exporting
local i = 0
tempfile toappend
foreach k in ginc rinc full_ginc {
	foreach j in 3 5 {
		local i = `i' + 1
		clear 

		svmat `k'`j'y

		rename `k'`j'y1 corr_pat
		rename `k'`j'y2 se_pat
		rename `k'`j'y3 num_pat

		rename `k'`j'y4 corr_mat
		rename `k'`j'y5 se_mat
		rename `k'`j'y6 num_mat
		
		ge estimate = "`k'`j'y"
		order estimate
		ge i = `i'
		
		if (`i'>1) {
			append using `toappend'
		}
		save `toappend',replace

	}
}
use `toappend',clear
sort i
drop i
save "${our_dir}/output/ginc_pool", replace

* ==============================================================================
* 2 - IGE education
* ==============================================================================

*-------------------------------------------------------------------------------
* 2.1 - By year -- Figure 2(a) and Table A.4
*------------------------------------------------------------------------------

use ${our_dir}/data/pop_edu, clear

keep if yob_s >= 1950
gen max_school_p = max(school_years_p,school_years_m)
keep school_years_s school_years_m school_years_p max_school_p female yob_s

**** Father and Mother education
local start = 1950
local N = 1989 - `start' + 1
	
matrix res = J(`N',7,.)
local r = 0
qui forvalues y = `start'(1)1989 {
	local r = `r' + 1
	matrix res[`r',1] = `y'
	bootstrap r(rho), reps(200): corr school_years_s school_years_p if female == 0 & yob_s == `y' & school_years_s!=. & school_years_p!=.
	matrix res[`r',2] = _b[_bs_1]
	matrix res[`r',3] = _se[_bs_1]
	matrix res[`r',4] = e(N)
		
	bootstrap r(rho), reps(200): corr school_years_s school_years_m if female == 1 & yob_s == `y' & school_years_s!=. & school_years_m!=.
	matrix res[`r',5] = _b[_bs_1]
	matrix res[`r',6] = _se[_bs_1]
	matrix res[`r',7] = e(N)
}

preserve
	svmat res
	keep res*
	drop if res1 == .
	rename res1 year
	rename res2 beta_pat
	rename res3 se_pat
	rename res4 num_pat

	rename res5 beta_mat
	rename res6 se_mat
	rename res7 num_mat

	compress
	save "${our_dir}/output/ige_by_yob_edu", replace
restore

*-------------------------------------------------------------------------------
* 2.2 - Pooling -- Table 1 - Rows 1 and 2
*------------------------------------------------------------------------------

use ${our_dir}/data/pop_edu, clear

keep if yob_s >= 1950
gen max_school_p = max(school_years_p,school_years_m)
keep school_years_s school_years_m school_years_p max_school_p female 

matrix res = J(1,,.)

set seed 123456

**** mother and father education
bootstrap r(rho), reps(200): corr school_years_s school_years_p if female==0 & school_years_s!=. & school_years_p!=.
matrix res[1,1] = _b[_bs_1]
matrix res[1,2] = _se[_bs_1]
matrix res[1,3] = e(N)

bootstrap r(rho), reps(200): corr school_years_s school_years_m if female==1 & school_years_s!=. & school_years_m!=.
matrix res[1,4] = _b[_bs_1]
matrix res[1,5] = _se[_bs_1]
matrix res[1,6] = e(N)


**** exporting
clear 
svmat res 
rename res1 cor_edu_pat_son
rename res2 se_edu_pat_son
rename res3 num_pat_son

rename res4 cor_edu_mat_dau
rename res5 se_edu_mat_dau
rename res6 num_mat_dau

save "${our_dir}/output/pop_edu_corr", replace

* ==============================================================================
* 3 - LF education 
* ==============================================================================

*-------------------------------------------------------------------------------
* 3.1 - Create sample
*------------------------------------------------------------------------------
use ${our_dir}/data/pop_edu, clear

local start = 1950
local N = 1989 - `start' + 1

cap drop max_edu_*

ge max_edu_p = max(school_years_p,school_years_m)
ge max_edu_gpo = max(school_years_pp,school_years_mp,school_years_pm,school_years_mm)

cap drop samp_nrest 
ge byte samp_nrest = 

keep if max_edu_gpo !=.

keep school_years_s max_edu_* female samp_* yob_s
compress 
save "${our_dir}/data/temp_4lfedu", replace

*-------------------------------------------------------------------------------
* 3.2 - Program definition
*------------------------------------------------------------------------------

cap program drop boot_lambda
program define boot_lambda, rclass
	corr school_years_s max_edu_p
	scalar cor1 = r(rho)
	corr school_years_s max_edu_gpo
	scalar cor2 = r(rho)
	return scalar lambda = cor2/cor1 
	return scalar rho = sqrt((cor1^2)/cor2)
end

*-------------------------------------------------------------------------------
* 3.3 - Estimates (Pooling) -- Table 1 -- Rows from 5 to 8
*------------------------------------------------------------------------------

use ${our_dir}/data/temp_4lfedu, clear 
set seed 123456
keep if yob_s >= 1970
matrix res = J(1,10,.)	

preserve 	
	keep if female
	bootstrap r(rho) r(lambda), reps(200): boot_lambda
	matrix res[1,1] = _b[_bs_1]
	matrix res[1,2] = _se[_bs_1]
	matrix res[1,3] = _b[_bs_2]
	matrix res[1,4] = _se[_bs_2]
	matrix res[1,5] = e(N)
restore

preserve 	
	keep if !female
	bootstrap r(rho) r(lambda), reps(200): boot_lambda
	matrix res[1,6] = _b[_bs_1]
	matrix res[1,7] = _se[_bs_1]
	matrix res[1,8] = _b[_bs_2]
	matrix res[1,9] = _se[_bs_2]
	matrix res[1,10] = e(N)
restore

**** exporting
clear
svmat res

rename res1 mean_rho_dau
rename res2 se_rho_dau
rename res3 mean_lambda_dau
rename res4 se_lambda_dau
rename res5 num_dau
rename res6 mean_rho_son
rename res7 se_rho_son
rename res8 mean_lambda_son
rename res9 se_lambda_son
rename res10 num_son

save "${our_dir}/output/lf_pooling", replace

*-------------------------------------------------------------------------------
* 3.4 - Estimates (by year) -- Figure 3 and Table A.6
*------------------------------------------------------------------------------

use ${our_dir}/data/temp_4lfedu, clear 
set seed 123456

matrix res = J(20,11,.)	
local r = 0
qui forvalues y = 1970(1)1989 {
	noi: di `y'
	local r = `r' + 1
	preserve 	
		keep if female & yob_s == `y'
		bootstrap r(rho) r(lambda), reps(200): boot_lambda
		matrix res[`r',1] = _b[_bs_1]
		matrix res[`r',2] = _se[_bs_1]
		matrix res[`r',3] = _b[_bs_2]
		matrix res[`r',4] = _se[_bs_2]
		matrix res[`r',5] = e(N)
	restore

	preserve 	
		keep if !female & yob_s == `y'
		bootstrap r(rho) r(lambda), reps(200): boot_lambda
		matrix res[`r',6] = _b[_bs_1]
		matrix res[`r',7] = _se[_bs_1]
		matrix res[`r',8] = _b[_bs_2]
		matrix res[`r',9] = _se[_bs_2]
		matrix res[`r',10] = e(N)
	restore
	matrix res[`r',11] = `y'
}

**** exporting
clear
svmat res

rename res1 mean_rho_dau
rename res2 se_rho_dau
rename res3 mean_lambda_dau
rename res4 se_lambda_dau
rename res5 num_dau
rename res6 mean_rho_son
rename res7 se_rho_son
rename res8 mean_lambda_son
rename res9 se_lambda_son
rename res10 num_son
rename res11 year

save "${our_dir}/output/lf_byyear", replace


*-------------------------------------------------------------------------------
* 3.5 - Estimates full matrilineal and matrilineal -- Table A.3
*------------------------------------------------------------------------------

* define 2 different programs
cap program drop boot_lambda_m
program define boot_lambda_m, rclass
	corr school_years_s school_years_m
	scalar cor1 = r(rho)
	corr school_years_s school_years_mm
	scalar cor2 = r(rho)
	return scalar lambda = cor2/cor1 
	return scalar rho = sqrt((cor1^2)/cor2)
end

cap program drop boot_lambda_p
program define boot_lambda_p, rclass
	corr school_years_s school_years_p
	scalar cor1 = r(rho)
	corr school_years_s school_years_pp
	scalar cor2 = r(rho)
	return scalar lambda = cor2/cor1 
	return scalar rho = sqrt((cor1^2)/cor2)
end


use ${our_dir}/data/temp_4lfedu, clear 
set seed 123456
keep if yob_s >= 1970
matrix res = J(1,10,.)	

preserve 	
	keep if female & school_years_m!=. & school_years_mm!=.
	bootstrap r(rho) r(lambda), reps(200): boot_lambda_m
	matrix res[1,1] = _b[_bs_1]
	matrix res[1,2] = _se[_bs_1]
	matrix res[1,3] = _b[_bs_2]
	matrix res[1,4] = _se[_bs_2]
	matrix res[1,5] = e(N)
restore

preserve 	
	keep if !female & school_years_p!=. & school_years_pp!=.
	bootstrap r(rho) r(lambda), reps(200): boot_lambda_p
	matrix res[1,6] = _b[_bs_1]
	matrix res[1,7] = _se[_bs_1]
	matrix res[1,8] = _b[_bs_2]
	matrix res[1,9] = _se[_bs_2]
	matrix res[1,10] = e(N)
restore

**** exporting
clear
svmat res

rename res1 mean_rho_dau
rename res2 se_rho_dau
rename res3 mean_lambda_dau
rename res4 se_lambda_dau
rename res5 num_dau
rename res6 mean_rho_son
rename res7 se_rho_son
rename res8 mean_lambda_son
rename res9 se_lambda_son
rename res10 num_son

save "${our_dir}/output/lf_full_mat_pat", replace
