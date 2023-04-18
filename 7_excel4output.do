
cd "${our_dir}"
global pathexport "H:\export"
*===============================================================================
* 1 - Descriptives
*===============================================================================

use "output/descriptives", clear 
export excel using "${pathexport}\results_to_export.xlsx", sheet("descriptives", replace) firstrow(variables) 

*===============================================================================
* 2 - Results 
*===============================================================================


use "output/ginc_pool", clear
rename corr_* mean_*
reshape long mean se num, i(estimate) j(suff) string
ge type = estimate + suff
drop estimate suff
order type
gen param = "corr"
save "4output/results", replace

use "output/pop_edu_corr", clear
rename cor_* mean_*
cap drop fake 
gen fake = 1
rename num* num_edu* 
reshape long mean_ se_ num_, i(fake) j(type) string
drop fake 
gen param = "corr"
rename *_ *
append using "4output/results"
save "4output/results", replace

use "output/lf_pooling", clear
cap drop fake 
gen fake = 1
reshape long mean_lambda_ se_lambda_ mean_rho_ se_rho_ num_, i(fake) j(type) string
drop fake
rename *_ *
reshape long mean_ se_ , i(type) j(param) string
rename *_ *
replace type = type + "_lf" 
append using "4output/results"
save "4output/results", replace


use "output/lf_byyear", clear
reshape long mean_lambda_ se_lambda_ mean_rho_ se_rho_ num_, i(year) j(type) string
rename *_ *
reshape long mean_ se_ , i(type year) j(param) string
rename *_ *
gen byyear = 1
replace type = type + "_lf" 
append using "4output/results"
save "4output/results", replace


use "output/lf_full_mat_pat", clear
cap drop fake 
gen fake = 1
reshape long mean_lambda_ se_lambda_ mean_rho_ se_rho_ num_, i(fake) j(type) string
drop fake
rename *_ *
reshape long mean_ se_ , i(type) j(param) string
rename *_ *
replace type = type + "_lf_full_mat_pat" 
append using "4output/results"
save "4output/results", replace


use "output/ige_by_yob_ginc", clear
rename beta_* mean_*
rename * *_ginc 
rename year_ginc year 
reshape long mean_ se_ num_, i(year) j(type) string
rename *_ *
gen param = "corr"
gen byyear = 1
append using "4output/results"
save "4output/results", replace


use "output/ige_by_yob_edu", clear
rename beta_* mean_*
rename * *_edu
rename year_edu year 
reshape long mean_ se_ num_, i(year) j(type) string
rename *_ *
gen param = "corr"
gen byyear = 1
append using "4output/results"
save "4output/results", replace


**------ Table 2 - edu trends ------**
use "output/table2", clear
ge type="edutrends_mat" if female
replace type ="edutrends_pat" if !female
drop female
ge byte byyear = 0
ge param = "corr_" + moving
drop moving
rename corr mean
append using "4output/results"
save "4output/results", replace


**------ Export ------**
use "4output/results", clear 
replace byyear = 0 if byyear ==. 

export excel using "${pathexport}\results_to_export.xlsx", sheet("results", replace) firstrow(variables) 
erase "4output/results.dta"
