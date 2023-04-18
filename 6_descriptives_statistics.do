*===================================================================
*				6 - DESCRIPTIVE STATISTICS
*===================================================================
/* In this file: we produce all the descriptive statistics, and we store them
	in an excel file ready to be exported. 
	This includes:
 1. Figure 1
 2. Figure A.1
 3. Table A.2
 4. Figure A.2
 */
*===================================================================

*===============================================================================
* 1 - Income sample
*===============================================================================

global varlist_inc yob ginc_3y ginc_5y rinc_3y rinc_5y full_ginc_3y full_ginc_5y

foreach i in 0 1 {
	use ${our_dir}/data/pop_ginc, clear
	keep if yob_s >= 1970
	
	if `i' == 0 {
		local k = "male"
		local j = "p"
		keep if female_s == `i'
		cap drop gender
		gen gender = "`k'"
	}
			
	if `i' == 1 {
		local k = "female"
		local j = "m"
		keep if female_s == `i' 
		cap drop gender
		gen gender = "`k'"
	}
	
	foreach v of global varlist_inc {
		di "`v'"
		foreach h in s `j' {
			su `v'_`h' if `v'_s !=. & `v'_`j'!=.,det
			foreach stat in p10 p25 p50 p75 p90 mean sd {
				cap drop `v'_`h'_`stat' 
				ge `v'_`h'_`stat' = `r(`stat')'
			}
			cap drop `v'_`h'_num
			cou if `v'_s !=. & `v'_`j'!=.
			gen `v'_`h'_num = `r(N)'
	
		}
	}

	drop rin_s-yob_m
	keep if _n==1
	save ${our_dir}/output/temp/pop_ginc_gender_`k', replace	
	
}

*-------------------------------------------------------------------------------
* 1.1 - Income: append and reshape
*-------------------------------------------------------------------------------
global var2reshape ""
foreach v of global varlist_inc{
	foreach suff in s m p {
		global var2reshape "$var2reshape `v'_`suff'"
	}
}

use ${our_dir}/output/temp/pop_ginc_gender_male, clear	
append using ${our_dir}/output/temp/pop_ginc_gender_female

reshape long ${var2reshape}, i(gender) j(stat) string
replace stat = subinstr(stat,"_","",.)

cap drop sample
gen sample = "pop_ginc"
save ${our_dir}/output/temp/desc_pop_ginc, replace	


*===============================================================================
* 2 - Education sample
*===============================================================================

global varlist_edu yob school_years max_school 
* here we compute 2 identical max_school for looping and then we drop max_school_m
foreach i in 0 1 {
	
	use ${our_dir}/data/pop_edu, clear
	
	keep if yob_s >= 1950
	cap drop max_school_p
	gen max_school_s = school_years_s
	gen max_school_p = max(school_years_p,school_years_m)
	gen max_school_m = max(school_years_p,school_years_m)

	if `i' == 0 {
		local k = "male"
		local j = "p"
		keep if female == `i'
		cap drop gender
		gen gender = "`k'"
	}
			
	if `i' == 1 {
		local k = "female"
		local j = "m"
		keep if female == `i'
		cap drop gender
		gen gender = "`k'"
	}
	
	di "`k'"
	foreach v of global varlist_edu {
		di "`v'"
		foreach h in s `j' {
			su `v'_`h' if `v'_s !=. & `v'_`j'!=.,det
			foreach stat in p10 p25 p50 p75 p90 mean sd {
				cap drop `v'_`h'_`stat' 
				ge `v'_`h'_`stat' = `r(`stat')'
			}
			cap drop `v'_`h'_num
			cou if `v'_s !=. & `v'_`j'!=.
			gen `v'_`h'_num = `r(N)'
	
		}
	}

	
	drop rin_s-max_school_m
	keep if _n==1
	save ${our_dir}/output/temp/pop_edu_gender_`k', replace	
	
}

*-------------------------------------------------------------------------------
* 2.1 - Education: append and reshape
*-------------------------------------------------------------------------------
global var2reshape ""
foreach v of global varlist_edu {
	foreach suff in s m p {
		global var2reshape "$var2reshape `v'_`suff'"
	}
}

use ${our_dir}/output/temp/pop_edu_gender_male, clear	
append using ${our_dir}/output/temp/pop_edu_gender_female

reshape long ${var2reshape}, i(gender) j(stat) string
replace stat = subinstr(stat,"_","",.)

cap drop sample
gen sample = "pop_edu"
save ${our_dir}/output/temp/desc_pop_edu, replace	

*===============================================================================
* 3 - LF
*===============================================================================

global varlist_lf yob_s yob_p yob_m yob_gp_avg ///
				  school_years_s school_years_p school_years_m ///
				  max_edu_p max_edu_gpo 

foreach i in 0 1 {

	use ${our_dir}/data/pop_edu, clear
	
	keep if yob_s >= 1970
	**** generate variable for sample restriction
	cap drop max_edu_*
	gen max_edu_p = max(school_years_p,school_years_m)
	ge max_edu_gpo = max(school_years_pp,school_years_mp,school_years_pm,school_years_mm)

	keep if max_edu_gpo !=.

	cap drop yob_gp_avg
	egen yob_gp_avg = rowmean(yob_pp yob_pm yob_mp yob_mm)
	
	**** sample restriction
	cap drop sample
	gen sample = "LF"
	**** gender restriction	
	if `i' == 0 {
		local k = "male"
		keep if female == `i'
		cap drop gender
		gen gender = "`k'"
	}
			
	if `i' == 1 {
		local k = "female"
		keep if female == `i'
		cap drop gender
		gen gender = "`k'"
	}
	di "`k'"
	
	foreach v of global varlist_lf {
		
	
	di "`v'"
	su `v',det
	foreach stat in p10 p25 p50 p75 p90 mean sd {

		cap drop `v'_`stat'
		ge `v'_`stat' = `r(`stat')'	
	}
	cap drop `v'_num
	cou if `v'!=.
	gen `v'_num = `r(N)'


}


drop rin_s-yob_gp_avg
keep if _n==1
save ${our_dir}/output/temp/pop_LF_gender_`k', replace	
	
}

*-------------------------------------------------------------------------------
* 3.1 - LF: append and reshape
*-------------------------------------------------------------------------------

use ${our_dir}/output/temp/pop_LF_gender_male, clear	
append using ${our_dir}/output/temp/pop_LF_gender_female

reshape long ${varlist_lf}, i(sample gender) j(stat) string
replace stat = subinstr(stat,"_","",.)

save ${our_dir}/output/temp/desc_pop_LF, replace	

*===============================================================================
* 4 - Descriptives mean school -- Figure A.2
*===============================================================================

use rinpersoon 	gbageslacht gbageboortejaar ///
	using "${gbapersoon_dir}/2020/geconverteerde data/GBAPERSOON2020TABV3.dta", clear

rename gbageboortejaar yob
destring yob,replace
cap drop female 
gen byte female = gbageslacht=="2"
drop gbageslacht

merge 1:1 rinpersoon using  "${our_dir}\data\gbapersoon_edu"
keep if _merge == 3 & yob<=1990 & yob>=1920
drop _merge rinpersoon still_edu

sort yob female
by yob female: egen mean_school_ = mean(school_years)
by yob female: gen num_school_ = _N
by yob: egen mean_school_tot = mean(school_years)
by yob: gen num_school_tot = _N
by yob female: keep if _n == 1
ge j = "female" if female
replace j = "male" if !female
drop female school_years
reshape wide mean_school_ num_school_, i(yob mean_school_tot num_school_tot) j(j) string


tostring yob, ge(yobs)
cap drop sample
ge sample = "desc_yob"+ yobs
drop yob*
order sample
reshape long mean_school_ num_school_, i(sample) j(gender) string
rename *_ *
rename num_school dim
replace gender = "both" if gender == "tot"
sort gender sample

save ${our_dir}/output/temp/desc_pop_mean_school, replace

*===============================================================================
* 5 - Put descriptives together
*===============================================================================

use ${our_dir}/output/temp/desc_pop_LF, clear	

foreach k in edu ginc mean_school {
	append using ${our_dir}/output/temp/desc_pop_`k'
}

compress
save ${our_dir}/output/descriptives, replace	
