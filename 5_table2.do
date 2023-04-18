* ==============================================================================
* This do-file reproduces table 2
* ==============================================================================
use ${our_dir}/data/full_population, clear

drop rinpersoon_pmm rinpersoon_mmm rinpersoon_ppm rinpersoon_mpm rinpersoon_pmp rinpersoon_mmp rinpersoon_ppp rinpersoon_mpp genera4_found

rename rinpersoon* rin*
rename rin rin_s

rename rin_s rinpersoon
merge 1:1 rinpersoon using ${our_dir}/gross_income_allpop_03_20, keep(3) nogenerate
rename gross_income* ginc*_s
rename rinpersoon rin_s

foreach i in p m {
		
	rename rin_`i' rinpersoon
	merge m:1 rinpersoon using ${our_dir}/gross_income_allpop_03_20, keep(1 3) nogenerate
	
	rename gross_income* ginc*_`i'
	rename rinpersoon rin_`i'
	
	cap drop pinc_`i'
	egen pinc_`i' = rowmean(ginc*_`i')
}

keep if pinc_p != . | pinc_m != . 
drop pinc_*

keep if year(dofm(dobym))==1988

drop if (year(dofm(dobym_p)) < 1946 & female == 0) | (year(dofm(dobym_m)) < 1946 & female == 1)


foreach i in s p m {
	
	forvalues y=2003(1)2020 {
		replace ginc`y'_`i' = 1000000 if ginc`y'_`i' > 1000000 & ginc`y'_`i' !=.
		replace ginc`y'_`i' = . if ginc`y'_`i' <= 0

	}
}

ge inc_1y_s = ginc2016_s
ge inc_2y_s = (ginc2016_s+ginc2017_s)/2
ge inc_3y_s = (ginc2016_s+ginc2017_s+ginc2018_s)/3
ge inc_4y_s = (ginc2016_s+ginc2017_s+ginc2018_s+ginc2019_s)/4
ge inc_5y_s = (ginc2016_s+ginc2017_s+ginc2018_s+ginc2019_s+ginc2020_s)/5

foreach s in p m {

	ge inc_1y_`s' = ginc2003_`s'
	ge inc_2y_`s' = (ginc2003_`s'+ginc2004_`s')/2
	ge inc_3y_`s' = (ginc2003_`s'+ginc2004_`s'+ginc2005_`s')/3
	ge inc_4y_`s' = (ginc2003_`s'+ginc2004_`s'+ginc2005_`s'+ginc2006_`s')/4
	ge inc_5y_`s' = (ginc2003_`s'+ginc2004_`s'+ginc2005_`s'+ginc2006_`s'+ginc2007_`s')/5

}


matrix res = J(30,5,.)

forvalues i = 1(1)5 {
	bootstrap r(rho), reps(200): corr inc_`i'y_s inc_`i'y_p if female == 0
	matrix res[`i',1] = _b[_bs_1]
	matrix res[`i',2] = _se[_bs_1]
	matrix res[`i',3] = e(N)
	matrix res[`i',4] = `i'
	matrix res[`i',5] = 0

}


forvalues i = 1(1)5 {
	local r = `i' + 5
	bootstrap r(rho), reps(200): corr inc_`i'y_s inc_`i'y_m if female == 1
	matrix res[`r',1] = _b[_bs_1]
	matrix res[`r',2] = _se[_bs_1]
	matrix res[`r',3] = e(N)
	matrix res[`r',4] = `i'
	matrix res[`r',5] = 1
	

}

forvalues i = 1(1)5 {
	local r = `i' + 10
	bootstrap r(rho), reps(200): corr inc_1y_s inc_`i'y_p if female == 0
	matrix res[`r',1] = _b[_bs_1]
	matrix res[`r',2] = _se[_bs_1]
	matrix res[`r',3] = e(N)
	matrix res[`r',4] = `i'
	matrix res[`r',5] = 0
}

forvalues i = 1(1)5 {
	local r = `i' + 15
	bootstrap r(rho), reps(200): corr inc_1y_s inc_`i'y_m if female == 1
	matrix res[`r',1] = _b[_bs_1]
	matrix res[`r',2] = _se[_bs_1]
	matrix res[`r',3] = e(N)
	matrix res[`r',4] = `i'
	matrix res[`r',5] = 1
	
}

forvalues i = 1(1)5 {
	local r = `i' + 20
	bootstrap r(rho), reps(200): corr inc_`i'y_s inc_1y_p if female == 0
	matrix res[`r',1] = _b[_bs_1]
	matrix res[`r',2] = _se[_bs_1]
	matrix res[`r',3] = e(N)
	matrix res[`r',4] = `i'
	matrix res[`r',5] = 0
}

forvalues i = 1(1)5 {
	local r = `i' + 25
	bootstrap r(rho), reps(200): corr inc_`i'y_s inc_1y_m if female == 1
	matrix res[`r',1] = _b[_bs_1]
	matrix res[`r',2] = _se[_bs_1]
	matrix res[`r',3] = e(N)
	matrix res[`r',4] = `i'
	matrix res[`r',5] = 1
}


clear 
svmat res
rename res1 corr
rename res2 se
rename res3 num
rename res4 years_included
rename res5 female

cap drop moving
ge moving = "both" if _n<=10
replace moving = "only parent" if _n>10 & _n<=20
replace moving = "only offspring" if _n>20

compress
save "${our_dir}/output/table2", replace


