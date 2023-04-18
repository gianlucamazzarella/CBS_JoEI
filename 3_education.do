*===================================================================
*				3 - EDUCATION 
*===================================================================
*--------------------------------------------------------------------
* In this file:
* 1. Crosswalks for education codes
* 2. Append annual data
* 3. From education achievements in years of schooling
* 4. merge with population (generated in script 1_families.do)
*-----------------------------------------------------------------------

*===================================================================
* 1 - Crosswalk
*===================================================================
tempfile tomatch 
use "${util_edu}/OPLEIDINGSNRREFV26", clear
keep oplnr CTO2016V
rename CTO2016V CTO
save "`tomatch'"

use "${util_edu}/CTOREFV8", clear
keep OPLNIVSOI2016AGG4ACTHG CTO
merge 1:m CTO using "`tomatch'"

keep if _merge==3
drop CTO _merge

rename OPLNIVSOI2016AGG4ACTHG edu_cat

save "${our_dir}\data\cross_educat18_oplnr.dta", replace

*===================================================================
* 2 - Append annual data
*===================================================================

forval y = 1999/2020 {
	local c
	local s
	local t1 	"TAB"
	local t2
	local v		1

	if `y' >= 1999 & `y' <= 2005 {
		local c 	"120726"
		local s		" "
		}
	if `y' >= 2006 & `y' <= 2009 {
		local c 	"120619"
		local s		" "
	}
	if `y' == 2010 {
		local c 	"120918"
		local s		" "
	}
	if `y' == 2011 {
		local c 	"130924"
		local s		" "
	}
	if `y' == 2012 {
		local c 	"141020"
		local s		" "
	}
	if `y' >= 2013 & `y' <= 2015 {
		local t1 	
		local t2 	"TAB"
		local v 	2
	}
		if `y' == 2018 {
		local t1 	
		local t2 	"TAB"
		local v 	3
	}
		if `y' == 2019 {
		local t1 	
		local t2 	"TAB"
		local v 	1
	}
		if `y' == 2020 {
		local t1 	
		local t2 	"TAB"
		local v 	1
	}


	use "${hoogsteopltab_dir}\`y'\geconverteerde data/`c'`s'HOOGSTEOPL`t1'`s'`y'`t2'V`v'.dta", clear
	dis `y'
	rename *, lower 
	keep rinpersoon oplnrhb
	cap drop year 
	g year = `y'
	compress 
	sa "${our_dir}\data\gbapersoon_edu_`y'", replace
}
	
use "${our_dir}\data\gbapersoon_edu_2020", clear
foreach y of numlist 2020(-1)1999 {
	append using "${our_dir}\data\gbapersoon_edu_`y'"
	cap drop flag
	duplicates tag rinpersoon, g(flag) 
	ta flag 
	drop if flag > 0 & year == `y'
}

drop flag year
rename oplnrhb oplnr
sa "${our_dir}\data\gbapersoon_edu", replace

forval y = 1999/2020 {
	erase "${our_dir}\data\gbapersoon_edu_`y'.dta"
}

*===================================================================
* 3 - convert education achievements into years of schooling
*===================================================================

use "${our_dir}\data\gbapersoon_edu", clear

merge m:1 oplnr using "${our_dir}\data\cross_educat18_oplnr.dta"
keep if _merge == 3
drop _merge 

ge byte school_years=.
replace school_years=2 	if edu_cat=="1111" /* primary edu gr1-2 */
replace school_years=8 	if edu_cat=="1112" /* primary edu gr3-8 */
replace school_years=12 if edu_cat=="1211" /* practical edu */
replace school_years=12	if edu_cat=="1212" /* vmbo-b/k */
replace school_years=13	if edu_cat=="1213" /* mbo1 */
replace school_years=12	if edu_cat=="1221" /* vmbo/g */
replace school_years=11	if edu_cat=="1222" /* havo vwo-onderbouw */
replace school_years=14	if edu_cat=="2111" /* mbo2 */
replace school_years=16	if edu_cat=="2112" /* mbo3 */
replace school_years=16	if edu_cat=="2121" /* mbo4 */
replace school_years=13	if edu_cat=="2131" /* havo superstructure */
replace school_years=14	if edu_cat=="2132" /* vwo superstructure */
replace school_years=15	if edu_cat=="3111" /* hbo associate degree */
replace school_years=17	if edu_cat=="3112" /* hbo bachelor  */
replace school_years=17	if edu_cat=="3113" /* wo bachelor  */
replace school_years=18	if edu_cat=="3211" /* hbo master */
replace school_years=18	if edu_cat=="3212" /* wo master */
replace school_years=22	if edu_cat=="3213" /* PhD */

drop oplnr edu_cat
save "${our_dir}\data\gbapersoon_edu", replace


* ==============================================================================
* 4 - Link education to population
* ==============================================================================

use ${our_dir}/data/full_population, clear

drop rinpersoon_pmm rinpersoon_mmm rinpersoon_ppm rinpersoon_mpm rinpersoon_pmp rinpersoon_mmp rinpersoon_ppp rinpersoon_mpp genera4_found id_ancestor*

**** keep only those born before 1990
cap drop yob_s
gen yob_s = year(dofm(dobym))
drop dobym
drop if yob_s > 1989

**** attach offspring education
merge 1:1 rinpersoon using "${our_dir}/data/gbapersoon_edu"
keep if _merge == 3
drop _merge still_edu

rename rinpersoon* rin*
rename rin rin_s
rename school_years school_years_s

**** attach relatives education
foreach i in p m pp pm mp mm {
	
	cap drop yob_`i'
	gen yob_`i' = year(dofm(dobym_`i'))
	drop dobym_`i'
	
	rename rin_`i' rinpersoon
	merge m:1 rinpersoon using "${our_dir}/data/gbapersoon_edu", keep(1 3) nogenerate
	drop still_edu
	rename school_years school_years_`i'
	rename rinpersoon rin_`i'
}

**** check generations found
cap drop gen_2_found
gen gen_2_found = 1 if school_years_p != . | school_years_m != .
replace gen_2_found = 0 if school_years_p == . & school_years_m == .

cap drop gen_3_found
gen gen_3_found = 1 if school_years_pp != . | school_years_pm != . | school_years_mp != . | school_years_mm != .
replace gen_3_found = 0 if school_years_pp == . & school_years_pm == . & school_years_mp == . & school_years_mm == .

**** keep only if gen_2 is found
keep if gen_2_found == 1
drop gen_2_found

**** gen max parental and grandparental education // VEDERE SE SERVE ANCORA. SECONDO ME NO. LO FACCIAMO DOPO DOVE SERVE
cap drop school_year_par
egen school_year_par = rowmax(school_years_p school_years_m) 

cap drop school_year_gpar
egen school_year_gpar = rowmax(school_years_pp school_years_mp school_years_pm school_years_mm) 

compress
save ${our_dir}/data/pop_edu, replace
