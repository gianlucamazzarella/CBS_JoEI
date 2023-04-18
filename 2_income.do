*===================================================================
*				2 - INCOME 
*===================================================================
/* In this file: 
 1. append and cleaning annual income data.
 2. generate all the variables on interest (3 years mean, 5 years, etc.)
 3. link to full population (generated in script 1_families.do) and creation of sample
*/
*===================================================================


*===================================================================
* 1 - Append and cleaning annual data
*-----------------------------------------------------------------------
/*
DATA SOURCES: 
	- INTPERSINC from 2003 to 2015
	- INPATAB from 2011 to 2020
   so we adopt the following strategy:
	1. from 2003 to 2010 -> INTPERSINC
	2. from 2011 to 2015 -> merge between INTPERSINC and INPATAB
	3. from 2016 -> INPATAB
VARIABLES:
INPATAB
inppersbrut /*gross income */

INTPERSINC
persbrut /* gross income */
*/

forvalues y=2003(1)2020 {
	if (`y'<2011) {
		cd "${intpersinc}\`y'\geconverteerde data\"
		qui: fs *dta
		foreach f in `r(files)' {

			use "`f'", clear
			rename *, lower
		}
		keep if rinpersoons=="R"
		keep rinpersoon persbrut
		replace persbrut = 0 if persbrut==999999999
		rename persbrut gross_income`y'
		sa "${our_dir}/data/inc`y'", replace
	}
	if (`y'>=2011 & `y'<=2015) {
		* we want to complement the two sources
		* we start from inpatab
		use "${inpatab}\INPA`y'TABV2",clear
		rename *, lower
		replace inppersbrut = 0 if inppersbrut == 9999999999
		rename inppersbrut gross_income`y'
		keep rinpersoon gross_income`y'
		replace gross_income`y'= 0 if gross_income`y'==.
		tempfile inpat
		sa "`inpat'", replace
		
		* then we load persoonink
		cd "${intpersinc}\`y'\geconverteerde data\"
		fs *dta
		foreach f in `r(files)' {
			if ("`f'" !="persoonink2014tabv3_selectie.dta") {
				use "`f'", clear
			}
		}
		rename *, lower
		keep if rinpersoons=="R"
		cap rename vrlpersbrut persbrut
		keep rinpersoon persbrut
		replace persbrut = 0 if persbrut==999999999
		merge 1:1 rinpersoon using "`inpat'"
		replace  gross_income`y'= persbrut if gross_income`y'==.
		drop persbrut _merge
		sa "${our_dir}/data/inc`y'", replace
	}
	if (`y'==2016) {
		use "${inpatab}\INPA2016TABV3",clear
		rename *, lower
		replace inppersbrut = 0 if inppersbrut==9999999999
		rename inppersbrut gross_income`y'

		keep rinpersoon gross_income`y'
		replace gross_income`y'= 0 if gross_income`y'==.
		compress
		sa "${our_dir}/data/inc`y'", replace
	}
	if (`y'==2017) {
		use "${inpatab}\INPA2017TABV3",clear
		rename *, lower
		replace inppersbrut = 0 if inppersbrut==9999999999
		rename inppersbrut gross_income`y'
		keep rinpersoon gross_income`y'
		replace gross_income`y'= 0 if gross_income`y'==.
		compress
		sa "${our_dir}/data/inc`y'", replace
	}
		if (`y'==2018) {
		use "${inpatab}\INPA2018TABV2",clear
		rename *, lower
		replace inppersbrut = 0 if inppersbrut==9999999999
		rename inppersbrut gross_income`y'
		keep rinpersoon gross_income`y'
		replace gross_income`y'= 0 if gross_income`y'==.
		compress
		sa "${our_dir}/data/inc`y'", replace
	}
	if (`y'==2019) {
		use "${inpatab}\INPA2019TABV2",clear
		rename *, lower
		replace inppersbrut = 0 if inppersbrut==9999999999
		rename inppersbrut gross_income`y'
		keep rinpersoon gross_income`y'
		replace gross_income`y'= 0 if gross_income`y'==.
		compress
		sa "${our_dir}/data/inc`y'", replace
	}
	if (`y'==2020) {
		use "${inpatab}\INPA2020TABV1",clear
		rename *, lower
		replace inppersbrut = 0 if inppersbrut==9999999999
		rename inppersbrut gross_income`y'
		keep rinpersoon gross_income`y'
		replace gross_income`y'= 0 if gross_income`y'==.
		compress
		sa "${our_dir}/data/inc`y'", replace
	}

}
use "${our_dir}/inc2003",clear
forvalues y = 2004(1)2020 {
	merge 1:1 rinpersoon using "${our_dir}/data/inc`y'", nogenerate
}

compress
sa "${our_dir}/data/gross_income_allpop_03_20", replace

forvalues y = 2003(1)2020 {
	capture erase "${our_dir}/data/inc`y'.dta"
}

* ==============================================================================
* 2 - Income: generating variables of interest
* ==============================================================================

*-------------------------------------------------------------------------------
* 2.1 computing months of employment
*-------------------------------------------------------------------------------
use "${secmbus}/SECMBUS2020V1", clear
keep if RINPERSOONS  == "R"
drop RINPERSOONS ONDERWIJSNR_crypt

rename *, lower
keep if secm == "11" | secm == "12" | secm == "13" | secm == "14"
keep rinpersoon aanvsecm eindsecm

ge startym= ym(real(substr(aanvsecm,1,4)),real(substr(aanvsecm,5,2)))
ge endym= ym(real(substr(eindsecm,1,4)),real(substr(eindsecm,5,2)))

ge year_end = real(substr(eindsecm,1,4))
ge year_start = real(substr(aanvsecm,1,4))
drop aanvsecm eindsecm
format startym endym %tm

forvalues y = 2003(1)2020{
	ge byte length`y' = 0
	replace length`y' = min(ym(`y',12),endym) - max(startym,ym(`y',1)) + 1 if year_start<=`y' & year_end>=`y'
}

keep rinpersoon length*

collapse (sum) length*, by(rinpersoon)

compress
save ${our_dir}/data/working_months, replace

*-------------------------------------------------------------------------------
* 2.2 merge months of employment with income
*-------------------------------------------------------------------------------
use ${our_dir}/data/gross_income_allpop_03_20, clear
merge 1:1 rinpersoon using "${gbapersoon_dir}/2020/geconverteerde data/GBAPERSOON2020TABV3.dta", ///
	keepusing(rinpersoon gbageboortejaar gbageboortemaand) keep(match) nogenerate


foreach x in gbageboortejaar gbageboortemaand {
	gen int de_`x' = real(`x')
	drop `x'
	rename de_`x' `x' 
}

cap drop dobym 
gen int dobym = ym(gbageboortejaar,gbageboortemaand)
format dobym %tm
drop gbageboortejaar gbageboortemaand
order dobym, after(rinpersoon)
cap drop yob
gen yob = year(dofm(dobym))

merge 1:1 rinpersoon using ${our_dir}/data/working_months, keep(master match) nogenerate

* minimum and maximum age
global eta_min = 28
global eta_max = 60

rename gross_income* ginc*

cap drop count_not_miss
ge count_not_miss = 0

cap drop count_full_not_miss
ge count_full_not_miss = 0

* inflation for real income
local infl2003 = 0.8243
local infl2004 = 0.8348
local infl2005 = 0.8488
local infl2006 = 0.8582
local infl2007 = 0.8720
local infl2008 = 0.8937
local infl2009 = 0.9044
local infl2010 = 0.9159
local infl2011 = 0.9373
local infl2012 = 0.9604
local infl2013 = 0.9844
local infl2014 = 0.9940
local infl2015 = 1.0000
local infl2016 = 1.0032
local infl2017 = 1.0170
local infl2018 = 1.0344
local infl2019 = 1.0616
local infl2020 = 1.0751

* computing years of observation and computing means
qui forvalues y=2003(1)2020 {
	noi: di `y'
	replace ginc`y' = 1000000 if ginc`y' > 1000000 & ginc`y' !=.
	replace ginc`y' = . if (`y' - yob > ${eta_max} | `y' - yob < ${eta_min})
	replace ginc`y' = . if ginc`y' <= 0
	
	cap drop rinc`y'
	ge rinc`y' = ginc`y'/(`infl`y'')
	replace count_not_miss = count_not_miss + 1 if ginc`y' !=.
	
	cap drop full_ginc`y'
	ge full_ginc`y' = ginc`y' if length`y' == 12
	replace count_full_not_miss = count_full_not_miss + 1 if full_ginc`y' !=.
}

egen avg_ginc = rowmean(ginc*)
ge avg_ginc_3y = avg_ginc if count_not_miss >=3
ge avg_ginc_5y = avg_ginc if count_not_miss >=5
drop avg_ginc ginc*


egen avg_rinc = rowmean(rinc*)
ge avg_rinc_3y = avg_rinc if count_not_miss >=3
ge avg_rinc_5y = avg_rinc if count_not_miss >=5
drop avg_rinc rinc*

egen avg_full_ginc = rowmean(full_ginc*)
ge avg_full_ginc_3y = avg_full_ginc if count_full_not_miss >=3
ge avg_full_ginc_5y = avg_full_ginc if count_full_not_miss >=5
drop avg_full_ginc full_ginc*

drop if avg_ginc_3y == . 
drop count* length* dobym yob
compress

save ${our_dir}/data/avg_income, replace

* ==============================================================================
* 3 - Link income to population
* ==============================================================================

use ${our_dir}/data/full_population, clear

rename rinpersoon* rin*
rename rin rinpersoon

merge 1:1 rinpersoon using ${our_dir}/data/avg_income, keep(3) nogenerate
rename rinpersoon rin_s
rename avg_* *_s

cap drop yob_s
gen yob_s = year(dofm(dobym))

foreach i in p m {
	
	rename rin_`i' rinpersoon
	merge m:1 rinpersoon using ${our_dir}/data/avg_income, keep(1 3) nogenerate
	
	rename avg_* *_`i'
	rename rinpersoon rin_`i'
	
	cap drop yob_`i'
	gen yob_`i' = year(dofm(dobym_`i'))
	
}

keep rin* yob* female* ginc* rinc* full_ginc*
rename female female_s

compress
save ${our_dir}/data/pop_ginc, replace

