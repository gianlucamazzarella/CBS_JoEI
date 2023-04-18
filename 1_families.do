*====================================================================
*				1 - FAMILIES
*====================================================================

*-----------------------------------------------------------------------
* In this file:
* 1. start creating file with parents' ids (file KINDOUDERTAB)
* 2. take individuals' characteristics (file GBAPERSOONTAB 2020)
* 3. link parents
* 4. recursive merge to link parent's information and id grandparents and grandparents info
* 5. save file as "rin_parents_granparents_10_20"
*-----------------------------------------------------------------------

*====================================================================
* 1 - rinpersoon's parents
*====================================================================

* here the locals with the file names
local p0 "G:/Bevolking/KINDOUDERTAB/2010/geconverteerde data/120229 KINDOUDERTAB 2010V1"
local p1 "G:/Bevolking/KINDOUDERTAB/2011/geconverteerde data/120719 KINDOUDERTAB 2011V1"
local p2 "G:/Bevolking/KINDOUDERTAB/2012/geconverteerde data/130704 KINDOUDERTAB 2012v1"
local p3 "G:/Bevolking/KINDOUDERTAB/2013/geconverteerde data/140717 KINDOUDERTAB 2013V1"
local p4 "G:/Bevolking/KINDOUDERTAB/2014/geconverteerde data/KINDOUDERTAB 2014V1"
local p5 "G:/Bevolking/KINDOUDERTAB/2015/geconverteerde data/KINDOUDER2015TABV2"
local p6 "G:/Bevolking/KINDOUDERTAB/2016/geconverteerde data/KINDOUDER2016TABV1"
local p7 "G:/Bevolking/KINDOUDERTAB/2017/geconverteerde data/KINDOUDER2017TABV1"
local p8 "G:\Bevolking\KINDOUDERTAB\2018\geconverteerde data/KINDOUDER2018TABV1"
local p9 "G:\Bevolking\KINDOUDERTAB\2019\geconverteerde data/KINDOUDER2019TABV2"

/* This database updates each year, so we follow the same rules:
 1. keep only individuals with rinpersoons=="R" (i.e. people in the registries)
 2. correct to missing ids !="R" for parents
 3. save annual files
 4. starting from the most recent file, we merge the oldest and correct missing 
	info with not-missing if available.
 */
forvalues pp=0(1)9 {
	use "`p`pp''",clear
	rename *, lower
	keep if rinpersoons=="R"
	replace rinpersoonpa="" if rinpersoonspa!="R"
	replace rinpersoonma="" if rinpersoonsma!="R"

	keep rinpersoon rinpersoonpa rinpersoonma
	replace rinpersoonpa="" if substr(rinpersoonpa,1,1)=="-"
	replace rinpersoonma="" if substr(rinpersoonma,1,1)=="-"
	replace rinpersoonma="" if rinpersoon==rinpersoonma
	replace rinpersoonpa="" if rinpersoon==rinpersoonpa
	save "${our_dir}/data/kindou`pp'", replace
}

import spss using G:\Bevolking\KINDOUDERTAB\2020\KINDOUDER2020TABV2.sav, clear
rename *, lower
keep if rinpersoons=="R"
replace rinpersoonpa="" if rinpersoonspa!="R"
replace rinpersoonma="" if rinpersoonsma!="R"

keep rinpersoon rinpersoonpa rinpersoonma
replace rinpersoonpa="" if substr(rinpersoonpa,1,1)=="-"
replace rinpersoonma="" if substr(rinpersoonma,1,1)=="-"
replace rinpersoonma="" if rinpersoon==rinpersoonma
replace rinpersoonpa="" if rinpersoon==rinpersoonpa
save "${our_dir}/data/kindou10", replace

* recursive merge from the newest to the oldest to update missings
clear
use ${our_dir}/data/kindou0, clear
ge byte todrop=0
forvalues y=1(1)10 {
	merge 1:1 rinpersoon using ${our_dir}/data/kindou`y', update
	replace todrop=0 if todrop==.
	replace todrop=1 if todrop==0 & _merge==5
	drop _merge
}
*
drop if todrop
drop todrop
save ${our_dir}/data/rin_parents_10_20, replace

* delete annual files
forvalues y=0(1)10 {
	erase ${our_dir}/data/kindou`y'.dta
} 
* ==============================================================================
* 2 - GBAPERSOONTAB - 2020
* ==============================================================================

use rinpersoon 	gbageslacht gbageboorteland gbageboortelandmoeder gbageboortelandvader ///
				gbageboortejaar gbageboortemaand gbageboortejaarmoeder gbageboortemaandmoeder ///
				gbageboortejaarvader gbageboortemaandvader ///
	using "${gbapersoon_dir}/2020/geconverteerde data/GBAPERSOON2020TABV3.dta", clear

foreach x in 	gbageboorteland gbageboortelandmoeder gbageboortelandvader ///
				gbageboortejaar gbageboortemaand gbageboortejaarmoeder ///
				gbageboortemaandmoeder gbageboortejaarvader gbageboortemaandvader {
	gen int de_`x' = real(`x')
	drop `x'
	rename de_`x' `x' 
}

* ------------------------------------------------------------------------------
* 2.1 - Date of birth (Dob), Dob father, Dob mother
* ------------------------------------------------------------------------------

cap drop dobym 
gen int dobym = ym(gbageboortejaar,gbageboortemaand)

cap drop dobym_p
gen int dobym_p = ym(gbageboortejaarvader,gbageboortemaandvader)

cap drop dobym_m
gen int dobym_m = ym(gbageboortejaarmoeder,gbageboortemaandmoeder)

drop gbageboortejaar* gbageboortemaand* 
format dobym* %tm

cap drop female 
gen byte female = gbageslacht=="2"
drop gbageslacht

compress
save ${our_dir}/data/nat_gba2020, replace

* ==============================================================================
* 3 - Parents
* ==============================================================================

use ${our_dir}/data/nat_gba2020, replace
merge 1:1 rinpersoon using ${our_dir}/data/rin_parents_10_20
drop if _merge==2
ge byte parents_notfound=(_merge==1)
drop _merge

rename rinpersoonma rinpersoon_m
rename rinpersoonpa rinpersoon_p
drop nat_* parents_notfound

* ==============================================================================
* 4 - Cascade linkages for parents
* ==============================================================================

preserve
	keep if female == 1
	rename rinpersoon_m rinpersoon_mm
	rename rinpersoon_p rinpersoon_pm
	rename rinpersoon rinpersoon_m
	drop dobym_p dobym_m female
	save ${our_dir}/data/maternal, replace
restore

preserve
	keep if female == 0
	rename rinpersoon_m rinpersoon_mp
	rename rinpersoon_p rinpersoon_pp
	rename rinpersoon rinpersoon_p
	drop dobym_p dobym_m female
	save ${our_dir}/data/paternal, replace
restore


* dummy to understand if I found info on grandparents
cap drop genera3_found
ge byte genera3_found =	rinpersoon_pp!="" | rinpersoon_mp!="" | rinpersoon_pm!="" | ///
						rinpersoon_mm!=""

* ------------------------------------------------------------------------------
* 4.3 - Compress & save
* ------------------------------------------------------------------------------

compress
save  ${our_dir}/data/full_population, replace
