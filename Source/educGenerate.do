/* educGenerate.do v0.00         damiancclarke             yyyy-mm-dd:2016-03-09
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

Generates educational data by year and country using a combination of Barro-Lee,
MICS and DHS.  See imputationNotes.doc for a full description.
*/
    
vers 11
clear all
set more off
cap log close

********************************************************************************
*** (1) globals and locals
********************************************************************************
global DAT "~/investigacion/2013/WorldMMR/Data"


********************************************************************************
*** (2) Full countries
********************************************************************************
use "$DAT/LangGender_dataset"
drop if not_country==1
drop yr_sch
rename isocode WBcode
replace WBcode = "ROM" if WBcode == "MDA"
replace WBcode = "SER" if WBcode == "SRB"

merge 1:1 WBcode year using  "$DAT/BL2013_F2599_v2.1.dta" 
* drop BL data with year<1950 and for Taiwan and Reunion
drop if _merge==2 

********************************************************************************
*** (3) Fill in from FDR education (MICS and DHS).  Orig file saved as HDREduc
********************************************************************************
replace yr_sch = 10.3 if year==2010&country=="Samoa"
replace yr_sch = 11.1 if year==2010&country=="Bahamas, The"
replace yr_sch = 11.4 if year==2010&country=="Belarus"
replace yr_sch = 7.2  if year==2010&country=="Bosnia and Herzegovina"
replace yr_sch = 1.9  if year==2010&country=="Burkina Faso"
replace yr_sch = 0.6  if year==2010&country=="Chad"
replace yr_sch = 1.4  if year==2010&country=="Ethiopia"
replace yr_sch = 11.9 if year==2010&country=="Georgia"
replace yr_sch = 0.8  if year==2010&country=="Guinea"
replace yr_sch = 1.4  if year==2010&country=="Guinea-Bissau"
replace yr_sch = 7.6  if year==2010&country=="Lebanon"
replace yr_sch = 7.9  if year==2010&country=="Macedonia, FYR"
replace yr_sch = 4.8  if year==2010&country=="Madagascar"
replace yr_sch = 5.6  if year==2010&country=="Micronesia, Fed. Sts."
replace yr_sch = 9.9  if year==2010&country=="Montenegro"
replace yr_sch = 4.2  if year==2010&country=="Nigeria"
replace yr_sch = 12.2 if year==2010&country=="Palau"
replace yr_sch = 4.0  if year==2010&country=="Sao Tome and Principe"
replace yr_sch = 9.4  if year==2010&country=="Seychelles"
replace yr_sch = 7.3  if year==2010&country=="Suriname"
replace yr_sch = 3.6  if year==2010&country=="Timor-Leste"
replace yr_sch = 9.5  if year==2010&country=="Uzbekistan"
replace yr_sch = 8.0  if year==2010&country=="Vanuatu"

********************************************************************************
*** (4) Multiple imputation
********************************************************************************
xtset, clear
stset, clear
preserve
keep if year==1990|year==1995|year==2000|year==2005|year==2010
drop if MMR == .
drop if lgdp_5 == .
gen lgdpsq = lgdp_5*lgdp_5
mi set wide
mi register imputed yr_sch
mi register regular lgdp_5 year contcode
mi impute regress yr_sch lgdp_5 lgdpsq i.year i.contcode, add(20) rseed(42)

rename _20_yr_sch yr_sch_impute 
keep yr_sch_impute year WBcode _mi_miss
tempfile impeduc
save `impeduc'
restore

drop _merge
merge 1:1 year WBcode using `impeduc'


********************************************************************************
*** (5) Test
********************************************************************************
mi set, clear
xtset cncode year

gen lMMR = log(MMR)
gen womparl_gdp_5 = womparl_5*lgdp_5
gen yrsq = yr_sch_impute*yr_sch_impute
local vars womparl_5 lgdp_5 womparl_gdp_5

xtreg lMMR `vars' i.year                           , fe cluster(WBcode)
xtreg lMMR `vars' health_exp_5 i.year              , fe cluster(WBcode)
xtreg lMMR `vars' health_exp_5 yr_sch_i i.year     , fe cluster(WBcode)
xtreg lMMR `vars' health_exp_5 yr_sch_i i.year yrsq, fe cluster(WBcode)
