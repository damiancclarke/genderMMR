/* ploughMMR.do v0.00             SB/DC/JG/AV              yyyy-mm-dd:2015-07-20
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

This file uses data from Alesina et al (2013) to test their plough estimates us-
ing MMR.  It runs country-level regressions, exactly following their specificat-
ions.

Replication code is found at:
http://scholar.harvard.edu/files/nunn/files/plough_replication_files.zip
*/

vers 11
clear all
set more off
cap log close

*-------------------------------------------------------------------------------
*--- (1) globals and locals
*-------------------------------------------------------------------------------
global MMR "~/investigacion/2013/WorldMMR/Data/"
global PLW "~/investigacion/2013/WorldMMR/Data/plough/ReplicationCodes"
global OUT "~/investigacion/2013/WorldMMR/Results"
global LOG "~/investigacion/2013/WorldMMR/Log"

log using "$LOG/ploughMMR.txt", text replace





*-------------------------------------------------------------------------------
*--- (2) Open MMR data and shape so MMR is per year, merge to Alesina et al
*-------------------------------------------------------------------------------
use "$MMR/MMR_WB.dta"
merge 1:1 countryname year using "$MMR/GDPpercap"
rename ny_gdp GDP
drop if _merge==2
drop _merge

keep if year==1990|year==1995|year==2000|year==2005|year==2010|year==2013
reshape wide MMR GDP, i(countryname countrycode) j(year)
rename countrycode isocode

merge 1:1 isocode using "$PLW/crosscountry_dataset"
egen aveMMR = rowmean(MMR*)
encode continent, gen(contCode)

lab var plow                     "Traditional plough use"
lab var agricultural_suitability "Agricultural suitability"
lab var tropical_climate         "Tropical Climate"
lab var large_animals            "Presence of large animals"
lab var political_hierarchies    "Political hierarchies"
lab var economic_complexity      "Economic complexity"
lab var MMR2000                  "Maternal Mortality Ratio in 2000"
lab var aveMMR                   "Average MMR (1990-2013)"

gen lgdp = log(GDP2000)
gen plowlgdp = plow*lgdp
*-------------------------------------------------------------------------------
*--- (3) Regressions
*-------------------------------------------------------------------------------
local c1 agricultural_suitability tropical_climate large_animals
local c2 political_hierarchies economic_complexity

eststo: reg flfp2000 plow `c1' `c2', r
su flfp2000 if e(sample), mean
loc m1: di %8.2fc r(mean) 

eststo: reg flfp2000 plow `c1' `c2' i.contCode, r
eststo: reg MMR2000  plow `c1' `c2', r
su MMR2000 if e(sample), mean
loc m2: di %8.2fc r(mean)

eststo: reg MMR2000  plow `c1' `c2' i.contCode, r
eststo: reg aveMMR   plow `c1' `c2', r
su aveMMR if e(sample), mean
loc m3: di %8.2fc r(mean)
eststo: reg aveMMR   plow `c1' `c2' i.contCode, r

#delimit ;
esttab est1 est2 est3 est4 est5 est6, keep(plow `c1' `c2') label
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) collabels(none) 
stats (r2 N, fmt(%9.2f %9.0g) label(R-squared Observations))
postfoot("Continent fixed effects       no              yes             no              yes             no              yes"
         "--------------------------------------------------------------------------------------------------------------------" "Columns 1-2 are from Alesina et al. (2013). All other models follow the same specifications.  Full notes are" "available in table 3 of Alesina et al. (2013)."
"Mean of dependent variable: `m1' `m2' `m3'");
#delimit cr
estimates clear

local c3 lgdp plowlgdp

eststo: reg flfp2000 plow `c3' `c1' `c2', r
su flfp2000 if e(sample), mean
loc m1: di %8.2fc r(mean) 

eststo: reg flfp2000 plow `c3' `c1' `c2' i.contCode, r
eststo: reg MMR2000  plow `c3' `c1' `c2', r
su MMR2000 if e(sample), mean
loc m2: di %8.2fc r(mean)

eststo: reg MMR2000  plow `c3' `c1' `c2' i.contCode, r
eststo: reg aveMMR   plow `c3' `c1' `c2', r
su aveMMR if e(sample), mean
loc m3: di %8.2fc r(mean)
eststo: reg aveMMR   plow `c3' `c1' `c2' i.contCode, r

#delimit ;
esttab est1 est2 est3 est4 est5 est6, keep(plow `c1' `c2' `c3') label
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) collabels(none) 
stats (r2 N, fmt(%9.2f %9.0g) label(R-squared Observations))
postfoot("Continent fixed effects       no              yes             no              yes             no              yes"
         "--------------------------------------------------------------------------------------------------------------------" "Columns 1-2 are from Alesina et al. (2013). All other models follow the same specifications.  Full notes are" "available in table 3 of Alesina et al. (2013)."
"Mean of dependent variable: `m1' `m2' `m3'");
#delimit cr


*mgroups("Female labor force participation in 2000" "Maternal Mortality Ratio in 2000" "Average MMR (1990-2013)", pattern(1 0 1 0 1 0))
