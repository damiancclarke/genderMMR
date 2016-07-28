/* quotaAnalysis.do v0.00        damiancclarke             yyyy-mm-dd:2016-07-28
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

Examine effects of quotas on female representation, and on maternal mortality.

*/

vers 11
clear all
set more off
cap log close

*-------------------------------------------------------------------------------
*--- (1) globals
*-------------------------------------------------------------------------------
global DAT "~/investigacion/2013/WorldMMR/Data/Gender"
global OUT "~/investigacion/2013/WorldMMR/MMRWomenLeaders"

log using "$OUT/Log/quotaAnalysis.txt", replace text


*-------------------------------------------------------------------------------
*--- (2) Generate series
*-------------------------------------------------------------------------------
use "$DAT/LangGender_dataset.dta", clear
drop if not_country==1
keep if womparl!=.
keep country womparl year
keep if year>=1990

merge 1:1 country year using "MMR", gen(_mergeMMR)
merge 1:1 country year using "GDP", gen(_mergeGDP)
drop if year<1990
merge m:1 country using quotasComplete

*-------------------------------------------------------------------------------
*--- (3) Basic regressions
*-------------------------------------------------------------------------------
keep if sh_sta_mmrt !=.&womparl !=.&ny_gdp_pcap_pp_kd !=.
sort country year
bys country: gen MMRt1 = sh_sta_mmrt[_n+1]
gen lnMMRt1      = log(MMRt1)
gen lnGDP        = log(ny_gdp)
gen womparxlnGDP = womparl*lnGDP
replace quotayear=. if quotayear==2013

gen quota = year>quotayear
encode country, gen(ccode)
xtset ccode year

xtreg lnMMRt1 womparl               i.year, fe cluster(ccode)
xtreg lnMMRt1 womparl lnGDP         i.year, fe cluster(ccode)
xtreg lnMMRt1 womparl lnGDP womparx i.year, fe cluster(ccode)


gen quotaxlnGDP = quota*lnGDP
gen quotaRes    = quotatype!="Legislated Candidate Quota"&quota==1
gen quotaCand   = quotatype=="Legislated Candidate Quota"&quota==1
gen quotaRxlnGDP = quotaRes*lnGDP
gen quotaCxlnGDP = quotaCand*lnGDP

xtreg womparl quota lnGDP i.year, fe cluster(ccode)
xtreg womparl quotaRes i.year, fe cluster(ccode)
xtreg womparl quotaCan i.year, fe cluster(ccode)
exit
xtreg lnMMRt1 quotaRes lnGDP  i.year, fe cluster(ccode)
xtreg lnMMRt1 quotaCan lnGDP  i.year, fe cluster(ccode)
exit
*ivreg2 lnMMRt1 lnGDP i.ccode i.year (womparl womparx = quota quotax), /*
**/ partial(i.ccode i.year) cluster(ccode)

*-------------------------------------------------------------------------------
*--- (4) Event Study Women in Parliament
*-------------------------------------------------------------------------------
preserve    
replace quotayear=. if quotatype=="Legislated Candidate Quota"
replace quotayear=. if quotayear==2013

drop quota
gen quota = year>=quotayear


gen prepost = year-(quotayear-1)
bys country: egen qcountry = max(quota)
sum prepost
local min = (-1*r(min))+1
local max = r(max)-1

foreach num of numlist 1(1)`min' {
    gen intyear = prepost==-`num'
    gen quotaLag`num' = qcountry*intyear
    drop intyear
}
foreach num of numlist 1(1)`max' {
    gen intyear = prepost==`num'
    gen quotaLead`num' = qcountry*intyear
    drop intyear
}
encode region, gen(rcode)


xtreg womparl i.year quotaLag* quotaLead* lnGDP i.rcode#c.year, fe cluster(ccode)
local j = 1
gen time   = .
gen PointE = .
gen UBound = .
gen LBound = .
foreach num of numlist `min'(-1)1 {
    replace time   = -`num' in `j'
    replace PointE = _b[quotaLag`num'] in `j'
    replace LBound = _b[quotaLag`num']-1.96*_se[quotaLag`num'] in `j'
    replace UBound = _b[quotaLag`num']+1.96*_se[quotaLag`num'] in `j'
    local ++j
}
foreach num of numlist 1(1)`max' {
    replace time   =  `num' in `j'
    replace PointE = _b[quotaLead`num'] in `j'
    replace LBound = _b[quotaLead`num']-1.96*_se[quotaLead`num'] in `j'
    replace UBound = _b[quotaLead`num']+1.96*_se[quotaLead`num'] in `j'
    local ++j
}
#delimit ;
twoway line PointE time || rcap LBound UBound time,
scheme(s1mono) ytitle("Women in Parliament")
yline(0, lpattern(dot)) xline(0, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year 0 is omitted as the base case.");
graph export "$OUT/quotas/eventWomParl.eps", as(eps) replace;
#delimit cr
drop time PointE UBound LBound prepost qcountry quotaLag* quotaLead* 

*-------------------------------------------------------------------------------
*--- (4) Event Study MMR
*-------------------------------------------------------------------------------



bys country: egen meanGDP = mean(lnGDP)
*sum meanGDP, d
*keep if meanGDP<=r(p75)

drop quota
gen quota = year>=quotayear

gen prepost = year-(quotayear-1)
bys country: egen qcountry = max(quota)
sum prepost
local min = (-1*r(min))+1
local max = r(max)-1

foreach num of numlist 1(1)`min' {
    gen intyear = prepost==-`num'
    gen quotaLag`num' = qcountry*intyear
    drop intyear
}
foreach num of numlist 1(1)`max' {
    gen intyear = prepost==`num'
    gen quotaLead`num' = qcountry*intyear
    drop intyear
}

xtreg lnMMRt1 i.year quotaLag* quotaLead* lnGDP, fe cluster(ccode)
local j = 1
gen time   = .
gen PointE = .
gen UBound = .
gen LBound = .
foreach num of numlist `min'(-1)1 {
    replace time   = -`num' in `j'
    replace PointE = _b[quotaLag`num'] in `j'
    replace LBound = _b[quotaLag`num']-1.96*_se[quotaLag`num'] in `j'
    replace UBound = _b[quotaLag`num']+1.96*_se[quotaLag`num'] in `j'
    local ++j
}
foreach num of numlist 1(1)`max' {
    replace time   =  `num' in `j'
    replace PointE = _b[quotaLead`num'] in `j'
    replace LBound = _b[quotaLead`num']-1.96*_se[quotaLead`num'] in `j'
    replace UBound = _b[quotaLead`num']+1.96*_se[quotaLead`num'] in `j'
    local ++j
}
#delimit ;
twoway line PointE time || rcap LBound UBound time,
scheme(s1mono) ytitle("log(Maternal Deaths)")
yline(0, lpattern(dot)) xline(0, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year 0 is omitted as the base case.");
graph export "$OUT/quotas/eventlnMDeath.eps", as(eps) replace;
#delimit cr


restore











*-------------------------------------------------------------------------------
*--- (4) Event Study Women in Parliament
*-------------------------------------------------------------------------------
preserve    
replace quotayear=. if quotatype=="Reserved" 
replace quotayear=. if quotayear==2013

drop quota
gen quota = year>=quotayear


gen prepost = year-(quotayear-1)
bys country: egen qcountry = max(quota)
sum prepost
local min = (-1*r(min))+1
local max = r(max)-1

foreach num of numlist 1(1)`min' {
    gen intyear = prepost==-`num'
    gen quotaLag`num' = qcountry*intyear
    drop intyear
}
foreach num of numlist 1(1)`max' {
    gen intyear = prepost==`num'
    gen quotaLead`num' = qcountry*intyear
    drop intyear
}
encode region, gen(rcode)


xtreg womparl i.year quotaLag* quotaLead* lnGDP, fe cluster(ccode)
local j = 1
gen time   = .
gen PointE = .
gen UBound = .
gen LBound = .
foreach num of numlist `min'(-1)1 {
    replace time   = -`num' in `j'
    replace PointE = _b[quotaLag`num'] in `j'
    replace LBound = _b[quotaLag`num']-1.96*_se[quotaLag`num'] in `j'
    replace UBound = _b[quotaLag`num']+1.96*_se[quotaLag`num'] in `j'
    local ++j
}
foreach num of numlist 1(1)`max' {
    replace time   =  `num' in `j'
    replace PointE = _b[quotaLead`num'] in `j'
    replace LBound = _b[quotaLead`num']-1.96*_se[quotaLead`num'] in `j'
    replace UBound = _b[quotaLead`num']+1.96*_se[quotaLead`num'] in `j'
    local ++j
}
#delimit ;
twoway line PointE time || rcap LBound UBound time,
scheme(s1mono) ytitle("Women in Parliament")
yline(0, lpattern(dot)) xline(0, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year 0 is omitted as the base case.");
graph export "$OUT/quotas/eventWomParl_cands.eps", as(eps) replace;
#delimit cr
drop time PointE UBound LBound prepost qcountry quotaLag* quotaLead* 

*-------------------------------------------------------------------------------
*--- (4) Event Study MMR
*-------------------------------------------------------------------------------



bys country: egen meanGDP = mean(lnGDP)
*sum meanGDP, d
*keep if meanGDP<=r(p75)

drop quota
gen quota = year>=quotayear

gen prepost = year-(quotayear-1)
bys country: egen qcountry = max(quota)
sum prepost
local min = (-1*r(min))+1
local max = r(max)-1

foreach num of numlist 1(1)`min' {
    gen intyear = prepost==-`num'
    gen quotaLag`num' = qcountry*intyear
    drop intyear
}
foreach num of numlist 1(1)`max' {
    gen intyear = prepost==`num'
    gen quotaLead`num' = qcountry*intyear
    drop intyear
}

xtreg lnMMRt1 i.year quotaLag* quotaLead* lnGDP, fe cluster(ccode)
local j = 1
gen time   = .
gen PointE = .
gen UBound = .
gen LBound = .
foreach num of numlist `min'(-1)1 {
    replace time   = -`num' in `j'
    replace PointE = _b[quotaLag`num'] in `j'
    replace LBound = _b[quotaLag`num']-1.96*_se[quotaLag`num'] in `j'
    replace UBound = _b[quotaLag`num']+1.96*_se[quotaLag`num'] in `j'
    local ++j
}
foreach num of numlist 1(1)`max' {
    replace time   =  `num' in `j'
    replace PointE = _b[quotaLead`num'] in `j'
    replace LBound = _b[quotaLead`num']-1.96*_se[quotaLead`num'] in `j'
    replace UBound = _b[quotaLead`num']+1.96*_se[quotaLead`num'] in `j'
    local ++j
}
#delimit ;
twoway line PointE time || rcap LBound UBound time,
scheme(s1mono) ytitle("log(Maternal Deaths)")
yline(0, lpattern(dot)) xline(0, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year 0 is omitted as the base case.");
graph export "$OUT/quotas/eventlnMDeath_cands.eps", as(eps) replace;
#delimit cr


restore
