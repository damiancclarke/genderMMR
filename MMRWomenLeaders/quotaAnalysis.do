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
keep country womparl year tb_death_rate
keep if year>=1990
merge 1:1 country year using "MMR"         , gen(_mergeMMR)
merge 1:1 country year using "GDP"         , gen(_mergeGDP)
merge 1:1 country year using "population"  , gen(_mergepop)
drop if year<1990
merge m:1 country using quotasComplete
bys country: egen pop=mean(sp_pop_totl)

*-------------------------------------------------------------------------------
*--- (3) Basic regressions
*-------------------------------------------------------------------------------
keep if sh_sta_mmrt !=.&womparl !=.&ny_gdp_pcap_pp_kd !=.
sort country year
bys country: gen MMRt1 = sh_sta_mmrt[_n+2]
bys country: gen TBt1  = tb_death_rate[_n+1]
gen lnMMRt1      = log(MMRt1)
gen lnGDP        = log(ny_gdp)
gen lnTBt1       = log(TBt1)
gen womparxlnGDP = womparl*lnGDP
replace quotayear=. if quotayear==2013

gen quota = year>quotayear
encode country, gen(ccode)
encode region, gen(rcode)
xtset ccode year

xtreg lnMMRt1 womparl               i.year, fe cluster(ccode)
xtreg lnMMRt1 womparl lnGDP         i.year, fe cluster(ccode)
xtreg lnMMRt1 womparl lnGDP womparx i.year, fe cluster(ccode)


gen quotaxlnGDP  = quota*lnGDP
gen quotaRes     = quotatype!="Legislated Candidate Quota"&quota==1
gen quotaCand    = quotatype=="Legislated Candidate Quota"&quota==1
gen quotaRxlnGDP = quotaRes*lnGDP
gen quotaCxlnGDP = quotaCand*lnGDP
gen qVal         = quota*quotapercent1
gen qResVal      = quotaRes*quotapercent1
gen qCanVal      = quotaCan*quotapercent1


xtreg womparl quota lnGDP i.year, fe cluster(ccode)
xtreg womparl quotaRes lnGDP i.year, fe cluster(ccode)
xtreg womparl qResVal lnGDP i.year, fe cluster(ccode)
xtreg womparl quotaCan i.year, fe cluster(ccode)
xtreg womparl qCanVal i.year, fe cluster(ccode)


xtreg lnMMRt1 quotaRes lnGDP i.year, fe cluster(ccode)
xtreg lnMMRt1 qResVal  lnGDP i.year , fe cluster(ccode)
xtreg lnMMRt1 quotaCan lnGDP i.year, fe cluster(ccode)

xtreg lnTBt1 quotaRes lnGDP  i.year, fe cluster(ccode)
xtreg lnTBt1 quotaCan lnGDP  i.year, fe cluster(ccode)
*ivreg2 lnMMRt1 lnGDP i.ccode i.year (womparl womparx = quota quotax), /*
**/ partial(i.ccode i.year) cluster(ccode)

qui xtreg lnMMRt1  quota lnGDP i.year, fe cluster(ccode)
preserve
keep if e(sample)==1

eststo: xtreg womparl quota lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quota lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  quota lnGDP i.year, fe cluster(ccode)

eststo: xtreg womparl quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  quotaRes lnGDP i.year, fe cluster(ccode)

eststo: xtreg womparl quotaCan lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaCan lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  quotaCan lnGDP i.year, fe cluster(ccode)
lab var womparl "\% Women"
lab var lnMMRt1 "ln(MMR)"
lab var lnTBt1 "ln(TB)"
lab var quota "Any Quotas"
lab var quotaRes "Quota (reserved)"
lab var quotaCan "Quota (candidates)"

#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/Quota.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(quota quotaCand quotaRes) title("Gender Quotas, Women in Parliament and Health")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{10}{p{20.8cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation.  Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Candidate quota"
         " refers to those countries where submitted candidate lists must comply"
         " with a minimum proportion of women (or each gender), but where there "
         "are no guarantees for representation in parliament. Standard errors   "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

qui xtreg lnMMRt1  quota lnGDP i.year, fe cluster(ccode)

eststo: xtreg womparl qVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 qVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  qVal lnGDP i.year, fe cluster(ccode)

eststo: xtreg womparl qResVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 qResVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  qResVal lnGDP i.year, fe cluster(ccode)

eststo: xtreg womparl qCanVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 qCanVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  qCanVal lnGDP i.year, fe cluster(ccode)
lab var womparl "\% Women"
lab var lnMMRt1 "ln(MMR)"
lab var lnTBt1 "ln(TB)"
lab var qVal    "Gender Quota (\%)"
lab var qResVal "Quota (reserved, \%)"
lab var qCanVal "Quota (candidates, \%)"

#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/QuotaVals.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(qVal qCanVal qResVal) title("Gender Quotas, Women in Parliament and Health")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{10}{p{20.8cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation.  Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Candidate quota"
         " refers to those countries where submitted candidate lists must comply"
         " with a minimum proportion of women (or each gender), but where there "
         "are no guarantees for representation in parliament. Standard errors   "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


eststo: xtreg womparl quota lnGDP i.year, fe cluster(ccode)
eststo: xtreg MMRt1 quota lnGDP i.year, fe cluster(ccode)
eststo: xtreg TBt1  quota lnGDP i.year, fe cluster(ccode)

eststo: xtreg womparl quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg MMRt1 quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg TBt1  quotaRes lnGDP i.year, fe cluster(ccode)

eststo: xtreg womparl quotaCan lnGDP i.year, fe cluster(ccode)
eststo: xtreg MMRt1 quotaCan lnGDP i.year, fe cluster(ccode)
eststo: xtreg TBt1  quotaCan lnGDP i.year, fe cluster(ccode)
lab var MMRt1 "MMR"
lab var TBt1 "TB"

#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/QuotaLevels.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(quota quotaCand quotaRes) title("Gender Quotas, Women in Parliament and Health")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{10}{p{20.8cm}}{\begin{footnotesize} Both MMR   "
         "and TB are recorded as deaths per 100,000 events.  For MMR, this is    "
         "deaths per 100,000 live births, while for TB this is deaths per 100,000"
         " population. Standard errors   "
         "clustered at the level of the country are reported in parentheses.     "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

restore

*-------------------------------------------------------------------------------
*--- (4a) Event Study Women in Parliament
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
*--- (4b) Event Study MMR
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

drop time PointE UBound LBound prepost qcountry quotaLag* quotaLead* 

*-------------------------------------------------------------------------------
*--- (4c) Event Study TB
*-------------------------------------------------------------------------------
drop if year>2013
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

xtreg lnTBt1 i.year quotaLag* quotaLead* lnGDP, fe cluster(ccode)
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
scheme(s1mono) ytitle("log(Tuberculosis)")
yline(0, lpattern(dot)) xline(0, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year 0 is omitted as the base case.");
graph export "$OUT/quotas/eventlnTB.eps", as(eps) replace;
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


*-------------------------------------------------------------------------------
*--- (5) Descriptive plots
*-------------------------------------------------------------------------------
preserve
collapse quotayear rcode, by(country quotatype)
gen num = 1
collapse (sum) num, by(quotayear)
sort quotayear
gen qCountries = sum(num)
drop if quotayear==.
#delimit ;
line qCountries quotayear, scheme(s1mono) lcolor(red) lwidth(thick)
ytitle("Total Number of Countries with a Gender Quota") xtitle("Year")
;
graph export $OUT/quotas/quotaTime.eps, as(eps) replace;
#delimit cr

restore



collapse quotayear rcode, by(country quotatype)
gen num = 1
collapse (sum) num, by(quotayear rcode)
bys rcode (quotayear): gen qCountries = sum(num)
drop if quotayear==.
tempfile quotas
save `quotas'

clear
set obs 7
gen rcode = _n
expand 24
bys rcode: gen quotayear = _n+1988
merge 1:1 quotayear rcode using `quotas'
drop if rcode==5
replace num = 0 if num==.
drop qCountries
bys rcode (quotayear): gen qCountries = sum(num)
keep rcode quotayear qCountries


outsheet using quotaTimes.csv, comma names replace
