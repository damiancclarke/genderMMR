/* quotaAnalysis.do v1.00        damiancclarke             yyyy-mm-dd:2017-01-06
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

Examine effects of quotas on female representation, and on maternal mortality. L
onger series of tests for econ paper.

*/

vers 11
clear all
set more off
cap log close

*-------------------------------------------------------------------------------
*--- (1) globals
*-------------------------------------------------------------------------------
global DAT "~/investigacion/2013/WorldMMR/Data/quotas"
global OUT "~/investigacion/2013/WorldMMR/Results/Quotas"
global LOG "~/investigacion/2013/WorldMMR/Log"

log using "$LOG/quotaAnalysis.txt", replace text

#delimit ;
local statform cells("count(label(N)) mean(fmt(2) label(Mean))
sd(fmt(2) label(Std.\ Dev.)) min(fmt(2) label(Min)) max(fmt(2) label(Max))");
#delimit cr

*-------------------------------------------------------------------------------
*--- (2) Generate series
*-------------------------------------------------------------------------------
use "$DAT/LangGender_dataset.dta", clear
drop if not_country==1
keep if womparl!=.

keep country womparl year tb_death_rate yr_sch democ health_exp contcode 
merge 1:1 country year using "$DAT/MMR"              , gen(_mergeMMR)
merge 1:1 country year using "$DAT/MMR_5yr"          , gen(_mergeMM5)
merge 1:1 country year using "$DAT/GDP"              , gen(_mergeGDP)
merge 1:1 country year using "$DAT/population"       , gen(_mergepop)
merge 1:1 country year using "$DAT/mechanisms"       , gen(_mergeMec)
merge 1:1 country year using "$DAT/fertility"        , gen(_mergeFer)
merge 1:1 country year using "$DAT/maleMortality5imp", gen(_mergemMo5)
drop if year<1960

merge m:1 country using "$DAT/quotasComplete"

*-------------------------------------------------------------------------------
*--- (3) Setup
*-------------------------------------------------------------------------------
keep if sh_sta_mmrt !=.&womparl !=.&ny_gdp_pcap_pp_kd !=.
rename MMR MMR5
gen lnGDP  = log(ny_gdp)

sort country year
bys country: ipolate yr_sch year, gen(yrs_school) epolate
bys country: ipolate health_exp year, gen(healthExpenditure) epolate
bys country: ipolate democ year, gen(democracy) epolate
bys country: gen  MMRt1   = sh_sta_mmrt[_n+2]
bys country: gen  teenPregt1    = teenPreg[_n+1]
bys country: gen  antenatalt1   = antenatal[_n+1]
bys country: gen  birthAttendt1 = birthAttend[_n+1]
bys country: gen  fertt1  = fert[_n]
bys country: gen  mmortt1 = maleMort[_n+1]
bys country: replace womparl = womparl[_n+1]
bys country: gen  TBt1    = tb_death_rate[_n+1]
bys country: egen pop     =mean(sp_pop_totl)
bys country: egen aveGDP  = mean(lnGDP)
keep if sh_sta_mmrt !=.&womparl !=.&ny_gdp_pcap_pp_kd !=.

gen lnMMRt1       = log(MMRt1)
gen lnTBt1        = log(TBt1)
gen lnfertt1      = log(fertt1)
gen lnmmortt1     = log(mmortt1)
gen womparxlnGDP  = womparl*lnGDP
replace quotayear = . if quotayear==2013
sum aveGDP, d
gen lowGDP = aveGDP<=r(p50)


gen quota = year>quotayear
encode country, gen(ccode)
encode region,  gen(rcode)
xtset ccode year

gen quotaRes     = quotatype!="Legislated Candidate Quota"&quota==1
gen quotaCand    = quotatype=="Legislated Candidate Quota"&quota==1
gen qVal         = quota*quotapercent1
gen qResVal      = quotaRes*quotapercent1
gen qCanVal      = quotaCan*quotapercent1
bys country: egen quotaResC = max(quotaRes)
bys country: egen quotaCandC = max(quotaCand)
foreach var of varlist quota quotaRes quotaCand {
    gen `var'p1_4=`var'==1&(year-quotayear>0)&(year-quotayear<=4)
    gen `var'p5_8=`var'==1&(year-quotayear>4)&(year-quotayear<=8)
    gen `var'p9_12=`var'==1&(year-quotayear>8)&(year-quotayear<=12)
    gen `var'p13=`var'==1&(year-quotayear>12)&quotayear!=.
             }
foreach var of varlist quotaRes quotaCand {
    gen `var'n1_4=`var'C==1&(quotayear-year>=2)&(quotayear-year<=4)
    gen `var'n5_8=`var'C==1&(quotayear-year>4)&(quotayear-year<=8)
    gen `var'n9_12=`var'C==1&(quotayear-year>8)&(quotayear-year<=12)
    gen `var'n13=`var'C==1&(quotayear-year>12)&quotayear!=.
                      }
drop quotaResC quotaCandC
gen quotaLower = quota==1&parl=="Bicameral"&(quotapercent1!=0&quotapercent1!=.)
gen quotaUpper = quota==1&parl=="Bicameral"&(quotapercent2!=0&quotapercent2!=.)
gen quotaUni   = quota==1&parl=="Unicameral"
replace quotapercent2=. if quotapercent2==0
gen reservedLower   = quotaLower*quotaRes
gen reservedUpper   = quotaUpper*quotaRes
gen reservedUni     = quotaUni*quotaRes
gen candidatesLower = quotaLower*quotaCand
gen candidatesUpper = quotaUpper*quotaCand
gen candidatesUni   = quotaUni*quotaCand

bys country: egen resCountry=max(quotaRes)
bys country: egen quotaSize=max(quotapercent1) if resCountry==1
gen qResVal2=quotaRes*quotaSize


lab var quota          "Any Quota"
lab var quotaRes       "Reserved Seats"
lab var quotaCand      "Legislated Candidate Quota"
lab var quotayear      "Year Quota Implemented"
lab var womparl        "% Women in Parliament"
lab var lnMMRt1        "ln(Maternal Mortality Ratio)"
lab var lnGDP          "ln(GDP per capita)"
lab var quotapercent1  "Quota Size (lower house/uni-cameral)"
lab var quotapercent2  "Quota Size (upper house)"
lab var quotap1_4      "Quota (1-4 years)"
lab var quotap5_8      "Quota (5-8 years)"
lab var quotap9_12     "Quota (9-12 years)"
lab var quotap13       "Quota (> 12  years)"
lab var quotaResp1_4   "Reserved Seats (1-4 years)"
lab var quotaResp5_8   "Reserved Seats (5-8 years)"
lab var quotaResp9_12  "Reserved Seats (9-12 years)"
lab var quotaResp13    "Reserved Seats ($>$ 12 years)"
lab var quotaCandn1_4  "Candidates (1-4 years prior)"
lab var quotaCandn5_8  "Candidates (5-8 years prior)"
lab var quotaCandn9_12 "Candidates (9-12 years prior)"
lab var quotaCandn13   "Candidates ($>$ 12 years prior)"
lab var quotaResn1_4   "Reserved Seats (1-4 years prior)"
lab var quotaResn5_8   "Reserved Seats (5-8 years prior)"
lab var quotaResn9_12  "Reserved Seats (9-12 years prior)"
lab var quotaResn13    "Reserved Seats ($>$ 12 years prior)"
lab var quotaCandp1_4  "Candidates (1-4 years)"
lab var quotaCandp5_8  "Candidates (5-8 years)"
lab var quotaCandp9_12 "Candidates (9-12 years)"
lab var quotaCandp13   "Candidates ($>$ 12 years)"
lab var qVal           "Gender Quota (%)"
lab var qResVal        "Quota (reserved, %)"
lab var qResVal2       "Quota Size (% of seats)"
lab var qCanVal        "Quota (candidates, %)"

/*
*-------------------------------------------------------------------------------
*--- (3a) Summary statistics
*-------------------------------------------------------------------------------
preserve
keep if resCountry==1&lnMMRt1!=.&womparl!=.&democ!=.
estpost sum womparl lnMMRt1 quotaSize lnGDP democ yrs_scho healthExp lnmmortt1
estout using "SumQuota.csv", replace label `statform'
restore

preserve
keep if resCountry==0&lnMMRt1!=.&womparl!=.&democ!=.
estpost sum womparl lnMMRt1 quotaSize lnGDP democ yrs_scho healthExp lnmmortt1
estout using "SumNoQuota.csv", replace label `statform'
restore

lab var womparl        "\% Women"
lab var lnMMRt1        "ln(MMR)"
lab var lnTBt1         "ln(TB)"
lab var quota          "Any Quotas"
lab var quotaRes       "Quota (reserved)"
lab var quotaCand      "Quota (candidates)"

*/
*-------------------------------------------------------------------------------
*--- (4) Main Regressions
*-------------------------------------------------------------------------------
local cntrl1  lnGDP i.democ
local cntrl1b lnGDP i.democ healthExp
local cntrl2  lnGDP i.democ healthExp yrs_school

eststo: xtreg lnMMRt1 i.year quotaRes lnGDP , fe cluster(ccode)
eststo: xtreg womparl quotaRes lnGDP i.year if e(sample)==1, fe cluster(ccode)

eststo: xtreg lnMMRt1 `cntrl1' i.year quotaRes, fe cluster(ccode)
eststo: xtreg womparl quotaRes `cntrl1' i.year if e(sample)==1, fe cluster(ccode)

eststo: xtreg lnMMRt1 quotaRes `cntrl2' i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg womparl quotaRes `cntrl2' i.year if e(sample)==1, fe cluster(ccode)

eststo: xtreg lnMMRt1 quotaRes `cntrl1' i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg womparl quotaRes `cntrl1' i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes lnGDP i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg womparl quotaRes lnGDP i.year if e(sample)==1, fe cluster(ccode)

sum quotaRes
local qr = string(r(mean), "%5.3f")

#delimit ;
esttab est1 est3 est5 est2 est4 est6 using "$OUT/quotaDifDif.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes _cons)
collabels(none) label mlabels(, none) replace
mgroups("ln(Maternal Mortality Ratio)" "\% Women in Parliament", pattern(1 0 0 1 0 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Difference-in-differences estimates of the effect of Reserved Seats"\label{qDD})
postfoot("GDP Control & Y & Y & Y & Y & Y & Y \\"
         "Democracy Indicators &  & Y & Y &  & Y & Y \\"
         "Health and Educ Controls &  &  & Y &  &  & Y \\"
         "\bottomrule\multicolumn{7}{p{15.2cm}}{\begin{footnotesize} Each "
         "regression includes country and year fixed effects and clusters "
         "standard errors by country. A number of (small) countries do not"
         "have a democracy score from Polity IV, and additional countries "
         "do not have educational or health care spending control data for"
         "all years. Refer to table \ref{qDDsamp} for the estimates       "
         "consistently using the sample where all covariates are available."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr

#delimit ;
esttab est9 est7 est5 est10 est8 est6 using "$OUT/quotaDifDif-samp.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes _cons)
collabels(none) label mlabels(, none) replace
mgroups("ln(Maternal Mortality Ratio)" "\% Women in Parliament", pattern(1 0 0 1 0 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Difference-in-differences estimates of the effect of Reserved Seats"\label{qDDsamp})
postfoot("GDP Control & Y & Y & Y & Y & Y & Y \\"
         "Democracy Indicators &  & Y & Y &  & Y & Y \\"
         "Health and Educ Controls &  &  & Y &  &  & Y \\"
         "\bottomrule\multicolumn{7}{p{15.2cm}}{\begin{footnotesize} Refer to"
         "table \ref{qDD} for notes."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


preserve
drop if country=="China"|country=="India"
local wt [aw=pop]
eststo: xtreg lnMMRt1 i.year quotaRes lnGDP  `wt', fe cluster(ccode)
eststo: xtreg womparl quotaRes lnGDP i.year if e(sample)==1 `wt', fe cluster(ccode)
eststo: xtreg lnMMRt1 `cntrl1' i.year quotaRes `wt', fe cluster(ccode)
eststo: xtreg womparl quotaRes `cntrl1' i.year if e(sample)==1 `wt', fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes `cntrl2' i.year if e(sample)==1 `wt', fe cluster(ccode)
eststo: xtreg womparl quotaRes `cntrl2' i.year if e(sample)==1 `wt', fe cluster(ccode)
#delimit ;
esttab est1 est3 est5 est2 est4 est6 using "$OUT/quotaDifDif-wt.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes _cons)
collabels(none) label mlabels(, none) replace
mgroups("ln(Maternal Mortality Ratio)" "\% Women in Parliament", pattern(1 0 0 1 0 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Difference-in-differences effect of Reserved Seats (Population Weighting)"\label{qDD})
postfoot("GDP Control & Y & Y & Y & Y & Y & Y \\"
         "Democracy Indicators &  & Y & Y &  & Y & Y \\"
         "Health and Educ Controls &  &  & Y &  &  & Y \\"
         "\bottomrule\multicolumn{7}{p{15.2cm}}{\begin{footnotesize} Refer to  "
         "table \ref{qDD} for notes. Population weights are used rather than   "
         "unweighted regressions.  India and China are removed from the sample "
         "otherwise regression results are largely driven by these two         "
         "countries which have a population an order of magnitude larger than  "
         "the remaining countries."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
restore
estimates clear

*-------------------------------------------------------------------------------
*--- (5) Placebo
*-------------------------------------------------------------------------------
eststo: xtreg lnmmortt1 i.year quotaRes lnGDP , fe cluster(ccode)
eststo: xtreg womparl quotaRes lnGDP i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg lnmmortt1 `cntrl1' i.year quotaRes, fe cluster(ccode)
eststo: xtreg womparl quotaRes `cntrl1' i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg lnmmortt1 quotaRes `cntrl2' i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg womparl quotaRes `cntrl2' i.year if e(sample)==1, fe cluster(ccode)

#delimit ;
esttab est1 est3 est5 est2 est4 est6 using "$OUT/quotaPlacebo.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes _cons)
collabels(none) label mlabels(, none) replace
mgroups("ln(Male Mortality Ratio) 15-49" "\% Women in Parliament", pattern(1 0 0 1 0 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Placebo Diff-in-diffs estimates of the effect of Reserved Seats"\label{qDD})
postfoot("GDP Control & Y & Y & Y & Y & Y & Y \\"
         "Democracy Indicators &  & Y & Y &  & Y & Y \\"
         "Health and Educ Controls &  &  & Y &  &  & Y \\"
         "\bottomrule\multicolumn{7}{p{15.2cm}}{\begin{footnotesize} Columns "
         "1-3 replace the logarithm of maternal mortality rates with the log "
         "of male mortality in the same age group (15-49). All other details "
         "follow those described in  table \ref{qDDsamp}."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

*-------------------------------------------------------------------------------
*--- (6) Mechanism
*-------------------------------------------------------------------------------
eststo: xtreg antenatalt1 i.year quotaRes lnGDP , fe cluster(ccode)
eststo: xtreg birthAttendt1 quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg antenatalt1 `cntrl1' i.year quotaRes, fe cluster(ccode)
eststo: xtreg birthAttendt1 quotaRes `cntrl1' i.year, fe cluster(ccode)
eststo: xtreg antenatalt1 quotaRes `cntrl2' i.year, fe cluster(ccode)
eststo: xtreg birthAttendt1 quotaRes `cntrl2' i.year, fe cluster(ccode)

#delimit ;
esttab est1 est3 est5 est2 est4 est6 using "$OUT/quotaMechanism.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes _cons)
collabels(none) label mlabels(, none) replace
mgroups("Antenatal Care Coverage" "Attended Births Coverage", pattern(1 0 0 1 0 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Diff-in-diffs estimates of the effect of Reserved Seats on Intermediate Outcomes"\label{mDD})
postfoot("GDP Control & Y & Y & Y & Y & Y & Y \\"
         "Democracy Indicators &  & Y & Y &  & Y & Y \\"
         "Health and Educ Controls &  &  & Y &  &  & Y \\"
         "\bottomrule\multicolumn{7}{p{14.6cm}}{\begin{footnotesize} Antenatal"
         "coverage and birth attendance are accessed from the World Bank     "
         "databank."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr




*-------------------------------------------------------------------------------
*--- (4a) Event Study Women in Parliament -- Reserved Seats
*-------------------------------------------------------------------------------
replace quotayear=. if quotatype=="Legislated Candidate Quota"
replace quotayear=. if quotayear==2013

drop quota
gen quota = year>=quotayear

gen prepost = year-(quotayear-1) if democ!=.

bys country: egen qcountry = max(quota)
sum prepost
local min = (-1*r(min))
local max = r(max)-1

foreach num of numlist 2(1)`min' {
    gen intyear = prepost==-`num'
    gen quotaLag`num' = qcountry*intyear
    drop intyear
}
foreach num of numlist 0(1)`max' {
    gen intyear = prepost==`num'
    gen quotaLead`num' = qcountry*intyear
    drop intyear
}


xtreg womparl i.year quotaLag* quotaLead* lnGDP i.democ, fe cluster(ccode)
local j = 1
gen time   = .
gen PointE = .
gen UBound = .
gen LBound = .
foreach num of numlist `min'(-1)2 {
    replace time   = -`num' in `j'
    replace PointE = _b[quotaLag`num'] in `j'
    replace LBound = _b[quotaLag`num']-1.96*_se[quotaLag`num'] in `j'
    replace UBound = _b[quotaLag`num']+1.96*_se[quotaLag`num'] in `j'
    local ++j
}
replace time = -1 in `j'
replace PointE = 0 in `j'
replace LBound = 0 in `j'
replace UBound = 0 in `j'
local ++j
foreach num of numlist 0(1)`max' {
    replace time   =  `num' in `j'
    replace PointE = _b[quotaLead`num'] in `j'
    replace LBound = _b[quotaLead`num']-1.96*_se[quotaLead`num'] in `j'
    replace UBound = _b[quotaLead`num']+1.96*_se[quotaLead`num'] in `j'
    local ++j
}
    
#delimit ;
twoway line PointE time || rcap LBound UBound time,
scheme(s1mono) ytitle("Women in Parliament")
yline(0, lpattern(dash)) xline(-1, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year -1 is omitted as the base case.");
graph export "$OUT/eventWomParl.eps", as(eps) replace;
#delimit cr
drop time PointE UBound LBound prepost qcountry quotaLag* quotaLead*

*-------------------------------------------------------------------------------
*--- (4b) Event Study MMR -- Reserved Seats
*-------------------------------------------------------------------------------
bys country: egen meanGDP = mean(lnGDP)

drop quota
gen quota = year>=quotayear

gen prepost = year-(quotayear-1) if democ!=.

bys country: egen qcountry = max(quota)
sum prepost
local min = (-1*r(min))
local max = r(max)-1

foreach num of numlist 2(1)`min' {
    gen intyear = prepost==-`num'
    gen quotaLag`num' = qcountry*intyear
    drop intyear
}
foreach num of numlist 0(1)`max' {
    gen intyear = prepost==`num'
    gen quotaLead`num' = qcountry*intyear
    drop intyear
}

xtreg lnMMRt1 i.year quotaLag* quotaLead* lnGDP i.democ, fe cluster(ccode)
local j = 1
gen time   = .
gen PointE = .
gen UBound = .
gen LBound = .
foreach num of numlist `min'(-1)2 {
    replace time   = -`num' in `j'
    replace PointE = _b[quotaLag`num'] in `j'
    replace LBound = _b[quotaLag`num']-1.96*_se[quotaLag`num'] in `j'
    replace UBound = _b[quotaLag`num']+1.96*_se[quotaLag`num'] in `j'
    local ++j
}
replace time = -1 in `j'
replace PointE = 0 in `j'
replace LBound = 0 in `j'
replace UBound = 0 in `j'
local ++j
foreach num of numlist 0(1)`max' {
    replace time   =  `num' in `j'
    replace PointE = _b[quotaLead`num'] in `j'
    replace LBound = _b[quotaLead`num']-1.96*_se[quotaLead`num'] in `j'
    replace UBound = _b[quotaLead`num']+1.96*_se[quotaLead`num'] in `j'
    local ++j
}

#delimit ;
twoway line PointE time || rcap LBound UBound time,
scheme(s1mono) ytitle("log(Maternal Deaths)")
yline(0, lpattern(dash)) xline(-1, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year -1 is omitted as the base case.");
graph export "$OUT/eventlnMDeath.eps", as(eps) replace;
#delimit cr
drop time PointE UBound LBound prepost qcountry quotaLag* quotaLead*
                                                    
*-------------------------------------------------------------------------------
*--- (4c) Event Study Male mortality -- Reserved Seats
*-------------------------------------------------------------------------------
drop quota
gen quota = year>=quotayear

gen prepost = year-(quotayear-1) if democ!=.

bys country: egen qcountry = max(quota)
sum prepost
local min = (-1*r(min))
local max = r(max)-1

foreach num of numlist 2(1)`min' {
    gen intyear = prepost==-`num'
    gen quotaLag`num' = qcountry*intyear
    drop intyear
}
foreach num of numlist 0(1)`max' {
    gen intyear = prepost==`num'
    gen quotaLead`num' = qcountry*intyear
    drop intyear
}

xtreg lnmmortt1 i.year quotaLag* quotaLead* lnGDP i.democ, fe cluster(ccode)
local j = 1
gen time   = .
gen PointE = .
gen UBound = .
gen LBound = .
foreach num of numlist `min'(-1)2 {
    replace time   = -`num' in `j'
    replace PointE = _b[quotaLag`num'] in `j'
    replace LBound = _b[quotaLag`num']-1.96*_se[quotaLag`num'] in `j'
    replace UBound = _b[quotaLag`num']+1.96*_se[quotaLag`num'] in `j'
    local ++j
}
replace time = -1 in `j'
replace PointE = 0 in `j'
replace LBound = 0 in `j'
replace UBound = 0 in `j'
local ++j
foreach num of numlist 0(1)`max' {
    replace time   =  `num' in `j'
    replace PointE = _b[quotaLead`num'] in `j'
    replace LBound = _b[quotaLead`num']-1.96*_se[quotaLead`num'] in `j'
    replace UBound = _b[quotaLead`num']+1.96*_se[quotaLead`num'] in `j'
    local ++j
}

#delimit ;
twoway line PointE time || rcap LBound UBound time,
scheme(s1mono) ytitle("log(Male Mortality)")
yline(0, lpattern(dash)) xline(-1, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year -1 is omitted as the base case.");
graph export "$OUT/eventlnmmort.eps", as(eps) replace;
#delimit cr
drop time PointE UBound LBound prepost qcountry quotaLag* quotaLead*
                                                    
