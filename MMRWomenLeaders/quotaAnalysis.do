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

keep country womparl year tb_death_rate yr_sch democ health_exp contcode MMR_b_DHS100
keep if year>=1990
merge 1:1 country year using "MMR"           , gen(_mergeMMR)
merge 1:1 country year using "MMR_5yr"       , gen(_mergeMM5)
merge 1:1 country year using "GDP"           , gen(_mergeGDP)
merge 1:1 country year using "population"    , gen(_mergepop)
merge 1:1 country year using "mechanisms"    , gen(_mergeMec)
merge 1:1 country year using "fertility"     , gen(_mergeFer)
*merge 1:1 country year using "malemortality" , gen(_mergemMo)
merge 1:1 country year using "maleMortality5imp", gen(_mergemMo5)
drop if year<1960

merge m:1 country using quotasComplete
*replace quotayear = 2005 if country=="Uganda"


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
bys country: gen  MMRt1DHS= MMR_b_DHS100[_n+2]
bys country: gen  fertt1  = fert[_n+2]
bys country: gen  mmortt1 = maleMort[_n+1]
bys country: gen  TBt1    = tb_death_rate[_n+1]
bys country: egen pop     =mean(sp_pop_totl)
bys country: egen aveGDP  = mean(lnGDP)

gen lnMMRt1       = log(MMRt1)
gen lnMMRt1DHS    = log(MMRt1DHS)
gen lnTBt1        = log(TBt1)
gen lnfertt1      = log(fertt1)
gen lnmmortt1     = log(mmortt1)
gen womparxlnGDP  = womparl*lnGDP
replace quotayear = . if quotayear==2013
sum aveGDP, d
gen lowGDP = aveGDP<=r(p50)


gen quota = year>quotayear
encode country, gen(ccode)
encode region, gen(rcode)
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
lab var womparl        "\% Women in Parliament"
lab var lnMMRt1        "ln(Maternal Mortality Ratio)"
lab var lnGDP          "ln(GDP per capita)"
lab var quotapercent1  "Quota Size (lower house/uni-cameral)"
lab var quotapercent2  "Quota Size (upper house)"
lab var quotap1_4      "Quota (1-4 years)"
lab var quotap5_8      "Quota (5-8 years)"
lab var quotap9_12     "Quota (9-12 years)"
lab var quotap13       "Quota ($>$ 12  years)"
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
lab var qVal           "Gender Quota (\%)"
lab var qResVal        "Quota (reserved, \%)"
lab var qResVal2       "Quota (reserved, \%)"
lab var qCanVal        "Quota (candidates, \%)"
preserve
bys country: egen qcountry = max(quota)
collapse MMRt1 lnMMRt1, by(qcountry year)
keep if MMRt1!=.

#delimit ;
twoway line lnMMRt1 year if qcountry==1, lcolor(blue) lwidth(thick)
    || line lnMMRt1 year if qcountry==0, lcolor(red) lpattern(dash)
scheme(s1mono) legend(lab(1 "Quota Countries") lab(2 "Non-Quota Countries"))
ytitle("log(MMR)") xtitle("Year");
graph export "$OUT/quotas/basicTrends.eps", replace;

twoway line MMRt1 year if qcountry==1, lcolor(blue) lwidth(thick)
    || line MMRt1 year if qcountry==0, lcolor(red) lpattern(dash)
scheme(s1mono) legend(lab(1 "Quota Countries") lab(2 "Non-Quota Countries"))
ytitle("Maternal Mortality Ratio)") xtitle("Year");
graph export "$OUT/quotas/basicTrendsMMR.eps", replace;

#delimit cr
restore


*-------------------------------------------------------------------------------
*--- (3a) Summary statistics
*-------------------------------------------------------------------------------
/*
preserve
keep if lnMMRt1!=.
#delimit ;
estpost sum quota quotaRes quotaCand quotayear womparl lnMMRt1 lnGDP
quotapercent1 quotapercent2;
#delimit cr
estout using "quotas/sum_all.tex", replace label style(tex) `statform'
restore

preserve
bys country: egen qtcountry=mean(quota)
keep if lnMMRt1!=.&qtcountry>0&qtcountry!=.
#delimit ;
estpost sum quota quotaRes quotaCand quotayear womparl lnMMRt1 lnGDP
quotapercent1 quotapercent2;
#delimit cr
estout using "quotas/sum_quota.tex", replace label style(tex) `statform'
restore

preserve
bys country: egen qtcountry=mean(quotaRes)
keep if lnMMRt1!=.&qtcountry>0&qtcountry!=.
#delimit ;
estpost sum quota quotaRes quotaCand quotayear womparl lnMMRt1 lnGDP
quotapercent1 quotapercent2;
#delimit cr
estout using "quotas/sum_quotaRes.tex", replace label style(tex) `statform'
restore

preserve
bys country: egen qtcountry=mean(quotaCand)
keep if lnMMRt1!=.&qtcountry>0&qtcountry!=.
#delimit ;
estpost sum quota quotaRes quotaCand quotayear womparl lnMMRt1 lnGDP
quotapercent1 quotapercent2;
#delimit cr
estout using "quotas/sum_Cand.tex", replace label style(tex) `statform'
restore

preserve
bys country: egen qtcountry=mean(quota)
keep if lnMMRt1!=.&qtcountry==0
#delimit ;
estpost sum quota quotaRes quotaCand quotayear womparl lnMMRt1 lnGDP
quotapercent1 quotapercent2;
#delimit cr
estout using "quotas/sum_Noquota.tex", replace label style(tex) `statform'
restore

exit
*/
/*
preserve
*bys country: egen qtcountry=mean(quotaRes)
*keep if lnMMRt1!=.&qtcountry>0&qtcountry!=.
gen quotaY  = quotayear if quotaRes==1
gen quotap1 = quotapercent1 if quotaRes==1
gen quotap2 = quotapercent2 if quotaRes==1
lab var quotaY   "Year Quota Implemented"
lab var quotap1  "Quota Size (lower house/uni-cameral)"
lab var quotap2  "Quota Size (upper house)"
lab var lnTBt1         "ln(Tuberculosis Mortality)"
keep if lnMMRt1!=.
estpost sum quotaRes quotaY womparl lnMMRt1 lnTBt1 lnGDP quotap1 quotap2
estout using "quotas/sum_quotaRes.tex", replace label style(tex) `statform'
restore
exit
*/
    
lab var womparl        "\% Women"
lab var lnMMRt1        "ln(MMR)"
lab var lnTBt1         "ln(TB)"
lab var quota          "Any Quotas"
lab var quotaRes       "Quota (reserved)"
lab var quotaCand      "Quota (candidates)"


xtreg lnMMRt1 quotaRes lnGDP i.year, fe cluster(ccode)
xtreg lnMMRt1 quotaRes quotaCand lnGDP i.year, fe cluster(ccode)

xtreg lnMMRt1 quotaRes quotaCand lnGDP yrs_school democ i.year, fe cluster(ccode)
xtreg lnMMRt1 quotaRes quotaCand lnGDP yrs_school health_exp democ i.year i.contcode#i.year, fe cluster(ccode)
xtreg lnMMRt1 quotaRes quotaCand lnGDP i.year if e(sample)==1, fe cluster(ccode)


*-------------------------------------------------------------------------------
*--- (4) Main Regressions
*-------------------------------------------------------------------------------
#delimit ;
local opts replace cells(b(star fmt(%-9.3f)) ci(fmt(%-9.2f)  par("(" "," ")"))
                         p(fmt(%-9.3f) par([\emph{p}= ]) )) stats 
              (r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
              starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label;
#delimit cr
*local cntrl  lnGDP i.democ health_e
local cntrl  lnGDP i.democ

eststo: xtreg lnMMRt1 quotaRes `cntrl' i.year, fe cluster(ccode)
eststo: xtreg womparl quotaRes `cntrl' i.year if e(sample), fe cluster(ccode)
sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")

eststo: xtreg lnMMRt1 qResVal2 `cntrl' i.year, fe cluster(ccode)
eststo: xtreg womparl qResVal2 `cntrl' i.year if e(sample), fe cluster(ccode)
sum qResVal if e(sample)==1
local qr2 = string(r(mean), "%5.3f")

#delimit ;
esttab est2 est1 est4 est3 using "$OUT/table2.tex", `opts'
keep(quotaRes qResVal2 _cons)
title("Difference-in-differences estimates of effect of reserved seats")
mgroups("Has Reserved Seats" "Proportion of Reserved Seats", pattern(1 0 1 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{5}{p{14.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         "seats reserved for women, and if so the year of implementation of the "
         "the quota law. The proportion of     "
         "countries with reserved seats is `qr'.  The average proportion of     "
         "reserved seats in these countries is `qr2'.  Full summary statistics are in   "
         "table 1. Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Standard errors"
         "are clustered at the level of the country. 95\% confidence intervals  "
         "are reported in round brackets, and \emph{p}-values associated with   "
         "each coefficient are in square brackets."
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

preserve
drop if country=="China"
local wt [aw=pop]
eststo: xtreg lnMMRt1 quotaRes `cntrl' i.year `wt', fe cluster(ccode)
eststo: xtreg womparl quotaRes `cntrl' i.year if e(sample) `wt', fe cluster(ccode)
sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")

eststo: xtreg lnMMRt1 qResVal2 `cntrl' i.year `wt', fe cluster(ccode)
eststo: xtreg womparl qResVal2 `cntrl' i.year if e(sample) `wt', fe cluster(ccode)
sum qResVal if e(sample)==1
local qr2 = string(r(mean), "%5.3f")

#delimit ;
esttab est2 est1 est4 est3 using "$OUT/table2-pop.tex", `opts'
keep(quotaRes qResVal2 _cons)
title("Difference-in-differences estimates of effect of reserved seats")
mgroups("Has Reserved Seats" "Proportion of Reserved Seats", pattern(1 0 1 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{5}{p{14.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         "seats reserved for women, and if so the year of implementation of the "
         "the quota law. The proportion of     "
         "countries with reserved seats is `qr'.  The average proportion of     "
         "reserved seats in these countries is `qr2'.  Full summary statistics are in   "
         "table 1. Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Standard errors"
         "are clustered at the level of the country. 95\% confidence intervals  "
         "are reported in round brackets, and \emph{p}-values associated with   "
         "each coefficient are in square brackets."
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear
restore

local nquint 7

xtile quotaQuant = quotaSize, nquantiles(`nquint')
foreach num of numlist 1(1)`nquint' {
    gen qResQ`num'= quotaRes==1&quotaQuant==`num'
}

xtreg lnMMRt1 `cntrl' i.year qResQ*, fe cluster(ccode)
gen MMRest = .
gen MMRub  = .
gen MMRlb  = .
gen quant  = .
foreach num of numlist 1(1)`nquint' {
    replace quant=`num' in `num'
    replace MMRest = _b[qResQ`num'] in `num'
    replace MMRlb  = _b[qResQ`num']-1.96*_se[qResQ`num'] in `num'
    replace MMRub  = _b[qResQ`num']+1.96*_se[qResQ`num'] in `num'
}

xtreg womparl `cntrl' i.year qResQ*, fe cluster(ccode)
gen WPest  = .
gen WPub   = .
gen WPlb   = .
gen quant2 = .
foreach num of numlist 1(1)`nquint' {
    replace quant2 = `num'.1 in `num'
    replace WPest  = _b[qResQ`num'] in `num'
    replace WPlb   = _b[qResQ`num']-1.96*_se[qResQ`num'] in `num'
    replace WPub   = _b[qResQ`num']+1.96*_se[qResQ`num'] in `num'
}

local m1 mcolor(blue) msymbol(Oh)
local m2 mcolor(red) msymbol(S)
local d {&Delta}

#delimit ;
twoway scatter MMRest quant, yaxis(1) ylabel(0.8(-0.2)-0.8, axis(1)) `m1' ||
       scatter WPest quant2, yaxis(2) ylabel(-40(10)40, axis(2)) `m2' ||
       rcap MMRlb MMRub quant, yaxis(1) lcolor(blue) || 
       rcap WPlb WPub  quant2, yaxis(2) lcolor(red) xlabel(1(1)`nquint')  
scheme(s1mono) yline(0, lcolor(black) lpattern(dash))
ytitle("`d' ln(Maternal Mortality)", axis(1))
ytitle("`d' % Women in Parliament", axis(2))
xtitle("Size of Reserved Seat Quota (Percentiles)")
legend(order(1 "`d' ln(Maternal Mortality)" 3 "`d' % Women in Parliament"))
;
#delimit cr
graph export "$OUT/doseResponse.eps", as(eps) replace



exit


local quotarD quotaResn13 quotaResn9_12 quotaResn5_8 quotaResn1_4 quotaResp1_4 quotaResp5_8 quotaResp9_12 quotaResp13
eststo: xtreg lnMMRt1 `quotarD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg womparl `quotarD' `cntrl' i.year if e(sample), fe cluster(ccode)


#delimit ;
esttab est2 est1 using "$OUT/table3.tex",
`opts' keep(`quotarD')
title("Dynamic Effects of Gender Quotas on Women in Parliament and Health")
postfoot("Country and Year FE &Y&Y \\                                   "
         "\bottomrule\multicolumn{3}{p{11.4cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation.  Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Years of the   "
         "independent variables refer to the total time period for which the    "
         "quota has been in place (or preceding its implementation), and are    "
         "mutually exclusive. Standard errors"
         "are clustered at the level of the country. 95\% confidence intervals  "
         "are reported in round brackets, and \emph{p}-values associated with   "
         "each coefficient are in square brackets."
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


eststo: xtreg lnTBt1 quotaRes `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1 qResVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1 `quotarD' `cntrl' i.year, fe cluster(ccode)
#delimit ;
esttab est1 est2 est3 using "$OUT/table4.tex",
`opts' keep(quotaRes qResVal `quotarD')
title("Placebo Tests with Tuberculosis Mortality")
postfoot("Country and Year FE &Y&Y&Y \\                                         "
         "\bottomrule\multicolumn{4}{p{12.6cm}}{\begin{footnotesize} Columns    "
         "1 to 3 replicate results from tables 2 and 3, replacing the natural   "
         "logarithm of the maternal mortality ratio with the natural logarithm  "
         "of Tuberculosis mortality. Standard errors are clustered at the level "
         " of the country. 95\% confidence intervals are reported in round      "
         "brackets, and \emph{p}-values associated with each coefficient are in "
         "square brackets. ***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


lab var lnMMRt1DHS "ln(MMR)"

eststo: xtreg lnMMRt1DHS quotaRes `cntrl' i.year, fe cluster(ccode)
eststo: xtreg womparl quotaRes `cntrl' i.year if e(sample), fe cluster(ccode)
sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")

eststo: xtreg lnMMRt1DHS qResVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg womparl qResVal lnGDP i.year if e(sample), fe cluster(ccode)
sum qResVal if e(sample)==1
local qr2 = string(r(mean), "%5.3f")

#delimit ;
esttab est2 est1 est4 est3 using "$OUT/table2_DHS.tex", `opts'
keep(quotaRes qResVal _cons)
title("Difference-in-differences estimates of effect of reserved seats (DHS Data)")
mgroups("Has Reserved Seats" "Proportion of Reserved Seats", pattern(1 0 1 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{5}{p{14.2cm}}{\begin{footnotesize} Results    "
         "from table 2 are replicated, however using alternative data generated "
         "from DHS microdata.  The proportion of     "
         "countries with reserved seats is `qr'.  The average proportion of     "
         "reserved seats in these countries is `qr2'.  Refer to table 2 for     "
         "additional notes."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


local quotarD quotaResn13 quotaResn9_12 quotaResn5_8 quotaResn1_4 quotaResp1_4 quotaResp5_8 quotaResp9_12 quotaResp13
eststo: xtreg lnMMRt1DHS `quotarD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg womparl `quotarD' `cntrl' i.year if e(sample), fe cluster(ccode)


#delimit ;
esttab est2 est1 using "$OUT/table3_DHS.tex",
`opts' keep(`quotarD')
title("Dynamic Effects of Gender Quotas on Women in Parliament and Health (DHS)")
postfoot("Country and Year FE &Y&Y \\                                   "
         "\bottomrule\multicolumn{3}{p{11.4cm}}{\begin{footnotesize} Results    "
         "from table 2 are replicated, however using alternative data generated "
         "from DHS microdata.  Refer to table 3 for additional notes."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

/*
exit


qui xtreg lnMMRt1  quota lnGDP i.year, fe cluster(ccode)
preserve
keep if e(sample)==1


eststo: xtreg womparl quotaRes `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  quotaRes `cntrl' i.year, fe cluster(ccode)
sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")

eststo: xtreg womparl quotaCand `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaCand `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  quotaCand `cntrl' i.year, fe cluster(ccode)
sum quotaRes if e(sample)==1
local qc = string(r(mean), "%5.3f")


#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/Quota.tex", `opts'
keep(quotaCand quotaRes _cons) title("Gender Quotas, Women in Parliament and Health")
mgroups("Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{7}{p{16.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation. The proportion of     "
         "countries with reserved seats or a candidate list quota is      "
         "(respectively) `qr' and `qc'.  Full summary statistics are in   "
         "table 2. Reserved Seats refers"
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

eststo: xtreg womparl qResVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 qResVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  qResVal lnGDP i.year, fe cluster(ccode)
sum qResVal if e(sample)==1
local qr = string(r(mean), "%5.3f")

eststo: xtreg womparl qCanVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 qCanVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  qCanVal lnGDP i.year, fe cluster(ccode)
sum qCanVal if e(sample)==1
local qc = string(r(mean), "%5.3f")

#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/QuotaVals.tex", `opts'
keep(qCanVal qResVal _cons) title("Size of Gender Quota, Women in Parliament and Health")
mgroups("Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{7}{p{16.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording the size of the quota in "
         "percent of reserved seats or candidates on the ballot. The average size"
         " of quotas in the countries with "
         "reserved seats and countries with candidate quotas is (respectively)   "
         "`qr' and `qc'.  Full descriptive values are in table 2.          "
         "Coefficients are  interpreted as the effect of increasing the quota by "
         "an additional 1\% \emph{conditional on having a quota in place}.       "
         "Standard errors clustered at the level of the country are reported in  "
         "parentheses. ***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


local quotatD quotap1_4 quotap5_8 quotap9_12 quotap13

local quotarD quotaResp1_4 quotaResp5_8 quotaResp9_12 quotaResp13
eststo: xtreg womparl `quotarD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `quotarD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotarD' `cntrl' i.year, fe cluster(ccode)

local quotacD quotaCandp1_4 quotaCandp5_8 quotaCandp9_12 quotaCandp13
eststo: xtreg womparl `quotacD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `quotacD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotacD' `cntrl' i.year, fe cluster(ccode)

#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/QuotaDynamics.tex",
`opts' keep(`quotarD' `quotacD')
title("Dynamic Effects of Gender Quotas on Women in Parliament and Health")
mgroups("Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y \\                                   "
         "\bottomrule\multicolumn{7}{p{16.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation.  Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Years of the   "
         "independent variables refer to the total time period for which the    "
         "quota has been in place, and are mutually exclusive. Standard errors  "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

local quotatN quotan1_4 quotan5_8 quotan9_12 quotan13

local quotarN quotaResn1_4 quotaResn5_8 quotaResn9_12 quotaResn13
local qrp     quotaResp1_4 quotaResp5_8 quotaResp9_12 quotaResp13
eststo: xtreg womparl `quotarN' `qrp' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `quotarN' `qrp' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotarN' `qrp' `cntrl' i.year, fe cluster(ccode)

local quotacN quotaCandn1_4 quotaCandn5_8 quotaCandn9_12 quotaCandn13
local qcp     quotaCandp1_4 quotaCandp5_8 quotaCandp9_12 quotaCandp13
eststo: xtreg womparl `quotacN' `qcp' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `quotacN' `qcp' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotacN' `qcp' `cntrl' i.year, fe cluster(ccode)

#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/QuotaDynamicsPlacebo.tex",
`opts' keep(`quotarN' `quotacN')
title("Placebo Test: Pre-Quota Dynamics")
mgroups("Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y \\                                   "
         "\bottomrule\multicolumn{7}{p{16.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotanroject.org, recording whether each country has"
         " a quota, and if so its year of implementation.  Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Years of the   "
         "independent variables refer to the total time period for which the    "
         "quota has been in place, and are mutually exclusive. Standard errors  "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


*-------------------------------------------------------------------------------
*--- (4X) Regressions with One Model
*-------------------------------------------------------------------------------
eststo: xtreg womparl quotaRes quotaCand `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes quotaCand `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  quotaRes quotaCand `cntrl' i.year, fe cluster(ccode)
sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")
sum quotaCand if e(sample)==1
local qc = string(r(mean), "%5.3f")


#delimit ;
esttab est1 est2 est3 using "$OUT/Quota_Same.tex", `opts'
keep(quotaCand quotaRes _cons) title("Gender Quotas, Women in Parliament and Health")
postfoot("Country and Year FE &Y&Y&Y \\                       "
         "\bottomrule\multicolumn{4}{p{10.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation. The proportion of     "
         "countries with reserved seats or a candidate list quota is      "
         "(respectively) `qr' and `qc'.  Full summary statistics are in   "
         "table 2. Reserved Seats refers"
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

eststo: xtreg womparl qResVal qCanVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 qResVal qCanVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  qResVal qCanVal lnGDP i.year, fe cluster(ccode)
sum qResVal if e(sample)==1
local qc = string(r(mean), "%5.3f")
sum qCanVal if e(sample)==1
local qc = string(r(mean), "%5.3f")

#delimit ;
esttab est1 est2 est3 using "$OUT/QuotaVals_Same.tex", `opts'
keep(qCanVal qResVal _cons) title("Size of Gender Quota, Women in Parliament and Health")
postfoot("Country and Year FE &Y&Y&Y \\                       "
         "\bottomrule\multicolumn{4}{p{10.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording the size of the quota in "
         "percent of reserved seats or candidates on the ballot. The average size"
         " of quotas in the countries with "
         "reserved seats and countries with candidate quotas is (respectively)   "
         "`qr' and `qc'.  Full descriptive values are in table 2.          "
         "Coefficients are  interpreted as the effect of increasing the quota by "
         "an additional 1\% \emph{conditional on having a quota in place}.       "
         "Standard errors clustered at the level of the country are reported in  "
         "parentheses. ***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


local quotatD quotap1_4 quotap5_8 quotap9_12 quotap13
local cntrl  lnGDP 

local quotarD quotaResp1_4 quotaResp5_8 quotaResp9_12 quotaResp13
local quotacD quotaCandp1_4 quotaCandp5_8 quotaCandp9_12 quotaCandp13
eststo: xtreg womparl `quotarD' `quotacD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `quotarD' `quotacD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotarD' `quotacD' `cntrl' i.year, fe cluster(ccode)


#delimit ;
esttab est1 est2 est3 using "$OUT/QuotaDynamics_Same.tex",
`opts' keep(`quotarD' `quotacD')
title("Dynamic Effects of Gender Quotas on Women in Parliament and Health")
postfoot("Country and Year FE &Y&Y&Y \\                                   "
         "\bottomrule\multicolumn{4}{p{10.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation.  Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Years of the   "
         "independent variables refer to the total time period for which the    "
         "quota has been in place, and are mutually exclusive. Standard errors  "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

local quotatN quotan1_4 quotan5_8 quotan9_12 quotan13
local cntrl  lnGDP 

local quotarN quotaResn1_4 quotaResn5_8 quotaResn9_12 quotaResn13
local qrp     quotaResp1_4 quotaResp5_8 quotaResp9_12 quotaResp13
local quotacN quotaCandn1_4 quotaCandn5_8 quotaCandn9_12 quotaCandn13
local qcp     quotaCandp1_4 quotaCandp5_8 quotaCandp9_12 quotaCandp13
eststo: xtreg womparl `quotarN' `qrp' `quotacN' `qcp' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `quotarN' `qrp' `quotacN' `qcp' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotarN' `qrp' `quotacN' `qcp' `cntrl' i.year, fe cluster(ccode)

#delimit ;
esttab est1 est2 est3 using "$OUT/QuotaDynamicsPlacebo_Same.tex",
`opts' keep(`quotarN' `quotacN')
title("Placebo Test: Pre-Quota Dynamics")
postfoot("Country and Year FE &Y&Y&Y \\                                   "
         "\bottomrule\multicolumn{4}{p{12.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotanroject.org, recording whether each country has"
         " a quota, and if so its year of implementation.  Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Years of the   "
         "independent variables refer to the total time period for which the    "
         "quota has been in place, and are mutually exclusive. Standard errors  "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

*-------------------------------------------------------------------------------
*--- (4X) Regressions with One Model
*-------------------------------------------------------------------------------
eststo: xtreg womparl quotaRes quotaCand `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes quotaCand `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  quotaRes quotaCand `cntrl' i.year, fe cluster(ccode)
sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")
sum quotaCand if e(sample)==1
local qc = string(r(mean), "%5.3f")


#delimit ;
esttab est1 est2 est3 using "$OUT/Quota_Same.tex", `opts'
keep(quotaCand quotaRes _cons) title("Gender Quotas, Women in Parliament and Health")
postfoot("Country and Year FE &Y&Y&Y \\                       "
         "\bottomrule\multicolumn{4}{p{10.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation. The proportion of     "
         "countries with reserved seats or a candidate list quota is      "
         "(respectively) `qr' and `qc'.  Full summary statistics are in   "
         "table 2. Reserved Seats refers"
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

eststo: xtreg womparl qResVal qCanVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 qResVal qCanVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  qResVal qCanVal lnGDP i.year, fe cluster(ccode)
sum qResVal if e(sample)==1
local qc = string(r(mean), "%5.3f")
sum qCanVal if e(sample)==1
local qc = string(r(mean), "%5.3f")

#delimit ;
esttab est1 est2 est3 using "$OUT/QuotaVals_Same.tex", `opts'
keep(qCanVal qResVal _cons) title("Size of Gender Quota, Women in Parliament and Health")
postfoot("Country and Year FE &Y&Y&Y \\                       "
         "\bottomrule\multicolumn{4}{p{10.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording the size of the quota in "
         "percent of reserved seats or candidates on the ballot. The average size"
         " of quotas in the countries with "
         "reserved seats and countries with candidate quotas is (respectively)   "
         "`qr' and `qc'.  Full descriptive values are in table 2.          "
         "Coefficients are  interpreted as the effect of increasing the quota by "
         "an additional 1\% \emph{conditional on having a quota in place}.       "
         "Standard errors clustered at the level of the country are reported in  "
         "parentheses. ***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


local quotatD quotap1_4 quotap5_8 quotap9_12 quotap13
local cntrl  lnGDP 

local quotarD quotaResp1_4 quotaResp5_8 quotaResp9_12 quotaResp13
local quotacD quotaCandp1_4 quotaCandp5_8 quotaCandp9_12 quotaCandp13
eststo: xtreg womparl `quotarD' `quotacD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `quotarD' `quotacD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotarD' `quotacD' `cntrl' i.year, fe cluster(ccode)


#delimit ;
esttab est1 est2 est3 using "$OUT/QuotaDynamics_Same.tex",
`opts' keep(`quotarD' `quotacD')
title("Dynamic Effects of Gender Quotas on Women in Parliament and Health")
postfoot("Country and Year FE &Y&Y&Y \\                                   "
         "\bottomrule\multicolumn{4}{p{10.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation.  Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Years of the   "
         "independent variables refer to the total time period for which the    "
         "quota has been in place, and are mutually exclusive. Standard errors  "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

local quotatN quotan1_4 quotan5_8 quotan9_12 quotan13
local cntrl  lnGDP 

local quotarN quotaResn1_4 quotaResn5_8 quotaResn9_12 quotaResn13
local qrp     quotaResp1_4 quotaResp5_8 quotaResp9_12 quotaResp13
local quotacN quotaCandn1_4 quotaCandn5_8 quotaCandn9_12 quotaCandn13
local qcp     quotaCandp1_4 quotaCandp5_8 quotaCandp9_12 quotaCandp13
eststo: xtreg womparl `quotarN' `qrp' `quotacN' `qcp' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `quotarN' `qrp' `quotacN' `qcp' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotarN' `qrp' `quotacN' `qcp' `cntrl' i.year, fe cluster(ccode)

#delimit ;
esttab est1 est2 est3 using "$OUT/QuotaDynamicsPlacebo_Same.tex",
`opts' keep(`quotarN' `quotacN')
title("Placebo Test: Pre-Quota Dynamics")
postfoot("Country and Year FE &Y&Y&Y \\                                   "
         "\bottomrule\multicolumn{4}{p{12.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotanroject.org, recording whether each country has"
         " a quota, and if so its year of implementation.  Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Years of the   "
         "independent variables refer to the total time period for which the    "
         "quota has been in place, and are mutually exclusive. Standard errors  "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

*-------------------------------------------------------------------------------
*--- (4Y) Regressions with DHS data
*-------------------------------------------------------------------------------
keep if lnMMRt1DHS!=.
lab var lnMMRt1DHS "ln(MMR)"

eststo: xtreg womparl quotaRes `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1DHS quotaRes `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  quotaRes `cntrl' i.year, fe cluster(ccode)
sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")

eststo: xtreg womparl quotaCand `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1DHS quotaCand `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  quotaCand `cntrl' i.year, fe cluster(ccode)
sum quotaCand if e(sample)==1
local qc = string(r(mean), "%5.3f")


#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/Quota_DHS.tex", `opts'
keep(quotaCand quotaRes _cons)
title("Gender Quotas, Women in Parliament and Health (DHS Data)")
mgroups("Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{7}{p{16.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation. The proportion of     "
         "countries with reserved seats or a candidate list quota is      "
         "(respectively) `qr' and `qc'.  Full summary statistics are in   "
         "table 2. Reserved Seats refers"
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

eststo: xtreg womparl qResVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1DHS qResVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  qResVal lnGDP i.year, fe cluster(ccode)
sum qResVal if e(sample)==1
local qr = string(r(mean), "%5.3f")

eststo: xtreg womparl qCanVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1DHS qCanVal lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  qCanVal lnGDP i.year, fe cluster(ccode)
sum qCanVal if e(sample)==1
local qc = string(r(mean), "%5.3f")

#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/QuotaVals_DHS.tex", `opts'
keep(qCanVal qResVal _cons)
title("Size of Gender Quota, Women in Parliament and Health (DHS Data)")
mgroups("Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{7}{p{16.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording the size of the quota in "
         "percent of reserved seats or candidates on the ballot. The average size"
         " of quotas in the countries with "
         "reserved seats and countries with candidate quotas is (respectively)   "
         "`qr' and `qc'.  Full descriptive values are in table 2.          "
         "Coefficients are  interpreted as the effect of increasing the quota by "
         "an additional 1\% \emph{conditional on having a quota in place}.       "
         "Standard errors clustered at the level of the country are reported in  "
         "parentheses. ***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


local quotatD quotap1_4 quotap5_8 quotap9_12 quotap13

local quotarD quotaResp1_4 quotaResp5_8 quotaResp9_12 quotaResp13
eststo: xtreg womparl `quotarD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1DHS `quotarD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotarD' `cntrl' i.year, fe cluster(ccode)

local quotacD quotaCandp1_4 quotaCandp5_8 quotaCandp9_12 quotaCandp13
eststo: xtreg womparl `quotacD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1DHS `quotacD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotacD' `cntrl' i.year, fe cluster(ccode)

#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/QuotaDynamics_DHS.tex",
`opts' keep(`quotarD' `quotacD')
title("Dynamic Effects of Gender Quotas on Women in Parliament and Health (DHS Data)")
mgroups("Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y \\                                   "
         "\bottomrule\multicolumn{7}{p{16.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation.  Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Years of the   "
         "independent variables refer to the total time period for which the    "
         "quota has been in place, and are mutually exclusive. Standard errors  "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

local quotatN quotan1_4 quotan5_8 quotan9_12 quotan13

local quotarN quotaResn1_4 quotaResn5_8 quotaResn9_12 quotaResn13
local qrp     quotaResp1_4 quotaResp5_8 quotaResp9_12 quotaResp13
eststo: xtreg womparl `quotarN' `qrp' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1DHS `quotarN' `qrp' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotarN' `qrp' `cntrl' i.year, fe cluster(ccode)

local quotacN quotaCandn1_4 quotaCandn5_8 quotaCandn9_12 quotaCandn13
local qcp     quotaCandp1_4 quotaCandp5_8 quotaCandp9_12 quotaCandp13
eststo: xtreg womparl `quotacN' `qcp' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1DHS `quotacN' `qcp' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotacN' `qcp' `cntrl' i.year, fe cluster(ccode)

#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/QuotaDynamicsPlacebo_DHS.tex",
`opts' keep(`quotarN' `quotacN')
title("Placebo Test: Pre-Quota Dynamics (DHS Data)")
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y \\                                   "
         "\bottomrule\multicolumn{7}{p{16.2cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotanroject.org, recording whether each country has"
         " a quota, and if so its year of implementation.  Reserved Seats refers"
         " only to those countries where a fixed proportion of representation is"
         " guaranteed for women with binding sanctions in place. Years of the   "
         "independent variables refer to the total time period for which the    "
         "quota has been in place, and are mutually exclusive. Standard errors  "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear



*-------------------------------------------------------------------------------
*--- (4a) Alternative Regressions 
*-------------------------------------------------------------------------------


local cnd if lowGDP==1
eststo: xtreg womparl quota lnGDP i.year `cnd', fe cluster(ccode)
eststo: xtreg lnMMRt1 quota lnGDP i.year `cnd', fe cluster(ccode)
eststo: xtreg lnTBt1  quota lnGDP i.year `cnd', fe cluster(ccode)
sum quota if e(sample)==1
local qn = string(r(mean), "%5.3f")

eststo: xtreg womparl quotaRes lnGDP i.year `cnd', fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes lnGDP i.year `cnd', fe cluster(ccode)
eststo: xtreg lnTBt1  quotaRes lnGDP i.year `cnd', fe cluster(ccode)
sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")

eststo: xtreg womparl quotaCand lnGDP i.year `cnd', fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaCand lnGDP i.year `cnd', fe cluster(ccode)
eststo: xtreg lnTBt1  quotaCand lnGDP i.year `cnd', fe cluster(ccode)
sum quotaCand if e(sample)==1
local qc = string(r(mean), "%5.3f")

#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/QuotaLowGDP.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(quota quotaCand quotaRes)
title("Gender Quotas, Women in Parliament and Health (GDP $\leq$ Median)")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{10}{p{20.8cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation.  The proportion of    "
         "countries with a quota, reserved seats, and a candidate quota in this "
         "sample is (respectively) `qn', `qr' and `qc'.  Full summary statistics"
         " are in table 2. Reserved Seats refers"
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

local cnd if lowGDP==0
eststo: xtreg womparl quota lnGDP i.year `cnd', fe cluster(ccode)
eststo: xtreg lnMMRt1 quota lnGDP i.year `cnd', fe cluster(ccode)
eststo: xtreg lnTBt1  quota lnGDP i.year `cnd', fe cluster(ccode)
sum quota if e(sample)==1
local qn = string(r(mean), "%5.3f")

eststo: xtreg womparl quotaRes lnGDP i.year `cnd', fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes lnGDP i.year `cnd', fe cluster(ccode)
eststo: xtreg lnTBt1  quotaRes lnGDP i.year `cnd', fe cluster(ccode)
sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")

eststo: xtreg womparl quotaCand lnGDP i.year `cnd', fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaCand lnGDP i.year `cnd', fe cluster(ccode)
eststo: xtreg lnTBt1  quotaCand lnGDP i.year `cnd', fe cluster(ccode)
sum quotaCand if e(sample)==1
local qc = string(r(mean), "%5.3f")

#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/QuotaHighGDP.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(quota quotaCand quotaRes)
title("Gender Quotas, Women in Parliament and Health (GDP $>$ Median)")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{10}{p{20.8cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation. The proportion of    "
         "countries with a quota, reserved seats, and a candidate quota in this "
         "sample is (respectively) `qn', `qr' and `qc'.  Full summary statistics"
         " are in table 2. Reserved Seats refers"
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



sum lnGDP
gen lnGDP2 = lnGDP-r(min)
gen quotaxlnGDP  = quota*lnGDP2
gen quotaRxlnGDP = quotaRes*lnGDP2
gen quotaCxlnGDP = quotaCand*lnGDP2

local q1 quota quotaxlnGDP
local q2 quotaRes quotaRxlnGDP
local q3 quotaCand quotaCxlnGDP

eststo: xtreg womparl `q1' lnGDP2 i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `q1' lnGDP2 i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `q1' lnGDP2 i.year, fe cluster(ccode)

eststo: xtreg womparl `q2' lnGDP2 i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `q2' lnGDP2 i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `q2' lnGDP2 i.year, fe cluster(ccode)

eststo: xtreg womparl `q3' lnGDP2 i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `q3' lnGDP2 i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `q3' lnGDP2 i.year, fe cluster(ccode)
lab var quotaxlnGDP  "Quota $\times$ ln(GDP)"
lab var quotaRxlnGDP "Reserved $\times$ ln(GDP)"
lab var quotaCxlnGDP "Candidates $\times$ ln(GDP)"
lab var lnGDP2       "ln(GDP)"


#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/QuotaGDP.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(`q1' `q2' `q3' lnGDP2) title("Gender Quotas, Women in Parliament and Health")
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



gen qvalxlnGDP  = qVal*lnGDP2
gen qRvalxlnGDP = qResVal*lnGDP2
gen qCvalxlnGDP = qCanVal*lnGDP2

local q1 qVal    qvalxlnGDP
local q2 qResVal qRvalxlnGDP
local q3 qCanVal qCvalxlnGDP

eststo: xtreg womparl `q1' lnGDP2 i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `q1' lnGDP2 i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `q1' lnGDP2 i.year, fe cluster(ccode)

eststo: xtreg womparl `q2' lnGDP2 i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `q2' lnGDP2 i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `q2' lnGDP2 i.year, fe cluster(ccode)

eststo: xtreg womparl `q3' lnGDP2 i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `q3' lnGDP2 i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `q3' lnGDP2 i.year, fe cluster(ccode)
lab var qvalxlnGDP "Quota \% $\times$ ln(GDP)"
lab var qRvalxlnGDP "Reserved \% $\times$ ln(GDP)"
lab var qCvalxlnGDP "Candidates \% $\times$ ln(GDP)"
lab var lnGDP2       "ln(GDP)"


#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/QuotaGDPVals.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(`q1' `q2' `q3' lnGDP2) title("Gender Quotas, Women in Parliament and Health")
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



eststo: xtreg lnMMRt1 quotaRes      , fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes lnGDP i.rcode#i.year i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes lnGDP i.ccode#c.year i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 qRes      , fe cluster(ccode)
eststo: xtreg lnMMRt1 qRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 qRes lnGDP i.rcode#i.year i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 qRes lnGDP i.ccode#c.year i.year, fe cluster(ccode)


#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 using "$OUT/QuotaAlternative.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(quotaRes qResVal)
title("Gender Quotas, Women in Parliament and Health (Various Specifications)")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 0 0 1 0 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country FE & Y & Y & Y & Y & Y & Y & Y & Y  \\                         "
         "Year FE    &   & Y & Y & Y &   & Y & Y & Y  \\                         "
         "Regional $\times$ Year FE &   &  & Y &  &   &  & Y &   \\              "
         "Country Trends  &   &  &   & Y &   &  &  & Y   \\                      "
         "\bottomrule\multicolumn{9}{p{19.2cm}}{\begin{footnotesize} Both MMR    "
         "and TB are recorded as deaths per 100,000 events.  For MMR, this is    "
         "deaths per 100,000 live births, while for TB this is deaths per 100,000"
         " population. Standard errors   "
         "clustered at the level of the country are reported in parentheses.     "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear




local quotat quotaLower quotaUpper quotaUni
local cntrl  lnGDP 
eststo: xtreg womparl `quotat' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `quotat' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotat' `cntrl' i.year, fe cluster(ccode)

local reservet reservedLower reservedUpper reservedUni
eststo: xtreg womparl `reservet' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `reservet' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `reservet' `cntrl' i.year, fe cluster(ccode)

local candt candidatesLower candidatesUpper candidatesUni
eststo: xtreg womparl `candt' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `candt' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `candt' `cntrl' i.year, fe cluster(ccode)

lab var womparl "\% Women"
lab var lnMMRt1 "ln(MMR)"
lab var lnTBt1 "ln(TB)"
lab var quotaLower "Quota (Lower Chamber)"
lab var quotaUpper "Quota (Upper Chamber)"
lab var quotaUni   "Quota (Unicameral)"
lab var reservedLower "Reserved Seats (Lower Chamber)"
lab var reservedUpper "Reserved Seats (Upper Chamber)"
lab var reservedUni   "Reserved Seats (Unicameral)"
lab var candidatesLower "Candidates (Lower Chamber)"
lab var candidatesUpper "Candidates (Upper Chamber)"
lab var candidatesUni   "Candidates (Unicameral)"


#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/QuotaType.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(`quotat' `reservet' `candt')
title("Effects of Gender Quotas at Different Levels on Women in Parliament and Health")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y \\                                   "
         "\bottomrule\multicolumn{10}{p{20.8cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, whether the parliament in uni-cameral or bi-cameral, and if "
         "the parliament is bi-cameral in which house of parliament the quota is"
         " in place.  Standard errors  "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

eststo: xtreg antenatal   quota lnGDP i.year, fe cluster(ccode)
eststo: xtreg birthAttend quota lnGDP i.year, fe cluster(ccode)
eststo: xtreg teenPreg    quota lnGDP i.year, fe cluster(ccode)

eststo: xtreg antenatal   quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg birthAttend quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg teenPreg    quotaRes lnGDP i.year, fe cluster(ccode)

eststo: xtreg antenatal   quotaCand lnGDP i.year, fe cluster(ccode)
eststo: xtreg birthAttend quotaCand lnGDP i.year, fe cluster(ccode)
eststo: xtreg teenPreg    quotaCand lnGDP i.year, fe cluster(ccode)

lab var antenatal   "Antenatal"
lab var birthAttend "Attendance"
lab var teenPreg    "Teen Preg"

#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/QuotaMechanism.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(quota quotaRes quotaCand)
title("Mechanism Tests: Quotas and Health Policies")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y \\                                   "
         "\bottomrule\multicolumn{10}{p{20.8cm}}{\begin{footnotesize} Health     "
         " information comes from the most complete set of measures available   "
         "from the World Bank databank.  Antenatal refers to the percent of     "
         " pregnant women receiving antenatal care, "
         "Attendance refers to the percent of births attended by skilled health "
         "staff, and Teen preg refers to the adolescent fertility rate per 1,000"
         "women aged 15-19.  Standard errors  "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


local cntrl  lnGDP 
eststo: xtreg antenatal   `quotatD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg birthAttend `quotatD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg teenPreg    `quotatD' `cntrl' i.year, fe cluster(ccode)

eststo: xtreg antenatal   `quotarD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg birthAttend `quotarD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg teenPreg    `quotarD' `cntrl' i.year, fe cluster(ccode)

eststo: xtreg antenatal   `quotacD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg birthAttend `quotacD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg teenPreg    `quotacD' `cntrl' i.year, fe cluster(ccode)

#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/QuotaMechanismsDynamics.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(`quotatD' `quotarD' `quotacD')
title("Dynamic Mechanism Tests: Quotas and Health Policies (5 Year Intervals)")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y \\                           "
         "\bottomrule\multicolumn{10}{p{22.8cm}}{\begin{footnotesize} Health    "
         " information comes from the most complete set of measures available   "
         "from the World Bank databank.  Antenatal refers to the percent of     "
         " pregnant women receiving antenatal care, "
         "Attendance refers to the percent of births attended by skilled health "
         "staff, and Teen preg refers to the adolescent fertility rate per 1,000"
         "women aged 15-19.  Standard errors  "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear



keep if quota==1

eststo: reg womparl qVal lnGDP i.year, cluster(ccode)
eststo: reg lnMMRt1 qVal lnGDP i.year, cluster(ccode)
eststo: reg lnTBt1  qVal lnGDP i.year, cluster(ccode)
sum qVal if e(sample)==1
local qn = string(r(mean), "%5.3f")

eststo: reg womparl qResVal lnGDP i.year, cluster(ccode)
eststo: reg lnMMRt1 qResVal lnGDP i.year, cluster(ccode)
eststo: reg lnTBt1  qResVal lnGDP i.year, cluster(ccode)
sum qResVal if e(sample)==1
local qr = string(r(mean), "%5.3f")

eststo: reg womparl qCanVal lnGDP i.year, cluster(ccode)
eststo: reg lnMMRt1 qCanVal lnGDP i.year, cluster(ccode)
eststo: reg lnTBt1  qCanVal lnGDP i.year, cluster(ccode)
sum qCanVal if e(sample)==1
local qc = string(r(mean), "%5.3f")
lab var womparl "\% Women"
lab var lnMMRt1 "ln(MMR)"
lab var lnTBt1 "ln(TB)"
lab var qVal    "Gender Quota (\%)"
lab var qResVal "Quota (reserved, \%)"
lab var qCanVal "Quota (candidates, \%)"

#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/QuotaVals_NoFE.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(qVal qCanVal qResVal)
title("Size of Gender Quota, Women in Parliament and Health (No Country FEs)")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Year FE &Y&Y&Y&Y&Y&Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{10}{p{20.8cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording the size of the quota in "
         "percent of reserved seats or candidates on the ballot.  The average size"
         " of quotas in the full sample of countries with quotas, countries with "
         "reserved seats and countries with candidate quotas is (respectively)   "
         "`qn', `qr' and `qc'.  Full descriptive values are in table 2.          "
         "Coefficients are  interpreted as the effect of increasing the quota by "
         "an additional 1\% \emph{conditional on having a quota in place}.       "
         " Standard errors "
         "clustered at the level of the country are reported in parentheses.    "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

restore
*/
*-------------------------------------------------------------------------------
*--- (4a) Event Study Women in Parliament -- Reserved Seats
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
*--- (4b) Event Study MMR -- Reserved Seats
*-------------------------------------------------------------------------------
bys country: egen meanGDP = mean(lnGDP)

drop quota
gen quota = year>=quotayear

gen prepost = year-(quotayear-1)
bys country: egen qcountry = max(quota)
sum prepost
local min = (-1*r(min))+1
local max = r(max)-2

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
*--- (4bx) Event Study MMR -- Reserved Seats
*-------------------------------------------------------------------------------
drop quota
gen quota = year>=quotayear

gen prepost = year-(quotayear-1)
bys country: egen qcountry = max(quota)
sum prepost
local min = (-1*r(min))+1
local max = r(max)-2

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

xtreg MMRt1 i.year quotaLag* quotaLead* lnGDP, fe cluster(ccode)
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
scheme(s1mono) ytitle("Maternal Deaths")
yline(0, lpattern(dot)) xline(0, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year 0 is omitted as the base case.");
graph export "$OUT/quotas/eventMMRreserved.eps", as(eps) replace;
#delimit cr
drop time PointE UBound LBound prepost qcountry quotaLag* quotaLead* 
    
*-------------------------------------------------------------------------------
*--- (4bii) Event Study Fertility -- Reserved Seats
*-------------------------------------------------------------------------------
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

xtreg fertt1 i.year quotaLag* quotaLead* lnGDP i.rcode#c.year, fe cluster(ccode)
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
scheme(s1mono) ytitle("Fertility Rate")
yline(0, lpattern(dot)) xline(0, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year 0 is omitted as the base case.");
graph export "$OUT/quotas/eventFert.eps", as(eps) replace;
#delimit cr

drop time PointE UBound LBound prepost qcountry quotaLag* quotaLead* 


*-------------------------------------------------------------------------------
*--- (4biii) Event Study Male Mortality -- Reserved Seats
*-------------------------------------------------------------------------------
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

xtreg lnmmortt1 i.year quotaLag* quotaLead* lnGDP, fe cluster(ccode)
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
scheme(s1mono) ytitle("Male Mortality")
yline(0, lpattern(dot)) xline(0, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year 0 is omitted as the base case.");
graph export "$OUT/quotas/eventMMort.eps", as(eps) replace;
#delimit cr

drop time PointE UBound LBound prepost qcountry quotaLag* quotaLead* 

restore

*-------------------------------------------------------------------------------
*--- (4c) Event Study Women in Parliament -- Candidates
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
*--- (4d) Event Study MMR  -- Candidates
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
ytitle("Total Number of Countries with a Gender Quota") xtitle("Year");
graph export $OUT/quotas/quotaTime.eps, as(eps) replace;
#delimit cr

restore

preserve
collapse quotayear rcode, by(country quotatype)
gen num = 1
collapse (sum) num, by(quotayear)
sort quotayear
gen qCountries = sum(num)
drop if quotayear==.
tempfile quotatime
save `quotatime'
restore
preserve
collapse womparl, by(year)
rename year quotayear
merge 1:1 quotayear using `quotatime'
keep if _merge==3
lab var womparl "Average % of Women in Parliament"
#delimit ;
twoway line qCountries quotayear, scheme(s1mono) lcolor(red) lwidth(thick) yaxis(1)
    || line womparl quotayear, lcolor(blue) lwidth(thick) lpattern(dash) yaxis(2)
ytitle("Total Number of Countries with a Gender Quota") xtitle("Year")
legend(lab(1 "Number of Quotas") lab(2 "Women in Parliament"))
;
graph export $OUT/quotas/quotaTimeWP.eps, as(eps) replace;
#delimit cr
restore

preserve
keep if quotatype=="Candidates/Reserved"|quotatype=="Reserved"
collapse quotayear, by(country)
replace quotayear=1990 if quotayear==1989
gen num = 1
collapse (sum) num, by(quotayear)
sort quotayear
gen qCountries = sum(num)
drop if quotayear==.
tempfile quotatime2
save `quotatime2'
restore
preserve
collapse womparl, by(year)
rename year quotayear
merge 1:1 quotayear using `quotatime2'
*keep if _merge==3
keep if quotayear<=2012
lab var womparl "Average % of Women in Parliament"
#delimit ;
twoway line qCountries quotayear, scheme(s1mono) lcolor(red) lwidth(thick) yaxis(1)
    || line womparl quotayear, lcolor(blue) lwidth(thick) lpattern(dash) yaxis(2)
ytitle("Total Number of Countries with Reserved Seats") xtitle("Year")
legend(lab(1 "Number of Quotas") lab(2 "Women in Parliament"));
graph export $OUT/quotas/reservedSeatsWP.eps, as(eps) replace;
#delimit cr
restore


preserve
collapse womparl lnMMRt1, by(year)
rename year quotayear
lab var womparl "Average % of Women in Parliament"
lab var lnMMRt1 "ln(Maternal Mortality Ratio)"
keep if quotayear<=2013
#delimit ;
twoway line womparl quotayear, scheme(s1mono) lcolor(red) lwidth(thick) yaxis(1)
    || line lnMMRt1 quotayear, lcolor(blue) lwidth(thick) lpattern(dash) yaxis(2)
xlabel(1990 1995 2000 2005 2010)
ytitle("Average % of Women in Parliament") xtitle("Year")
legend(lab(1 "Women in Parliament") lab(2 "ln(MMR)"));
graph export $OUT/quotas/WomParlMMR.eps, as(eps) replace;
#delimit cr
restore


preserve
collapse quotayear rcode, by(country quotatype)
gen num = 1
collapse (sum) num, by(quotayear)
sort quotayear
gen qCountries = sum(num)
drop if quotayear==.
tempfile quotatime
save `quotatime'
restore
preserve
collapse lnMMRt1, by(year)
rename year quotayear
merge 1:1 quotayear using `quotatime'
keep if _merge==3
lab var lnMMRt1 "ln(Maternal Mortality Ratio)"
#delimit ;
twoway line qCountries quotayear, scheme(s1mono) lcolor(red) lwidth(thick) yaxis(1)
    || line lnMMRt1    quotayear, lcolor(blue) lwidth(thick) lpattern(dash) yaxis(2)
ytitle("Total Number of Countries with a Gender Quota") xtitle("Year")
legend(lab(1 "Number of Quotas") lab(2 "ln(MMR)"))
;
graph export $OUT/quotas/quotaTimeMMR.eps, as(eps) replace;
#delimit cr
restore


*-------------------------------------------------------------------------------
*--- (6) Country plots over time
*-------------------------------------------------------------------------------
preserve
bys country: egen qcountry = max(quota)
keep if qcountry==1
drop if country=="Haiti"
replace country = subinstr(country, ",", " ", .)
levelsof country if quotatype=="Legislated Candidate Quota", local(countries)
local Cgraphs

local i=1
foreach cc of local countries {
    dis "`cc'"
    sum quotayear if country==`"`cc'"'
    local ly = r(mean)
    sum quotapercent1 if country==`"`cc'"'
    local qp = string(r(mean), "%5.3f")
    
    #delimit ;
    twoway line womparl year if country==`"`cc'"', lcolor(black) lwidth(thick)
    scheme(s1mono) xline(`ly', lcolor(red)) title(`cc') ytitle("% Women in Parlimanet")
    xtitle("Year") note("quotaproject listed quota is `qp'%") name(c`i', replace);
    #delimit cr
    graph export "$OUT/quotas/countries/c`i'.eps", replace
    local Cgraphs `Cgraphs' c`i' 
    local ++i
}
graph combine `Cgraphs', scheme(s1mono)
graph export "$OUT/quotas/CandidateCountries.eps", replace

levelsof country if quotatype!="Legislated Candidate Quota", local(countries)
local Rgraphs

foreach cc of local countries {
    dis "`cc'"
    sum quotayear if country==`"`cc'"'
    local ly = r(mean)
    sum quotapercent1 if country==`"`cc'"'
    local qp = string(r(mean), "%5.3f")
    
    #delimit ;
    twoway line womparl year if country==`"`cc'"', lcolor(black) lwidth(thick)
    scheme(s1mono) xline(`ly', lcolor(red)) title(`cc') ytitle("% Women in Parlimanet")
    xtitle("Year") note("quotaproject listed quota is `qp'%") name(c`i', replace);
    #delimit cr
    graph export "$OUT/quotas/countries/c`i'.eps", replace
    local Rgraphs `Rgraphs' c`i' 
    local ++i
}
graph combine `Rgraphs', scheme(s1mono)
graph export "$OUT/quotas/ReservedCountries.eps", replace


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
restore



*-------------------------------------------------------------------------------
*--- (7) 5 year averages
*-------------------------------------------------------------------------------
bys country (year): gen MMR5t1 = MMR5[_n+2]
gen lnMMR5t1            = log(MMR5t1)
xtset ccode year
generate womparl5 = (F2.womparl + F1.womparl + womparl+L1.womparl+L2.womparl)/5
keep if MMR5t1!=.&womparl5!=.


qui xtreg lnMMR5t1  quota lnGDP i.year, fe cluster(ccode)
preserve
keep if e(sample)==1

eststo: xtreg womparl5 quota lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMR5t1 quota lnGDP i.year, fe cluster(ccode)
sum quota if e(sample)==1
local qn = string(r(mean), "%5.3f")


eststo: xtreg womparl5 quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMR5t1 quotaRes lnGDP i.year, fe cluster(ccode)
sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")

eststo: xtreg womparl5 quotaCand lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMR5t1 quotaCand lnGDP i.year, fe cluster(ccode)
sum quotaCand if e(sample)==1
local qc = string(r(mean), "%5.3f")
lab var womparl "\% Women"
lab var lnMMRt1 "ln(MMR)"
lab var lnTBt1 "ln(TB)"
lab var quota "Any Quotas"
lab var quotaRes "Quota (reserved)"
lab var quotaCand "Quota (candidates)"


#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/Quota_5yr.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(quota quotaCand quotaRes) title("Gender Quotas, Women in Parliament and Health")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 1 0 1 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y\\                       "
         "\bottomrule\multicolumn{7}{p{14.8cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording whether each country has"
         " a quota, and if so its year of implementation. The proportion of     "
         "countries with a quota, reserved seats, and a candidate quota is      "
         "(respectively) `qn', `qr' and `qc'.  Full summary statistics are in   "
         "table 2. Reserved Seats refers"
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
