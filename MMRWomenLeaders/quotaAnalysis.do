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
keep country womparl year tb_death_rate
keep if year>=1990
merge 1:1 country year using "MMR"         , gen(_mergeMMR)
merge 1:1 country year using "GDP"         , gen(_mergeGDP)
merge 1:1 country year using "population"  , gen(_mergepop)
merge 1:1 country year using "mechanisms"  , gen(_mergeMec)


*drop if year<1990
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






gen quotaRes     = quotatype!="Legislated Candidate Quota"&quota==1
gen quotaCand    = quotatype=="Legislated Candidate Quota"&quota==1
gen qVal         = quota*quotapercent1
gen qResVal      = quotaRes*quotapercent1
gen qCanVal      = quotaCan*quotapercent1
foreach var of varlist quota quotaRes quotaCand {
    gen `var'p1_4=`var'==1&(year-quotayear>0)&(year-quotayear<=4)
    gen `var'p5_8=`var'==1&(year-quotayear>4)&(year-quotayear<=8)
    gen `var'p9_12=`var'==1&(year-quotayear>8)&(year-quotayear<=12)
    gen `var'p13=`var'==1&(year-quotayear>12)&quotayear!=.
}
gen quotaLower = quota==1&parliament=="Bicameral"&(quotapercent1!=0&quotapercent1!=.)
gen quotaUpper = quota==1&parliament=="Bicameral"&(quotapercent2!=0&quotapercent2!=.)
gen quotaUni   = quota==1&parliament=="Unicameral"
gen reservedLower = quotaLower*quotaRes
gen reservedUpper = quotaUpper*quotaRes
gen reservedUni   = quotaUni*quotaRes
gen candidatesLower = quotaLower*quotaCand
gen candidatesUpper = quotaUpper*quotaCand
gen candidatesUni   = quotaUni*quotaCand


lab var quota     "Any Quota"
lab var quotaRes  "Reserved Seats"
lab var quotaCand "Legislated Candidate Quota"
lab var quotayear "Year Quota Implemented"
lab var womparl   "\% Women in Parliament"
lab var lnMMRt1   "ln(Maternal Mortality Ratio)"
lab var lnGDP     "ln(GDP per capita)"
lab var quotapercent1 "Quota Size (lower house/uni-cameral)"
lab var quotapercent2 "Quota Size (upper house)"
replace quotapercent2=. if quotapercent2==0

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




qui xtreg lnMMRt1  quota lnGDP i.year, fe cluster(ccode)
preserve
keep if e(sample)==1

eststo: xtreg womparl quota lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quota lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  quota lnGDP i.year, fe cluster(ccode)

eststo: xtreg womparl quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  quotaRes lnGDP i.year, fe cluster(ccode)

eststo: xtreg womparl quotaCand lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaCand lnGDP i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  quotaCand lnGDP i.year, fe cluster(ccode)
lab var womparl "\% Women"
lab var lnMMRt1 "ln(MMR)"
lab var lnTBt1 "ln(TB)"
lab var quota "Any Quotas"
lab var quotaRes "Quota (reserved)"
lab var quotaCand "Quota (candidates)"


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
keep(qVal qCanVal qResVal) title("Size of Gender Quota, Women in Parliament and Health")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y&Y&Y&Y \\                       "
         "\bottomrule\multicolumn{10}{p{20.8cm}}{\begin{footnotesize} Quota data "
         "is coded from the quotaproject.org, recording the size of the quota in "
         "percent of reserved seats or candidates on the ballot. Coefficients are"
         " interpreted as the effect of increasing the quota by an additional 1\%."
         " Standard errors "
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



eststo: xtreg womparl quota lnGDP i.year, fe cluster(ccode)
eststo: xtreg MMRt1 quota lnGDP i.year, fe cluster(ccode)
eststo: xtreg TBt1  quota lnGDP i.year, fe cluster(ccode)

eststo: xtreg womparl quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg MMRt1 quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg TBt1  quotaRes lnGDP i.year, fe cluster(ccode)

eststo: xtreg womparl quotaCand lnGDP i.year, fe cluster(ccode)
eststo: xtreg MMRt1 quotaCand lnGDP i.year, fe cluster(ccode)
eststo: xtreg TBt1  quotaCand lnGDP i.year, fe cluster(ccode)
lab var MMRt1 "MMR"
lab var TBt1 "TB"

#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/QuotaLevels.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(quota quotaCand quotaRes) title("Gender Quotas, Women in Parliament and Health (not in logs)")
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

local quotatD quotap1_4 quotap5_8 quotap9_12 quotap13
local cntrl  lnGDP 
eststo: xtreg womparl `quotatD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `quotatD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotatD' `cntrl' i.year, fe cluster(ccode)

local quotarD quotaResp1_4 quotaResp5_8 quotaResp9_12 quotaResp13
eststo: xtreg womparl `quotarD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `quotarD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotarD' `cntrl' i.year, fe cluster(ccode)

local quotacD quotaCandp1_4 quotaCandp5_8 quotaCandp9_12 quotaCandp13
eststo: xtreg womparl `quotacD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 `quotacD' `cntrl' i.year, fe cluster(ccode)
eststo: xtreg lnTBt1  `quotacD' `cntrl' i.year, fe cluster(ccode)

lab var womparl "\% Women"
lab var lnMMRt1 "ln(MMR)"
lab var lnTBt1 "ln(TB)"
lab var quotap1_4 "Quota (1-4 years)"
lab var quotap5_8 "Quota (5-8 years)"
lab var quotap9_12 "Quota (9-12 years)"
lab var quotap13 "Quota ($>$ 13  years)"
lab var quotaResp1_4 "Reserved Seats (1-4 years)"
lab var quotaResp5_8 "Reserved Seats (5-8 years)"
lab var quotaResp9_12 "Reserved Seats (9-12 years)"
lab var quotaResp13 "Reserved Seats ($>$ 13 years)"
lab var quotaCandp1_4 "Candidates (1-4 years)"
lab var quotaCandp5_8 "Candidates (5-8 years)"
lab var quotaCandp9_12 "Candidates (9-12 years)"
lab var quotaCandp13 "Candidatess ($>$ 13 years)"

#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 est9 using "$OUT/QuotaDynamics.tex",
replace cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations)) booktabs
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
keep(`quotatD' `quotarD' `quotacD')
title("Dynamic Effects of Gender Quotas on Women in Parliament and Health")
mgroups("Any Quota" "Reserved Seats" "Candidate Quota", pattern(1 0 0 1 0 0 1 0 0)
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
postfoot("Country and Year FE &Y&Y&Y&Y&Y&Y \\                                   "
         "\bottomrule\multicolumn{10}{p{22.8cm}}{\begin{footnotesize} Quota data "
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
title("Dynamic Mechanism Tests: Quotas and Health Policies")
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
