/* quotaAnalysis.do v1.00        damiancclarke             yyyy-mm-dd:2017-01-06
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

Examine effects of quotas on female representation, and on maternal mortality. L
onger series of tests for econ paper.

NOTE: Maps are in worldDesc.do
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

#delimit ;
keep country womparl year tb_death_rate yr_sch democ health_exp contcode
     wecon wopol wosoc womens_rights1 womens_rights2;
#delimit cr
merge 1:1 country year using "$DAT/DPI2012"          , gen(_mergePolitics)
merge 1:1 country year using "$DAT/peacekeepingYears", gen(_mergePKeeping)
merge 1:1 country year using "$DAT/ODA"              , gen(_mergeODA)
merge 1:1 country year using "$DAT/MMR"              , gen(_mergeMMR)
merge 1:1 country year using "$DAT/MMR_5yr"          , gen(_mergeMM5)
merge 1:1 country year using "$DAT/GDP"              , gen(_mergeGDP)
merge 1:1 country year using "$DAT/GDPtotal"         , gen(_mergeGDPtot)
merge 1:1 country year using "$DAT/population"       , gen(_mergepop)
merge 1:1 country year using "$DAT/mechanisms"       , gen(_mergeMec)
merge 1:1 country year using "$DAT/fertility"        , gen(_mergeFer)
merge 1:1 country year using "$DAT/maleMortality5imp", gen(_mergemMo5)
merge 1:1 country year using "$DAT/InfantMort"       , gen(_mergemIM)
drop if year<1960

merge m:1 country using "$DAT/quotasComplete"

*-------------------------------------------------------------------------------
*--- (3) Setup
*-------------------------------------------------------------------------------
keep if sh_sta_mmrt !=.&womparl !=.&ny_gdp_pcap_pp_kd !=.
rename MMR MMR5
gen lnGDP  = log(ny_gdp_pcap)

sort country year
bys country: ipolate yr_sch year, gen(yrs_school) epolate
bys country: ipolate health_exp year, gen(healthExp) epolate
bys country: ipolate democ year, gen(democracy) epolate
bys country: gen  MMRt1   = sh_sta_mmrt[_n+2]
bys country: gen  teenPregt1    = teenPreg[_n+1]
bys country: gen  antenatalt1   = antenatal[_n+1]
bys country: gen  birthAttendt1 = birthAttend[_n+1]
bys country: gen  healthExpt1   = health_exp[_n+1]
bys country: gen  fEduct1       = yrs_school[_n+1]
bys country: gen  IMFt1         = IMfemale[_n+1]
bys country: gen  IMMt1         = IMmale[_n+1]
bys country: gen  fertt1  = fert[_n]
bys country: gen  mmortt1 = maleMort[_n+1]
bys country: replace womparl = womparl[_n+1]
bys country: gen  TBt1    = tb_death_rate[_n+1]
bys country: egen pop     =mean(sp_pop_totl)
bys country: egen aveGDP  = mean(lnGDP)
keep if sh_sta_mmrt !=.&womparl !=.&ny_gdp_pcap_pp_kd !=.

gen lnMMRt1       = log(MMRt1)
gen lnIMFt1       = log(IMFt1)
gen lnIMMt1       = log(IMMt1)
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

bys country: gen Drights = wecon-wecon[_n-1]
foreach right in wecon wopol wosoc Drights {
    gen lag1`right'= `right'[_n-1]
    gen lag2`right'= `right'[_n-2]
}

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

lab var MMRt1 "Maternal Mortality Ratio"
lab var mmortt1 "Male Mortality Rate (15-49)"
lab var birthAttendt1 "Percent of Births Attended by Skilled Staff"
lab var antenatalt1   "Percent of Pregnancies Receiving Prenatal Care"

reg lnMMRt1 i.year quotaRes lnGDP
estpost sum womparl MMRt1 quotaRes lnGDP antenatalt1 birthAttendt1 mmortt1 if e(sample)==1
estout using "$OUT/SumQuota.tex", replace label `statform' style(tex)


*-------------------------------------------------------------------------------
*--- (4a) Main Regressions
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

xtreg lnMMRt1 quotaRes `cntrl1' i.year, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes `cntrl1' i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg womparl quotaRes `cntrl1' i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg lnMMRt1 quotaRes lnGDP i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg womparl quotaRes lnGDP i.year if e(sample)==1, fe cluster(ccode)

sum quotaRes
local qr = string(r(mean), "%5.3f")

#delimit ;
esttab est1 est3 est2 est4 using "$OUT/quotaDifDif.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes _cons)
collabels(none) label mlabels(, none) replace
mgroups("ln(Maternal Mortality Ratio)" "\% Women in Parliament", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Difference-in-differences estimates of the effect of Reserved Seats"\label{qDD1})
postfoot("GDP Control & Y & Y & Y & Y \\"
         "Democracy Indicators &  & Y &  & Y \\"
         "\bottomrule\multicolumn{5}{p{13.2cm}}{\begin{footnotesize} Each "
         "regression includes country and year fixed effects and clusters "
         "standard errors by country. A number of (small) countries do not"
         "have a democracy score from Polity IV. Refer to table           "
         "\ref{qDDsamp} for the estimates       "
         "consistently using the sample where all covariates are available."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr

#delimit ;
esttab est9 est7 est10 est8 using "$OUT/quotaDifDif-samp.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes _cons)
collabels(none) label mlabels(, none) replace
mgroups("ln(Maternal Mortality Ratio)" "\% Women in Parliament", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Difference-in-differences estimates of the effect of Reserved Seats"\label{qDDsamp})
postfoot("GDP Control & Y & Y & Y & Y\\"
         "Democracy Indicators &  & Y  &  & Y \\"
         "\bottomrule\multicolumn{5}{p{13.2cm}}{\begin{footnotesize} Refer to"
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
esttab est1 est3 est2 est4 using "$OUT/quotaDifDif-wt.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes _cons)
collabels(none) label mlabels(, none) replace
mgroups("ln(Maternal Mortality Ratio)" "\% Women in Parliament", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Difference-in-differences effect of Reserved Seats (Population Weighting)"\label{qDD})
postfoot("GDP Control & Y & Y & Y & Y \\"
         "Democracy Indicators &  & Y &  & Y \\"
         "\bottomrule\multicolumn{5}{p{13.2cm}}{\begin{footnotesize} Refer to  "
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



eststo: xtreg lnMMRt1 i.year quotaRes quotaCand lnGDP , fe cluster(ccode)
eststo: xtreg womparl quotaRes quotaCand lnGDP i.year if e(sample)==1, fe cluster(ccode)

eststo: xtreg lnMMRt1 `cntrl1' i.year quotaRes quotaCand, fe cluster(ccode)
eststo: xtreg womparl quotaRes quotaCand `cntrl1' i.year if e(sample)==1, fe cluster(ccode)
lab var quotaCand "Candidate Quota"

#delimit ;
esttab est1 est3 est2 est4 using "$OUT/quotaDifDif-CR.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes quotaCand _cons)
collabels(none) label mlabels(, none) replace
mgroups("ln(Maternal Mortality Ratio)" "\% Women in Parliament", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Difference-in-differences estimates of the effect of Reserved Seats"\label{qDD-CR})
postfoot("GDP Control & Y & Y & Y & Y \\"
         "Democracy Indicators &  & Y &  & Y \\"
         "\bottomrule\multicolumn{5}{p{13.2cm}}{\begin{footnotesize} Each "
         "regression includes country and year fixed effects and clusters "
         "standard errors by country."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

eststo: xtreg MMRt1 i.year quotaRes lnGDP , fe cluster(ccode)
eststo: xtreg MMRt1 `cntrl1' i.year quotaRes, fe cluster(ccode)

#delimit ;
esttab est1 est2 using "$OUT/quotaDifDif-level.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes _cons)
collabels(none) label mlabels(, none) replace
mgroups("Maternal Mortality Ratio", pattern(1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Difference-in-differences estimates of the effect of Reserved Seats"\label{qDDlevel})
postfoot("GDP Control & Y & Y \\"
         "Democracy Indicators &  & Y \\"
         "\bottomrule\multicolumn{3}{p{8.2cm}}{\begin{footnotesize} Each "
         "regression includes country and year fixed effects and clusters "
         "standard errors by country."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

gen yearsQuota = max(0,year-quotayear+1)
gen quotaXYears=quotaRes*yearsQuota
eststo: xtreg lnMMRt1 i.year quotaXYears lnGDP , fe cluster(ccode)
eststo: xtreg womparl quotaXYears lnGDP i.year if e(sample)==1, fe cluster(ccode)

eststo: xtreg lnMMRt1 `cntrl1' i.year quotaXYears, fe cluster(ccode)
eststo: xtreg womparl quotaXYears `cntrl1' i.year if e(sample)==1, fe cluster(ccode)


lab var quotaXYears "Number of Years Quota in Place"

#delimit ;
esttab est1 est3 est2 est4 using "$OUT/quotaDifDif-years.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaXYears _cons)
collabels(none) label mlabels(, none) replace
mgroups("ln(Maternal Mortality Ratio)" "\% Women in Parliament", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Intensive Margin Impacts of Reserved Seats (By Quota Time)"\label{qDDtime})
postfoot("GDP Control & Y & Y & Y & Y \\"
         "Democracy Indicators &  & Y &  & Y \\"
         "\bottomrule\multicolumn{5}{p{14.2cm}}{\begin{footnotesize} Each "
         "regression includes country and year fixed effects and clusters "
         "standard errors by country. A number of (small) countries do not"
         "have a democracy score from Polity IV. Refer to table           "
         "\ref{qDDsamp} for the estimates       "
         "consistently using the sample where all covariates are available."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


*-------------------------------------------------------------------------------
*--- (4b) Infant Mortality
*-------------------------------------------------------------------------------
eststo: xtreg lnIMFt1 i.year quotaRes lnGDP , fe cluster(ccode)
eststo: xtreg lnIMMt1 quotaRes lnGDP i.year if e(sample)==1, fe cluster(ccode)

eststo: xtreg lnIMFt1 `cntrl1' i.year quotaRes, fe cluster(ccode)
eststo: xtreg lnIMMt1 quotaRes `cntrl1' i.year if e(sample)==1, fe cluster(ccode)

#delimit ;
esttab est1 est3 est2 est4 using "$OUT/quotaDifDif-IM.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes _cons)
collabels(none) label mlabels(, none) replace
mgroups("log(Female IMR)" "log(Male IMR)", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("The effect of Reserved Seats on log(Infant Mortality)"\label{qDD-CR})
postfoot("GDP Control & Y & Y & Y & Y \\"
         "Democracy Indicators &  & Y &  & Y \\"
         "\bottomrule\multicolumn{5}{p{11.4cm}}{\begin{footnotesize} Each "
         "regression includes country and year fixed effects and clusters "
         "standard errors by country."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

local qVars quotaRes quotaCand
eststo: xtreg lnIMFt1 i.year `qVars' lnGDP , fe cluster(ccode)
eststo: xtreg lnIMMt1 `qVars' lnGDP i.year if e(sample)==1, fe cluster(ccode)

eststo: xtreg lnIMFt1 `cntrl1' i.year `qVars', fe cluster(ccode)
eststo: xtreg lnIMMt1 `qVars' `cntrl1' i.year if e(sample)==1, fe cluster(ccode)

#delimit ;
esttab est1 est3 est2 est4 using "$OUT/quotaDifDif-IM-both.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(`qVars' _cons)
collabels(none) label mlabels(, none) replace
mgroups("log(Female IMR)" "log(Male IMR)", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("The effect of Reserved Seats on log(Infant Mortality)"\label{qDD-CR})
postfoot("GDP Control & Y & Y & Y & Y \\"
         "Democracy Indicators &  & Y &  & Y \\"
         "\bottomrule\multicolumn{5}{p{11.4cm}}{\begin{footnotesize} Each "
         "regression includes country and year fixed effects and clusters "
         "standard errors by country."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


exit

*-------------------------------------------------------------------------------
*--- (4c) IV Regressions
*-------------------------------------------------------------------------------
local cntrl1  lnGDP i.democ
local cntrl2  lnGDP i.democ healthExp yrs_school
local ivse    vce(cluster ccode)
local popts   vce(cluster ccode) gmax(0) gmin(-0.01)

ivregress 2sls lnMMRt1 i.year i.ccode lnGDP (womparl=quotaRes), `ivse'
estimates store IV1
estat firststage
mat IV = r(singleresults)
estadd scalar Fstat = IV[1,4]: IV1
estadd scalar pval  = IV[1,7]: IV1
plausexog uci lnMMRt1 i.year i.ccode lnGDP (womparl=quotaRes), `popts'
local clb = string(`e(lb_womparl)', "%5.3f")
local cub = string(`e(ub_womparl)', "%5.3f")
estadd local conf95 "[`clb';`cub']": IV1

ivregress 2sls lnMMRt1 i.year i.ccode `cntrl1' (womparl=quotaRes), `ivse'
estimates store IV2
estat firststage
mat IV = r(singleresults)
estadd scalar Fstat = IV[1,4]: IV2
estadd scalar pval  = IV[1,7]: IV2
plausexog uci lnMMRt1 i.year i.ccode lnGDP `cntrl1' (womparl=quotaRes), `popts' 
local clb = string(`e(lb_womparl)', "%5.3f")
local cub = string(`e(ub_womparl)', "%5.3f")
estadd local conf95 "[`clb';`cub']": IV2

ivregress 2sls lnMMRt1 i.year i.ccode `cntrl2' (womparl=quotaRes), `ivse'
estimates store IV3
estat firststage
mat IV = r(singleresults)
estadd scalar Fstat = IV[1,4]: IV3
estadd scalar pval  = IV[1,7]: IV3
plausexog uci lnMMRt1 i.year i.ccode lnGDP `cntrl2' (womparl=quotaRes), `popts'
local clb = string(`e(lb_womparl)', "%5.3f")
local cub = string(`e(ub_womparl)', "%5.3f")
estadd local conf95 "[`clb';`cub']": IV3

lab var lnMMRt1 "ln(MMR)"
lab var womparl "\% Women in Parliament"
***EXPORT FORMAT
#delimit ;
esttab IV1 IV2 using "$OUT/IVestimates_quota.tex", replace
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(Fstat pval conf95 N, fmt(%5.3f %5.3f %5.3f %9.0gc)
    label("F-Statistic First Stage" "p-value First Stage"
          "95\% CI from \citet{Conleyetal2012}" Observations))
starlevels(* 0.10 ** 0.05 *** 0.01) collabels(,none) booktabs label
title("Reserved Seats as an IV for Women in Parliament"\label{qIV})
keep(womparl _cons)
postfoot("GDP Control & Y & Y \\"
         "Democracy Indicators &  & Y \\"
         "\bottomrule\multicolumn{3}{p{11.8cm}}{\begin{footnotesize} Instrumental "
         "variables regressions are run where the existence of a reserved seat law"
         "is used to instrument women in parliament.  The first stage regression  "
         "of women in parliament on reserved seats is displayed in columns 4-6 of "
         "table \ref{qDD1}.  F-Statistic of the first stage and the associated    "
         "p-value are traditional tests of instrumental relevance.  Displayed     "
         "coefficients give the effect of an additional percentage of women in    "
         "parliament on rates of maternal mortality, where women in parliament is "
         "instrumented with reserved seats.  The 95\% confidence interval from    "
         "\citet{Conleyetal2012} is a robustness test, where we allow the         "
         "instrument to be imperfect in the sense that the exclusion restriction  "
         "is only close to holding.  These confidence intervals are associated  "
         "with the estimates where quotas are able to have a direct positive      "
         "effect on MMR \emph{not} mediated by women in parliament of 0.01 (or    "
         "1\%) using \citet{Conleyetal2012}'s Union of Confidence Interval (UCI)  "
         "approach.  Each regression includes country and year fixed effects and  "
         "clusters standard errors by country. "
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

****SIZE
local cntrl1  lnGDP i.democ
local cntrl1b lnGDP i.democ healthExp
local cntrl2  lnGDP i.democ healthExp yrs_school
replace quotaSize=0 if quotaSize==.
gen quotaXSize=quotaRes*quotaSize
lab var quotaXSize "Reserved Seats $\times$ Quota Size"

eststo: xtreg lnMMRt1 i.year quotaXSize lnGDP , fe cluster(ccode)
eststo: xtreg womparl quotaXSize lnGDP i.year if e(sample)==1, fe cluster(ccode)

eststo: xtreg lnMMRt1 `cntrl1' i.year quotaXSize, fe cluster(ccode)
eststo: xtreg womparl quotaXSize `cntrl1' i.year if e(sample)==1, fe cluster(ccode)

eststo: xtreg lnMMRt1  quotaXSize `cntrl2' i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg womparl  quotaXSize `cntrl2' i.year if e(sample)==1, fe cluster(ccode)
                      
eststo: xtreg lnMMRt1  quotaXSize `cntrl1' i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg womparl  quotaXSize `cntrl1' i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg lnMMRt1  quotaXSize lnGDP i.year if e(sample)==1, fe cluster(ccode)
eststo: xtreg womparl  quotaXSize lnGDP i.year if e(sample)==1, fe cluster(ccode)

sum quotaRes
local qr = string(r(mean), "%5.3f")

#delimit ;
esttab est1 est3 est2 est4 using "$OUT/quotaDifDif_size.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaXSize _cons)
collabels(none) label mlabels(, none) replace
mgroups("ln(Maternal Mortality Ratio)" "\% Women in Parliament", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Intensive Margin Impacts of Reserved Seats (By Quota Size)"\label{qIDD})
postfoot("GDP Control & Y & Y & Y & Y \\"
         "Democracy Indicators &  & Y &  & Y \\"
         "\bottomrule\multicolumn{5}{p{13.9cm}}{\begin{footnotesize} The   "
         "idependent variable of interest is equal to zero whenever        "
         "reserved seats are not in place in a country, and equal to the   "
         "size of the quota when a reserved seat quota is implemented.     "
         "Coefficients are thus interpreted as the effect of an additional "
         "1 percent of seats reserved for women on rates of maternal       "
         "mortality and the percentage of women in parliament. Each"
         " regression includes country and year fixed effects and clusters "
         "standard errors by country. "
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
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
esttab est1 est3 est2 est4 using "$OUT/quotaPlacebo.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes _cons)
collabels(none) label mlabels(, none) replace
mgroups("ln(Male Mortality) 15-49" "\% Women in Parliament", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Placebo Diff-in-diffs estimates of the effect of Reserved Seats"\label{qDD})
postfoot("GDP Control & Y & Y & Y & Y  \\"
         "Democracy Indicators &  & Y &  & Y \\"
         "\bottomrule\multicolumn{5}{p{13.2cm}}{\begin{footnotesize} Columns "
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
eststo: xtreg antenatalt1 `cntrl1' i.year quotaRes, fe cluster(ccode)

eststo: xtreg birthAttendt1 quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg birthAttendt1 quotaRes `cntrl1' i.year, fe cluster(ccode)

eststo: xtreg healthExpt1 i.year quotaRes lnGDP , fe cluster(ccode)
eststo: xtreg healthExpt1 `cntrl1' i.year quotaRes, fe cluster(ccode)

eststo: xtreg fEduct1 quotaRes lnGDP i.year, fe cluster(ccode)
eststo: xtreg fEduct1 quotaRes `cntrl1' i.year, fe cluster(ccode)


#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 using "$OUT/quotaMechanism.tex",
booktabs cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes _cons)
collabels(none) label mlabels(, none) replace
mgroups("Antenatal Care" "Attended Births" "Health Spending" "Women's Education",
        pattern(1 0 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span
        erepeat(\cmidrule(lr){@span}))
title("Diff-in-diffs estimates of the effect of Reserved Seats on Intermediate Outcomes"\label{mDD})
postfoot("GDP Control & Y & Y & Y & Y & Y & Y & Y & Y \\"
         "Democracy Indicators & & Y & & Y & & Y & & Y \\"
         "\bottomrule\multicolumn{9}{p{12.6cm}}{\begin{footnotesize} Antenatal"
         "coverage and birth attendance are accessed from the World Bank     "
         "databank."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

local qvars quotaRes quotaCand
eststo: xtreg antenatalt1 i.year `qvars' lnGDP , fe cluster(ccode)
eststo: xtreg antenatalt1 `cntrl1' i.year  `qvars', fe cluster(ccode)
eststo: xtreg birthAttendt1 `qvars' lnGDP i.year, fe cluster(ccode)
eststo: xtreg birthAttendt1 `qvars' `cntrl1' i.year, fe cluster(ccode)
eststo: xtreg healthExpt1 i.year `qvars' lnGDP , fe cluster(ccode)
eststo: xtreg healthExpt1 `cntrl1' i.year `qvars', fe cluster(ccode)
eststo: xtreg fEduct1 `qvars' lnGDP i.year, fe cluster(ccode)
eststo: xtreg fEduct1 `qvars' `cntrl1' i.year, fe cluster(ccode)


#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8 using "$OUT/quotaMechanism-both.tex",
booktabs cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(`qvars' _cons)
collabels(none) label mlabels(, none) replace
mgroups("Antenatal Care" "Attended Births" "Health Spending" "Women's Education",
        pattern(1 0 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span
        erepeat(\cmidrule(lr){@span}))
title("Diff-in-diffs estimates of the effect of Reserved Seats on Intermediate Outcomes"\label{mDD})
postfoot("GDP Control & Y & Y & Y & Y & Y & Y & Y & Y \\"
         "Democracy Indicators & & Y & & Y & & Y & & Y \\"
         "\bottomrule\multicolumn{9}{p{12.6cm}}{\begin{footnotesize} Antenatal"
         "coverage and birth attendance are accessed from the World Bank     "
         "databank."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear

*-------------------------------------------------------------------------------
*--- (7a) Event Study Women in Parliament -- Reserved Seats
*-------------------------------------------------------------------------------
gen quotayearL = quotayear if quotatype=="Legislated Candidate Quota"
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
*--- (7b) Event Study MMR -- Reserved Seats
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


*-------------------------------------------------------------------------------
*--- (7b) Event Study MMR -- Both
*-------------------------------------------------------------------------------
gen quotaL   = year>=quotayearL

gen prepostL = year-(quotayearL-1) if democ!=.

bys country: egen qcountryL = max(quotaL)
sum prepostL
local min = (-1*r(min))
local max = r(max)-1

foreach num of numlist 2(1)`min' {
    gen intyear = prepostL==-`num'
    gen LquotaLag`num' = qcountryL*intyear
    drop intyear
}
foreach num of numlist 0(1)`max' {
    gen intyear = prepostL==`num'
    gen LquotaLead`num' = qcountryL*intyear
    drop intyear
}

local evals quotaLag* quotaLead* LquotaLag* LquotaLead*
xtreg lnMMRt1 i.year `evals' lnGDP i.democ, fe cluster(ccode)
local j = 1
gen PointER = .
gen UBoundR = .
gen LBoundR = .
gen PointEL = .
gen UBoundL = .
gen LBoundL = .
foreach num of numlist `min'(-1)2 {
    replace time   = -`num' in `j'
    cap replace PointER = _b[quotaLag`num'] in `j'
    cap replace LBoundR = _b[quotaLag`num']-1.96*_se[quotaLag`num'] in `j'
    cap replace UBoundR = _b[quotaLag`num']+1.96*_se[quotaLag`num'] in `j'
    cap replace PointEL = _b[LquotaLag`num'] in `j'
    cap replace LBoundL = _b[LquotaLag`num']-1.96*_se[LquotaLag`num'] in `j'
    cap replace UBoundL = _b[LquotaLag`num']+1.96*_se[LquotaLag`num'] in `j'
    local ++j
}
replace time = -1 in `j'
replace PointER = 0 in `j'
replace LBoundR = 0 in `j'
replace UBoundR = 0 in `j'
replace PointEL = 0 in `j'
replace LBoundL = 0 in `j'
replace UBoundL = 0 in `j'
local ++j
foreach num of numlist 0(1)`max' {
    replace time   =  `num' in `j'
    cap replace PointER = _b[quotaLead`num'] in `j'
    cap replace LBoundR = _b[quotaLead`num']-1.96*_se[quotaLead`num'] in `j'
    cap replace UBoundR = _b[quotaLead`num']+1.96*_se[quotaLead`num'] in `j'
    cap replace PointEL = _b[LquotaLead`num'] in `j'
    cap replace LBoundL = _b[LquotaLead`num']-1.96*_se[LquotaLead`num'] in `j'
    cap replace UBoundL = _b[LquotaLead`num']+1.96*_se[LquotaLead`num'] in `j'
    local ++j
}

#delimit ;
twoway line PointER time || rcap LBoundR UBoundR time,
scheme(s1mono) ytitle("log(Maternal Deaths)")
yline(0, lpattern(dash)) xline(-1, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year -1 is omitted as the base case.");
graph export "$OUT/eventlnMDeathL.eps", as(eps) replace;

twoway line PointEL time, lcolor(gs2) || rcap LBoundL UBoundL time, lcolor(gs6)
scheme(s1mono) ytitle("log(Maternal Deaths)")
yline(0, lpattern(dash)) xline(-1, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year -1 is omitted as the base case.");
graph export "$OUT/eventlnMDeathR.eps", as(eps) replace;

#delimit cr
drop time PointE UBound LBound prepost qcountry `evals'
exit
    
    
*-------------------------------------------------------------------------------
*--- (7c) Event Study Male mortality -- Reserved Seats
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
*/                                                   
*-------------------------------------------------------------------------------
*--- (8) Quota uptake
*-------------------------------------------------------------------------------
gen quotaImp = quotayear==year
gen ODA      = dt_oda
replace ODA  = 0 if ODA==.|ODA<0
replace ODA  = ODA/ny_gdp_m
*replace ODA  = log(ODA+1)
*replace ODA  = 0 if ODA==.
*replace ODA = ODA/1000000000
replace peacekeepers = 0 if peacekeepers==.
replace peacekeepers = peacekeepers/10000
bys country: gen lag1ODA = ODA[_n-1]
bys country: gen lag2ODA = ODA[_n-2]
bys country: gen lead1ODA = ODA[_n+1]
bys country: gen lead2ODA = ODA[_n+2]
bys country: gen lag1GDP = lnGDP[_n-1]
bys country: gen lag2GDP = lnGDP[_n-2]
bys country: gen lag1MMR = lnMMR[_n-2]
bys country: gen lag2MMR = lnMMR[_n-3]
bys country: gen lag1PK =  peacekeepers[_n-1]
bys country: gen lag2PK =  peacekeepers[_n-2]

gen execR = execrlc==1 if execrlc!=.
gen execL = execrlc==2 if execrlc!=.
gen execC = execrlc==3 if execrlc!=.
gen execN = execrlc!=1&execrlc!=2&execrlc!=3 if execrlc!=.
replace yrsoffc=. if yrsoffc==-999
gen control=allhouse==1
replace herfgov=. if herfgov==-999
gen transition=tensys>=1&tensys<5

tab quotaImp
#delimit ;
local lev  ODA peacekeepers Drights execR execL execN yrsoffc herfgov oppvote transition; 
local lag1 lag1ODA lag1PK lag1Drights;
local lag2 lag1ODA lag2ODA lag1PK lag2PK lag1Drights lag2Drights;
local lead lead1ODA lead2ODA;
#delimit cr

estimates clear
eststo: reg   quotaImp `lev'               i.year, cluster(country)
eststo: reg   quotaImp `lev' `lag1'        i.year, cluster(country)
eststo: reg   quotaImp `lev' `lag2'        i.year, cluster(country)
eststo: reg   quotaImp `lev' `lag2' `lead' i.year, cluster(country)
eststo: xtreg quotaImp `lev'               i.year, cluster(country) fe
eststo: xtreg quotaImp `lev' `lag1'        i.year, cluster(country) fe 
eststo: xtreg quotaImp `lev' `lag2'        i.year, cluster(country) fe
eststo: xtreg quotaImp `lev' `lag2' `lead' i.year, cluster(country) fe

lab var ODA "Overseas Development Assistance"
lab var peacekeepers "Peace Keepers"
lab var lag1ODA "First Lag (ODA)"
lab var lag2ODA "Second Lag (ODA)"
lab var lead1ODA "First Lead (ODA)"
lab var lead2ODA "Second Lead (ODA)"
lab var lag1PK "First Lag (peace keepers)"
lab var lag2PK "Second Lag (peace keepers)"
lab var lag1GDP "First Lag (GDP)"
lab var lag2GDP "Second Lag (GDP)"
lab var Drights "Change in Women's Rights"
lab var execR   "Right Wing Executive"
lab var execL   "Left Wing Executive"
lab var yrsoffc "Years in Power"
lab var control "Controls both Chambers"
lab var herfgov "Herfindahl Index"
lab var oppvote "Vote Share Opposition"
lab var transition "Transitioning Regime"
lab var lag1Drights "First Lag ($\Delta$ Womens Rights)"
lab var lag2Drights "Second Lag ($\Delta$ Womens Rights)"



*order(ODA lag1ODA lag2ODA `lead' peacekeepers lag1PK lag2PK
*      lnGDP lag1GDP lag2GDP lnMMR lag1MMR lag2MMR)

#delimit ;
esttab est1 est2 est3 est5 est6 est7 using
"$OUT/quotaImplementation.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(`lev' `lag1' `lag2' _cons)
collabels(none) label mlabels(, none) replace drop(execN _cons)
mgroups("No Country Fixed Effects" "Country Fixed Effects", pattern(1 0 0 1 0 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("The Passage of Reserved Seat Legislation"\label{qPass})
postfoot("\bottomrule\multicolumn{7}{p{15.8cm}}{\begin{footnotesize} Each    "
         "column regresses a variable indicating whether a quota law was     "
         "passed in a given year on proposed explanations of quota adoption. "
         "Each specification includes year fixed effects and standard errors "
         " are clustered by country.  Overseas Development Assistance (ODA)  "
         "measured as net inflows in current US dollars divided by GDP       "
         "in current US dollars is generated from the World Bank Data Bank.  "
         "Peacekeepers (measured in 1000s) are from the IPI Peacekeeping     "
         "Database, changes in women's rights refer to changes in economic   "
         "rights for women as compiled by the CIRI Human Rights Data Project,"
         " and political measures including the orientation of leader's      "
         "party, the time in power, Herfindahl Index of parties, vote shares "
         "and regime types and changes are recorded by the Database of       "
         "Political Institutions. Additional lags of relevant variables are  "
         "included in columns 2 and 3, and 5 and 6."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear


local cntrl1  lnGDP i.democ
local cntrl2  lnGDP i.democ healthExp yrs_school `lev' `lag2'

eststo: xtreg lnMMRt1 i.year quotaRes lnGDP, fe cluster(ccode)
eststo: xtreg womparl quotaRes lnGDP i.year if e(sample)==1, fe cluster(ccode)

eststo: xtreg lnMMRt1 quotaRes `cntrl2' i.year if e(sample)==1, fe cluster(ccode)
xtreg lnMMRt1 i.year quotaRes lnGDP if e(sample)==1, fe cluster(ccode)

eststo: xtreg womparl quotaRes `cntrl2' i.year if e(sample)==1, fe cluster(ccode)
xtreg womparl i.year quotaRes lnGDP if e(sample)==1, fe cluster(ccode)
exit
#delimit ;
esttab est1 est3 est2 est4 using "$OUT/quotasExplanations.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) keep(quotaRes `lev' `lag1' `lag2')
collabels(none) label mlabels(, none) replace drop(execN)
mgroups("ln(Maternal Mortality Ratio)" "\% Women in Parliament", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Estimates Including All Potential Quota Predictors"\label{qDDpred})
postfoot("GDP Control & Y & Y & Y & Y \\"
         "Proposed Predictors & & Y & & Y \\"
         "\bottomrule\multicolumn{5}{p{15.2cm}}{\begin{footnotesize} Each "
         "regression includes country and year fixed effects and clusters "
         "standard errors by country. A number of (small) countries do not"
         "have a democracy score from Polity IV, and additional countries "
         "do not have educational or health care spending control data for"
         "all years. Refer to table \ref{qDDsamp} for the estimates       "
         "consistently using the sample where all covariates are available."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
