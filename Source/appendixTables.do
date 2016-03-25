/* appendixTables.do v0.00       damiancclarke             yyyy-mm-dd:2016-03-23
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

  Alternative specifications and tests reported for the paper "Women's Political
Representation and Maternal Mortality: Cross-National, Longitudinal Analysis of
150 Countries between 1990-2010".

*/

vers 11
clear all
set more off
cap log close



********************************************************************************
*** (1) Set globals and locals
********************************************************************************
global DAT "~/investigacion/2013/WorldMMR/Data"
global OUT "~/investigacion/2013/WorldMMR/Results/WomenParliament/Appendix"
global LOG "~/investigacion/2013/WorldMMR/Log"
global DAT "/media/ubuntu/Impar/investigacion/2013/WorldMMR/Data"
global OUT "/media/ubuntu/Impar/investigacion/2013/WorldMMR/Results/WomenParliament/Appendix"
global LOG "/media/ubuntu/Impar/investigacion/2013/WorldMMR/Log"

cap mkdir $OUT
log using "$LOG/appendixTables.txt", text replace

#delimit ;
local estopt cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
             (r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations))
	     starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label;
#delimit cr



********************************************************************************
*** (2) Open data, generate vars
********************************************************************************
use "$DAT/LangGender_dataset.dta", clear
drop if not_country == 1

drop loggdppc_5
gen loggdppc_5 = log(gdpppp1_5)

quietly sum loggdppc_5
replace loggdppc_5  = loggdppc_5 - r(min)
gen lMMR            = log(MMR)
gen lMMR_DHS        = log(MMR_b_DHS100_5)
gen ltb_5           = log(tb_5 + 1)
gen ltb_death_5     = log(tb_death_rate + 1)
gen lmale_1549      = log(male_1549)
gen ln_ratio_FM1549 = log(ratio_FM1549)
gen womparl_gdp_5   = womparl_5*loggdppc_5
gen all             = 1
tab year, gen(_yFE)

lab var lMMR            "Log(MMR)"
lab var lMMR_DHS        "Log(MMR)"
lab var ltb_5           "Log of TB"
lab var ltb_death_5     "Log of TB death rate"
lab var lmale_1549      "Log of male mortality (15-49 years)"
lab var ln_ratio_FM1549 "Log of female/male mortality (15-49 years)"
lab var womparl_5       "\% Women in Parliament"
lab var loggdppc_5      "Log(GDP)"
lab var democ_5         "Democracy score"
lab var health_exp_5    "Health expenditure"
lab var yr_sch_impute   "Female years of schooling"
lab var lpop_5          "Log of Population"
lab var womparl_gdp_5   "Women Parl. $\times$ Log(GDP)"

sum loggdppc_5, d
gen lowinc = loggdppc_5<`r(mean)'


********************************************************************************
*** (2b) Descriptive figures
********************************************************************************
preserve
keep if lMMR_DHS!=.&lMMR!=.
#delimit;
scatter lMMR lMMR_DHS, mcolor(black) ||
   lfit lMMR lMMR_DHS, lcolor(red) lpattern(dash)
scheme(s1mono) xtitle("log(MMR) from WDI")
ytitle("log(MMR) from DHS") legend(lab(1 "log(MMR)") lab(2 "Fitted Value"));
graph export "$OUT/MMRcomparison.eps", replace;

restore;
preserve;
collapse lMMR_DHS lMMR, by(isocode);
keep if lMMR_DHS!=.&lMMR!=.;
scatter lMMR lMMR_DHS, msymbol(i) mlabel(isocode) ||
   lfit lMMR lMMR_DHS, lcolor(red) lpattern(dash)
  scheme(s1mono) xtitle("log(MMR) from WDI")
ytitle("log(MMR) from DHS") legend(lab(1 "log(MMR)") lab(2 "Fitted Value"));
graph export "$OUT/MMRcomparison_country.eps", replace;
#delimit cr
corr lMMR lMMR_DHS
restore

********************************************************************************
*** (2c) Summary Stats
********************************************************************************
qui reg lMMR womparl_5 loggdppc_5 democ_5 health_exp_5 yr_sch_impute
preserve
keep if e(sample)

lab var MMR_b_DHS100_5 "MMR from DHS"
lab var male_1549      "Male mortality (15-49 year-olds)"
lab var tb_5           "Tuberculosis Incidence (per 100,000 people)"
lab var tb_death_rate  "Tuberculosis Deaths (per 100,000 people)"
lab var gdppc_5        "GDP per capita"
lab var loggdppc_5     "log(GDP) per capita"
lab var health_exp_5   "Health expenditure (\% of GDP)"
lab var year           "Year"

local statform cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))")
local svar MMR MMR_b_DHS100_5 womparl_5 gdppc_5 loggdppc_5 democ_5 /*
*/ health_exp_5 yr_sch_impute male_1549 tb_5 tb_death_rate  year

estpost sum `svar' 
estout using "$OUT/MainSum.tex", replace label style(tex) `statform' 
foreach year of numlist 1995 2000 2005 2010 {
    estpost sum `svar' if year==`year'
    estout using "$OUT/Main`year'.tex", replace label style(tex) `statform' 
}
restore    

********************************************************************************
*** (3) Set locals
********************************************************************************
local dep "lMMR" 
local w_right "womparl_5"


local indep1 womparl_5 loggdppc_5                                          _yFE*
local indep2 `indep1' womparl_gdp_5                                        _yFE*
local indep3 `indep1' womparl_gdp_5 democ_5                                _yFE*
local indep4 `indep1' womparl_gdp_5 democ_5 health_exp_5 yr_sch_impute     _yFE*
local indep5 `indep4' i.contcode#i.year _yFE*


********************************************************************************
*** (4a) Main Regressions
********************************************************************************
preserve
xtreg `dep' `indep5', `se' fe
keep if e(sample)
foreach num of numlist 1(1)5 { 
    qui eststo: xtreg `dep' `indep`num'', `se' fe
}
#delimit ;
esttab est1 est2 est3 est4 est5 using "$OUT/MMRWomParl.tex", replace 
`estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5 democ_5)
title("The Effect of Women in Parliament on Rates of Maternal Mortality") 
style(tex) mlabels(, depvar) booktabs
postfoot("Country and Year FE  & Y & Y & Y & Y & Y\\                          "
         "Health/Educ Controls &   &   &   & Y & Y\\                          "
         "Continent by Year FE & & & & & Y \\ \bottomrule                     "
         "\multicolumn{6}{p{16cm}}{\begin{footnotesize}\textsc{Notes:}        "
         "The estimation sample consists of all countries for which WDI       "
         "maternal mortality data and full controls are avaialable.  Health   "
         "expenditure controls are not available in 1990 (refer to            "
         "supplementary tables for a version without health expenditure       "
         "controls). Health and Education controls (columns 4 and 5) refer to "
         "total health spending as a proportion of GDP, and average years of  "
         "women's education in the population of over 21 year-olds. Standard  "
         "errors clustered by country are presented in parentheses, and stars "
         "refer to statistical significance levels.   "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}");
#delimit cr

estimates clear
restore



    
local conds all==1 lowinc==0 lowinc==1
local names All HighInc LowInc
local se vce(cluster isocode)
tokenize `names'


foreach c of local conds {
	local topt
	if `"`1'"'=="LowInc" local topt "low income"
	if `"`1'"'=="HighInc" local topt "high income"

	preserve	
	keep if `c'
        quietly sum loggdppc_5
        replace loggdppc_5  = loggdppc_5 - r(min)
        replace womparl_gdp_5   = womparl_5*loggdppc_5

	quietly xtreg  `dep' `indep1', vce(cluster isocode)  fe
	xtreg `dep' `indep5', `se' fe
	keep if e(sample)
	foreach num of numlist 1(1)5 { 
            qui eststo: xtreg `dep' `indep`num'', `se' fe
	}
	#delimit ;
	esttab est1 est2 est3 est4 est5 using "$OUT/MMRParl`1'.tex", replace 
	`estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5 democ_5)
	title("Maternal Mortality Ratio and Women in Parliament (`topt' countries)") 
	style(tex) mlabels(, depvar) booktabs
	postfoot("Country and Year FE & Y & Y & Y & Y & Y\\                     "
                 "Health/Educ Controls &   &   &   & Y & Y\\                    "
                 "Continent by Year FE & & & & & Y \\ \bottomrule               "
                 "\multicolumn{6}{p{16cm}}{\begin{footnotesize}\textsc{Notes:}  "
                 "Estimation sample consists of all `topt' countries only, where"
                 "the income threshold is defined based on median per capita    "
                 "income in the sample. Each specification is identical to table"
                 "2 in the main paper.  Standard errors clustered by country are"
                 "presented in parentheses, and stars refer to significance     "
                 "levels."
                 "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
	"\end{footnotesize}}\end{tabular}\end{table}");
	#delimit cr
	
	estimates clear
	macro shift
	restore
}

********************************************************************************
*** (4c) Weighted
********************************************************************************
preserve
bys cncode: egen popwt = mean(pop_5)

xtreg `dep' `indep5', `se' fe
keep if e(sample)
foreach num of numlist 1(1)5 { 
    qui eststo: xtreg `dep' `indep`num'' [aw=popwt], `se' fe
}
#delimit ;
esttab est1 est2 est3 est4 est5 using "$OUT/MMRWomParl_popln.tex", replace 
`estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5 democ_5)
title("The Effect of Women in Parliament on Rates of Maternal Mortality") 
style(tex) mlabels(, depvar) booktabs
postfoot("Country and Year FE  & Y & Y & Y & Y & Y\\                          "
         "Health/Educ Controls &   &   &   & Y & Y\\                          "
         "Continent by Year FE & & & & & Y \\ \bottomrule                     "
         "\multicolumn{6}{p{16cm}}{\begin{footnotesize}\textsc{Notes:}        "
         "The estimation sample consists of all countries for which WDI       "
         "maternal mortality data and full controls are avaialable.  Health   "
         "expenditure controls are not available in 1990 (refer to            "
         "supplementary tables for a version without health expenditure       "
         "controls). Health and Education controls (columns 4 and 5) refer to "
         "total health spending as a proportion of GDP, and average years of  "
         "women's education in the population of over 21 year-olds. Standard  "
         "errors clustered by country are presented in parentheses, and stars "
         "refer to statistical significance levels.   "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}");
#delimit cr

estimates clear
restore


********************************************************************************
*** (4c) Replace WDI with DHS Measure
********************************************************************************
preserve
local dep "lMMR_DHS"
xtreg `dep' `indep5', `se' fe
keep if e(sample)
quietly sum loggdppc_5
replace loggdppc_5  = loggdppc_5 - r(min)
replace womparl_gdp_5   = womparl_5*loggdppc_5

foreach num of numlist 1(1)5 { 
    qui eststo: xtreg `dep' `indep`num'', `se' fe
}
#delimit ;
esttab est1 est2 est3 est4 est5 using "$OUT/MMRParl_DHS.tex", replace 
`estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5 democ_5)
title("Estimates Replacing WDI Measure of MMR with DHS Measure") booktabs
style(tex) mlabels(, depvar)
postfoot("Country and Year FE & Y & Y & Y & Y & Y\\                           "
         "Health/Educ Controls &   &   &   & Y & Y\\                          "
         "Continent by Year FE & & & & & Y \\ \bottomrule                     "
         "\multicolumn{6}{p{16cm}}{\begin{footnotesize}\textsc{Notes:}        "
         "Estimation sample consists of all DHS countries in which the        "
         "maternal mortality module has been applied. Each specification is   "
         "identical to the counterpart in table 2 of the main paper, however  "
         "the dependent variable (MMR) has been replaced with the DHS         "
         "sisterhood-method value. Standard errors clustered by country are   "
         "presented in parentheses, and stars refer to significance levels.   "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}");
#delimit cr

estimates clear

local dep "lMMR"
foreach num of numlist 1(1)5 { 
    qui eststo: xtreg `dep' `indep`num'', `se' fe
}
#delimit ;
esttab est1 est2 est3 est4 est5 using "$OUT/MMRParl_DHSSample.tex", replace 
`estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5 democ_5)
title("Estimates Using DHS Sample and WDI MMR Variable") booktabs
style(tex) mlabels(, depvar)
postfoot("Country and Year FE & Y & Y & Y & Y & Y\\                           "
         "Health/Educ Controls &   &   &   & Y & Y\\                          "
         "Continent by Year FE & & & & & Y \\ \bottomrule                     "
         "\multicolumn{6}{p{16cm}}{\begin{footnotesize}\textsc{Notes:}        "
         "Estimation sample consists of all DHS countries in which the        "
         "maternal mortality module has been applied, and the WDI maternal    "
	 "mortality data is used.  Standard errors clustered by country are   "
         "presented in parentheses, and stars refer to significance levels.   "
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}");
#delimit cr

estimates clear

restore


********************************************************************************
*** (4d) Placebo tests
********************************************************************************
lab var lmale_1549  "log(Mort)"
lab var ltb_death_5 "log(TB)"
    
local i=1
foreach placebo of varlist lmale_1549 ltb_death_5 {
    preserve
    if `i'==1 local name MMort
    if `i'==2 local name TBDeath
    if `i'==1 local cm   15.6cm
    if `i'==2 local name 13.6cm
    if `i'==1 local note Male mortality rate (15-49)
    if `i'==2 local note Rates of Tuberculosis Death

    
    xtreg `placebo' `indep5', `se' fe
    keep if e(sample)
    quietly sum loggdppc_5
    replace loggdppc_5  = loggdppc_5 - r(min)
    replace womparl_gdp_5   = womparl_5*loggdppc_5
    foreach num of numlist 1(1)5 { 
        qui eststo: xtreg `placebo' `indep`num'', `se' fe
    }
    #delimit ;
    esttab est1 est2 est3 est4 est5 using "$OUT/Placebo_`name'.tex", replace 
    `estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5 democ_5)
    title("Placebo Test using `note' as the Dependent Variable") booktabs
    style(tex) mlabels(, depvar)
    postfoot("Country and Year FE & Y & Y & Y & Y & Y\\                        "
             "Health/Educ Controls &   &   &   & Y & Y\\                       "
             "Continent by Year FE & & & & & Y \\ \bottomrule                  "
             "\multicolumn{6}{p{`cm'}}{\begin{footnotesize}\textsc{Notes:}     "
             "Placebo tests run identical specifications as those in table 2 of"
             "the main paper, however replace the log of MMR (a woman-specific "
             "health outcome, with the log of a male, or gender neutral health "
             "outcome. Standard errors clustered by country are presented in   "
             "parentheses, and stars refer to significance levels."
             "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
             "\end{footnotesize}}\end{tabular}\end{table}");
    #delimit cr

    estimates clear
    local ++i
    restore
}



********************************************************************************
*** (5) Alternative controls
********************************************************************************
local dep    lMMR
local indep1 womparl_5 loggdppc_5                                          _yFE*
local indep2 `indep1' womparl_gdp_5                                        _yFE*
local indep3 `indep1' womparl_gdp_5 democ_5                                _yFE*
local indep4 `indep1' womparl_gdp_5 democ_5 yr_sch_imput                   _yFE*
local indep5 `indep1' womparl_gdp_5 democ_5 yr_sch_imput i.contcode#i.year _yFE*


xtreg `dep' `indep5', `se' fe
keep if e(sample)
quietly sum loggdppc_5
replace loggdppc_5  = loggdppc_5 - r(min)
replace womparl_gdp_5   = womparl_5*loggdppc_5

foreach num of numlist 1(1)5 { 
    qui eststo: xtreg `dep' `indep`num'', `se' fe
}
#delimit ;
esttab est1 est2 est3 est4 est5 using "$OUT/MMRParl_NoHealth.tex", replace 
`estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5 democ_5)
title("Main Estimates with No Health Controls") booktabs
style(tex) mlabels(, depvar)
postfoot("Country and Year FE & Y & Y & Y & Y & Y\\                           "
         "Female Educ Controls &   &   &   & Y & Y\\                          "
         "Continent by Year FE & & & & & Y \\ \bottomrule                     "
         "\multicolumn{6}{p{16.4cm}}{\begin{footnotesize}\textsc{Notes:}      "
         "The full sample of countries from table 2 are included, however     "
         "the health expenditure control is \emph{not} included, resulting in "
         "an additional year of data (1990).  All other details are identical "
         "to table 2. Standard errors clustered by country are presented in   "
         "parentheses, and stars refer to significance levels."
         "***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}");
#delimit cr

estimates clear



********************************************************************************
*** (X) Close
********************************************************************************
log close
