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

cap mkdir $OUT
log using "$LOG/appendixTables.txt", text replace

#delimit ;
local estopt cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats
             (r2 N, fmt(%9.3f %9.0g) label(R-Squared Observations))
	     starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label;
#delimit cr

cap which estout
if _rc!=0 ssc install estout

********************************************************************************
*** (2) Open data, generate vars
********************************************************************************
use "$DAT/LangGender_dataset.dta", clear
drop if not_country == 1

drop loggdppc_5
gen loggdppc_5 = log(gdpppp1_5)
gen logGDP     = log(gdpppp1_5)

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
lab var womparl_5       "% Women in Parliament"
lab var loggdppc_5      "Log(GDP)"
lab var democ_5         "Democracy score"
lab var health_exp_5    "Health expenditure"
lab var yr_sch_impute   "Female years of schooling"
lab var lpop_5          "Log of Population"
lab var womparl_gdp_5   "Women Parl. x Log(GDP)"

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

lab var MMR            "Maternal mortality per 100,000 births (WDI)"
lab var MMR_b_DHS100_5 "Maternal mortality per 100,000 births (DHS)"
lab var male_1549      "Male mortality (15-49 year-olds)"
lab var tb_death_rate  "Tuberculosis Deaths (per 100,000 people)"
lab var gdppc_5        "GDP per capita"
lab var loggdppc_5     "log(GDP) per capita"
lab var health_exp_5   "Health expenditure (% of GDP)"
lab var year           "Year"

local statform cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))")
local svar MMR MMR_b_DHS100_5 womparl_5 gdppc_5 loggdppc_5 democ_5 /*
*/ health_exp_5 yr_sch_impute male_1549 tb_death_rate 

estpost sum `svar' 
#delimit ;
esttab using "$OUT/MainSum.csv", replace label `statform' nonumber
 title(Summary Statistics) noobs delimit(";");
#delimit cr
foreach year of numlist 1995 2000 2005 2010 {
    estpost sum `svar' if year==`year'
    esttab using "$OUT/Main`year'.csv", replace label `statform' nonumber /*
    */ title(Summary Statistics) noobs
}
estpost sum `svar' if lowinc==1 
esttab using "$OUT/SumLowinc.csv", replace label `statform' nonumber /*
*/ title(Summary Statistics) noobs delimit(";")
estpost sum `svar' if lowinc==0
esttab using "$OUT/SumHighinc.csv", replace label `statform' nonumber /*
*/ title(Summary Statistics) noobs delimit(";")
restore    

********************************************************************************
*** (3) Set locals
********************************************************************************
local dep "lMMR" 
local w_right "womparl_5"
gen miss1990 = 1
replace miss1990 = . if year==1990

local indep1 womparl_5 loggdppc_5                                           _yFE*
local indep2 `indep1' womparl_gdp_5                                         _yFE*
local indep3 `indep1' womparl_gdp_5 democ_5 yr_sch_impute i.contcode#i.year _yFE*
local indep4 `indep3' miss1990
local indep5 `indep4' health_exp_5


********************************************************************************
*** (4a) Main Regressions
********************************************************************************
preserve
foreach num of numlist 3 2 1 5 4 { 
    qui eststo: xtreg `dep' `indep`num'', `se' fe
    local c`num' = e(N_g)
    keep if e(sample)==1
    if `num'==3 {
       sum `dep'
       local depmean = string(r(mean), "%5.2f")
       local depsd   = string(r(sd), "%5.2f")
       sum womparl_5
       local wommean = string(r(mean), "%5.2f")
       local womsd   = string(r(sd), "%5.2f")

       sum logGDP, d
       local minGDP = string(r(min), "%5.2f")
       local maxGDP = string(r(max), "%5.2f")

       sum loggdppc_5, d
       tempvar tv1
       gene `tv1'=_n if loggdppc_5==r(min)
       summ `tv1', meanonly
       local index=r(min)
       local mincc = country[`index']
       drop `tv1'

       sum loggdppc_5, d
       tempvar tv1
       gene `tv1'=_n if loggdppc_5==r(max)
       summ `tv1', meanonly
       local index=r(min)
       local maxcc=country[`index']
       drop `tv1'
   }
}
#delimit ;
esttab est3 est2 est1 est5 est4 using "$OUT/MMRWomParl.csv", replace 
`estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5)
title("The Effect of Women in Parliament on Rates of Maternal Mortality") 
nomtitles nonumbers delimiter(";") 
posthead("Dependent variable: Logarithm of maternal mortality ratio"
	 ";(1);(2);(3);(4);(5) ")
postfoot("Number of Countries  ;`c3';`c2';`c1';`c5';`c4'                      "
	 "Country and Year FE  ; Y ; Y ; Y ; Y ; Y                            "
	 "Controls             ;   ;   ; Y ; Y ; Y                            "
         "1995-2010 Only       ;   ;   ;   ; Y ; Y                            "
         "Health Expenditure   ;   ;   ;   ;   ; Y                            "
         "Notes: The estimation sample consists of all countries for which WDI maternal mortality data and full controls are available, with quinquennial observations from 1990-2010 (inclusive) unless otherwise indicated. Controls consist of the democracy score, average female years of education, continent by year fixed effects, and, where indicated, total health expenditure as a proportion of GDP. The mean and standard deviation of the dependent variable in the 1990-2010 sample is `depmean' and `depsd'. The mean (sd) for the percent of women in parliament in the sample is `wommean'(`womsd').  The range of inflation and PPP adjusted log GDP per capita in the sample is from `minGDP' (`mincc') to `maxGDP' (`maxcc'). When included in the regression, this variable has been standardised by subtracting the minimum value from each observation, so GDP is interpreted as the effect in the poorest country. Standard errors clustered by country are presented in parentheses, and stars refer to statistical significance levels."
"***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01.");
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

	foreach num of numlist 3 2 1 5 4 { 
    	    qui eststo: xtreg `dep' `indep`num'', `se' fe
    	    local c`num' = e(N_g)
    	    keep if e(sample)==1
            if `num'==3 {
               sum `dep'
               local depmean = string(r(mean), "%5.2f")
               local depsd   = string(r(sd), "%5.2f")
               sum womparl_5
               local wommean = string(r(mean), "%5.2f")
               local womsd   = string(r(sd), "%5.2f")
            
               sum logGDP, d
               local minGDP = string(r(min), "%5.2f")
               local maxGDP = string(r(max), "%5.2f")
            
               sum loggdppc_5, d
               tempvar tv1
               gene `tv1'=_n if loggdppc_5==r(min)
               summ `tv1', meanonly
               local index=r(min)
               local mincc = country[`index']
               drop `tv1'
            
               sum loggdppc_5, d
               tempvar tv1
               gene `tv1'=_n if loggdppc_5==r(max)
               summ `tv1', meanonly
               local index=r(min)
               local maxcc=country[`index']
               drop `tv1'
           }        
        }

	#delimit ;
	esttab est3 est2 est1 est5 est4 using "$OUT/MMRParl`1'.csv", replace 
	`estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5) nomtitles
	title("Maternal Mortality Ratio and Women in Parliament (`topt' countries)") 
	nonumbers delimiter(";")
	posthead("Dependent variable: Logarithm of maternal mortality ratio"
        	";(1);(2);(3);(4);(5)") 
        postfoot("Number of Countries  ;`c3';`c2';`c1';`c5';`c4'                "
        	 "Country and Year FE  ; Y ; Y ; Y ; Y ; Y                      "
        	 "Controls             ;   ;   ; Y ; Y ; Y                      "
                 "1995-2010 Only       ;   ;   ;   ; Y ; Y                      "
                 "Health Expenditure   ;   ;   ;   ;   ; Y                      "
 		 "The estimation sample consists of `topt' all countries for which WDI maternal mortality data and full controls are available, with quinquennial observations from 1990-2010 (inclusive) unless otherwise indicated. The income status (high versus low) is defined based on median income in the sample.  Controls consist of the democracy score, average female years of education, continent by year fixed effects, and, where indicated, total health expenditure as a proportion of GDP. The mean and standard deviation of the dependent variable in the 1990-2010 sample is `depmean' and `depsd'. The mean (sd) for the percent of women in parliament in the sample is `wommean'(`womsd'). The range of inflation and PPP adjusted log GDP per capita in the sample is from `minGDP' (`mincc') to `maxGDP' (`maxcc'). When included in the regression, this variable has been standardised by subtracting the minimum value from each observation, so GDP is interpreted as the effect in the poorest country. Standard errors clustered by country are presented in parentheses, and stars refer to statistical significance levels. ***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01.");
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

foreach num of numlist 3 2 1 5 4 { 
    qui eststo: xtreg `dep' `indep`num'' [aw=popwt], `se' fe
    local c`num' = e(N_g)
    keep if e(sample)==1
    if `num'==3 {
       sum `dep' [aw=popwt]
       local depmean = string(r(mean), "%5.2f")
       local depsd   = string(r(sd), "%5.2f")
       sum womparl_5 [aw=popwt]
       local wommean = string(r(mean), "%5.2f")
       local womsd   = string(r(sd), "%5.2f")

       sum logGDP [aw=popwt], d
       local minGDP = string(r(min), "%5.2f")
       local maxGDP = string(r(max), "%5.2f")

       sum loggdppc_5 [aw=popwt], d
       tempvar tv1
       gene `tv1'=_n if loggdppc_5==r(min)
       summ `tv1', meanonly
       local index=r(min)
       local mincc = country[`index']
       drop `tv1'

       sum loggdppc_5 [aw=popwt], d
       tempvar tv1
       gene `tv1'=_n if loggdppc_5==r(max)
       summ `tv1', meanonly
       local index=r(min)
       local maxcc=country[`index']
       drop `tv1'
   }
}
#delimit ;
esttab est3 est2 est1 est5 est4 using "$OUT/MMRWomParl_popln.csv", replace 
`estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5)
title("The Effect of Women in Parliament on Rates of Maternal Mortality") 
nomtitles nonumbers delimiter(";")
posthead("Dependent variable: Logarithm of maternal mortality ratio"
	";(1);(2);(3);(4);(5)")
postfoot("Number of Countries  ;`c3';`c2';`c1';`c5';`c4'                      "
	 "Country and Year FE  ; Y ; Y ; Y ; Y ; Y                            "
	 "Controls             ;   ;   ; Y ; Y ; Y                            "
         "1995-2010 Only       ;   ;   ;   ; Y ; Y                            "
         "Health Expenditure   ;   ;   ;   ;   ; Y                            "
         "The estimation sample consists of all countries for which WDI maternal mortality data and full controls are available, with quinquennial observations from 1990-2010 (inclusive) unless otherwise indicated. Controls consist of the democracy score, average female years of education, continent by year fixed effects, and, where indicated, total health expenditure as a proportion of GDP. The population-weighted mean and standard deviation of the dependent variable in the 1990-2010 sample is `depmean' and `depsd'. The population-weighted mean (sd) for the percent of women in parliament in the sample is `wommean'(`womsd'). The range of inflation and PPP adjusted log GDP per capita in the sample is from `minGDP' (`mincc') to `maxGDP' (`maxcc'). When included in the regression, this variable has been standardised by subtracting the minimum value from each observation, so GDP is interpreted as the effect in the poorest country. Standard errors clustered by country are presented in parentheses, and stars refer to statistical significance levels. ***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01.");
#delimit cr
estimates clear
restore

********************************************************************************
*** (4d) Replace WDI with DHS Measure
********************************************************************************
local dep "lMMR_DHS"
preserve
keep if year>=1990
foreach num of numlist 3 2 1 5 4 { 
    qui eststo: xtreg `dep' `indep`num'', `se' fe
    local c`num' = e(N_g)
    keep if e(sample)==1
    if `num'==3 {
       sum `dep'
       local depmean = string(r(mean), "%5.2f")
       local depsd   = string(r(sd), "%5.2f")
       sum womparl_5
       local wommean = string(r(mean), "%5.2f")
       local womsd   = string(r(sd), "%5.2f")

       sum logGDP, d
       local minGDP = string(r(min), "%5.2f")
       local maxGDP = string(r(max), "%5.2f")

       sum loggdppc_5, d
       tempvar tv1
       gene `tv1'=_n if loggdppc_5==r(min)
       summ `tv1', meanonly
       local index=r(min)
       local mincc = country[`index']
       drop `tv1'

       sum loggdppc_5, d
       tempvar tv1
       gene `tv1'=_n if loggdppc_5==r(max)
       summ `tv1', meanonly
       local index=r(min)
       local maxcc=country[`index']
       drop `tv1'
   }
}
#delimit ;
esttab est3 est2 est1 est5 est4 using "$OUT/MMRParl_DHS.csv", replace 
`estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5)
title("Replacing WDI Measure of Maternal Mortality Ratio with DHS Measure") 
nomtitles nonumbers delimiter(";")
posthead("Dependent variable: Logarithm of maternal mortality ratio (DHS) "
	 " ;(1);(2);(3);(4);(5)")
postfoot("Number of Countries  ;`c3';`c2';`c1';`c5';`c4'                      "
	 "Country and Year FE  ; Y ; Y ; Y ; Y ; Y                            "
	 "Controls             ;   ;   ; Y ; Y ; Y                            "
         "1995-2010 Only       ;   ;   ;   ; Y ; Y                            "
         "Health Expenditure   ;   ;   ;   ;   ; Y                            "
"The estimation sample consists of all countries in which a Demographic and Health Survey has been conducted, and in which the maternal mortality module was applied.  The DHS measure of the dependent variable is constructed for the same quinquennial periods as the WDI measure (1990-2010) wherever survey data is available. Controls consist of the democracy score, average female years of education, continent by year fixed effects, and, where indicated, total health expenditure as a proportion of GDP. The mean and standard deviation of the dependent variable in the 1990-2010 sample is `depmean' and `depsd'. The mean (sd) for the percent of women in parliament in the sample is `wommean'(`womsd'). The range of inflation and PPP adjusted log GDP per capita in the sample is from `minGDP' (`mincc') to `maxGDP' (`maxcc'). When included in the regression, this variable has been standardised by subtracting the minimum value from each observation, so GDP is interpreted as the effect in the poorest country. Standard errors clustered by country are presented in parentheses, and stars refer to statistical significance levels. ***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01.");
#delimit cr
estimates clear
restore

preserve
keep if lMMR_DHS!=.
local dep "lMMR"

foreach num of numlist 3 2 1 5 4 { 
    qui eststo: xtreg `dep' `indep`num'', `se' fe
    local c`num' = e(N_g)
    keep if e(sample)==1
    if `num'==3 {
       sum `dep'
       local depmean = string(r(mean), "%5.2f")
       local depsd   = string(r(sd), "%5.2f")
       sum womparl_5
       local wommean = string(r(mean), "%5.2f")
       local womsd   = string(r(sd), "%5.2f")

       sum logGDP, d
       local minGDP = string(r(min), "%5.2f")
       local maxGDP = string(r(max), "%5.2f")

       sum loggdppc_5, d
       tempvar tv1
       gene `tv1'=_n if loggdppc_5==r(min)
       summ `tv1', meanonly
       local index=r(min)
       local mincc = country[`index']
       drop `tv1'

       sum loggdppc_5, d
       tempvar tv1
       gene `tv1'=_n if loggdppc_5==r(max)
       summ `tv1', meanonly
       local index=r(min)
       local maxcc=country[`index']
       drop `tv1'
   }
}
#delimit ;
esttab est3 est2 est1 est5 est4 using "$OUT/MMRParl_DHSSample.csv", replace 
`estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5)
title("Estimates Using DHS Sample and WDI MMR Variable")
nomtitles nonumbers delimiter(";")
posthead("Dependent variable: Logarithm of maternal mortality ratio"
	 ";(1);(2);(3);(4);(5)")
postfoot("Number of Countries  ;`c3';`c2';`c1';`c5';`c4'                     "
	 "Country and Year FE  ; Y ; Y ; Y ; Y ; Y                           "
	 "Controls             ;   ;   ; Y ; Y ; Y                           "
         "1995-2010 Only       ;   ;   ;   ; Y ; Y                           "
         "Health Expenditure   ;   ;   ;   ;   ; Y                           "
"The estimation sample consists of all countries in which a Demographic and Health Survey has been conducted, and in which the maternal mortality module was applied.  This table uses the WDI measuse of the maternal mortality ratio, however for the DHS sample. Controls consist of the democracy score, average female years of education, continent by year fixed effects, and, where indicated, total health expenditure as a proportion of GDP. The mean and standard deviation of the dependent variable in the 1990-2010 sample is `depmean' and `depsd'. The mean (sd) for the percent of women in parliament in the sample is `wommean'(`womsd'). The range of inflation and PPP adjusted log GDP per capita in the  sample is from `minGDP' (`mincc') to `maxGDP' (`maxcc'). When included in the regression, this variable has been standardised by subtracting the minimum value from each observation, so GDP is interpreted as the effect in the poorest country. Standard errors clustered by country are presented in parentheses, and stars refer to statistical significance levels. ***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01.");
#delimit cr
estimates clear
restore



********************************************************************************
*** (4e) Placebo tests
********************************************************************************
lab var lmale_1549  "log(Mort)"
lab var ltb_death_5 "log(TB)"
    
local i=1
foreach placebo of varlist lmale_1549 ltb_death_5 {
    preserve
    keep if lMMR!=.
    if `i'==1 local name MMort
    if `i'==2 local name TBDeath
    if `i'==1 local cm   13.2cm
    if `i'==2 local cm   13.2cm
    if `i'==1 local note Male Mortality Rate (15-49)
    if `i'==2 local note Rates of Tuberculosis Death


    foreach num of numlist 3 2 1 5 4 { 
    	qui eststo: xtreg `placebo' `indep`num'', `se' fe
	local c`num' = e(N_g)
    	keep if e(sample)==1
    	if `num'==3 {
       	   sum `placebo'
       	   local depmean = string(r(mean), "%5.2f")
       	   local depsd   = string(r(sd), "%5.2f")
       	   sum womparl_5
       	   local wommean = string(r(mean), "%5.2f")
       	   local womsd   = string(r(sd), "%5.2f")

	   sum logGDP, d
       	   local minGDP = string(r(min), "%5.2f")
       	   local maxGDP = string(r(max), "%5.2f")

	   foreach s in min max {
       	       sum loggdppc_5, d
       	       tempvar tv1
       	       gene `tv1'=_n if loggdppc_5==r(`s')
       	       summ `tv1', meanonly
       	       local index=r(`s')
       	       local `s'cc = country[`index']
	       drop `tv1'
	   }
       }
    }
    if `i'==1 local ests est3 est2 est1 est5 est4
    if `i'==2 local ests est8 est7 est6 est10 est9
    if `i'==1 {
        local cM = `c4'
        local mM = `depmean'
        local sM = `depsd'
        local wM = `wommean'
        local wsM = `womsd'
    }
    #delimit ;
    esttab `ests' using "$OUT/Placebo_`name'.csv", replace 
    `estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5) nomtitles delimiter(";")
    title("Placebo Test using `note' as the Dependent Variable") nonumbers
    posthead("Dependent variable: Logarithm of `note'"
	     "  ;(1);(2);(3);(4);(5)")
    postfoot("Number of Countries  ;`c3';`c2';`c1';`c5';`c4'                  "
       	     "Country and Year FE  ; Y ; Y ; Y ; Y ; Y                        "
	     "Controls             ;   ;   ; Y ; Y ; Y                        "
             "1995-2010 Only       ;   ;   ;   ; Y ; Y                        "
             "Health Expenditure   ;   ;   ;   ;   ; Y                        "
"Placebo tests run identical specifications as those in table 1 of the main paper, however replace the log of MMR (a woman-specific health outcome, with the log of a male, or gender neutral health outcome. Controls consist of the democracy score, average female years of education, continent by year fixed effects, and, where indicated, total health expenditure as a proportion of GDP. The mean and standard deviation of the dependent variable in the 1990-2010 sample is `depmean' and `depsd'. The mean (sd) for the percent of women in parliament in the sample is `wommean'(`womsd'). The range of inflation and PPP adjusted log GDP per capita in the sample is from `minGDP' (`mincc') to `maxGDP' (`maxcc'). When included in the regression, this variable has been standardised by subtracting the minimum value from each observation, so GDP is interpreted as the effect in the poorest country. Standard errors clustered by country are presented in parentheses, and stars refer to statistical significance levels. ***p-value$<$0.01, **p-value$<$0.05, *p-value$<$0.01.");
    #delimit cr
    
    local ++i
    restore
}

#delimit ;
esttab est9 est4 using "$OUT/PlaceboTogether.csv", replace 
`estopt' keep(womparl_5 loggdppc_5 womparl_gdp_5) delimiter(";")
title("Placebo Tests using Gender Neutral Disease Burden")
mtitles("ln(TB Deaths)" "ln(Male Mortality)")
postfoot("Number of Countries  ;`c4';`cM'"
         "Mean of Dependent Variable  ;`depmean';`mM'"
         "Std Dev of Dependent Variable  ;`depsd';`sM'"
         "Mean of Women Parl in Sample;`wommean';`wM'"
         "Std Dev of Women Parl in Sample;`womsd';`wsM'"         
"Placebo tests run identical specifications as those in table 1 of the main paper, however replace the log of MMR (a woman-specific health outcome, with the log of a male, or gender neutral health outcome. Controls consist of the full set displayed in column 5 of table 1 and the country sample is identical.  The sample of years for the rates of tuberculosis death consists of 1995, 2000, 2005 and 2010, while male mortality (15-49) is available for 1995, 2000 and 2005.  Standard errors clustered by country are presented in parentheses.  All estimates presented have $ p>0.1. $");
    #delimit cr



********************************************************************************
*** (X) Close
********************************************************************************
log close
