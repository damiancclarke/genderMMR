/* MMRParliamentPlots.do v0.00        DCC                  yyyy-mm-dd:201603-21
----|----1----|----2----|----3----|----4----|----5----|----6----|----7---|----8

  Plots for the paper "Women's Political Representation and Maternal Mortality:
Cross-National, Longitudinal Analysis of 150 Countries between 1990-2010". These
work with the main data sample in the file "LangGender_dataset.dta". For generat
ing details see XXXXX.dta

*/

vers 11
clear all
set more off
cap log close

********************************************************************************
*** (1) Global and locals
********************************************************************************
global DAT "~/investigacion/2013/WorldMMR/Data"
global OUT "~/investigacion/2013/WorldMMR/Results/WomenParliament"    
global LOG "~/investigacion/2013/WorldMMR/Log"
global DAT "/media/ubuntu/Impar/investigacion/2013/WorldMMR/Data"
global OUT "/media/ubuntu/Impar/investigacion/2013/WorldMMR/Results/WomenParliament"    
global LOG "/media/ubuntu/Impar/investigacion/2013/WorldMMR/Log"

log using "$LOG/MMRParliamentPlots.txt", text replace

********************************************************************************
*** (1) Open data
********************************************************************************
use "$DAT/LangGender_dataset.dta", clear
drop if not_country == 1

gen lMMR            = log(MMR)
gen lMMR_DHS        = log(MMR_b_DHS100_5)
gen ltb_5           = log(tb_5 + 1)
gen lmale_1549      = log(male_1549)
gen ln_ratio_FM1549 = log(ratio_FM1549)

label variable lMMR          "ln(MMR)"
label variable lMMR_DHS      "Log of MMR from DHS"
label variable ltb_5         "Log of TB"
label variable lmale_1549    "Log of male mortality in the 15- 49 age group"
label variable womparl_5     "\% Women in Parliament"
label variable lgdp_5        "ln(GDP)"
label variable democ_5       "Democracy score"
label variable health_exp_5  "Health expenditure (\% of GDP)"

gen womparl_gdp_5 = womparl_5*lgdp_5
label variable womparl_gdp_5 "Women in Parliament $\times$ Log GDP"


keep if MMR!=.
gsort country -year
by country: gen FDMMR    = MMR[_n-1]-MMR[_n]
by country: gen FDwompar = womparl_5[_n-1]-womparl_5[_n]

********************************************************************************
*** (1) Graph by GDP level
********************************************************************************    
local dep "lMMR"
local w_right "womparl_5"

local indep1 womparl_5 lgdp_5                                              _yFE*
local indep2 `indep1' womparl_gdp_5                                        _yFE*
local indep3 `indep1' democ_5 health_exp_5                                 _yFE*
local indep4 `indep1' womparl_gdp_5 democ_5 health_exp_5                   _yFE*
local indep5 `indep1' womparl_gdp_5 democ_5 health_exp_5 i.contcode#i.year _yFE*

xtreg lMMR womparl_5 lgdp_5 womparl_gdp_5 i.year, vce(cluster isocode) fe
keep if e(sample)==1
local nQ 10

xtile GDPquant = lgdp_5, nquantile(`nQ')
foreach num of numlist 1(1)`nQ' {
    gen GDPquantile`num' = GDPquant==`num'
    gen wparlXquant`num' = GDPquantile`num'*womparl_5
}

local controls democ_5 health_exp_5 i.contcode#i.year

drop MMR
gen MMR = lMMR
gen TB  = log(tb_deaths_5)
gen MaleMortality = lmale_1549

foreach y of varlist MMR TB MaleMortality {
    xtreg `y' wparlX* lgdp_5 i.year `controls', vce(cluster isocode) fe

    preserve
    collapse lgdp_5 `y' womparl GDPquant, by(isocode)

    gen pointEstim = .
    gen upperBound = .
    gen lowerBound = .
    gen gdpQuint   = .
    foreach n of numlist 1(1)`nQ' {
    	replace pointEstim = _b[wparlXquant`n']                          in `n'
    	replace upperBound = _b[wparlXquant`n']+1.96*_se[wparlXquant`n'] in `n'
    	replace lowerBound = _b[wparlXquant`n']-1.96*_se[wparlXquant`n'] in `n'
    	replace gdpQuint   = `n' in `n'
    }


    #delimit ;
    scatter pointEstim gdpQuint, color(black) xlabel(1(1)`nQ')
    || rcap upperBound lowerBound gdpQuint, lcolor(black) scheme(s1mono)
    yline(0, lcolor(red) lwidth(thick))
    || scatter `y' lgdp_5, xaxis(2) yaxis(2) mlabel(isocode) mlabp(0) mlabs(tiny)
    jitter(2) jitterseed(27)
    msymbol(i) mlabcolor(gs7) xlabel(minmax, axis(2)) xtitle(" ", axis(2))
    xtitle("ln(GDP) Decile") ytitle("ln(`y')", axis(2))
    ytitle("{&Delta} ln(`y') / {&Delta} Women in Parliament" " ")
    legend(order(1 "Point Estimate" 2 "95% CI"));
    graph export "$OUT/nonParametricEffect_`y'.eps", as(eps) replace;
    #delimit cr
    restore
}

********************************************************************************
*** (2) Graph showing changes vs changes
********************************************************************************
drop loggdppc_5
gen loggdppc_5 = log(gdpppp1_5)
sum loggdppc_5, d
local median = r(p50)
gen lowinc = loggdppc_5<`median'


collapse FDMMR FDwompar loggdppc_5 gdpppp1_5 lowinc, by(country)
replace lowinc = . if gdpppp1_5==.

gen all = 1
local conds all==1 lowinc==1 lowinc==0
local names All LowGDP HighGDP


lab var FDMMR    "Change in Maternal Mortality Ratio"
lab var FDwompar "Change in Proportion of Women in Parliament"

tokenize `names'
foreach c of local conds {
    preserve
    keep if `c'

    
    reg FDMMR FDwompar
    local Pcoef = _b[FDwompar]
    local Ptsta = _b[FDwompar]/_se[FDwompar]
    local Ppval = 1-ttail(e(N),`Ptsta')
    local Ppval = string(`Ppval', "%5.3f")
    local Pcval = round((`Pcoef')*100)/100
    if `Pcval'==1 local Pcval 1.00

    local n1 "A 1 unit increase in representation is associated with a"
    local n2 " change in MMR (p-value = "


    #delimit ;
    scatter FDMMR FDwompar, mlabel(country) mlabsize(vsmall) mlabpos(9) m(i)||
    lfit FDMMR FDwompar, lcolor(red) lwidth(thick) lpattern(---) scheme(s1mono)
    xline(0, lwidth(thin)) yline(0, lwidth(thin))
    note("`n1' `Pcval' `n2' `Ppval')");
    graph export "$OUT/MMRwomparDeltas_`1'.eps", as(eps) replace;
    #delimit cr
    
    restore
    macro shift
}

********************************************************************************
*** (3) Calculation of sensitivity test
********************************************************************************
use "$DAT/LangGender_dataset.dta", clear
drop if not_country == 1
drop loggdppc_5
gen loggdppc_5 = log(gdpppp1_5)

gen lMMR              = log(MMR)
sum loggdppc_5 
replace loggdppc_5    = loggdppc_5 - r(min)
gen     womparl_gdp_5 = womparl_5*loggdppc_5


    
sum loggdppc_5, d
local median = r(p50)
egen iqrWParl=iqr(womparl_5) if loggdppc_5<`median'
sum iqrWParl
local iqrval = r(mean)
dis `iqrval'


local vars womparl_5 loggdppc_5 womparl_gdp_5
local cont democ_5 health_exp_5 yr_sch_impute i.year

xtreg lMMR `vars' `cont', vce(cluster isocode) fe

sum loggdppc_5 if loggdppc_5<`median'&e(sample)
local change = _b[womparl_5]*`iqrval'+_b[womparl_gdp_5]*`iqrval'*r(mean)
dis `change'

local totaldecline = 69.5/216.7
local WomenExplain = `change'/`totaldecline'

#delimit ;
dis "In total, a 1 IQR increase (`iqrval') of women in parliament in low income
     countries would reduce MMR by `change'% (approximately).  This explains
     `WomenExplain' of the entire reduction required needed to achieve the SDGs.";
#delimit cr
