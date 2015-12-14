use "LangGender_dataset.dta", clear
drop if not_country == 1
drop if country=="Equatorial Guinea"
drop if country=="Maldives"


sum MMR MMR_b_DHS100_5 MMR_w_DHS100_5
gen lMMR = log(MMR)
gen lMMR_b_DHS = log(MMR_b_DHS100_5 + 1)
gen ltb_5 = log(tb_5 + 1)

lab var MMR "Maternal Mortality Ratio"
lab var womparl_5 "Proportion of Women in Parliament"

keep if MMR!=.
gsort country -year
by country: gen FDMMR    = MMR[_n-1]-MMR[_n]
by country: gen FDwompar = womparl_5[_n-1]-womparl_5[_n]

********************************************************************************
*** (1) Absolute
********************************************************************************    
collapse FDMMR FDwompar gdppc, by(country)
sum gdppc, d
local median = r(p50)

lab var FDMMR    "Change in Maternal Mortality Ratio"
lab var FDwompar "Change in Proportion of Women in Parliament"

reg FDMMR FDwompar if gdppc<`median'
local Pcoef = _b[FDwompar]
local Ptsta = _b[FDwompar]/_se[FDwompar]
local Ppval = 1-ttail(e(N),`Ptsta')
local Ppval = round(`Ppval'*100)/100
local Pcval = round(`Pcoef'*100)/100

reg FDMMR FDwompar if gdppc>=`median'
local Rcoef = _b[FDwompar]
local Rtsta = _b[FDwompar]/_se[FDwompar]
local Rpval = 1-ttail(e(N),`Rtsta')
local Rpval = round(`Rpval'*100)/100
local Rcval = round(`Rcoef'*100)/100

local n1 "A 1 unit increase in female representation is associated with a"
local n2 " change in MMR (p-value = "



#delimit ;
preserve;
keep if gdppc<`median';
scatter FDMMR FDwompar, mlabel(country) mlabsize(vsmall) mlabpos(9) m(i)||
    lfit FDMMR FDwompar, lcolor(red) lpattern(---) scheme(s1mono)
    xline(0, lwidth(thin)) yline(0, lwidth(thin))
note("`n1' `Pcval' `n2' `Ppval' )");
graph export MMRwomparDeltas_low.eps, as(eps) replace;
restore;

preserve;
keep if gdppc>=`median';
scatter FDMMR FDwompar, mlabel(country) mlabsize(vsmall) mlabpos(9) m(i)||
    lfit FDMMR FDwompar, lcolor(red) lpattern(---) scheme(s1mono)
    xline(0, lwidth(thin)) yline(0, lwidth(thin))
note("`n1' `Rcval' `n2' `Rpval' )");
graph export MMRwomparDeltas_high.eps, as(eps) replace;
restore;
#delimit cr


********************************************************************************
*** (2) Conditional on GDP
********************************************************************************    
reg FDMMR gdppc
predict FDMMRres, resid

reg FDwompar gdppc
predict FDwomparres, resid


collapse FDMMRr FDwomparr gdppc, by(country)
sum gdppc, d
local median = r(p50)

lab var FDMMR    "Change in Maternal Mortality Ratio"
lab var FDwompar "Change in Proportion of Women in Parliament"

reg FDMMR FDwompar if gdppc<`median'
local Pcoef = _b[FDwompar]
local Ptsta = _b[FDwompar]/_se[FDwompar]
local Ppval = 1-ttail(e(N),`Ptsta')
local Ppval = round(`Ppval'*100)/100
local Pcval = round(`Pcoef'*100)/100

reg FDMMR FDwompar if gdppc>=`median'
local Rcoef = _b[FDwompar]
local Rtsta = _b[FDwompar]/_se[FDwompar]
local Rpval = 1-ttail(e(N),`Rtsta')
local Rpval = round(`Rpval'*100)/100
local Rcval = round(`Rcoef'*100)/100

local n1 "A 1 unit increase in female representation is associated with a"
local n2 " change in MMR (p-value = "



#delimit ;
preserve;
keep if gdppc<`median';
scatter FDMMR FDwompar, mlabel(country) mlabsize(vsmall) mlabpos(9) m(i)||
    lfit FDMMR FDwompar, lcolor(red) lpattern(---) scheme(s1mono)
    xline(0, lwidth(thin)) yline(0, lwidth(thin))
note("`n1' `Pcval' `n2' `Ppval' )");
graph export MMRwomparDeltas_low_cond.eps, as(eps) replace;
restore;

preserve;
keep if gdppc>=`median';
scatter FDMMR FDwompar, mlabel(country) mlabsize(vsmall) mlabpos(9) m(i)||
    lfit FDMMR FDwompar, lcolor(red) lpattern(---) scheme(s1mono)
    xline(0, lwidth(thin)) yline(0, lwidth(thin))
note("`n1' `Rcval' `n2' `Rpval' )");
graph export MMRwomparDeltas_high_cond.eps, as(eps) replace;
restore;
#delimit cr
