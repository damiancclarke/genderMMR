/* sulfaSuffrage.do v0.00            AV/DCC                yyyy-mm-dd:2017-01-05
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

This file uses data on historic mortality data (described in Jayachandran et al)
and suffrage data from Miller to examine the impact of the arrival of sulfa dru-
gs on rates of maternal mortality, and a gender neutral mortality cause.
*/

vers 11
clear all
set more off
cap log close

global DAT "~/investigacion/2013/WorldMMR/Data/sulfaSuffrage"
global OUT "~/investigacion/2013/WorldMMR/Results/Suffrage"
global DAT "/media/damian/DCC-0001/investigacion/2013/WorldMMR/Data/sulfaSuffrage"
global OUT "/media/damian/DCC-0001/investigacion/2013/WorldMMR/Results/Suffrage"

cap which spmap
if _rc!=0 ssc install spmap

*-------------------------------------------------------------------------------
*--- (1) Generate data
*-------------------------------------------------------------------------------     
use "$DAT/historic_mortality.dta", clear
joinby birth_state using "$DAT/Miller_women_suffrage.dta", unmatched(master)
drop _merge
merge m:1 birth_state using "$DAT/flfp_1940.dta"
rename employed flfp

gen    early = year_suff
recode early (min/1919 = 1) (1920 = 0)
gen    y = birth_year - 1937
gen    post = y>=0
gen    post_y = post*y
gen    early_y = early*y
gen    early_post = early*post
gen    early_post_y = post_y*early
gen    post_flfp    = post*flfp
gen    flfp_y       = flfp*y


gen ln_mmr = ln(mmr)
gen ln_ipr = ln(inf)


*-------------------------------------------------------------------------------
*--- (2) Summary details
*-------------------------------------------------------------------------------
preserve
use "$DAT/Miller_women_suffrage.dta", clear
rename state_name NAME
merge 1:1 NAME using "$DAT/US_db"

#delimit ;
spmap year_suffrage if NAME!="Alaska"&NAME!="Hawaii"&NAME!="Puerto Rico"
using "$DAT/US_coord_mercator", clmethod(custom)
clbreaks(1868 1911 1912 1913 1914 1917 1918 1919 1920)
id(_ID) osize(thin) legtitle("Suffrage Declaration") fcolor(Spectral)
legend(symy(*1.2) symx(*1.2) size(*1.4) rowgap(1)
       lab(1 "Test") lab(2 "< 1912") lab(3 "1912") lab(4 "1913") lab(5 "1914")
       lab(6 "1917") lab(7 "1918") lab(8 "1919") lab(9 "1920"));
graph export "$OUT/SuffrageYear.eps", as(eps) replace;
#delimit cr

restore

*-------------------------------------------------------------------------------
*--- (3) Regressions
*-------------------------------------------------------------------------------     
local c1  post early early_post
local c2  post early_post early_post_y early_y y post_y early 
local wt  [pw = pop]
local cnd if birth_year>=1925&birth_year<=1943
local se  abs(birth_state) cluster(birth_state)

lab var post         "Post Sulfa"
lab var early_post   "Early Suffrage $\times$ Post Sulfa"
lab var post_y       "Post Sulfa $\times$ Time"
lab var y            "Time"
lab var early_post_y "Early Suffrage $\times$ Post Sulfa $\times$ Time"
lab var early_y      "Early Suffrage $\times$ Time"

eststo: areg ln_mmr `c1' `wt' `cnd', `se'
eststo: areg ln_mmr `c2' `wt' `cnd', `se'
eststo: areg ln_ipr `c1' `wt' `cnd', `se'
eststo: areg ln_ipr `c2' `wt' `cnd', `se'

#delimit ;
esttab est1 est2 est3 est4 using "$OUT/SulfaSuffrage_Regs.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats 
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) drop(early) order(_cons `c2')
collabels(none) label mlabels(, none) replace
mgroups("ln(Maternal Mortality Ratio)" "ln(Pneumonia Mortality Rate)", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Difference-in-differences estimates of the effect of Sulfa Drugs")
postfoot("\bottomrule\multicolumn{5}{p{15.2cm}}{\begin{footnotesize} Each "
         "regression includes state fixed effects and clusters standard"
         "errors by state.  States are weighted by their population."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);

esttab est2 est4 using "$OUT/SulfaSuffrage_Regs_2cols.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats 
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) drop(early) order(_cons `c2')
collabels(none) label mlabels("ln(MMR)" "ln(Pneumonia)") replace
title("Difference-in-differences estimates of the effect of Sulfa Drugs"
      \label{SulfaReg})
postfoot("\bottomrule\multicolumn{3}{p{10.8cm}}{\begin{footnotesize} Each "
         "regression includes state fixed effects and clusters standard"
         "errors by state.  States are weighted by their population."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);


#delimit cr
estimates clear



gen mortdat = 1 if birth_year>=1925&birth_year<=1943&mmr!=.
bys birth_state: egen mortcover = total(mortdat)


eststo: areg ln_mmr `c2' `wt' `cnd', `se'
eststo: areg ln_mmr `c2' post_flfp flfp_y `wt' `cnd', `se'
local cnd if birth_year>=1925&birth_year<=1943&mortcover==19
eststo: areg ln_mmr `c2' `wt' `cnd', `se'
eststo: areg ln_mmr `c2' post_flfp flfp_y `wt' `cnd', `se'

lab var post_flfp "FLFP $\times$ Post Sulfa"
lab var flfp_y    "FLFP $\times$ Time"

#delimit ;
esttab est1 est3 est2 est4 using "$OUT/SulfaSuffrage_Regs_FLFP.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats 
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) drop(early) order(_cons `c2')
collabels(none) label mlabels("ln(MMR)" "ln(MMR)" "ln(MMR)" "ln(MMR)") replace
mgroups("Original Regressions" "FLFP Controls", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Difference-in-differences estimates of the effect of Sulfa Drugs (FLFP controls)"
      \label{SulfaRegFLFP})
postfoot("\bottomrule\multicolumn{5}{p{13.8cm}}{\begin{footnotesize} Each "
         "regression includes state fixed effects and clusters standard   "
         "errors by state.  States are weighted by their population.      "
         "Columns 1 and 2 replicate original regressions for the full     "
         "sample and balanced mortality data sample.  Columns 3 and 4 add "
         "indicators for baseline female labour force participation rate  "
         "(from the 1930 census microdata file) interacted with the       "
         "post-Sulfa dummy and a linear time trend."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
#delimit cr
estimates clear
exit

eststo: areg ln_mmr `c1' `wt' `cnd', `se'
eststo: areg ln_mmr `c2' `wt' `cnd', `se'
eststo: areg ln_ipr `c1' `wt' `cnd', `se'
eststo: areg ln_ipr `c2' `wt' `cnd', `se'

#delimit ;
esttab est1 est2 est3 est4 using "$OUT/SulfaSuffrage_Regs_cover.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats 
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) drop(early)
collabels(none) label mlabels(, none) replace
mgroups("ln(Maternal Mortality Ratio)" "ln(Pneumonia Mortality Rate)", pattern(1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
title("Difference-in-differences estimates of the effect of Sulfa Drugs (Balanced Data)")
postfoot("\bottomrule\multicolumn{5}{p{15.2cm}}{\begin{footnotesize} Each "
         "regression includes state fixed effects and clusters standard"
         "errors by state.  States are weighted by their population. In"
         "these specifications only states with mortality data for all years"
         "are used, resulting in a balanced panel of 34 states from 1925-1934."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);
esttab est2 est4 using "$OUT/SulfaSuffrage_Regs_cover_2cols.tex", booktabs
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats 
(N r2, fmt(%9.0g %5.3f) label(Observations R-Squared))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) drop(early) order(_cons `c2')
collabels(none) label mlabels("ln(MMR)" "ln(Pneumonia)") replace
title("Difference-in-differences estimates of the effect of Sulfa Drugs (Balanced Data)")
postfoot("\bottomrule\multicolumn{3}{p{10.9cm}}{\begin{footnotesize} Each "
         "regression includes state fixed effects and clusters standard"
         "errors by state.  States are weighted by their population. In"
         "these specifications only states with mortality data for all years"
         "are used, resulting in a balanced panel of 34 states from 1925-1934."
         "* p$<$0.10; ** p$<$0.05; *** p$<$0.01."
         "\end{footnotesize}}\end{tabular}\end{table}") style(tex);

#delimit cr



*-------------------------------------------------------------------------------
*--- (4) Graphical and event study
*-------------------------------------------------------------------------------     
preserve
collapse (mean) mmr inf [pw = pop], by(birth_year early)
gen ln_mmr = ln(mmr)
gen ln_ipr = ln(inf)
lab def st 0 "Late Suffrage" 1 "Early Suffrage"
lab val early st

lgraph ln_mmr birth_year early if birth_year>=1925&birth_year<=1943, /*
*/ scheme(s1mono) legend(label(1 "Late Suffrage") label(2 "Early Suffrage")) /*
*/ ytitle("ln(Maternal Mortality Ratio)") xtitle("Year") /*
*/ xline(1937, lpattern(dash) lcolor(red))
graph export "$OUT/MMRtrends.eps", as(eps) replace
lgraph ln_ipr birth_year early if birth_year>=1925&birth_year<=1943, /*
*/ scheme(s1mono) legend(label(1 "Late Suffrage") label(2 "Early Suffrage")) /*
*/ ytitle("ln(Pneumonia/Influenza Mortality Rates)") xtitle("Year") /*
*/ xline(1937, lpattern(dash) lcolor(red))
graph export "$OUT/IPRtrends.eps", as(eps) replace
restore

***EVENT STUDY
foreach y of numlist 6(-1)-12 {
    if `y'>=0 local n p
    if `y'<0 local  n m
    local num `=abs(`y')'
    
    gen interaction = y==`y'
    gen event`n'`num'= interaction*early
    drop interaction
}

set more off
preserve
local i=1
foreach var of varlist ln_mmr ln_ipr {
    local title MMR
    if `"`var'"'=="ln_ipr" local title IPR
    
    areg `var' eventp* eventm2-eventm12 i.birth_year [pw = pop] if/*
    */ birth_year>=1925&birth_year<=1943, abs(birth_state) robust cluster(birth_state)


    local j=1
    gen j=.
    qui gen `title'est=0 in 18
    qui gen `title'uCI=0 in 18
    qui gen `title'lCI=0 in 18
    foreach var of varlist eventp* eventm2-eventm12 {
        replace j=`j' in `j'
        qui replace `title'est = _b[`var'] in `j'
        qui replace `title'uCI = _b[`var']+1.96*_se[`var'] in `j'
        qui replace `title'lCI = _b[`var']-1.96*_se[`var'] in `j'
        local ++j
    }

    gsort -j
    gen time = _n-13 in 1/18
    replace time = time +1 if time>=-1

    list `title'est time `title'uCI `title'lCI j in 1/18
    #delimit ;
    twoway line `title'est time || rcap `title'lCI `title'uCI time,
    scheme(s1mono) ytitle("`title'") yline(0, lpattern(dash)) xline(-1, lcolor(red))
    legend(order(1 "Point Estimate" 2 "95% CI"))
    note("Year -1 is omitted as the base case.");
    graph export "$OUT/event`title'.eps", as(eps) replace;
    #delimit cr

    if `i'==1 gsort j
    if `i'==1 drop time j

    local ++i
}
restore

local i=1
foreach var of varlist ln_mmr ln_ipr {
    local title MMR
    if `"`var'"'=="ln_ipr" local title IPR
    
    areg `var' eventp* eventm2-eventm12 i.birth_year [pw = pop] if birth_year>=1925& /*
    */ birth_year<=1943&mortcover==19, abs(birth_state) cluster(birth_state)


    local j=1
    gen j=.
    qui gen `title'est=0 in 18
    qui gen `title'uCI=0 in 18
    qui gen `title'lCI=0 in 18
    foreach var of varlist eventp* eventm2-eventm12 {
        replace j=`j' in `j'
        qui replace `title'est = _b[`var'] in `j'
        qui replace `title'uCI = _b[`var']+1.96*_se[`var'] in `j'
        qui replace `title'lCI = _b[`var']-1.96*_se[`var'] in `j'
        local ++j
    }

    gsort -j
    gen time = _n-13 in 1/18
    replace time = time +1 if time>=-1

    list `title'est time `title'uCI `title'lCI j in 1/18
    #delimit ;
    twoway line `title'est time || rcap `title'lCI `title'uCI time,
    scheme(s1mono) ytitle("`title'") yline(0, lpattern(dash)) xline(-1, lcolor(red))
    legend(order(1 "Point Estimate" 2 "95% CI"))
    note("Year -1 is omitted as the base case.");
    graph export "$OUT/event`title'_cover.eps", as(eps) replace;
    #delimit cr

    if `i'==1 gsort j
    if `i'==1 drop time j

    local ++i
}

#delimit ;
twoway line MMRest time || rcap MMRlCI MMRuCI time
    || line IPRest time, lpattern(dash) || rcap IPRlCI IPRuCI time, lpattern(dash)
scheme(s1mono) ytitle("Death Rate") yline(0, lpattern(dot)) xline(-1, lcolor(red))
legend(order(1 "Point Estimate" 2 "95% CI"))
note("Year -1 is omitted as the base case.");
graph export "$OUT/eventCombined.eps", as(eps) replace;
#delimit cr
