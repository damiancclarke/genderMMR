/* panelEstimates.do v0.00        SB/DC/JG/AV              yyyy-mm-dd:2015-05-18
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

File to run country*year estimates of the effect of gender inequality on female-
specific health outcomes and stocks.

The file is completely controlled by the globals and locals in section (1).

>> Change note to table
>> Change title to table
>> Change format of outcomes
>> Change x var for non 5-year outcomes
*/

vers 11
clear all
set more off
cap log close

*-------------------------------------------------------------------------------
*--- (1) globals and locals
*-------------------------------------------------------------------------------
global DAT "~/investigacion/2013/WorldMMR/Data/Gender"
global OUT "~/investigacion/2013/WorldMMR/Results"
global LOG "~/investigacion/2013/WorldMMR/Log"

log using "$LOG/panelEstimates.txt", text replace

local estopt stats (r2 N, fmt(%9.2f %9.0g) label(R-squared Observations))   /*
*/           starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label




*-------------------------------------------------------------------------------
*--- (2) Open data and set-up
*-------------------------------------------------------------------------------
use "$DAT/LangGender_dataset"
drop if not_country
tab year, gen(_Y)

gen DSR_5     = dsr1_15_25_5
gen DSR       = dsr1_15_25
gen DSR_gdp   = DSR*lgdp
gen DSR_gdp_5 = DSR_5*lgdp_5

lab var DSR         "Desired Sex Ratio"
lab var DSR_5       "Desired Sex Ratio"
lab var DSR_gdp     "Desired Sex Ratio$\times$ ln(GDP)"
lab var DSR_gdp_5   "Desired Sex Ratio$\times$ ln(GDP)"
lab var lgdp_5      "ln(GDP)"
lab var lgdp        "ln(GDP)"
lab var MMR         "MMR \ \"
lab var tb          "TB"
lab var ln_LE_ratio "LE ratio"


*-------------------------------------------------------------------------------
*--- (3) Regressions of MMR, LE advantage on DSR 
*-------------------------------------------------------------------------------
foreach y of varlist tb ln_LE_ratio MMR {
    if `"`y'"'=="MMR" local 5 _5

    local ab abs(cncode)
    local se cluster(cncode)

    eststo:  reg `y' DSR`5'                                       ,      `se'
    eststo: areg `y' DSR`5' _Y*                                   , `ab' `se'
    eststo: areg `y' DSR`5' lgdp`5' _Y*                           , `ab' `se'
    eststo: areg `y' DSR`5' lgdp`5' ideal_15_25`5' _Y*            , `ab' `se'
    eststo: areg `y' DSR`5' lgdp`5' ideal_15_25`5' DSR_gdp`5' _Y* , `ab' `se'
    
    foreach var of varlist `y' DSR {
        sum `var' if e(sample)
        local M`var'  = round(r(mean)*100)/100
        local se`var' = round( r(sd) *100)/100    
    }

    local f    1
    local t1   1990
    local t2   2012
    local time yearly
    local tp
    local mes  13
    if `"`y'"'=="MMR" {
        local Yn MMR
        local time quinquennialy
        local t2 2010
        local tp 5
        local mes 14.1
    }
    if `"`y'"'=="tb" local Yn TB
    if `"`y'"'=="ln_LE_ratio" {
        local Yn Life expectancy ratio
        local t1 1961
        local f  3
        local mes 14.8
    }

    #delimit ;
    esttab est1 est2 est3 est4 est5 using "$OUT/`y'-DSR.tex",
    replace `estopt' title("`Yn' and Desired Sex Ratio (boys/girls)")
    cells(b(star fmt(%-9.`f'f)) se(fmt(%-9.`f'f) par([ ]) )) 
    keep(_cons DSR* lgdp* DSR_gdp*) style(tex) booktabs mlabels(, depvar)
    postfoot("\midrule Country FE &&Y&Y&Y&Y\\ Year FE&&Y&Y&Y&Y\\ "
             "Desired Fertility&&&&Y&Y\\"
             "\bottomrule\multicolumn{6}{p{`mes'cm}}{\begin{footnotesize} "
             "\textsc{Notes:} DSR (desired sex ratio of boys/girls) is "
             "calculated from mothers' responses in the DHS. `Yn' data from "
             "the WHO is available `time' between `t1' and `t2'. The "
             "estimation sample consists of the 63 DHS respondent countries "
             "(developing countries) for the years in which `Yn' data is "
             "available from the WDI. The mean (sd) of `Yn' and DSR are "
             "`M`y'' (`se`y'') and `MDSR' (`seDSR'). All variables are `tp'"
             "yearly averages. Standard errors are clustered by country."
             "\end{footnotesize}}\end{tabular}\end{table}");
    #delimit cr
    estimates clear
}
