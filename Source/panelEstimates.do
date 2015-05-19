/* panelEstimates.do v0.00        SB/DC/JG/AV              yyyy-mm-dd:2015-05-18
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

File to run country*year estimates of the effect of gender inequality on female-
specific health outcomes and stocks.

The file is completely controlled by the globals and locals in section (1).

>> Change note to table
>> Change title to table
>> Change format of outcomes

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

local estopt cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) stats /*
*/           (r2 N, fmt(%9.2f %9.0g) label(R-squared Observations))     /*
*/           starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label




*-------------------------------------------------------------------------------
*--- (2) Open data and set-up
*-------------------------------------------------------------------------------
use "$DAT/LangGender_dataset"
drop if not_country

gen DSR     = dsr1_15_25_5
gen DSR_gdp = DSR*lgdp_5

lab var DSR         "Desired Sex Ratio"
lab var DSR_gdp     "Desired Sex Ratio$\times$ ln(GDP)"
lab var lgdp_5      "ln(GDP)"
lab var MMR         "MMR \ \"
lab var tb          "TB"
lab var ln_LE_ratio "LE ratio"

*-------------------------------------------------------------------------------
*--- (3) Regressions of MMR, LE advantage on DSR 
*-------------------------------------------------------------------------------
foreach y of varlist MMR tb ln_LE_ratio {
    local ab abs(cncode)
    local se cluster(cncode)

    eststo:  reg `y' DSR                                      ,      `se'
    eststo: areg `y' DSR i.year                               , `ab' `se'
    eststo: areg `y' DSR lgdp_5 i.year                        , `ab' `se'
    eststo: areg `y' DSR lgdp_5 ideal_15_25_5 i.year          , `ab' `se'
    eststo: areg `y' DSR lgdp_5 ideal_15_25_5 DSR_gdp i.year  , `ab' `se'
    
    foreach var of varlist `y' DSR {
        sum `var'    
        local M`var'  = round(r(mean)*100)/100
        local se`var' = round( r(sd) *100)/100    
    }
    
    
    #delimit ;
    esttab est1 est2 est3 est4 est5 using "$OUT/`y'-DSR.tex",
    replace `estopt' title("`y' and Desired Sex Ratio (boys/girls)")
    keep(_cons DSR lgdp_5 DSR_gdp) style(tex) booktabs mlabels(, depvar)
    postfoot("\midrule Country FE &&Y&Y&Y&Y\\ Year FE&&Y&Y&Y&Y\\ "
             "Desired Fertility&&&&Y&Y\\"
             "\bottomrule\multicolumn{6}{p{14.2cm}}{\begin{footnotesize} "
             "\textsc{Notes:} DSR (desired sex ratio of boys/girls) is "
             "calculated from mothers' responses in the DHS.  MMR data from "
             "the WHO is available quinquienally between 1990 and 2010. The "
             "estimation sample consists of the 63 DHS respondent countries "
             "(developing countries) for the years in which MMR data is "
             "available from the WDI. The mean (sd) of `y' and DSR are "
             "`M`y'' (`se`y'') and `MDSR' (`seDSR'). All variables are 5"
             "yearly averages. Standard errors are clustered by country."
             "\end{footnotesize}}\end{tabular}\end{table}");
    #delimit cr
    estimates clear
}
