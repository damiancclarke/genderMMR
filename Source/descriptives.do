/* descriptives.do v0.00          SB/DC/JG/AV              yyyy-mm-dd:2015-06-09
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

Produces summary stats and descriptives from the dataset put together by JG:
  LangGender_dataset
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

local ab abs(cncode)
local se cluster(cncode)


*-------------------------------------------------------------------------------
*--- (2) Open data and set-up
*-------------------------------------------------------------------------------
use "$DAT/LangGender_dataset"
drop if not_country

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
*--- (3) Summary stats
*-------------------------------------------------------------------------------
local health MMR MMR_b_DHS100_5 le_male le_female ln_LE_ratio lgdp tb
local gender wecon wopol wosoc gii0 womparl withoutrestrictions DSR


lab var MMR            "MMR (Deaths per 100,000 live births)"
lab var MMR_b_DHS100_5 "MMR calculated from DHS microdata"
lab var le_male        "Life Expectancy (Male)"
lab var le_female      "Life Expectancy (Female)"
lab var ln_LE_ratio    "ln(Life Expectancy Ratio)$\times$100,000 (F/M)"
lab var tb             "Tuberculosis Incidence per 100,000 people"
lab var gii0           "Gender Inequality Index"
lab var womparl        "Proportion of Women in Parliament"
lab var withoutres     "Abortion allowed without restrictions"
lab var act            "Year original abortion act was passed"

replace ln_LE_ratio = ln_LE_ratio * 100000
foreach s in health gender {
    sum ``s''
    estpost tabstat ``s'', statistics(count mean sd min max) columns(statistics)
    esttab using "$OUT/summary/`s'.tex", title("Descriptive Statistics") noobs /*
    */ cells("count(fmt(0)) mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(0))")  /*
    */ replace label 
}
