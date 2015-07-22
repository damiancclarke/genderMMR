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

gen DSR_5      = dsr1_15_25_5
gen DSR        = dsr1_15_25
gen DSR_gdp    = DSR*lgdp
gen DSR_gdp_5  = DSR_5*lgdp_5
gen pReform    = progressive == year if withoutrestriction != .
gen nReform    = regressive == year if withoutrestriction != .
replace pReform = pReform*100
replace nReform = nReform*100

lab var DSR            "Desired Sex Ratio"
lab var DSR_5          "Desired Sex Ratio"
lab var DSR_gdp        "Desired Sex Ratio$\times$ ln(GDP)"
lab var DSR_gdp_5      "Desired Sex Ratio$\times$ ln(GDP)"
lab var lgdp_5         "ln(GDP)"
lab var lgdp           "ln(GDP)"
lab var MMR            "MMR \ \"
lab var tb             "TB"
lab var ln_LE_ratio    "LE ratio"
lab var pReform        "Progressive Abortion Reform$\times$100"
lab var nReform        "Regressive Abortion Reform$\times$100"
lab var female_1549    "Female Mortality (15-49 year olds)"
lab var male_1549      "Male Mortality (15-49 year olds)"
lab var female_014     "Female Mortality (0-14 year olds)"
lab var male_014       "Male Mortality (0-14 year olds)"
lab var lnratio_FM1549 "log(F/M mortality)$\times$100,000 (15-49)"

*-------------------------------------------------------------------------------
*--- (3) Summary stats
*-------------------------------------------------------------------------------
local mort female_014 male_014 female_1549 male_1549
local health MMR MMR_b_DHS100_5 le_male le_female ln_LE_ratio lgdp tb `mort'
local gender wecon wopol wosoc gii0 womparl withoutrestrictions *Reform DSR lnratio_FM1549


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


*-------------------------------------------------------------------------------
*--- (4a) Graphs as eps (LExp)
*-------------------------------------------------------------------------------
preserve
use "$DAT/LangGender_dataset", clear
replace mideast = 1 if country  == "West Bank and Gaza"

local isos EAS EAP ECS ECA HIC NOC OEC LCN LAC LDC LMY LIC LMC MEA MNA MIC SSA/*
*/ UMC WLD SAS

label variable le_total     "LE M & F"
label variable le_male      "LE males"
label variable le_female    "LE females"
label variable LE_ratio_F_M "LE ratio (F/M)"
label variable ln_LE_ratio  "LE ratio (F/M)"
label variable LE_diff_F_M  "LE difference (F - M)"


/*
foreach c of local isos {
    sort isocode year
    #delimit ;
    twoway (line le_total le_male le_female year if isocode == "`c'", yaxis(1) 
            lpattern(solid dash shortdash)), scheme(s1color) subtitle("`c'");
    graph export "$OUT/trends/LExp/`c'Trends.eps", as(eps) replace;

    twoway (line ln_LE_ratio year if isocode == "`c'", yaxis(1) lpattern(dash)) 
           (line LE_diff_F_M year if isocode == "`c'", yaxis(2) lpattern(solid))
    , scheme(s1color) subtitle("`c'");
    graph export "$OUT/trends/LExp/`c'Diff.eps", as(eps) replace;
    #delimit cr
}
*/
restore

local years 1970 1990 2010
replace ln_LE_ratio = ln_LE_ratio / 100000
label variable ln_LE_ratio  "LE ratio"

foreach yr in `years' {
    preserve
    keep if year==`yr' & lgdp!=.
    
    #delimit ;
    twoway (lfitci le_female lgdp) (scatter le_female lgdp) if year ==  `yr',
       subtitle("`yr'") scheme(s1color);
    graph export "$OUT/summary/plots/lLEGDP`yr'.eps", as(eps) replace;

    twoway (lfitci ln_LE_ratio lgdp) (scatter ln_LE_ratio lgdp) if year == `yr',
    subtitle("`yr'") scheme(s1color);
    graph export "$OUT/summary/plots/lLErGDP`yr'.eps", as(eps) replace;
    #delimit cr
    restore
}

*-------------------------------------------------------------------------------
*--- (4b) Graphs as eps (MMR)
*-------------------------------------------------------------------------------
preserve
drop if MMR_w_DHS  == . 
local years "1970 1980 1990 2000 2010"

foreach yr in `years' {
    #delimit ;
    twoway (lfitci MMR_w_DHS100_5 lgdp) (scatter MMR_w_DHS100_5 lgdp) if year==`yr',
    scheme(s1color) subtitle("`yr'");
    graph export "$OUT/summary/plots/lMMRwDHSGDP`yr'.eps", as(eps) replace;
    #delimit cr
}
restore

preserve
drop if MMR == .
local years "1990 2000 2010"

foreach yr in `years' {
    #delimit ;
    twoway (lfitci MMR lgdp) (scatter MMR lgdp) if year == `yr',
    scheme(s1color) subtitle("`yr'") yscale(range(0 2000));
    graph export "$OUT/summary/plots/lMMRWDIGDP`yr'.eps", as(eps) replace;
    #delimit cr
}
restore

*-------------------------------------------------------------------------------
*--- (4c) Graphs as eps (desired sex ratio)
*-------------------------------------------------------------------------------
preserve
use "$DAT/LangGender_dataset", clear
drop if not_country == 1

local Names lLEr lMMRbDHS lMMRWDI
tokenize `Names'

lab var dsr1_15_25_5   "DSR"
lab var MMR_b_DHS100_5 "MMR (DHS)"

foreach y of varlist ln_LE_ratio_5 MMR_b_DHS100_5 MMR {
    foreach t of numlist 1990 2000 2010 {
        #delimit ;
        twoway (lfitci `y' dsr1_15_25_5) (scatter `y' dsr1_15_25_5) if year==`t',
        subtitle("`t'") scheme(s1color);
        graph export "$OUT/dsr/plots/`1'desired`t'.eps", as(eps) replace;
        #delimit cr
    }
    macro shift
}

collapse MMR MMR_b_DHS100 dsr1_15_25 le_female ln_LE_ratio, by(country)

lab var dsr1_15_25        "DSR"
lab var ln_LE_ratio       "Female LE Advantage"
lab var le_female         "Female LE"
lab var MMR_b_DHS100      "MMR (DHS)"
lab var MMR               "MMR (WDI)" 


#delimit ;
twoway (lfitci le_female dsr1_15_25) (scatter le_female dsr1_15_25),
subtitle("All years") scheme(s1color);
graph export "$OUT/dsr/plots/lLEdesiredall.eps", as(eps) replace;

twoway (lfitci ln_LE_ratio dsr1_15_25) (scatter ln_LE_ratio dsr1_15_25),
subtitle("All years") scheme(s1color) ylabel(-0.05(0.05)0.15)
yscale(range(-0.05 0.05));
graph export "$OUT/dsr/plots/lLErdesiredall.eps", as(eps) replace;

twoway (lfitci MMR_b_DHS100 dsr1_15_25) (scatter MMR_b_DHS100 dsr1_15_25),
subtitle("All years") scheme(s1color);
graph export "$OUT/dsr/plots/lMMRbDHSdesiredall.eps", as(eps) replace;

twoway (lfitci MMR dsr1_15_25) (scatter MMR dsr1_15_25),
subtitle("All years") scheme(s1color);
graph export "$OUT/dsr/plots/lMMRWDIdesiredall.eps", as(eps) replace;
#delimit cr
restore
