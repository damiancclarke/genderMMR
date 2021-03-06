/* panelEstimates.do v0.00        SB/DC/JG/AV              yyyy-mm-dd:2015-05-18
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

File to run country*year estimates of the effect of gender inequality on female-
specific health outcomes and stocks.

The file is completely controlled by the globals and locals in section (1).

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
tab year, gen(_Y)

gen DSR_5     = dsr1_15_25_5
gen DSR       = dsr1_15_25
gen DSR_gdp   = DSR*lgdp
gen DSR_gdp_5 = DSR_5*lgdp_5
gen abortion  = withoutrestrictions
gen pReform  =  progressive == year if abortion != .
gen nReform  =  regressive == year if abortion != .

gen abortionLeg     =  1 if progressive == year
replace abortionLeg = -1 if regressive==year
replace abortionLeg =  0 if abortion != . & abortionLeg == .
replace imr_ratio_WDI = imr_ratio_WDI*100000

lab var pReform     "Progressive"
lab var nReform     "Regressive"
lab var DSR         "Desired Sex Ratio"
lab var DSR_5       "Desired Sex Ratio"
lab var DSR_gdp     "Desired Sex Ratio$\times$ ln(GDP)"
lab var DSR_gdp_5   "Desired Sex Ratio$\times$ ln(GDP)"
lab var lgdp_5      "ln(GDP)"
lab var lgdp        "ln(GDP)"
lab var MMR         "MMR \ \"
lab var tb          "TB"
lab var ln_LE_ratio "LE ratio"
lab var abortion    "Unrestricted"
lab var abortionLeg "Legislation"
lab var imr_ratio_W "IMR (F/M) \ \ \ \ "

*-------------------------------------------------------------------------------
*--- (3) Excess mortality analysis
*-------------------------------------------------------------------------------
gen incLevel = "low" if wb_income_group2 == "Low income"
replace incLevel = "mid" if wb_income_group2 == "Middle Income"
replace incLevel = "high" if wb_income_group2 == "High Income"
	
foreach inc in "low" "mid" "high" {
    local cond if incLevel == "`inc'"
    foreach a in 014 1549 50 {
        eststo: areg lnratio_FM`a' MMR lgdp_5 i.year `cond', `ab' `se'
        eststo: areg lnratio_FM`a' MMR lgdp_5 imr_ratio_W i.year `cond', `ab' `se'
    }

    #delimit ;
    esttab est1 est2 est3 est4 est5 est6 using "$OUT/mortality/Mortality`inc'.tex",
    replace cells(b(star fmt(%-9.2f)) se(fmt(%-9.2f) par([ ]) )) collabels(none)
    label stats (r2 N, fmt(%9.2f %9.0g) label(R-squared Observations)) booktabs
    starlevel ("*" 0.10 "**" 0.05 "***" 0.01) style(tex)
    mlabels("Mort F/M" "Mort F/M" "Mort F/M" "Mort F/M" "Mort F/M" "Mort F/M")
    title("Contributions of IMR and MMR to Excess Female Mortality")
    keep(MMR lgdp_5 imr_ratio_WDI)
    mgroups("0-14 Year-olds" "15-49 Year-olds" "50+ Year-olds", pattern(1 0 1 0 1 0)
        prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(r){@span}));
    #delimit cr
    estimates clear
}

*-------------------------------------------------------------------------------
*--- (3) Regressions of MMR, LE advantage on DSR 
*-------------------------------------------------------------------------------
foreach y of varlist tb ln_LE_ratio MMR abortion abortionLeg {
    if `"`y'"'=="MMR" local 5 _5

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
    if `"`y'"'=="ln_LE_ratio" local f 3
    if `"`y'"'=="tb" local Yn TB
    if `"`y'"'=="ln_LE_ratio" {
        local Yn Life expectancy ratio
        local t1 1961
        local mes 14.8
    }
    if `"`y'"'=="abortion"    local Yn Abortion
    if `"`y'"'=="abortion"    local f 3
    if `"`y'"'=="abortion"    local mes 17.8
    if `"`y'"'=="abortionLeg" local Yn "Abortion legislation changed"
    if `"`y'"'=="abortionLeg" local f 3
    if `"`y'"'=="abortionLeg" local mes 17.8


    #delimit ;
    esttab est1 est2 est3 est4 est5 using "$OUT/dsr/`y'-DSR.tex",
    replace `estopt' title("`Yn' and Desired Sex Ratio (boys/girls)")
    cells(b(star fmt(%-9.`f'f)) se(fmt(%-9.`f'f) par([ ]) )) 
    keep(_cons DSR* lgdp* DSR_gdp*) style(tex) booktabs mlabels(, depvar)
    postfoot(" Country FE &&Y&Y&Y&Y\\ Year FE&&Y&Y&Y&Y\\ "
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
             "\label{TAB:`y'DSR}"
             "\end{footnotesize}}\end{tabular}\end{table}");
    #delimit cr
    estimates clear
}

*-------------------------------------------------------------------------------
*--- (4a) Regressions of MMR on rights
*-------------------------------------------------------------------------------
gen democ_gdp = democ_5*lgdp_5
local ct 

lab var wopol_5 "Political Rights"
lab var wecon_5 "Economic Rights"
lab var wosoc_5 "Social Rights"

foreach rt of varlist wopol_5 wecon_5 wosoc_5 {
    cap gen `rt'GDP = `rt'*lgdp_5
    qui areg MMR `rt' lgdp_5 democ_5 `rt'GDP democ_gdp  _Y*, `ab' `se'
    local cn if e(sample)
    
    eststo: areg MMR `rt' lgdp_5                                `cn',`ab'`se'
    eststo: areg MMR `rt' lgdp_5                            _Y* `cn',`ab'`se'
    eststo: areg MMR `rt' lgdp_5 democ_5                    _Y* `cn',`ab'`se'
    eststo: areg MMR `rt' lgdp_5         `rt'GDP            _Y* `cn',`ab'`se'
    eststo: areg MMR `rt' lgdp_5 democ_5 `rt'GDP            _Y* `cn',`ab'`se'
    eststo: areg MMR `rt' lgdp_5 democ_5 `rt'GDP democ_gdp  _Y* `cn',`ab'`se'
    
    #delimit ;
    esttab est1 est2 est3 est4 est5 est6 using "$OUT/rights/MMR-`rt'.tex",
    replace `estopt' title("MMR and Women's Rights")
    cells(b(star fmt(%-9.2f)) se(fmt(%-9.2f) par([ ]) )) 
    keep(`rt') style(tex) booktabs mlabels(, depvar)
    postfoot("\midrule Year FE&&Y&Y&Y&Y&Y\\ Democracy controls &&&Y&&Y&Y\\ "
             "Rights$\times$ GDP &&&&Y&Y&Y\\ Democracy$\times$ GDP &&&&&&Y\\"
             "\bottomrule\end{tabular}\end{table}");
    #delimit cr
    estimates clear
}

*-------------------------------------------------------------------------------
*--- (4b) Regressions of LE, TB, abortion on rights
*-------------------------------------------------------------------------------
drop democ_gdp
gen democ_gdp = democ*lgdp
local ct 

replace ln_LE_ratio =ln_LE_ratio*100000
lab var wopol "Political Rights"
lab var wecon "Economic Rights"
lab var wosoc "Social Rights"

foreach y of varlist tb ln_LE_ratio abortion abortionLeg {
    local Yn "Life Expectancy Ratio"
    if `"`y'"'=="tb" local Yn "TB"
    if `"`y'"'=="abortion" local Yn "Abortion"
    if `"`y'"'=="abortionLeg" local Yn "Abortion legislation changed"

    
    foreach rt of varlist wopol wecon wosoc {
        cap drop `rt'GDP
        gen `rt'GDP = `rt'*lgdp
        qui areg `y' `rt' lgdp democ `rt'GDP democ  _Y*, `ab' `se'
        local cn if e(sample)
    
        eststo: areg `y' `rt' lgdp                              `cn',`ab'`se'
        eststo: areg `y' `rt' lgdp                          _Y* `cn',`ab'`se'
        eststo: areg `y' `rt' lgdp democ                    _Y* `cn',`ab'`se'
        eststo: areg `y' `rt' lgdp       `rt'GDP            _Y* `cn',`ab'`se'
        eststo: areg `y' `rt' lgdp democ `rt'GDP            _Y* `cn',`ab'`se'
        eststo: areg `y' `rt' lgdp democ `rt'GDP democ_gdp  _Y* `cn',`ab'`se'
    
        #delimit ;
        esttab est1 est2 est3 est4 est5 est6 using "$OUT/rights/`y'-`rt'.tex",
        replace `estopt' title("`Yn' and Women's Rights")
        cells(b(star fmt(%-9.2f)) se(fmt(%-9.2f) par([ ]) )) 
        keep(`rt') style(tex) booktabs mlabels(, depvar)
        postfoot("\midrule Year FE&&Y&Y&Y&Y&Y\\ Democracy controls &&&Y&&Y&Y\\ "
                 "Rights$\times$ GDP &&&&Y&Y&Y\\ Democracy$\times$ GDP &&&&&&Y\\"
                 "\bottomrule\end{tabular}\end{table}");
        #delimit cr
        estimates clear
    }
}

*-------------------------------------------------------------------------------
*--- (5a) Regressions of MMR on parliament
*-------------------------------------------------------------------------------
gen democ_gdp5  = democ_5*lgdp_5
gen womparlGDP5 = womparl_5*lgdp_5
gen womparlGDP  = womparl*lgdp

lab var womparl_5   "Female Representation"
lab var womparl     "Female Representation"
lab var democ_gdp5  "Democracy $\times$ ln(GDP)"
lab var democ_gdp   "Democracy $\times$ ln(GDP)"
lab var womparlGDP  "Female Rep $\times$ ln(GDP)"
lab var womparlGDP5 "Female Rep $\times$ ln(GDP)"
lab var democ_5     "Democracy Indicator"
lab var democ       "Democracy Indicator"

qui areg MMR womparl_5 lgdp_5 democ_5 womparlGDP5 democ_gdp  _Y*, `ab' `se'
local cn if e(sample)
local opts `cn', `ab' `se'

eststo: areg MMR womparl_5 lgdp_5                                     `opts'
eststo: areg MMR womparl_5 lgdp_5                                 _Y* `opts'
eststo: areg MMR womparl_5 lgdp_5 democ_5                         _Y* `opts'
eststo: areg MMR womparl_5 lgdp_5         womparlGDP5             _Y* `opts'
eststo: areg MMR womparl_5 lgdp_5 democ_5 womparlGDP5             _Y* `opts'
eststo: areg MMR womparl_5 lgdp_5 democ_5 womparlGDP5 democ_gdp5  _Y* `opts'
    
#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/rights/MMR-WP.tex",
replace `estopt' title("MMR and Women's Representation in Parliament")
cells(b(star fmt(%-9.2f)) se(fmt(%-9.2f) par([ ]) )) mlabels(, depvar)
keep(womparl_5 lgdp_5 democ_5 womparlGDP5 democ_gdp5) style(tex) booktabs 
postfoot("\bottomrule\multicolumn{7}{p{14.6cm}}{\begin{footnotesize} "
         "\textsc{Notes:} Country and year FE always included. MMR and "
         "political representation data comes from the WDI. The estimation"
         "sample consist of 40 countries for whom the parliamentary "
         "representation variable is available between 1997 and 2011."
         "Standard errors are clustered by "
         "country.\end{footnotesize}}\end{tabular}\end{table}");
#delimit cr
estimates clear

*-------------------------------------------------------------------------------
*--- (5b) Regressions of LE, TB, abortion on participation
*-------------------------------------------------------------------------------
foreach y of varlist tb ln_LE_ratio abortion abortionLeg {
    local Yn "Life Expectancy Ratio"
    local f 3
    if `"`y'"'=="ln_LE_ratio" local f 2
    if `"`y'"'=="tb" local Yn "TB"
    if `"`y'"'=="abortion" local Yn "Abortion"
    if `"`y'"'=="abortionLeg" local Yn "Abortion legislation changed"

    qui areg `y' womparl lgdp democ womparlGDP democ_gdp  _Y*, `ab' `se'
    local cn if e(sample)
    
    eststo: areg `y' womparl lgdp                                 `cn',`ab'`se'
    eststo: areg `y' womparl lgdp                             _Y* `cn',`ab'`se'
    eststo: areg `y' womparl lgdp democ                       _Y* `cn',`ab'`se'
    eststo: areg `y' womparl lgdp       womparlGDP            _Y* `cn',`ab'`se'
    eststo: areg `y' womparl lgdp democ womparlGDP            _Y* `cn',`ab'`se'
    eststo: areg `y' womparl lgdp democ womparlGDP democ_gdp  _Y* `cn',`ab'`se'
    
    #delimit ;
    esttab est1 est2 est3 est4 est5 est6 using "$OUT/rights/`y'-WP.tex",
    replace `estopt' title("`Yn' and Women's Representation in Parliament")
    cells(b(star fmt(%-9.`f'f)) se(fmt(%-9.`f'f) par([ ]) )) mlabels(, depvar)
    keep(womparl lgdp democ womparlGDP democ_gdp) style(tex) booktabs 
    postfoot("\bottomrule\multicolumn{7}{p{14.6cm}}{\begin{footnotesize} "
             "\textsc{Notes:} Country and year FE are always included. `Yn' "
             "and political representation data comes from the WDI database."
             "Standard errors are clustered by "
             "country.\end{footnotesize}}\end{tabular}\end{table}");
    #delimit cr
    estimates clear
}


*-------------------------------------------------------------------------------
*--- (6a) Gender intensity of language with MMR
*-------------------------------------------------------------------------------
encode continent      , gen(cont)
encode wb_income_group, gen(wbig)
local xvars percentage lgdp_5 lpop_5 i.decade i.cont i.wbig pprotest pcatholic /*
*/          pmuslim kgatrstr

gen SBII = sbii
gen NGII = ngii
gen GPII = gpii
gen GAII = gaii
gen GII0 = gii0
gen GII1 = gii1
gen GII2 = gii2
gen GTroiano = gt_pronoun

foreach var of varlist NGII SBII GPII GAII GII0 GII1 GII2 GTroiano {
    cap drop GII GIIxGDP
    gen GII = `var'
    gen GIIxGDP = GII*lgdp_5
    lab var GII     "Gender Intensity Index"
    lab var GIIxGDP "GII $\times$ ln(GDP)"
    
    reg MMR GII `xvars', `se'
    estimates store `var'

    reg MMR GII GIIxGDP `xvars', `se'
    estimates store `var'int
}

#delimit ;
esttab NGII SBII GPII GAII GII0 GII1 GII2 GTroiano
using "$OUT/gii/MMRGII.tex", keep(GII lgdp_5) style(tex) booktabs 
replace `estopt' title("MMR and Gender Intensity of Language Measures")
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) mtitles;

esttab NGIIint SBIIint GPIIint GAIIint GII0int GII1int GII2int GTroianoint
using "$OUT/gii/MMRGII.tex", keep(GII GIIxGDP lgdp_5) style(tex) booktabs 
append `estopt' title("MMR and Gender Intensity of Language Measures")
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) mtitles
postfoot("\bottomrule\multicolumn{9}{p{21.6cm}}{\begin{footnotesize} "
         "\textsc{Notes:} In each case the dependent variable is maternal "
         "deaths per 100,000 live births. The GII measures are defined by "
         "Gay et al (2013) and Givati and Troiano (2012).  Particular measures"
         " of the GII are indicated in column headings and described in "
         "section X.X.  Controls are the log of population, dummies for the "
         "World Bank Income groups classification, the percentage of "
         "population that is Protestant, Catholic and Muslim, the proportion "
         "of the country that is tropical or subtropical, and the percent of "
         "speakers of the majority language. Standard errors are clustered by "
         "country.\end{footnotesize}}\end{tabular}\end{table}");
#delimit cr
estimates clear

*-------------------------------------------------------------------------------
*--- (6b) Gender intensity of language with LE, TB, abortion
*-------------------------------------------------------------------------------
local xvars percentage lgdp lpop i.decade i.cont i.wbig pprotest pcatholic /*
*/          pmuslim kgatrstr

foreach y of varlist ln_LE_ratio tb abortion abortionLeg {

    local mes 20.6
    local Yn "Life Expectancy Ratio"
    local note "the log of the ratio of female to male LE times 100,000"
    local f 3
    if `"`y'"'=="ln_LE_ratio" local f 1
    if `"`y'"'=="tb" {
        local Yn "TB"
        local note "TB infection rates (per 100,000) from the WDI database"
    }
    if `"`y'"'=="abortion" {
        local Yn "Abortion"
        local note "Abortion allowed in unrestricted circumstances"
        local mes 18.8
    }
    if `"`y'"'=="abortionLeg" {
        local Yn "Abortion legislation changed"
        local note "Abortion legislation changed"
    }
    
    foreach var of varlist NGII SBII GPII GAII GII0 GII1 GII2 GTroiano {
        cap drop GII GIIxGDP
        
        gen GII = `var'
        gen GIIxGDP = GII*lgdp
        lab var GII     "Gender Intensity Index"
        lab var GIIxGDP "GII $\times$ ln(GDP)"
    
        reg `y' GII `xvars', `se'
        estimates store `var'

        reg `y' GII GIIxGDP `xvars', `se'
        estimates store `var'int    
    }

    #delimit ;
    esttab NGII SBII GPII GAII GII0 GII1 GII2 GTroiano
    using "$OUT/gii/`y'GII.tex", keep(GII lgdp) style(tex) booktabs 
    replace `estopt' title("`Yn' and Gender Intensity of Language Measures")
    cells(b(star fmt(%-9.`f'f)) se(fmt(%-9.`f'f) par([ ]) )) mtitles;

    esttab NGIIint SBIIint GPIIint GAIIint GII0int GII1int GII2int GTroianoint
    using "$OUT/gii/`y'GII.tex", keep(GII GIIxGDP lgdp) style(tex) booktabs 
    append `estopt' title("`Yn' and Gender Intensity of Language Measures")
    cells(b(star fmt(%-9.`f'f)) se(fmt(%-9.`f'f) par([ ]) )) mtitles
    postfoot("\bottomrule\multicolumn{9}{p{`mes'cm}}{\begin{footnotesize} "
         "\textsc{Notes:} In each case the dependent variable is `note'. "
         " The GII measures are defined by "
         "Gay et al (2013) and Givati and Troiano (2012).  Particular measures"
         " of the GII are indicated in column headings and described in "
         "section X.X.  Controls are the log of population, dummies for the "
         "World Bank Income groups classification, the percentage of "
         "population that is Protestant, Catholic and Muslim, the proportion "
         "of the country that is tropical or subtropical, and the percent of "
         "speakers of the majority language. Standard errors are clustered by "
         "country.\end{footnotesize}}\end{tabular}\end{table}");
    #delimit cr
    estimates clear
}


*-------------------------------------------------------------------------------
*--- (7) MMR on abortion
*-------------------------------------------------------------------------------
lab var withoutrestrictions   "With no restrictions"
lab var tosafehealthofmother  "For mother's health"
lab var abortion_allowed      "In at least some cases"
local se cluster(cncode)
local ab fe

xtset cncode
gen abortionXDSR = withoutrestrictions*DSR
eststo: reg MMR withoutrestrictions                     , `se'
eststo: xtreg MMR withoutrestrictions                   , `se' `ab'
eststo: xtreg MMR withoutrestrictions abortionX      DSR, `se' `ab'
eststo: xtreg MMR withoutrestrictions abortionX lgdp DSR, `se' `ab'
drop abortionXDSR
gen abortionXDSR = tosafehealth*DSR
eststo: reg MMR tosafehealthofmother                     , `se'
eststo: xtreg MMR tosafehealthofmother                   , `se' `ab'
eststo: xtreg MMR tosafehealthofmother abortionX      DSR, `se' `ab'
eststo: xtreg MMR tosafehealthofmother abortionX lgdp DSR, `se' `ab'
drop abortionXDSR
gen abortionXDSR = abortion_allowed*DSR
eststo: reg MMR abortion_allowed                 , `se'
eststo: xtreg MMR abortion_allowed               , `se' `ab'
eststo: xtreg MMR abortion_allowed lgdp abortionX DSR, `se' `ab'

lab var abortionXDSR "Abortion $\times$ DSR"

#delimit ;
esttab est1 est2 est3 est4 est5 est6 est7 est8
using "$OUT/abortionMMR.tex", keep(with* tos* abor* DSR) style(tex)
booktabs replace `estopt' title("MMR and Abortion Legislation")
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) ))
mtitles("MMR" "MMR" "MMR" "MMR" "MMR" "MMR" "MMR" "MMR")
mgroups("Unrestricted Abortion" "Abortion for maternal health",
        pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(})
        span erepeat(\cmidrule(r){@span}))
postfoot("Country FE&&Y&Y&Y&&Y&Y&Y \\ GDP controls &&&&Y&&&&Y \\"
         "\bottomrule\multicolumn{9}{p{18cm}}{\begin{footnotesize} "
         "\textsc{Notes:} Standard errors are clustered by "
         "country.\end{footnotesize}}\end{tabular}\end{table}");
#delimit cr
estimates clear
    
gen postMDG = year>2000
gen abortyear = year if withoutrestrictions == 1
bys country: egen minabortyear = min(abortyear)
gen earlyAbort = minabortyear<2000 if withoutrestrictions != .
gen earlyabortionXpostMDG = earlyAbort*postMDG
lab var postMDG     "post MDG"
lab var earlyAbort  "Early Abortion (no restrictions)"
lab var earlyaborti "Early Abortion $\times$ post MDG"

eststo: reg MMR earlyAbort                          , `se'
eststo: xtreg MMR postMDG earlyabortionXpostMDG     , `se' `ab'
eststo: xtreg MMR postMDG earlyabortionXpostMDG lgdp, `se' `ab'
drop earlyAbort earlyabortionX* abortyear minabortyear

gen abortyear = year if tosafehealth == 1
bys country: egen minabortyear = min(abortyear)
gen earlyAbort = minabortyear<2000 if withoutrestrictions != .
gen earlyabortionXpostMDG = earlyAbort*postMDG
lab var postMDG     "post MDG"
lab var earlyAbort  "Early Abortion (to save health)"
lab var earlyaborti "Early Abortion $\times$ post MDG"
eststo: reg MMR earlyAbort                                     , `se'
eststo: xtreg MMR postMDG earlyabortionXpostMDG     , `se' `ab'
eststo: xtreg MMR postMDG earlyabortionXpostMDG lgdp, `se' `ab'

#delimit ;
esttab est1 est2 est3 est4 est5 est6
using "$OUT/abortionMMR_MDGs.tex", keep(early* postMDG) style(tex)
booktabs replace `estopt' title("MMR, MDGs and Abortion Legislation")
cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) ))
mtitles("MMR" "MMR" "MMR" "MMR" "MMR" "MMR" "MMR" "MMR")
mgroups("Unrestricted Abortion" "Abortion for maternal health",
        pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(})
        span erepeat(\cmidrule(r){@span}))
postfoot("Country FE&&Y&Y&&Y&Y \\ GDP controls &&&Y&&&Y \\"
         "\bottomrule\multicolumn{7}{p{18cm}}{\begin{footnotesize} "
         "\textsc{Notes:} Standard errors are clustered by "
         "country.\end{footnotesize}}\end{tabular}\end{table}");
#delimit cr
estimates clear


exit
*-------------------------------------------------------------------------------
*--- (8) Abortion probits
*-------------------------------------------------------------------------------
cap drop democ_gdp
gen democ_gdp = democ*lgdp

foreach y of varlist abortion {
    if `"`y'"'== "abortion" local Yn "Unrestricted abortion"
    if `"`y'"'== "pReform"  local Yn "Progressive abortion reform"
    if `"`y'"'== "nReform"  local Yn "Regressive abortion reform"

    ***WOMEN'S RIGHTS
    foreach rt of varlist wopol wecon wosoc {
        cap drop `rt'GDP
        gen `rt'GDP = `rt'*lgdp
        qui areg `y' `rt' lgdp democ `rt'GDP democ  _Y*, `ab' `se'
        local cn if e(sample)
    
        probit `y' `rt' lgdp                          i.cncode `cn', `se'
        estpost margins, dydx(`rt' lgdp)
        estimates store p2

        probit `y' `rt' lgdp democ                    i.cncode `cn', `se'
        estpost margins, dydx(`rt' lgdp democ)
        estimates store p3

        probit `y' `rt' lgdp       `rt'GDP            i.cncode `cn', `se'
        estpost margins, dydx(`rt' lgdp `rt'GDP)
        estimates store p4
        
        probit `y' `rt' lgdp democ `rt'GDP            i.cncode `cn', `se'
        estpost margins, dydx(`rt' lgdp democ `rt'GDP)
        estimates store p5

        probit `y' `rt' lgdp democ `rt'GDP democ_gdp  i.cncode `cn', `se'
        estpost margins, dydx(`rt' lgdp democ `rt'GDP democ_gdp)
        estimates store p6
    
        #delimit ;
        esttab p2 p3 p4 p5 p6 using "$OUT/rights/Probit`y'-`rt'.tex",
        replace `estopt' title("`Yn' and Women's Rights")
        cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) 
        keep(`rt') style(tex) booktabs mlabels(, depvar)
        postfoot("\midrule Year FE&&Y&Y&Y&Y\\ Democracy controls &&Y&&Y&Y\\ "
                 "Rights$\times$ GDP &&&&Y&Y\\ Democracy$\times$ GDP &&&&&Y\\"
                 "\bottomrule\end{tabular}\end{table}");
        #delimit cr
        estimates clear
    }
}
exit
    ***POLITICAL PARTICIPATION
    qui areg `y' womparl lgdp democ womparlGDP democ_gdp  _Y*, `ab' `se'
    local cn if e(sample)
    
    eststo: areg `y' womparl lgdp                                 `cn',`ab'`se'
    eststo: areg `y' womparl lgdp                             _Y* `cn',`ab'`se'
    eststo: areg `y' womparl lgdp democ                       _Y* `cn',`ab'`se'
    eststo: areg `y' womparl lgdp       womparlGDP            _Y* `cn',`ab'`se'
    eststo: areg `y' womparl lgdp democ womparlGDP            _Y* `cn',`ab'`se'
    eststo: areg `y' womparl lgdp democ womparlGDP democ_gdp  _Y* `cn',`ab'`se'
    
    #delimit ;
    esttab est1 est2 est3 est4 est5 est6 using "$OUT/rights/`y'-WP.tex",
    replace `estopt' title("`Yn' and Women's Representation in Parliament")
    cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) mlabels(, depvar)
    keep(womparl lgdp democ womparlGDP democ_gdp) style(tex) booktabs 
    postfoot("\bottomrule\multicolumn{7}{p{14.6cm}}{\begin{footnotesize} "
             "\textsc{Notes:} Country and year FE are always included. `Yn' "
             "and political representation data comes from the WDI database."
             "Standard errors are clustered by "
             "country.\end{footnotesize}}\end{tabular}\end{table}");
    #delimit cr
    estimates clear

    ***GENDER INTENSITY
    foreach var of varlist NGII SBII GPII GAII GII0 GII1 GII2 GTroiano {
        cap drop GII GIIxGDP
        
        gen GII = `var'
        gen GIIxGDP = GII*lgdp
        lab var GII     "Gender Intensity Index"
        lab var GIIxGDP "GII $\times$ ln(GDP)"
    
        reg `y' GII `xvars', `se'
        estimates store `var'

        reg `y' GII GIIxGDP `xvars', `se'
        estimates store `var'int    
    }

    #delimit ;
    esttab NGII SBII GPII GAII GII0 GII1 GII2 GTroiano
    using "$OUT/gii/`y'GII.tex", keep(GII lgdp) style(tex) booktabs 
    replace `estopt' title("`Yn' and Gender Intensity of Language Measures")
    cells(b(star fmt(%-9.3f)) se(fmt(%-9.3f) par([ ]) )) mtitles;

    esttab NGIIint SBIIint GPIIint GAIIint GII0int GII1int GII2int GTroianoint
    using "$OUT/gii/`y'GII.tex", keep(GII GIIxGDP lgdp) style(tex) booktabs 
    append `estopt' title("`Yn' and Gender Intensity of Language Measures")
    cells(b(star fmt(%-9.3f)) se(fmt(%-9.`f'f) par([ ]) )) mtitles
    postfoot("\bottomrule\multicolumn{9}{p{16cm}}{\begin{footnotesize} "
         "\textsc{Notes:} In each case the dependent variable is `Yn'. "
         " The GII measures are defined by "
         "Gay et al (2013) and Givati and Troiano (2012).  Particular measures"
         " of the GII are indicated in column headings and described in "
         "section X.X.  Controls are the log of population, dummies for the "
         "World Bank Income groups classification, the percentage of "
         "population that is Protestant, Catholic and Muslim, the proportion "
         "of the country that is tropical or subtropical, and the percent of "
         "speakers of the majority language. Standard errors are clustered by "
         "country.\end{footnotesize}}\end{tabular}\end{table}");
    #delimit cr
    estimates clear

}
