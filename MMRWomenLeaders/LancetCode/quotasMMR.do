/* quotasMMR.do v1.00            damiancclarke             yyyy-mm-dd:2016-07-28
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

Examine effects of quotas on female representation, and on maternal mortality.

*/

vers 11
clear all
set more off
cap log close

*-------------------------------------------------------------------------------
*--- (1) globals, locals, external libraries 
*-------------------------------------------------------------------------------
cap which estout
if _rc!=0 ssc install estout

global TABS "tables"
global FIGS "figures"

#delimit ;
local epts replace cells(b(star fmt(%-9.3f)) ci(fmt(%-9.2f)  par("(" "," ")"))
p(fmt(%-9.3f) par([p= ]) )) plain stats(r2 N hY hC, fmt(%9.3f %9.0g)
label(R-Squared Observations "Baseline Controls" "Full Controls"))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label
mgroups("Women in Parliament" "ln(Maternal Mortality Ratio)", pattern(1 0 1 0))
mtitles((1) (2) (3) (4));
#delimit cr

cap mkdir $TABS
cap mkdir $FIGS
*-------------------------------------------------------------------------------
*--- (2) Open log and data 
*-------------------------------------------------------------------------------
log using "quotasMMR.txt", replace text
use "quotas-womparl-MMR.dta", clear


*-------------------------------------------------------------------------------
*--- (3) Generate Summary statistics
*-------------------------------------------------------------------------------
#delimit ;
estpost sum womparl lnMMRt1 quotaSize lnGDP democ yrs_scho healthExp lnmmortt1
if resCountry==1&democ!=.&lnGDP!=.;

estout using "$TABS/SumQuota.csv", replace label cells("count(label(N))
mean(fmt(2) label(Mean)) sd(fmt(2) label(Std.\ Dev.)) min(fmt(2) label(Min))
max(fmt(2) label(Max))");

estpost sum womparl lnMMRt1 quotaSize lnGDP democ yrs_scho healthExp lnmmortt1
if resCountry==0&democ!=.&lnGDP!=.;

estout using "$TABS/SumNoQuota.csv", replace label cells("count(label(N))
mean(fmt(2) label(Mean)) sd(fmt(2) label(Std.\ Dev.)) min(fmt(2) label(Min))
max(fmt(2) label(Max))");
#delimit cr
    
*-------------------------------------------------------------------------------
*--- (4a) Main Regressions (women in parliament and MMR on quotas)
*-------------------------------------------------------------------------------
local cntrl1  lnGDP i.democ 
local cntrl2  lnGDP i.democ healthExp yrs_sch

eststo: xtreg lnMMRt1 quotaRes `cntrl1' i.year, fe cluster(ccode)
gen s1=e(sample)
estadd local hY "Y"
estadd local hC " "

eststo: xtreg lnMMRt1 quotaRes `cntrl2' i.year if s1==1, fe cluster(ccode)
gen s2=e(sample)
estadd local hY "Y"
estadd local hC "Y"

eststo: xtreg womparl quotaRes `cntrl1' i.year if s1==1, fe cluster(ccode)
estadd local hY "Y"
estadd local hC " "

eststo: xtreg womparl quotaRes `cntrl2' i.year if s2==1, fe cluster(ccode)
estadd local hY "Y"
estadd local hC "Y"

sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")
#delimit ;
esttab est3 est4 est1 est2 using "$TABS/table2.csv", `epts' keep(quotaRes _cons)
title("Difference-in-differences estimates of effect of reserved seats")
postfoot("Baseline controls include country and year fixed effects, the log of
per capita GDP and categorical controls for the strength of intitutionalised
democracy.  Full controls include baseline controls, plus average education level
among women and health expenditures as a proportion of GDP.
* p<0.05; ** p<0.01; *** p<0.001.");

esttab est3 est4 est1 est2 using "$TABS/table2-Full.csv", `epts'
title("Difference-in-differences estimates of effect of reserved seats")
postfoot("Baseline controls include country and year fixed effects, the log of
per capita GDP and categorical controls for the strength of intitutionalised
democracy.  Full controls include baseline controls, plus average education level
among women and health expenditures as a proportion of GDP.
* p<0.05; ** p<0.01; *** p<0.001.");
#delimit cr
estimates clear
drop s1 s2

*-------------------------------------------------------------------------------
*--- (4b) Replicate with DHS sub-sample
*-------------------------------------------------------------------------------
lab var lnMMRt1DHS "ln(MMR)"
eststo: xtreg lnMMRt1DHS quotaRes `cntrl1' i.year, fe cluster(ccode)
gen s1=e(sample)
estadd local hY "Y"
estadd local hC " "

eststo: xtreg lnMMRt1DHS quotaRes `cntrl2' i.year if s1==1, fe cluster(ccode)
gen s2=e(sample)
estadd local hY "Y"
estadd local hC "Y"

eststo: xtreg womparl quotaRes `cntrl1' i.year if s1==1, fe cluster(ccode)
estadd local hY "Y"
estadd local hC " "

eststo: xtreg womparl quotaRes `cntrl2' i.year if s2==1, fe cluster(ccode)
estadd local hY "Y"
estadd local hC "Y"

sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")
#delimit ;
esttab est3 est4 est1 est2 using "$TABS/table2-DHS.csv", `epts' 
title("Difference-in-differences estimates of effect of reserved seats")
postfoot("Baseline controls include country and year fixed effects, the log of
per capita GDP and categorical controls for the strength of intitutionalised
democracy.  Full controls include baseline controls, plus average education level
among women and health expenditures as a proportion of GDP.
* p<0.05; ** p<0.01; *** p<0.001.") keep(quotaRes _cons);
#delimit cr
estimates clear


eststo: xtreg lnMMRt1 quotaRes `cntrl1' i.year if s1==1, fe cluster(ccode)
estadd local hY "Y"
estadd local hC " "
eststo: xtreg lnMMRt1 quotaRes `cntrl2' i.year if s2==1, fe cluster(ccode)
estadd local hY "Y"
estadd local hC "Y"

eststo: xtreg womparl quotaRes `cntrl1' i.year if s1==1, fe cluster(ccode)
estadd local hY "Y"
estadd local hC " "

eststo: xtreg womparl quotaRes `cntrl2' i.year if s2==1, fe cluster(ccode)
estadd local hY "Y"
estadd local hC "Y"

sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")
#delimit ;
esttab est3 est4 est1 est2 using "$TABS/table2-DHSWDI.csv", `epts'
title("Difference-in-differences estimates of effect of reserved seats")
postfoot("Baseline controls include country and year fixed effects, the log of
per capita GDP and categorical controls for the strength of intitutionalised
democracy.  Full controls include baseline controls, plus average education
level among women and health expenditures as a proportion of GDP.
* p<0.05; ** p<0.01; *** p<0.001.") keep(quotaRes _cons);
#delimit cr
estimates clear
drop s1 s2


*-------------------------------------------------------------------------------
*--- (4c) Population weighting
*-------------------------------------------------------------------------------
preserve
drop if country=="China"
local wt [aw=pop]
eststo: xtreg lnMMRt1 quotaRes `cntrl1' i.year `wt', fe cluster(ccode)
gen s1=e(sample)
estadd local hY "Y"
estadd local hC " "
eststo: xtreg lnMMRt1 quotaRes `cntrl2' i.year if s1==1 `wt', fe cluster(ccode)
gen s2=e(sample)
estadd local hY "Y"
estadd local hC "Y"

eststo: xtreg womparl quotaRes `cntrl1' i.year if s1==1 `wt', fe cluster(ccode)
estadd local hY "Y"
estadd local hC " "

eststo: xtreg womparl quotaRes `cntrl2' i.year if s2==1 `wt', fe cluster(ccode)
estadd local hY "Y"
estadd local hC "Y"

sum quotaRes if e(sample)==1
local qr = string(r(mean), "%5.3f")
#delimit ;
esttab est3 est4 est1 est2 using "$TABS/table2-weights.csv", `epts'
title("Difference-in-differences estimates of effect of reserved seats")
postfoot("Baseline controls include country and year fixed effects, the log of
per capita GDP and categorical controls for the strength of intitutionalised
democracy.  Full controls include baseline controls, plus average education
level among women and health expenditures as a proportion of GDP.
* p<0.05; ** p<0.01; *** p<0.001.") keep(quotaRes _cons);
#delimit cr
estimates clear
restore


*-------------------------------------------------------------------------------
*--- (5) Placebo test with mortality
*-------------------------------------------------------------------------------
eststo: xtreg lnmmortt1 quotaRes `cntrl1' i.year, fe cluster(ccode)
gen s1=e(sample)
estadd local hY "Y"
estadd local hC " "

eststo: xtreg lnmmortt1 quotaRes `cntrl2' i.year if s1==1, fe cluster(ccode)
gen s2=e(sample)
estadd local hY "Y"
estadd local hC "Y"

eststo: xtreg womparl quotaRes `cntrl1' i.year if s1==1, fe cluster(ccode)
estadd local hY "Y"
estadd local hC " "

eststo: xtreg womparl quotaRes `cntrl2' i.year if s2==1, fe cluster(ccode)
estadd local hY "Y"
estadd local hC "Y"

#delimit ;
esttab est3 est4 est1 est2 using "$TABS/placebo.csv", `epts' keep(quotaRes _cons)
title("Difference-in-differences estimates of effect of reserved seats");
#delimit cr
estimates clear
drop s1 s2

*-------------------------------------------------------------------------------
*--- (6) Extensive margin regression
*-------------------------------------------------------------------------------
eststo: xtreg lnMMRt1 resValue `cntrl1' i.year, fe cluster(ccode)
gen s1=e(sample)
estadd local hY "Y"
estadd local hC " "

eststo: xtreg lnMMRt1 resValue `cntrl2' i.year if s1==1, fe cluster(ccode)
gen s2=e(sample)
estadd local hY "Y"
estadd local hC "Y"

eststo: xtreg womparl resValue `cntrl1' i.year if s1==1, fe cluster(ccode)
estadd local hY "Y"
estadd local hC " "

eststo: xtreg womparl resValue `cntrl2' i.year if s2==1, fe cluster(ccode)
estadd local hY "Y"
estadd local hC "Y"

sum resValue if e(sample)==1
local qr = string(r(mean), "%5.3f")
#delimit ;
esttab est3 est4 est1 est2 using "$TABS/extensiveMargin.csv", `epts'
title("Extensive Margin Effect of Reserved Seats")
postfoot("Baseline controls include country and year fixed effects, the log of
per capita GDP and categorical controls for the strength of intitutionalised
democracy.  Full controls include baseline controls, plus average education
level among women and health expenditures as a proportion of GDP.
* p<0.05; ** p<0.01; *** p<0.001.") keep(resValue _cons);
#delimit cr
estimates clear
drop s1 s2


*-------------------------------------------------------------------------------
*--- (7) Dynamic effects
*-------------------------------------------------------------------------------
#delimit ;
local epts replace cells(b(star fmt(%-9.3f)) ci(fmt(%-9.2f)  par("(" "," ")"))
                         p(fmt(%-9.3f) par([p= ]) )) plain
stats(r2 N Fs ps hY hC, fmt(%9.3f %9.0g %9.3f %9.3f)
      label(R-Squared Observations "Joint Siginificance of Pre-Trend (F-Test)"
            "Joint Siginificance of Pre-Trend (p-values)" "Baseline Controls"
            "Full Controls")) mtitles((1) (2))
starlevel ("*" 0.10 "**" 0.05 "***" 0.01) collabels(none) label;

local quotarD quotaResn13 quotaResn9_12 quotaResn5_8 quotaResn1_4
              quotaResp1_4 quotaResp5_8 quotaResp9_12 quotaResp13;
#delimit cr

eststo: xtreg lnMMRt1 `quotarD' `cntrl1' i.year, fe cluster(ccode)
local end "% increase in ln(GDP per capita)"
dis "Years 1-4 are equivalent to a"  _b[quotaResp1_4]/_b[lnGDP]*4  "`end'"
dis "Years 5-8 are equivalent to a"  _b[quotaResp5_8]/_b[lnGDP]*4  "`end'"
dis "Years 9-12 are equivalent to a" _b[quotaResp9_12]/_b[lnGDP]*4 "`end'"
dis "Years 13+ are equivalent to a"  _b[quotaResp13]/_b[lnGDP]*4   "`end'"
gen s1 = e(sample)==1
test quotaResn13 quotaResn9_12 quotaResn5_8 quotaResn1_4
estadd scalar Fs r(F)
estadd scalar ps r(p)
estadd local hY "Y"
estadd local hC " "

eststo: xtreg lnMMRt1 `quotarD' `cntrl2' i.year if s1==1, fe cluster(ccode)
test quotaResn13 quotaResn9_12 quotaResn5_8 quotaResn1_4
estadd scalar Fs r(F)
estadd scalar ps r(p)
estadd local hY "Y"
estadd local hC "Y"


#delimit ;
esttab est1 est2 using "$TABS/dynamic.csv", `epts' keep(`quotarD' _cons)
title("Variation in Impact with Duration of Exposure")
postfoot("Baseline controls include country and year fixed effects, the log
of per capita GDP and categorical controls for the strength of intitutionalised
democracy.  Full controls include baseline controls, plus average education
level among women and health expenditures as a proportion of GDP.
* p<0.05; ** p<0.01; *** p<0.001.");
#delimit cr
estimates clear
drop s1


*-------------------------------------------------------------------------------
*--- (8) Extensive plots
*-------------------------------------------------------------------------------
local nquint 4
gen qResQ1= quotaRes==1&quotaSize>0&quotaSize<10
gen qResQ2= quotaRes==1&quotaSize>=10&quotaSize<20
gen qResQ3= quotaRes==1&quotaSize>=20&quotaSize<30
gen qResQ4= quotaRes==1&quotaSize>=30


xtreg lnMMRt1 `cntrl1' i.year qResQ*, fe cluster(ccode)
gen MMRest = .
gen MMRub  = .
gen MMRlb  = .
gen quant  = .
foreach num of numlist 1(1)`nquint' {
    replace quant=`num' in `num'
    replace MMRest = _b[qResQ`num'] in `num'
    replace MMRlb  = _b[qResQ`num']-1.96*_se[qResQ`num'] in `num'
    replace MMRub  = _b[qResQ`num']+1.96*_se[qResQ`num'] in `num'
}

xtreg womparl `cntrl' i.year qResQ*, fe cluster(ccode)
gen WPest  = .
gen WPub   = .
gen WPlb   = .
gen quant2 = .
foreach num of numlist 1(1)`nquint' {
    replace quant2 = `num'.1 in `num'
    replace WPest  = _b[qResQ`num'] in `num'
    replace WPlb   = _b[qResQ`num']-1.96*_se[qResQ`num'] in `num'
    replace WPub   = _b[qResQ`num']+1.96*_se[qResQ`num'] in `num'
}

local m1 mcolor(blue) msymbol(Oh)
local m2 mcolor(red) msymbol(S)
local d {&Delta}

#delimit ;
twoway scatter MMRest quant, yaxis(1) ylabel(0.8(-0.2)-0.8, axis(1)) `m1' ||
       scatter WPest quant2, yaxis(2) ylabel(-40(10)40, axis(2)) `m2' ||
       rcap MMRlb MMRub quant, yaxis(1) lcolor(blue) || 
       rcap WPlb WPub  quant2, yaxis(2) lcolor(red) xlabel(1(1)`nquint')  
scheme(s1mono) yline(0, lcolor(black) lpattern(dash))
ytitle("`d' ln(Maternal Mortality)", axis(1))
ytitle("`d' % Women in Parliament", axis(2))
xtitle("Size of Reserved Seat Quota (Percentiles)")
legend(order(1 "`d' ln(Maternal Mortality)" 3 "`d' % Women in Parliament"));
#delimit cr
graph export "$FIGS/doseResponse.eps", as(eps) replace


#delimit ;
twoway scatter MMRest quant, `m1' ||
       rcap MMRlb MMRub quant, lcolor(blue) lwidth(medthick)
scheme(lean1) yline(0, lcolor(black) lpattern(dash))
xlabel(1 "0.1-9%" 2 "10-19%" 3 "20-29%" 4 "30%")
ytitle("`d' ln(Maternal Mortality)")
xtitle("Size of Reserved Seat Quota (Percent)")
legend(order(1 "Estimate" 2 "95% CI"));
#delimit cr
graph export "$FIGS/doseResponse-MMR.eps", as(eps) replace


#delimit ;
twoway scatter WPest quant, `m2' ||
       rcap WPlb WPub quant, lcolor(red) lwidth(medthick)
scheme(lean1) yline(0, lcolor(black) lpattern(dash))
xlabel(1 "0.1-9%" 2 "10-19%" 3 "20-29%" 4 "30%")
ytitle("`d' % Women in Parliament")
xtitle("Size of Reserved Seat Quota (Percent)")
legend(order(1 "Estimate" 2 "95% CI"));
#delimit cr
graph export "$FIGS/doseResponse-WP.eps", as(eps) replace




*-------------------------------------------------------------------------------
*--- (9) Examine individual country plots over time
*-------------------------------------------------------------------------------
cap mkdir "$FIGS/countries"
levelsof country if resCountry==1&country!="South Sudan", local(countries)
local Rgraphs

local i=1
foreach cc of local countries {
    dis "`cc'"
    sum quotayear if country==`"`cc'"'
    local ly = r(mean)
    sum quotaSize if country==`"`cc'"'
    local qp = string(r(mean), "%5.3f")
    
    #delimit ;
    twoway line womparl year if country==`"`cc'"', lcolor(black) lwidth(thick)
    scheme(s1mono) xline(`ly', lcolor(red)) title(`cc') ytitle("% Women in Parlimanet")
    xtitle("Year") note("quotaproject listed quota is `qp'%") name(c`i', replace);
    #delimit cr
    graph export "$FIGS/countries/c`i'.eps", replace
    local Rgraphs `Rgraphs' c`i' 
    local ++i
}
graph combine `Rgraphs', scheme(s1mono)
graph export "$FIGS/countries/ReservedCountries.eps", replace


*-------------------------------------------------------------------------------
*--- (10) Export data for py figure
*-------------------------------------------------------------------------------
exit
collapse quotayear rcode, by(country quotatype)
gen num = 1
collapse (sum) num, by(quotayear rcode)
bys rcode (quotayear): gen qCountries = sum(num)
drop if quotayear==.
tempfile quotas
save `quotas'

clear
set obs 7
gen rcode = _n
expand 24
bys rcode: gen quotayear = _n+1988
merge 1:1 quotayear rcode using `quotas'
drop if rcode==5
replace num = 0 if num==.
drop qCountries
bys rcode (quotayear): gen qCountries = sum(num)
keep rcode quotayear qCountries


outsheet using quotaTimes.csv, comma names replace
restore
