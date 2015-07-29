/* mmrRegion.do  v0.00           damiancclarke             yyyy-mm-dd:2015-06-25
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

Take all DHS files from 1985-2013 and, if maternal mortality module was run, re-
shape to one line per sister with survival status, maternal mortality status, a-
nd if relevant, date of death.  From this and fertility, caclulate the maternal 
mortality ratio by area (country, region).

   contact: damian.clarke@economics.ox.ac.uk

Using the locals "group" and "file" this can be switched to make country by reg-
ion level data or country level data.  For country, switch country to 1, for re-
gion, switch country to 0.


*/

    
vers 11
clear all
set more off
cap log close
set maxvar 15000

********************************************************************************
*** (1) Globals and Locals
********************************************************************************
global DAT "~/database/DHS/DHS_Data"
global LOG "~/investigacion/2013/WorldMMR/Log"
global OUT "~/investigacion/2013/WorldMMR/Data"

log using "$LOG/mmrRegion.txt", text replace

#delimit ;
local MMRv mmidx_ mm1_ mm2_ mm3_ mm4_ mm5_ mm6_ mm7_ mm8_ mm9_ mm10_ mm11_
           mm12_ mm13_ mm14_ mm15_;
local IMRv b1_ b2_ b3_ b4_ b7_;
#delimit cr

local Mcreate 0
local Fcreate 0
local Ecreate 0
local Wcreate 1
local yearGen 0
local regionL 1
local country 0
local violGen 1

local group _cou _year region v101
local file Regions

if `country'==1 {
    local group _cou
    local file Country
}

********************************************************************************
*** (2a) Reshape to form MMR Base (one line per sibling, living or dead)
********************************************************************************
if `Mcreate' == 1 {
    foreach file in 1 2 3 4 5 6 7 {
        dis "Working on mortality file `file'..."
        tempfile MM`file'
	
        use "$DAT/World_IR_p`file'", clear
        count
        gen mid="a"
        egen id=concat(_cou mid _year mid v001 mid v002 mid v150 mid caseid)
        keep _cou _year id caseid v007 v008 v010 v024 mm* v005 v101
        bys id: gen counter=_n
        tab counter
        drop if counter>1
        decode v101, gen(region)
        
        foreach var of local MMRv {
            foreach num of numlist 1(1)9 {
                rename `var'0`num' `var'`num'
            }
        }
        cap drop mmc1-mmc5

        reshape long `MMRv', i(id) j(sister)
        drop if mmidx==.

        save `MM`file''
    }
    
    clear
    append using `MM1' `MM2' `MM3' `MM4' `MM5' `MM6' `MM7'

    bys _cou: gen dcou=_N
    rename mm1_ sibSex
    rename mm3_ sibAge
    rename v007 yearInterview
    
    keep if sibSex==2 & dcou>50 

    gen maternDeath   = (mm9_>1 & mm9_<7) | mm10_==1
    gen sibYOB        = int((mm4_-1)/12)+1900

    replace yearInterview = yearInterview+1900 if yearInterview<114
    replace yearInterview = yearInterview+100  if yearInterview<1980    
    
    replace sibYOB        = sibYOB - 57        if _cou=="Nepal"
    replace sibYOB        = sibYOB + 100       if _cou=="Nepal" & sibYOB < 1900    
    replace yearInterview = yearInterview - 57 if _cou=="Nepal"

    drop dcou counter

    lab var id    "Unique ID per woman from survey (may have various sisters)"
    lab var _cou  "Country Name"
    lab var v024  "Region of country"
    lab var v101  "Region of country (alternate)"
    
    
    save "$OUT/Microbase_MMR_year", replace
}

********************************************************************************
*** (2b) Male Recode
********************************************************************************
if `Ecreate' == 1 {
    foreach file in 1 2 3 4 5 6 7 {
        dis "Working on male recode file `file'..."
        tempfile Male`file'
	
        use "$DAT/World_MR_p`file'", clear
        count
        keep _cou _year mv010 mv005 mv101 mv744*
        replace mv010 = mv010 + 1900 if mv010<100
        replace mv010 = mv010 - 57  if _cou=="Nepal"
        replace mv010 = mv010 + 100 if _cou=="Nepal" & mv010<1900
        
        gen maleViolence_a = mv744a == 1 if mv744a==0|mv744a==1
        gen maleViolence_b = mv744b == 1 if mv744b==0|mv744b==1
        gen maleViolence_c = mv744c == 1 if mv744c==0|mv744c==1
        gen maleViolence_d = mv744d == 1 if mv744d==0|mv744d==1
        gen maleViolence_e = mv744e == 1 if mv744e==0|mv744e==1

        keep _cou _year maleViol*
        save `Male`file''
    }
    clear
    append using `Male1' `Male2' `Male3' `Male4' `Male5' `Male6' `Male7'

            
    save "$OUT/Microbase_Male_year", replace
}

********************************************************************************
*** (2c) female Recode
********************************************************************************
if `Wcreate' == 1 {
    foreach file in 1 2 3 4 5 6 7 {
        dis "Working on womens recode file `file'..."
        tempfile Ff`file'
	
        use "$DAT/World_IR_p`file'", clear
        count
        keep _cou _year v010 v005 v101 v744* v133
        replace v010 = v010 + 1900 if v010<100
        replace v010 = v010 - 57  if _cou=="Nepal"
        replace v010 = v001 + 100 if _cou=="Nepal" & v010<1900
        rename v010 birthYear
        
        gen femaleViolence_a = v744a == 1 if v744a==0|v744a==1
        gen femaleViolence_b = v744b == 1 if v744b==0|v744b==1
        gen femaleViolence_c = v744c == 1 if v744c==0|v744c==1
        gen femaleViolence_d = v744d == 1 if v744d==0|v744d==1
        gen femaleViolence_e = v744e == 1 if v744e==0|v744e==1

        gen femaleEducation = v133 if v133<=25
        keep _cou _year femaleViol* femaleEducation v101 v005 birthYear
        save `Ff`file''
    }
    clear
    append using `Ff1' `Ff2' `Ff3' `Ff4' `Ff5' `Ff6' `Ff7'

    save "$OUT/Microbase_Female_year", replace
}

********************************************************************************
*** (3) Fertility base (one line per birth)
********************************************************************************
if `Fcreate' == 1 {
    foreach file in 1 2 3 4 5 6 7 {
        dis "Working on fertility file `file'..."
        tempfile fert`file'
		
        use "$DAT/World_IR_p`file'", clear
        gen mid="a"
        egen id=concat(_cou mid _year mid v001 mid v002 mid v150 mid caseid)
        keep _cou _year id caseid v007 v008 v010 v201 v005 v024 v101 /*
          */ b1_* b2_* b3_* b4_* b7_* 
        bys id: gen counter=_n
        drop if counter>1
        foreach num of numlist 1(1)9 {
            foreach var of local IMRv {
                rename `var'0`num' `var'`num'
            }
        }
        reshape long `IMRv', i(id) j(birthdate)
        keep if b3_! = .
        gen childYOB     = floor((b3_-1)/12+1900)
        replace childYOB = childYOB - 57  if _cou=="Nepal"
        replace childYOB = childYOB + 100 if _cou=="Nepal" & childYOB<1900
        save `fert`file''
    }

    clear
    append using `fert1' `fert2' `fert3' `fert4' `fert5' `fert6' `fert7'
    cap drop b1_92* b2_92* b4_92* b7_92* b1_x* b2_x* b4_x*
    decode v101, gen(region)
        
    bys _cou: gen dcou=_N
    drop if dcou<50

    rename v010 motherYOB
    replace motherYOB = motherYOB+1900 if motherYOB<114
    replace motherYOB = motherYOB+100  if motherYOB>=1900 & motherYOB<1910
    replace motherYOB = motherYOB-57   if _cou=="Nepal"
    replace motherYOB = motherYOB+100  if _cou=="Nepal" & motherYOB<1900
    replace motherYOB = motherYOB+2000 if motherYOB<13


    rename v007 yearInterview
    replace yearInterview = yearInterview + 1900 if yearInterview < 114
    replace yearInterview = yearInterview + 100  if yearInterview < 1980
    replace yearInterview = yearInterview - 57   if _cou=="Nepal"
    drop dcou
    rename v201 fertility
    drop if v005<0
    

    save "$OUT/Microbase_fertility_year", replace	
}


********************************************************************************
*** (4a) Collapse maternal mortality by year and number of women exposed
********************************************************************************
if `yearGen' == 1 {
    use "$OUT/Microbase_MMR_year", clear

    gen yearDeath     = int((mm8_-1)/12)+1900
    replace yearDeath = yearDeath - 57  if _cou=="Nepal"
    replace yearDeath = yearDeath + 100 if _cou=="Nepal" & yearDeath<1900

    local allfiles
    foreach y of numlist 1970(1)2013 {
        dis "`y'"
        preserve
        gen age=`y' - sibYOB
        keep if age>14&age<50
        gen MMrate = yearDeath==`y' & maternDeath == 1
        keep if yearDeath >= `y'

        bys `group': egen maxage_MMR  = max(age)
        bys `group': egen minage_MMR  = min(age)
        bys `group': egen meanage_MMR = mean(age)	

        collapse MMrate maxage_MMR minage_MMR meanage_MMR [pw=v005], by(`group')
        gen year=`y'
        tempfile MM`y'
        save `MM`y''
        restore
        local allfiles `allfiles' `MM`y''
    }
    clear
    append using `allfiles'

    save "$OUT/MMyears"
}

********************************************************************************
*** (4b) Collapse births by year and number of women exposed
********************************************************************************
if `yearGen' == 1 {
    use "$OUT/Microbase_fertility_year", clear

    gen age      = yearInterview - motherYOB
    gen exposure = age - 15
    gen birth    = fertility/exposure
    keep if age>14&age<50

    collapse birth, by(_cou v101 _year)    
 
    merge 1:m _cou v101 _year using "$OUT/MMyears"
    keep if _merge==3
    drop _merge
    
    gen MMR = (MMrate / birth) * 100000
    replace MMrate = MMrate * 1000
    
    lab var MMR    "Maternal mortality ratio (deaths per 100,000 live births)"
    lab var birth  "Births per woman per year"
    lab var MMrate "Maternal mortality rate (deaths per 1,000 women)"
    
    lab dat "Unaltered MMR and MMrate by country, region and year"
    save "$OUT/mmr`file'Year", replace

    keep if MMR!=.
    gen nonZeroMMR=year if MMR!=0
    bys `group': egen maxnonZeroMMR=max(nonZeroMMR)
    bys `group': egen minnonZeroMMR=min(nonZeroMMR)

    keep if year>=minnonZeroMMR & year<=maxnonZeroMMR
    drop minnonZeroMMR maxnonZeroMMR nonZeroMMR


    gen years = .
    local j = 1
    foreach yy of numlist 1970(5)2010 {
        local yu = `yy'+5
        replace years = `j' if year>=`yy'&year<`yu'
        local ++j
    }
    lab def yrs 1 "1970-1974" 2 "1975-1979" 3 "1980-1984" 4 "1985-1989" 5 /*
    */ "1990-1994" 6 "1995-1999" 7 "2000-2004" 8 "2005-2009" 9 "2010+" 
    lab val years yrs
    lab var years "5 year period"
    collapse MMR MMrate birth, by(_cou _year v101 years)
    
    lab dat "Unaltered MMR and MMrate by country, region and 5 year period"
    save "$OUT/mmr`file'", replace
}

********************************************************************************
*** (5) Add region labels
********************************************************************************
if `regionL' == 1 {
    #delimit ;
    local COU Benin                      1996
              Benin                      2006
              Bolivia                    1994
              Bolivia                    2003
              Bolivia                    2008
              Brazil                     1996
              Burkina-Faso               1999
              Burkina-Faso               2003
              Burkina-Faso               2010
              Burundi                    2010
              Cambodia                   2000
              Cambodia                   2005
              Cambodia                   2010
              Cameroon                   1998
              Cameroon                   2004
              Cameroon                   2011
              Central-African-Republic   1994    
              Chad                       1997
              Chad                       2004
              Congo-Brazzaville          2005
              Congo-Brazzaville          2011
              Congo-Democratic-Republic  2007
              Cote-d-Ivoire              1994
              Cote-d-Ivoire              2005
              Cote-d-Ivoire              2012
              Dominican-Republic         2002
              Dominican-Republic         2007
              Ethiopia                   2000
              Ethiopia                   2005
              Ethiopia                   2011
              Gabon                      2000
              Gabon                      2012
              Guatemala                  1995
              Guinea                     1999
              Guinea                     2005
              Haiti                      2000
              Haiti                      2006 
              Indonesia                  1994
              Indonesia                  1997
              Indonesia                  2003
              Indonesia                  2007
              Indonesia                  2012
              Jordan                     1997
              Kenya                      1998
              Kenya                      2008
              Lesotho                    2004
              Lesotho                    2009
              Liberia                    2007
              Madagascar                 1992
              Madagascar                 1997
              Madagascar                 2004
              Madagascar                 2008
              Malawi                     1992
              Malawi                     2000
              Malawi                     2004
              Malawi                     2010
              Mali                       1996
              Mali                       2001
              Mali                       2006
              Morocco                    1992
              Morocco                    2003
              Mozambique                 1997
              Mozambique                 2003
              Mozambique                 2011
              Namibia                    1992
              Namibia                    2000
              Namibia                    2006
              Nepal                      1996    
              Nepal                      2006    
              Nepal                      1996    
              Niger                      1992
              Niger                      2006
              Nigeria                    1999
              Nigeria                    2008
              Peru                       1992
              Peru                       1996
              Peru                       2000
              Philippines                1993
              Philippines                1998
              Rwanda                     2000
              Rwanda                     2005
              Rwanda                     2010
              Sao-Tome-and-Principe      2008
              Senegal                    1993
              Senegal                    2005
              Senegal                    2010    
              Sierra-Leone               2008
              South-Africa               1998
              Swaziland                  2006
              Tanzania                   1996
              Tanzania                   2004
              Tanzania                   2010
              Togo                       1998             
              Uganda                     1995
              Uganda                     2000
              Uganda                     2006
              Uganda                     2011
              Zambia                     1996
              Zambia                     2002
              Zambia                     2007
              Zimbabwe                   1994
              Zimbabwe                   1999
              Zimbabwe                   2005
              Zimbabwe                   2010;
    local SUR BJIR31DT BJIR51DT BOIR31DT BOIR41DT BOIR51DT BRIR31DT BFIR31DT
     BFIR43DT BFIR61DT BUIR61DT KHIR42DT KHIR51DT KHIR61DT CMIR31DT CMIR44DT
     CMIR60DT CFIR31DT TDIR31DT TDIR41DT CGIR51DT CGIR60DT CDIR50DT CIIR35DT
     CIIR50DT CIIR61DT DRIR4ADT DRIR52DT ETIR41DT ETIR51DT ETIR61DT GAIR41DT
     GAIR60DT GUIR34DT GNIR41DT GNIR52DT HTIR42DT HTIR52DT IDIR31DT IDIR3ADT
     IDIR42DT IDIR51DT IDIR61DT JOIR31DT KEIR3ADT KEIR52DT LSIR41DT LSIR60DT
     LBIR51DT MDIR21DT MDIR31DT MDIR41DT MDIR51DT MWIR22DT MWIR41DT MWIR4DDT
     MWIR61DT MLIR32DT MLIR41DT MLIR52DT MAIR21DT MAIR43DT MZIR31DT MZIR41DT
     MZIR62DT NMIR21DT NMIR41DT NMIR51DT NPIR31DT NPIR51DT NPIR31DT NIIR22DT 
     NIIR51DT NGIR41DT NGIR52DT PEIR21DT PEIR31DT PEIR41DT PHIR31DT PHIR3ADT 
     RWIR41DT RWIR53DT RWIR61DT STIR50DT SNIR21DT SNIR4HDT SNIR60DT SLIR51DT 
     ZAIR31DT SZIR51DT TZIR3ADT TZIR4IDT TZIR62DT TGIR31DT UGIR33DT UGIR41DT 
     UGIR52DT UGIR60DT ZMIR31DT ZMIR42DT ZMIR51DT ZWIR31DT ZWIR42DT ZWIR51DT 
     ZWIR62DT;    
    #delimit cr

    tokenize `COU'
    local j = 1
    foreach survey of local SUR {
        dis "country: `1', year: `2', survey: `survey'"
        use "$DAT/`1'/`2'/`survey'.dta"
        gen n=1
        collapse n, by(v101)
        gen _cou = "`1'"
        decode v101, gen(regionName)
        keep v101 _cou regionName
        gen _year = "`2'"
        tempfile f`j'
        save `f`j''
        
        macro shift
        macro shift
        local ++j
    }
    dis `j'
    clear
    #delimit ;
    append using `f1' `f2' `f3' `f4' `f5' `f6' `f7' `f8' `f9' `f10' `f11' `f12'
                 `f13' `f14' `f15' `f16' `f17' `f18' `f19' `f20' `f21' `f22'
                 `f23' `f24' `f25' `f26' `f27' `f28' `f29' `f30' `f31' `f32'
                 `f33' `f34' `f35' `f36' `f37' `f38' `f39' `f40' `f41' `f42'
                 `f43' `f44' `f45' `f46' `f47' `f48' `f49' `f50' `f51' `f52'
                 `f53' `f54' `f55' `f56' `f57' `f58' `f59' `f60' `f61' `f62'
                 `f63' `f64' `f65' `f66' `f67' `f68' `f69' `f70' `f71' `f72'
                 `f73' `f74' `f75' `f76' `f77' `f78' `f79' `f80' `f81' `f82'
                 `f83' `f84' `f85' `f86' `f87' `f88' `f89' `f90' `f91' `f92'
                 `f93' `f94' `f95' `f96' `f97' `f98' `f99' `f100' `f101' `f102'
                 `f103' `f104';
    #delimit cr
    bys v101 _cou _year: gen n=_n
    drop if n==2
    drop n
    merge 1:m _cou _year v101 using "$OUT/mmrRegions"
    drop if _merge == 1
    drop _merge

    save "$OUT/mmrRegions", replace
}


********************************************************************************
*** (6) Merge covariates
********************************************************************************
if `violGen' == 1 {
    /*
    use "$OUT/Microbase_Male_year"
    collapse maleViolence* [pw=mv005], by(_cou _year mv101 mv010)
    rename mv010 year
    rename mv101 v101
    gen years = .
    local j = 1
    foreach yy of numlist 1940(5)1980 {
        local yu = `yy'+5
        replace years = `j' if year>=`yy'&year<`yu'
        local ++j
    }
    lab def yrs 1 "1970-1974" 2 "1975-1979" 3 "1980-1984" 4 "1985-1989" 5 /*
    */ "1990-1994" 6 "1995-1999" 7 "2000-2004" 8 "2005-2009" 9 "2010+" 
    lab val years yrs
    lab var years "5 year period"
    collapse maleViolence*, by(years _cou _year v101)
    
    tempfile MaleYears
    save `MaleYears'
    */

    use "$OUT/Microbase_Female_year"
    collapse femaleViolence* femaleEduc [pw=v005], by(_cou _year v101 birthYear)
    rename birthYear year
    gen years = .
    local j = 1
    foreach yy of numlist 1940(5)1980 {
        local yu = `yy'+5
        replace years = `j' if year>=`yy'&year<`yu'
        local ++j
    }
    lab def yrs 1 "1970-1974" 2 "1975-1979" 3 "1980-1984" 4 "1985-1989" 5 /*
    */ "1990-1994" 6 "1995-1999" 7 "2000-2004" 8 "2005-2009" 9 "2010+" 
    lab val years yrs
    lab var years "5 year period"
    collapse femaleEduc, by(years _cou _year v101)
    
    merge 1:1 years _cou _year v101 using "$OUT/mmr`file'"
    keep if _merge==3|_merge==2
    gen hasFemaleEduc=_merge==3
    drop _merge

    /*
    merge 1:1 years _cou _year v101 using `MaleYears'
    keep if _merge==3|_merge==1
    gen hasMaleViolence=_merge==3
    drop _merge

    save "$OUT/mmr`file'_DViolence", replace
    */
    save "$OUT/mmrRegions_covars"    
}


********************************************************************************
*** (X) Close file
********************************************************************************
cap log close
