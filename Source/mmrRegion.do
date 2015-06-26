/* mmrRegion.do  v0.00           damiancclarke             yyyy-mm-dd:2015-06-25
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

Take all DHS files from 1985-2013 and, if maternal mortality module was run, re-
shape to one line per sister with survival status, maternal mortality status, a-
nd if relevant, date of death.  From this and fertility, caclulate the maternal 
mortality ratio by area (country, region).

   contact: damian.clarke@economics.ox.ac.uk

** Need to recreate the IR files with a region variable which is directly conve
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
local yearGen 1
local regionL 0
local country 1

local group _cou region v101
local file Regions

if `country'==1 {
    local group _cou
    local file Country
}

********************************************************************************
*** (2) Reshape to form MMR Base (one line per sibling, living or dead)
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
    tempfile MMyears

    save `MMyears'
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

    collapse birth, by(`group')    
 
    merge 1:m `group' using `MMyears'
    gen MMR = (MMrate / birth) * 100000
    replace MMrate = MMrate * 1000
    
    lab var MMR    "Maternal mortality ratio (deaths per 100,000 live births)"
    lab var birth  "Births per woman per year"
    lab var MMrate "Maternal mortality rate (deaths per 1,000 women)"
    
    lab dat "Unaltered MMR and MMrate by country, region and year"
    save "$OUT/mmr`file'Year", replace

    keep if MMR!=.
    gen nonZeroMMR=year if MMR!=0
    bys _cou region: egen maxnonZeroMMR=max(nonZeroMMR)
    bys _cou region: egen minnonZeroMMR=min(nonZeroMMR)

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
    collapse MMR MMrate birth, by(`group' years)
    
    lab dat "Unaltered MMR and MMrate by country, region and 5 year period"
    save "$OUT/mmr`file'", replace
}

********************************************************************************
*** (5) Add region labels
********************************************************************************
if `regionL' == 1 {
    #delimit ;
    local COU Benin Bolivia Brazil Burkina-Faso Burundi Cambodia Cameroon Chad 
              Congo-Brazzaville Congo-Democratic-Republic Cote-d-Ivoire
              Dominican-Republic Ethiopia Gabon Indonesia Madagascar Mali
              Morocco Mozambique Namibia Niger Peru Philippines Rwanda Senegal
              South-Africa Tanzania Uganda Zimbabwe;
    local SUR 2012/BJIR61DT 2008/BOIR51DT 1996/BRIR31DT 2010/BFIR61DT
              2010/BUIR61DT 2010/KHIR61DT 2011/CMIR60DT 2004/TDIR41DT
              2011/CGIR60DT 2007/CDIR50DT 2012/CIIR61DT 2007/DRIR52DT
              2011/ETIR61DT 2012/GAIR60DT 2012/IDIR61DT 2008/MDIR51DT
              2006/MLIR52DT 2003/MAIR43DT 2011/MZIR62DT 2006/NMIR51DT
              2012/NIIR61DT 2000/PEIR41DT 2008/PHIR52DT 2010/RWIR61DT
              2010/SNIR60DT 1998/ZAIR31DT 2012/TZIR6ADT 2011/UGIR60DT
              2010/ZWIR62DT;
    #delimit cr

    tokenize `SUR'
    local j = 1
    foreach country of local COU {
        dis "`country'"
        use "$DAT/`country'/``j''.dta"
        gen n=1
        collapse n, by(v101)
        gen _cou = "`country'"
        decode v101, gen(regionName)
        keep v101 _cou regionName
        tempfile f`j'
        save `f`j''
        
        local ++j
    }
    dis `j'
    clear
    #delimit ;
    append using `f1' `f2' `f3' `f4' `f5' `f6' `f7' `f8' `f9' `f10' `f11' `f12'
                 `f13' `f14' `f15' `f16' `f17' `f18' `f19' `f20' `f21' `f22'
                 `f23' `f24' `f25' `f26' `f27' `f28' `f29';
    #delimit cr
    merge 1:m _cou v101 using "$OUT/mmrRegions"
    drop if _merge == 1
    drop region _merge

    save "$OUT/mmrRegions", replace
}


********************************************************************************
*** (X) Close file
********************************************************************************
cap log close
