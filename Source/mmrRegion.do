/* mmrRegion.do  v0.00           damiancclarke             yyyy-mm-dd:2015-06-25
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

Take all DHS files from 1985-2013 and, if maternal mortality module was run, re-
shape to one line per sister with survival status, maternal mortality status, a-
nd if relevant, date of death.  From this and fertility, caclulate the maternal 
mortality ratio by area (country, region).

   contact: damian.clarke@economics.ox.ac.uk

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

local Mcreate 1
local Fcreate 1
local yearGen 1

local group _cou region v101

********************************************************************************
*** (2) Reshape to form MMR Base (one line per sibling, living or dead)
********************************************************************************
if `Mcreate' == 1 {
    foreach file in 7 {
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
    foreach file in 7 {
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
    save "$OUT/mmrRegionsYear", replace

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
    collapse MMR MMrate birth, by(_cou region v101 years)
    
    lab dat "Unaltered MMR and MMrate by country, region and 5 year period"
    save "$OUT/mmrRegions", replace
}



********************************************************************************
*** (X) Close file
********************************************************************************
cap log close
