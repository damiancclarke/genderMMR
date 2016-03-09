/* educGenerate.do v0.00         damiancclarke             yyyy-mm-dd:2016-03-09
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

Generates educational data by year and country using a combination of Barro-Lee,

exit

merge 1:m countrycode using "$DAT/GDPpercap"
keep if _merge==3

keep country countrycode iso2code cncode
collapse cncode, by(country* iso*)
rename countrycode WBcode

*/
vers 11
clear all
set more off
cap log close

********************************************************************************
*** (1) globals and locals
********************************************************************************
global DAT "~/investigacion/2013/WorldMMR/Data"


********************************************************************************
*** (2) Full countries
********************************************************************************
use "$DAT/LangGender_dataset"
drop if not_country==1
drop yr_sch
rename isocode WBcode
replace WBcode = "ROM" if WBcode == "MDA"
replace WBcode = "SER" if WBcode == "SRB"

merge 1:1 WBcode year using  "$DAT/BL2013_F2599_v2.1.dta" 
* drop BL data with year<1950 and for Taiwan and Reunion
drop if _merge==2 


********************************************************************************
*** (3) Fill in from FDR education (MICS and DHS).  Orig file saved as HDREduc
********************************************************************************
replace yr_sch = 10.3 if year==2010&country=="Samoa"
replace yr_sch = 11.1 if year==2010&country=="Bahamas, The"
replace yr_sch = 11.4 if year==2010&country=="Belarus"
replace yr_sch = 7.2  if year==2010&country=="Bosnia and Herzegovina"
replace yr_sch = 1.9  if year==2010&country=="Burkina Faso"
replace yr_sch = 0.6  if year==2010&country=="Chad"
replace yr_sch = 1.4  if year==2010&country=="Ethiopia"
replace yr_sch = 11.9 if year==2010&country=="Georgia"
replace yr_sch = 0.8  if year==2010&country=="Guinea"
replace yr_sch = 1.4  if year==2010&country=="Guinea-Bissau"
replace yr_sch = 7.6  if year==2010&country=="Lebanon"
replace yr_sch = 7.9  if year==2010&country=="Macedonia, FYR"
replace yr_sch = 4.8  if year==2010&country=="Madagascar"
replace yr_sch = 5.6  if year==2010&country=="Micronesia, Fed. Sts."
replace yr_sch = 9.9  if year==2010&country=="Montenegro"
replace yr_sch = 4.2  if year==2010&country=="Nigeria"
replace yr_sch = 12.2 if year==2010&country=="Palau"
replace yr_sch = 4.0  if year==2010&country=="Sao Tome and Principe"
replace yr_sch = 9.4  if year==2010&country=="Seychelles"
replace yr_sch = 7.3  if year==2010&country=="Suriname"
replace yr_sch = 3.6  if year==2010&country=="Timor-Leste"
replace yr_sch = 9.5  if year==2010&country=="Uzbekistan"
replace yr_sch = 8.0  if year==2010&country=="Vanuatu"


exit
