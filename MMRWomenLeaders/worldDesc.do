use "countries/world", clear

#delimit ;
drop if NAME=="Aruba"|NAME=="Aland"|NAME=="American Samoa"|NAME=="Antarctica";
drop if NAME=="Ashmore and Cartier Is."|NAME=="Fr. S. Antarctic Lands";
drop if NAME=="Bajo Nuevo Bank (Petrel Is.)"|NAME=="Clipperton I.";
drop if NAME=="Cyprus U.N. Buffer Zone"|NAME=="Cook Is."|NAME=="Coral Sea Is.";
drop if NAME=="Cayman Is."|NAME=="N. Cyprus"|NAME=="Dhekelia"|NAME=="Falkland Is.";
drop if NAME=="Faeroe Is."|NAME=="Micronesia"|NAME=="Guernsey";
drop if NAME=="Heard I. and McDonald Is."|NAME=="Isle of Man"|NAME=="Indian Ocean Ter.";
drop if NAME=="Br. Indian Ocean Ter."|NAME=="Baikonur"|NAME=="Siachen Glacier";
drop if NAME=="St. Kitts and Nevis"|NAME=="Saint Lucia"|NAME=="St-Martin";
drop if NAME=="Marshall Is."|NAME=="N. Mariana Is."|NAME=="New Caledonia";
drop if NAME=="Norfolk Island"|NAME=="Niue"|NAME=="Nauru"|NAME=="Pitcairn Is.";
drop if NAME=="Spratly Is."|NAME=="Fr. Polynesia"|NAME=="Scarborough Reef";
drop if NAME=="Serranilla Bank"|NAME=="S. Geo. and S. Sandw. Is."|NAME=="San Marino";
drop if NAME=="St. Pierre and Miquelon"|NAME=="Sint Maarten"|NAME=="Seychelles";
drop if NAME=="Turks and Caicos Is."|NAME=="U.S. Minor Outlying Is.";
drop if NAME=="St. Vin. and Gren."|NAME=="British Virgin Is.";
drop if NAME=="USNB Guantanamo Bay"|NAME=="Wallis and Futuna Is."|NAME=="Akrotiri";
drop if NAME=="Antigua and Barb."|NAME=="Bermuda"|NAME=="Kiribati"|NAME=="St-Barthélemy";
drop if NAME=="Curaçao"|NAME=="Dominica"|NAME=="Guam";
drop if NAME=="Malta"|NAME=="Montserrat"|NAME=="Palau"|NAME=="Mauritius";
drop if NAME=="Tonga"|NAME=="Trinidad and Tobago";
drop if NAME=="Tuvalu"|NAME=="U.S. Virgin Is."|NAME=="Vanuatu";
#delimit cr

gen country = NAME
replace country = "Bosnia and Herzegovina" if country=="Bosnia and Herz."
replace country = "Dominican Republic" if country=="Dominican Rep."
replace country = "Korea, Rep." if country=="Korea"
replace country = "Kyrgyz Republic" if country=="Kyrgyzstan"
replace country = "Macedonia, FYR" if country=="Macedonia"
merge 1:1 country using quotasComplete

drop if _merge==2
gen quota = 2 if quotatype=="Reserved"|quotatype=="Candidates/Reserved"
replace quota = 1 if quota==.
*replace quota = 3 if 
*replace quota = 1 if quota==.

#delimit ;
spmap quota using "countries/world_coords" , id(_ID) osize(vvthin)
fcolor(BuYlRd) clmethod(unique) clbreaks(0 1 2)
legend(title("Quota Type", size(*1.45) bexpand justification(left)))
legend(label(1 "None") label(2 "No Reserved Seats")) legend(label(3 "Reserved Seats"))
legend(label(4 "Both")) 
legend(symy(*1.45) symx(*1.45) size(*1.98))
;

graph export "quotas/coverage.eps", as(eps) replace;
#delimit cr
