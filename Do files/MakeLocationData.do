*** gen location variables
*** Author: Martin Wiegand
*** Last changed: 25.04.2022



// set global
clear
set more off
global Raw "Directory\Progresa\Raw data" // change "Directory" to Progresa directory
global Intermediate "Directory\Progresa\Intermediate data" // change "Directory" to Progresa directory


*** get variables measuring the degree of marginalization
use "$Raw\1999\socioec_encel_99m.dta", clear
keep entidad mpio local grado indice
duplicates drop entidad mpio local, force
tempfile margi temp
save `margi'


*** Generate location predictors from 98 October survey
use "$Raw\1998\bd_rur_1998_o_localidad_Stata12", clear

// 3. Is there a...
gen mayor = r00301 == 1 if r00301 != 9 & r00301 != . // mayor
gen delegate = r00302 == 1 if r00302 != 9 & r00302 != . // delegate or municipal agent
gen subdelegate = r00303 == 1 if r00303 != 9 & r00303 != . // municipal subdelegate
gen commissioner = r00304 == 1 if r00304 != 9 & r00304 != .
gen commissary = r00305 == 1 if r00305 != 9 & r00305 != .
gen devcommittee = r00306 == 1 if r00306 != 9 & r00306 != .
gen healthcommittee = r00307 == 1 if r00307 != 9 & r00307 != .
gen educommittee = r00308 == 1 if r00308 != 9 & r00308 != .
gen agricommittee = r00309 == 1 if r00309 != 9 & r00309 != .
gen cattlecommittee = r00310 == 1 if r00310 != 9 & r00310 != .
gen DICONSA = r00311 == 1 if r00311 != 9 & r00311 != .

// 4. Is there a...
gen prodcoop = r00401 == 1 if r00401 != .
gen creditunion = r00402 == 1 if r00402 != .
gen conscoop = r00403 == 1 if r00403 != .
gen church = r00404 == 1 if r00404 != .
gen politorga = r00405 == 1 if r00405 != .
gen parentassoc = r00406 == 1 if r00406 != .
gen commassembly = r00407 == 1 if r00407 != .
gen NGO = r00408 == 1 if r00408 != 9 & r00408 != .
gen commwork = r00409 == 1 if r00409 != .

// 11. - 17. services indicative of development status
gen watersource = r01105 // public drinking water network
replace watersource = r01104 if watersource == . // pipe or truck
replace watersource = r01103 if watersource == . // standing water (lake, dam, etc.)
replace watersource = r01102 if watersource == . // running water (river, etc.)
replace watersource = r01101 if watersource == . // community well or wheel
replace watersource = r01106 if watersource == . // other
gen trash = r01205 // picked up by local authorities
replace trash = r01204 if trash == . // public garbage dump
replace trash = r01203 if trash == . // corral (?), open field or river
replace trash = r01202 if trash == . // bury
replace trash = r01201 if trash == . // burn
replace trash = r01206 if trash == . //other
gen allelectric = r013 == 1 if r013 != . // electricity available everywhere
gen partselectric = r013 == 1 | r013 == 2 if r013 != . // electricity at least partly available
gen allsewers = r014 == 1 if r014 != 9 & r014 != . // public drainage available everywhere
gen partsewers = r014 == 1 | r014 == 2 if r014 != 9 & r014 != . // public drainage at least partly available
gen phone = r01501 == 1 | r01502 == 1 // there is a (public) phone available
gen posttelegraph = r016 == 1 | r017 == 1 // post or telegraph office nearby

// 18. - 25. schools
gen numberpreschools = r018
gen allgetinpreschool = r019 == 1 if r019 != .
gen numberprimschools = r020
gen allgetinprimschool = r021 == 1 if r021 != . & r021 != 9
gen numbertelesecondaria = r022 if r022 != 9
gen allgetintelesecondaria = r023 == 1 if r023 != . & r023 != 9
gen numbersecondary = r024 if r024 != 9 // only four communities have secondary schools (and only one has a high school)
gen allgetinsecundary = r025 if r025 != .

// 26. more schools
// these are pretty much non-existent

// 36. main sectors
gen primeactivity = r03601
gen secondactivity = r03602
gen thirdactivity = r03603

// 42. - 55. average incomes / salaries
gen childlabor = r047 != 0 if r047 != .
gen avchildwage = r047
replace avchildwage = . if r047 == 99.99




/// Generate school variables (using the 2003 location survey)


// Rename location identifiers and merge with 2003 location survey
rename cveest entidad
rename cvemun muni
rename cveloc local
destring entidad, replace
destring muni, replace
destring local, replace
merge 1:1 entidad muni local using "$Raw\2003\bd_rur_2003_localidad_13oct08_Stata12.dta", nogen
drop if tipo == 3
rename muni mpio


// private secondary schools

gen hoursprivsec = l1_16_i1 if l1_16_i1 != 98
gen minprivsec = l1_16_i2 if l1_16_i1 != 98
gen ttprivsec = hoursprivsec + minprivsec/60
replace ttprivsec = hoursprivsec if hoursprivsec != . & minprivsec == .
replace ttprivsec = minprivsec/60 if hoursprivsec == . & minprivsec != .
replace ttprivsec = . if ttprivsec == 0 | ttprivsec > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttprivsec = 0.333333 if ttprivsec == . & l1_15_i3 == 1 // assume 20 minutes to school if "school is in this location"

gen hoursprivsec2 = l1_16_j1 if l1_16_j1 != 98
gen minprivsec2 = l1_16_j2 if l1_16_j1 != 98
gen ttprivsec2 = hoursprivsec2 + minprivsec2/60
replace ttprivsec2 = hoursprivsec2 if hoursprivsec2 != . & minprivsec2 == .
replace ttprivsec2 = minprivsec2/60 if hoursprivsec2 == . & minprivsec2 != .
replace ttprivsec2 = . if ttprivsec2 == 0 | ttprivsec2 > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttprivsec2 = 0.333333 if ttprivsec2 == . & l1_15_j3 == 1 // assume 20 minutes to school if "school is in this location"

replace ttprivsec = min(ttprivsec, ttprivsec2)


// public secondary schools

gen hourspubsec = l1_16_k1 if l1_16_k1 != 98
gen minpubsec = l1_16_k2 if l1_16_k1 != 98
gen ttpubsec = hourspubsec + minpubsec/60
replace ttpubsec = hourspubsec if hourspubsec != . & minpubsec == .
replace ttpubsec = minpubsec/60 if hourspubsec == . & minpubsec != .
replace ttpubsec = . if ttpubsec == 0 | ttpubsec > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttpubsec = 0.333333 if ttpubsec == . & l1_15_k3 == 1 // assume 20 minutes to school if "school is in this location"
replace ttpubsec = . if l1_13_k1 != . & l1_13_k1 > 1997 // year of creation after 1997

gen hourspubsec2 = l1_16_l1 if l1_16_l1 != 98
gen minpubsec2 = l1_16_l2 if l1_16_l1 != 98
gen ttpubsec2 = hourspubsec2 + minpubsec2/60
replace ttpubsec2 = hourspubsec2 if hourspubsec2 != . & minpubsec2 == .
replace ttpubsec2 = minpubsec2/60 if hourspubsec2 == . & minpubsec2 != .
replace ttpubsec2 = . if ttpubsec2 == 0 | ttpubsec2 > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttpubsec2 = 0.333333 if ttpubsec2 == . & l1_15_l3 == 1 // assume 20 minutes to school if "school is in this location"
replace ttpubsec2 = . if l1_13_l1 != . & l1_13_l1 > 1997 // year of creation after 1997

replace ttpubsec = min(ttpubsec, ttpubsec2)


// telesecondary

gen hourstelesec = l1_16_m1 if l1_16_m1 != 98
gen mintelesec = l1_16_m2 if l1_16_m1 != 98
gen tttelesec = hourstelesec + mintelesec/60
replace tttelesec = hourstelesec if hourstelesec != . & mintelesec == .
replace tttelesec = mintelesec/60 if hourstelesec == . & mintelesec != .
replace tttelesec = . if tttelesec == 0 | tttelesec > 10 // 0 minutes or more than 10 hours seems like a mistake
replace tttelesec = 0.333333 if tttelesec == . & l1_15_m3 == 1 // assume 20 minutes to school if "school is in this location"
replace tttelesec = . if l1_13_m1 != . & l1_13_m1 > 1997 // year of creation after 1997

gen hourstelesec2 = l1_16_n1 if l1_16_n1 != 98
gen mintelesec2 = l1_16_n2 if l1_16_n1 != 98
gen tttelesec2 = hourstelesec2 + mintelesec2/60
replace tttelesec2 = hourstelesec2 if hourstelesec2 != . & mintelesec2 == .
replace tttelesec2 = mintelesec2/60 if hourstelesec2 == . & mintelesec2 != .
replace tttelesec2 = . if tttelesec2 == 0 | tttelesec2 > 10 // 0 minutes or more than 10 hours seems like a mistake
replace tttelesec2 = 0.333333 if tttelesec2 == . & l1_15_n3 == 1 // assume 20 minutes to school if "school is in this location"
replace tttelesec2 = . if l1_13_n1 != . & l1_13_n1 > 1997 // year of creation after 1997

replace tttelesec = min(tttelesec, tttelesec2)


// closest secondary school
gen ttclosestsec = min(ttprivsec, ttpubsec, tttelesec)


// CONALEP (technical school)

gen hoursCONALEP = l1_16_o1 if l1_16_o1 != 98
gen minCONALEP = l1_16_o2 if l1_16_o1 != 98
gen ttCONALEP = hoursCONALEP + minCONALEP/60
replace ttCONALEP = hoursCONALEP if hoursCONALEP != . & minCONALEP == .
replace ttCONALEP = minCONALEP/60 if hoursCONALEP == . & minCONALEP != .
replace ttCONALEP = . if ttCONALEP == 0 | ttCONALEP > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttCONALEP = 0.333333 if ttCONALEP == . & l1_15_o3 == 1 // assume 20 minutes to school if "school is in this location"
replace ttCONALEP = . if l1_13_o1 != . & l1_13_o1 > 1997 // year of creation after 1997


// CETA

gen hoursCETA = l1_16_p1 if l1_16_p1 != 98
gen minCETA = l1_16_p2 if l1_16_p1 != 98
gen ttCETA = hoursCETA + minCETA/60
replace ttCETA = hoursCETA if hoursCETA != . & minCETA == .
replace ttCETA = minCETA/60 if hoursCETA == . & minCETA != .
replace ttCETA = . if ttCETA == 0 | ttCETA > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttCETA = 0.333333 if ttCETA == . & l1_15_p3 == 1 // assume 20 minutes to school if "school is in this location"
replace ttCETA = . if l1_13_p1 != . & l1_13_p1 > 1997 // year of creation after 1997


// CETIS

gen hoursCETIS = l1_16_q1 if l1_16_q1 != 98
gen minCETIS = l1_16_q2 if l1_16_q1 != 98
gen ttCETIS = hoursCETIS + minCETIS/60
replace ttCETIS = hoursCETIS if hoursCETIS != . & minCETIS == .
replace ttCETIS = minCETIS/60 if hoursCETIS == . & minCETIS != .
replace ttCETIS = . if ttCETIS == 0 | ttCETIS > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttCETIS = 0.333333 if ttCETIS == . & l1_15_q3 == 1 // assume 20 minutes to school if "school is in this location"
replace ttCETIS = . if l1_13_q1 != . & l1_13_q1 > 1997 // year of creation after 1997


// CEBTA

gen hoursCEBTA = l1_16_r1 if l1_16_r1 != 98
gen minCEBTA = l1_16_r2 if l1_16_r1 != 98
gen ttCEBTA = hoursCEBTA + minCEBTA/60
replace ttCEBTA = hoursCEBTA if hoursCEBTA != . & minCEBTA == .
replace ttCEBTA = minCEBTA/60 if hoursCEBTA == . & minCEBTA != .
replace ttCEBTA = . if ttCEBTA == 0 | ttCEBTA > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttCEBTA = 0.333333 if ttCEBTA == . & l1_15_r3 == 1 // assume 20 minutes to school if "school is in this location"
replace ttCEBTA = . if l1_13_r1 != . & l1_13_r1 > 1997 // year of creation after 1997


// CEBTIS

gen hoursCEBTIS = l1_16_s1 if l1_16_s1 != 98
gen minCEBTIS = l1_16_s2 if l1_16_s1 != 98
gen ttCEBTIS = hoursCEBTIS + minCEBTIS/60
replace ttCEBTIS = hoursCEBTIS if hoursCEBTIS != . & minCEBTIS == .
replace ttCEBTIS = minCEBTIS/60 if hoursCEBTIS == . & minCEBTIS != .
replace ttCEBTIS = . if ttCEBTIS == 0 | ttCEBTIS > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttCEBTIS = 0.333333 if ttCEBTIS == . & l1_15_s3 == 1 // assume 20 minutes to school if "school is in this location"
replace ttCEBTIS = . if l1_13_s1 != . & l1_13_s1 > 1997 // year of creation after 1997


// private high school

gen hoursprivhigh = l1_16_t1 if l1_16_t1 != 98
gen minprivhigh = l1_16_t2 if l1_16_t1 != 98
gen ttprivhigh = hoursprivhigh + minprivhigh/60
replace ttprivhigh = hoursprivhigh if hoursprivhigh != . & minprivhigh == .
replace ttprivhigh = minprivhigh/60 if hoursprivhigh == . & minprivhigh != .
replace ttprivhigh = . if ttprivhigh == 0 | ttprivhigh > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttprivhigh = 0.333333 if ttprivhigh == . & l1_15_t3 == 1 // assume 20 minutes to school if "school is in this location"
replace ttprivhigh = . if l1_13_t1 != . & l1_13_t1 > 1997 // year of creation after 1997

gen hoursprivhigh2 = l1_16_u1 if l1_16_u1 != 98
gen minprivhigh2 = l1_16_u2 if l1_16_u1 != 98
gen ttprivhigh2 = hoursprivhigh2 + minprivhigh2/60
replace ttprivhigh2 = hoursprivhigh2 if hoursprivhigh2 != . & minprivhigh2 == .
replace ttprivhigh2 = minprivhigh2/60 if hoursprivhigh2 == . & minprivhigh2 != .
replace ttprivhigh2 = . if ttprivhigh2 == 0 | ttprivhigh2 > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttprivhigh2 = 0.333333 if ttprivhigh2 == . & l1_15_u3 == 1 // assume 20 minutes to school if "school is in this location"
replace ttprivhigh2 = . if l1_13_u1 != . & l1_13_u1 > 1997 // year of creation after 1997

replace ttprivhigh = min(ttprivhigh, ttprivhigh2)


// public high school

gen hourspubhigh = l1_16_v1 if l1_16_v1 != 98
gen minpubhigh = l1_16_v2 if l1_16_v1 != 98
gen ttpubhigh = hourspubhigh + minpubhigh/60
replace ttpubhigh = hourspubhigh if hourspubhigh != . & minpubhigh == .
replace ttpubhigh = minpubhigh/60 if hourspubhigh == . & minpubhigh != .
replace ttpubhigh = . if ttpubhigh == 0 | ttpubhigh > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttpubhigh = 0.333333 if ttpubhigh == . & l1_15_v3 == 1 // assume 20 minutes to school if "school is in this location"
replace ttpubhigh = . if l1_13_v1 != . & l1_13_v1 > 1997 // year of creation after 1997

gen hourspubhigh2 = l1_16_w1 if l1_16_w1 != 98
gen minpubhigh2 = l1_16_w2 if l1_16_w1 != 98
gen ttpubhigh2 = hourspubhigh2 + minpubhigh2/60
replace ttpubhigh2 = hourspubhigh2 if hourspubhigh2 != . & minpubhigh2 == .
replace ttpubhigh2 = minpubhigh2/60 if hourspubhigh2 == . & minpubhigh2 != .
replace ttpubhigh2 = . if ttpubhigh2 == 0 | ttpubhigh2 > 10 // 0 minutes or more than 10 hours seems like a mistake
replace ttpubhigh2 = 0.333333 if ttpubhigh2 == . & l1_15_w3 == 1 // assume 20 minutes to school if "school is in this location"
replace ttpubhigh2 = . if l1_13_w1 != . & l1_13_w1 > 1997 // year of creation after 1997

replace ttpubhigh = min(ttpubhigh, ttpubhigh2)


// closest high school
gen ttclosesthigh = min(ttprivhigh, ttpubhigh)




// village is indigenous
gen indloc = tipo_ind == 3 if tipo_ind != .

merge 1:1 entidad mpio local using `margi', nogen



global locationvars "grado indice mayor delegate subdelegate commissioner commissary devcommittee healthcommittee educommittee agricommittee cattlecommittee DICONSA prodcoop creditunion conscoop church politorga parentassoc commassembly NGO commwork watersource trash allelectric partselectric allsewers partsewers phone posttelegraph numberpreschools allgetinpreschool numberprimschools allgetinprimschool numbertelesecondaria allgetintelesecondaria numbersecondary allgetinsecundary primeactivity secondactivity thirdactivity childlabor avchildwage indloc"
global schoolvars "ttprivsec ttpubsec tttelesec ttclosestsec ttCONALEP ttCETA ttCETIS ttCEBTA ttCEBTIS ttprivhigh ttpubhigh ttclosesthigh"
global alllocationvars "$locationvars $schoolvars"
keep entidad mpio local $alllocationvars 
save "$Intermediate\location", replace

