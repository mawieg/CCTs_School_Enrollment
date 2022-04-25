*** Merge different survey rounds, define relevant variables, and construct
*** datasets for analysis in R
*** Author: Martin Wiegand
*** Last changed: 24.10.2020





// set global
clear
set more off
global Progresa "Directory\Progresa" // change this to Progresa directory

use "$Progresa\1999\socioec_encel_99n.dta", clear // November 1999 survey
drop if renglon == .
keep folio renglon n002 n039 n041a n041b n042 // n039: currently going to school, n041a n041b: highest completed level of schooling, n002: age, n042: number of years repeated; current level of schooling is missing
tempfile temp99n
save `temp99n'
use "$Progresa\2000\socioec_encel2000m.dta", clear // March 2000 survey
drop if renglon == .
keep folio renglon z35 z2creada // z2creada: age, z35: person went to school in school year 99/00; there is no variable indicating current school year or highest completed grade
tempfile temp00m
save `temp00m'
use "$Progresa\2000\socioec_encel_2000n.dta", clear // November 2000 survey
drop if renglon == .
rename contba treatment2000n
* Generate pc expenditure variable
gen exp = 0
forv i = 1/40 { // weekly food expenditure
	local j = 12000 + `i'
	replace exp = exp + w`j' * 30/7 if w`j' != 988 & w`j' != 999 & w`j' != .
}
forv i = 1/3 { // weekly expenses on transportation, health, clothing
	local j = 12600 + `i'
	replace exp = exp + w`j' * 30/7 if w`j' != 999 & w`j' != .
}
forv i = 1/7 { // monthly expenses on transportation, health, clothing
	local j = 12700 + `i'
	replace exp = exp + w`j' if w`j' != 999 & w`j' != .
}
forv i = 1/14 { // biannual expenses on transportation, health, clothing
	local j = 12800 + `i'
	replace exp = exp + w`j' * 30/183 if w`j' != 9999 & w`j' != .
}
forv i = 1/16 { // annual expenses on assets
	local j = 13200 + `i'
	replace exp = exp + w`j' * 30/365 if w`j' != 98888 & w`j' != 99999 & w`j' != .
}
forv i = 17/25 { // annual expenses on home improvements
	local j = 13500 + `i' - 16
	replace exp = exp + w`j' * 30/365 if w`j' != 98888 & w`j' != 99999 & w`j' != .
}
forv i = 1/7 { // health service expenditure in the last 4 weeks
	local j = 14700 + `i'
	replace exp = exp + w`j'a * 30/28 if w`j'a != 988 & w`j'a != 999 & w`j'a != .
	replace exp = exp + w`j'b * 30/28 if w`j'b != 988 & w`j'b != 999 & w`j'b != .
	replace exp = exp + w`j'c * 30/28 if w`j'c != 988 & w`j'c != 999 & w`j'c != .
	
	local j = 14800 + `i'
	replace exp = exp + w`j'a * 30/28 if w`j'a != 988 & w`j'a != 999 & w`j'a != .
	replace exp = exp + w`j'b * 30/28 if w`j'b != 988 & w`j'b != 999 & w`j'b != .
	replace exp = exp + w`j'c * 30/28 if w`j'c != 988 & w`j'c != 999 & w`j'c != .

	local j = 15000 + `i'
	replace exp = exp + w`j'a * 30/28 if w`j'a != 988 & w`j'a != 999 & w`j'a != .
	replace exp = exp + w`j'b * 30/28 if w`j'b != 988 & w`j'b != 999 & w`j'b != .
	replace exp = exp + w`j'c * 30/28 if w`j'c != 988 & w`j'c != 999 & w`j'c != .	

	local j = 15100 + `i'
	replace exp = exp + w`j'a * 30/28 if w`j'a != 988 & w`j'a != 999 & w`j'a != .
	replace exp = exp + w`j'b * 30/28 if w`j'b != 988 & w`j'b != 999 & w`j'b != .
	replace exp = exp + w`j'c * 30/28 if w`j'c != 988 & w`j'c != 999 & w`j'c != .
}

bysort folio: egen expenditure = total(exp) 
gen inhouse = w004 == 3 | w004 == 4
bysort folio: egen hhsize = total(inhouse) 
gen pcexp = expenditure / hhsize if hhsize != 0 // per capita expenditure
gen lnpcexp = ln(pcexp) // log per capita expenditure

keep folio renglon treatment2000n hhsize pcexp lnpcexp w002cor w008 w041a w041b w044a w043 w044b w045 w046 w047 w049 w050 w051 w05201 w05202 w05203 w05204 w05208 w05209 w053 w054 // w041a w041b: highest completed level of schooling, w044a w044b: current level, w043: going to school now, w002cor: age, w008: gender, w046: until what age did you attend school, w047: have you ever repeated a grade
tempfile temp00n
save `temp00n'


// merge baseline data from 1997 and 1998

// prepare 1998 predictors

use "$Progresa\1998\socioec_encel_98m_Stata12.dta", clear

gen goodatschool = p009 == 1 if p009 != 0 & p009 != 7 & p009 != 9 & p009 != . // student is good at school
gen cansec = p011 >= 2 & p011 <=6 if p011 != 0 & p011 != 7 & p011 != 9 // can go to secondary or higher
gen canfurther = p011 >= 3 & p011 <=6 if p011 != 0 & p011 != 7 & p011 != 9 // can go further than secondary
gen canhigh = p011 == 3 | p011 ==6 if p011 != 0 & p011 != 7 & p011 != 9 // can go to high school or uni
gen canuni = p011 == 6 if p011 != 0 & p011 != 7 & p011 != 9 // can go to uni
bysort folio: egen desgirl = total(ep022) // desired education level for girls
gen desgirlsec = desgirl >= 2 & desgirl <=6 if desgirl != 0 & desgirl != 7 & desgirl != 9 // girls should go to secondary or higher
gen desgirlfurther = desgirl >= 3 & desgirl <=6 if desgirl != 0 & desgirl != 7 & desgirl != 9 // girls should go further than secondary
gen desgirlhigh = desgirl == 3 | desgirl ==6 if desgirl != 0 & desgirl != 7 & desgirl != 9 // girls should go to high school or uni
gen desgirluni = desgirl == 6 if desgirl != 0 & desgirl != 7 & desgirl != 9 // girls should go to uni
bysort folio: egen desboy = total(p023) // desired education level for boys
gen desboysec = desboy >= 2 & desboy <=6 if desboy != 0 & desboy != 7 & desboy != 9 // boys should go to secondary or higher
gen desboyfurther = desboy >= 3 & desboy <=6 if desboy != 0 & desboy != 7 & desboy != 9 // boys should go further than secondary
gen desboyhigh = desboy == 3 | desboy ==6 if desboy != 0 & desboy != 7 & desboy != 9 // boys should go to high school or uni
gen desboyuni = desboy == 6 if desboy != 0 & desboy != 7 & desboy != 9 // boys should go to uni
bysort folio: egen breakfast = total(p017) // Children eat breakfast before school
replace breakfast = . if breakfast == 0 | breakfast == 9
replace breakfast = 0 if breakfast == 2
bysort folio: egen whynobreakfast = total(p019) // Reason why children don't eat breakfast before school
replace whynobreakfast = . if whynobreakfast == 0 | whynobreakfast == 9
bysort folio: egen talkwithteacher = total(p020) // Have parents talked to teacher this year?
replace talkwithteacher = . if talkwithteacher == 0 | talkwithteacher == 9
replace talkwithteacher = 0 if talkwithteacher == 2
bysort folio: egen whytalkwithteacher = total(ep021) // Reason for talk with teacher
replace whytalkwithteacher = . if whytalkwithteacher == 0 | whytalkwithteacher == 9
bysort folio: egen guardian = total(p024) // Parent participates in parent / guardian association of school
bysort folio: egen schoolwork = total(p025) // Parent participates in school work
bysort folio: egen problemdiscipline = total(p02601) // In the school, there are problems with lack of discipline
bysort folio: egen problemteachers = total(p02602) // In the school, there are problems with lack of interest of the teachers
bysort folio: egen problemcommunication = total(p02603) // In the school, there are problems with poor communication between teachers and parents
bysort folio: egen problemteachattendance = total(p02604) // In the school, there are problems with poor teacher attendance
bysort folio: egen teacherprepared = total(p02701) // Is your child(ren)’s teacher prepared
bysort folio: egen teacherfulfilled = total(p02702) // Is your child(ren)’s teacher fulfilled
bysort folio: egen teacherpunctual = total(p02703) // Is your child(ren)’s teacher punctual
bysort folio: egen teacherpatient = total(p02704) // Is your child(ren)’s teacher patient with the children
bysort folio: egen helpsibgirl = total(p028) // age from which girls can help younger siblings
qui replace helpsibgirl = . if helpsibgirl == 0 | helpsibgirl == 77 | helpsibgirl == 99
bysort folio: egen helpsibboy = total(p029) // age from which boys can help younger siblings
qui replace helpsibboy = . if helpsibboy == 0 | helpsibboy == 77 | helpsibboy == 99
bysort folio: egen helpworkgirl = total(p030) // age from which girls can help with work
qui replace helpworkgirl = . if helpworkgirl == 0 | helpworkgirl == 77 | helpworkgirl == 99
bysort folio: egen helpworkboy = total(ep031) // age from which boys can help with work
qui replace helpworkboy = . if helpworkboy == 0 | helpworkboy == 77 | helpworkboy == 99
bysort folio: egen earnmongirl = total(ep032) // age from which girls can work to earn money
qui replace earnmongirl = . if earnmongirl == 0 | earnmongirl == 77 | earnmongirl == 99
bysort folio: egen earnmonboy = total(p033) // age from which boys can work to earn money
qui replace earnmonboy = . if earnmonboy == 0 | earnmonboy == 77 | earnmonboy == 99
bysort folio: egen weekexp1 = total(p04801) // Weekly expenditures for school transportation
bysort folio: egen weekexp2 = total(p04802) // Weekly expenditures for other trips
bysort folio: egen weekexp3 = total(p04803) // Weekly expenditures for cigars or tobacco
bysort folio: egen weekexp4 = total(p04804) // Weekly expenditures for alcoholic beverages 
bysort folio: egen weekexp5 = total(p04805) // Weekly expenditures for non-alcoholic beverages
bysort folio: egen monthexp1 = total(p04901) // Monthly expenditures for hygiene items
bysort folio: egen monthexp2 = total(p04902) // Monthly expenditures for medicine
bysort folio: egen monthexp3 = total(p04903) // Monthly expenditures for medical consultations
bysort folio: egen sixmonthexp1 = total(p05001) // 6-Monthly expenditures for home accessories
bysort folio: egen sixmonthexp2 = total(p05002) // 6-Monthly expenditures for toys
bysort folio: egen sixmonthexp3 = total(p05003) // 6-Monthly expenditures for clothing (girls)
bysort folio: egen sixmonthexp4 = total(p05004) // 6-Monthly expenditures for clothing (boys)
bysort folio: egen sixmonthexp5 = total(p05005) // 6-Monthly expenditures for clothing (women)
bysort folio: egen sixmonthexp6 = total(p05006) // 6-Monthly expenditures for clothing (men)
bysort folio: egen sixmonthexp7 = total(p05007) // 6-Monthly expenditures for shoes (girls)
bysort folio: egen sixmonthexp8 = total(p05008) // 6-Monthly expenditures for shoes (boys)
bysort folio: egen sixmonthexp9 = total(p05009) // 6-Monthly expenditures for shoes (women)
bysort folio: egen sixmonthexp10 = total(p05010) // 6-Monthly expenditures for shoes (men)
bysort folio: egen sixmonthexp11 = total(p05011) // 6-Monthly expenditures for school supplies
bysort folio: egen sixmonthexp12 = total(p05012) // 6-Monthly expenditures for school fees
bysort folio: egen spendmoreon1 = total(p05101) // If we had more money, we'd spend it on food
bysort folio: egen spendmoreon2 = total(p05102) // If we had more money, we'd spend it on clothes or shoes
bysort folio: egen spendmoreon3 = total(p05103) // If we had more money, we'd spend it on debt payments
bysort folio: egen spendmoreon4 = total(p05104) // If we had more money, we'd spend it on animals
bysort folio: egen spendmoreon5 = total(p05105) // If we had more money, we'd spend it on seeds or plants
bysort folio: egen spendmoreon6 = total(p05106) // If we had more money, we'd spend it on work tools
bysort folio: egen spendmoreon7 = total(p05107) // If we had more money, we'd spend it on home appliances
bysort folio: egen spendmoreon8 = total(p05108) // If we had more money, we'd spend it on alcoholic beverages
bysort folio: egen spendmoreon9 = total(p05109) // If we had more money, we'd spend it on walks and entertainment
bysort folio: egen spendmoreon10 = total(p05110) // If we had more money, we'd spend it on medicine
bysort folio: egen spendmoreon11 = total(p05111) // If we had more money, we'd spend it on school supplies
bysort folio: egen spendmoreon12 = total(p05112) // If we had more money, we'd spend it on toys
bysort folio: egen spendmoreon13 = total(p05113) // If we had more money, we'd save it
bysort folio: egen spendmoreon14 = total(p05114) // If we had more money, we'd spend it on (other)

tempfile temp98m
save `temp98m'


// prepare 1997 predictors

use "$Progresa\1997\ENCASEH 97_CALIF ORIG Y 2003_Stata12.dta", clear

bysort claveofi: egen vilcount = count(claveofi)
bysort claveofi: egen inter = count(claveofi) if pobre
bysort claveofi: egen vilcountpoor = mean(inter)
drop inter
gen young = p08 < 15
gen age15to20 = 1 if p08 >= 15 & p08 <= 20
gen age21to30 = 1 if p08 >= 21 & p08 <= 30
gen poor1 = 1 if pobre
gen age15to20poor = 1 if p08 >= 15 & p08 <= 20 & pobre
gen age21to30poor = 1 if p08 >= 21 & p08 <= 30 & pobre
bysort folio: egen numunder15 = sum(young) // number of hh members under 15 in 1997
gen rightage = p20 == 62 & p08 >= 11 & p08 <= 14 // highest finished grade is 6th grade primary school & age between 11 and 14
bysort claveofi: egen rightagetotal = total(rightage)
gen rightageshare = rightagetotal / vilcount // share of potential secondary school finishers by 2000
gen rightagepoor = rightage if pobre
bysort claveofi: egen rightagepoortotal = total(rightagepoor)
gen rightagepoorshare = rightagepoortotal / vilcountpoor // share of potential secondary school finishers by 2000
gen rightageattend97 = rightage & p21 == 1
bysort claveofi: egen rightageattend97total = total(rightageattend97)
gen rightageattend97share = rightageattend97total / vilcount // share of potential secondary school finishers by 2000
gen rightageattend97poor = rightageattend97 if pobre
bysort claveofi: egen rightageattend97poortotal = total(rightageattend97poor)
gen rightageattend97poorshare = rightageattend97poortotal / vilcountpoor // share of potential secondary school finishers by 2000
bysort claveofi: egen educount = count(p20) // number of people in village with nonmissing education
bysort claveofi: egen educount15to20 = count(age15to20) // number of people between 15 and 20 in village with nonmissing education
bysort claveofi: egen educount21to30 = count(age21to30) // number of people between 21 and 30 in village with nonmissing education
bysort claveofi: egen educountpoor = count(poor1) // number of people in village with nonmissing education
bysort claveofi: egen educount15to20poor = count(age15to20poor) // number of people between 15 and 20 in village with nonmissing education
bysort claveofi: egen educount21to30poor = count(age21to30poor) // number of people between 21 and 30 in village with nonmissing education
gen fatherathome = p13 != 77 & p13 != 99
gen motherathome = p14 != 77 & p13 != 99
gen literate = p18 == 1 if p18 == 1 | p18 == 2
gen fatherlit = . // father literate
gen motherlit = . // mother literate
gen wenttoschool = p19 == 1 if p19 == 1 | p19 == 2
gen fatherschool = . // father went to school
gen motherschool = . // mother went to school
gen minprimschool = (p20 == 13 | p20 == 14 | p20 == 15 | p20 == 16 | p20 == 17 | p20 == 23 | p20 == 24 | p20 == 25 | p20 == 26 | p20 == 27 | p20 == 33 | p20 == 34 | p20 == 35 | p20 == 36 | p20 == 37 | p20 == 43 | p20 == 44 | p20 == 45 | p20 == 46 | p20 == 47 | p20 == 53 | p20 == 54 | p20 == 55 | p20 == 56 | p20 == 57 | p20 == 62 | p20 == 63 | p20 == 64 | p20 == 65 | p20 == 66 | p20 == 67 | p20 == 76 | p20 == 86 | p20 == 93 | p20 == 94 | p20 == 95 | p20 == 96) if wenttoschool != . & p20 != 99 // = 1 if person has completed at least primary education
gen minprimschool15to20 = minprimschool if age15to20
gen minprimschool21to30 = minprimschool if age21to30
gen minprimschoolpoor = minprimschool if pobre
gen minprimschool15to20poor = minprimschool if age15to20 & pobre
gen minprimschool21to30poor = minprimschool if age21to30 & pobre
bysort claveofi: egen minprimtotal = total(minprimschool)
bysort claveofi: egen minprimtotal15to20 = total(minprimschool15to20)
bysort claveofi: egen minprimtotal21to30 = total(minprimschool21to30)
bysort claveofi: egen minprimshare = mean(minprimtotal/educount)
bysort claveofi: egen minprimshare15to20 = mean(minprimtotal15to20/educount15to20)
bysort claveofi: egen minprimshare21to30 = mean(minprimtotal21to30/educount21to30)
bysort claveofi: egen minprimtotalpoor = total(minprimschoolpoor)
bysort claveofi: egen minprimtotal15to20poor = total(minprimschool15to20poor)
bysort claveofi: egen minprimtotal21to30poor = total(minprimschool21to30poor)
bysort claveofi: egen minprimsharepoor = mean(minprimtotalpoor/educountpoor)
bysort claveofi: egen minprimshare15to20poor = mean(minprimtotal15to20poor/educount15to20poor)
bysort claveofi: egen minprimshare21to30poor = mean(minprimtotal21to30poor/educount21to30poor)
gen fatherminprim = .
gen motherminprim = .
gen minsecschool = (p20 == 14 | p20 == 15 | p20 == 16 | p20 == 17 | p20 == 24 | p20 == 25 | p20 == 26 | p20 == 27 | p20 == 33 | p20 == 34 | p20 == 35 | p20 == 36 | p20 == 37 | p20 == 44 | p20 == 46 | p20 == 47 | p20 == 56 | p20 == 66 | p20 == 76 | p20 == 86 | p20 == 94 | p20 == 95 | p20 == 96) if wenttoschool != . & p20 != 99 // = 1 if person has at least completed secondary education
gen minsecschool15to20 = minsecschool if age15to20
gen minsecschool21to30 = minsecschool if age21to30
gen minsecschoolpoor = minsecschool if pobre
gen minsecschool15to20poor = minsecschool if age15to20 & pobre
gen minsecschool21to30poor = minsecschool if age21to30 & pobre
bysort claveofi: egen minsectotal = total(minsecschool)
bysort claveofi: egen minsectotal15to20 = total(minsecschool15to20)
bysort claveofi: egen minsectotal21to30 = total(minsecschool21to30)
bysort claveofi: egen minsecshare = mean(minsectotal/educount)
bysort claveofi: egen minsecshare15to20 = mean(minsectotal15to20/educount15to20)
bysort claveofi: egen minsecshare21to30 = mean(minsectotal21to30/educount21to30)
bysort claveofi: egen minsectotalpoor = total(minsecschoolpoor)
bysort claveofi: egen minsectotal15to20poor = total(minsecschool15to20poor)
bysort claveofi: egen minsectotal21to30poor = total(minsecschool21to30poor)
bysort claveofi: egen minsecsharepoor = mean(minsectotalpoor/educountpoor)
bysort claveofi: egen minsecshare15to20poor = mean(minsectotal15to20poor/educount15to20poor)
bysort claveofi: egen minsecshare21to30poor = mean(minsectotal21to30poor/educount21to30poor)
gen fatherminsec = .
gen motherminsec = .
gen minhighschool = (p20 == 17 | p20 == 27 | p20 == 34 | p20 == 35 | p20 == 37 | p20 == 44 | p20 == 47 | p20 == 57) if wenttoschool != . & p20 != 99 // = 1 if person has at least completed high school or basica normal education
gen minhighschool15to20 = minhighschool if age15to20
gen minhighschool21to30 = minhighschool if age21to30
gen minhighschoolpoor = minhighschool if pobre
gen minhighschool15to20poor = minhighschool if age15to20 & pobre
gen minhighschool21to30poor = minhighschool if age21to30 & pobre
bysort claveofi: egen minhightotal = total(minhighschool)
bysort claveofi: egen minhightotal15to20 = total(minhighschool15to20)
bysort claveofi: egen minhightotal21to30 = total(minhighschool21to30)
bysort claveofi: egen minhighshare = mean(minhightotal/educount)
bysort claveofi: egen minhighshare15to20 = mean(minhightotal15to20/educount15to20)
bysort claveofi: egen minhighshare21to30 = mean(minhightotal21to30/educount21to30)
bysort claveofi: egen minhightotalpoor = total(minhighschoolpoor)
bysort claveofi: egen minhightotal15to20poor = total(minhighschool15to20poor)
bysort claveofi: egen minhightotal21to30poor = total(minhighschool21to30poor)
bysort claveofi: egen minhighsharepoor = mean(minhightotalpoor/educountpoor)
bysort claveofi: egen minhighshare15to20poor = mean(minhightotal15to20poor/educount15to20poor)
bysort claveofi: egen minhighshare21to30poor = mean(minhightotal21to30poor/educount21to30poor)
gen fatherminhigh = .
gen motherminhigh = .
gen verypoor = pobextre == 11 // very poor
gen poor = pobextre == 11 | pobextre == 21 // at least a bit poor
gen notrich = pobextre == 11 | pobextre == 21 | pobextre == 10 // poor or almost poor, but not clearly non-poor
gen povertycont = mpcalif // continuous poverty measure
gen povertycont2 = povertycont^2
gen povertycont3 = povertycont^3
sum renglon, meanonly
forv i = 1/`r(max)' {
	qui {
	// literacy
	gen inter1 = p18 if renglon == `i'
	bysort folio: egen inter2 = mean(inter1)
	replace fatherlit = (inter2 == 1) if p13 == `i'
	gen inter3 = p18 if renglon == `i'
	bysort folio: egen inter4 = mean(inter3)
	replace motherlit = (inter4 == 1) if p14 == `i'
	qui drop inter1 inter2 inter3 inter4
	// went to school
	gen inter1 = p19 if renglon == `i'
	bysort folio: egen inter2 = mean(inter1)
	replace fatherschool = (inter2 == 1) if p13 == `i'
	gen inter3 = p19 if renglon == `i'
	bysort folio: egen inter4 = mean(inter3)
	replace motherschool = (inter4 == 1) if p14 == `i'
	qui drop inter1 inter2 inter3 inter4
	// at least primary school
	gen inter1 = minprimschool if renglon == `i'
	bysort folio: egen inter2 = mean(inter1)
	replace fatherminprim = (inter2 == 1) if p13 == `i'
	gen inter3 = minprimschool if renglon == `i'
	bysort folio: egen inter4 = mean(inter3)
	replace motherminprim = (inter4 == 1) if p14 == `i'
	qui drop inter1 inter2 inter3 inter4
	// at least secondary school
	gen inter1 = minsecschool if renglon == `i'
	bysort folio: egen inter2 = mean(inter1)
	replace fatherminsec = (inter2 == 1) if p13 == `i'
	gen inter3 = minsecschool if renglon == `i'
	bysort folio: egen inter4 = mean(inter3)
	replace motherminsec = (inter4 == 1) if p14 == `i'
	qui drop inter1 inter2 inter3 inter4
	// at least high school or basica normal
	gen inter1 = minhighschool if renglon == `i'
	bysort folio: egen inter2 = mean(inter1)
	replace fatherminhigh = (inter2 == 1) if p13 == `i'
	gen inter3 = minhighschool if renglon == `i'
	bysort folio: egen inter4 = mean(inter3)
	replace motherminhigh = (inter4 == 1) if p14 == `i'
	qui drop inter1 inter2 inter3 inter4
	}
}

merge 1:n folio renglon using `temp98m'
drop if _merge == 2
rename _merge merge98m
merge 1:1 folio renglon using `temp99n'
rename _merge merge99n
merge 1:1 folio renglon using `temp00m'
rename _merge merge00m
merge 1:1 folio renglon using `temp00n'
rename _merge merge00n
merge n:1 entidad mpio local using "$Progresa\location"
rename _merge mergeloc

// prepare some more predictor variables
gen female = 1 if p11 == 2
replace female = 0 if p11 == 1
gen age97 = p08
gen level98 = p20 // only for treatment effects unconditional on surviving secondary school
gen attend97 = p21 == 1 if p21 != .
gen attend98 = p006 == 1 if p006 != .
gen attend9798 = p006 == 1 & p21 == 1 if p006 != . & p21 != .
rename p07 hhsize97

// merge with 2003 dataset
rename hogares hogar
merge 1:1 folio hogar renglon using "$Progresa\2003\bd_rur_2003_socioeconomico_personas_Stata12.dta", keepusing(s2_4 s3_4 s3_1_1 s3_1_2 s3_1_3 s3_12_1 s3_12_2 s5_25_2 s5_25_1 s5_30_1 s5_30_2 s5_34 s5_35_1 s5_35_2 s5_5_1 s5_5_2 hog_nue fase_in anio_inc bim_inc tipo res_fin)
drop if _merge == 2
rename _merge merge03

gen treatment = contba == 1

save "$Progresa\Big Dataset", replace

// exit







// Make conditional sample
use "$Progresa\Big Dataset", clear
keep if w041a == 3 & w041b == 3 & n039 == 1 & z35 == 1 // keep only those who were in last year of secondary school by the end of school year 99/00

gen agedif = w002cor - w046 if w046 <= 50 // age difference between current age and age when quit school
gen lastyear = agedif == -1 | agedif == 0 | (agedif == 1 & p09m >= 7 & p09m <= 11)
keep if w041a == 3 & w041b == 3 // & z35 == 1
drop if w044b == 3 | w044b == 9 // drop if people say they are still in secondary school (despite the fact that these (3) students went to the last grade of sec. school last year and claim to not ever have repeated a year) or NR
drop if w044a == 2 | w044a == 3 // not sure whether these students skipped the first grade of high school (which seems unlikely) or have already been enrolled there for a year (in which case they haven't been exposed to the program for the same length as the rest); I decide to drop them
drop if p11 != p003 & p11 != . & p003 != . // delete observations with a sex change between November 1997 and March 1998
gen genderconsistent9703 = (p11 == s3_4 | s3_4 == .) // these observations are gender-consistent between November 1997 and (Winter) 2003
gen schoolinconsistent03 = s5_5_2 == 2 | s5_5_2 == 3 | s3_12_2 == 0 | s3_12_2 == 2
drop if merge98m == .

gen agedif9798 = p004 - p08
gen agedif9700 = w002cor - p08
keep if agedif9700 >= 2 & agedif9700 <= 4 & w002cor < 18 // drop if age is 18 or older in November 2000 or in case of age inconsistencies between 1997 and 2000
drop if (agedif9798 < 0 | agedif9798 > 1) & agedif9798 != . // drop age inconsistencies between 1997 and 1998
gen agedif9703 = s2_4 - p08 // for the highschool03 variable only
egen numkids = count(claveofi), by(claveofi)
egen internumkidspoor = count(claveofi) if pobre, by(claveofi)
egen numkidspoor = mean(internumkidspoor), by(claveofi)
qui replace numkidspoor = 0 if numkidspoor == .
egen internumkidsnonpoor = count(claveofi) if !pobre, by(claveofi)
egen numkidsnonpoor = mean(internumkidsnonpoor), by(claveofi)
qui replace numkidsnonpoor = 0 if numkidsnonpoor == .
drop internumkidspoor internumkidsnonpoor

// treatment and outcome variables
gen highschool = (w044b == 5 | w044b == 6) // high school in November 2000
gen highschool03 = 0 if s3_12_2 >= 3 & s3_12_2 <= 8 // highest school level in 2003 was high school, basica normal, or postgraduate
replace highschool03 = 1 if s3_12_2 == 4 | s3_12_2 == 5 | s3_12_2 == 8
replace highschool03 = . if (agedif9703 < 5 & agedif9703 != .) | (agedif9703 > 7 & agedif9703 != .) | genderconsistent9703 == 0 | schoolinconsistent03 == 1 // set to missing if age difference is greater than one year or in case of sex change between 1997 and 2003
gen highschool03fin = 0 if s3_12_2 >= 3 & s3_12_2 <= 8 
replace highschool03fin = 1 if ((s3_12_2 == 4 | s3_12_2 == 5) & s3_12_1 == 3) | s3_12_2 == 8 // completed high school by 2003 or ...
replace highschool03fin = 1 if s5_5_2 == 4 & (s5_5_1 == 3 | s5_5_1 == 4) // ... in last grade of high school in 2003
replace highschool03fin = . if (agedif9703 < 5 & agedif9703 != .) | (agedif9703 > 7 & agedif9703 != .) | genderconsistent9703 == 0 | schoolinconsistent03 == 1 // set to missing if age difference is greater than one year or in case of sex change between 1997 and 2003

gen moneyreason = w045 == 1 | w045 == 2 | w045 == 3 // reason for not continuing school was money-related
gen otherreason = moneyreason != 1 & highschool != 1 // reason for not continuing school was not money-related
gen missing03 = highschool03 == . if highschool != . // missing outcomes from 2003 survey

save "$Progresa\Highschool_big.dta", replace

// export to csv (for analysis with R)
global features97 "female age97 attend97 attend98 hhsize97 numunder15 fatherathome motherathome fatherlit motherlit fatherschool motherschool fatherminprim motherminprim fatherminsec motherminsec fatherminhigh motherminhigh verypoor poor notrich yycali mpcalif"
global features98m "goodatschool cansec canfurther canhigh canuni desgirlsec desgirlfurther desgirlhigh desgirluni desboysec desboyfurther desboyhigh desboyuni breakfast whynobreakfast talkwithteacher whytalkwithteacher guardian schoolwork problemdiscipline problemteachers problemcommunication problemteachattendance teacherprepared teacherfulfilled teacherpunctual teacherpatient helpsibgirl helpsibboy helpworkgirl helpworkboy earnmongirl earnmonboy weekexp1 weekexp2 weekexp3 weekexp4 weekexp5 monthexp1 monthexp2 monthexp3 sixmonthexp1 sixmonthexp2 sixmonthexp3 sixmonthexp4 sixmonthexp5 sixmonthexp6 sixmonthexp7 sixmonthexp8 sixmonthexp9 sixmonthexp10 sixmonthexp11 sixmonthexp12 spendmoreon1 spendmoreon2 spendmoreon3 spendmoreon4 spendmoreon5 spendmoreon6 spendmoreon7 spendmoreon8 spendmoreon9 spendmoreon10 spendmoreon11 spendmoreon12 spendmoreon13 spendmoreon14"
global locationfeatures "grado indice mayor delegate subdelegate commissioner commissary devcommittee healthcommittee educommittee agricommittee cattlecommittee DICONSA prodcoop creditunion conscoop church politorga parentassoc commassembly NGO commwork watersource trash allelectric partselectric allsewers partsewers phone posttelegraph numberpreschools allgetinpreschool numberprimschools allgetinprimschool numbertelesecondaria allgetintelesecondaria numbersecondary allgetinsecundary primeactivity secondactivity thirdactivity childlabor avchildwage indloc vilcount vilcountpoor rightagetotal rightageshare rightagepoortotal rightagepoorshare rightageattend97total rightageattend97share rightageattend97poortotal rightageattend97poorshare minprimtotal minprimtotal15to20 minprimtotal21to30 minprimshare minprimshare15to20 minprimshare21to30 minprimtotalpoor minprimtotal15to20poor minprimtotal21to30poor minprimsharepoor minprimshare15to20poor minprimshare21to30poor minsectotal minsectotal15to20 minsectotal21to30 minsecshare minsecshare15to20 minsecshare21to30 minsectotalpoor minsectotal15to20poor minsectotal21to30poor minsecsharepoor minsecshare15to20poor minsecshare21to30poor minhightotal minhightotal15to20 minhightotal21to30 minhighshare minhighshare15to20 minhighshare21to30 minhightotalpoor minhightotal15to20poor minhightotal21to30poor minhighsharepoor minhighshare15to20poor minhighshare21to30poor"
global schoolfeatures "ttprivsec ttpubsec tttelesec ttclosestsec ttCONALEP ttCETA ttCETIS ttCEBTA ttCEBTIS ttprivhigh ttpubhigh ttclosesthigh"
global outcomes "highschool highschool03 highschool03fin moneyreason otherreason missing03 pcexp lnpcexp"
keep treatment pobre $outcomes $features97 $features98m $locationfeatures $schoolfeatures
drop fatherminhigh motherminhigh spendmoreon8 spendmoreon9 spendmoreon10 spendmoreon13 mayor cattlecommittee creditunion conscoop allsewers posttelegraph allgetinpreschool allgetinprimschool allgetintelesecondaria numbersecondary allgetinsecundary // drop these for lack of variation

export delimited using "$Progresa\Highschool_big.csv", replace




// restricted conditional sample
use "$Progresa\Highschool_big.dta", clear
keep if n041b == 3 & n041a == 2 // remove asterisk to get restricted sample
save "$Progresa\Highschool_small.dta", replace

// export to csv (for analysis with R)
global features97 "female age97 attend97 attend98 hhsize97 numunder15 fatherathome motherathome fatherlit motherlit fatherschool motherschool fatherminprim motherminprim fatherminsec motherminsec fatherminhigh motherminhigh verypoor poor notrich yycali mpcalif"
global features98m "goodatschool cansec canfurther canhigh canuni desgirlsec desgirlfurther desgirlhigh desgirluni desboysec desboyfurther desboyhigh desboyuni breakfast whynobreakfast talkwithteacher whytalkwithteacher guardian schoolwork problemdiscipline problemteachers problemcommunication problemteachattendance teacherprepared teacherfulfilled teacherpunctual teacherpatient helpsibgirl helpsibboy helpworkgirl helpworkboy earnmongirl earnmonboy weekexp1 weekexp2 weekexp3 weekexp4 weekexp5 monthexp1 monthexp2 monthexp3 sixmonthexp1 sixmonthexp2 sixmonthexp3 sixmonthexp4 sixmonthexp5 sixmonthexp6 sixmonthexp7 sixmonthexp8 sixmonthexp9 sixmonthexp10 sixmonthexp11 sixmonthexp12 spendmoreon1 spendmoreon2 spendmoreon3 spendmoreon4 spendmoreon5 spendmoreon6 spendmoreon7 spendmoreon8 spendmoreon9 spendmoreon10 spendmoreon11 spendmoreon12 spendmoreon13 spendmoreon14"
global locationfeatures "grado indice mayor delegate subdelegate commissioner commissary devcommittee healthcommittee educommittee agricommittee cattlecommittee DICONSA prodcoop creditunion conscoop church politorga parentassoc commassembly NGO commwork watersource trash allelectric partselectric allsewers partsewers phone posttelegraph numberpreschools allgetinpreschool numberprimschools allgetinprimschool numbertelesecondaria allgetintelesecondaria numbersecondary allgetinsecundary primeactivity secondactivity thirdactivity childlabor avchildwage indloc vilcount vilcountpoor rightagetotal rightageshare rightagepoortotal rightagepoorshare rightageattend97total rightageattend97share rightageattend97poortotal rightageattend97poorshare minprimtotal minprimtotal15to20 minprimtotal21to30 minprimshare minprimshare15to20 minprimshare21to30 minprimtotalpoor minprimtotal15to20poor minprimtotal21to30poor minprimsharepoor minprimshare15to20poor minprimshare21to30poor minsectotal minsectotal15to20 minsectotal21to30 minsecshare minsecshare15to20 minsecshare21to30 minsectotalpoor minsectotal15to20poor minsectotal21to30poor minsecsharepoor minsecshare15to20poor minsecshare21to30poor minhightotal minhightotal15to20 minhightotal21to30 minhighshare minhighshare15to20 minhighshare21to30 minhightotalpoor minhightotal15to20poor minhightotal21to30poor minhighsharepoor minhighshare15to20poor minhighshare21to30poor"
global schoolfeatures "ttprivsec ttpubsec tttelesec ttclosestsec ttCONALEP ttCETA ttCETIS ttCEBTA ttCEBTIS ttprivhigh ttpubhigh ttclosesthigh"
global outcomes "highschool highschool03 highschool03fin moneyreason otherreason missing03 pcexp lnpcexp"
keep treatment pobre $outcomes $features97 $features98m $locationfeatures $schoolfeatures
drop fatherminhigh motherminhigh spendmoreon8 spendmoreon9 spendmoreon10 spendmoreon13 mayor cattlecommittee creditunion conscoop allsewers posttelegraph allgetinpreschool allgetinprimschool allgetintelesecondaria numbersecondary allgetinsecundary // drop these for lack of variation

export delimited using "$Progresa\Highschool_small.csv", replace




// Make unconditional sample
use "$Progresa\Big Dataset", clear
gen agedif = w002cor - w046 if w046 <= 50 // age difference between current age and age when quit school
drop if p11 != p003 & p11 != . & p003 != . // delete observations with a sex change between November 1997 and March 1998
gen genderconsistent9703 = (p11 == s3_4 | s3_4 == .) // these observations are gender-consistent between November 1997 and (Winter) 2003
gen schoolinconsistent03 = s5_5_2 == 1 | s5_5_2 == 2 | s3_12_2 == 0 | s3_12_2 == 1 | (s3_12_2 == 2 & s3_12_1 != 6) | s3_12_1 == 0 // claims to have finished less than 6th grade primary school or is enrolled in preschool or primary school in winter 2003
gen schoolinconsistent00 = w041b == 1 | (w041b == 2 & w041a != 6) | w044b == 1 | w044b == 2 // claims to have finished less than 6th grade primary school or is enrolled in preschool or primary school in November 2000

gen agedif9700 = w002cor - p08
gen agedif9703 = s2_4 - p08 // for the highschool03 variable only

// middle school variables
gen finishedmiddle00 = w041a == 3 & w041b == 3
gen somemiddle00 = (w041a == 2 | w041a == 3) & w041b == 3
gen middleschool00 = finishedmiddle00 & (agedif9700 >= 2 & agedif9700 <= 4 & w002cor < 20) & schoolinconsistent00 != 1 if w041b != . // finished middle school in November 2000
gen somemiddleschool00 = somemiddle00 & (agedif9700 >= 2 & agedif9700 <= 4 & w002cor < 20) & schoolinconsistent00 != 1 if w041b != . // finished middle school or was in second last grade in November 2000
gen missingmiddle00 = middleschool00 == . // middle school outcomes missing
// high school variables
gen highschool = (w044b == 5 | w044b == 6) if agedif9700 >= 2 & agedif9700 <= 4 & !(w043 == . | w044b == 1 | w044b == 2 | w044b == 8 | w044b == 9)
gen highschool03 = 0 if s3_12_2 >= 3 & s3_12_2 <= 8 // highest school level in 2003 was high school, basica normal, or postgraduate
replace highschool03 = 1 if s3_12_2 == 4 | s3_12_2 == 5 | s3_12_2 == 8
replace highschool03 = . if (agedif9703 < 5 & agedif9703 != .) | (agedif9703 > 7 & agedif9703 != .) | genderconsistent9703 == 0 | schoolinconsistent03 == 1 // set to missing if age difference is greater than one year or in case of sex change between 1997 and 2003
gen highschool03fin = 0 if s3_12_2 >= 3 & s3_12_2 <= 8 
replace highschool03fin = 1 if ((s3_12_2 == 4 | s3_12_2 == 5) & s3_12_1 == 3) | s3_12_2 == 8 // completed high school by 2003 or ...
replace highschool03fin = 1 if s5_5_2 == 4 & (s5_5_1 == 3 | s5_5_1 == 4) // ... in last grade of high school in 2003
replace highschool03fin = . if (agedif9703 < 5 & agedif9703 != .) | (agedif9703 > 7 & agedif9703 != .) | genderconsistent9703 == 0 | schoolinconsistent03 == 1 // set to missing if age difference is greater than one year or in case of sex change between 1997 and 2003
gen missing00 = highschool == . // missing outcomes from November 2000 survey
gen missing03 = highschool03 == . // missing outcomes from 2003 survey

keep if rightage == 1
keep if (p21 == 1 | p006 == 1 | p008 == 0) & !(p012 <= 7) // Only consider those who have completed primary school in school year 1996/97. These must be those kids whose highest completed grade is the last grade 
														  // of primary school and who are either still enrolled in school (unless they took a break between primary and secondary school; to prevent this, I exclude 
														  // all those students who claim to have taken a break from school for 1 or more years) or who stopped going to school less than a year ago (by March 1998).  

save "$Progresa\Unconditional.dta", replace

// export for R
global features97 "female age97 attend97 attend98 hhsize97 numunder15 fatherathome motherathome fatherlit motherlit fatherschool motherschool fatherminprim motherminprim fatherminsec motherminsec fatherminhigh motherminhigh verypoor poor notrich yycali mpcalif"
global features98m "goodatschool cansec canfurther canhigh canuni desgirlsec desgirlfurther desgirlhigh desgirluni desboysec desboyfurther desboyhigh desboyuni breakfast whynobreakfast talkwithteacher whytalkwithteacher guardian schoolwork problemdiscipline problemteachers problemcommunication problemteachattendance teacherprepared teacherfulfilled teacherpunctual teacherpatient helpsibgirl helpsibboy helpworkgirl helpworkboy earnmongirl earnmonboy weekexp1 weekexp2 weekexp3 weekexp4 weekexp5 monthexp1 monthexp2 monthexp3 sixmonthexp1 sixmonthexp2 sixmonthexp3 sixmonthexp4 sixmonthexp5 sixmonthexp6 sixmonthexp7 sixmonthexp8 sixmonthexp9 sixmonthexp10 sixmonthexp11 sixmonthexp12 spendmoreon1 spendmoreon2 spendmoreon3 spendmoreon4 spendmoreon5 spendmoreon6 spendmoreon7 spendmoreon8 spendmoreon9 spendmoreon10 spendmoreon11 spendmoreon12 spendmoreon13 spendmoreon14"
global locationfeatures "grado indice mayor delegate subdelegate commissioner commissary devcommittee healthcommittee educommittee agricommittee cattlecommittee DICONSA prodcoop creditunion conscoop church politorga parentassoc commassembly NGO commwork watersource trash allelectric partselectric allsewers partsewers phone posttelegraph numberpreschools allgetinpreschool numberprimschools allgetinprimschool numbertelesecondaria allgetintelesecondaria numbersecondary allgetinsecundary primeactivity secondactivity thirdactivity childlabor avchildwage indloc"
global schoolfeatures "ttprivsec ttpubsec tttelesec ttclosestsec ttCONALEP ttCETA ttCETIS ttCEBTA ttCEBTIS ttprivhigh ttpubhigh ttclosesthigh"
global outcomes "highschool highschool03 highschool03fin missing00 missing03 middleschool00 somemiddleschool00 missingmiddle00"
keep treatment pobre $outcomes $features97 $features98m $locationfeatures $schoolfeatures
drop fatherminhigh motherminhigh spendmoreon8 spendmoreon9 spendmoreon10 spendmoreon13 mayor cattlecommittee creditunion conscoop allsewers posttelegraph allgetinpreschool allgetinprimschool allgetintelesecondaria numbersecondary allgetinsecundary // drop these for lack of variation

export delimited using "$Progresa\Unconditional.csv", replace


