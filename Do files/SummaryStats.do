*** Show summary statistics
*** Author: Martin Wiegand
*** Last changed: 24.10.2020



// set global
clear
set more off
global Progresa "Directory\Progresa" // change this to Progresa directory
set seed 12345

// summary statistics for conditional unrestricted sample; the bootstrap is
// to obtain the correct p-value (Wild cluster bootstrap)

use "$Progresa\Highschool_big.dta", clear

// eligible (poor)
ttest highschool if pobre, by(treatment)
qui reg highschool treatment if pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest highschool03 if pobre, by(treatment)
qui reg highschool03 treatment if pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest highschool03fin if pobre, by(treatment)
qui reg highschool03fin treatment if pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

// non-eligible (non-poor)
ttest highschool if !pobre, by(treatment)
qui reg highschool treatment if !pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest highschool03 if !pobre, by(treatment)
qui reg highschool03 treatment if !pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest highschool03fin if !pobre, by(treatment)
qui reg highschool03fin treatment if !pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)



// summary statistics for conditional restricted sample; the bootstrap is
// to obtain the correct p-value (Wild cluster bootstrap)

use "$Progresa\Highschool_small.dta", clear

// eligible (poor)
ttest highschool if pobre, by(treatment)
qui reg highschool treatment if pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest highschool03 if pobre, by(treatment)
qui reg highschool03 treatment if pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest highschool03fin if pobre, by(treatment)
qui reg highschool03fin treatment if pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

// non-eligible (non-poor)
ttest highschool if !pobre, by(treatment)
qui reg highschool treatment if !pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest highschool03 if !pobre, by(treatment)
qui reg highschool03 treatment if !pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest highschool03fin if !pobre, by(treatment)
qui reg highschool03fin treatment if !pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)



// summary statistics for unconditional sample; the bootstrap is
// to obtain the correct p-value (Wild cluster bootstrap)

use "$Progresa\Unconditional.dta", clear

// eligible (poor)
ttest highschool if pobre, by(treatment)
qui reg highschool treatment if pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest highschool03 if pobre, by(treatment)
qui reg highschool03 treatment if pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest highschool03fin if pobre, by(treatment)
qui reg highschool03fin treatment if pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest middleschool00 if pobre, by(treatment)
qui reg middleschool00 treatment if pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest somemiddleschool00 if pobre, by(treatment)
qui reg somemiddleschool00 treatment if pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

// non-eligible (non-poor)
ttest highschool if !pobre, by(treatment)
qui reg highschool treatment if !pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest highschool03 if !pobre, by(treatment)
qui reg highschool03 treatment if !pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest highschool03fin if !pobre, by(treatment)
qui reg highschool03fin treatment if !pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest middleschool00 if !pobre, by(treatment)
qui reg middleschool00 treatment if !pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)

ttest somemiddleschool00 if !pobre, by(treatment)
qui reg somemiddleschool00 treatment if !pobre
boottest treatment, cluster(claveofi) weighttype(mammen) reps(99999)



