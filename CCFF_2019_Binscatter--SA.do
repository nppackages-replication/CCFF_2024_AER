********************************************************************************
* Replication "On Binscatter" -- Supplemental Appendix
* Cattaneo, Crump, Farrell and Feng (2019)
* Date: 20-MAR-2019
********************************************************************************
clear all

use dataACS.dta, replace

// Drop Puerto Rico observations
drop if (geoid2>=00600 & geoid2<=00799) | (geoid2>=00900 & geoid2<=00999) // for zipcode level
// Drop 2016 data
drop if year ~= 2017

********************************************************************************
* Make Gini coefficient plots by income
********************************************************************************

local giniControls "percentBachelorsEdu medianAge uninsuredRate percentHsEdu ueRate"

// binsreg
binsreg giniIndex medianHouseholdIncome, ///
  title("2017 Gini Index by Median Household Income (No Controls)", size(medium)) ///   
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off)
graph export graphs/giniNoControls.pdf, replace

binsreg giniIndex medianHouseholdIncome, cb(0,0) ///
  title("2017 Gini Index by Median Household Income (No Controls)", size(medium)) ///   
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off)
graph export graphs/giniNoControls_cb00.pdf, replace

binsreg giniIndex medianHouseholdIncome, cb(3,3) line(3,3) ///
  title("2017 Gini Index by Median Household Income (No Controls)", size(medium)) ///   
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off)
graph export graphs/giniNoControls_cb33.pdf, replace

binsreg giniIndex medianHouseholdIncome `giniControls', ///
  title("2017 Gini Index by Median Household Income (With Controls)", size(medium)) ///
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off)
graph export graphs/giniWithControls.pdf, replace

binsreg giniIndex medianHouseholdIncome `giniControls', cb(0,0) ///
  title("2017 Gini Index by Median Household Income (With Controls)", size(medium)) ///
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off)
graph export graphs/giniWithControls_cb00.pdf, replace

binsreg giniIndex medianHouseholdIncome `giniControls', cb(3,3) line(3,3) ///
  title("2017 Gini Index by Median Household Income (With Controls)", size(medium)) ///
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off)  
graph export graphs/giniWithControls_cb33.pdf, replace

// binscatter
binscatter giniIndex medianHouseholdIncome, ///
  title("2017 Gini Index by Median Household Income (No Controls)", size(medium)) ///   
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off)
graph export graphs/giniBinscatterNoControls.pdf, replace 

binscatter giniIndex medianHouseholdIncome, controls(`giniControls') ///
  title("2017 Gini Index by Median Household Income (With Controls)", size(medium)) ///
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off)  
graph export graphs/giniBinscatterWithControls.pdf, replace

********************************************************************************
* Internet access by income
********************************************************************************

local webControls "percentBachelorsEdu medianAge uninsuredRate percentHsEdu ueRate"
gen percentWeb = 100-percentNoWeb

// binsreg
binsreg percentWeb medianHouseholdIncome, ///
  title("2017 Percent With Internet Access by Median Household Income (No Controls)", size(small)) ///   
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") legend(off)
graph export graphs/webNoControls.pdf, replace

binsreg percentWeb medianHouseholdIncome, cb(0,0) ///
  title("2017 Percent With Internet Access by Median Household Income (No Controls)", size(small)) ///   
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") legend(off)
graph export graphs/webNoControls_cb00.pdf, replace

binsreg percentWeb medianHouseholdIncome, cb(3,3) line(3,3) ///
  title("2017 Percent With Internet Access by Median Household Income (No Controls)", size(small)) ///   
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") legend(off)
graph export graphs/webNoControls_cb33.pdf, replace

binsreg percentWeb medianHouseholdIncome `webControls', ///
  title("2017 Percent With Internet Access by Median Household Income (With Controls)", size(small)) ///   
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") legend(off)  
graph export graphs/webWithControls.pdf, replace

binsreg percentWeb medianHouseholdIncome `webControls', cb(0,0) ///
  title("2017 Percent With Internet Access by Median Household Income (With Controls)", size(small)) ///   
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") legend(off)  
graph export graphs/webWithControls_cb00.pdf, replace

binsreg percentWeb medianHouseholdIncome `webControls', cb(3,3) line(3,3) ///
  title("2017 Percent With Internet Access by Median Household Income (With Controls)", size(small)) ///   
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") legend(off)  
graph export graphs/webWithControls_cb33.pdf, replace

// binscatter
binscatter percentWeb medianHouseholdIncome, ///
  title("2017 Percent With Internet Access by Median Household Income (No Controls)", size(small)) ///   
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") legend(off)
graph export graphs/webBinscatterNoControls.pdf, replace 

binscatter percentWeb medianHouseholdIncome, controls(`webControls') ///
  title("2017 Percent With Internet Access by Median Household Income (With Controls)", size(small)) ///   
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") legend(off)  
graph export graphs/webBinscatterWithControls.pdf, replace



********************************************************************************
* Uninsured Rate by income
********************************************************************************

local unInsuredControls "percentBachelorsEdu medianAge percentHsEdu ueRate"

// binsreg
binsreg uninsuredRate perCapitaIncome, ///
  title("2017 Uninsured Rate by per Capita Income (No Controls)", size(small)) ///   
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off)
graph export graphs/unInsuredNoControls.pdf, replace

binsreg uninsuredRate perCapitaIncome, cb(0,0) ///
  title("2017 Uninsured Rate by per Capita Income (No Controls)", size(small)) ///   
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off)
graph export graphs/unInsuredNoControls_cb00.pdf, replace

binsreg uninsuredRate perCapitaIncome, cb(3,3) line(3,3) ///
  title("2017 Uninsured Rate by per Capita Income (No Controls)", size(small)) ///   
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off)
graph export graphs/unInsuredNoControls_cb33.pdf, replace

binsreg uninsuredRate perCapitaIncome `unInsuredControls', ///
  title("2017 Uninsured Rate by per Capita Income (With Controls)", size(small)) ///   
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off)  
graph export graphs/unInsuredWithControls.pdf, replace

binsreg uninsuredRate perCapitaIncome `unInsuredControls', cb(0,0) ///
  title("2017 Uninsured Rate by per Capita Income (With Controls)", size(small)) ///   
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off)  
graph export graphs/unInsuredWithControls_cb00.pdf, replace

binsreg uninsuredRate perCapitaIncome `unInsuredControls', cb(3,3) line(3,3) ///
  title("2017 Uninsured Rate by per Capita Income (With Controls)", size(small)) ///   
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off)  
graph export graphs/unInsuredWithControls_cb33.pdf, replace

// binscatter
binscatter uninsuredRate perCapitaIncome, ///
  title("2017 Uninsured Rate by per Capita Income (No Controls)", size(small)) ///   
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off)
graph export graphs/unInsuredBinscatterNoControls.pdf, replace 

binscatter uninsuredRate perCapitaIncome, controls(`unInsuredControls') ///
  title("2017 Uninsured Rate by per Capita Income (With Controls)", size(small)) ///   
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off)  
graph export graphs/unInsuredBinscatterWithControls.pdf, replace


********************************************************************************
*Unemployment by income
********************************************************************************
/*
qui summ ueRate, de
drop if ueRate >= r(p95) | ueRate == 0
*/

local phillipsControls "percentBachelorsEdu medianAge percentHsEdu uninsuredRate"

// binsreg
binsreg medianHouseholdIncomeGrowth ueRate, ///
  title("2017 Unemployment Rate by Household Income Growth (No Controls)", size(small)) ///   
  ytitle("Household Income Growth") ///
  xtitle("Unemployment Rate") legend(off)
graph export graphs/phillipsNoControls.pdf, replace

binsreg medianHouseholdIncomeGrowth ueRate, cb(0,0) ///
  title("2017 Unemployment Rate by Household Income Growth (No Controls)", size(small)) ///   
  ytitle("Household Income Growth") ///
  xtitle("Unemployment Rate") legend(off)
graph export graphs/phillipsNoControls_cb00.pdf, replace

binsreg medianHouseholdIncomeGrowth ueRate, cb(3,3) line(3,3) ///
  title("2017 Unemployment Rate by Household Income Growth (With Controls)", size(small)) ///   
  ytitle("Household Income Growth") ///
  xtitle("Unemployment Rate") legend(off)  
graph export graphs/phillipsNoControls_cb33.pdf, replace

binsreg medianHouseholdIncomeGrowth  ueRate `phillipsControls', ///
  title("2017 Unemployment Rate by Household Income Growth (With Controls)", size(small)) ///   
  ytitle("Household Income Growth") ///
  xtitle("Unemployment Rate") legend(off)  
graph export graphs/phillipsWithControls.pdf, replace

binsreg medianHouseholdIncomeGrowth ueRate `phillipsControls', cb(0,0) ///
  title("2017 Unemployment Rate by Household Income Growth (With Controls)", size(small)) ///   
  ytitle("Household Income Growth") ///
  xtitle("Unemployment Rate") legend(off)  
graph export graphs/phillipsWithControls_cb00.pdf, replace

binsreg medianHouseholdIncomeGrowth ueRate `phillipsControls', cb(3,3) line(3,3) ///
  title("2017 Unemployment Rate by Household Income Growth (With Controls)", size(small)) ///   
  ytitle("Household Income Growth") ///
  xtitle("Unemployment Rate") legend(off)  
graph export graphs/phillipsWithControls_cb33.pdf, replace

// binscatter
binscatter medianHouseholdIncomeGrowth ueRate, ///
  title("2017 Unemployment Rate by Household Income Growth (No Controls)", size(small)) ///   
  ytitle("Household Income Growth") ///
  xtitle("Unemployment Rate") legend(off)
graph export graphs/phillipsBinscatterNoControls.pdf, replace 

binscatter medianHouseholdIncomeGrowth ueRate, controls(`phillipsControls') ///
  title("2017 Unemployment Rate by Household Income Growth (With Controls)", size(small)) ///   
  ytitle("Household Income Growth") ///
  xtitle("Unemployment Rate") legend(off)  
graph export graphs/phillipsBinscatterWithControls.pdf, replace

graph close _all

