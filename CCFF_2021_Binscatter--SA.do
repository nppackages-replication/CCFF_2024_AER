********************************************************************************
* Replication "On Binscatter" -- Supplemental Appendix
* Cattaneo, Crump, Farrell and Feng (2021)
* Date: 05-AUG-2021
********************************************************************************
clear all

use "CCFF_2021_Binscatter--SA.dta", replace

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
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off) ///
  dotsplotopt(mcolor(blue)) ///
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)
graph export graphs/giniNoControls.pdf, replace

binsreg giniIndex medianHouseholdIncome, cb(1,1) ci(1,1) ///
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off) ///
  dotsplotopt(mcolor(blue)) ciplotopt(lcolor(blue)) ///
  cbplotopt(fcolor(blue%75) fintensity(20) lwidth(none)) ///
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)
graph export graphs/giniNoControls_cb11.pdf, replace

binsreg giniIndex medianHouseholdIncome, cb(3,3) line(3,3) ///
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off) ///
  dotsplotopt(mcolor(blue)) ciplotopt(lcolor(blue)) ///
  lineplotopt(lcolor(blue)) ///
  cbplotopt(fcolor(blue%75) fintensity(20) lwidth(none)) ///  
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)
graph export graphs/giniNoControls_cb33.pdf, replace

binsreg giniIndex medianHouseholdIncome `giniControls', ///
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off) ///
  dotsplotopt(mcolor(blue)) ///  
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/giniWithControls.pdf, replace

binsreg giniIndex medianHouseholdIncome `giniControls', cb(1,1) ci(1,1) ///
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off) ///
  dotsplotopt(mcolor(blue)) ciplotopt(lcolor(blue)) /// 
  cbplotopt(fcolor(blue%75) fintensity(20) lwidth(none)) ///    
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/giniWithControls_cb11.pdf, replace

binsreg giniIndex medianHouseholdIncome `giniControls', cb(3,3) line(3,3) ///
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off) ///
  dotsplotopt(mcolor(blue)) ciplotopt(lcolor(blue)) ///
  lineplotopt(lcolor(blue)) ///  
  cbplotopt(fcolor(blue%75) fintensity(20) lwidth(none)) ///  
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/giniWithControls_cb33.pdf, replace

// binscatter
binscatter giniIndex medianHouseholdIncome, ///
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off) ///
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/giniBinscatterNoControls.pdf, replace 

binscatter giniIndex medianHouseholdIncome, controls(`giniControls') ///
  xtitle("Median Household Income") ///
  ytitle("Gini Index") legend(off) ///
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/giniBinscatterWithControls.pdf, replace

********************************************************************************
* Internet access by income
********************************************************************************

local webControls "percentBachelorsEdu medianAge uninsuredRate percentHsEdu ueRate"
local webControlsNoAge "percentBachelorsEdu uninsuredRate percentHsEdu ueRate"
gen percentWeb = 100-percentNoWeb
gen under40 = (medianAge < 40) if !missing(medianAge)

// binsreg
binsreg percentWeb medianHouseholdIncome, ///
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") legend(off) ///
  dotsplotopt(mcolor(blue)) ///  
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/webNoControls.pdf, replace

binsreg percentWeb medianHouseholdIncome, by(under40) ///
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") ///
  bysymbols(O S) bycolors(blue green) ///
  legend(order(2 "Under 40" 1 "40 and Above") position(0) bplacement(seast) cols(1)) ///  
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/webNoControls_byAge.pdf, replace

binsreg percentWeb medianHouseholdIncome, by(under40) cb(3,3) line(3,3) ///
  plotxrange(25000,150000) xlabel(25000[25000]150000) ///
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") ///
  bysymbols(O S) bycolors(blue green) bylpatterns(solid dot) ///
  legend(order(5 "Under 40" 2 "40 and Above") position(0) bplacement(seast) cols(1)) ///    
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/webNoControls_byAge_cb33.pdf, replace

binsreg percentWeb medianHouseholdIncome `webControls', ///
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") legend(off) ///
  dotsplotopt(mcolor(blue)) ///
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/webWithControls.pdf, replace

binsreg percentWeb medianHouseholdIncome `webControlsNoAge', by(under40) ///
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") ///  
  bysymbols(O S) bycolors(blue green) ///  
  legend(order(2 "Under 40" 1 "40 and Above") position(0) bplacement(seast) cols(1)) ///    
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/webWithControls_byAge.pdf, replace

binsreg percentWeb medianHouseholdIncome `webControlsNoAge', by(under40) cb(3,3) line(3,3) ///
  plotxrange(25000,150000) xlabel(25000[25000]150000) ///
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") ///
  bysymbols(O S) bycolors(blue green) bylpatterns(solid dot) ///  
  legend(order(5 "Under 40" 2 "40 and Above") position(0) bplacement(seast) cols(1)) ///      
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/webWithControls_byAge_cb33.pdf, replace

// binscatter
binscatter percentWeb medianHouseholdIncome, by(under40) ///
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") ///
  legend(order(2 "Under 40" 1 "40 and Above")) ///
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/webBinscatterNoControls.pdf, replace 

binscatter percentWeb medianHouseholdIncome, by(under40) controls(`webControls') ///
  xtitle("Median Household Income") ///
  ytitle("% With Internet Access") ///
  legend(order(2 "Under 40" 1 "40 and Above")) ///
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/webBinscatterWithControls.pdf, replace

********************************************************************************
* Uninsured Rate by income
********************************************************************************

local unInsuredControls "percentBachelorsEdu medianAge percentHsEdu ueRate"

sort perCapitaIncome

// binsreg
binsreg uninsuredRate perCapitaIncome, ///
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off) ///
  dotsplotopt(mcolor(blue)) ///    
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)  
graph export graphs/unInsuredNoControls.pdf, replace

binsreg uninsuredRate perCapitaIncome, ci(3,3) polyreg(3) polyregplotopt(lcolor(orange)) plotxrange(0, 100000) ///
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate")  ///
  dotsplotopt(mcolor(blue)) ciplotopt(lcolor(blue)) ///
  lineplotopt(lcolor(blue)) ///  
  legend(on order(2 "Cubic Fit" ) position(0) bplacement(neast) cols(1)) ///
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)    
graph export graphs/unInsuredNoControls_ci33.pdf, replace

binsreg uninsuredRate perCapitaIncome, cb(3,3) line(3,3) plotxrange(0, 100000) ///
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off) ///
  dotsplotopt(mcolor(blue)) ciplotopt(lcolor(blue)) /// 
  cbplotopt(fcolor(blue%75) fintensity(20) lwidth(none)) ///      
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)    
graph export graphs/unInsuredNoControls_cb33.pdf, replace

binsreg uninsuredRate perCapitaIncome `unInsuredControls', ///
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off) ///
  dotsplotopt(mcolor(blue)) ///    
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)    
graph export graphs/unInsuredWithControls.pdf, replace

binsreg uninsuredRate perCapitaIncome `unInsuredControls', ci(3,3) polyreg(3) polyregplotopt(lcolor(orange)) plotxrange(0, 100000) ///
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") ///
  dotsplotopt(mcolor(blue)) ciplotopt(lcolor(blue)) ///
  lineplotopt(lcolor(blue)) ///  
  legend(on order(2 "Cubic Fit" ) position(0) bplacement(neast) cols(1)) ///  
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)    
graph export graphs/unInsuredWithControls_ci33.pdf, replace

binsreg uninsuredRate perCapitaIncome `unInsuredControls', cb(3,3) line(3,3) plotxrange(0, 100000) ///
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off) ///
  dotsplotopt(mcolor(blue)) ciplotopt(lcolor(blue)) /// 
  cbplotopt(fcolor(blue%75) fintensity(20) lwidth(none)) ///      
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)    
graph export graphs/unInsuredWithControls_cb33.pdf, replace

// binscatter
binscatter uninsuredRate perCapitaIncome, ///
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off) ///
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)    
graph export graphs/unInsuredBinscatterNoControls.pdf, replace 

binscatter uninsuredRate perCapitaIncome, controls(`unInsuredControls') ///
  xtitle("per Capita Income") ///
  ytitle("Uninsured Rate") legend(off) ///
  graphregion(color(white) margin(large)) ///
  plotregion(lcolor(black)) ylabel(, nogrid)   
graph export graphs/unInsuredBinscatterWithControls.pdf, replace


graph close _all


