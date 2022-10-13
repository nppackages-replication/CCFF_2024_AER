********************************************************************************
* Replication "On Binscatter"
* Cattaneo, Crump, Farrell and Feng (2022)
* Date: 13-OCT-2022
* ACS Application
********************************************************************************

clear all
* ******************************************************
* ENTER APPROPRIATE DIRECTORY AND UNCOMMENT NEXT COMMAND
* ******************************************************
*global main ""
cd "$main"

use CCFF_2022_ACS_1, clear
keep y x uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate

********************************************************************************
* Figure 1(a) Plain scatter
********************************************************************************
scatter uninsuredRate perCapitaIncome, color(navy) msize(vsmall) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) ytitle("Percent Uninsured") xtitle("Per Capita Income")
graph export "graphs/Census_Scatter.pdf", replace
graph export "graphs/Census_Scatter.png", replace

********************************************************************************
* Figure 1(b) Demonstration of scatter to binscatter
********************************************************************************
binsreg y x, nbins(10) line(0,0) nodraw savedata($main/data/temp/tmp1b) replace

use data/temp/tmp1b, clear
rename line_isknot knots_ind
rename dots_x x
rename dots_fit y
keep x y knots_ind line_x
gen br=1

append using CCFF_2022_ACS_1

bysort br (x): replace knots_ind=1 if (_n==1 | _n==_N) & br==.
bysort br (x): replace line_x=x if (_n==1 | _n==_N) & br==.
levelsof line_x if knots_ind==1, local(exes)

preserve

keep if knots_ind == 1
replace x = line_x
replace line_x = .
replace y = -1
replace br = _n + 1
save data/temp/tmp1, replace

use data/temp/tmp1, clear
replace y = 41
save data/temp/tmp2, replace

restore

append using data/temp/tmp1
append using data/temp/tmp2

forvalues i = 2/11 {
	local binlines "`binlines' (line y x if br == `i', color(black) lpattern(dash) lwidth(vthin))" 	
}

tw scatter y x if br==. & inrange(x,0,80000) & inrange(y,0,40), color(bluishgray) msize(vsmall) || scatter y x if br==1, mcolor(black) || `binlines', legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid) ytitle("Percent Uninsured") xtitle("Per Capita Income")
graph export "graphs/Census_ScatterAndBinscatter.pdf", replace
graph export "graphs/Census_ScatterAndBinscatter.png", replace

********************************************************************************
* Figure 1(c) Demonstration of conventional binscatter plot
********************************************************************************
binsreg y x , nbins(10) xlabel(0(20000)80000) plotxrange(0,80000) polyreg(1) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(0(10)40, nogrid) ytitle("Percent Uninsured") xtitle("Per Capita Income") legend(on order(1 "Binscatter" 2 "Linear Fit") ring(0) pos(1) cols(1))
graph export "graphs/Census_BinscatterAndLine.pdf", replace
graph export "graphs/Census_BinscatterAndLine.png", replace

********************************************************************************
* Figure 1(d) Demonstration of piecewise constant conditional mean estimate
********************************************************************************
use CCFF_2022_ACS_1, clear
binsreg y x, nbins(10) dots(F) line(0 0)  linegrid(1000) polyreg(1) lineplotopt(lcolor(black) lwidth(medthick)) polyregplotopt(lcolor(forest_green)) plotxrange(0,80000) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(0(10)40, nogrid nogextend) ytitle("Percent Uninsured") xtitle("Per Capita Income") xlabel(0(20000)80000)
graph export "graphs/Census_BinscatterAndCondExp.pdf", replace
graph export "graphs/Census_BinscatterAndCondExp.png", replace

********************************************************************************
* Figure 1(e) Confidence band
********************************************************************************
use CCFF_2022_ACS_1, clear
binsreg uninsuredRate perCapitaIncome, randcut(1) cb(T) plotxrange(0 80000) dotsplotopt(mcolor(black)) cbplotopt(fcolor(edkblue) fintensity(20) lwidth(none)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(0(10)40, nogrid nogextend) ytitle("Percent Uninsured") xtitle("Per Capita Income") legend(off)
graph export "graphs/Census_CB_noControls.pdf", replace
graph export "graphs/Census_CB_noControls.png", replace

********************************************************************************
* Figure 1(f) Conditional quantiles 90/50/10
********************************************************************************
use CCFF_2022_ACS_1, clear
binsqreg y x, quantile(.1) nodraw savedata($main/data/temp/tmp1f1_10) replace
binsqreg y x, quantile(.5) nodraw savedata($main/data/temp/tmp1f1_50) replace
binsqreg y x, quantile(.9) nodraw savedata($main/data/temp/tmp1f1_90) replace

use $main/data/temp/tmp1f1_10, clear
gen q = 10
append using $main/data/temp/tmp1f1_50
replace q = 50 if missing(q)
append using $main/data/temp/tmp1f1_90
replace q = 90 if missing(q)

tw scatter dots_fit dots_x  if q == 10, mcolor(gs9) mlcolor(black) || scatter dots_fit dots_x  if q == 50, mcolor(black) ||  scatter dots_fit dots_x  if q == 90, mcolor(gs9) mlcolor(black) legend(order(1 "10th" 2 "50th" 3 "90th") row(1) ring(0) position(1)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid) ytitle("Percent Uninsured") xtitle("Per Capita Income")
graph export "graphs/Census_CQ_noControls.pdf", replace
graph export "graphs/Census_CQ_noControls.png", replace

********************************************************************************
* Figure 2 Conditioning on covariates
********************************************************************************
use CCFF_2022_ACS_1, clear
/*
binscatter y x, controls(percentBachelorsEdu medianAge percentHsEdu ueRate) linetype(none) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) ytitle("Percent Uninsured") xtitle("Per Capita Income") xlabel(0(20000)80000)
*/

reg y percentBachelorsEdu medianAge percentHsEdu ueRate
predict resid_y, res
egen mean_y = mean(y)
replace resid_y = resid_y + mean_y

reg x percentBachelorsEdu medianAge percentHsEdu ueRate
predict resid_x, res
egen mean_x = mean(x)
replace resid_x = resid_x + mean_x

binsreg resid_y resid_x percentBachelorsEdu medianAge percentHsEdu ueRate, nbins(20) polyreg(1) dotsplotopt(mcolor(dkorange)) polyregplotopt(lcolor(black))  plotxrange(0,80000) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(8(1)13, nogrid nogextend) ytitle("Percent Uninsured") xtitle("Per Capita Income") xlabel(0(20000)80000)
graph export "graphs/Census_binscatter_withControls.pdf", replace
graph export "graphs/Census_binscatter_withControls.png", replace

binsreg y x percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) polyreg(1) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) plotxrange(0,80000) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(8(1)13, nogrid nogextend) ytitle("Percent Uninsured") xtitle("Per Capita Income") xlabel(0(20000)80000)
graph export "graphs/Census_binsreg_withControls.pdf", replace
graph export "graphs/Census_binsreg_withControls.png", replace

********************************************************************************
* Figure 6 Logit specification
********************************************************************************
gen idxHighUninsured = (uninsuredRate >= 10)

scatter idxHighUninsured perCapitaIncome, mcolor(navy) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) ytitle(" ") xtitle("Per Capita Income") 
graph export "graphs/Census_scatterLDV.pdf", replace
graph export "graphs/Census_scatterLDV.png", replace

binslogit idxHighUninsured perCapitaIncome, cb(T) randcut(1) dotsplotopt(mcolor(black))cbplotopt(fcolor(edkblue) fintensity(20) lwidth(none)) plotxrange(0 100000) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(0(.2)1, nogrid nogextend) xtitle("Per Capita Income") ytitle(" ")
graph export "graphs/Census_binslogit.pdf", replace
graph export "graphs/Census_binslogit.png", replace

binslogit idxHighUninsured perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, cb(T) dots(0 0) randcut(1) dotsplotopt(mcolor(black))cbplotopt(fcolor(edkblue) fintensity(20) lwidth(none)) plotxrange(0 100000) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(0(.2)1, nogrid nogextend)  xtitle("Per Capita Income") ytitle(" ")
graph export "graphs/Census_binslogit_withControls.pdf", replace
graph export "graphs/Census_binslogit_withControls.png", replace

********************************************************************************
* Figure 4 Parametric fits versus confidence bands
********************************************************************************

* Do with and without controls
binsreg uninsuredRate perCapitaIncome, randcut(1) cb(T) polyreg(3) nodraw savedata($main/data/temp/tmp3a1) replace
binsreg uninsuredRate perCapitaIncome, randcut(1) cb(T) polyreg(1) nodraw savedata($main/data/temp/tmp3a2) replace

binsreg uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) cb(T) polyreg(3) nodraw savedata($main/data/temp/tmp3b1) replace
binsreg uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) cb(T) polyreg(1) nodraw savedata($main/data/temp/tmp3b2) replace

use $main/data/temp/tmp3a1, clear
append using $main/data/temp/tmp3a2, generate(source)

tw (rarea CB_l CB_r CB_x if source == 0 & inrange(CB_x,0,80000), fcolor(edkblue) fintensity(20) lwidth(none)) (scatter dots_fit dots_x if source == 0 & inrange(dots_x,0,80000), mcolor(black))  (line poly_fit poly_x  if source == 0 & inrange(poly_x,0,80000), color(forest_green)) (line poly_fit poly_x  if source == 1 & inrange(poly_x,0,80000), color(forest_green) lpattern(dash)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) xtitle("Per Capita Income") ytitle("Percent Uninsured") legend(order(4 "Linear Fit" 3 "Cubic Fit"))
graph export "graphs/Census_binsreg_withParametricFit.pdf", replace
graph export "graphs/Census_binsreg_withParametricFit.png", replace

use $main/data/temp/tmp3b1, clear
append using $main/data/temp/tmp3b2, generate(source)

tw (rarea CB_l CB_r CB_x if source == 0 & inrange(CB_x,0,80000), fcolor(edkblue) fintensity(20) lwidth(none)) (scatter dots_fit dots_x if source == 0 & inrange(dots_x,0,80000), mcolor(black))  (line poly_fit poly_x  if source == 0 & inrange(poly_x,0,80000), color(forest_green)) (line poly_fit poly_x  if source == 1 & inrange(poly_x,0,80000), color(forest_green) lpattern(dash)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) xtitle("Per Capita Income") ytitle("Percent Uninsured") legend(order(4 "Linear Fit" 3 "Cubic Fit"))
graph export "graphs/Census_binsreg_withParametricFit_withControls.pdf", replace
graph export "graphs/Census_binsreg_withParametricFit_withControls.png", replace

********************************************************************************
* Figure 5 Two Sample Results
********************************************************************************

use CCFF_2022_ACS_2, clear

binsreg uninsuredRate  perCapitaIncome , by(idxpopdens) cb(T) randcut(1) plotxrange(0 80000) dotsplotopt(msize(small)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) ytitle("Percent Uninsured") xtitle("Per Capita Income") legend(order(1 "High Pop. Density States" 3 "Low Pop. Density States")) ylabel(0(10)30, nogrid nogextend)
graph export "graphs/Census_binsreg_byPopDensity.pdf", replace
graph export "graphs/Census_binsreg_byPopDensity.png", replace

binsreg uninsuredRate  perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, by(idxpopdens) cb(T) randcut(1) plotxrange(0 80000) dotsplotopt(msize(small)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) ytitle("Percent Uninsured") xtitle("Per Capita Income") legend(order(1 "High Pop. Density States" 3 "Low Pop. Density States")) ylabel(0(10)30, nogrid nogextend)
graph export "graphs/Census_binsreg_byPopDensity_withControls.pdf", replace
graph export "graphs/Census_binsreg_byPopDensity_withControls.png", replace


********************************************************************************
* Figures 3 and Table 1
********************************************************************************

use CCFF_2022_ACS_1, clear

* Make controls evaluation points
summ percentBachelorsEdu medianAge percentHsEdu ueRate, de

preserve
clear
set obs 1
gen percentBachelorsEdu = 0
gen medianAge = 3.9 
gen percentHsEdu = 0 
gen ueRate = 0
save $main/data/temp/controls_min, replace
restore

preserve
clear
set obs 1
gen percentBachelorsEdu = 100
gen medianAge = 89.5 
gen percentHsEdu = 100 
gen ueRate = 100
save $main/data/temp/controls_max, replace
restore

********************************************************************************
* Figure 3 Binned Scatter Plots with Parametric Fit
********************************************************************************

binsreg uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) polyreg(1) at($main/data/temp/controls_max) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) plotxrange(0,80000) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) ytitle("Percent Uninsured") xtitle("Per Capita Income") xlabel(0(20000)80000)
graph export "graphs/Census_binsreg_withControls_atMax.pdf", replace
graph export "graphs/Census_binsreg_withControls_atMax.png", replace

binsreg uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) polyreg(1) at($main/data/temp/controls_min) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) plotxrange(0,80000) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) ytitle("Percent Uninsured") xtitle("Per Capita Income") xlabel(0(20000)80000)
graph export "graphs/Census_binsreg_withControls_atMin.pdf", replace
graph export "graphs/Census_binsreg_withControls_atMin.png", replace

binsreg uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) polyreg(1) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) plotxrange(0,80000) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(8(1)13, nogrid nogextend) ytitle("Percent Uninsured") xtitle("Per Capita Income") xlabel(0(20000)80000)
graph export "graphs/Census_binsreg_withControls_atMean.pdf", replace
graph export "graphs/Census_binsreg_withControls_atMean.png", replace

********************************************************************************
* Table 1 Tests: Parametric Specifications
********************************************************************************

* No controls
binstest uninsuredRate perCapitaIncome, randcut(1) testmodelpoly(1) nsims(50000)
binstest uninsuredRate perCapitaIncome, randcut(1) testmodelpoly(1) lp(2) nsims(50000)
binstest uninsuredRate perCapitaIncome, randcut(1) testmodelpoly(3) nsims(50000)
binstest uninsuredRate perCapitaIncome, randcut(1) testmodelpoly(3) lp(2) nsims(50000)

* With controls
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(1) at($main/data/temp/controls_min)  nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(1) at($main/data/temp/controls_min) lp(2)  nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(3) at($main/data/temp/controls_min) nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(3) at($main/data/temp/controls_min) lp(2) nsims(50000)

binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(1) at(mean) nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(1) at(mean) lp(2) nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(3) at(mean) nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(3) at(mean) lp(2) nsims(50000)

binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(1) at($main/data/temp/controls_max) nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(1) at($main/data/temp/controls_max) lp(2) nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(3) at($main/data/temp/controls_max) nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(3) at($main/data/temp/controls_max) lp(2) nsims(50000)

binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(1) deriv(1) nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(1) lp(2) deriv(1) nsims(50000)

binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(3) deriv(1) nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testmodelpoly(3) lp(2) deriv(1) nsims(50000)

********************************************************************************
* Table 1 Tests: Shape Restrictions
********************************************************************************

binstest uninsuredRate perCapitaIncome, randcut(1) testshapel(0) deriv(1) nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testshapel(0) deriv(1) nsims(50000)

binstest uninsuredRate perCapitaIncome, randcut(1) testshaper(0) deriv(2) nsims(50000)
binstest uninsuredRate perCapitaIncome percentBachelorsEdu medianAge percentHsEdu ueRate, randcut(1) testshaper(0) deriv(2) lp(2) nsims(50000)





