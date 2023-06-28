********************************************************************************
* Replication "On Binscatter"
* Cattaneo, Crump, Farrell and Feng (2023)
* M Application
********************************************************************************

clear all
set maxvar 120000
* ******************************************************
* ENTER APPROPRIATE DIRECTORY AND UNCOMMENT NEXT COMMAND
* ******************************************************
*global main ""
cd "$main"

use CCFF_2023_AER--M1, clear
sort x

********************************************************************************
* Figure 8(a) Scatter plot
********************************************************************************
scatter y x, msize(tiny) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("log Number of Patents per Inventor per Year") xtitle("log Cluster Size") ylabel(, nogrid) xlabel(-11(2)-1) 
graph export "graphs/M_scatter.pdf", replace
graph export "graphs/M_scatter.png", replace

********************************************************************************
* Figures 2(e), 2(f), 8(b), 8(c), 8(d) Conditioning on covariates
********************************************************************************
reg y i.bea i.year i.zd
predict resid_y, res
egen mean_y = mean(y)
replace resid_y = resid_y + mean_y

reg x i.bea i.year i.zd
predict resid_x, res
egen mean_x = mean(x)
replace resid_x = resid_x + mean_x

binsreg resid_y resid_x, nbins(40) polyreg(1) dotsplotopt(mcolor(dkorange)) polyregplotopt(lcolor(black)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("log Number of Patents per Inventor per Year") xtitle("log Cluster Size") ylabel(, nogrid) xlabel(-5.5(1)-2.5) plotxrange(-5.5 -2.5) plotyrange(-.35 -.15) ylabel(-.35(.1)-.15, nogrid)
graph export "graphs/M_binscatter.pdf", replace
graph export "graphs/M_binscatter.png", replace

binsreg resid_y resid_x, nbins(40) polyreg(1) line(0 0) linegrid(1000) lineplotopt(lwidth(medthick) lcolor(dkorange) cmissing(y)) polyregplotopt(lcolor(black)) dotsplotopt(mcolor(dkorange) msize(small)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("log Number of Patents per Inventor per Year") xtitle("log Cluster Size") ylabel(, nogrid) xlabel(-5.5(1)-2.5) plotxrange(-5.5 -2.5) plotyrange(-.35 -.15) ylabel(-.35(.1)-.15, nogrid)
graph export "graphs/M_binscatter_pwc.pdf", replace
graph export "graphs/M_binscatter_pwc.png", replace

binsreg resid_y resid_x, nbins(40) polyreg(1) savedata($main/data/temp/tmpM1binscatter) replace dotsplotopt(mcolor(dkorange)) polyregplotopt(lcolor(black)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("log Number of Patents per Inventor per Year") xtitle("log Cluster Size") ylabel(-.5(.1)0, nogrid) xlabel(-8(1)-1) plotxrange(-8 -1) plotyrange(-.5 0)

binsreg y x, nbins(40) absorb(bea year zd) polyreg(1) savedata($main/data/temp/tmpM1binsreg) replace dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("log Number of Patents per Inventor per Year") xtitle("log Cluster Size") ylabel(-.5(.1)0, nogrid) xlabel(-8(1)-1) plotxrange(-8 -1) plotyrange(-.5 0)
graph export "graphs/M_covariateAdjustments_binsreg.pdf", replace
graph export "graphs/M_covariateAdjustments_binsreg.png", replace


preserve

use $main/data/temp/tmpM1binscatter, clear
gen binsreg = 0
append using $main/data/temp/tmpM1binsreg
replace binsreg = 1 if missing(binsreg)

tw (scatter dots_fit dots_x if binsreg==0, color(dkorange)) (scatter dots_fit dots_x if binsreg==1, color(black)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("log Number of Patents per Inventor per Year") xtitle("log Cluster Size") ylabel(, nogrid) legend(order(1 "Residualized" 2 "Semi-linear" ) row(1) ring(0) position(11)) xlabel(-11(1)-1)
graph export "graphs/M_covariateAdjustments.pdf", replace
graph export "graphs/M_covariateAdjustments.png", replace

local new = _N + 2
set obs `new'
qui summ dots_x if binsreg==0
replace poly_x = `r(min)' if _n == `new'-1
replace poly_x = `r(max)' if _n == `new'
replace binsreg = 2 if _n >= `new'-1
gen poly_fit2 = .

reg poly_fit poly_x if binsreg == 0
replace poly_fit2 =  _b[_cons] + _b[poly_x]*poly_x if binsreg == 2

tw (scatter dots_fit dots_x if binsreg==0, color(dkorange)) (line poly_fit poly_x if binsreg==0, color(black) lpattern(dot) lwidth(medthick)) (line poly_fit2 poly_x if binsreg==2, color(black) lwidth(medthick)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("log Number of Patents per Inventor per Year") xtitle("log Cluster Size") ylabel(-.5(.1)0, nogrid) xlabel(-8(1)-1) legend(off)
graph export "graphs/M_covariateAdjustments_binscatter.pdf", replace
graph export "graphs/M_covariateAdjustments_binscatter.png", replace

restore

********************************************************************************
* Figure 8(e) Optimal Choice of J
********************************************************************************
binsreg y x, randcut(1) absorb(bea year zd) graphregion(color(white) margin(large)) plotregion(lcolor(black)) dotsplotopt(mcolor(black)) ytitle("log Number of Patents per Inventor per Year") xtitle("log Cluster Size") ylabel(, nogrid) plotxrange(-8 -1) xlabel(-8(1)-1)
graph export "graphs/M_binsregOptimal.pdf", replace
graph export "graphs/M_binsregOptimal.png", replace

********************************************************************************
* Figure 8(f) Derivative Estimation
********************************************************************************
use CCFF_2023_AER--M2, clear

* Same setup as Spec (8) in Table 3 with deriv(1)
binsreg y x, absorb(year bea zd class cluster1 cluster_bea_class cluster_zd_year cluster_class_year inventor cluster_bea_year org_new) vce(cluster cluster1) randcut(1) deriv(1) cb(T) cbplotopt(fcolor(edkblue) fintensity(20) lwidth(none)) dotsplotopt(mcolor(black)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("log Number of Patents per Inventor per Year") xtitle("log Cluster Size") ylabel(, nogrid) yline(0, lcolor(black) lwidth(vthin) lpattern(dash))
graph export "graphs/M_binsregDeriv.pdf", replace
graph export "graphs/M_binsregDeriv.png", replace

********************************************************************************
* [Not in Paper]: Example of how evaluation point affects inference
********************************************************************************
binsreg y x i.bea i.year i.zd, randcut(1) cb(T) vce(cluster cluster1)
graph export "graphs/M_ex1.pdf", replace
graph export "graphs/M_ex1.png", replace

binsreg y x , randcut(1) cb(T) vce(cluster cluster1) absorb(bea year zd)
graph export "graphs/M_ex2.pdf", replace
graph export "graphs/M_ex2.png", replace

