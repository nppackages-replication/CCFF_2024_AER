********************************************************************************
* Replication "On Binscatter"
* Cattaneo, Crump, Farrell, and Feng (2024)
* Date: 05-JAN-2024
* M Application
********************************************************************************

* ******************************************************************************
* INSTRUCTIONS
* 0. Ensure all necessary packages are installed: run CCFF_2024_configStata.do
* 1. Set main directory below
* 2. Create folders "graphs" and "data/temp" in this directory
* ******************************************************************************

* ******************************************************************************
* NOTE
* This code produces finalized graphs for the paper using a large number of 
* simulations (nsims) and gridpoints (simsgrid); code can be sped up substantially
* by restoring default choices for these two options.
* ******************************************************************************

clear all
set maxvar 120000
set scheme s2color
********************************************************************************
* ENTER APPROPRIATE DIRECTORY AND UNCOMMENT NEXT COMMAND
********************************************************************************
*global main ""
cd "$main"

use $main/CCFF_2024_AER--M1, clear
sort x
********************************************************************************
* Figure 6(a) Scatter plot
********************************************************************************
scatter y x, msize(tiny) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log Cluster Size") ylabel(, nogrid) xlabel(-11(2)-1) 
graph export "graphs/M_scatter.pdf", replace
graph export "graphs/M_scatter.png", replace as(png) width(800) height(600)

********************************************************************************
* Figure 6(b)--6(d) Conditioning on covariates
********************************************************************************
reg y i.bea i.year i.zd
predict resid_y, res
egen mean_y = mean(y)
replace resid_y = resid_y + mean_y

reg x i.bea i.year i.zd
predict resid_x, res
egen mean_x = mean(x)
replace resid_x = resid_x + mean_x

binsreg resid_y resid_x, nbins(40) polyreg(1) dotsplotopt(mcolor(dkorange)) polyregplotopt(lcolor(black)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log Cluster Size") ylabel(, nogrid) xlabel(-5.5(1)-2.5) plotxrange(-5.5 -2.5) plotyrange(-.35 -.15) ylabel(-.35(.1)-.15, nogrid)
graph export "graphs/M_binscatter.pdf", replace
graph export "graphs/M_binscatter.png", replace as(png) width(800) height(600)

binsreg resid_y resid_x, nbins(40)line(0 0) linegrid(1000) lineplotopt(lwidth(medthick) lcolor(dkorange) cmissing(y)) polyregplotopt(lcolor(black)) dotsplotopt(mcolor(dkorange) msize(small)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log Cluster Size") ylabel(, nogrid) xlabel(-5.5(1)-2.5) plotxrange(-5.5 -2.5) plotyrange(-.35 -.15) ylabel(-.35(.1)-.15, nogrid)
graph export "graphs/M_binscatter_pwc.pdf", replace
graph export "graphs/M_binscatter_pwc.png", replace as(png) width(800) height(600)

binsreg resid_y resid_x, nbins(40) polyreg(1) savedata($main/data/temp/tmpM1binscatter) replace dotsplotopt(mcolor(dkorange)) polyregplotopt(lcolor(black)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log Cluster Size") ylabel(-.5(.1)0, nogrid) xlabel(-8(1)-1) plotxrange(-8 -1) plotyrange(-.5 0)

binsreg y x, nbins(40) absorb(bea year zd) polyreg(1) savedata($main/data/temp/tmpM1binsreg) replace dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log Cluster Size") ylabel(-.5(.1)0, nogrid) xlabel(-8(1)-1) plotxrange(-8 -1) plotyrange(-.5 0)

preserve

use $main/data/temp/tmpM1binscatter, clear
gen binsreg = 0
append using $main/data/temp/tmpM1binsreg
replace binsreg = 1 if missing(binsreg)

tw (scatter dots_fit dots_x if binsreg==0, color(dkorange)) (scatter dots_fit dots_x if binsreg==1, color(black)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log Cluster Size") ylabel(, nogrid) legend(order(1 "Residualized" 2 "Semi-linear" ) row(1) ring(0) position(11)) xlabel(-11(1)-1)
graph export "graphs/M_covariateAdjustments.pdf", replace
graph export "graphs/M_covariateAdjustments.png", replace as(png) width(800) height(600)

local new = _N + 2
set obs `new'
qui summ dots_x if binsreg==0
replace poly_x = `r(min)' if _n == `new'-1
replace poly_x = `r(max)' if _n == `new'
replace binsreg = 2 if _n >= `new'-1
gen poly_fit2 = .

reg poly_fit poly_x if binsreg == 0
replace poly_fit2 =  _b[_cons] + _b[poly_x]*poly_x if binsreg == 2

tw (scatter dots_fit dots_x if binsreg==0, color(dkorange)) (line poly_fit poly_x if binsreg==0, color(black) lpattern(dot) lwidth(medthick)) (line poly_fit2 poly_x if binsreg==2, color(black) lwidth(medthick)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log Cluster Size") ylabel(-.5(.1)0, nogrid) xlabel(-8(1)-1) legend(off)
*graph export "graphs/M_covariateAdjustments_binscatter.pdf", replace
*graph export "graphs/M_covariateAdjustments_binscatter.png", replace as(png) width(800) height(600)

restore

********************************************************************************
* Figure 6(e) Optimal Choice of J/Confidence Band
********************************************************************************
use $main/CCFF_2024_AER--M2, clear
sort x

binsreg y x, absorb(year bea zd) vce(cluster cluster1) randcut(1) cb(T) cbplotopt(fcolor(edkblue) fintensity(20) lwidth(none)) dotsplotopt(mcolor(black)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log Cluster Size") ylabel(, nogrid) nsims(5000) simsgrid(200)
graph export "graphs/M_confidenceBand.pdf", replace
graph export "graphs/M_confidenceBand.png", replace  as(png) width(800) height(600)

********************************************************************************
* Figure 6(f) Optimal Choice of J/Confidence Band (full specification)
********************************************************************************

binsreg y x, absorb(year bea zd class cluster1 cluster_bea_class cluster_zd_year cluster_class_year inventor cluster_bea_year org_new) vce(cluster cluster1) randcut(1) cb(T) cbplotopt(fcolor(edkblue) fintensity(20) lwidth(none)) dotsplotopt(mcolor(black)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log Cluster Size") ylabel(, nogrid) nsims(5000) simsgrid(200)
graph export "graphs/M_confidenceBandFullSpec.pdf", replace
graph export "graphs/M_confidenceBandFullSpec.png", replace  as(png) width(800) height(600)




