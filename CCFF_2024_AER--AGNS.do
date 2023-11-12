********************************************************************************
* Replication "On Binscatter"
* Cattaneo, Crump, Farrell, and Feng (2024)
* Date: 10-NOV-2023
* AGNS Application
********************************************************************************

* ******************************************************************************
* INSTRUCTIONS
* 1. Set main directory below
* 2. Create folders "graphs" and "data/temp" in this directory
* ******************************************************************************

clear all
set scheme s2color
********************************************************************************
* ENTER APPROPRIATE DIRECTORY AND UNCOMMENT NEXT COMMAND
********************************************************************************
*global main ""
cd "$main"

use $main/CCFF_2024_AER--AGNS

********************************************************************************
* Figure 1(a) Scatter plot
********************************************************************************
scatter lnpat mtr90_lag3, graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") ylabel(, nogrid) xlabel(-0.9(0.3)0) 
graph export "graphs/AGNS_scatter.pdf", replace 
graph export "graphs/AGNS_scatter.png", replace as(png) width(800) height(600)

********************************************************************************
* Figure 1(b) Demonstration of scatter to binscatter
********************************************************************************
gen y = lnpat
gen x = mtr90_lag3
drop lnpat mtr90_lag3
save $main/data/temp/tmp_CCFF_2024_AGNS, replace
binsreg y x, nbins(10) line(0,0) nodraw savedata($main/data/temp/tmp1b) replace

use data/temp/tmp1b, clear
rename line_isknot knots_ind
rename dots_x x
rename dots_fit y
keep x y knots_ind line_x
gen brk=1

append using $main/data/temp/tmp_CCFF_2024_AGNS

bysort brk (x): replace knots_ind=1 if (_n==1 | _n==_N) & brk==.
bysort brk (x): replace line_x=x if (_n==1 | _n==_N) & brk==.
levelsof line_x if knots_ind==1, local(exes)

preserve

keep if knots_ind == 1
replace x = line_x
replace line_x = .
replace y = 4
replace brk = _n + 1
save data/temp/tmp1, replace

use data/temp/tmp1, clear
replace y = 8
save data/temp/tmp2, replace

restore

append using data/temp/tmp1
append using data/temp/tmp2

forvalues i = 2/12 {
	local binlines "`binlines' (line y x if brk == `i', color(black) lpattern(dash) lwidth(vthin))" 	
}

tw scatter y x if brk==. & inrange(x,-0.9,0) & inrange(y,4,8), color(bluishgray) msize(vsmall) || scatter y x if brk==1, mcolor(black) || `binlines', legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(4(2)8, nogrid) xlabel(-0.9(0.3)0) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate")
graph export "graphs/AGNS_ScatterAndBinscatter.pdf", replace
graph export "graphs/AGNS_ScatterAndBinscatter.png", replace as(png) width(800) height(600)

********************************************************************************
* Figure 1(c) Demonstration of conventional binscatter plot
********************************************************************************
use $main/CCFF_2024_AER--AGNS, clear

binsreg lnpat mtr90_lag3, nbins(10) xlabel(-0.9(0.3)0) plotxrange(-0.9,0) polyreg(1) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(4(2)8, nogrid) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") legend(on order(1 "Binscatter" 2 "Linear Fit") ring(0) pos(1) cols(1)) savedata(test) replace
graph export "graphs/AGNS_BinscatterAndLine.pdf", replace
graph export "graphs/AGNS_BinscatterAndLine.png", replace as(png) width(800) height(600)

********************************************************************************
* Figure 1(d) Demonstration of piecewise constant conditional mean estimate
********************************************************************************
binsreg lnpat mtr90_lag3, nbins(10) dots(F) line(0 0)  linegrid(1000) polyreg(1) lineplotopt(lcolor(black) lwidth(medthick)) polyregplotopt(lcolor(forest_green)) plotxrange(-0.9,0) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(4(2)8, nogrid nogextend) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") xlabel(-0.9(0.3)0)
graph export "graphs/AGNS_BinscatterAndCondExp.pdf", replace
graph export "graphs/AGNS_BinscatterAndCondExp.png", replace as(png) width(800) height(600)

********************************************************************************
* Figure 2 Conditioning on covariates
********************************************************************************

* Residualized Binscatter
areg lnpat top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 i.statenum [aw=pop1940], a(year)
predict resid_lnpat, res
egen mean_lnpat = mean(lnpat)
replace resid_lnpat = resid_lnpat + mean_lnpat

areg mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 i.statenum [aw=pop1940], a(year)
predict resid_mtr90_lag3, res
egen mean_mtr90_lag3 = mean(mtr90_lag3)
replace resid_mtr90_lag3 = resid_mtr90_lag3 + mean_mtr90_lag3

binsreg resid_lnpat resid_mtr90_lag3 [aw=pop1940], nbins(50) polyreg(1) dotsplotopt(mcolor(dkorange)) polyregplotopt(lcolor(black)) savedata($main/data/temp/tmpAGNS1binscatter) replace graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") ylabel(5.7(0.1)6.2, nogrid) xlabel(-0.525(0.05)-0.325)
graph export "graphs/AGNS_binscatter.pdf", replace
graph export "graphs/AGNS_binscatter.png", as(png) replace width(800) height(600)

binsreg resid_lnpat resid_mtr90_lag3 [aw=pop1940], nbins(50) polyreg(1) line(0 0) linegrid(1000) lineplotopt(lwidth(medthick) lcolor(dkorange) cmissing(y)) polyregplotopt(lcolor(black)) dotsplotopt(mcolor(dkorange) msize(small)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") ylabel(5.7(0.1)6.2, nogrid) xlabel(-0.525(0.05)-0.325)
graph export "graphs/AGNS_binscatter_pwc.pdf", replace
graph export "graphs/AGNS_binscatter_pwc.png", as(png) replace width(800) height(600)

* Correct handling of covariates (w/ regression line)
binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], nbins(50) polyreg(1) absorb(statenum year) replace at(mean) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") ylabel(, nogrid) xlabel(-0.9(0.3)0) 
graph export "graphs/AGNS_covariateAdjustments_binsreg_wRegLine.pdf", replace
graph export "graphs/AGNS_covariateAdjustments_binsreg_wRegLine.png", replace as(png) width(800) height(600)

* Correct handling of covariates (w/out regression line)
binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], nbins(50) absorb(statenum year) replace at(mean) dotsplotopt(mcolor(black)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") ylabel(, nogrid) xlabel(-0.9(0.3)0) 
graph export "graphs/AGNS_covariateAdjustments_binsreg_woutRegLine.pdf", replace
graph export "graphs/AGNS_covariateAdjustments_binsreg_woutRegLine.png", replace as(png) width(800) height(600)

* Put both approaches on same plot
binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3  [aw=pop1940], nbins(50) absorb(statenum year) at(mean) polyreg(1) savedata($main/data/temp/tmpAGNS1binsreg) replace

preserve

use $main/data/temp/tmpAGNS1binscatter, clear
gen binsreg = 0
append using $main/data/temp/tmpAGNS1binsreg
replace binsreg = 1 if missing(binsreg)

local new = _N + 2
set obs `new'
replace poly_x = -0.8 if _n == `new'-1
replace poly_x = 0 if _n == `new'
replace binsreg = 2 if _n >= `new'-1
gen poly_fit2 = .

reg poly_fit poly_x if binsreg==0
replace poly_fit2 =  _b[_cons] + _b[poly_x]*poly_x if binsreg == 2

tw (scatter dots_fit dots_x if binsreg==0, color(dkorange)) (line poly_fit poly_x if binsreg==0, color(black) lwidth(medthick)) (line poly_fit2 poly_x if binsreg==2, color(black) lpattern(dot) lwidth(medthick)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") ylabel(, nogrid) xlabel(-0.9(0.3)0) ylabel(5(.5)8) legend(off)
graph export "graphs/AGNS_covariateAdjustments_binscatter.pdf", replace
graph export "graphs/AGNS_covariateAdjustments_binscatter.png", replace as(png) width(800) height(600)

restore

********************************************************************************
* Figure 3: confidence bands
********************************************************************************

use $main/CCFF_2024_AER--AGNS, clear

/*
* Direct binsreg implementation of confidence band
binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], vce(cluster fiveyrblockbystate year) absorb(statenum year) randcut(1) cb(T) replace at(mean) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") ylabel(, nogrid) nsims(5000) simsgrid(200)
graph export "graphs/AGNS_confidenceBandwDots.pdf", replace
graph export "graphs/AGNS_confidenceBandwDots.png", as(png) replace 
*/

binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], vce(cluster fiveyrblockbystate year) absorb(statenum year) randcut(1) cb(T) at(mean) nodraw savedata($main/data/temp/tmp3) replace nsims(5000) simsgrid(200)

use $main/data/temp/tmp3, clear

tw (rarea CB_l CB_r CB_x, fcolor(edkblue) fintensity(20) lwidth(none)) (scatter dots_fit dots_x, mcolor(black)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) legend(off) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate")
graph export "graphs/AGNS_confidenceBandwDots.pdf", replace
graph export "graphs/AGNS_confidenceBandwDots.png", replace as(png) width(800) height(600)

qui summ CB_r if CB_x < -0.8
gen horline = `r(max)'

gen CB_avg = 0.4*CB_r+0.6*CB_l
reg CB_avg CB_x
predict inline, xb 

tw (rarea CB_l CB_r CB_x, fcolor(edkblue) fintensity(20) lwidth(none)) (line inline CB_x, lcolor(red)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) legend(off) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate")
graph export "graphs/AGNS_confidenceBandwLine.pdf", replace
graph export "graphs/AGNS_confidenceBandwLine.png", replace as(png) width(800) height(600)

tw (rarea CB_l CB_r CB_x, fcolor(edkblue) fintensity(20) lwidth(none)) (scatter dots_fit dots_x, mcolor(black)) (line horline CB_x, lcolor(black) lpattern(dash)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) legend(off) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate")
graph export "graphs/AGNS_confidenceBandwHorLine.pdf", replace
graph export "graphs/AGNS_confidenceBandwHorLine.png", replace as(png) width(800) height(600)

********************************************************************************
* NEW  Figure 4: choice of J
********************************************************************************
use $main/CCFF_2024_AER--AGNS, clear

* 5 bins -- too few for consistent estimation, interpret as deciles
binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], absorb(statenum year) nbins(5) at(mean) polyreg(1) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") ylabel(, nogrid) xlabel(-0.9(0.3)0) ylabel(6(1)9) 
graph export "graphs/AGNS_nbins5.pdf", replace
graph export "graphs/AGNS_nbins5.png", replace as(png) width(800) height(600)

* IMSE optimal
binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], absorb(statenum year) vce(cluster fiveyrblockbystate year) randcut(1) at(mean) polyreg(1) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") ylabel(, nogrid) xlabel(-0.9(0.3)0) ylabel(6(1)9) 
graph export "graphs/AGNS_nbinsOptimal.pdf", replace
graph export "graphs/AGNS_nbinsOptimal.png", replace as(png) width(800) height(600)

* 50 bins -- undersmoothed
binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], absorb(statenum year) nbins(50) at(mean)  polyreg(1) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") ylabel(, nogrid) xlabel(-0.9(0.3)0) ylabel(6(1)9) 
*xlabel(-0.9(0.3)0) ylabel(5(.5)8)
graph export "graphs/AGNS_nbins50.pdf", replace
graph export "graphs/AGNS_nbins50.png", replace as(png) width(800) height(600)

********************************************************************************
* Figure 5 Confidence Bands
********************************************************************************

/*
* Direct binsreg implementation of confidence band
binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], randcut(1) absorb(statenum year) nbins(5) at(mean) cb(0 0) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate") ylabel(, nogrid) nsims(5000) simsgrid(200)
*/

binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], randcut(1) absorb(statenum year) vce(cluster fiveyrblockbystate year) nbins(5) at(mean) cb(0 0) nodraw savedata($main/data/temp/tmp5) replace nsims(5000) simsgrid(200)

use $main/data/temp/tmp5, clear

gen horline = 7.055

tw (rarea CB_l CB_r CB_x, cmissing(n) fcolor(edkblue) fintensity(20) lwidth(none) ) (scatter dots_fit dots_x, mcolor(black)) (line horline CB_x, lcolor(black) lpattern(dash)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) legend(off) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate")
graph export "graphs/AGNS_fixedJconfidenceBandwDots.pdf", replace
graph export "graphs/AGNS_fixedJconfidenceBandwDots.png", replace as(png) width(800) height(600)

********************************************************************************
* Figure SA-1 Evaluation Point
********************************************************************************

use $main/CCFF_2024_AER--AGNS, clear

preserve
insobs 1
gen var1 = .
summ top_corp_lag3
replace var1 = `r(max)' if _n==_N
drop top_corp_lag3
rename var1 top_corp_lag3
gen var2 = .
summ lreal_gdp_pc
replace var2 = `r(max)' if _n==_N
drop lreal_gdp_pc
rename var2 lreal_gdp_pc
gen var3 = .
summ lpopulation_density
replace var3 = `r(max)' if _n==_N
drop lpopulation_density
rename var3 lpopulation_density
gen var4 = .
summ rd_credit_lag3
replace var4 = `r(max)' if _n==_N
drop rd_credit_lag3
rename var4 rd_credit_lag3
keep top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3
keep if _n == _N
save $main/data/temp/wmax, replace
restore

preserve
insobs 1
gen var1 = .
summ top_corp_lag3
replace var1 = `r(min)' if _n==_N
drop top_corp_lag3
rename var1 top_corp_lag3
gen var2 = .
summ lreal_gdp_pc
replace var2 = `r(min)' if _n==_N
drop lreal_gdp_pc
rename var2 lreal_gdp_pc
gen var3 = .
summ lpopulation_density
replace var3 = `r(min)' if _n==_N
drop lpopulation_density
rename var3 lpopulation_density
gen var4 = .
summ rd_credit_lag3
replace var4 = `r(min)' if _n==_N
drop rd_credit_lag3
rename var4 rd_credit_lag3
keep top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3
keep if _n == _N
save $main/data/temp/wmin, replace
restore

use $main/CCFF_2024_AER--AGNS, clear

binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], randcut(1) at($main/data/temp/wmin) polyreg(1) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) legend(off) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate")
graph export "graphs/AGNS_wControls_atWmin.pdf", replace
graph export "graphs/AGNS_wControls_atWmin.png", replace as(png) width(800) height(600)

binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], randcut(1) at($main/data/temp/wmax) polyreg(1) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) legend(off) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate")
graph export "graphs/AGNS_wControls_atWmax.pdf", replace
graph export "graphs/AGNS_wControls_atWmax.png", replace as(png) width(800) height(600)

binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], randcut(1) at(mean) polyreg(1) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ylabel(, nogrid nogextend) legend(off) ytitle("Log Patents") xtitle("Log 90th Percentile Marginal Net of Tax Rate")
graph export "graphs/AGNS_wControls_atWmean.pdf", replace
graph export "graphs/AGNS_wControls_atWmean.png", replace as(png) width(800) height(600)
