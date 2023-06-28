********************************************************************************
* Replication "On Binscatter"
* Cattaneo, Crump, Farrell and Feng (2023)
* AGNS Application
********************************************************************************

clear all
* ******************************************************
* ENTER APPROPRIATE DIRECTORY AND UNCOMMENT NEXT COMMAND
* ******************************************************
*global main ""
cd "$main"

use $main/CCFF_2023_AGNS

********************************************************************************
* Figure 7(a) Scatter plot
********************************************************************************
scatter lnpat mtr90_lag3, graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Combined Marginal Tax Rate for 90th Percentile") ylabel(, nogrid) xlabel(-0.9(0.3)0) 
graph export "graphs/AGNS_scatter.pdf", replace
graph export "graphs/AGNS_scatter.png", replace

********************************************************************************
* Figures 2(c), 2(d), 7(b), 7(c), 7(d) Conditioning on covariates
********************************************************************************
areg lnpat top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 i.statenum [aw=pop1940], a(year)
predict resid_lnpat, res
egen mean_lnpat = mean(lnpat)
replace resid_lnpat = resid_lnpat + mean_lnpat

areg mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 i.statenum [aw=pop1940], a(year)
predict resid_mtr90_lag3, res
egen mean_mtr90_lag3 = mean(mtr90_lag3)
replace resid_mtr90_lag3 = resid_mtr90_lag3 + mean_mtr90_lag3

binsreg resid_lnpat resid_mtr90_lag3 [aw=pop1940], nbins(50) randcut(1) polyreg(1) dotsplotopt(mcolor(dkorange)) polyregplotopt(lcolor(black)) savedata($main/data/temp/tmpAGNS1binscatter) replace graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Combined Marginal Tax Rate for 90th Percentile") ylabel(5.7(0.1)6.2, nogrid) xlabel(-0.525(0.05)-0.325)
graph export "graphs/AGNS_binscatter.pdf", replace
graph export "graphs/AGNS_binscatter.png", replace

binsreg resid_lnpat resid_mtr90_lag3 [aw=pop1940], nbins(50) polyreg(1) line(0 0) linegrid(1000) lineplotopt(lwidth(medthick) lcolor(dkorange) cmissing(y)) polyregplotopt(lcolor(black)) dotsplotopt(mcolor(dkorange) msize(small)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Combined Marginal Tax Rate for 90th Percentile") ylabel(5.7(0.1)6.2, nogrid) xlabel(-0.525(0.05)-0.325)
graph export "graphs/AGNS_binscatter_pwc.pdf", replace
graph export "graphs/AGNS_binscatter_pwc.png", replace

binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 i.statenum i.year [aw=pop1940], randcut(1) nbins(50) polyreg(1) replace at(0) dotsplotopt(mcolor(black)) polyregplotopt(lcolor(forest_green)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Combined Marginal Tax Rate for 90th Percentile") ylabel(, nogrid) xlabel(-0.9(0.3)0) 
graph export "graphs/AGNS_covariateAdjustments_binsreg.pdf", replace
graph export "graphs/AGNS_covariateAdjustments_binsreg.png", replace

binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 i.statenum i.year [aw=pop1940], randcut(1) nbins(50) polyreg(1) savedata($main/data/temp/tmpAGNS1binsreg) replace

preserve

use $main/data/temp/tmpAGNS1binscatter, clear
gen binsreg = 0
append using $main/data/temp/tmpAGNS1binsreg
replace binsreg = 1 if missing(binsreg)

tw (scatter dots_fit dots_x if binsreg==0, color(dkorange)) (scatter dots_fit dots_x if binsreg==1, mcolor(black)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Combined Marginal Tax Rate for 90th Percentile") ylabel(, nogrid) legend(order(1 "Residualized" 2 "Semi-linear") row(1) ring(0) position(11)) xlabel(-0.9(0.3)0)  
graph export "graphs/AGNS_covariateAdjustments.pdf", replace
graph export "graphs/AGNS_covariateAdjustments.png", replace

local new = _N + 2
set obs `new'
replace poly_x = -0.8 if _n == `new'-1
replace poly_x = 0 if _n == `new'
replace binsreg = 2 if _n >= `new'-1
gen poly_fit2 = .

reg poly_fit poly_x if binsreg==0
replace poly_fit2 =  _b[_cons] + _b[poly_x]*poly_x if binsreg == 2

tw (scatter dots_fit dots_x if binsreg==0, color(dkorange)) (line poly_fit poly_x if binsreg==0, color(black) lwidth(medthick)) (line poly_fit2 poly_x if binsreg==2, color(black) lpattern(dot) lwidth(medthick)), graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Combined Marginal Tax Rate for 90th Percentile") ylabel(, nogrid) xlabel(-0.9(0.3)0) ylabel(5(.5)8) legend(off)
graph export "graphs/AGNS_covariateAdjustments_binscatter.pdf", replace
graph export "graphs/AGNS_covariateAdjustments_binscatter.png", replace

restore

********************************************************************************
* Figure 7(e) Optimal Choice of J
********************************************************************************
binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 i.statenum i.year  [aw=pop1940],  randcut(1) dotsplotopt(mcolor(black)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Combined Marginal Tax Rate for 90th Percentile") ylabel(, nogrid) xlabel(-0.9(0.3)0)
graph export "graphs/AGNS_binsregOptimal.pdf", replace
graph export "graphs/AGNS_binsregOptimal.png", replace

********************************************************************************
* [Not in Paper]: Example of binsqreg
********************************************************************************
binsqreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 i.statenum i.year  [pw=pop1940],  randcut(1) qregopt(iterate(1000))
graph export "graphs/AGNS_binsqreg.pdf", replace
graph export "graphs/AGNS_binsqreg.png", replace

********************************************************************************
* Figure 7(f) Derivative Estimation
********************************************************************************

binsreg lnpat  mtr90_lag3 top_corp_lag3 lreal_gdp_pc lpopulation_density rd_credit_lag3 [aw=pop1940], randcut(1) cb(T) deriv(1) absorb(statenum year) vce(cluster fiveyrblockbystate year) cbplotopt(fcolor(edkblue) fintensity(20) lwidth(none)) dotsplotopt(mcolor(black)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Log Patents") xtitle("Combined Marginal Tax Rate for 90th Percentile") ylabel(, nogrid) xlabel(-0.9(0.3)0) yline(0, lcolor(black) lwidth(vthin) lpattern(dash))
graph export "graphs/AGNS_binsregDeriv.pdf", replace
graph export "graphs/AGNS_binsregDeriv.png", replace

