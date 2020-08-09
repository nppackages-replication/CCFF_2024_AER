********************************************************************************
* Replicate the figures in "On Binscatter"
* Cattaneo, Crump, Farrell and Feng (2019)
* Date: 20-MAR-2019
********************************************************************************
clear all
capture log close
capture program drop _all
capture macro drop _all
drop _all
set more off

set scheme s1color
********************************************************************************
* Gen Datasets *****************************************************************
********************************************************************************
set obs 1000
set seed 1234

gen x = rbeta(2,4)
gen e = rnormal(0,0.5)
gen w = runiform(-1,1)
gen w_corr = 3*(x-0.5) + runiform(-0.5,0.5)
gen mu_x = ((0.54)-(1.11*x)+(2.81*x^2)-(2.47*x^3)+(0.6*x^4))*40 
gen y = mu_x + e + w
gen y_corr = mu_x + e + w_corr 
sort x
save data1, replace

clear
set obs 1000

gen x = rbeta(2,4)
gen e = rnormal(0,0.5)
gen w = runiform(-1,1)
gen w_corr = 3*(x-0.5) + runiform(-0.5,0.5)
gen mu_x = ((0.54-(1.11*x)+(2.81*x^2)-(2.47*x^3)+(0.6*x^4))*40) + 1
gen y = mu_x + e + w
gen y_corr = mu_x + e + w_corr
save data2, replace

********************************************************************************
* Figure 1: The Basic Construction of a Binned Scatter Plot ********************
********************************************************************************
use data1, clear

binsreg y x, nbins(10) line(0,0) nodraw savedata(fig1_data) replace

use fig1_data, clear
rename line_isknot knots_ind
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y knots_ind line_x
gen br=1

append using data1

preserve
bysort br (x): replace knots_ind=1 if (_n==1 | _n==_N) & br==.
bysort br (x): replace line_x=x if (_n==1 | _n==_N) & br==.
levelsof line_x if knots_ind==1, local(exes)

tw scatter y x if br==. & inrange(y,14,22), color(bluishgray) || scatter y x if br==1, xline(`exes', lwidth(thin) lcolor(black) lpattern(dash) lstyle(grid)) xlabel(none) ylabel(none) color(blue) legend(off) name(Fig1a)

tw lfit y x if br==. & inrange(y,14,22), lcolor(black) || scatter y x if br==1, color(blue) xlabel(none) ylabel(none) legend(label(1 "linear fit") label(2 "binscatter")) ytitle("y") name(Fig1b)
restore

********************************************************************************
* Figure 2: Scatter and Binscatter Pots with Different Variability *************
********************************************************************************

*** Original Variance ***
tw scatter y x if br==. & inrange(y,14,22), color(bluishgray) || scatter y x if br==1, xlabel(none) ylabel(none) color(blue) legend(off) name(Fig2a)

levelsof line_x if knots_ind==1, local(exes)
gen ind_bin=.
gen marker=.
forval i = 1/9 {
	local marker : word `i' of `exes'
	di "`marker'"
	replace marker = `marker'
	replace ind_bin = `i' if x<= `marker' & ind_bin>`i' & br==.
}
replace ind_bin=10 if ind_bin==. & br==.

bysort ind_bin : egen y_bin_mean = mean(y) if br==.

gen y_high_var = y + 1 *(y - y_bin_mean) if br==.
gen y_mod_hsk = y + 1.5*((ind_bin - 2.5)^2/50)*(y - y_bin_mean) if br==.
gen y_high_hsk = y + 10*((abs(ind_bin - 3.5) - 4.5)^2/50)*(y - y_bin_mean) if br==.

save fig2_data, replace

binsreg y_high_var x, nbins(10) nodraw savedata(fig2b) replace
binsreg y_mod_hsk x, nbins(10) nodraw savedata(fig2c) replace
binsreg y_high_hsk x, nbins(10) nodraw savedata(fig2d) replace

*** Greater Variance ***
use fig2b, clear
rename dots_x x
rename dots_fit y_high_var
lab var x "x"
lab var y_high_var "y"
keep x y_high_var
gen br=1

append using fig2_data

tw scatter y_high_var x if br==. & inrange(y_high_var,14,22), color(bluishgray) || scatter y_high_var x if br==1, xlabel(none) ylabel(none) color(blue) legend(off) name(Fig2b)

*** Moderate Heteroskedasticity ***
use fig2c, clear
rename dots_x x
rename dots_fit y_mod_hsk
lab var x "x"
lab var y_mod_hsk "y"
keep x y_mod_hsk
gen br=1

append using fig2_data

tw scatter y_mod_hsk x if br==. & inrange(y_mod_hsk,13,25), color(bluishgray) || scatter y_mod_hsk x if br==1, xlabel(none) ylabel(none) color(blue) legend(off) name(Fig2c)

*** Greater Heteroskedasticity ***
use fig2d, clear
rename dots_x x
rename dots_fit y_high_hsk
lab var x "x"
lab var y_high_hsk "y"
keep x y_high_hsk
gen br=1

append using fig2_data

tw scatter y_high_hsk x if br==. & inrange(y_high_hsk,13,25), color(bluishgray) || scatter y_high_hsk x if br==1, xlabel(none) ylabel(none) color(blue) legend(off) name(Fig2d)

********************************************************************************
* Figure 3: The Actual Regressogram Nonparametric Estimator Corresponding to a Binned Scatter Plot
********************************************************************************
use data1, clear

binsreg y x, dotsgrid(1000) nbins(10) nodraw savedata(fig3_reg) replace
binsreg y x, nbins(10) nodraw savedata(fig3_bin) replace

use fig3_reg, clear
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y
gen br=2
save fig3_reg.dta, replace

use fig3_bin, clear
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y knots_ind
gen br=1

append using data1
append using fig3_reg

tw scatter y x if br==. & inrange(y,14,22), color(bluishgray) || scatter y x if br==1, color(blue) legend(off) || scatter y x if br==2, xlabel(none) ylabel(none) color(blue) msymbol(p) name(Fig3)

********************************************************************************
* Figure 4: Comparison of Covariate Adjustment Approaches **********************
********************************************************************************

*** X correlated with W ***
use data1, clear
reg y_corr w_corr
predict resid_y, residuals
egen mean_y = mean(y_corr)
replace resid_y = resid_y + mean_y

reg(x w_corr)
predict resid_x, residuals
egen mean_x = mean(x)
replace resid_x = resid_x + mean_x

binsreg resid_y resid_x w_corr, dotsgrid(1000) nbins(10) nodraw savedata(fig4a_resid_reg) replace
binsreg resid_y resid_x w_corr, nbins(10) nodraw savedata(fig4a_resid) replace

binsreg y x w_corr, dotsgrid(1000) nbins(10) nodraw savedata(fig4a_cov_reg) replace 
binsreg y x w_corr, nbins(10) nodraw savedata(fig4a_cov) replace

use fig4a_resid.dta, clear
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y
gen br=4
save fig4a_resid.dta, replace 

use fig4a_resid_reg.dta, clear
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y knots_ind
gen br=3
save fig4a_resid_reg.dta, replace

use fig4a_cov_reg.dta, clear
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y
gen br=2
save fig4a_cov_reg.dta, replace

use fig4a_cov.dta, clear
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y knots_ind
gen br=1

append using data1.dta
append using fig4a_cov_reg.dta
append using fig4a_resid.dta
append using fig4a_resid_reg.dta

sort x
tw line mu_x x if br==., color(black) msymbol(p) || scatter y x if br==1, color(blue) ///
|| scatter y x if br==2, color(blue) msymbol(p) || scatter y x if br==4, msymbol(S) color(red) || scatter y x if br==3, ytitle("y") xlabel(none) ylabel(none) color(red) msymbol(p) legend(order(1 "true f." 2 "cov adj." 4 "resid")) name(Fig4a)


*** X independent of W ***
use data1, clear

reg(y w)
predict resid_y, residuals
egen mean_y = mean(y)
replace resid_y = resid_y + mean_y

reg(x w)
predict resid_x, residuals
egen mean_x = mean(x)
replace resid_x = resid_x + mean_x

binsreg resid_y resid_x, dotsgrid(1000) nbins(10) nodraw savedata(fig4b_resid_reg) replace
binsreg resid_y resid_x, nbins(10) nodraw savedata(fig4b_resid) replace

binsreg y x w, dotsgrid(1000) nbins(10) nodraw savedata(fig4b_cov_reg) replace 
binsreg y x w, nbins(10) nodraw savedata(fig4b_cov) replace

use fig4b_resid.dta, clear
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y
gen br=4
save fig4b_resid.dta, replace

use fig4b_resid_reg.dta, clear
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y knots_ind
gen br=3
save fig4b_resid_reg.dta, replace

use fig4b_cov_reg.dta, clear
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y
gen br=2
save fig4b_cov_reg.dta, replace

use fig4b_cov.dta, clear
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y knots_ind
gen br=1

append using data1
append using fig4b_cov_reg
append using fig4b_resid.dta
append using fig4b_resid_reg.dta

sort x
tw line mu_x x if br==., color(black) msymbol(p) || scatter y x if br==1, color(blue) ///
|| scatter y x if br==2, color(blue) msymbol(p) || scatter y x if br==4, msymbol(S) color(red) ///
|| scatter y x if br==3, xlabel(none) ylabel(none) color(red) msymbol(p) ///
legend(order(1 "true f." 2 "cov adj." 4 "resid")) ytitle("y") name(Fig4b)


********************************************************************************
* Figure 5: Binscatter Generalizations *****************************************
********************************************************************************

*** Figure 5a ***
use data1, clear
macro drop _all
binsreg y x, line(1 0) nbins(10) nodraw savedata(fig5a) replace

use fig5a, clear
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
rename line_x line_x
rename line_fit line_y
lab var x "x"
lab var y "y"
keep x y knots_ind line_x line_y line_isknot
gen br=1

sort line_x

append using data1.dta

levelsof line_x if line_isknot==1, local(exes)
foreach i of local exes {
	local xlines = "`xlines' `i'"
}

local nbins: word count(`xlines')
local xlines_first: word 1 of `xlines'
local xlines_last: word 9 of `xlines'

local line = "line line_y line_x if br==1 & line_x <= `xlines_first', lcolor(blue) || "

local nbins_less = 10 - 1
forval i = 2/`nbins_less' {
local i_less = `i' - 1
local j: word `i_less' of `xlines'
local k: word `i' of `xlines'
local line = "`line' line line_y line_x if br==1 & line_x<=`k' & line_x>=`j', lcolor(blue) || "

}
local line = "`line' line line_y line_x if br==1 & line_x>`xlines_last', lcolor(blue)"

tw scatter y x if br==1, mcolor(blue) || `line' || line mu_x x if br==., xlabel(none) ylabel(none) lcolor(black) legend(off) ytitle("y") name(Fig5a)

*** Figure 5b ***
use data1, clear
binsreg y x, line(1 1) nbins(10) nodraw savedata(fig5b) replace

use fig5b, clear
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
rename line_x line_x
rename line_fit line_y
lab var x "x"
lab var y "y"

keep x y knots_ind line_x line_y

gen br=1
sort line_x

append using data1

tw scatter y x if br==1, mcolor(blue) || line line_y line_x if br==1, lcolor(blue) || line mu_x x if br==., xlabel(none) ylabel(none) ytitle("y") lcolor(black) lwidth(vthin) legend(off) name(Fig5b)

*** Figure 5c ***
use data1, clear
macro drop _all
binsreg y x, line(2 0) nbins(10) nodraw savedata(fig5c) replace

use fig5c, clear
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
rename line_x line_x
rename line_fit line_y
lab var x "x"
lab var y "y"

keep x y knots_ind line_x line_y line_isknot

gen br=1
sort line_x

append using data1

levelsof line_x if line_isknot==1, local(exes)
foreach i of local exes {
	local xlines = "`xlines' `i'"
}

local nbins: word count(`xlines')
local xlines_first: word 1 of `xlines'
local xlines_last: word 9 of `xlines'

local line = "line line_y line_x if br==1 & line_x <= `xlines_first', lcolor(blue) || "

local nbins_less = 10 - 1
forval i = 2/`nbins_less' {
local i_less = `i' - 1
local j: word `i_less' of `xlines'
local k: word `i' of `xlines'
local line = "`line' line line_y line_x if br==1 & line_x<=`k' & line_x>=`j', lcolor(blue) || "

}
local line = "`line' line line_y line_x if br==1 & line_x>`xlines_last', lcolor(blue)"

tw scatter y x if br==1, mcolor(blue) || `line' || line mu_x x if br==., xlabel(none) ylabel(none) lcolor(black) legend(off) ytitle("y") name(Fig5c)

*** Figure 5d ***
use data1, clear
binsreg y x, line(2 1) nbins(10) nodraw savedata(fig5d) replace

use fig5d, clear
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
rename line_x line_x
rename line_fit line_y
lab var x "x"
lab var y "y"

keep x y knots_ind line_x line_y

gen br=1
sort line_x

append using data1

tw scatter y x if br==1, mcolor(blue) || line line_y line_x if br==1, lcolor(blue) || line mu_x x if br==., xlabel(none) ylabel(none) lcolor(black) lwidth(vthin) legend(off) ytitle("y") name(Fig5d)

********************************************************************************
* Figure 6: Number of Bins (J) *************************************************
********************************************************************************

*** Figure 6a ***
use data1, clear

binsreg y x, dotsgrid(1000) nbins(10) nodraw savedata(fig6a_reg) replace
binsreg y x, nbins(10) nodraw savedata(fig6a_bins) replace

use fig6a_reg, clear
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y

gen br=2
save fig6a_reg, replace

use fig6a_bins, clear
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y knots_ind

gen br=1

append using data1.dta
append using fig6a_reg.dta

tw scatter y x if br==1, color(blue) legend(off) || scatter y x if br==2, color(blue) msymbol(p) || line mu_x x, xlabel(none) ylabel(none) ytitle("y") lcolor(black) lwidth(vthin) name(Fig6a)

*** Figure 6b ***
use data1, clear

binsreg y x, dotsgrid(1000) binsmethod(dpi) nodraw savedata(fig6b_reg) replace
binsreg y x, nodraw savedata(fig6b_bins) binsmethod(dpi) replace

use fig6b_reg, clear
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y

gen br=2
save fig6b_reg, replace

use fig6b_bins, clear
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y knots_ind

gen br=1

append using data1
append using fig6b_reg

tw scatter y x if br==1, color(blue) legend(off) || scatter y x if br==2, color(blue) msymbol(p) || line mu_x x, xlabel(none) ylabel(none) ytitle("y") lcolor(black) lwidth(vthin) name(Fig6b)

********************************************************************************/
********************************************************************************
* Figure 7: Confidence Intervals and Confidence Bands **************************
********************************************************************************
use fig2_data, clear
keep if br==.
save fig7_data, replace
*** Figure 7a ***
binsreg y_mod_hsk x, line(0,0) ci(1,1) cb(1,1) legend(off) xlabel(none) ylabel(none) ytitle("y") nodraw savedata(fig7a) replace

*** Figure 7b ***
binsreg y_mod_hsk x, dots(2,2) line(2,2) ci(3,3) cb(3,3) legend(off) nodraw xlabel(none) ylabel(none) ytitle("y") savedata(fig7b) replace

*** Figure 7c ***
binsreg y_high_hsk x, line(0,0) ci(1,1) cb(1,1) legend(off) nodraw  xlabel(none)ylabel(none) ytitle("y") savedata(fig7c) replace

*** Figure 7d ***
binsreg y_high_hsk x, dots(2,2) line(2,2) ci(3,3) cb(3,3) legend(off) nodraw xlabel(none) ylabel(none) ytitle("y") savedata(fig7d) replace

*** Figure 7a ***
use fig7a, clear
macro drop _all
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
rename line_x line_x
rename line_fit line_y
lab var x "x"
lab var y "y"
keep x y knots_ind line_x line_y line_isknot CI* CB*
gen br=1

sort line_x

append using fig7_data

levelsof line_x if line_isknot==1, local(exes)
foreach i of local exes {
  local xlines = "`xlines' `i'"
}

local nbins: word count(`xlines')
local xlines_first: word 1 of `xlines'
local nbins_less = `nbins' - 1
local xlines_last: word `nbins_less' of `xlines'

local line = "line line_y line_x if br==1 & line_x <= `xlines_first', lcolor(blue) || " 

forval i = 2/`nbins_less' {
local i_less = `i' - 1
local j: word `i_less' of `xlines'
local k: word `i' of `xlines'
local line = "`line' line line_y line_x if br==1 & line_x<=`k' & line_x>=`j', lcolor(blue) ||  " 

}
local line = "`line' line line_y line_x if br==1 & line_x>`xlines_last', lcolor(blue)|| " 

di "`line'"

tw  scatter y_mod_hsk x if br==. & inrange(y_mod_hsk,13,25), color(bluishgray)  msize(small) ///
|| rarea CB_l CB_r CB_x, fcolor(blue) fintensity(20) lwidth(none) legend(off) || `line' || ///
scatter y x if br==1, mcolor(blue) msize(small) xlabel(none) ylabel(none) ytitle("y") || rspike CI_l CI_r CI_x if br==1, lcolor(blue) name(Fig7a)

*** Figure 7b ***
use fig7b, clear
macro drop _all
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
rename line_x line_x
rename line_fit line_y
lab var x "x"
lab var y "y"
keep x y knots_ind line_x line_y line_isknot CI* CB*
gen br=1

sort line_x

append using fig7_data

tw  scatter y_mod_hsk x if br==. & inrange(y_mod_hsk,13,25), color(bluishgray)  msize(small) ///
|| rarea CB_l CB_r CB_x, fcolor(blue) fintensity(20) lwidth(none) legend(off) || line line_y line_x if br==1, lcolor(blue) || ///
rspike CI_l CI_r CI_x, lcolor(blue) || scatter y x if br==1, mcolor(blue) msize(small) xlabel(none) ylabel(none) ytitle("y") name(Fig7b)

*** Figure 7c ***
use fig7c, clear
macro drop _all
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
rename line_x line_x
rename line_fit line_y
lab var x "x"
lab var y "y"
keep x y knots_ind line_x line_y line_isknot CI* CB*
gen br=1

sort line_x

append using fig7_data

levelsof line_x if line_isknot==1, local(exes)
foreach i of local exes {
  local xlines = "`xlines' `i'"
}

local nbins: word count(`xlines')
di `nbins'
local xlines_first: word 1 of `xlines'
local nbins_less = `nbins' - 1
local xlines_last: word `nbins_less' of `xlines'

local line = "line line_y line_x if br==1 & line_x <= `xlines_first', lcolor(blue) ||"

forval i = 2/`nbins_less' {
local i_less = `i' - 1
local j: word `i_less' of `xlines'
local k: word `i' of `xlines'
local line = "`line' line line_y line_x if br==1 & line_x<=`k' & line_x>=`j', lcolor(blue)||"

}
local line = "`line' line line_y line_x if br==1 & line_x>`xlines_last', lcolor(blue)"

di "`line'"

tw  scatter y_high_hsk x if br==. & inrange(y_high_hsk,13,25), color(bluishgray)  msize(small) ///
|| rarea CB_l CB_r CB_x, fcolor(blue) fintensity(20) lwidth(none) legend(off) || `line' || rspike CI_l CI_r CI_x, lcolor(blue) || scatter y x if br==1, mcolor(blue) msize(small) ///
xlabel(none) ylabel(none) ytitle("y") name(Fig7c)

*** Figure 7d ***
use fig7d, clear
macro drop _all
rename dots_isknot knots_ind
rename dots_x x
rename dots_fit y
rename line_x line_x
rename line_fit line_y
lab var x "x"
lab var y "y"
keep x y knots_ind line_x line_y line_isknot CI* CB*
gen br=1

sort line_x

append using fig7_data

levelsof line_x if line_isknot==1, local(exes)
foreach i of local exes {
  local xlines = "`xlines' `i'"
}

local nbins: word count(`xlines')
local xlines_first: word 1 of `xlines'
local nbins_less = `nbins' - 1
local xlines_last: word `nbins_less' of `xlines'

local line = "line line_y line_x if br==1 & line_x <= `xlines_first', lcolor(blue) || rspike CI_l CI_r CI_x if br==1 & line_x <= `xlines_first', lcolor(blue) vertical || "

forval i = 2/`nbins_less' {
local i_less = `i' - 1
local j: word `i_less' of `xlines'
local k: word `i' of `xlines'
local line = "`line' line line_y line_x if br==1 & line_x<=`k' & line_x>=`j', lcolor(blue) ||  rspike CI_l CI_r CI_x if br==1 & line_x<=`k' & line_x>=`j', lcolor(blue) vertical ||"

}
local line = "`line' line line_y line_x if br==1 & line_x>`xlines_last', lcolor(blue)|| rspike CI_l CI_r CI_x if br==1 & line_x>`xlines_last', lcolor(blue) vertical"

di "`line'"

tw  scatter y_high_hsk x if br==. & inrange(y_high_hsk,13,25), color(bluishgray)  msize(small) ///
|| rarea CB_l CB_r CB_x, fcolor(blue) fintensity(20) lwidth(none) legend(off) || line line_y line_x, lcolor(blue) || rspike CI_l CI_r CI_x, lcolor(blue) || scatter y x if br==1, ///
xlabel(none) ylabel(none) ytitle("y") mcolor(blue) msize(small) name(Fig7d)


********************************************************************************
* Figure 8: Graphical Testing of Substantive Hypotheses ************************
********************************************************************************

*** Figure 8a ***
use data1, clear
gen Group=1

save fig8_data, replace
binsreg y x, cb(1,1) line(0,0) polyreg(1) xlabel(none) ylabel(none) legend(order(1 "binscatter" 3 "linear fit")) name(Fig8a)

*** Figure 8b ***
use data2, clear
gen Group=2

append using fig8_data
binsreg y x, by(Group) cb(1,1) line(0,0) xlabel(none) ylabel(none) legend(order(1 "Group 1" 4 "Group 2")) name(Fig8b)


********************************************************************************/
********************************************************************************
* Figure 9:  Graphical Representation of Parametric Specification Testing ******
********************************************************************************
use data1, clear

gen subset=1 if x<=.3

*** Figure 9a ***
binsreg y x if subset==1, dotsgrid(1000) nodraw savedata(fig9a_reg) replace
binsreg y x if subset==1, cb(1,1) nodraw savedata(fig9a) replace
* Linear *
binsreg y x if subset==1, line(0,0) cb(0,0) polyreg(1) nodraw savedata(linfit) replace
* Quadratic *
binsreg y x if subset==1, polyreg(2) nodraw savedata(qfit) replace
* Constant *
binsreg y x if subset==1, polyreg(0) nodraw savedata(const) replace

use fig9a, clear
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y CB*

gen br=4

save fig9a, replace

use fig9a_reg, clear
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y

gen br=.
save fig9a_reg, replace

use linfit, clear
rename dots_x x
rename dots_fit y
rename poly_x poly_x
rename poly_fit poly_y
gen br=1
save linfit, replace

use qfit, clear
rename dots_x x
rename dots_fit y
rename poly_x poly_x
rename poly_fit poly_y
gen br=2
save qfit, replace

use const, clear
rename dots_x x
rename dots_fit y
rename poly_x poly_x
rename poly_fit poly_y
gen br=3

append using linfit
append using qfit
append using fig9a_reg
append using fig9a

tw rarea CB_l CB_r CB_x if br==4, fcolor(blue) fintensity(20) lwidth(none) ||  scatter y x if br==1, color(blue)|| ///
line poly_y poly_x if br==1,  lcolor(black) || ///
line poly_y poly_x if br==2,  lpattern(dash) lcolor(black) || ///
line poly_y poly_x if br==3, lcolor(black) lpattern(dot) || scatter y x if br==., color(blue) msymbol(p) legend(order(2 "binscatter" 3 "linear" 4 "quadratic" 5 "constant")) ///
ytitle("y") xtitle("x") xlabel(none) ylabel(none) name(Fig9a)


*** Figure 9b ***
use data1, clear
binsreg y x, dotsgrid(1000) nodraw savedata(fig9b_reg) replace
binsreg y x, nodraw cb(1,1) savedata(fig9b) replace
* Linear *
binsreg y x, line(0,0) cb(0,0) polyreg(1) nodraw savedata(linfit) replace
* Quadratic *
binsreg y x, polyreg(2) nodraw savedata(qfit) replace
* Constant *
binsreg y x, polyreg(0) nodraw savedata(cons) replace

use fig9b, clear
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y CB*

gen br=4

save fig9b, replace

use fig9b_reg, clear
rename dots_x x
rename dots_fit y
lab var x "x"
lab var y "y"
keep x y

gen br=.
save fig9b_reg, replace

use linfit, clear
rename dots_x x
rename dots_fit y
rename poly_x poly_x
rename poly_fit poly_y
gen br=1
save linfit, replace

use qfit, clear
rename dots_x x
rename dots_fit y
rename poly_x poly_x
rename poly_fit poly_y
gen br=2
save qfit, replace

use cons, clear
rename dots_x x
rename dots_fit y
rename poly_x poly_x
rename poly_fit poly_y
gen br=3

append using linfit
append using qfit
append using fig9b_reg
append using fig9b

tw rarea CB_l CB_r CB_x if br==4, fcolor(blue) fintensity(20) lwidth(none) || scatter y x if br==4, color(blue) || line poly_y poly_x if br==1,  lcolor(black) || ///
line poly_y poly_x if br==2,  lpattern(dash) lcolor(black) || ///
line poly_y poly_x if br==3, lcolor(black) lpattern(dot) ||  scatter y x if br==., color(blue) msymbol(p) legend(order(2 "binscatter" 3 "linear" 4 "quadratic" 5 "constant")) ///
ytitle("y") xtitle("x") xlabel(none) ylabel(none) name(Fig9b)


********************************************************************************
* Table 1: Formal Testing of Substantive Hypothesis ****************************
********************************************************************************
use data1, clear

gen subset=1 if x<=.3

* Constant *
binsregtest y x if subset==1, bins(0,0) testmodelpoly(0)
binsregtest y x, bins(0,0) testmodelpoly(0)

* Linear *
binsregtest y x if subset==1, bins(0,0) testmodelpoly(1)
binsregtest y x, bins(0,0) testmodelpoly(1)

* Quadratic *
binsregtest y x if subset==1, bins(0,0) testmodelpoly(2)
binsregtest y x, bins(0,0) testmodelpoly(2)

*** Shape restriction ***
* negativity: <=0 *
binsregtest y x if subset==1, deriv(0) testshapel(0)
binsregtest y x, deriv(0) testshapel(0)

* Decreasing: first-order deriv <= 0 *
binsregtest y x if subset==1, bins(1,1) deriv(1) testshapel(0)
binsregtest y x, bins(1,1) deriv(1) testshapel(0)

* Concavity: second-order deriv <= 0 *
binsregtest y x if subset==1, bins(2,2) deriv(2) testshapel(0)
binsregtest y x, bins(2,2) deriv(2) testshapel(0)


********************************************************************************
* Figure 10:  Gini Index versus Household Income *******************************
********************************************************************************

use CCFF_2019_Binscatter--SA.dta, clear

// Drop Puerto Rico observations
drop if (geoid2>=00600 & geoid2<=00799) | (geoid2>=00900 & geoid2<=00999) // for zipcode level
// Drop 2016 data
drop if year ~= 2017

local giniControls "percentBachelorsEdu medianAge uninsuredRate percentHsEdu ueRate"

//No Controls
binsreg giniIndex medianHouseholdIncome, cb(3,3) line(3,3) ///
  xtitle("x") ///
  ytitle("y") legend (off) name(Fig10a)

//With Controls
binsreg giniIndex medianHouseholdIncome `giniControls', cb(3,3) line(3,3) ///
  xtitle("x") ///
  ytitle("y") legend (off) name(Fig10b)

  
********************************************************************************
* Table 2: Formal Testing of Substantive Hypothesis ****************************
********************************************************************************
* Constant *
binsregtest giniIndex medianHouseholdIncome `giniControls', bins(0,0) testmodelpoly(0)

* Linear *
binsregtest giniIndex medianHouseholdIncome `giniControls', bins(0,0) testmodelpoly(1)

* Quadratic *
binsregtest giniIndex medianHouseholdIncome `giniControls', bins(0,0) testmodelpoly(2)

*** Shape restriction ***
* positivity: >=0 *
binsregtest giniIndex medianHouseholdIncome `giniControls', deriv(0) testshaper(0)

* Decreasing: first-order deriv <= 0 *
binsregtest giniIndex medianHouseholdIncome `giniControls', bins(1,1) deriv(1) testshapel(0)

* Convexity: second-order deriv >= 0 *
binsregtest giniIndex medianHouseholdIncome `giniControls', bins(2,2) deriv(2) testshaper(0)




