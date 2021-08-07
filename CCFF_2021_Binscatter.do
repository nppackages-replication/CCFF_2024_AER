********************************************************************************
** Replication file: "On Binscatter"
** Authors: M. D. Cattaneo, R. Crump, M. Farrell and Y. Feng
** Last update: 6-AUG-2021
********************************************************************************
clear all
capture log close
capture program drop _all
capture macro drop _all
drop _all
set more off

set scheme s1color
********************************************************************************
** DGP and Setup
********************************************************************************

if (0>0) {
	set seed 1234

	set obs 2000

	gl sig = 0.5
	gl xmin = 0
	gl xmax = 1

	********************************************************************************
	** Generate data
	********************************************************************************
	gen t = 1
	replace t = 2 in 1001/2000

	** Main dataset (t=1)
	gen x = rbeta(2,4) if t==1
	gen w = 3*(x - ($xmax+$xmin)/2) + runiform(-0.5, 0.5) if t==1
	gen y_true = ((0.54)-(1.11*x)+(2.81*x^2)-(2.47*x^3)+(0.6*x^4))*40 if t==1
	gen y = y_true + w + rnormal(0,$sig) if t==1

	** Used to illustrate cov adj. (w indep of x)
	gen w_x = runiform(-1,1) if t==1
	gen y_wx = y_true + w_x + rnormal(0,$sig) if t==1

	** Add conditional heteroskedasticity
	xtile bin_membership = x if t==1, nq(10)
	replace y = y + 10*((abs(bin_membership - 3.5) - 4.5)^2/50) * rnormal(0, $sig) if t==1

	** Second group dataset (t=2)
	replace x = rbeta(2, 4) if t==2
	replace w = 3*(x - ($xmax+$xmin)/2) + runiform(-0.5, 0.5) if t==2
	replace y_true = ((0.54)-(1.11*x)+(2.81*x^2)-(2.47*x^3)+(0.6*x^4))*40 - 25*(x-0.5)^2+2 if t==2
	replace y = y_true + w + rnormal(0,$sig) if t==2

	** Final dataset
	save CCFF_2021_Binscatter, replace
	
}

set seed 1234

********************************************************************************
** Figure 1: Canonical Binscatter
********************************************************************************

use CCFF_2021_Binscatter, clear
drop if t==2

binsreg y x, nbins(10) line(0,0) nodraw savedata(fig1_data) replace

use fig1_data, clear
rename line_isknot knots_ind
rename dots_x x
rename dots_fit y
keep x y knots_ind line_x
gen br=1

append using CCFF_2021_Binscatter
drop if t==2

bysort br (x): replace knots_ind=1 if (_n==1 | _n==_N) & br==.
bysort br (x): replace line_x=x if (_n==1 | _n==_N) & br==.
levelsof line_x if knots_ind==1, local(exes)

tw scatter y x if br==. & inrange(y,12,22), color(bluishgray) msize(vsmall) || scatter y x if br==1, xline(`exes', lwidth(thin) lcolor(black) lpattern(dash) lstyle(grid)) xlabel(none) ylabel(12[2]22, labcolor(white) notick nogrid) color(blue) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Y") xtitle("X") name(Fig1a)

tw lfit y x if br==., lcolor(black) || scatter y x if br==1, color(blue) xlabel(none) ylabel(12[2]22, labcolor(white) notick nogrid) legend(order(2 "binscatter" 1 "linear fit" ) position(0) bplacement(neast) cols(1)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Y") xtitle("X") name(Fig1b)

********************************************************************************
** Figure 2: Extended Binscatter
********************************************************************************

*** Figure 2a ***
use CCFF_2021_Binscatter, clear
drop if t==2

binsreg y x w, nbins(10) at(0) dots(1,0) line(1,0) linegrid(300) nodraw savedata(fig2a_data) replace

use fig2a_data, clear
gen br=1
append using CCFF_2021_Binscatter
drop if t==2

sort x line_x 

tw scatter y x if br==. & inrange(y,12,22), color(bluishgray) msize(vsmall) || line y_true x if br==., lcolor(gs8)  || scatter dots_fit dots_x if br==1, mcolor(blue) || line line_fit line_x if br==1, lcolor(blue) cmissing(n) xlabel(none)  ylabel(12[2]22, labcolor(white) notick nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Y") xtitle("X") name(Fig2a)

*** Figure 2b ***
use CCFF_2021_Binscatter, clear
drop if t==2

binsreg y x w, nbins(10) at(0) dots(2,2) line(2,2) linegrid(300) nodraw savedata(fig2b_data) replace

use fig2b_data, clear
gen br=1
append using CCFF_2021_Binscatter
drop if t==2

sort x line_x

tw scatter y x if br==. & inrange(y,12,22), color(bluishgray) msize(vsmall) || line y_true x if br==., lcolor(gs8) || scatter dots_fit dots_x if br==1, mcolor(blue) || line line_fit line_x if br==1, lcolor(blue) xlabel(none) ylabel(12[2]22, labcolor(white) notick nogrid) ytitle("Y") xtitle("X") legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black)) name(Fig2b)

********************************************************************************
** Figure 3: Covariate Adjustment
********************************************************************************

*** X correlated with W ***
use CCFF_2021_Binscatter, clear
drop if t==2

reg y w 
predict resid_y, residuals
egen mean_y = mean(y)
replace resid_y = resid_y + mean_y

reg x w
predict resid_x, residuals
egen mean_x = mean(x)
replace resid_x = resid_x + mean_x

binsreg resid_y resid_x, nbins(10) line(0,0) at(0) nodraw savedata(fig3a_resid) replace
binsreg y x w, nbins(10) line(0,0) at(0) nodraw savedata(fig3a_cov) replace

use fig3a_resid, clear
gen br=2
save fig3a_resid, replace 

use fig3a_cov, clear
gen br=1

append using CCFF_2021_Binscatter
drop if t==2
append using fig3a_resid

sort x line_x

tw line y_true x if br==., color(gs8) || scatter dots_fit dots_x if br==1, color(blue) msymbol(O) || line line_fit line_x if br == 1, lcolor(blue) cmissing(n) || scatter dots_fit dots_x if br==2, color(red) msymbol(S) || line line_fit line_x if br == 2, lcolor(red) lpattern(dash) cmissing(n) text(15.15 0.35 "<--------  supp. narrowed  -------->", color(red))  xlabel(none) ylabel(15[1]22, labcolor(white) notick nogrid) ytitle("Y") xtitle("X") legend(order(4 "residualized canonical binscatter" 2 "semi-linear canonical binscatter" 1 "true function" ) position(0) bplacement(neast) cols(1)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) name(Fig3a)

*** X independent of W ***
use CCFF_2021_Binscatter, clear
drop if t==2

reg y_wx w_x
predict resid_y, residuals
egen mean_y = mean(y_wx)
replace resid_y = resid_y + mean_y

reg x w_x
predict resid_x, residuals
egen mean_x = mean(x)
replace resid_x = resid_x + mean_x

binsreg resid_y resid_x, nbins(10) line(0,0) at(0) nodraw savedata(fig3b_resid) replace
binsreg y_wx x w_x, nbins(10) line(0,0) at(0) nodraw savedata(fig3b_cov) replace

use fig3b_resid, clear
gen br=2
save fig3b_resid, replace

use fig3b_cov, clear
gen br=1

append using CCFF_2021_Binscatter
drop if t==2
append using fig3b_resid.dta

sort x line_x

tw line y_true x if br==., color(gs8) || scatter dots_fit dots_x if br==1, color(blue) msymbol(O) || line line_fit line_x if br == 1, lcolor(blue) cmissing(n) || scatter dots_fit dots_x if br==2, color(red) msymbol(S) || line line_fit line_x if br == 2, lcolor(red) lpattern(dash) cmissing(n) xlabel(none) ylabel(15[1]22, labcolor(white) notick nogrid) ytitle("Y") xtitle("X") legend(order(4 "residualized canonical binscatter" 2 "semi-linear canonical binscatter" 1 "true function" ) position(0) bplacement(neast) cols(1)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) name(Fig3b)


********************************************************************************
** Figure 4: IMSE-optimal J
********************************************************************************

*** Figure 4a ***
use CCFF_2021_Binscatter, clear
drop if t==2

binsreg y x w, nbins(10) at(0) dots(0,0) line(0,0) nodraw savedata(fig4a_data) replace

use fig4a_data, clear
gen br=1
append using CCFF_2021_Binscatter
drop if t==2

sort line_x x

tw scatter y x if br==. & inrange(y,12,22), color(bluishgray) msize(vsmall) || line y_true x if br==., lcolor(gs8) || scatter dots_fit dots_x if br==1, mcolor(blue) || line line_fit line_x, lcolor(blue) cmissing(n)  xlabel(none) ylabel(12[2]22, labcolor(white) notick nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Y") xtitle("X") name(Fig4a)

*** Figure 4b ***
use CCFF_2021_Binscatter, clear
drop if t==2

binsreg y x w, at(0) dots(0,0) line(0,0) nodraw savedata(fig4b_data) replace

use fig4b_data, clear
gen br=1
append using CCFF_2021_Binscatter
drop if t==2

sort line_x x

tw scatter y x if br==. & inrange(y,12,22), color(bluishgray) msize(vsmall) || line y_true x if br==., lcolor(gs8) || scatter dots_fit dots_x if br==1, mcolor(blue) || line line_fit line_x, lcolor(blue) cmissing(n) xlabel(none) ylabel(12[2]22, labcolor(white) notick nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Y") xtitle("X") name(Fig4b)

********************************************************************************
** Figure 5: Uncertanty Quantification
********************************************************************************

*** Figure 5a ***
use CCFF_2021_Binscatter, clear
drop if t==2

binsreg y x w, at(0) line(0,0) ci(1,1) cb(1,1) nodraw savedata(fig5a_data) replace

use fig5a_data, clear
gen br=1
append using CCFF_2021_Binscatter
drop if t==2

tw  scatter y x if br==. & inrange(y,12,23), color(bluishgray)  msize(small) || rarea CB_l CB_r CB_x, fcolor(blue) fintensity(20) lwidth(none) || line line_fit line_x, lcolor(blue) cmissing(n) || scatter dots_fit dots_x if br==1, mcolor(blue) msize(small) xlabel(none) ylabel(none) || rcap CI_l CI_r CI_x if br==1, lcolor(blue) ylabel(12[1]23, labcolor(white) notick nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Y") xtitle("X") name(Fig5a)

*** Figure 5b ***
use CCFF_2021_Binscatter, clear
drop if t==2

binsreg y x w, at(0) dots(2,2) line(2,2) ci(3,3) cb(3,3) nodraw savedata(fig5b_data) replace

use fig5b_data, clear
gen br=1
append using CCFF_2021_Binscatter
drop if t==2

tw  scatter y x if br==. & inrange(y,12,23), color(bluishgray)  msize(small) || rarea CB_l CB_r CB_x, fcolor(blue%75) fintensity(20) lwidth(none) legend(off) || line line_fit line_x if br==1, lcolor(blue) || rcap CI_l CI_r CI_x, lcolor(blue) || scatter dots_fit dots_x if br==1, mcolor(blue) msize(small) xlabel(none) ylabel(12[1]23, labcolor(white) notick nogrid) legend(off) graphregion(color(white) margin(large)) plotregion(lcolor(black)) ytitle("Y") xtitle("X") name(Fig5b)

********************************************************************************
** Figure 6: Hypothesis Testing -- Parametric Specification
********************************************************************************

*** Figure 6a ***
use CCFF_2021_Binscatter, clear
drop if t==2

binsreg y x w, at(0) dots(0,0) line(0,0) cb(1,1) polyreg(1) nodraw savedata(fig6a_data) replace
* Quadratic *
binsreg y x w, at(0) polyreg(2) nodraw savedata(fig6a_qfit) replace
* Linear *
binsreg y x w, at(0) polyreg(1) nodraw savedata(fig6a_linfit) replace
* Constant *
binsreg y x w, at(0) polyreg(0) nodraw savedata(fig6a_const) replace

use fig6a_data, clear
gen br = 3
save fig6a_data, replace

use fig6a_qfit, clear
gen br = 2
save fig6a_qfit, replace

use fig6a_linfit, clear
gen br = 1
save fig6a_linfit, replace

use fig6a_const, clear
gen br = 0
save fig6a_const, replace

append using fig6a_linfit
append using fig6a_qfit
append using fig6a_data
append using CCFF_2021_Binscatter
drop if t==2

tw scatter y x if br == . & inrange(y,12,23), color(bluishgray) msize(vsmall) || rarea CB_l CB_r CB_x if br == 3, fcolor(blue%75) fintensity(20) lwidth(none) || line poly_fit poly_x if br==2,  lpattern(dash) lcolor(black) lwidth(vthin) || line poly_fit poly_x if br==1, lcolor(black) lwidth(vthin) || line poly_fit poly_x if br==0, lcolor(black) lpattern(dot) lwidth(vthin) || scatter dots_fit dots_x if br == 3, color(blue) || line line_fit line_x if br == 3, lcolor(blue) cmissing(n) lwidth(vthin) xlabel(none) ytitle("Y") xtitle("X") ylabel(12[1]23, labcolor(white) notick nogrid) legend(order(6 "binscatter" 5 "constant" 4 "linear" 3 "quadratic" ) position(0) bplacement(neast) cols(1)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) name(Fig6a)


*** Figure 6b ***
use CCFF_2021_Binscatter, clear
drop if t==2

binsreg y x w, at(0) dots(2,2) line(2,2) cb(3,3) polyreg(1) nodraw savedata(fig6b_data) replace
* Quadratic *
binsreg y x w, at(0) polyreg(2) nodraw savedata(fig6b_qfit) replace
* Linear *
binsreg y x w, at(0) polyreg(1) nodraw savedata(fig6b_linfit) replace
* Constant *
binsreg y x w, at(0) polyreg(0) nodraw savedata(fig6b_const) replace

use fig6b_data, clear
gen br = 3
save fig6b_data, replace

use fig6b_qfit, clear
gen br = 2
save fig6b_qfit, replace

use fig6b_linfit, clear
gen br = 1
save fig6b_linfit, replace

use fig6b_const, clear
gen br = 0
save fig6b_const, replace

append using fig6b_linfit
append using fig6b_qfit
append using fig6b_data
append using CCFF_2021_Binscatter
drop if t==2

tw scatter y x if br == . & inrange(y,12,23), color(bluishgray) msize(vsmall) || rarea CB_l CB_r CB_x if br == 3, fcolor(blue%75) fintensity(20) lwidth(none) || line poly_fit poly_x if br==2,  lpattern(dash) lcolor(black) lwidth(vthin) || line poly_fit poly_x if br==1, lcolor(black) lwidth(vthin) || line poly_fit poly_x if br==0, lcolor(black) lpattern(dot) lwidth(vthin)|| scatter dots_fit dots_x if br == 3, color(blue) || line line_fit line_x if br == 3, lcolor(blue) cmissing(n) lwidth(vthin) ylabel(12[1]23, labcolor(white) notick nogrid) xlabel(none) ytitle("Y") xtitle("X") legend(order(6 "binscatter" 5 "constant" 4 "linear" 3 "quadratic" ) position(0) bplacement(neast) cols(1)) graphregion(color(white) margin(large)) plotregion(lcolor(black)) name(Fig6b)

********************************************************************************
** Table 1: Hypothesis Testing -- Parametric Specification
********************************************************************************

use CCFF_2021_Binscatter, clear
drop if t==2

binstest y x w, at(0) bins(0,0) testmodel(1,1) testmodelpoly(0)
binstest y x w, at(0) bins(0,0) testmodel(1,1) testmodelpoly(0) lp(2)

binstest y x w, at(0) bins(0,0) testmodel(1,1) testmodelpoly(1)
binstest y x w, at(0) bins(0,0) testmodel(1,1) testmodelpoly(1) lp(2)

binstest y x w, at(0) bins(0,0) testmodel(1,1) testmodelpoly(2)
binstest y x w, at(0) bins(0,0) testmodel(1,1) testmodelpoly(2) lp(2)

binstest y x w, at(0) bins(2,2) testmodel(3,3) testmodelpoly(0)
binstest y x w, at(0) bins(2,2) testmodel(3,3) testmodelpoly(0) lp(2)

binstest y x w, at(0) bins(2,2) testmodel(3,3) testmodelpoly(1)
binstest y x w, at(0) bins(2,2) testmodel(3,3) testmodelpoly(1) lp(2)

binstest y x w, at(0) bins(2,2) testmodel(3,3) testmodelpoly(2)
binstest y x w, at(0) bins(2,2) testmodel(3,3) testmodelpoly(2) lp(2)


********************************************************************************
** Table 2: Hypothesis Testing -- Shape Restriction
********************************************************************************

use CCFF_2021_Binscatter, clear
drop if t==2

binstest y x w, at(0) deriv(0) testshapel(0) bins(0,0) testshape(1,1)

binstest y x w, at(0) deriv(0) testshapel(0) bins(1,1) testshape(2,2)
binstest y x w, at(0) deriv(1) testshapel(0) bins(1,1) testshape(2,2)

binstest y x w, at(0) deriv(0) testshapel(0) bins(2,2) testshape(3,3)
binstest y x w, at(0) deriv(1) testshapel(0) bins(2,2) testshape(3,3)
binstest y x w, at(0) deriv(2) testshapel(0) bins(2,2) testshape(3,3)


********************************************************************************
** Figure 7: Two-group Comparison
********************************************************************************

*** Figure 7a ***
use CCFF_2021_Binscatter, clear
binsreg y x w, at(0) by(t) line(0,0) cb(1,1) lineplotopt(lwidth(vthin)) bysymbols(O S) bycolors(blue green) bylpatterns(solid dot) legend(order(2 "Group 0" 5 "Group 1") position(0) bplacement(neast) cols(1)) ylabel(12[2]22, labcolor(white) notick nogrid) xlabel(none) graphregion(color(white) margin(large)) plotregion(lcolor(black)) xtitle("X") ytitle("Y") xline(0.235, lcolor(gs15) lwidth(4)) xline(0.22 0.25, lcolor(gs8) lpattern(shortdash)) name(Fig7a)

*** Figure 7b ***
use CCFF_2021_Binscatter, clear
binsreg y x w, at(0) by(t) dots(2,2) line(2,2) cb(3,3) lineplotopt(lwidth(vthin)) bysymbols(O S) bycolors(blue green) bylpatterns(solid dot) legend(order(2 "Group 0" 5 "Group 1") position(0) bplacement(neast) cols(1)) ylabel(12[2]22, labcolor(white) notick nogrid) xlabel(none) graphregion(color(white) margin(large)) plotregion(lcolor(black)) xtitle("X") ytitle("Y") xline(0.235, lcolor(gs15) lwidth(4)) xline(0.22 0.25, lcolor(gs8) lpattern(shortdash)) name(Fig7b)

********************************************************************************
** Table 3: Two-group Comparison
********************************************************************************

use CCFF_2021_Binscatter, clear

gen ind = (x >= 0.22 & x <= 0.25) if !missing(x)

binspwc y x w if ind, at(0) by(t) bins(0,0) pwc(1,1) 
binspwc y x w, at(0) by(t) bins(0,0) pwc(1,1) 

binspwc y x w if ind, at(0) by(t) bins(2,2) pwc(3,3) 
binspwc y x w, at(0) by(t) bins(2,2) pwc(3,3) 


********************************************************************************
** Figure 8: Generalized Non-linear Binscatter
********************************************************************************

************************************************************
** Row 1: Quantile Regression
************************************************************

*** Figure 8a ***
use CCFF_2021_Binscatter, clear
drop if t==2

binsqreg y x w, at(0) quantile(0.25) line(0,0) ci(1,1) cb(1,1) nodraw savedata(fig8a_data_25) replace
binsqreg y x w, at(0) quantile(0.75) line(0,0) ci(1,1) cb(1,1) nodraw savedata(fig8a_data_75) replace

use fig8a_data_25, clear
gen br = 25
save fig8a_data_25, replace

use fig8a_data_75, clear
gen br = 75

append using fig8a_data_25
append using CCFF_2021_Binscatter
drop if t==2

tw scatter y x if br == . & inrange(y,12,23), color(bluishgray) msize(vsmall) || rarea CB_l CB_r CB_x if br == 25, color(blue%75) fintensity(20) lwidth(none) || scatter dots_fit dots_x if br == 25, color(blue) msize(small) || line line_fit line_x if br == 25, color(blue) cmissing(n) lwidth(vthin) || rcap CI_l CI_r CI_x if br == 25, lcolor(blue) || rarea CB_l CB_r CB_x if br == 75, color(green%75) fintensity(20) lwidth(none) || scatter dots_fit dots_x if br == 75, color(green) msymbol(S) msize(small) || line line_fit line_x if br == 75, color(green) lpattern(dash) cmissing(n) lwidth(vthin) || rcap CI_l CI_r CI_x if br == 75, lcolor(green) graphregion(color(white) margin(large)) legend(order(3 "0.25 quantile" 7 "0.75 quantile" ) position(0) bplacement(neast) cols(1)) xlabel(none) ylabel(12[1]23, labcolor(white) notick nogrid)  ytitle("Y") xtitle("X") xline(0.31, lcolor(gs15) lwidth(49)) xline(0.12 0.50, lcolor(gs8) lpattern(shortdash)) plotregion(lcolor(black)) name(Fig8a)

*** Figure 8b ***
use CCFF_2021_Binscatter, clear
drop if t==2

binsqreg y x w, at(0) quantile(0.25) dots(2,2) line(2,2) ci(3,3) cb(3,3) nodraw savedata(fig8b_data_25) replace
binsqreg y x w, at(0) quantile(0.75) dots(2,2) line(2,2) ci(3,3) cb(3,3) nodraw savedata(fig8b_data_75) replace

use fig8b_data_25, clear
gen br = 25
save fig8b_data_25, replace

use fig8b_data_75, clear
gen br = 75

append using fig8b_data_25
append using CCFF_2021_Binscatter
drop if t==2

tw scatter y x if br == . & inrange(y,12,23), color(bluishgray) msize(vsmall) || rarea CB_l CB_r CB_x if br == 25, color(blue%75) fintensity(20) lwidth(none) || scatter dots_fit dots_x if br == 25, color(blue) msize(small) || line line_fit line_x if br == 25, color(blue) lwidth(vthin) || rcap CI_l CI_r CI_x if br == 25, lcolor(blue) || rarea CB_l CB_r CB_x if br == 75, color(green%75) fintensity(20) lwidth(none) || scatter dots_fit dots_x if br == 75, color(green) msymbol(S) msize(small) || line line_fit line_x if br == 75, color(green) lpattern(dash) lwidth(vthin) || rcap CI_l CI_r CI_x if br == 75, lcolor(green) graphregion(color(white) margin(large)) legend(order(3 "0.25 quantile" 7 "0.75 quantile" ) position(0) bplacement(neast) cols(1))  xlabel(none) ylabel(12[1]23, labcolor(white) notick nogrid) ytitle("Y") xtitle("X") xline(0.31, lcolor(gs15) lwidth(49)) xline(0.12 0.50, lcolor(gs8) lpattern(shortdash)) plotregion(lcolor(black)) name(Fig8b)


************************************************************
** Row 2: Quantile Regression
************************************************************

*** Figure 8c ***
use CCFF_2021_Binscatter, clear
drop if t==2
centile y, centile(33)
gen d = (y <= `r(c_1)') if !missing(y)
gen ind = (x >= 0.12 & x <= 0.50) if !missing(x)

binslogit d x w if ind, at(0) line(0,0) ci(1,1) cb(1,1) dotsplotopt(mcolor(blue)) lineplotopt(lcolor(blue)) ciplotopt(lcolor(blue)) cbplotopt(fcolor(blue%75) fintensity(20) lwidth(none)) graphregion(color(white) margin(large)) ylabel(none, nogrid) xlabel(none) plotregion(lcolor(black)) xtitle("X") ytitle("Y") name(Fig8c)

*** Figure 8d ***
use CCFF_2021_Binscatter, clear
drop if t==2
centile y, centile(33)
gen d = (y <= `r(c_1)') if !missing(y)
gen ind = (x >= 0.12 & x <= 0.50) if !missing(x)

binslogit d x w if ind, at(0) dots(2,2) line(2,2) ci(3,3) cb(3,3) dotsplotopt(mcolor(blue)) lineplotopt(lcolor(blue)) ciplotopt(lcolor(blue)) cbplotopt(fcolor(blue%75) fintensity(20) lwidth(none)) graphregion(color(white) margin(large)) ylabel(none, nogrid) xlabel(none) plotregion(lcolor(black)) xtitle("X") ytitle("Y") name(Fig8d)

************************************************************
** Row 3: Quantile Regression
************************************************************

*** Figure 8e ***
use CCFF_2021_Binscatter, clear

binsqreg y x w, at(0) quantile(0.75) line(0,0) cb(1,1) by(t) bysymbols(O S) bycolors(blue green) bylpatterns(solid dot) lineplotopt(lwidth(vthin)) legend(order(2 "Group 0" 5 "Group 1") position(0) bplacement(neast) cols(1)) ylabel(12[1]23, labcolor(white) notick nogrid) graphregion(color(white) margin(large)) xlabel(none) plotregion(lcolor(black)) xtitle("X") ytitle("Y") name(Fig8e)

*** Figure 8f ***
use CCFF_2021_Binscatter, clear

binsqreg y x w, at(0) quantile(0.75) dots(2,2) line(2,2) cb(3,3) by(t) bysymbols(O S) bycolors(blue green) bylpatterns(solid dot) lineplotopt(lwidth(vthin)) legend(order(2 "Group 0" 5 "Group 1") position(0) bplacement(neast) cols(1)) ylabel(12[1]23, labcolor(white) notick nogrid) graphregion(color(white) margin(large)) xlabel(none) plotregion(lcolor(black)) xtitle("X") ytitle("Y") name(Fig8f)

