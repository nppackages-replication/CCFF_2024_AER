********************************************************************************
* Replication "On Binscatter"
* Cattaneo, Crump, Farrell, and Feng (2024)
* Date: 05-JAN-2024
* Setup Program
********************************************************************************

/* This program installs all necessary Stata packages to run the replication code.

   Note(1): the reghdfe package must have a version number greater than or equal to 
   5.9.0 (03jun2020).  The code will check the version number and, if reghdfe is 
   installed, but is not a sufficient version, will prompt you to manually install
   reghdfe via "ssc install reghdfe".
   
   Note(2): this program was derived from 
   https://github.com/gslab-econ/template/blob/master/config/config_stata.do
   with minor changes made.
*/

clear all

********************************************************************************
* Install regdhfe
********************************************************************************
capture which reghdfe
if _rc == 111 {                 
	display "Installing regdhfe"
		quietly ssc install reghdfe, replace
}
else {
	* Check reghdfe version number
	findfile reghdfe.ado
	import delimited "`r(fn)'", clear
	ds
	foreach var in `r(varlist)' {
		if substr(`var',1,10)!="*! version" {
		drop `var'		
		}
	}
	rename v* v1new
	keep if substr(v1new,1,10)=="*! version"
	replace v1new = word(substr(v1new,12,.),1)
	local tmp = strpos(v1new,".")
	gen ver1 = substr(v1new,1,`tmp'-1)
	replace v1new = substr(v1new,`tmp'+1,.)
	local tmp = strpos(v1new,".")
	gen ver2 = substr(v1new,1,`tmp'-1)
	keep ver1 ver2
	destring ver1 ver2, replace
	if !(ver1 > 5 | (ver1==5 & ver2 >= 9)) {
		display "The reghdfe package must have a version number greater than or equal to 5.9.0 (03jun2020); please manually install reghdfe via 'ssc install regdhfe' which will overwrite your current version of the package." 
	}
}

********************************************************************************
* Install ftools
********************************************************************************
capture which ftools
if _rc == 111 {                 
	dis "Installing ftools"
	quietly ssc install ftools, replace
}

********************************************************************************
* Install binsreg
********************************************************************************
net install binsreg, from(https://raw.githubusercontent.com/nppackages/binsreg/master/stata) replace



