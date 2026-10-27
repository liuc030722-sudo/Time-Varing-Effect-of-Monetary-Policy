clear
import delimited "ecb_data.csv", clear
save "ecb_data.dta", replace

clear
import delimited "fed_data.csv", clear
save "fed_data.dta", replace


* ECB Analysis
use "ecb_data.dta", clear
gen mdate = monthly(substr(month, 1, 7), "YM")
format mdate %tm
tsset mdate

xtbreak test i_lag ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2002m12 2008m10 2022m8, fmt(tm)) ///
    trimming(0.10)
	
xtbreak test unemployment ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2005m10 2008m8 2011m5 2013m12 2020m3 2022m10, fmt(tm)) ///
    trimming(0.10)

xtbreak test inflation ///
    i_lag_hat unemployment_hat, ///
    breakpoints(2007m7 2010m2 2016m11 2020m5 2022m12, fmt(tm)) ///
    trimming(0.10)

	
* Fed Analysis
use "fed_data.dta", clear
gen mdate = monthly(substr(month, 1, 7), "YM")
format mdate %tm
tsset mdate

xtbreak test i_lag ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(1980m10 1987m6, fmt(tm)) ///
    trimming(0.10)

xtbreak test unemployment ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2020m3, fmt(tm)) ///
    trimming(0.10)
	
xtbreak test inflation ///
    i_lag_hat unemployment_hat, ///
    breakpoints(1980m11 1987m2 1999m12 2009m10 2020m6, fmt(tm)) ///
    trimming(0.10)
	
* Test for each breaks - ECB
use "ecb_data.dta", clear
gen mdate = monthly(substr(month, 1, 7), "YM")
format mdate %tm
tsset mdate

xtbreak test i_lag ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2002m12, fmt(tm))

xtbreak test i_lag ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2008m10, fmt(tm))

xtbreak test i_lag ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2022m8, fmt(tm))
	
xtbreak test unemployment ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2005m10, fmt(tm))

xtbreak test unemployment ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2008m8, fmt(tm))

xtbreak test unemployment ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2011m5, fmt(tm))

xtbreak test unemployment ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2013m12, fmt(tm))

xtbreak test unemployment ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2020m3, fmt(tm))

xtbreak test unemployment ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2022m10, fmt(tm))
	
xtbreak test inflation ///
    i_lag_hat unemployment_hat, ///
    breakpoints(2007m7, fmt(tm))

xtbreak test inflation ///
    i_lag_hat unemployment_hat, ///
    breakpoints(2010m2, fmt(tm))

xtbreak test inflation ///
    i_lag_hat unemployment_hat, ///
    breakpoints(2016m11, fmt(tm))

xtbreak test inflation ///
    i_lag_hat unemployment_hat, ///
    breakpoints(2020m5, fmt(tm))

xtbreak test inflation ///
    i_lag_hat unemployment_hat, ///
    breakpoints(2022m12, fmt(tm))	
	
* Test for each breaks - Fed	
use "fed_data.dta", clear
gen mdate = monthly(substr(month, 1, 7), "YM")
format mdate %tm
tsset mdate

xtbreak test i_lag ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(1980m10, fmt(tm))

xtbreak test i_lag ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(1987m6, fmt(tm))	
	
xtbreak test unemployment ///
    pi_lag2 pi_lag3 u_lag2 u_lag3 i_lag2 i_lag3, ///
    breakpoints(2020m3, fmt(tm))	
	
xtbreak test inflation ///
    i_lag_hat unemployment_hat, ///
    breakpoints(1980m11, fmt(tm))

xtbreak test inflation ///
    i_lag_hat unemployment_hat, ///
    breakpoints(1987m2, fmt(tm))

xtbreak test inflation ///
    i_lag_hat unemployment_hat, ///
    breakpoints(1999m12, fmt(tm))

xtbreak test inflation ///
    i_lag_hat unemployment_hat, ///
    breakpoints(2009m10, fmt(tm))

xtbreak test inflation ///
    i_lag_hat unemployment_hat, ///
    breakpoints(2020m6, fmt(tm))	
	
	
	

