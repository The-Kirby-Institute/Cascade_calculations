*TREND ANALYSIS

global ROOT "C:\Users\Hmcmanus\Desktop\WORK_UNITS\CONCEPTS\HIV_CASCADE_RG"
global SOURCE "$ROOT\SOURCE"
global TEMP "$ROOT\TEMP"
global OUTPUT "$ROOT\OUTPUT"

global START=2006
global END=2015

cd "$SOURCE"

import delimited "HIVcascadeEstimates-2015.csv", delimiter(comma) varnames(1) clear

describe *

set more off

cd "$TEMP"

*PREP DATA
destring lower, gen(CI_L) force
destring upper, gen(CI_U) force

encode population, gen(Population)
qui summ Population, detail
global Pop=r(max)

keep if year>=$START & year<$END

gen T=year-$START

gen Annual_Rate_predicted=.
gen Annual_Rate_CI_L_predicted=.
gen Annual_Rate_CI_U_predicted=.
gen pseudo_R2=.
gen p_dev=.

*Assign weight based on inverse of standardised credible interval proportion of inverse of standardised credible interval for given stage and population
gen weight=value/abs(CI_L-CI_U)
bysort stage population:egen denominator=total(weight)
replace weight=weight/denominator
drop denominator

gen T2=T*T

save Temp.dta, replace

*CREATE OUTPUT FILE
keep stage population
duplicates drop
save output.dta, replace

foreach val in "numART" "infected" "pldhiv"  "suppressed" {
forvalues val2=1/$Pop { 

use Temp.dta, clear

keep if stage=="`val'"
keep if Population==`val2'

if(_N>0){

global Population=population[1]

*Fit Poisson model with T & T^2 term and reduce by backward selection at p(alpha=0.05). Use robust variance to correct any heteroskedasticity, use (probability) weights based on inverse of normalised credible interval
stepwise , pr(0.05): poisson value T T2 [pweight=weight], vce(robust) 

matrix beta=e(b)
svmat beta

*In event of MV model calculate intercept and significance of time trend
capture confirm variable beta3
if !_rc{
capture noisily global I_year=string(round(exp(beta1[1]+beta2[1]),0.001))
global Intercept=string(round(exp(beta3[1]),0.001))
test (T+T2=0) 
global pwald=.
global pwald="0"+string(round(e(p),0.001))
} 

*In event of UV model calculate intercept and significance of time trend
else {
global I_year=string(round(exp(beta1[1]),0.001))
global Intercept=string(round(exp(beta2[1]),0.001))
test (T=0)
global pwald=.
global pwald="0"+string(round(e(p),0.001))
}

*Chi squared test for significance of Deviance
qui estat gof

global pdev="0"+string(round(chi2tail(r(df),r(chi2_d)),0.001))

predictnl Predicted=exp(xb()), ci(CI_L_P CI_U_P) se(SE)


*Z-Test for random order of (unstandardised) residuals)
gen res=Predicted-value
runtest res 
global pzrord="0"+string(round(r(p),0.001))

*Plot of predicted v data mean values with prediction interval for model fit to data mean values
twoway ///
(rarea CI_L_P CI_U_P year, sort fcolor(eltgreen) lcolor(eltgreen)) ///
(scatter value year, sort) ///
(line Predicted year, sort ) ///
, scheme(s1mono)  ///
 plotregion(m(b=0)) ///
title("$Population, `val'", size(small) position(11)) ///
note("p-test (wald) for annual rate of increase=$pwald" ///
"Deviance goodness-of-fit=$pdev" ///
"Random order z-test=$pzrord" ///
"Baseline ($START)=$Intercept" ///
, size(small) position(4) ring(0)) ///
legend(order(2 "observed" 3 "predicted (95% prediction interval)" ) size(medsmall) position(11) ring(0)) ///
saving("Fitted-$Population-`val'.gph", replace)

*Output file of summary statistics and Fitted values
qui summ T, detail
global N=r(max)

keep stage population year Predicted CI_L_P CI_U_P

reshape wide Predicted CI_L_P CI_U_P, i(stage population) j(year)

svmat beta

gen p_trend=$pwald
gen p_dev=$pdev
gen p_rord=$pzrord

merge 1:1 stage population using output.dta
drop _merge
save output.dta, replace
}
}
}

cd "$OUTPUT"
save output.dta, replace

