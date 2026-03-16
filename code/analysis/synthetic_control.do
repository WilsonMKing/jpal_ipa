use "/Users/wilsonking/Desktop/jpal_ipa_df.dta", clear

tabulate regime, generate(regime_)

******************************
* Run SDID Model 1 (JPAL)    *
******************************

* Run SDID Regression

sdid frequency_rct country year jpal_treated, ///
    vce(bootstrap) ///
    covariates(logged_pop logged_gdp conflict regime_1 regime_2 regime_3 regime_4 independent ipa_treated igc_treated)

eststo sdid_jpal_controls

sdid frequency_rct country year ipa_treated, ///
    vce(bootstrap) ///
    covariates(logged_pop logged_gdp conflict regime_1 regime_2 regime_3 regime_4 independent jpal_treated igc_treated)

eststo sdid_ipa_controls

sdid frequency_rct country year jpal_treated, ///
    vce(bootstrap)

eststo sdid_jpal_no_controls

sdid frequency_rct country year ipa_treated, ///
    vce(bootstrap)

eststo sdid_ipa_no_controls

	
* Export to LaTeX
esttab sdid_jpal_no_controls sdid_jpal_controls sdid_ipa_controls sdid_ipa_no_controls using "/Users/wilsonking/Desktop/sdid_table.tex", replace ///
    se label star(* 0.10 ** 0.05 *** 0.01) ///
    title("SDID Regression Results") ///
    mgroups("SDID", pattern(1 0))


******************************
* Run SDID Model 2 (IPA)     *
******************************
sdid frequency_rct country year ipa_treated, ///
    vce(bootstrap) ///
    covariates(logged_pop logged_gdp conflict regime_1 regime_2 regime_3 regime_4 independent jpal_treated igc_treated)

matrix b2 = e(b)
matrix V2 = e(V)

preserve
matrix b2t = b2'
svmat b2t, names(col)
gen term = ""
local i = 1
foreach v of colnames b2 {
    replace term = "`v'" in `i'
    local i = `i' + 1
}

matrix se2 = vecdiag(cholesky(V2))
svmat se2, names(col)
rename se21 se

gen model = "ipa_model"

keep model term b2t1 se
rename b2t1 estimate

tempfile ipa
save `ipa'
restore


******************************
* Combine and Export         *
******************************
use `jpal', clear
append using `ipa'

export delimited using "sdid_results.csv", replace
