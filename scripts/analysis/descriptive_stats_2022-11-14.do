*******************************************************************************
** setup and clean workspace
*******************************************************************************
* nate spilka
* 2022-11-14
cls
clear all

*******************************************************************************
** load data
*******************************************************************************

global data_location = "/Users/nathanielhugospilka/Documents/Thesis/quant_work/data/processed/"

use ${data_location}thesis_dataset_2023-02-07.dta, clear
browse
describe
summarize

*******************************************************************************
** imputaion
*******************************************************************************

impute median_hh_inc ///
prcnt_unemp ///
 total_pop prcnt_white prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
 prcnt_no_hs_deg prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
 num_cmplnt_ttl num_cmplnt_vlnt crime_rate violent_crime_rate, ///
 generate(imputed_mhhi)

* some income imputaions were negative - ADDRESS
replace imputed_mhhi = 0 if imputed_mhhi < 0

impute prcnt_unemp ///
median_hh_inc ///
 total_pop prcnt_white prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
 prcnt_no_hs_deg prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
 num_cmplnt_ttl num_cmplnt_vlnt crime_rate violent_crime_rate, ///
 generate(imputed_prcnt_unemp)

* we need to turn geoid into a numeric for the regression?
destring geoid, replace

*******************************************************************************
** labling variables
*******************************************************************************

* dv 
label variable crime_rate "NYPD Complaint Rate (2019-2021)"

* iv
label variable  site_for_0_1_2_3 "NYC DYCD Programs (2019)"
 
* economic control variables
label variable imputed_mhhi "Median Household Income (2019)"
label variable imputed_prcnt_unemp "Unemployment Rate" 

* population
label variable total_pop "Population"

* demographic control variables
* label variable prcnt_white "Proportion White Residents (non-Latino)" 
label variable prcnt_black "Proportion Black Residents (non-Latino)" 
label variable prcnt_hisp "Proportion Latino Residents"
* label variable prcnt_asian "Proportion Asian Residents"
label variable prcnt_other "Proportion 'Other' Residents"
label variable prcnt_yth_yng_adlt "Proportion Residents 10-24 years-old"

* education control variables
label variable prcnt_no_hs_deg "Proportion Without a High School Degree"
label variable prcnt_ba_deg "Proportion With a Bachelor's Degree or Higher"

* prior crime control variables
label variable cmplnt_rate_2006_2016 "Reported Crime Rate (2006-2016)"

*******************************************************************************
** output
*******************************************************************************

* ssc install estout, replace


tabstat crime_rate ///
total_pop prcnt_white prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt, stat(mean sd min max) col(stat) by(geoid)


tabstat cmplnt_rate_2019 ///
num_dycd_programs ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp /* prcnt_asian */ prcnt_other prcnt_yth_yng_adlt ///
 prcnt_no_hs_deg prcnt_ba_deg ///
 cmplnt_rate_2006_2016, ///
 c(stat) stat(sum mean sd min max n)

* clear the stored estimates
est clear

estpost tabstat cmplnt_rate_2019 ///
num_dycd_programs ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp /* prcnt_asian */ prcnt_other prcnt_yth_yng_adlt ///
 prcnt_no_hs_deg prcnt_ba_deg ///
 cmplnt_rate_2006_2016, ///
 c(stat) stat(sum mean sd min max n) elabels
 
*ereturn list 

* formatted output
esttab . using results1.rtf, replace ///
   cells("Sum(fmt(%6.0fc)) Mean(fmt(%6.2fc)) SD(fmt(%6.2fc)) Min Max count") nonumber ///
   nomtitle nonote noobs label ///
   collabels("Sum" "Mean" "SD" "Min" "Max" "N")


*******************************************************************************
*******************************************************************************
** regression
*******************************************************************************
*******************************************************************************

log using "/Users/nathanielhugospilka/Documents/Thesis/quant_work/output/analyses/reg_results_2023-02-07.log"

*******************************************************************************
** simple regressions - crime_rate
*******************************************************************************

regress crime_rate dycd_site, robust

xtreg crime_rate dycd_site, i(geoid) fe vce(robust)

xtreg crime_rate dycd_site i.year, i(geoid) fe vce(robust)

*******************************************************************************
** regression with economic controls - crime_rate
*******************************************************************************

xtreg crime_rate dycd_site ///
imputed_mhhi imputed_prcnt_unemp ///
i.year, i(geoid) fe vce(robust)

*******************************************************************************
** regression with demographic controls - crime_rate
*******************************************************************************

xtreg crime_rate dycd_site ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
i.year, i(geoid) fe vce(robust)

*******************************************************************************
** regression with education controls - crime_rate
*******************************************************************************

xtreg crime_rate dycd_site ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

*******************************************************************************
** regression with all controls (full force) - crime_rate
*******************************************************************************

xtreg crime_rate dycd_site ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

*******************************************************************************
** simple regressions - violent_crime_rate
*******************************************************************************

regress violent_crime_rate dycd_site, robust

xtreg violent_crime_rate dycd_site, i(geoid) fe vce(robust)

xtreg violent_crime_rate dycd_site i.year, i(geoid) fe vce(robust)

*******************************************************************************
** regression with economic controls - violent_crime_rate
*******************************************************************************

xtreg violent_crime_rate dycd_site ///
imputed_mhhi imputed_prcnt_unemp ///
i.year, i(geoid) fe vce(robust)

*******************************************************************************
** regression with demographic controls - violent_crime_rate
*******************************************************************************

xtreg violent_crime_rate dycd_site ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
i.year, i(geoid) fe vce(robust)

*******************************************************************************
** regression with education controls - violent_crime_rate
*******************************************************************************

xtreg violent_crime_rate dycd_site ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

*******************************************************************************
** regression with all controls (full force) - violent_crime_rate
*******************************************************************************

xtreg violent_crime_rate dycd_site ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)


log close


translate /Users/nathanielhugospilka/Documents/Thesis/quant_work/output/analyses/reg_results_2023-02-07.smcl /Users/nathanielhugospilka/Documents/Thesis/quant_work/output/analyses/reg_results_2023-02-07.pdf, translator(smcl2pdf)





















