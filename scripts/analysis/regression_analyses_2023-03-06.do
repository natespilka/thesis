*******************************************************************************
** setup and clean workspace
*******************************************************************************
* nate spilka
* 2023-03-06
cls
clear all

cd "/Users/nathanielhugospilka/Documents/Thesis/quant_work/"

*******************************************************************************
** load data
*******************************************************************************

global data_location = "/Users/nathanielhugospilka/Documents/Thesis/quant_work/data/processed/"

use ${data_location}thesis_dataset_2023-03-28.dta, clear

browse
describe
summarize

* we need to turn geoid into a numeric for the regression?
// destring geoid, replace
xtset geoid year

*******************************************************************************
*******************************************************************************
** regressions
*******************************************************************************
*******************************************************************************

*******************************************************************************
** Table 4 - logged crime_rate
*******************************************************************************
// replace all_site_type = all_site_type*-1
* (1) bivariate
regress logd_crime_rate all_site_type, robust
// outreg2 using reg1, replace excel dec(3)

* (2) multivariate 
regress logd_crime_rate all_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg 
// outreg2 using reg1, append excel dec(3)

* (3) multivariate + entity FE
xtreg logd_crime_rate all_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg, ///
i(geoid) fe vce(robust)
// outreg2 using reg1, append excel dec(3)

* (4) multivariate + year/entity FE
xtreg logd_crime_rate all_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
// outreg2 using reg1, append excel dec(3)

* (5) multivariate + year/entity FE + program site type
xtreg logd_crime_rate syep_site_type asp_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
// outreg2 using reg1, append excel dec(3)

* (6) multivariate + year/entity FE + interaction (youth)
* creating youth interaction
generate interaction_youth = all_site_type*above_yth_prop_threshold
xtreg logd_crime_rate all_site_type above_yth_prop_threshold interaction_youth ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_youth = 0
// outreg2 using reg1, append excel dec(3)

* (7) multivariate + year/entity FE + interaction (income)
* creating income interaction
generate interaction_inc = all_site_type*below_inc_theshold
xtreg logd_crime_rate all_site_type below_inc_theshold interaction_inc ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

test all_site_type + interaction_inc = 0
// outreg2 using reg1, append excel dec(3)

*******************************************************************************
** Table 5 - logged violent & property crime_rate
*******************************************************************************

* (1) violent crime
xtreg logd_violent_crime_rate all_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
// outreg2 using reg2, replace excel dec(3)

* (2) violent crime + program type
xtreg logd_violent_crime_rate syep_site_type asp_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
// outreg2 using reg2, append excel dec(3)

* (3) violent crime + interaction (youth)
xtreg logd_violent_crime_rate all_site_type above_yth_prop_threshold interaction_youth ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_youth = 0
// outreg2 using reg2, append excel dec(3)

* (4) violent crime + lowest income quartile interaction
xtreg logd_violent_crime_rate all_site_type below_inc_theshold interaction_inc ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_inc = 0
// outreg2 using reg2, append excel dec(3)

* (5) property crime
xtreg logd_property_crime_rate all_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
// outreg2 using reg2, append excel dec(3)

* (6) property crime + program type
xtreg logd_property_crime_rate syep_site_type asp_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
// outreg2 using reg2, append excel dec(3)

* (7) property crime + year/entity FE + interaction (youth)
xtreg logd_property_crime_rate all_site_type above_yth_prop_threshold interaction_youth ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_youth = 0
// outreg2 using reg2, append excel dec(3)

* (8) property crime + lowest income quartile
xtreg logd_property_crime_rate all_site_type below_inc_theshold interaction_inc ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_inc = 0
// outreg2 using reg2, append excel dec(3)

*******************************************************************************
** Table 6 - lead logged crime_rate
*******************************************************************************

* (1) multivariate + year/entity FE
xtreg lead_logd_crime_rate all_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
// outreg2 using reg2, append excel dec(3)

* (2) multivariate + program site type
xtreg lead_logd_crime_rate syep_site_type asp_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
// outreg2 using reg2, append excel dec(3)

* (3) multivariate + interaction (youth)
xtreg lead_logd_crime_rate all_site_type above_yth_prop_threshold interaction_youth ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_youth = 0
// outreg2 using reg2, append excel dec(3)

* (4) multivariate + year/entity FE + interaction (income)
xtreg lead_logd_crime_rate all_site_type below_inc_theshold interaction_inc ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_inc = 0
// outreg2 using reg2, append excel dec(3)

*******************************************************************************
** Table 7 - lead logged violent & property crime_rate
*******************************************************************************

* (1) violent crime
xtreg lead_logd_violent_crime_rate all_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
// outreg2 using reg2, append excel dec(3)

* (2) violent crime + program type
xtreg lead_logd_violent_crime_rate syep_site_type asp_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
// outreg2 using reg2, append excel dec(3)

* (3) lead violent crime + youth
xtreg lead_logd_violent_crime_rate all_site_type above_yth_prop_threshold interaction_youth ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_youth = 0
// outreg2 using reg2, append excel dec(3)

* (4) violent crime + lowest income quartile
xtreg lead_logd_violent_crime_rate all_site_type below_inc_theshold interaction_inc ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_inc = 0
// outreg2 using reg2, append excel dec(3)

* (5) property crime
xtreg lead_logd_property_crime_rate all_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
// outreg2 using reg2, append excel dec(3)

* (6) property crime + program type
xtreg lead_logd_property_crime_rate syep_site_type asp_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
// outreg2 using reg2, append excel dec(3)

* (7) lead property crime + youth
xtreg lead_logd_property_crime_rate all_site_type above_yth_prop_threshold interaction_youth ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_youth = 0
// outreg2 using reg2, append excel dec(3)

* (8) property crime + lowest income quartile
xtreg lead_logd_property_crime_rate all_site_type below_inc_theshold interaction_inc ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_inc = 0
// outreg2 using reg2, append excel dec(3)



















