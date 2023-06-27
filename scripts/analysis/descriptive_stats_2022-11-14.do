*******************************************************************************
** setup and clean workspace
*******************************************************************************
* nate spilka
* 2022-11-14
cls
clear all

cd "/Users/nathanielhugospilka/Documents/Thesis/quant_work/"

*******************************************************************************
** load data
*******************************************************************************

* FOR SPATIAL DATA CHECK THE END OF THIS DOC FOR NOW
// global data_location = "/Users/nathanielhugospilka/Documents/Thesis/quant_work/data/processed/shapefiles/shape1/"
// use ${data_location}thesis_dataset_2023-03-04.shp, clear

global data_location = "/Users/nathanielhugospilka/Documents/Thesis/quant_work/data/processed/"
use ${data_location}thesis_dataset_2023-03-06.dta, clear

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

// log using "/Users/nathanielhugospilka/Documents/Thesis/quant_work/output/analyses/reg_results_2023-02-14.log"

*******************************************************************************
** main regressions - logged crime_rate
*******************************************************************************

* bivariate
regress logd_crime_rate all_site_type, robust

* multivariate 
regress logd_crime_rate all_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg 

* multivariate + entity FE
xtreg logd_crime_rate all_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg, ///
i(geoid) fe vce(robust)

* multivariate + year/entity FE
xtreg logd_crime_rate all_site_type ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)


******************************* TESTING DIST VARIABLE

* multivariate + year/entity FE
xtreg logd_crime_rate mean_dist_all ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

xtreg lead_logd_crime_rate indx_dist_all ///
median_hh_inc prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)


mean_dist_all
mean_dist_syep
mean_dist_asp
logd_crime_rate
logd_property_crime_rate
logd_violent_crime_rate

lead_logd_crime_rate
lead_logd_property_crime_rate
lead_logd_violent_crime_rate

*******************************************************************************
** regressions - program site type
*******************************************************************************

* multivariate + year/entity FE + program site type
xtreg logd_crime_rate syep_site_type asp_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

*******************************************************************************
** regressions - Interactions
*******************************************************************************

* creating youth interaction
generate interaction_youth = all_site_type*above_yth_prop_threshold
* multivariate + year/entity FE + interaction (youth)
xtreg logd_crime_rate all_site_type above_yth_prop_threshold interaction_youth ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

test all_site_type + interaction_youth = 0

* creating income interaction
generate interaction_inc = all_site_type*below_inc_theshold
* multivariate + year/entity FE + interaction (income)
xtreg logd_crime_rate all_site_type below_inc_theshold interaction_inc ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

test all_site_type + interaction_inc = 0


// log close


// translate /Users/nathanielhugospilka/Documents/Thesis/quant_work/output/analyses/reg_results_2023-02-14.smcl /Users/nathanielhugospilka/Documents/Thesis/quant_work/output/analyses/reg_results_2023-02-14.pdf, translator(smcl2pdf)

*******************************************************************************
** regressions - violent and property crime
*******************************************************************************

* violent crime
xtreg logd_violent_crime_rate all_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

* property crime
xtreg logd_property_crime_rate all_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

* creating youth interaction
generate interaction_youth = all_site_type*above_yth_prop_threshold
* violent crime + year/entity FE + interaction (youth)
xtreg logd_violent_crime_rate all_site_type above_yth_prop_threshold interaction_youth ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

test all_site_type + interaction_youth = 0

* violent crime + lowest income quartile
xtreg logd_violent_crime_rate all_site_type below_inc_theshold interaction_inc ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_inc = 0

* property crime + year/entity FE + interaction (youth)
xtreg logd_property_crime_rate all_site_type above_yth_prop_threshold interaction_youth ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

test all_site_type + interaction_youth = 0

* property crime + lowest income quartile
// generate interaction_inc = all_site_type*below_inc_theshold
xtreg logd_property_crime_rate all_site_type below_inc_theshold interaction_inc ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_inc = 0

* violent crime + program type
xtreg logd_violent_crime_rate syep_site_type asp_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

* property crime + program type
xtreg logd_property_crime_rate syep_site_type asp_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

*******************************************************************************
** main regressions - LAGGED/LEAD logged crime_rate
*******************************************************************************

* multivariate + year/entity FE
xtreg lead_logd_crime_rate all_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

* multivariate + year/entity FE + program site type
xtreg lead_logd_crime_rate syep_site_type asp_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

*******************************************************************************
** regressions - Interactions
*******************************************************************************

* creating youth interaction
generate interaction_youth = all_site_type*above_yth_prop_threshold
* multivariate + year/entity FE + interaction (youth)
xtreg lead_logd_crime_rate all_site_type above_yth_prop_threshold interaction_youth ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

test all_site_type + interaction_youth = 0

* creating income interaction
generate interaction_inc = all_site_type*below_inc_theshold
* multivariate + year/entity FE + interaction (income)
xtreg lead_logd_crime_rate all_site_type below_inc_theshold interaction_inc ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

test all_site_type + interaction_inc = 0

*******************************************************************************
** regressions - violent and property crime
*******************************************************************************

* violent crime
xtreg lead_logd_violent_crime_rate all_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

* property crime
xtreg lead_logd_property_crime_rate all_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

* lead violent crime + youth
xtreg lead_logd_violent_crime_rate all_site_type above_yth_prop_threshold interaction_youth ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

test all_site_type + interaction_youth = 0

* violent crime + lowest income quartile
xtreg lead_logd_violent_crime_rate all_site_type below_inc_theshold interaction_inc ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_inc = 0

* lead property crime + youth
xtreg lead_logd_property_crime_rate all_site_type above_yth_prop_threshold interaction_youth ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

test all_site_type + interaction_youth = 0

* property crime + lowest income quartile
// generate interaction_inc = all_site_type*below_inc_theshold
xtreg lead_logd_property_crime_rate all_site_type below_inc_theshold interaction_inc ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)
test all_site_type + interaction_inc = 0

* violent crime + program type
xtreg lead_logd_violent_crime_rate syep_site_type asp_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

* property crime + program type
xtreg lead_logd_property_crime_rate syep_site_type asp_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

*******************************************************************************
** spatial autocorrelation
*******************************************************************************

*******************************************************************************
** setup and clean workspace
*******************************************************************************
* nate spilka
* 2023-03-04

cls
clear all

* set main dir
cd "/Users/nathanielhugospilka/Documents/Thesis/quant_work/data/processed/"

* spatial data location
global spatial_data_location = "/Users/nathanielhugospilka/Documents/Thesis/quant_work/data/processed/"

*******************************************************************************
** working with spatial and non-spatial data
*******************************************************************************

* load/prep spatial data (NYC cut out by block groups)
spshape2dta ${spatial_data_location}thesis_shapefile_2023-03-05, replace

use thesis_shapefile_2023-03-05, clear
spset

bysort geoid: assert _N==1
assert geoid != .

spset geoid, modify replace

save, replace

* Step 7b
use thesis_dataset_2023-03-05, clear

* initiating panel dataset
xtset geoid year
spbalance


*******************************************************************************
** imputaion
*******************************************************************************

* imputing data 
impute median_hh_inc ///
prcnt_unemp ///
 total_pop prcnt_white prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
 prcnt_no_hs_deg prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
 crime_rate violent_crime_rate property_crime_rate, ///
 generate(imputed_mhhi)

* some income imputaions were negative - ADDRESS
replace imputed_mhhi = 0 if imputed_mhhi < 0

impute prcnt_unemp ///
median_hh_inc ///
 total_pop prcnt_white prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
 prcnt_no_hs_deg prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
 num_cmplnt_ttl num_cmplnt_vlnt crime_rate violent_crime_rate, ///
 generate(imputed_prcnt_unemp)

*******************************************************************************
** imputaion
*******************************************************************************



merge m:1 geoid using thesis_shapefile_2023-03-05

keep if _merge == 3
drop _merge

save, replace 

xtset _ID year
spset


* check data
browse
describe
summarize


*******************************************************************************
** setup for spatial analysis (spset, matricies, and spgenerate)
*******************************************************************************

* checking the spatial features
// spmap median_hh_inc using thesis_shapefile_2023-03-05_shp.dta if year == 2019, id(_ID) 
// grmap median_hh_inc if year == 2019, t(2019)

* creating an adjacency matrix for spatially lagged variables 
* (0 = not adjacent; 1 = adjacent)
spmatrix create contiguity adj_mat if year == 2019, replace

* creating an inverse distance matrix for spatially lagged variables 
* (lower values = onjects are spatially closer)
spmatrix create idistance idv_ma, replace

* multivariate + year/entity FE
xtreg logd_crime_rate all_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)


estat moran, errorlag(adj_mat)


spgenerate adj_crime = adj_mat * logd_crime_rate
spgenerate idv_crime = idv_ma * logd_crime_rate




xsmle !!































