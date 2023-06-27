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
** 
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

* ------

* creating an adjacency matrix for spatially lagged variables 
* (0 = not adjacent; 1 = adjacent)
spmatrix create contiguity adj_mat if year == 2019, replace
// spmatrix export adj_mat using adj_mat.txt
spmatrix summarize adj_mat
regress logd_crime_rate if year == 2019
estat moran, errorlag(adj_mat)

* ------

* creating an inverse distance matrix for spatially lagged variables 
* (lower values = onjects are spatially closer)
spmatrix create idistance idv_mat if year == 2019, replace
// spmatrix export idv_mat using idv_mat.txt
spmatrix summarize idv_mat
estat moran, errorlag(idv_mat)

* ------

* regression
spxtregress logd_crime_rate all_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
, fe dvarlag(adj_mat) errorlag(adj_mat)

* ------

* multivariate + year/entity FE
xtreg logd_crime_rate all_site_type ///
imputed_mhhi imputed_prcnt_unemp ///
total_pop /* prcnt_white */ prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
/*prcnt_hs_no_ba_deg */ prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg ///
i.year, i(geoid) fe vce(robust)

xtreg logd_crime_rate all_site_type i.year, i(geoid) fe vce(robust)

xtmoran logd_crime_rate, wname(adj_mat)

mat summarize adj_mat

spgenerate idv_crime = idv_ma * logd_crime_rate




estat impact all_site_type if year == 2019




























