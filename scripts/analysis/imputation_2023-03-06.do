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
use ${data_location}data_to_be_imputed_2023-03-06.dta, clear

browse
describe
summarize

*******************************************************************************
** imputaion
*******************************************************************************

impute median_hh_inc ///
prcnt_unemp ///
 total_pop prcnt_white prcnt_black prcnt_hisp prcnt_asian prcnt_all_other prcnt_yth_yng_adlt ///
 prcnt_no_hs_deg prcnt_hs_no_ba_deg prcnt_ba_or_hghr_deg, ///
 generate(imputed_mhhi)

* some income imputaions were negative - ADDRESS
replace imputed_mhhi = 0 if imputed_mhhi < 0

*******************************************************************************
** send it back to r
*******************************************************************************

export delimited using ${data_location}imputed_data_2023-03-06, replace


















