clear all
prog drop _all

* IMPORTANT: Change proj_dir to point to the directory on your computer

global proj_dir  "~/Dropbox/World Bank/IEs/Big Data Poverty Estimation/Data/LSMS/RawData"
global raw_dir   "$proj_dir/individual_files"
global final_dir "$proj_dir/appended"

*import PPPs
import delimited "$raw_dir/PPP_conversion_WB.csv", varnames(1) stripquote(no) case(upper) encoding(UTF-8) 
foreach v of varlist V5 V6 V7 V8 V9 {
   local x : variable label `v'
   rename `v' deflateur`x'
}
 reshape long deflateur, i( COUNTRYCODE ) j(year) 
rename COUNTRYCODE country
save "$raw_dir/PPPs.dta", replace

*load items that aren't in individual, consumption, hh or welfare agg
* add consomption module - radio or motorbike
foreach f in "tgo" "civ" "ben" "bfa" {
	use "$raw_dir/`f'/s12_me_`f'2018.dta", clear

	gen has_motorbike=0
	replace  has_motorbike=1 if (s12q01==29 ) & s12q02==1
	gen has_radio=0
	replace has_radio=1 if (s12q01==19 | s12q01==33) & s12q02==1
	
	collapse (max) has_motorbike has_radio, by(vague grappe menage)
	
	* add welfare aggregates
	merge 1:1 vague grappe menage using "$raw_dir/`f'/ehcvm_welfare_`f'2018.dta"
	drop _merge
	
	* add menage level indicators for dwelling chars
	merge 1:1 hhid using "$raw_dir/`f'/ehcvm_menage_`f'2018.dta"
	drop _merge
	di "here ok"
	*add dwelling questionnaire
	merge 1:1 vague grappe menag using "$raw_dir/`f'/s11_me_`f'2018.dta"
	drop _merge
	gen water_source_piped_dwelling=0
	
	decode s11q27b, gen(type)
	replace water_source_piped_dwelling=1 if (strpos(type, "Robinet")>1)
	
	
	* number of people per roo,
	gen n_sleeping_rooms_pp_cat= s11q02/hhsize
	
	* align on dhs names
	ren elec_ac has_electricity
	rename tv has_tv
	rename frigo has_fridge
	rename car has_car
	gen roof_material_cat=3
    replace roof_material_cat=1 if s11q20==4 | s11q20==7
    replace roof_material_cat=2 if s11q20==3 | s11q20==6 |s11q20==8
    gen wall_material_cat=3
    replace wall_material_cat=1 if s11q19==7 | s11q19==6
    replace wall_material_cat=2 if s11q19==3 |s11q19==4

	gen floor_material_cat=3
	replace floor_material_cat=1 if s11q21==3 |  s11q21==4
	replace floor_material_cat=2 if s11q21==2

	gen water_time_to_get_cat=1 
	replace water_time_to_get_cat=2 if s11q29a<30 & s11q29a>15 
	replace water_time_to_get_cat=3 if s11q29a>=30 
    
	
	sum water_time_to_get_cat
	
	gen flush_toilet_sewer=0
	replace flush_toilet_sewer=1 if s11q55==1 | s11q55==2
	rename heduc educ_years_hh_max_scale
	
	keep hhid country year grappe menage vague hhsize hgender hage hmstat hdiploma dtot pcexp zref def_spa def_temp water_source_piped_dwelling n_sleeping_rooms_pp_cat has_electricity has_tv has_fridge has_car floor_material_cat wall_material_cat roof_material_cat educ_years_hh_max_scale flush_toilet_sewer has_radio has_motorbike water_time_to_get_cat
	* add gps
	merge m:1 grappe using "$raw_dir/`f'/grappe_gps_`f'2018.dta"
	drop _merge
	*add PPP_conversion_WB
	
	merge m:1 country year using "$raw_dir/PPPs.dta"
	
	drop _merge 
	gen poverty_measure=pcexp/deflateur
	rename coordonnes_gps__Latitude LAT
	rename coordonnes_gps__Longitude LON
	*save by country and year
	save "$final_dir/clean_with_gps_west_africa/`f'.dta", replace 
	corr has_radio poverty_measure
	
}

foreach f in "tgo" "civ" "ben" {
	append using "$final_dir/clean_with_gps_west_africa/`f'.dta"
	drop if hhid==.
	
	save "$final_dir/clean_with_gps_west_africa/all_countries.dta", replace
}

* adding ethiopia
{

use "$raw_dir/eth/sect11_hh_w4.dta", clear
keep if asset_cd==21 | asset_cd==14 | asset_cd==22 |asset_cd==9 | asset_cd==8

gen has_motorbike=0
replace has_motorbike=1 if asset_cd==21 & s11q00==1
gen has_car=0
replace has_car=1 if asset_cd==22 & s11q00==1
gen has_radio=0
replace has_radio=1 if asset_cd==8 & s11q00==1
gen has_tv=0
replace has_tv=1 if asset_cd==9 & s11q00==1
gen has_fridge=0
replace has_fridge=1 if asset_cd==14 & s11q00==1


collapse (max) has_motorbike has_radio has_car has_fridge has_tv, by(household_id)
	
merge 1:1 household_id using "$raw_dir/eth/cons_agg_w4.dta"
drop food_cons_ann nonfood_cons_ann educ_cons_ann fafh_cons_ann utilities_cons_ann total_cons_ann nom_foodcons_aeq nom_nonfoodcons_aeq nom_educcons_aeq nom_utilities_aeq _merge
gen year =2018
gen country ="ETH"

merge 1:1 household_id using "$raw_dir/eth/ETH_HouseholdGeovariables_Y4.dta"

keep lat_mod lon_mod household_id year country spat* nom* hh_size ea_id saq01 saq14 has_motorbike has_radio has_car has_fridge has_tv

merge 1:1 household_id using "$raw_dir/eth/sect10a_hh_w4.dta"
gen roof_material_cat=3
replace roof_material_cat=1 if s10aq08==3 | s10aq08==4
replace roof_material_cat=2 if s10aq08==6 | s10aq08==5 

gen wall_material_cat=3
replace wall_material_cat=1 if s10aq07==17 | s10aq07==2 |  s10aq07==10
replace wall_material_cat=2 if s10aq07==3 | s10aq07==4 |  s10aq07==5 | s10aq08==12

gen floor_material_cat=3
replace floor_material_cat=1 if s10aq09==1 
replace floor_material_cat=2 if s10aq09==2 | s10aq09==3

gen water_source_piped_dwelling=0 
replace water_source_piped_dwelling=1 if s10aq21==1 | s10aq21==2

gen water_time_to_get_cat=1 if s10aq29==1
replace water_time_to_get_cat=2 if s10aq29<3
replace water_time_to_get_cat=3 if s10aq29>=3

gen flush_toilet_sewer=0
replace flush_toilet_sewer=1 if s10aq12==1 | s10aq12==2
gen has_electricity=0
replace has_electricity=1 if s10aq35>1

gen n_sleeping_rooms_pp_cat=s10aq06/hh_size

keep roof_material_cat wall_material_cat floor_material_cat water_source_piped_dwelling water_time_to_get_cat flush_toilet_sewer has_electricity has_motorbike has_radio has_car has_fridge household_id ea_id saq01 saq14 hh_size nom_totcons_aeq spat_totcons_aeq year country lat_mod lon_mod n_sleeping_rooms_pp_cat has_tv

rename lat_mod LAT
rename lon_mod LON
merge m:1 country year using "$raw_dir/PPPs.dta"

gen poverty_measure=nom_totcons_aeq/deflateur

save "$final_dir/clean_with_gps_east_africa/eth.dta", replace
}

* adding Malawi

{
 use "$raw_dir/mwi/HH_MOD_L.dta", clear
keep if hh_l02==507 | hh_l02==508 | hh_l02==514 |hh_l02==509 | hh_l02==518 | hh_l02==517
gen has_motorbike=0
replace has_motorbike=1 if hh_l02==517 & hh_l03!=.
gen has_car=0
replace has_car=1 if hh_l02==518 & hh_l03!=.
gen has_radio=0
replace has_radio=1 if (hh_l02==507|hh_l02==508) & hh_l03!=.
gen has_tv=0
replace has_tv=1 if hh_l02==509 & hh_l03!=.
gen has_fridge=0
replace has_fridge=1 if hh_l02==514 & hh_l03!=.
collapse (max) has_motorbike has_car has_fridge has_radio has_tv, by(case_id HHID)

merge 1:1 case_id HHID using "$raw_dir/mwi/HH_MOD_F.dta"
gen has_electricity=0
replace has_electricity=1 if hh_f19==1

gen roof_material_cat=3
replace roof_material_cat=1 if hh_f08==1
replace roof_material_cat=2 if hh_f08==5 |hh_f08==2 

gen wall_material_cat=3
replace wall_material_cat=1 if hh_f07<4 
replace wall_material_cat=2 if (hh_f07==5 | hh_f07==7 | hh_f07==8)

gen floor_material_cat=3
replace floor_material_cat=1 if hh_f09<3 
replace floor_material_cat=2 if hh_f09==5 | hh_f09==3

gen water_source_piped_dwelling=0 
replace water_source_piped_dwelling=1 if hh_f36_1<3

gen water_time_to_get_cat=1 if hh_f38_1a<15 & hh_f38b==1
replace water_time_to_get_cat=2 if   hh_f38_1a>15 & hh_f38_1a<30 & hh_f38b==1
replace water_time_to_get_cat=3 if (hh_f38_1a>30 & hh_f38b==1) | hh_f38b==2

gen flush_toilet_sewer=0
replace flush_toilet_sewer=1 if hh_f45==1 | hh_f45==2
keep HHID case_id has_motorbike has_car has_fridge has_tv has_radio floor_material_cat flush_toilet_sewer roof_material_cat wall_material_cat water_source_piped_dwelling water_time_to_get_cat hh_f10 has_electricity

merge 1:1 case_id HHID using "$raw_dir/mwi/ihs5_consumption_aggregate.dta"
gen n_sleeping_rooms_pp_cat=hh_f10/hhsize

drop sdate smonth syear hhsize adulteq hh_wgt rexp_cat021 rexp_cat111 rexp_cat011 rexp_cat101 rexp_cat061 rexp_cat062 rexp_cat063 rexp_cat083 rexp_cat044 rexp_cat022 rexp_cat073 rexp_cat121 rexp_cat081 rexp_cat072 rexp_cat093 rexp_cat031 rexp_cat032 rexp_cat054 rexp_cat123 rexp_cat094 rexp_cat112 rexp_cat052 rexp_cat092 rexp_cat071 rexp_cat041 rexp_cat042 rexp_cat012 rexp_cat045 rexp_cat051 rexp_cat053 rexp_cat055 rexp_cat056 rexp_cat091 rexp_cat095 rexp_cat01 rexp_cat02 rexp_cat03 rexp_cat04 rexp_cat05 rexp_cat06 rexp_cat07 rexp_cat08 rexp_cat09 rexp_cat10 rexp_cat11 rexp_cat12 price_indexL expagg rexpagg _merge

merge 1:1 case_id using "$raw_dir/mwi/householdgeovariables_ihs5.dta"

 drop dist_road dist_agmrkt dist_auction dist_admarc dist_border dist_popcenter dist_boma ssa_aez09 twi_mwi sq1 sq2 sq3 sq4 sq5 sq6 sq7 af_bio_1_x af_bio_8_x af_bio_12_x af_bio_13_x af_bio_16_x popdensity cropshare h2018_tot h2018_wetQstart h2018_wetQ h2019_tot h2019_wetQstart h2019_wetQ anntot_avg wetQ_avgstart wetQ_avg h2018_ndvi_avg h2018_ndvi_max h2019_ndvi_avg h2019_ndvi_max ndvi_avg ndvi_max _merge
 
gen year =2019
gen country ="MWI"

merge m:1 country year using "$raw_dir/PPPs.dta"
rename ea_lat_mod LAT
rename ea_lon_mod LON
drop _merge 
gen poverty_measure=expaggpc/deflateur

save "$final_dir/clean_with_gps_east_africa/mwi.dta", replace
}


use "$final_dir/clean_with_gps_east_africa/mwi.dta"

append using "$final_dir/clean_with_gps_east_africa/eth.dta"


keep if country=="ETH" |  country=="MWI"| country=="BEN"| country=="TGO"| country=="CIV"| country=="BFA"

append using "$final_dir/clean_with_gps_west_africa/all_countries.dta"

save "$final_dir/clean_East_west_africa.dta", replace

