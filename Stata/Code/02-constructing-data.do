* RRF 2025 - Construction Data Solutions	
*-------------------------------------------------------------------------------	
* Data construction: HH 
*------------------------------------------------------------------------------- 

	use "${data}/Intermediate/TZA_CCT_HH.dta", clear
	
	* Area in acre
	// Equal to area farm if unit is acres, 
	// otherwise multiplied by value of hectare in acres
	
	global acre_conv 2.47
	
	di $acre_conv
	
	generate 	area_acre = ar_farm 				if ar_unit == 1 , after(ar_farm)
	replace 	area_acre = ar_farm * $acre_conv 	if ar_unit == 2
	
	lab var		area_acre "Area farmed in acres"
	
	* Consumption in usd
	global usd 0.00037
	
	foreach cons_var in food_cons nonfood_cons {
		
		* Save labels 
		local `cons_var'_lab: variable label `cons_var'
		
		* generate vars
		gen `cons_var'_usd = `cons_var' * $usd , after(`cons_var')
		
		* apply labels to new variables
		lab var `cons_var'_usd "``cons_var'_lab' (USD)"
		
	}
	
	
	* Winsorize variables with outliers 
	local winvars area_acre food_cons_usd nonfood_cons_usd
	
	foreach win_var of local winvars {
		
		local `win_var'_lab: variable label `win_var'
		
		winsor 	`win_var', p(0.05) high gen(`win_var'_w)
		order 	`win_var'_w, after(`win_var')
		lab var `win_var'_w "``win_var'_lab' (Winsorized 0.05)"
		
	}
	
	tempfile	 hh
	save 		`hh'
	
*-------------------------------------------------------------------------------	
* Data construction: HH - mem
*------------------------------------------------------------------------------- 	

	use "${data}/Intermediate/TZA_CCT_HH_mem.dta", clear
	
	* Collapse to hh level for total treatment cost
	// any member sick, can read/write
	// average sick days
	collapse 	(sum) treat_cost ///
				(max) read sick ///
				(mean) m_cost = treat_cost days_sick, by(hhid)
				
	replace treat_cost = m_cost if mi(m_cost)		
	
	* Cost in USD
	gen treat_cost_usd = treat_cost * $usd

	* Add labels
	lab var read 				"Any member can read/write"
	lab var sick 				"Any member was sick in the last 4 weeks"
	lab var days_sick 			"Average sick days"
	lab var treat_cost_usd 		"Total cost of treatment (USD)"
	
	drop treat_cost m_cost 
	
	tempfile mem 
	save 	`mem'
	
	
*-------------------------------------------------------------------------------	
* Data construction: merge all hh datasets
*------------------------------------------------------------------------------- 	
	
	* Start with hh level 
	use `hh', clear 
	
	* merge member data 
	merge 1:1 hhid using `mem', assert(3) nogen 
		
	* merge treatment 
	merge m:1 vid using "${data}/Raw/treat_status.dta", assert(3) nogen 
	
	* Save data
	save "${data}/Final/TZA_CCT_analysis.dta", replace

*-------------------------------------------------------------------------------	
* Data construction: Secondary data
*------------------------------------------------------------------------------- 	
	
	use "${data}/Intermediate/TZA_amenity_tidy.dta", clear
	
	* Total medical facilities 
	egen n_medical = rowtotal(n_clinic n_hospital)
	
	lab var n_medical "No. of medical facilities"

	* Save data
	save "${data}/Final/TZA_amenity_analysis.dta", replace

	
*************************************************************************** end!
	
