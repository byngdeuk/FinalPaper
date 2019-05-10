program drop _all

program define sim1
	use "H:\monte carlo.dta", clear

	generat u = uniform()
	generat e = log(u/(1-u))


	generat ystar = -2 + 0.5*vdem_gender + e
	generat y = (ystar >= 0)
	generat lawadopt = y
	replace lawadopt = . if ystar == .
	bysort ccode: gen lawadoptany = sum(lawadopt) >= 1

	generat ystar2 = -3 + 0.5*vdem_gender + e if lawadoptany == 1
	generate y2 = (ystar2 >= 0)
	gen napadopt = y2
	replace napadopt = . if ystar2 == .
	replace napadopt = 0 if lawadoptany == 0
	bysort ccode: gen napadoptany = sum(napadopt) >= 1

	logit napadopt vdem_gender if lawadoptany == 1, cluster(ccode)
end
simulate _b _se, reps(1000): sim1
save "H:\sim1.dta", replace

program drop _all

program define sim2
	use "H:\monte carlo.dta", clear

	generat u = uniform()
	generat e = log(u/(1-u))


	generat ystar = -2 + 0.5*vdem_gender + e
	generat y = (ystar >= 0)
	generat lawadopt = y
	replace lawadopt = . if ystar == .
	bysort ccode: gen lawadoptany = sum(lawadopt) >= 1

	generat ystar2 = -3 + 0.5*vdem_gender + e if lawadoptany == 1
	generate y2 = (ystar2 >= 0)
	gen napadopt = y2
	replace napadopt = . if ystar2 == .
	replace napadopt = 0 if lawadoptany == 0
	bysort ccode: gen napadoptany = sum(napadopt) >= 1

	by ccode: gen lag_lawadoptany = lawadoptany[_n-1]
	by ccode: gen lag_napadoptany = napadoptany[_n-1]
	replace lag_lawadoptany = 0 if lag_lawadoptany == .
	replace lag_napadoptany = 0 if lag_napadoptany == .
	
	mkdyads vdem_gender lawadopt lawadoptany napadopt napadoptany lag_lawadoptany lag_napadoptany, unit(ccode) time(year)

	logit napadopt_01 vdem_gender_01 if ccode_01 != ccode_02, cluster(ccode_01)

end
simulate _b _se, reps(1000): sim2
save "H:\sim2.dta", replace

program drop _all

program define sim3
	use "H:\monte carlo.dta", clear

	generat u = uniform()
	generat e = log(u/(1-u))


	generat ystar = -2 + 0.5*vdem_gender + e
	generat y = (ystar >= 0)
	generat lawadopt = y
	replace lawadopt = . if ystar == .
	bysort ccode: gen lawadoptany = sum(lawadopt) >= 1

	generat ystar2 = -3 + 0.5*vdem_gender + e if lawadoptany == 1
	generate y2 = (ystar2 >= 0)
	gen napadopt = y2
	replace napadopt = . if ystar2 == .
	replace napadopt = 0 if lawadoptany == 0
	bysort ccode: gen napadoptany = sum(napadopt) >= 1

	by ccode: gen lag_lawadoptany = lawadoptany[_n-1]
	by ccode: gen lag_napadoptany = napadoptany[_n-1]
	replace lag_lawadoptany = 0 if lag_lawadoptany == .
	replace lag_napadoptany = 0 if lag_napadoptany == .
	
	mkdyads vdem_gender lawadopt lawadoptany napadopt napadoptany lag_lawadoptany lag_napadoptany, unit(ccode) time(year)

	logit napadopt_01 vdem_gender_01 if ccode_01 != ccode_02 & lag_napadoptany_02 == 1, cluster(ccode_01)

end
simulate _b _se, reps(1000): sim3
save "H:\sim3.dta", replace

program drop _all

program define sim4
	use "H:\monte carlo.dta", clear

	generat u = uniform()
	generat e = log(u/(1-u))


	generat ystar = -2 + 0.5*vdem_gender + e
	generat y = (ystar >= 0)
	generat lawadopt = y
	replace lawadopt = . if ystar == .
	bysort ccode: gen lawadoptany = sum(lawadopt) >= 1

	generat ystar2 = -3 + 0.5*vdem_gender + e if lawadoptany == 1
	generate y2 = (ystar2 >= 0)
	gen napadopt = y2
	replace napadopt = . if ystar2 == .
	replace napadopt = 0 if lawadoptany == 0
	bysort ccode: gen napadoptany = sum(napadopt) >= 1

	by ccode: gen lag_lawadoptany = lawadoptany[_n-1]
	by ccode: gen lag_napadoptany = napadoptany[_n-1]
	replace lag_lawadoptany = 0 if lag_lawadoptany == .
	replace lag_napadoptany = 0 if lag_napadoptany == .
	
	mkdyads vdem_gender lawadopt lawadoptany napadopt napadoptany lag_lawadoptany lag_napadoptany, unit(ccode) time(year)

	logit napadopt_01 vdem_gender_01 if ccode_01 != ccode_02 & lag_napadoptany_02 == 1 & lawadoptany_01 == 1, cluster(ccode_01)

end
simulate _b _se, reps(1000): sim4
save "H:\sim4.dta", replace

*summary and kdensity*
use "H:\sim1.dta", clear
gen teststat = napadopt_b_vdem_gender/ napadopt_se_vdem_gender
sum napadopt_b_vdem_gender napadopt_se_vdem_gender teststat
kdensity napadopt_b_vdem_gender, saving(sim1) 
kdensity teststat, saving(tsim1) 
save "H:\sim1.dta", replace


use "H:\sim2.dta", clear
gen teststat = napadopt_01_b_vdem_gender_01 / napadopt_01_se_vdem_gender_01 
sum napadopt_01_b_vdem_gender_01 napadopt_01_se_vdem_gender_01 teststat
kdensity napadopt_01_b_vdem_gender_01, saving(sim2) 
kdensity teststat, saving(tsim2) 
save "H:\sim2.dta", replace

use "H:\sim3.dta", clear
gen teststat = napadopt_01_b_vdem_gender_01 / napadopt_01_se_vdem_gender_01 
sum napadopt_01_b_vdem_gender_01 napadopt_01_se_vdem_gender_01 teststat
kdensity napadopt_01_b_vdem_gender_01, saving(sim3) 
kdensity teststat, saving(tsim3) 
save "H:\sim3.dta", replace

use "H:\sim4.dta", clear
gen teststat = napadopt_01_b_vdem_gender_01 / napadopt_01_se_vdem_gender_01 
sum napadopt_01_b_vdem_gender_01 napadopt_01_se_vdem_gender_01 teststat
kdensity napadopt_01_b_vdem_gender_01, saving(sim4) 
kdensity teststat, saving(tsim4) 
save "H:\sim4.dta", replace

graph combine sim1.gph sim2.gph sim3.gph sim4.gph
graph combine tsim1.gph tsim2.gph tsim3.gph tsim4.gph

*merge for combined kdensity plots*
use "H:\sim1.dta", clear
gen num = _n
order num
rename napadopt_b_vdem_gender beta1
rename napadopt_b_cons betacon1
rename napadopt_se_vdem_gender se1
rename napadopt_se_cons secon1
rename teststat teststat1

save "H:\sim1formerge.dta", replace

use "H:\sim2.dta", clear
gen num = _n
order num
rename napadopt_01_b_vdem_gender_01 beta2
rename napadopt_01_b_cons betacon2
rename napadopt_01_se_vdem_gender_01 se2
rename napadopt_01_se_cons secon2
rename teststat teststat2
save "H:\sim2formerge.dta", replace

use "H:\sim3.dta", clear
gen num = _n
order num
rename napadopt_01_b_vdem_gender_01 beta3
rename napadopt_01_b_cons betacon3
rename napadopt_01_se_vdem_gender_01 se3
rename napadopt_01_se_cons secon3
rename teststat teststat3
save "H:\sim3formerge.dta", replace

use "H:\sim4.dta", clear
gen num = _n
order num
rename napadopt_01_b_vdem_gender_01 beta4
rename napadopt_01_b_cons betacon4
rename napadopt_01_se_vdem_gender_01 se4
rename napadopt_01_se_cons secon4
rename teststat teststat4
save "H:\sim4formerge.dta", replace

use "H:\sim1formerge.dta", clear
merge m:m num using "H:\sim2formerge.dta"
drop _merge
merge m:m num using "H:\sim3formerge.dta"
drop _merge
merge m:m num using "H:\sim4formerge.dta"
drop _merge
save "H:\simmerged.dta", replace

*MCMC Simulation: Simulated Beta*
use "H:\simmerged.dta", clear
sum beta1 se1 teststat1 beta2 se2 teststat2 beta3 se3 teststat3 beta4 se4 teststat4

kdensity beta1, nograph generate(x fx)
kdensity beta1, nograph generate(fx1) at(x)
kdensity beta2, nograph generate(fx2) at(x)
kdensity beta3, nograph generate(fx3) at(x)
kdensity beta4, nograph generate(fx4) at(x)
label var fx1 "Reference"
label var fx2 "Dyadic Unconditioned"
label var fx3 "Dyadic Conditioned"
label var fx4 "Dyadic Diffusion with Steps"
line fx1 fx3 fx4 x, xline(.1276298) xline(.4967278) xline(.2857486)
graph save Graph "H:\Kdensity(beta).gph", replace
drop fx x fx1 fx2 fx3 fx4


kdensity beta1, nograph generate(x fx)
kdensity beta1, nograph generate(fx1) at(x)
kdensity beta2, nograph generate(fx2) at(x)
kdensity beta3, nograph generate(fx3) at(x)
kdensity beta4, nograph generate(fx4) at(x)
label var fx1 "Reference"
label var fx2 "Dyadic Unconditioned"
label var fx3 "Dyadic Conditioned"
label var fx4 "Dyadic Diffusion with Steps"
line fx1 fx3 fx4 x, xline(.1276298) xline(.4967278) xline(.2857486)
graph save Graph "H:\Kdensity(beta).gph", replace
drop fx x fx1 fx2 fx3 fx4

*MCMC Simulation: Simulated T-stat*
kdensity teststat1, nograph generate(x fx)
kdensity teststat1, nograph generate(fx1) at(x)
kdensity teststat2, nograph generate(fx2) at(x)
kdensity teststat3, nograph generate(fx3) at(x)
kdensity teststat4, nograph generate(fx4) at(x)
label var fx1 "Reference"
label var fx2 "Dyadic Unconditioned"
label var fx3 "Dyadic Conditioned"
label var fx4 "Dyadic Diffusion with Steps"
line fx1 fx3 fx4 x, xline(.6300033) xline(2.236967) xline(1.279413)
graph save Graph "H:\Kdensity(teststat).gph", replace
drop fx x fx1 fx2 fx3 fx4


kdensity teststat1, nograph generate(x fx)
kdensity teststat1, nograph generate(fx1) at(x)
kdensity teststat2, nograph generate(fx2) at(x)
kdensity teststat3, nograph generate(fx3) at(x)
kdensity teststat4, nograph generate(fx4) at(x)
label var fx1 "Reference"
label var fx2 "Dyadic Unconditioned"
label var fx3 "Dyadic Conditioned"
label var fx4 "Dyadic Diffusion with Steps"
line fx1 fx3 fx4 x, xline(.6300033) xline(2.236967) xline(1.279413)
graph save Graph "H:\Kdensity(teststat).gph", replace
drop fx x fx1 fx2 fx3 fx4
