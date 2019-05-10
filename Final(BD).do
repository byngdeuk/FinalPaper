* Make dyadic relationship *
use "H:\replication.dta", clear
sort ccode year
by ccode: gen lag_L_any = L_any[_n-1]
by ccode: gen lag_CEDAW = CEDAW[_n-1]
by ccode: gen lag_percentage_CEDAW = percentage_CEDAW[_n-1]
by ccode: gen lag_NAP_any = NAP_any[_n-1]
drop if year < 1908
tab lag_L_any
tab lag_L_any, missing
replace lag_L_any = 0 if lag_L_any ==.
replace lag_CEDAW = 0 if lag_CEDAW ==.
replace lag_percentage_CEDAW = 0 if lag_percentage_CEDAW == .
replace lag_NAP_any = 0 if lag_NAP_any == .

* make more than one adoptions equals to one adoption *
recode L_Femicide (2 3 = 1)
recode L_forced_sterilization (2 3 = 1)
recode L_stalking (2 3 = 1)
recode L_property (2 3 = 1)
recode L_violence_against_women (2 3 = 1)
recode L_domestic (2 3 = 1)
recode L_sexual_violence (2 3 = 1)
recode L_new (2 3 = 1)
recode L_sexual_harassment (2 3 = 1)
recode L_FGM (2 3 = 1)
recode L_trafficking (2 3 = 1)
recode L_child_ealry_forced (2 3 = 1)
recode NAP_Femicide (2 3 = 1)
recode NAP_forced_sterilization (2 3 = 1)
recode NAP_stalking (2 3 = 1)
recode NAP_property (2 3 = 1)
recode NAP_violence_against_women (2 3 = 1)
recode NAP_domestic (2 3 = 1)
recode NAP_sexual_violence (2 3 = 1)
recode NAP_new (2 3 = 1)
recode NAP_sexual_harassment (2 3 = 1)
recode NAP_FGM (2 3 = 1)
recode NAPL_trafficking (2 3 = 1)
recode NAPL_child_ealry_forced (2 3 = 1)
recode lag_L_Feminicide (2 3 = 1)
recode lag_L_forced_sterilization (2 3 = 1)
recode lag_L_stalking (2 3 = 1)
recode lag_L_property (2 3 = 1)
recode lag_L_violence_against_women (2 3 = 1)
recode lag_L_domestic (2 3 = 1)
recode lag_L_sexual_violence (2 3 = 1)
recode lag_L_new (2 3 = 1)
recode lag_L_sexual_harassment (2 3 = 1)
recode lag_L_FGM (2 3 = 1)
recode lag_L_trafficking (2 3 = 1)
recode lag_L_child_ealry_forced (2 3 = 1)
recode lag_NAP_Femicide (2 3 = 1)
recode lag_NAP_forced_sterilization (2 3 = 1)
recode lag_NAP_stalking (2 3 = 1)
recode lag_NAP_property (2 3 = 1)
recode lag_NAP_violence_against_women (2 3 = 1)
recode lag_NAP_domestic (2 3 = 1)
recode lag_NAP_sexual_violence (2 3 = 1)
recode lag_NAP_new (2 3 = 1)
recode lag_NAP_sexual_harassment (2 3 = 1)
recode lag_NAP_FGM (2 3 = 1)
recode lag_NAPL_trafficking (2 3 = 1)
recode lag_NAPL_child_ealry_forced (2 3 = 1)

rename lag_NAP_violence_against_women lag_NAPviolenceagainstwomen
rename lag_NAPL_child_ealry_forced lag_NAP_childearlyforced
rename lag_Flaborforceparticipation15 lag_Flfparticipation15
rename lag_Flaborforceparticipatn15NAT lag_FlfparticipationNAT
rename lag_Flaborforceparticipatin1564 lag_Flfparticipation1564

replace lag_Femployment15ILO = . if lag_Femployment15ILO==0
replace lag_Femployment15NAT = . if lag_Flfparticipation15 ==0
replace lag_Flfparticipation15 = . if lag_Flfparticipation15 ==0
replace lag_FlfparticipationNAT = . if lag_FlfparticipationNAT ==0
replace lag_Flfparticipation1564 = . if lag_Flfparticipation1564 ==0
replace lag_Fprimarygross = . if lag_Fprimarygross ==0
replace lag_Fprimarynet = . if lag_Fprimarynet ==0
replace lag_Fsecondarygross = . if lag_Fsecondarygross ==0
replace lag_Fsecondarynet = . if lag_Fsecondarynet ==0
replace lag_FunemploymentILO = . if lag_FunemploymentILO ==0
replace lag_FunemploymentNAT = . if lag_FunemploymentNAT ==0
replace lag_FwageILO = . if lag_FwageILO ==0
replace lag_Fparliaments = . if lag_Fparliaments ==0

gen time2 = time*time
gen time3 = time*time*time
gen NAP_time = year - 1988
gen NAP_time2 = NAP_time*NAP_time
gen NAP_time3 = NAP_time*NAP_time*NAP_time


* rescale cgdppc *
gen rcgdppc = cgdppc/10000
gen rlag_cgdppc = lag_cgdppc/10000
drop cgdppc
drop lag_cgdppc
rename rcgdppc cgdppc
rename rlag_cgdppc lag_cgdppc

****************Table 1: Descriptive Table*********************
sum L_any NAP_any lag_percentage_CEDAW lag_CEDAW lag_cgdppc lag_actotal lag_polity2 lag_vdem_gender  DL_ht_region DL_ht_colonial DL_lp_legor DNAP_ht_region DNAP_ht_colonial DNAP_lp_legor lag_Femployment15ILO lag_Fprimarygross lag_Fparliaments
***************************************************************

* monadic repeated event history analysis *
*any laws*
logit L_any lag_vdem_gender lag_actotal lag_polity2 lag_CEDAW lag_cgdppc DL_ht_region DL_ht_colonial DL_lp_legor lag_percentage_CEDAW time time2 time3, cluster(ccode)
estimate store mona_any
estat ic

logit L_any lag_vdem_gender lag_actotal lag_polity2 lag_CEDAW lag_cgdppc time time2 time3, cluster(ccode)
estimate store mona_anywithoutdiffusion
estat ic

*by components only for n>30*
gen time_stalking = year - 1924
gen time_stalking2 = time_stalking*time_stalking
gen time_stalking3 = time_stalking*time_stalking*time_stalking
logit L_stalking lag_percentage_CEDAW lag_CEDAW lag_cgdppc  lag_actotal lag_polity2 lag_vdem_gender stalking_ht_region stalking_ht_colonial stalking_lp_legor time_stalking time_stalking2 time_stalking3 if year > 1923, cluster(ccode)
estimate store mona_stalking

gen time_vaw = year - 1959
gen time_vaw2 = time_vaw*time_vaw
gen time_vaw3 = time_vaw*time_vaw*time_vaw
logit L_violence_against_women lag_percentage_CEDAW lag_CEDAW lag_cgdppc  lag_actotal lag_polity2 lag_vdem_gender vaw_ht_region vaw_ht_colonial vaw_lp_legor time_vaw time_vaw2 time_vaw3 if year > 1958, cluster(ccode)
estimate store mona_vaw

gen time_domestic = year - 1973
gen time_domestic2 = time_domestic*time_domestic
gen time_domestic3 = time_domestic*time_domestic*time_domestic
logit L_domestic lag_percentage_CEDAW lag_CEDAW lag_cgdppc  lag_actotal lag_polity2 lag_vdem_gender domestic_ht_region domestic_ht_colonial domestic_lp_legor time_domestic time_domestic2 time_domestic3 if year > 1972, cluster(ccode)
estimate store mona_domestic

logit L_sexual_violence lag_percentage_CEDAW lag_CEDAW lag_cgdppc  lag_actotal lag_polity2 lag_vdem_gender sv_ht_region sv_ht_colonial sv_lp_legor time time2 time3, cluster(ccode)
estimate store mona_sexualviolence

gen time_sh = year - 1958
gen time_sh2 = time_sh*time_sh
gen time_sh3 = time_sh*time_sh*time_sh
logit L_sexual_harassment lag_percentage_CEDAW lag_CEDAW lag_cgdppc  lag_actotal lag_polity2 lag_vdem_gender sh_ht_region sh_ht_colonial sh_lp_legor time_sh time_sh2 time_sh3 if year > 1957, cluster(ccode)
estimate store mona_sexualharassment

gen time_FGM = year - 1966
gen time_FGM2 = time_FGM*time_FGM
gen time_FGM3 = time_FGM*time_FGM*time_FGM
logit L_FGM lag_percentage_CEDAW  lag_cgdppc lag_CEDAW lag_actotal lag_polity2 lag_vdem_gender FGM_ht_region FGM_ht_colonial FGM_lp_legor time_FGM time_FGM2 time_FGM3 if year > 1965, cluster(ccode)
estimate store mona_FGM

gen time_trafficking = year - 1956
gen time_trafficking2 = time_trafficking*time_trafficking
gen time_trafficking3 = time_trafficking*time_trafficking*time_trafficking
logit L_trafficking lag_percentage_CEDAW lag_CEDAW lag_cgdppc  lag_actotal lag_polity2 lag_vdem_gender trafficking_ht_region trafficking_ht_colonial trafficking_lp_legor time_trafficking time_trafficking2 time_trafficking3 if year > 1955, cluster(ccode)
estimate store mona_trafficking

gen time_CEF = year - 1961
gen time_CEF2 = time_CEF*time_CEF
gen time_CEF3 = time_CEF*time_CEF*time_CEF
logit L_child_ealry_forced lag_percentage_CEDAW lag_CEDAW lag_cgdppc  lag_actotal lag_polity2 lag_vdem_gender CEF_ht_region CEF_ht_colonial CEF_lp_legor time_CEF time_CEF2 time_CEF3 if year > 1960, cluster(ccode)
estimate store mona_early

esttab mona_any mona_stalking mona_vaw mona_domestic mona_sexualviolence mona_sexualharassment mona_FGM mona_trafficking mona_early, se b(3)

* monadic repeated event history analysis for NAP *
*without considering diffusion-diffusion*
logit NAP_any lag_vdem_gender lag_actotal lag_polity2 lag_CEDAW lag_cgdppc DNAP_ht_region DNAP_ht_colonial DNAP_lp_legor lag_percentage_CEDAW NAP_time NAP_time2 NAP_time3 if year > 1987, cluster(ccode)
estimate store mona_NAP
estat ic

*with considering diffusion-diffusion*
logit NAP_any lag_percentage_CEDAW lag_CEDAW lag_cgdppc lag_actotal lag_polity2 lag_vdem_gender DNAP_ht_region DNAP_ht_colonial DNAP_lp_legor NAP_time NAP_time2 NAP_time3 if year > 1987 & L_has_any == 1, cluster(ccode)
estimate store mona_dd
estat ic

logit NAP_any lag_CEDAW lag_cgdppc lag_actotal lag_polity2 lag_vdem_gender NAP_time NAP_time2 NAP_time3 if year > 1987 & L_has_any == 1, cluster(ccode)
estimate store mona_ddwithoutdiffusion
estat ic

esttab mona_NAP mona_dd, se b(3)

*vif*
reg L_any lag_vdem_gender lag_actotal lag_polity2 lag_CEDAW lag_cgdppc DL_ht_region DL_ht_colonial DL_lp_legor lag_percentage_CEDAW time time2 time3, cluster(ccode)
vif

reg NAP_any lag_percentage_CEDAW lag_CEDAW lag_cgdppc lag_actotal lag_polity2 lag_vdem_gender DNAP_ht_region DNAP_ht_colonial DNAP_lp_legor NAP_time NAP_time2 NAP_time3 if year > 1987 & L_has_any == 1, cluster(ccode)
vif

***stcox***
stset NAP_time, id(ccode) failure(NAP_any)
stcox lag_percentage_CEDAW lag_CEDAW lag_cgdppc lag_actotal lag_polity2 lag_vdem_gender DNAP_ht_region DNAP_ht_colonial DNAP_lp_legor NAP_time NAP_time2 NAP_time3, cluster(ccode) nohr
estimate store stcox1

stcox lag_percentage_CEDAW lag_CEDAW lag_cgdppc lag_actotal lag_polity2 lag_Fparliaments lag_Femployment15ILO lag_Fprimarygross DNAP_ht_region DNAP_ht_colonial DNAP_lp_legor NAP_time NAP_time2 NAP_time3, cluster(ccode) nohr
estimate store stcox2

stset, clear


* monadic repeated event history analysis for NAP, instead of vdem_gender *
*without considering diffusion-diffusion*
logit NAP_any lag_Fparliaments lag_Femployment15ILO lag_Fprimarygross  lag_percentage_CEDAW lag_CEDAW lag_cgdppc lag_actotal lag_polity2 DNAP_ht_region DNAP_ht_colonial DNAP_lp_legor NAP_time NAP_time2 NAP_time3 if year > 1987, cluster(ccode)
estimate store mona_NAP_no
margins, at(lag_Fparliaments=(0(10)70)) atmeans post
estimate store margins0

*with considering diffusion-diffusion*
logit NAP_any lag_Femployment15ILO lag_Fprimarygross lag_Fparliaments lag_percentage_CEDAW lag_CEDAW lag_cgdppc lag_actotal lag_polity2 DNAP_ht_region DNAP_ht_colonial DNAP_lp_legor NAP_time NAP_time2 NAP_time3 if year > 1987 & L_has_any == 1, cluster(ccode)
estimate store mona_dd_no
margins, at(lag_Fparliaments=(0(10)70)) atmeans post
estimate store margins00

logit NAP_any lag_Femployment15ILO lag_Fprimarygross lag_Fparliaments lag_CEDAW lag_cgdppc lag_actotal lag_polity2 NAP_time NAP_time2 NAP_time3 if year > 1987 & L_has_any == 1, cluster(ccode)
estimate store mona_dd_nowithoutdiffusion

esttab mona_NAP_no mona_dd_no, se b(3)

*vif*
reg NAP_any lag_Femployment15ILO lag_Fprimarygross lag_Fparliaments lag_percentage_CEDAW lag_CEDAW lag_cgdppc lag_actotal lag_polity2 DNAP_ht_region DNAP_ht_colonial DNAP_lp_legor NAP_time NAP_time2 NAP_time3 if year > 1987 & L_has_any == 1, cluster(ccode)
vif

*make dyads*
mkdyads stateabb L_Femicide L_forced_sterilization L_stalking L_property L_violence_against_women L_domestic L_sexual_violence L_new L_sexual_harassment L_FGM L_trafficking L_child_ealry_forced NAP_Femicide NAP_forced_sterilization NAP_stalking NAP_property NAP_violence_against_women NAP_domestic NAP_sexual_violence NAP_new NAP_sexual_harassment NAP_FGM NAPL_trafficking NAPL_child_ealry_forced lag_L_Feminicide lag_L_forced_sterilization lag_L_stalking lag_L_property lag_L_violence_against_women lag_L_domestic lag_L_sexual_violence lag_L_new lag_L_sexual_harassment lag_L_FGM lag_L_trafficking lag_L_child_ealry_forced lag_NAP_Femicide lag_NAP_forced_sterilization lag_NAP_stalking lag_NAP_property lag_NAPviolenceagainstwomen lag_NAP_domestic lag_NAP_sexual_violence lag_NAP_new lag_NAP_sexual_harassment lag_NAP_FGM lag_NAPL_trafficking lag_NAP_childearlyforced L_num NAP_num L_any lag_L_any NAP_any L_has_num NAP_has_num L_has_any NAP_has_any lag_L_has_any lag_NAP_has_any lag_L_has_num lag_NAP_has_num lag_polity2 lag_cgdppc lag_vdem_gender lag_actotal CEDAW lag_CEDAW DL_ht_region DNAP_ht_region DL_ht_colonial DNAP_ht_colonial DL_lp_legor DNAP_lp_legor percentage_CEDAW lag_percentage_CEDAW lag_NAP_any stalking_ht_region vaw_ht_region domestic_ht_region sv_ht_region sh_ht_region FGM_ht_region trafficking_ht_region CEF_ht_region stalking_ht_colonial vaw_ht_colonial domestic_ht_colonial sv_ht_colonial sh_ht_colonial FGM_ht_colonial trafficking_ht_colonial CEF_ht_colonial stalking_lp_legor vaw_lp_legor domestic_lp_legor sv_lp_legor sh_lp_legor FGM_lp_legor trafficking_lp_legor CEF_lp_legor lag_Femployment15ILO lag_Femployment15NAT lag_Flfparticipation15 lag_FlfparticipationNAT lag_Flfparticipation1564 lag_Fprimarygross lag_Fprimarynet lag_Fsecondarygross lag_Fsecondarynet lag_FunemploymentILO lag_FunemploymentNAT lag_FwageILO lag_Fparliaments  lag_L_stalking_any lag_L_vaw_any lag_L_domestic_any lag_L_sv_any lag_L_sh_any lag_L_FGM_any lag_L_trafficking_any lag_L_CEF_any, unit(ccode) time(year)
gen time = year - 1908
gen time2 = time*time
gen time3 = time*time*time

gen NAP_time = year - 1988
gen NAP_time2 = NAP_time*NAP_time
gen NAP_time3 = NAP_time*NAP_time*NAP_time

* gen difference control *
gen gdppc_diff = lag_cgdppc_01- lag_cgdppc_02
gen gdppc_diff_abs = abs(gdppc_diff)
gen polity2_diff = lag_polity2_01- lag_polity2_02
gen polity2_diff_abs = abs(polity2_diff)

* gen emulation any laws variable *
gen L_any_emulation = .
replace L_any_emulation = 0 if lag_L_any_02 == 0
replace L_any_emulation = 0 if L_any_01 == 0
replace L_any_emulation = 1 if L_any_emulation == .

* by components *
gen L_femicide_emulation = .
replace L_femicide_emulation = 0 if lag_L_Feminicide_02 == 0
replace L_femicide_emulation = 0 if L_Femicide_01 == 0
replace L_femicide_emulation = 1 if L_femicide_emulation == .

gen L_forcedsterilization_emulation = .
replace L_forcedsterilization_emulation = 0 if lag_L_forced_sterilization_02 == 0
replace L_forcedsterilization_emulation = 0 if L_forced_sterilization_01 == 0
replace L_forcedsterilization_emulation = 1 if L_forcedsterilization_emulation == .

gen L_stalking_emulation = .
replace L_stalking_emulation = 0 if lag_L_stalking_02 == 0
replace L_stalking_emulation = 0 if L_stalking_01 == 0
replace L_stalking_emulation = 1 if L_stalking_emulation == .

gen L_property_emulation = .
replace L_property_emulation = 0 if lag_L_property_02 == 0
replace L_property_emulation = 0 if L_property_01 == 0
replace L_property_emulation = 1 if L_property_emulation == .

gen L_violenceagainstwomen_emulation = .
replace L_violenceagainstwomen_emulation = 0 if lag_L_violence_against_women_02 == 0
replace L_violenceagainstwomen_emulation = 0 if L_violence_against_women_01 == 0
replace L_violenceagainstwomen_emulation = 1 if L_violenceagainstwomen_emulation == .

gen L_domestic_emulation = .
replace L_domestic_emulation = 0 if lag_L_domestic_02 == 0
replace L_domestic_emulation = 0 if L_domestic_01 == 0
replace L_domestic_emulation = 1 if L_domestic_emulation == .

gen L_sexual_violence_emulation = .
replace L_sexual_violence_emulation = 0 if lag_L_sexual_violence_02 == 0
replace L_sexual_violence_emulation = 0 if L_sexual_violence_01 == 0
replace L_sexual_violence_emulation = 1 if L_sexual_violence_emulation == .

gen L_new_emulation = .
replace L_new_emulation = 0 if lag_L_new_02 == 0
replace L_new_emulation = 0 if L_new_01 == 0
replace L_new_emulation = 1 if L_new_emulation == .

gen L_sexual_harassment_emulation = .
replace L_sexual_harassment_emulation = 0 if lag_L_sexual_harassment_02 == 0
replace L_sexual_harassment_emulation = 0 if L_sexual_harassment_01 == 0
replace L_sexual_harassment_emulation = 1 if L_sexual_harassment_emulation == .

gen L_FGM_emulation = .
replace L_FGM_emulation = 0 if lag_L_FGM_02 == 0
replace L_FGM_emulation = 0 if L_FGM_01 == 0
replace L_FGM_emulation = 1 if L_FGM_emulation == .

gen L_trafficking_emulation = .
replace L_trafficking_emulation = 0 if lag_L_trafficking_02 == 0
replace L_trafficking_emulation = 0 if L_trafficking_01 == 0
replace L_trafficking_emulation = 1 if L_trafficking_emulation == .

gen L_child_ealry_forced_emulation = .
replace L_child_ealry_forced_emulation = 0 if lag_L_child_ealry_forced_02 == 0
replace L_child_ealry_forced_emulation = 0 if L_child_ealry_forced_01 == 0
replace L_child_ealry_forced_emulation = 1 if L_child_ealry_forced_emulation == .


* run model for any law - unconditioned *
logit L_any_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 DL_ht_region_01 DL_ht_colonial_01 DL_lp_legor_01 time time2 time3 if ccode_01 != ccode_02, cluster(ccode_01)
estimate store un1

* by components *
gen time_stalking = year - 1924
gen time_stalking2 = time_stalking*time_stalking
gen time_stalking3 = time_stalking*time_stalking*time_stalking

gen time_vaw = year - 1959
gen time_vaw2 = time_vaw*time_vaw
gen time_vaw3 = time_vaw*time_vaw*time_vaw

gen time_domestic = year - 1973
gen time_domestic2 = time_domestic*time_domestic
gen time_domestic3 = time_domestic*time_domestic*time_domestic

gen time_sh = year - 1958
gen time_sh2 = time_sh*time_sh
gen time_sh3 = time_sh*time_sh*time_sh

gen time_FGM = year - 1966
gen time_FGM2 = time_FGM*time_FGM
gen time_FGM3 = time_FGM*time_FGM*time_FGM

gen time_trafficking = year - 1956
gen time_trafficking2 = time_trafficking*time_trafficking
gen time_trafficking3 = time_trafficking*time_trafficking*time_trafficking

gen time_CEF = year - 1961
gen time_CEF2 = time_CEF*time_CEF
gen time_CEF3 = time_CEF*time_CEF*time_CEF

 

logit L_stalking_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 stalking_ht_region_01 stalking_ht_colonial_01 stalking_lp_legor_01 time_stalking time_stalking2 time_stalking3 if ccode_01 != ccode_02 & year > 1923, cluster(ccode_01)
estimate store stalking_unemul1
logit L_stalking_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 stalking_ht_region_01 stalking_ht_colonial_01 stalking_lp_legor_01 time_stalking time_stalking2 time_stalking3 if ccode_01 != ccode_02 & year > 1923, cluster(ccode_01)
estimate store stalking_unemul2

logit L_violenceagainstwomen_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 vaw_ht_region_01 vaw_ht_colonial_01 vaw_lp_legor_01 time_vaw time_vaw2 time_vaw3 if ccode_01 != ccode_02 & year > 1958, cluster(ccode_01)
estimate store vaw_unemul1
logit L_violenceagainstwomen_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 vaw_ht_region_01 vaw_ht_colonial_01 vaw_lp_legor_01 time_vaw time_vaw2 time_vaw3 if ccode_01 != ccode_02 & year > 1958, cluster(ccode_01)
estimate store vaw_unemul2

logit L_domestic_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 domestic_ht_region_01 domestic_ht_colonial_01 domestic_lp_legor_01 time_domestic time_domestic2 time_domestic3 if ccode_01 != ccode_02 & year > 1972, cluster(ccode_01)
estimate store domestic_unemul1
logit L_domestic_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 domestic_ht_region_01 domestic_ht_colonial_01 domestic_lp_legor_01 time_domestic time_domestic2 time_domestic3 if ccode_01 != ccode_02 & year > 1972, cluster(ccode_01)
estimate store domestic_unemul2

logit L_sexual_violence_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 sv_ht_region_01 sv_ht_colonial_01 sv_lp_legor_01 time time2 time3 if ccode_01 != ccode_02, cluster(ccode_01)
estimate store sv_unemul1
logit L_sexual_violence_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 sv_ht_region_01 sv_ht_colonial_01 sv_lp_legor_01 time time2 time3 if ccode_01 != ccode_02, cluster(ccode_01)
estimate store sv_unemul2

logit L_sexual_harassment_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 sh_ht_region_01 sh_ht_colonial_01 sh_lp_legor_01 time_sh time_sh2 time_sh3 if ccode_01 != ccode_02 & year > 1957, cluster(ccode_01)
estimate store sh_unemul1
*logit L_sexual_harassment_emulation lag_percentage_CEDAW_01 CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 sh_ht_region_01 sh_ht_colonial_01 sh_lp_legor_01 time_sh time_sh2 time_sh3 if ccode_01 != ccode_02 & year > 1957, cluster(ccode_01)
*estimate store sh_unemul2

logit L_FGM_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 FGM_ht_region_01 FGM_ht_colonial_01 FGM_lp_legor_01 time_FGM time_FGM2 time_FGM3 if ccode_01 != ccode_02 & year > 1965, cluster(ccode_01)
estimate store FGM_unemul1
logit L_FGM_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 FGM_ht_region_01 FGM_ht_colonial_01 FGM_lp_legor_01 time_FGM time_FGM2 time_FGM3 if ccode_01 != ccode_02 & year > 1965, cluster(ccode_01)
estimate store FGM_unemul2

logit L_trafficking_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 trafficking_ht_region_01 trafficking_ht_colonial_01 trafficking_lp_legor_01 time_trafficking time_trafficking2 time_trafficking3 if ccode_01 != ccode_02 & year > 1955, cluster(ccode_01)
estimate store trafficking_unemul1
logit L_trafficking_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 trafficking_ht_region_01 trafficking_ht_colonial_01 trafficking_lp_legor_01 time_trafficking time_trafficking2 time_trafficking3 if ccode_01 != ccode_02 & year > 1955, cluster(ccode_01)
estimate store trafficking_unemul2

logit L_child_ealry_forced_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 CEF_ht_region_01 CEF_ht_colonial_01 CEF_lp_legor_01 time_CEF time_CEF2 time_CEF3 if ccode_01 != ccode_02 & year > 1960, cluster(ccode_01)
estimate store CEF_unemul1
logit L_child_ealry_forced_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 CEF_ht_region_01 CEF_ht_colonial_01 CEF_lp_legor_01 time_CEF time_CEF2 time_CEF3 if ccode_01 != ccode_02 & year > 1960, cluster(ccode_01)
estimate store CEF_unemul2

* by components without control variables and unconditioned *
esttab stalking_unemul1  vaw_unemul1  domestic_unemul1  sv_unemul1  sh_unemul1  FGM_unemul1  trafficking_unemul1  CEF_unemul1, se b(3)
* by components with control variables and unconditioned *
esttab stalking_unemul2  vaw_unemul2  domestic_unemul2  sv_unemul2  FGM_unemul2  trafficking_unemul2  CEF_unemul2, se b(3) 
*sh_unemul2 cannot be run*


* run model for any law - conditioned *
logit L_any_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 DL_ht_region_01 DL_ht_colonial_01 DL_lp_legor_01 time time2 time3 if ccode_01 != ccode_02 & lag_L_has_any_02 == 1, cluster(ccode_01)
estimate store con1
logit L_any_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 DL_ht_region_01 DL_ht_colonial_01 DL_lp_legor_01 time time2 time3 if ccode_01 != ccode_02 & lag_L_has_any_02 == 1, cluster(ccode_01)
estimate store con2
estat ic

logit L_any_emulation lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 time time2 time3 if ccode_01 != ccode_02 & lag_L_has_any_02 == 1, cluster(ccode_01)
estimate store con2withoutdiffusion

* by components*

logit L_stalking_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 stalking_ht_region_01 stalking_ht_colonial_01 stalking_lp_legor_01 time_stalking time_stalking2 time_stalking3 if ccode_01 != ccode_02 & year > 1923 & lag_L_stalking_any_02 == 1, cluster(ccode_01)
estimate store stalking_conemul1
logit L_stalking_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 stalking_ht_region_01 stalking_ht_colonial_01 stalking_lp_legor_01 time_stalking time_stalking2 time_stalking3 if ccode_01 != ccode_02 & year > 1923 & lag_L_stalking_any_02 == 1, cluster(ccode_01)
estimate store stalking_conemul2

logit L_violenceagainstwomen_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 vaw_ht_region_01 vaw_ht_colonial_01 vaw_lp_legor_01 time_vaw time_vaw2 time_vaw3 if ccode_01 != ccode_02 & year > 1958 & lag_L_vaw_any_02 == 1, cluster(ccode_01)
estimate store vaw_conemul1
logit L_violenceagainstwomen_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 vaw_ht_region_01 vaw_ht_colonial_01 vaw_lp_legor_01 time_vaw time_vaw2 time_vaw3 if ccode_01 != ccode_02 & year > 1958 & lag_L_vaw_any_02 == 1, cluster(ccode_01)
estimate store vaw_conemul2

logit L_domestic_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 domestic_ht_region_01 domestic_ht_colonial_01 domestic_lp_legor_01 time_domestic time_domestic2 time_domestic3 if ccode_01 != ccode_02 & year > 1972 & lag_L_domestic_any_02 == 1, cluster(ccode_01)
estimate store domestic_conemul1
logit L_domestic_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 domestic_ht_region_01 domestic_ht_colonial_01 domestic_lp_legor_01 time_domestic time_domestic2 time_domestic3 if ccode_01 != ccode_02 & year > 1972 & lag_L_domestic_any_02 == 1, cluster(ccode_01)
estimate store domestic_conemul2

logit L_sexual_violence_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 sv_ht_region_01 sv_ht_colonial_01 sv_lp_legor_01 time time2 time3 if ccode_01 != ccode_02 & lag_L_sv_any_02 == 1, cluster(ccode_01)
estimate store sv_conemul1
logit L_sexual_violence_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 sv_ht_region_01 sv_ht_colonial_01 sv_lp_legor_01 time time2 time3 if ccode_01 != ccode_02 & lag_L_sv_any_02 == 1, cluster(ccode_01)
estimate store sv_conemul2

logit L_sexual_harassment_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 sh_ht_region_01 sh_ht_colonial_01 sh_lp_legor_01 time_sh time_sh2 time_sh3 if ccode_01 != ccode_02 & year > 1957 & lag_L_sh_any_02 == 1, cluster(ccode_01)
estimate store sh_conemul1
logit L_sexual_harassment_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 sh_ht_region_01 sh_ht_colonial_01 sh_lp_legor_01 time_sh time_sh2 time_sh3 if ccode_01 != ccode_02 & year > 1957 & lag_L_sh_any_02 == 1, cluster(ccode_01)
estimate store sh_conemul2

logit L_FGM_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 FGM_ht_region_01 FGM_ht_colonial_01 FGM_lp_legor_01 time_FGM time_FGM2 time_FGM3 if ccode_01 != ccode_02 & year > 1965 & lag_L_FGM_any_02 == 1, cluster(ccode_01)
estimate store FGM_conemul1
logit L_FGM_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 FGM_ht_region_01 FGM_ht_colonial_01 FGM_lp_legor_01 time_FGM time_FGM2 time_FGM3 if ccode_01 != ccode_02 & year > 1965 & lag_L_FGM_any_02 == 1, cluster(ccode_01)
estimate store FGM_conemul2

logit L_trafficking_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 trafficking_ht_region_01 trafficking_ht_colonial_01 trafficking_lp_legor_01 time_trafficking time_trafficking2 time_trafficking3 if ccode_01 != ccode_02 & year > 1955 & lag_L_trafficking_any_02 == 1, cluster(ccode_01)
estimate store trafficking_conemul1
logit L_trafficking_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 trafficking_ht_region_01 trafficking_ht_colonial_01 trafficking_lp_legor_01 time_trafficking time_trafficking2 time_trafficking3 if ccode_01 != ccode_02 & year > 1955 & lag_L_trafficking_any_02 == 1, cluster(ccode_01)
estimate store trafficking_conemul2

logit L_child_ealry_forced_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 CEF_ht_region_01 CEF_ht_colonial_01 CEF_lp_legor_01 time_CEF time_CEF2 time_CEF3 if ccode_01 != ccode_02 & year > 1960 & lag_L_CEF_any_02 == 1, cluster(ccode_01)
estimate store CEF_conemul1
logit L_child_ealry_forced_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 CEF_ht_region_01 CEF_ht_colonial_01 CEF_lp_legor_01 time_CEF time_CEF2 time_CEF3 if ccode_01 != ccode_02 & year > 1960 & lag_L_CEF_any_02 == 1, cluster(ccode_01)
estimate store CEF_conemul2

* by components without control variables and conditioned *
esttab stalking_conemul1  vaw_conemul1  domestic_conemul1  sv_conemul1  sh_conemul1  FGM_conemul1  trafficking_conemul1  CEF_conemul1, se b(3) 
* by components with control variables and conditioned *
esttab stalking_conemul2  vaw_conemul2  domestic_conemul2  sv_conemul2  sh_conemul2  FGM_conemul2  trafficking_conemul2  CEF_conemul2, se b(3)

* any laws unconditioned, conditioned with and without control variables *
esttab un1 con1 con2, se b(3)

* NAP adoption *
gen NAP_any_emulation = .
replace NAP_any_emulation = 0 if lag_NAP_any_02 == 0
replace NAP_any_emulation = 0 if NAP_any_01 == 0
replace NAP_any_emulation = 1 if NAP_any_emulation == .

* run model for any NAP - unconditioned *
logit NAP_any_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & year > 1987, cluster(ccode_01)
estimate store napun1
logit NAP_any_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & year > 1987, cluster(ccode_01)
estimate store napun2

* run model for any NAP - conditioned *
logit NAP_any_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & lag_NAP_has_any_02 == 1 & year > 1987, cluster(ccode_01)
estimate store napcon1
logit NAP_any_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & lag_NAP_has_any_02 == 1 & year > 1987, cluster(ccode_01)
estimate store napcon2

* run model for any NAP - diffusion diffusion model conditioned *
logit NAP_any_emulation lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & year > 1987 & lag_NAP_has_any_02 == 1 & L_has_any_01 > 0, cluster(ccode_01)
estimate store ddcon1
logit NAP_any_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & year > 1987 & lag_NAP_has_any_02 == 1 & L_has_any_01 > 0, cluster(ccode_01)
estimate store ddcon2

esttab mona_NAP mona_dd napun1 napun2 napcon1 napcon2 ddcon1 ddcon2, se b(3)

esttab mona_NAP mona_dd napun2 napcon2 ddcon2, se b(3)

*** Using Parliament, employment, wage, labor force rather than vdem_gender***
* run model for any NAP - unconditioned *
logit NAP_any_emulation lag_Femployment15ILO_01 lag_Fprimarygross_01 lag_Fparliaments_01  lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01  DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & year > 1987, cluster(ccode_01)
estimate store napun1_no
logit NAP_any_emulation lag_Femployment15ILO_01 lag_Fprimarygross_01 lag_Fparliaments_01  lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01  DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & year > 1987, cluster(ccode_01)
estimate store napun2_no
margins, at(lag_Fparliaments_01=(0(10)70)) atmeans post
estimate store margins1

* run model for any NAP - conditioned *
logit NAP_any_emulation lag_Femployment15ILO_01 lag_Fprimarygross_01 lag_Fparliaments_01  lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01  DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & lag_NAP_has_any_02 == 1 & year > 1987, cluster(ccode_01)
estimate store napcon1_no
logit NAP_any_emulation lag_Femployment15ILO_01 lag_Fprimarygross_01 lag_Fparliaments_01 lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01  DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & lag_NAP_has_any_02 == 1 & year > 1987, cluster(ccode_01)
estimate store napcon2_no
margins, at(lag_Fparliaments_01=(0(10)70)) atmeans post
estimate store margins2

*heckprob*
heckprob NAP_any_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_Femployment15ILO_01 lag_Fprimarygross_01 lag_Fparliaments_01 DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & year > 1987 & lag_NAP_has_any_02 == 1, select(L_has_any_01 = lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs  lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 DL_ht_region_01 DL_ht_colonial_01 DL_lp_legor_01 time time2 time3)  vce(cluster ccode_01)
estimate store heckprob
margins, at(lag_Fparliaments_01=(0(10)70)) atmeans post
estimate store margins4

*ivreg*
ivreg NAP_any_emulation lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_Femployment15ILO_01 lag_Fprimarygross_01 lag_Fparliaments_01 DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 (L_has_any_01 = lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs  lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 DL_ht_region_01 DL_ht_colonial_01 DL_lp_legor_01 time time2 time3) if ccode_01 != ccode_02 & year > 1987 & lag_NAP_has_any_02 == 1, cluster(ccode_01)
estimate store ivreg
margins, at(lag_Fparliaments_01=(0(10)70)) atmeans post
estimate store margins5


* run model for any NAP - diffusion diffusion model conditioned *
logit NAP_any_emulation lag_Femployment15ILO_01 lag_Fprimarygross_01 lag_Fparliaments_01  lag_percentage_CEDAW_01 lag_cgdppc_01 lag_actotal_01 lag_polity2_01  DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & year > 1987 & lag_NAP_has_any_02 == 1 & L_has_any_01 > 0, cluster(ccode_01)
estimate store ddcon1_no
lroc

logit NAP_any_emulation lag_Femployment15ILO_01 lag_Fprimarygross_01 lag_Fparliaments_01  lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01  DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & year > 1987 & lag_NAP_has_any_02 == 1 & L_has_any_01 > 0, cluster(ccode_01)
estimate store ddcon2_no
margins, at(lag_Fparliaments_01=(0(10)70)) atmeans post
estimate store margins3



logit NAP_any_emulation lag_Femployment15ILO_01 lag_Fprimarygross_01 lag_Fparliaments_01  lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01  DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & year > 1987 & lag_NAP_has_any_02 == 1 & L_has_any_01 > 0, cluster(ccode_01)
margins, at(lag_Fparliaments_01=(0(10)70)) atmeans
marginsplot, recast(line) recastci(rarea)


coefplot (margins0) (margins00) (margins1) (margins2) (margins3), at ytitle(Probability of Adoption and Emulation of Laws) xtitle(Percent of Women in Parliaments) recast(line) lwidth(*2) ciopts(recast(rline)lpattern(dash))

*Marginal Effect of the Percent of Women in Parliaments*
coefplot (margins0) (margins00) (margins2) (margins3) (margins4), at ytitle(Probability of Adoption and Emulation of Laws) xtitle(Percent of Women in Parliaments) recast(line) lwidth(*2) ciopts(recast(rline)lpattern(dash))

*coefplot for women variables*
quietly logit NAP_any_emulation lag_Femployment15ILO_01 lag_Fprimarygross_01 lag_Fparliaments_01  lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01  DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & year > 1987 & lag_NAP_has_any_02 == 1 & L_has_any_01 > 0, cluster(ccode_01)
coefplot, keep(lag_Femployment15ILO_01 lag_Fprimarygross_01 lag_Fparliaments_01) xline(0) msymbol(d) mcolor(white) ///
 levels(99 95 90 80 70) ciopts(lwidth(3 ..) lcolor(*.2 *.4 *.6 *.8 *1)) ///
 legend(order(1 "99" 2 "95" 3 "90" 4 "80" 5 "70") row(1))

* 
 
logit NAP_any_emulation lag_Femployment15ILO_01 lag_Fprimarygross_01 lag_Fparliaments_01  lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01  DNAP_ht_region_01 DNAP_ht_colonial_01 DNAP_lp_legor_01 NAP_time NAP_time2 NAP_time3 if ccode_01 != ccode_02 & year > 1987 & lag_NAP_has_any_02 == 1 & L_has_any_01 > 0, cluster(ccode_01)
margins, at(lag_percentage_CEDAW_01=(0(0.2)1)) atmeans
marginsplot, recast(line) recastci(rarea)


esttab mona_NAP_no mona_dd_no napun1_no napun2_no napcon1_no napcon2_no ddcon1_no ddcon2_no, se b(3)

esttab mona_NAP_no mona_dd_no  napun2_no  napcon2_no  ddcon2_no, se b(3)

*save data for R analysis *
drop if ccode_01 == ccode_02
drop if year < 1988
drop if lag_NAP_any_02 != 1
drop if L_has_any_01 < 1
save "H:\R NAP.dta", replace

**PEHM**
use "H:\replication for PEHM.dta", clear
replace policy_adopt = 0
replace policy_adopt = 1 if l_stalking == 1& policy_num == 1
replace policy_adopt = 1 if l_violence_against_women == 1& policy_num == 2
replace policy_adopt = 1 if l_domestic == 1& policy_num == 3
replace policy_adopt = 1 if l_sexual_violence == 1& policy_num == 4
replace policy_adopt = 1 if l_sexual_harassment == 1& policy_num == 5
replace policy_adopt = 1 if l_fgm == 1& policy_num == 6
replace policy_adopt = 1 if l_trafficking == 1& policy_num == 7
replace policy_adopt = 1 if l_child_ealry_forced == 1& policy_num == 8

sort ccode year
by ccode: gen lag_L_any = l_any[_n-1]
by ccode: gen lag_CEDAW = cedaw[_n-1]
by ccode: gen lag_percentage_CEDAW = percentage_cedaw[_n-1]
by ccode: gen lag_NAP_any = nap_any[_n-1]
drop if year < 1908
tab lag_L_any
tab lag_L_any, missing
replace lag_L_any = 0 if lag_L_any ==.
replace lag_CEDAW = 0 if lag_CEDAW ==.
replace lag_percentage_CEDAW = 0 if lag_percentage_CEDAW == .
replace lag_NAP_any = 0 if lag_NAP_any == .

* make more than one adoptions equals to one adoption *
recode l_femicide (2 3 = 1)
recode l_forced_sterilization (2 3 = 1)
recode l_stalking (2 3 = 1)
recode l_property (2 3 = 1)
recode l_violence_against_women (2 3 = 1)
recode l_domestic (2 3 = 1)
recode l_sexual_violence (2 3 = 1)
recode l_new (2 3 = 1)
recode l_sexual_harassment (2 3 = 1)
recode l_fgm (2 3 = 1)
recode l_trafficking (2 3 = 1)
recode l_child_ealry_forced (2 3 = 1)
recode nap_femicide (2 3 = 1)
recode nap_forced_sterilization (2 3 = 1)
recode nap_stalking (2 3 = 1)
recode nap_property (2 3 = 1)
recode nap_violence_against_women (2 3 = 1)
recode nap_domestic (2 3 = 1)
recode nap_sexual_violence (2 3 = 1)
recode nap_new (2 3 = 1)
recode nap_sexual_harassment (2 3 = 1)
recode nap_fgm (2 3 = 1)
recode napl_trafficking (2 3 = 1)
recode napl_child_ealry_forced (2 3 = 1)
recode lag_l_feminicide (2 3 = 1)
recode lag_l_forced_sterilization (2 3 = 1)
recode lag_l_stalking (2 3 = 1)
recode lag_l_property (2 3 = 1)
recode lag_l_violence_against_women (2 3 = 1)
recode lag_l_domestic (2 3 = 1)
recode lag_l_sexual_violence (2 3 = 1)
recode lag_l_new (2 3 = 1)
recode lag_l_sexual_harassment (2 3 = 1)
recode lag_l_fgm (2 3 = 1)
recode lag_l_trafficking (2 3 = 1)
recode lag_l_child_ealry_forced (2 3 = 1)
recode lag_nap_femicide (2 3 = 1)
recode lag_nap_forced_sterilization (2 3 = 1)
recode lag_nap_stalking (2 3 = 1)
recode lag_nap_property (2 3 = 1)
recode lag_nap_violence_against_women (2 3 = 1)
recode lag_nap_domestic (2 3 = 1)
recode lag_nap_sexual_violence (2 3 = 1)
recode lag_nap_new (2 3 = 1)
recode lag_nap_sexual_harassment (2 3 = 1)
recode lag_nap_fgm (2 3 = 1)
recode lag_napl_trafficking (2 3 = 1)
recode lag_napl_child_ealry_forced (2 3 = 1)

gen time2 = time*time
gen time3 = time*time*time

* rescale cgdppc *
gen rcgdppc = cgdppc/10000
gen rlag_cgdppc = lag_cgdppc/10000
drop cgdppc
drop lag_cgdppc
rename rcgdppc cgdppc
rename rlag_cgdppc lag_cgdppc

*by components only for n>30*
gen time_stalking = year - 1924
gen time_stalking2 = time_stalking*time_stalking
gen time_stalking3 = time_stalking*time_stalking*time_stalking

gen time_vaw = year - 1959
gen time_vaw2 = time_vaw*time_vaw
gen time_vaw3 = time_vaw*time_vaw*time_vaw

gen time_domestic = year - 1973
gen time_domestic2 = time_domestic*time_domestic
gen time_domestic3 = time_domestic*time_domestic*time_domestic


gen time_sh = year - 1958
gen time_sh2 = time_sh*time_sh
gen time_sh3 = time_sh*time_sh*time_sh

gen time_FGM = year - 1966
gen time_FGM2 = time_FGM*time_FGM
gen time_FGM3 = time_FGM*time_FGM*time_FGM

gen time_trafficking = year - 1956
gen time_trafficking2 = time_trafficking*time_trafficking
gen time_trafficking3 = time_trafficking*time_trafficking*time_trafficking

gen time_CEF = year - 1961
gen time_CEF2 = time_CEF*time_CEF
gen time_CEF3 = time_CEF*time_CEF*time_CEF

gen time_policy = . 
replace time_policy = time_stalking if policy_num == 1
replace time_policy = time_vaw if policy_num == 2
replace time_policy = time_domestic if policy_num == 3
replace time_policy = time if policy_num == 4
replace time_policy = time_sh if policy_num == 5
replace time_policy = time_FGM if policy_num == 6
replace time_policy = time_trafficking if policy_num == 7
replace time_policy = time_CEF if policy_num ==8
gen time_policy2 = time_policy*time_policy
gen time_policy3 = time_policy*time_policy*time_policy

gen pehmdl_ht_region = .
replace pehmdl_ht_region = stalking_ht_region if policy_num == 1
replace pehmdl_ht_region = vaw_ht_region if policy_num == 2
replace pehmdl_ht_region = domestic_ht_region if policy_num == 3
replace pehmdl_ht_region = sv_ht_region if policy_num == 4
replace pehmdl_ht_region = sh_ht_region if policy_num == 5
replace pehmdl_ht_region = fgm_ht_region if policy_num == 6
replace pehmdl_ht_region = trafficking_ht_region if policy_num == 7
replace pehmdl_ht_region = cef_ht_region if policy_num ==8

gen pehmdl_ht_colonial = .
replace pehmdl_ht_colonial = stalking_ht_colonial if policy_num == 1
replace pehmdl_ht_colonial = vaw_ht_colonial if policy_num == 2
replace pehmdl_ht_colonial = domestic_ht_colonial if policy_num == 3
replace pehmdl_ht_colonial = sv_ht_colonial if policy_num == 4
replace pehmdl_ht_colonial = sh_ht_colonial if policy_num == 5
replace pehmdl_ht_colonial = fgm_ht_colonial if policy_num == 6
replace pehmdl_ht_colonial = trafficking_ht_colonial if policy_num == 7
replace pehmdl_ht_colonial = cef_ht_colonial if policy_num ==8

gen pehmdl_lp_legor = .
replace pehmdl_lp_legor = stalking_lp_legor if policy_num == 1
replace pehmdl_lp_legor = vaw_lp_legor if policy_num == 2
replace pehmdl_lp_legor = domestic_lp_legor if policy_num == 3
replace pehmdl_lp_legor = sv_lp_legor if policy_num == 4
replace pehmdl_lp_legor = sh_lp_legor if policy_num == 5
replace pehmdl_lp_legor = fgm_lp_legor if policy_num == 6
replace pehmdl_lp_legor = trafficking_lp_legor if policy_num == 7
replace pehmdl_lp_legor = cef_lp_legor if policy_num ==8


*MLM *
melogit policy_adopt lag_percentage_CEDAW lag_CEDAW lag_cgdppc  lag_actotal lag_polity2 lag_vdem_gender pehmdl_ht_region pehmdl_ht_colonial pehmdl_lp_legor time_policy time_policy2 time_policy3||policy_num:
estimate store mona_pehm
estat ic

melogit policy_adopt lag_percentage_CEDAW lag_CEDAW lag_cgdppc  lag_actotal lag_polity2 lag_vdem_gender pehmdl_ht_region pehmdl_ht_colonial pehmdl_lp_legor time_policy time_policy2 time_policy3 i.year||policy_num:


**dyadic PEHM**

use "H:\replication for dyadic PEHM.dta", clear
gen policy_adopt = .
replace policy_adopt = 1 if L_stalking_01 == 1 & policy_num == 1
replace policy_adopt = 1 if L_violence_against_women_01 == 1 & policy_num == 2
replace policy_adopt = 1 if L_domestic_01 == 1 & policy_num == 3
replace policy_adopt = 1 if L_sexual_violence_01 == 1 & policy_num == 4
replace policy_adopt = 1 if L_sexual_harassment_01 == 1 & policy_num == 5
replace policy_adopt = 1 if L_FGM_01 == 1 & policy_num == 6
replace policy_adopt = 1 if L_trafficking_01 == 1 & policy_num == 7
replace policy_adopt = 1 if L_child_ealry_forced_01 == 1 & policy_num == 8
replace policy_adopt = 0 if policy_adopt == .

gen time_policy = . 
replace time_policy = time_stalking if policy_num == 1
replace time_policy = time_vaw if policy_num == 2
replace time_policy = time_domestic if policy_num == 3
replace time_policy = time if policy_num == 4
replace time_policy = time_sh if policy_num == 5
replace time_policy = time_FGM if policy_num == 6
replace time_policy = time_trafficking if policy_num == 7
replace time_policy = time_CEF if policy_num ==8
gen time_policy2 = time_policy*time_policy
gen time_policy3 = time_policy*time_policy*time_policy

gen pehmdl_ht_region = .
replace pehmdl_ht_region = stalking_ht_region_01 if policy_num == 1
replace pehmdl_ht_region = vaw_ht_region_01 if policy_num == 2
replace pehmdl_ht_region = domestic_ht_region_01 if policy_num == 3
replace pehmdl_ht_region = sv_ht_region_01 if policy_num == 4
replace pehmdl_ht_region = sh_ht_region_01 if policy_num == 5
replace pehmdl_ht_region = FGM_ht_region_01 if policy_num == 6
replace pehmdl_ht_region = trafficking_ht_region_01 if policy_num == 7
replace pehmdl_ht_region = CEF_ht_region_01 if policy_num ==8

gen pehmdl_ht_colonial = .
replace pehmdl_ht_colonial = stalking_ht_colonial_01 if policy_num == 1
replace pehmdl_ht_colonial = vaw_ht_colonial_01 if policy_num == 2
replace pehmdl_ht_colonial = domestic_ht_colonial_01 if policy_num == 3
replace pehmdl_ht_colonial = sv_ht_colonial_01 if policy_num == 4
replace pehmdl_ht_colonial = sh_ht_colonial_01 if policy_num == 5
replace pehmdl_ht_colonial = FGM_ht_colonial_01 if policy_num == 6
replace pehmdl_ht_colonial = trafficking_ht_colonial_01 if policy_num == 7
replace pehmdl_ht_colonial = CEF_ht_colonial_01 if policy_num ==8

gen pehmdl_lp_legor = .
replace pehmdl_lp_legor = stalking_lp_legor_01 if policy_num == 1
replace pehmdl_lp_legor = vaw_lp_legor_01 if policy_num == 2
replace pehmdl_lp_legor = domestic_lp_legor_01 if policy_num == 3
replace pehmdl_lp_legor = sv_lp_legor_01 if policy_num == 4
replace pehmdl_lp_legor = sh_lp_legor_01 if policy_num == 5
replace pehmdl_lp_legor = FGM_lp_legor_01 if policy_num == 6
replace pehmdl_lp_legor = trafficking_lp_legor_01 if policy_num == 7
replace pehmdl_lp_legor = CEF_lp_legor_01 if policy_num ==8

gen lag_policy_02 = 0
replace lag_policy_02 = lag_L_stalking_02 if policy_num == 1
replace lag_policy_02 = lag_L_violence_against_women_02 if policy_num == 2
replace lag_policy_02 = lag_L_domestic_02 if policy_num == 3
replace lag_policy_02 = lag_L_sexual_violence_02 if policy_num == 4
replace lag_policy_02 = lag_L_sexual_harassment_02 if policy_num == 5
replace lag_policy_02 = lag_L_FGM_02 if policy_num == 6
replace lag_policy_02 = lag_L_trafficking_02 if policy_num == 7
replace lag_policy_02 = lag_L_child_ealry_forced_02 if policy_num ==8

drop if ccode_01 == ccode_02
drop if lag_policy_02 == 0
melogit policy_adopt lag_percentage_CEDAW_01 lag_CEDAW_01 gdppc_diff_abs polity2_diff_abs lag_cgdppc_01 lag_actotal_01 lag_polity2_01 lag_vdem_gender_01 pehmdl_ht_region pehmdl_ht_colonial pehmdl_lp_legor time_policy time_policy2 time_policy3 ||policy_num:
estimate store dyad_pehm
estat ic

*table 2 law* 
esttab mona_anywithoutdiffusion mona_any con2withoutdiffusion con2, se b(3)

*table 3 nap*
esttab mona_NAP mona_dd napcon2 ddcon2, se b(3)

*table 4 nap with disaggregated index*
esttab mona_NAP_no mona_dd_no  napcon2_no  ddcon2_no, se b(3)

*appendix*
esttab mona_stalking mona_vaw mona_domestic mona_sexualviolence mona_sexualharassment mona_FGM mona_trafficking mona_early, se b(3)
esttab stalking_unemul1  vaw_unemul1  domestic_unemul1  sv_unemul1  sh_unemul1  FGM_unemul1  trafficking_unemul1  CEF_unemul1, se b(3)
esttab stalking_unemul2  vaw_unemul2  domestic_unemul2  sv_unemul2  FGM_unemul2  trafficking_unemul2  CEF_unemul2, se b(3) 
esttab stalking_conemul1  vaw_conemul1  domestic_conemul1  sv_conemul1  sh_conemul1  FGM_conemul1  trafficking_conemul1  CEF_conemul1, se b(3) 
esttab stalking_conemul2  vaw_conemul2  domestic_conemul2  sv_conemul2  sh_conemul2  FGM_conemul2  trafficking_conemul2  CEF_conemul2, se b(3)
esttab mona_pehm dyad_pehm, se (b3)
esttab heckprob ivreg, se b(3)

*coefficent plot*
*1: adoption of laws*
coefplot (mona_any, rename(lag_percentage_CEDAW = CEDAW(%) lag_CEDAW = CEDAW lag_cgdppc = gdp lag_actotal = MEPV lag_polity2 = Polity2 lag_vdem_gender = Women Political Empowerment DL_ht_region = Region DL_ht_colonial = Colonial DL_lp_legor = Legal)) (con2, rename(lag_percentage_CEDAW_01 = CEDAW(%) lag_CEDAW_01 = CEDAW lag_cgdppc_01 = gdp lag_actotal_01 = MEPV lag_polity2_01 = Polity2 lag_vdem_gender_01 = Women Political Empowerment DL_ht_region_01 = Region DL_ht_colonial_01 = Colonial DL_lp_legor_01 = Legal pehmdl_ht_region = Region pehmdl_ht_colonial = Colonial pehmdl_lp_legor = Legal time_policy = time time_policy2 = time2 time_policy3 = time3 lag_percentage_CEDAW_01 = CEDAW(%) lag_CEDAW_01 = CEDAW lag_cgdppc_01 = gdp lag.actotal_01 = MEPV lag.polity2_01 = Polity2 lag.vdem_gender_01 = Women Political Empowerment DL_ht_region_01 = Region DL_ht_colonial_01 = Colonial DL_lp_legor_01 = Legal)), drop(_cons time* gdppc_diff_abs polity2_diff_abs lag_percentage_CEDAW lag_percentage_CEDAW_01) xline(0)

*2: adoption of NAP*
coefplot (mona_NAP, rename(lag_vdem_gender = Women lag_actotal = MEPV lag_polity2 = Polity2 lag_CEDAW = CEDAW lag_cgdppc = gdp DNAP_ht_region = Region DNAP_ht_colonial = Colonial DNAP_lp_legor = Legal)) (mona_dd, rename(lag_vdem_gender = Women lag_actotal = MEPV lag_polity2 = Polity2 lag_CEDAW = CEDAW lag_cgdppc = gdp DNAP_ht_region = Region DNAP_ht_colonial = Colonial DNAP_lp_legor = Legal)) (napcon2, rename(lag_vdem_gender_01 = Women lag_actotal_01 = MEPV lag_polity2_01 = Polity2 lag_CEDAW_01 = CEDAW lag_cgdppc_01 = gdp DNAP_ht_region_01 = Region DNAP_ht_colonial_01 = Colonial DNAP_lp_legor_01 = Legal)) (ddcon2, rename(lag_vdem_gender_01 = Women lag_actotal_01 = MEPV lag_polity2_01 = Polity2 lag_CEDAW_01 = CEDAW lag_cgdppc_01 = gdp DNAP_ht_region_01 = Region DNAP_ht_colonial_01 = Colonial DNAP_lp_legor_01 = Legal)), drop(_cons time* NAP_time* gdppc_diff_abs polity2_diff_abs lag_percentage_CEDAW lag_percentage_CEDAW_01 lag_actotal* lag_polity2* lag_CEDAW* lag_cgdppc*) yline(0) vertical  ciopts(recast(. racp))

coefplot (mona_NAP, rename(lag_vdem_gender = Women lag_actotal = MEPV lag_polity2 = Polity2 lag_CEDAW = CEDAW lag_cgdppc = gdp DNAP_ht_region = Region DNAP_ht_colonial = Colonial DNAP_lp_legor = Legal)) (mona_dd, rename(lag_vdem_gender = Women lag_actotal = MEPV lag_polity2 = Polity2 lag_CEDAW = CEDAW lag_cgdppc = gdp DNAP_ht_region = Region DNAP_ht_colonial = Colonial DNAP_lp_legor = Legal)) (napun2, rename(lag_vdem_gender_01 = Women lag_actotal_01 = MEPV lag_polity2_01 = Polity2 lag_CEDAW_01 = CEDAW lag_cgdppc_01 = gdp DNAP_ht_region_01 = Region DNAP_ht_colonial_01 = Colonial DNAP_lp_legor_01 = Legal)) (napcon2, rename(lag_vdem_gender_01 = Women lag_actotal_01 = MEPV lag_polity2_01 = Polity2 lag_CEDAW_01 = CEDAW lag_cgdppc_01 = gdp DNAP_ht_region_01 = Region DNAP_ht_colonial_01 = Colonial DNAP_lp_legor_01 = Legal)) (ddcon2, rename(lag_vdem_gender_01 = Women lag_actotal_01 = MEPV lag_polity2_01 = Polity2 lag_CEDAW_01 = CEDAW lag_cgdppc_01 = gdp DNAP_ht_region_01 = Region DNAP_ht_colonial_01 = Colonial DNAP_lp_legor_01 = Legal)), drop(_cons time* NAP_time* gdppc_diff_abs polity2_diff_abs lag_percentage_CEDAW lag_percentage_CEDAW_01 lag_actotal* lag_polity2* lag_CEDAW* lag_cgdppc*) yline(0) vertical  ciopts(recast(. racp))

*Decomposed indicators*
coefplot (mona_NAP_no, rename(lag_Fparliaments = Percentage of Women in Parliaments lag_Fprimarygross = primary lag_Femployment15ILO = employment)) (mona_dd_no, rename(lag_Fparliaments = Percentage of Women in Parliaments lag_Fprimarygross = primary lag_Femployment15ILO = employment)) (napcon2_no, rename(lag_Fparliaments_01 = Percentage of Women in Parliaments lag_Fprimarygross_01=primary lag_Femployment15ILO_01= employment)) (ddcon2_no, rename(lag_Fparliaments_01 = Percentage of Women in Parliaments lag_Fprimarygross_01=primary lag_Femployment15ILO_01= employment)), keep(lag_Femployment15ILO  lag_Fprimarygross lag_Fparliaments lag_Femployment15ILO_01  lag_Fprimarygross_01 lag_Fparliaments_01) xline(0)

coefplot (mona_NAP_no, rename(lag_Fparliaments = Percentage of Women in Parliaments lag_Fprimarygross = primary lag_Femployment15ILO = employment)) (mona_dd_no, rename(lag_Fparliaments = Percentage of Women in Parliaments lag_Fprimarygross = primary lag_Femployment15ILO = employment)) (napun2_no, rename(lag_Fparliaments_01 = Percentage of Women in Parliaments lag_Fprimarygross_01=primary lag_Femployment15ILO_01= employment)) (napcon2_no, rename(lag_Fparliaments_01 = Percentage of Women in Parliaments lag_Fprimarygross_01=primary lag_Femployment15ILO_01= employment)) (ddcon2_no, rename(lag_Fparliaments_01 = Percentage of Women in Parliaments lag_Fprimarygross_01=primary lag_Femployment15ILO_01= employment)), keep(lag_Femployment15ILO  lag_Fprimarygross lag_Fparliaments lag_Femployment15ILO_01  lag_Fprimarygross_01 lag_Fparliaments_01) xline(0)


coefplot (mona_NAP_no, rename(lag_Fparliaments = Percentage of Women in Parliaments lag_Fprimarygross = primary lag_Femployment15ILO = employment)) (mona_dd_no, rename(lag_Fparliaments = Percentage of Women in Parliaments lag_Fprimarygross = primary lag_Femployment15ILO = employment)) (napun2_no, rename(lag_Fparliaments_01 = Percentage of Women in Parliaments lag_Fprimarygross_01=primary lag_Femployment15ILO_01= employment)) (napcon2_no, rename(lag_Fparliaments_01 = Percentage of Women in Parliaments lag_Fprimarygross_01=primary lag_Femployment15ILO_01= employment)) (ddcon2_no, rename(lag_Fparliaments_01 = Percentage of Women in Parliaments lag_Fprimarygross_01=primary lag_Femployment15ILO_01= employment)), keep(lag_Femployment15ILO  lag_Fprimarygross lag_Fparliaments lag_Femployment15ILO_01  lag_Fprimarygross_01 lag_Fparliaments_01) xline(0)

