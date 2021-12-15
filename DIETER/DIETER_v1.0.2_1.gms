*==========
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.0.2, January 2016.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/DIETER.
This version constitutes a minor revision of the model documented in Zerrahn, A., Schill, W.-P. (2020): A greenfield model to evaluate long-run power storage requirements for high shares of renewables. DIW Discussion Paper 1457. http://www.diw.de/documents/publikationen/73/diw_01.c.498475.de/dp1457.pdf
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
*==========


*Fluctuating renewable Scenarios within each loop
*$include Scenario.gms

*For endogenous VRE shares
*parameter spv_share, wind_share_on, wind_share_off;



* Define net energy demand


*==========
*==========           GLOBAL OPTIONS *==========
*==========

* Set star to skip Excel upload and load data from gdx
$setglobal skip_Excel ""

* Set star to write Excel output file
$setglobal write_to_excel ""

* Set star to activate options
$setglobal DSM ""
$setglobal P2G "*"
$setglobal reserves ""


* Set star to run test variant with each second hour
$setglobal second_hour ""

* Definition of strings for report parameters and sanity checks
* (Do not change settings below)
$setglobal em_share ""
$setglobal sec_hour "1"

%second_hour%$ontext
$setglobal sec_hour "8760/2208"
$ontext
$offtext

*** ================== coupling switches =======================
****fuel cost option (averaged over iteration or not, averaged over years or not):
*smoothed will load averaged fuel cost over 3 iterations
*fixed will load fuel cost from the last uncoupled iteration of REMIND
*$setglobal fuel_cost_iter smoothed
*$setglobal fuel_cost_iter fixed (deprecated)
$setglobal fuel_cost_iter cubicFit
*-------------
****fuel cost option 2:
*averaged will use 3-year averaged fuel cost (calculated in DIETER)
*noavg will use non year-averaged fuel cost (but iteration averaged, done in remind)
*$setglobal fuel_cost_yr avg
$setglobal fuel_cost_yr no_avg
*-------------
****fuel cost option 3:
*with a supply curve
$setglobal fuel_cost_suppc no_suppcurve
*$setglobal fuel_cost_suppc suppcurve
*==========
****whether to shave off scarcity price
$setglobal price_shave on
*$setglobal price_shave off

**** capacity bound options (bound to remind's preInvest cap)
* none = no bound
* hardLo = hard lower bound for all tech
* softLo = soft lower bound (maximum of 20% peak capacity and remind preInvest cap) for dispatchables, hard lower bound for VRE
* earlyReti = for dispatchables: if remind has retired capacity in this year in the last iter, then no lower bound; otherwise it is fixed to remind capacity; hard lower bound for VRE
* fixed = fix to postInvest cap in REMIND, for speeding up computation
$setglobal cap_bound hardLo
*$setglobal cap_bound softLo1
*$setglobal cap_bound softLo2
*$setglobal cap_bound none
*$setglobal cap_bound fixed

*whether to split lignite and hard coal
$setglobal coal_split off
*$setglobal coal_split on

*whether couple elh2 flexible demand
*$setglobal elh2_coup on
$setglobal elh2_coup off

*whether ramping cost for conventional and for electrolyzers are turned on
*$setglobal ramping_cost on
$setglobal ramping_cost off


* to reduce the size of lst file
option limcol    = 0;
option limrow    = 0;

*==========
*==========

Sets
*============== remind sets ==================
yr          year for remind power sector             /2020/
yr_before   previous year from remind                /2015/
all_yr      for smoothing prices                        /2005,2020,2150/
t           year from remind to be loaded                
*te_remind   remind technonlogy					    /spv, wind, hydro, elh2, ngcc, ngccc, gaschp, ngt, biochp, bioigcc, bioigccc, igcc, igccc, pc, pcc, pco, coalchp, storspv, storwind, tnrs, fnrs, gridwind/
te_remind   remind technonlogy					    /spv, wind, hydro, elh2, ngcc, ngccc, ngt, bioigcc, bioigccc, igcc, igccc, pc, pcc, pco, storspv, storwind, tnrs, fnrs, gridwind/
gas_remind  remind emission gases                    /co2/
*COALte(te_remind) "coal to seel tech in REMIND"      /igcc, igccc, pc, pcc, pco, coalchp/
COALte(te_remind) "coal to seel tech in REMIND"      /igcc, igccc, pc, pcc, pco/
*NonPeakGASte(te_remind) "gas to seel tech in REMIND" /ngcc, ngccc, gaschp/
NonPeakGASte(te_remind) "gas to seel tech in REMIND" /ngcc, ngccc/
*BIOte(te_remind) "biomass to seel tech in REMIND"    /biochp, bioigcc, bioigccc/
BIOte(te_remind) "biomass to seel tech in REMIND"    /bioigcc, bioigccc/
NUCte(te_remind) "nuclear to seel tech in REMIND"    /tnrs, fnrs/

pe_remind   remind primary energy                    /pegas, pecoal,pewin,pesol,pebiolc,peur,pehyd/
se_remind   remind secondary energy                  /seel,seh2/
*omf is for fixed O&M cost
char_remind remind character                         /omf, omv, lifetime/
char_remind_dataren "remind character for renewable" /nur/
grade 	    remind grade level for technology	    /1*12/
reg         region set                               /DEU/

*============== DIETER sets ==================
year      yearly time data                       /2011, 2012, 2013, 2013_windonsmooth,2019/
all_te        all dieter techs                       /ror, nuc, lig, hc, CCGT, OCGT_eff, OCGT_ineff, bio, Wind_on, Wind_off, Solar,elh2/
te        all dieter techs                      
all_cdata Data for Conventional Technologies     /eta_con,carbon_content,c_up,c_do,c_fix_con,c_var_con,c_inv_overnight_con,inv_lifetime_con,inv_recovery_con,inv_interest_con,m_con,m_con_e,grad_per_min/
all_rdata Data for Renewable Technologies        /c_cu,c_fix_res,c_var_res,phi_min_res,c_inv_overnight_res,inv_lifetime_res,inv_recovery_res,inv_interest_res,m_res,m_res_e/
all_p2gdata                                      /c_fix_p2g, c_var_p2g, inv_lifetime_p2g,p2g_up,p2g_do/
all_griddata                                     /c_fix_grid, inv_lifetime_grid/
ct(all_te)        Conventional Technologies              /ror, nuc, lig, hc, CCGT, OCGT_eff, OCGT_ineff, bio/
ct_remind Conventional Technologies mapped from REMIND /ror, nuc, coal, CCGT, OCGT_eff, OCGT_ineff, bio/
non_nuc_ct(ct) Conv. Technologies except nuclear /ror, lig, hc, CCGT, OCGT_eff, OCGT_ineff, bio/
res(all_te)       Renewable technologies                 /Wind_on, Wind_off, Solar/
sto       Storage technolgies                    /Sto1*Sto7/
p2g(all_te)       Sector Coupling P2G Technologies       /elh2/
grid      Transmission grid cost for VRE         /vregrid/
all_dsm_cu Data for DSM curt                     /c_m_dsm_cu,c_fix_dsm_cu,c_inv_overnight_dsm_cu,inv_recovery_dsm_cu,inv_interest_dsm_cu,m_dsm_cu,t_dur_dsm_cu,t_off_dsm_cu/
all_dsm_shift Data for DSM shift                 /c_m_dsm_shift,eta_dsm_shift,c_fix_dsm_shift,c_inv_overnight_dsm_shift,inv_recovery_dsm_shift,inv_interest_dsm_shift,m_dsm_shift,t_dur_dsm_shift,t_off_dsm_shift/
all_storage Data for Storagge                    /c_m_sto,eta_sto,c_fix_sto,c_inv_overnight_sto_e,c_inv_overnight_sto_p,inv_lifetime_sto,inv_interest_sto,m_sto_e,m_sto_p,phi_sto_ini,etop_max/
all_reserve Data for Reserves                    /reserves_intercept,phi_reserves_share/
dsm_shift DSM shifting technologies              /DSM_shift1*DSM_shift5/
dsm_curt  Set of load curtailment technologies   /DSM_curt1*DSM_curt3/
reserves  Set of reserve qualities               /PR_up, PR_do, SR_up, SR_do, MR_up, MR_do/
h         hour                                   /h1*h8760/

DT_RM(ct,ct_remind)   "mapping DIETER and REMIND technologies for reporting"
/
lig.coal
ror.ror
nuc.nuc
CCGT.CCGT
OCGT_eff.OCGT_eff
bio.bio
/

*==========
*te(all_te) = ct(all_te) + res(all_te) + p2g(all_te);

$include dataload.gms

Alias (h,hh) ;
alias (res,resres) ;
alias(se_remind,se_remind2);
alias (reserves,reservesreserves) ;

*$stop
Parameter sm_eps small number: 1e-9 /1e-9/;
Parameter totLoad total secondary electricity load;
Parameter totFixedLoad total fixed load;
Parameter totFlexLoad total flexible load;
Parameter capfac_const(res) constant capacity factor as average of hourly RES potential;
Parameter capfac_ror constant cap. factor of hydro power;
Parameter remind_gridfac_reg  grid factor per region;
Parameter r Investment interest rate;
Parameter disc_fac_con(ct) Discount factor for overnight investment;
Parameter disc_fac_res(res) Discount factor for overnight investment;
Parameter disc_fac_p2g(p2g) Discount factor for overnight investment;
Parameter disc_fac_grid(grid) Discount factor for overnight investment;
Parameter preInv_remind_cap(yr, reg, te_remind, grade) Pre investment remind cap for dispatchable te transfer;
Parameter added_remind_cap(yr, reg, te_remind, grade) added cap in REMIND for reporting;
Parameter preInv_remind_prodSe(yr, reg, pe_remind, se_remind, te_remind) Pre investment remind prodSe for VRE gen share transfer;
Parameter RM_postInv_cap_con(yr,reg,ct_remind) Post-investment REMIND capacity for conventional
Parameter RM_postInv_cap_res(yr,reg,res) Post-investment REMIND capacity for renewable
Parameter RM_postInv_cap_p2g(yr,reg,p2g) Post-investment REMIND capacity for renewable
Parameter RM_postInv_cap_grid(yr,reg,grid) Post-investment REMIND capacity for renewable
Parameter RM_postInv_prodSe_con(yr,reg,ct_remind) Post-investment REMIND generation for conventional
Parameter RM_postInv_prodSe_res_xcurt(yr,reg,res) Post-investment REMIND generation for renewables excluding curtailment
Parameter RM_postInv_demSe(yr,reg,p2g) Post-investment REMIND demand for P2G


*==========

Variables
Z                Value objective function
;

Positive Variables
G_L(ct,h)        Generation level in hour h in MWh
G_UP(ct,h)       Generation upshift in hour h in MWh
G_DO(ct,h)       Generation downshift in hour h in MWh

C_P2G(p2g,h)     Flexible load in hour h in MWh
C_P2GUP(p2g,h)   Generation upshift in hour h in MWh
C_P2GDO(p2g,h)   Generation downshift in hour h in MWh

G_RES(res,h)     Generation renewables type res in hour h in MWh
CU(res,h)        Renewables curtailment technology res in hour h in MWh

STO_IN(sto,h)    Storage inflow technology sto hour h in MWh
STO_OUT(sto,h)   Storage outflow technology sto hour h in MWh
STO_L(sto,h)     Storage level technology sto hour h in MWh

P_RES(res)       Renewable technology built in MW
N_CON(ct)        Conventional technology ct built in MW
N_P2G(p2g)       P2G technology built in MW
N_GRID(grid)     Grid capacity in MW

N_STO_E(sto)     Storage technology built - Energy in MWh
N_STO_P(sto)     Storage loading and discharging capacity built - Capacity in MW

DSM_CU(dsm_curt,h)           DSM: Load curtailment hour h in MWh
DSM_UP(dsm_shift,h)          DSM: Load shifting up hour h technology dsm in MWh
DSM_DO(dsm_shift,h,hh)       DSM: Load shifting down in hour hh to account for upshifts in hour h technology dsm in MWh

DSM_UP_DEMAND(dsm_shift,h)   DSM: Load shifting up active for wholesale demand in hour h of technology dsm in MWh
DSM_DO_DEMAND(dsm_shift,h)   DSM: Load shifting down active for wholesale demand in hour h of technology dsm in MWh

N_DSM_CU(dsm_curt)           DSM: Load curtailment capacity in MW
N_DSM_SHIFT(dsm_shift)       DSM: Load shifting capacity in MWh

RP_CON(reserves,ct,h)                     Reserve provision by conventionals in hour h in MW
RP_RES(reserves,res,h)                    Reserve provision by renewables in hour h in MW
RP_STO_IN(reserves,sto,h)                 Reserve provision by storage in in hour h in MW
RP_STO_OUT(reserves,sto,h)                Reserve provision by storage out in hour h in MW
RP_DSM_CU(reserves,dsm_curt,h)            Reserve provision by DSM load curtailment in hour h in MW
RP_DSM_SHIFT(reserves,dsm_shift,h)        Reserve provision by DSM load shifting in hour h in MW

pref_FC(ct)              Fuel cost prefactor (share depenedent)
;

*================================================================
*================ scale up demand ===============================
DIETER_OLDtotdem = sum( h , d_y_reg('2019',"DEU",h));

totLoad = remind_totseelDem("2020", "DEU", "seel") * sm_TWa_2_MWh;

$IFTHEN %elh2_coup% == "off"
totFlexLoad = 0;
$ENDIF

$IFTHEN %elh2_coup% == "on"
totFlexLoad = remind_totseh2Dem("2020", "DEU", "seh2") * sm_TWa_2_MWh;
$ENDIF

totFixedLoad = totLoad - totFlexLoad;
d(h) = d_y_reg('2019',"DEU",h) * totFixedLoad / DIETER_OLDtotdem;


****************
*REMIND disinvestments cap: disinvestments = (early_reti(t) - early_reti(t-1) ) * cap(t) / (1 - early_reti(t)), it doesn't go into DIETER, I am only using DIETER for reporting

earlyRetiCap_reporting(yr, reg, te_remind)$(remind_capEarlyReti(yr, reg, te_remind) ne 1) = (remind_capEarlyReti(yr, reg, te_remind) - remind_capEarlyReti2("2015", reg, te_remind) ) * remind_cap(yr, reg, te_remind, "1")
                                                                            / (1 - remind_capEarlyReti(yr, reg, te_remind)) ;

****************
*AO* Match VRE CFs of DIETER to REMIND values
* General idea for wind: Read in 2019 input data for both wind onshore (CF 25%) and offshore (CF 50%).
*                        Calculate wind time series as a weighted average of onshore and offshore to match the REMIND CF.
* General idea for solar: Simply scale up or down time series
****************
Parameter
remind_VRECapFac(res)   "VRE capacity factors from REMIND"
remind_HydroCapFac      "Hydro capacity factor from REMIND"
dieter_VRECapFac(res)   "VRE capacity factors from time series input to DIETER"
share_wind_on_CF_match  "Share of required wind onshore power to match DIETER wind CF to REMIND values"
;

*AO* Calculate REMIND VRE CFs from grades
remind_VRECapFac("wind_on") = remind_CF("2020","DEU","wind") * sum(grade, remind_pm_dataren("DEU", "nur", grade, "wind") * remind_vm_CapDistr("2020", "DEU", "wind", grade) / remind_cap("2020", "DEU", "wind", "1"));
remind_VRECapFac("Solar") = remind_CF("2020","DEU","spv") * sum(grade, remind_pm_dataren("DEU", "nur", grade, "spv") * remind_vm_CapDistr("2020", "DEU", "spv", grade) / remind_cap("2020", "DEU", "spv", "1"));
remind_HydroCapFac = sum(grade, remind_pm_dataren("DEU", "nur", grade, "hydro") * remind_vm_CapDistr("2020", "DEU", "hydro", grade) / remind_cap("2020", "DEU", "hydro", "1"));

*AO* Calculate DIETER VRE CFs as given by the input data
dieter_VRECapFac(res) = sum(h, phi_res_y_reg("2019", "DEU", h, res)) / card(h);


*AO* Calculate necessary share of onshore wind to match DIETER wind CF to REMIND values according to:
* CF_{REMIND}  = x * CF_{DIETER, onshore} + (1 - x) * CF_{DIETER, offshore}
* ==> x = ( CF_{REMIND} - CF_{DIETER, offshore} ) / ( CF_{DIETER, onshore} - CF_{DIETER, offshore} ) 
share_wind_on_CF_match = (remind_VRECapFac("Wind_on") - dieter_VRECapFac("Wind_off")) / ( dieter_VRECapFac("Wind_on") - dieter_VRECapFac("Wind_off") );

* Limit 0<weight<1
share_wind_on_CF_match$(share_wind_on_CF_match > 1) = 1;
share_wind_on_CF_match$(share_wind_on_CF_match < 0) = 0;

*AO* Create time series of wind potential by calculating the weighted average of the actual wind onshore and wind offshore time series so that the CF of REMIND is matched
phi_res("Wind_on", h) = share_wind_on_CF_match * phi_res_y_reg("2019", "DEU", h, "Wind_on") + (1 - share_wind_on_CF_match) * phi_res_y_reg("2019", "DEU", h, "Wind_off");
*AO* Scale up time series of solar potential to match the CF of REMIND
phi_res("Solar", h) = phi_res_y_reg("2019", "DEU", h, "Solar") * remind_VRECapFac("Solar") / ( sum(hh, phi_res_y_reg("2019", "DEU", hh, "Solar")) / card(hh));

phi_res("Wind_on", h)$(phi_res("Wind_on", h) > 1)  = 1;
phi_res("Solar", h)$(phi_res("Solar", h) > 1)  = 1;
phi_res("Wind_on", h)$(phi_res("Wind_on", h) < 0)  = 0;

*AO* For hydro simply set CF to that of REMIND
capfac_ror = remind_HydroCapFac;

****************
*pass on VRE gen share from RM to DT instead of capacities, using the following transformation
*(total_generation X gen.share) / (cap.fac. X 8760) = capacity, where (total_generation X gen.share) = generation
*capacity = VRE_seProd / sum(h, cap.fac.(h))

* the prodSe that pre-investment REMIND sees in time step t: prodSe(t) -  pm_ts(t)/2 * prodSe(t) * (vm_deltacap(t)/vm_cap(t))
preInv_remind_prodSe(yr, "DEU", pe_remind, se_remind, te_remind)$(remind_cap(yr, "DEU", te_remind, "1") ne 0 ) = remind_prodSe(yr, "DEU", pe_remind, se_remind, te_remind)
                                                                       - remind_pm_ts(yr) /2 * remind_prodSe(yr, "DEU", pe_remind, se_remind, te_remind)
                                                                       * remind_deltaCap(yr, "DEU", te_remind, "1")
                                                                       /remind_cap(yr, "DEU", te_remind, "1");

**********************************************************************
*Remind post-investment cap ( TW-> MW )
RM_postInv_cap_con(yr,reg,"coal") = sum(te_remind, sum( grade, remind_cap(yr, reg, te_remind, grade)$(COALte(te_remind)) )) * 1e6;
RM_postInv_cap_con(yr,reg,"CCGT") =  sum(te_remind,sum( grade, remind_cap(yr, reg, te_remind, grade)$(NonPeakGASte(te_remind)) )) * 1e6;
RM_postInv_cap_con(yr,reg,"OCGT_eff") = sum( grade, remind_cap(yr, reg, "ngt", grade) ) * 1e6;
RM_postInv_cap_con(yr,reg,"bio") = sum(te_remind,sum( grade, remind_cap(yr, reg, te_remind, grade)$(BIOte(te_remind)) )) * 1e6;
RM_postInv_cap_con(yr,reg,"nuc") = sum(te_remind,sum( grade, remind_cap(yr, reg, te_remind, grade)$(NUCte(te_remind)) )) * 1e6;
RM_postInv_cap_res(yr,reg,"Solar") = sum( grade, remind_cap(yr, reg, "spv", grade)) * 1e6;
RM_postInv_cap_res(yr,reg,"Wind_on") = sum( grade, remind_cap(yr, reg, "wind", grade))* 1e6;
RM_postInv_cap_con(yr,reg,"ror") = sum( grade, remind_cap(yr, reg, "hydro", grade)) * 1e6;
RM_postInv_cap_p2g(yr,reg,"elh2") = sum( grade, remind_cap(yr, reg, "elh2", grade)) * 1e6;
RM_postInv_cap_grid(yr,reg,"vregrid") = sum( grade, remind_cap(yr, reg, "gridwind", grade)) * 1e6;

* Remind post-investment gen (excluding curtailment, only usable seel energy) for reporting ( TWa-> MWh )
RM_postInv_prodSe_con(yr,reg,"coal") = sum(te_remind, remind_prodSe(yr,reg, "pecoal", "seel", te_remind)$(COALte(te_remind)) )* sm_TWa_2_MWh;
RM_postInv_prodSe_con(yr,reg,"CCGT") = sum( te_remind, remind_prodSe(yr,reg, "pegas", "seel", te_remind)$(NonPeakGASte(te_remind)) )* sm_TWa_2_MWh;
RM_postInv_prodSe_con(yr,reg,"OCGT_eff") = remind_prodSe(yr,reg, "pegas", "seel", "ngt") * sm_TWa_2_MWh;
RM_postInv_prodSe_con(yr,reg,"bio") = sum(te_remind, remind_prodSe(yr,reg, "pebiolc", "seel", te_remind)$(BIOte(te_remind)) )* sm_TWa_2_MWh;
RM_postInv_prodSe_con(yr,reg,"nuc") = sum(te_remind, remind_prodSe(yr,reg, "peur", "seel", te_remind)$(NUCte(te_remind)) )* sm_TWa_2_MWh;
RM_postInv_prodSe_con(yr,reg,"ror") = remind_prodSe(yr, reg, "pehyd", "seel", "hydro")* sm_TWa_2_MWh;
RM_postInv_prodSe_res_xcurt(yr,reg,"Solar") = remind_prodSe_Resxcurt(yr, reg, "seel", "spv")* sm_TWa_2_MWh;
RM_postInv_prodSe_res_xcurt(yr,reg,"Wind_on") = remind_prodSe_Resxcurt(yr, reg, "seel", "wind")* sm_TWa_2_MWh ;
RM_postInv_demSe(yr,reg,"elh2") = totFlexLoad;
**********************************************************************

P_RES.fx(res)$sameas(res,"Wind_off") = 0; 
N_CON.fx("OCGT_ineff") = 0;

**********************************************************************
*********************** VALIDATION MODE ******************************
***   THIS MEANS CAP FROM REMIND IS PASSED AS LOWER BOUNDS ***********
**********************************************************************
*****************
* the cap that pre-investment REMIND sees in time step t: vm_cap(t) - pm_ts(t)/2 * vm_deltaCap(t) * (1-vm_earlyRetire) 
preInv_remind_cap(yr, "DEU", te_remind, grade) = remind_cap(yr, "DEU", te_remind, grade) - remind_pm_ts(yr) / 2 * remind_deltaCap(yr, "DEU", te_remind, grade) * (1 - remind_capEarlyReti(yr, "DEU", te_remind));
added_remind_cap(yr, "DEU", te_remind, grade) = remind_pm_ts(yr) / 2 * remind_deltaCap(yr, "DEU", te_remind, grade);


$IFTHEN.CB %cap_bound% == "hardLo"
P_RES.lo("Solar") = preInv_remind_prodSe("2020", "DEU", "pesol", "seel", "spv") * sm_TWa_2_MWh / ( remind_VRECapFac("Solar") * card(h)) * 1;
P_RES.lo("Wind_on") = preInv_remind_prodSe("2020", "DEU", "pewin", "seel", "wind") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_on") * card(h)) * 1;


*if (remind_iter < 7,
*P_RES.lo("Solar") = preInv_remind_prodSe("2020", "DEU", "pesol", "seel", "spv") * sm_TWa_2_MWh / ( remind_VRECapFac("Solar") * card(h)) * 0.8;
*P_RES.lo("Wind_on") = preInv_remind_prodSe("2020", "DEU", "pewin", "seel", "wind") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_on") * card(h)) * 0.8;
*);
*
*if (remind_iter > 6,
*P_RES.lo("Solar") = preInv_remind_prodSe("2020", "DEU", "pesol", "seel", "spv") * sm_TWa_2_MWh / ( remind_VRECapFac("Solar") * card(h)) * 0.9;
*P_RES.lo("Wind_on") = preInv_remind_prodSe("2020", "DEU", "pewin", "seel", "wind") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_on") * card(h)) * 0.9;
*);
*

N_CON.lo("ror") = preInv_remind_prodSe("2020", "DEU", "pehyd", "seel", "hydro") * sm_TWa_2_MWh / (capfac_ror * card(h)) ;


$IFTHEN.CS %coal_split% == "on"
* half-half split between lig and hc for DEU
* TW-> MW
N_CON.lo("lig") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6 /2;

N_CON.lo("hc") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6 /2;
$ENDIF.CS

$IFTHEN.CS %coal_split% == "off"
N_CON.lo("lig") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6;
                    
N_CON.fx("hc") = 0;
$ENDIF.CS


N_CON.lo("nuc") = sum(te_remind,
                   sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(NUCte(te_remind))   )
                    ) * 1e6;

N_CON.lo("CCGT") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(NonPeakGASte(te_remind))   )
                    ) * 1e6;

N_CON.lo("OCGT_eff") = sum(grade, preInv_remind_cap("2020", "DEU", "ngt", grade)) * 1e6 ;

      
N_CON.lo("bio") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(BIOte(te_remind))   )
                    ) * 1e6;

$IFTHEN.H2 %elh2_coup% == "on"
N_P2G.lo("elh2") = sum(   grade, preInv_remind_cap("2020", "DEU", "elh2", grade)  ) * 1e6;
$ENDIF.H2

*** gridwind is the only grid tech in REMIND
N_GRID.lo("vregrid") = sum(   grade, preInv_remind_cap("2020", "DEU", "gridwind", grade)  ) * 1e6;

$ENDIF.CB


$IFTHEN.CB %cap_bound% == "softLo1"
P_RES.lo("Solar") = preInv_remind_prodSe("2020", "DEU", "pesol", "seel", "spv") * sm_TWa_2_MWh / ( remind_VRECapFac("Solar") * card(h)) * 0.8;
P_RES.lo("Wind_on") = preInv_remind_prodSe("2020", "DEU", "pewin", "seel", "wind") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_on") * card(h)) * 0.8;

$IFTHEN.CS %coal_split% == "on"
N_CON.lo("lig") = min(sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6 /2,.2*SMax(h, d(h)));

N_CON.lo("hc") = min(sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6 /2,.2*SMax(h, d(h)));
$ENDIF.CS

$IFTHEN.CS %coal_split% == "off"
N_CON.lo("lig") = min(sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6,.2*SMax(h, d(h)));
                    
N_CON.fx("hc") = 0;
$ENDIF.CS

N_CON.lo("ror") = min(preInv_remind_prodSe("2020", "DEU", "pehyd", "seel", "hydro") * sm_TWa_2_MWh / (capfac_ror * card(h)) ,.2*SMax(h, d(h)));
                    
N_CON.lo("nuc") = min(sum(te_remind,
                   sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(NUCte(te_remind))   )
                    ) * 1e6,.2*SMax(h, d(h)));

N_CON.lo("CCGT") = min(sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(NonPeakGASte(te_remind))   )
                    ) * 1e6,.2*SMax(h, d(h)));

N_CON.lo("OCGT_eff") = min(sum(grade, preInv_remind_cap("2020", "DEU", "ngt", grade)) * 1e6,.2*SMax(h, d(h)));

      
N_CON.lo("bio") = min(sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(BIOte(te_remind))   )
                    ) * 1e6,.2*SMax(h, d(h)));
$ENDIF.CB

$IFTHEN.CB %cap_bound% == "softLo2"
P_RES.lo("Solar") = preInv_remind_prodSe("2020", "DEU", "pesol", "seel", "spv") * sm_TWa_2_MWh / ( remind_VRECapFac("Solar") * card(h)) * 0.8;
P_RES.lo("Wind_on") = preInv_remind_prodSe("2020", "DEU", "pewin", "seel", "wind") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_on") * card(h)) * 0.8;

$IFTHEN.CS %coal_split% == "on"
N_CON.lo("lig") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6 /2 * 0.8;

N_CON.lo("hc") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6 /2 * 0.8;
$ENDIF.CS

$IFTHEN.CS %coal_split% == "off"
N_CON.lo("lig") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6 * 0.8;
                    
N_CON.fx("hc") = 0;
$ENDIF.CS

N_CON.lo("ror") = preInv_remind_prodSe("2020", "DEU", "pehyd", "seel", "hydro") * sm_TWa_2_MWh / (capfac_ror * card(h)) * 0.8;
                    
N_CON.lo("nuc") = sum(te_remind,
                   sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(NUCte(te_remind))   )
                    ) * 1e6 * 0.8;

N_CON.lo("CCGT") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(NonPeakGASte(te_remind))   )
                    ) * 1e6 * 0.8;

N_CON.lo("OCGT_eff") = sum(grade, preInv_remind_cap("2020", "DEU", "ngt", grade)) * 1e6 * 0.8;

      
N_CON.lo("bio") = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(BIOte(te_remind))   )
                    ) * 1e6 * 0.8;
$ENDIF.CB

**** earlyReti is deprecated (and missing ror)
$IFTHEN.CB %cap_bound% == "earlyReti" 
P_RES.lo("Solar") = preInv_remind_prodSe("2020", "DEU", "pesol", "seel", "spv") * sm_TWa_2_MWh / ( remind_VRECapFac("Solar") * card(h)) * 1;
P_RES.lo("Wind_on") = preInv_remind_prodSe("2020", "DEU", "pewin", "seel", "wind") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_on") * card(h)) * 1;

***CG: implementation for DIETER lower bound to only kick in in the absence of early retirement in last iteration REMIND (turned off since it lead to non_optimal)
*** in the oscillation situation
*** if no early retirement from last REMIND iteration (capacity only can grow), DIETER gets REMIND capacity as lower bound      
N_CON.lo("lig")$(sum(te_remind, earlyRetiCap_reporting("2020", "DEU", te_remind)$(COALte(te_remind))) = 0) = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6 /2;

N_CON.lo("hc")$(sum(te_remind, earlyRetiCap_reporting("2020", "DEU", te_remind)$(COALte(te_remind))) = 0) = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(COALte(te_remind))   )
                    ) * 1e6 /2;
                    
N_CON.lo("nuc")$(sum(te_remind, earlyRetiCap_reporting("2020", "DEU", te_remind)$(NUCte(te_remind))) = 0)  = sum(te_remind,
                   sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(NUCte(te_remind))   )
                  ) * 1e6;

N_CON.lo("CCGT")$(sum(te_remind, earlyRetiCap_reporting("2020", "DEU", te_remind)$(NonPeakGASte(te_remind))) = 0) = sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(NonPeakGASte(te_remind))   )
                    ) * 1e6;

N_CON.lo("OCGT_eff")$(earlyRetiCap_reporting("2020", "DEU", "ngt") = 0) = sum(grade, preInv_remind_cap("2020", "DEU", "ngt", grade)) * 1e6;

      
N_CON.lo("bio")$(sum(te_remind, earlyRetiCap_reporting("2020", "DEU", te_remind)$(BIOte(te_remind))) = 0) =  sum(te_remind,
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade)$(BIOte(te_remind))   )
                    ) * 1e6;

**** if there is early retirement from last REMIND iteration, DIETER gets REMIND capacity as fx
N_CON.fx("lig")$(sum(te_remind, earlyRetiCap_reporting("2020", "DEU", te_remind)$(COALte(te_remind))) > 0) =
                        RM_postInv_cap_con("2020", "DEU", "coal") * 1e6 /2;

N_CON.fx("hc")$(sum(te_remind, earlyRetiCap_reporting("2020", "DEU", te_remind)$(COALte(te_remind))) > 0) =
                        RM_postInv_cap_con("2020", "DEU", "coal") * 1e6 /2;
                    
N_CON.fx("nuc")$(sum(te_remind, earlyRetiCap_reporting("2020", "DEU", te_remind)$(NUCte(te_remind))) > 0)  =
                        RM_postInv_cap_con("2020", "DEU", "nuc") * 1e6;

N_CON.fx("CCGT")$(sum(te_remind, earlyRetiCap_reporting("2020", "DEU", te_remind)$(NonPeakGASte(te_remind))) > 0) =
                        RM_postInv_cap_con("2020", "DEU", "CCGT") * 1e6;

N_CON.fx("OCGT_eff")$(earlyRetiCap_reporting("2020", "DEU", "ngt") > 0) = RM_postInv_cap_con("2020", "DEU", "OCGT_eff") * 1e6;


N_CON.fx("bio")$(sum(te_remind, earlyRetiCap_reporting("2020", "DEU", te_remind)$(BIOte(te_remind))) > 0) =
                        RM_postInv_cap_con("2020", "DEU", "bio") * 1e6;

$ENDIF.CB
**********************************************************************
*********************** END OF VALIDATION MODE ***********************
**********************************************************************

**********************************************************************
*********************** COUPLED MODE ******************************
***   THIS MEANS CAP FROM REMIND IS PASSED AS FIXED BOUNDS ********
**********************************************************************
$IFTHEN.CB %cap_bound% == "fixed"
P_RES.fx("Solar") = remind_prodSe("2020", "DEU", "pesol", "seel", "spv") * sm_TWa_2_MWh / (remind_VRECapFac("Solar") * 8760);
P_RES.fx("Wind_on") = remind_prodSe("2020", "DEU", "pewin", "seel", "wind") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_on") * 8760) ;
N_CON.fx("ror") = remind_prodSe("2020", "DEU", "pehyd", "seel", "hydro") * sm_TWa_2_MWh / (remind_HydroCapFac * 8760) ;
N_CON.fx("CCGT")= RM_postInv_cap_con("2020", "DEU", "CCGT") ;
N_CON.fx("OCGT_eff")= RM_postInv_cap_con("2020", "DEU", "OCGT_eff") ;
N_CON.fx("bio")= RM_postInv_cap_con("2020", "DEU", "bio") ;
N_CON.fx("nuc")= RM_postInv_cap_con("2020", "DEU", "nuc") ;
N_CON.fx("lig") = RM_postInv_cap_con("2020", "DEU", "coal")/2 ;
N_CON.fx("hc") = RM_postInv_cap_con("2020", "DEU", "coal")/2 ;
$ENDIF.CB
**********************************************************************
*********************** END OF COUPLED MODE ***********************
**********************************************************************


*N_STO_P.fx('Sto1') = remind_cap("2020", "DEU", "storspv", "1") * 3 * 1e6 + remind_cap("2020", "DEU", "storwind", "1") * 0.3* 1e6;

N_STO_P.fx(sto) = 0 ;
N_STO_P.fx(sto) = 0 ;
N_STO_E.fx(sto) = 0 ;
STO_IN.fx(sto,h) = 0 ;
STO_OUT.fx(sto,h) = 0 ;
STO_L.fx(sto,h) = 0 ;
RP_STO_IN.fx(reserves,sto,h) = 0 ;
RP_STO_OUT.fx(reserves,sto,h) = 0 ;

*================================================================
*======================= VARIABLE COST =============================
*================================================================
*================ read in fuel price from remind ================


*smooth/manipulate biomass PE price to a linear function
$IFTHEN.FC %fuel_cost_iter% == "smoothed"
*t(yr) = yr.val;
*if (sum(yr,t(yr)) lt 2055,
**if ((remind_iter eq 0),
remind_fuelprice("2020",reg,"pebiolc") = (remind_fuelprice("2150",reg,"pebiolc") - remind_fuelprice("2005",reg,"pebiolc"))/(2150 - 2005) * (2020 - 2005) + remind_fuelprice("2005",reg,"pebiolc");
remind_fuelprice("2020",reg,"pegas") = (remind_fuelprice("2150",reg,"pegas") - remind_fuelprice("2005",reg,"pegas"))/(2150 - 2005) * (2020 - 2005) + remind_fuelprice("2005",reg,"pegas");
remind_fuelprice("2020",reg,"pecoal") = (remind_fuelprice("2150",reg,"pecoal") - remind_fuelprice("2005",reg,"pecoal"))/(2150 - 2005) * (2020 - 2005) + remind_fuelprice("2005",reg,"pecoal");
**);
*);
$ENDIF.FC

*1.2 is the conversion btw twothousandfive$ and twentyfifteen$
*1e12 is the conversion btw Trillion$ to $

$IFTHEN.FC  %fuel_cost_iter% == "cubicFit"
*** NO need for unit conversion
$IFTHEN.CS %coal_split% == "on"
** split fuel cost of pecoal into lignite and hc for rough comparison (not finalized)
con_fuelprice_reg_remind("2020","lig",reg) = remind_fuelprice("2020",reg,"pecoal") - 3.6;
con_fuelprice_reg_remind("2020","hc",reg) = remind_fuelprice("2020",reg,"pecoal") + 1.8;
$ENDIF.CS

$IFTHEN.CS %coal_split% == "off"
con_fuelprice_reg_remind("2020","lig",reg) = remind_fuelprice("2020",reg,"pecoal");
con_fuelprice_reg_remind("2020","hc",reg) = remind_fuelprice("2020",reg,"pecoal");
$ENDIF.CS

con_fuelprice_reg_remind("2020","CCGT",reg) = remind_fuelprice("2020",reg,"pegas");
con_fuelprice_reg_remind("2020","OCGT_eff",reg) = remind_fuelprice("2020",reg,"pegas");
con_fuelprice_reg_remind("2020","nuc",reg) = remind_fuelprice("2020",reg,"peur");
con_fuelprice_reg_remind("2020","ror",reg) = 0;
con_fuelprice_reg_remind("2020","bio",reg) = remind_fuelprice("2020",reg,"pebiolc");
$ENDIF.FC



$IFTHEN.FC  %fuel_cost_iter% == "smoothed"
*** need unit conversion
$IFTHEN.CS %coal_split% == "on"
** split fuel cost of pecoal into lignite and hc for rough comparison (not finalized)
con_fuelprice_reg_remind("2020","lig",reg) = remind_fuelprice("2020",reg,"pecoal") * 1e12 / sm_TWa_2_MWh * 1.2 - 3.6;
con_fuelprice_reg_remind("2020","hc",reg) = remind_fuelprice("2020",reg,"pecoal") * 1e12 / sm_TWa_2_MWh * 1.2 + 1.8;
$ENDIF.CS

$IFTHEN.CS %coal_split% == "off"
con_fuelprice_reg_remind("2020","lig",reg) = remind_fuelprice("2020",reg,"pecoal") * 1e12 / sm_TWa_2_MWh * 1.2;
con_fuelprice_reg_remind("2020","hc",reg) = remind_fuelprice("2020",reg,"pecoal") * 1e12 / sm_TWa_2_MWh * 1.2;
$ENDIF.CS

con_fuelprice_reg_remind("2020","CCGT",reg) = remind_fuelprice("2020",reg,"pegas") * 1e12 / sm_TWa_2_MWh * 1.2;
con_fuelprice_reg_remind("2020","OCGT_eff",reg) = remind_fuelprice("2020",reg,"pegas") * 1e12 / sm_TWa_2_MWh * 1.2;
con_fuelprice_reg_remind("2020","nuc",reg) = remind_fuelprice("2020",reg,"peur") * 1e12 / sm_TWa_2_MWh * 1.2;
con_fuelprice_reg_remind("2020","ror",reg) = 0;
con_fuelprice_reg_remind("2020","bio",reg) = remind_fuelprice("2020",reg,"pebiolc") * 1e12 / sm_TWa_2_MWh * 1.2;
$ENDIF.FC
 
con_fuelprice_reg_remind_reporting(ct,reg) = con_fuelprice_reg_remind("2020",ct,reg);


$IFTHEN.FC2 %fuel_cost_yr% == "avg"
*smooth over 3 years regardless whether fuel cost if smoothed over iteration or fixed to first iteration
con_fuelprice_reg_yr_avg(ct,reg) = (con_fuelprice_reg_remind("2015",ct,reg) + 2 * con_fuelprice_reg_remind("2020",ct,reg) + con_fuelprice_reg_remind("2025",ct,reg)) / 4;
$ENDIF.FC2

$IFTHEN.FC2 %fuel_cost_yr% == "no_avg"
con_fuelprice_reg_yr_avg(ct,reg) =  con_fuelprice_reg_remind("2020",ct,reg);
$ENDIF.FC2

*================================================================

****** fuel efficiency eta from REMIND ***** 
** split pecoal into lignite and hc for rough comparison (not finalized), adding eta1 and eta2 together since sometimes eta from REMIND is stored in one parameter, sometimes the other
cdata("eta_con","lig")$(RM_postInv_prodSe_con("2020", "DEU","coal") ne 0)
    = sum(COALte, (remind_eta1("2020","DEU", COALte)+remind_eta2("2020","DEU", COALte)) * remind_prodSe("2020", "DEU", "pecoal", "seel", COALte))
     / sum(COALte, remind_prodSe("2020", "DEU", "pecoal", "seel", COALte));
cdata("eta_con","CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") ne 0)
    = sum(NonPeakGASte, (remind_eta1("2020","DEU", NonPeakGASte)+remind_eta2("2020","DEU", NonPeakGASte) ) * remind_prodSe("2020", "DEU", "pegas", "seel",NonPeakGASte))
     / sum(NonPeakGASte, remind_prodSe("2020", "DEU", "pegas", "seel",NonPeakGASte));
cdata("eta_con","OCGT_eff") = remind_eta1("2020","DEU","ngt");
cdata("eta_con","bio")$(RM_postInv_prodSe_con("2020", "DEU","bio") ne 0)
    = sum(BIOte, (remind_eta1("2020","DEU", BIOte) + remind_eta2("2020","DEU", BIOte)) * remind_prodSe("2020", "DEU", "pebiolc", "seel", BIOte))
     / sum(BIOte, remind_prodSe("2020", "DEU", "pebiolc", "seel", BIOte)); 
cdata("eta_con","ror") = remind_eta2("2020","DEU","hydro");
cdata("eta_con","nuc")$(RM_postInv_prodSe_con("2020", "DEU","nuc") ne 0)
    = sum(NUCte, (remind_eta1("2020","DEU", NUCte) + remind_eta2("2020","DEU", NUCte)) * remind_prodSe("2020", "DEU", "peur", "seel", NUCte))
     / sum(NUCte, remind_prodSe("2020", "DEU", "peur", "seel", NUCte));

*if there is no generation in REMIND, then just take the average eta value of REMIND techs in one category
cdata("eta_con","lig")$(RM_postInv_prodSe_con("2020", "DEU","coal") eq 0)=sum(COALte,(remind_eta1("2020","DEU", COALte)+remind_eta2("2020","DEU", COALte)))/card(COALte);
cdata("eta_con","hc") = cdata("eta_con","lig") * 0.92;
cdata("eta_con","CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") eq 0)=sum(NonPeakGASte,(remind_eta1("2020","DEU", NonPeakGASte)+remind_eta2("2020","DEU", NonPeakGASte) ) )/card(NonPeakGASte);
cdata("eta_con","bio")$(RM_postInv_prodSe_con("2020", "DEU","bio") eq 0)=sum(BIOte, (remind_eta1("2020","DEU", BIOte) + remind_eta2("2020","DEU", BIOte)))/card(BIOte);
*not averaging for nuclear since fnrs is small for the most part: though this should be checked
cdata("eta_con","nuc")$(RM_postInv_prodSe_con("2020", "DEU","nuc") eq 0)=remind_eta2("2020","DEU","tnrs");

***** carbon content from REMIND (average over REMIND te since CCS plants have lower carbon content) ***** 
*dieter value (tCO2/MWh) = REMIND value (GtC/TWa) * (sm_c_2_co2 * sm_Gt_2_t) / sm_TWa_2_MWh) = REMIND value * (44/12 * 1e9) / (8760000000) 
cdata("carbon_content","lig")$(RM_postInv_prodSe_con("2020", "DEU","coal") ne 0)
    = sum(COALte, remind_carboncontent("pecoal","seel",COALte,"co2") * remind_prodSe("2020", "DEU", "pecoal", "seel", COALte))
     / sum(COALte, remind_prodSe("2020", "DEU", "pecoal", "seel", COALte)) * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;
cdata("carbon_content","CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") ne 0)
    = sum(NonPeakGASte, remind_carboncontent("pegas","seel",NonPeakGASte,"co2") * remind_prodSe("2020", "DEU", "pegas", "seel",NonPeakGASte))
     / sum(NonPeakGASte, remind_prodSe("2020", "DEU", "pegas", "seel",NonPeakGASte)) * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;
cdata("carbon_content","OCGT_eff") = remind_carboncontent("pegas","seel","ngt","co2") * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;

*if there is no generation in REMIND, then just take the average carbon content value of REMIND techs
cdata("carbon_content","lig")$(RM_postInv_prodSe_con("2020", "DEU","coal") eq 0)
    = sum(COALte,remind_carboncontent("pecoal","seel", COALte,"co2"))/card(COALte) * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;
cdata("carbon_content","hc") = cdata("carbon_content","lig");
cdata("carbon_content","CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") eq 0)
    = sum(NonPeakGASte,remind_carboncontent("pegas","seel",NonPeakGASte,"co2"))/card(NonPeakGASte) * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;

*omv's unit in fulldata.gdx is T$(2005)/TWa, multiply by 1.2 to T$(2015)/TWa then multiply * 1e12 to get $(2015)/TWa, divides sm_TWa_2_MWh to get $(2015)/MWh

***** variable O&M from REMIND ***** 
cdata("c_var_con","lig")$(RM_postInv_prodSe_con("2020", "DEU","coal") ne 0)
    = sum(COALte, remind_OMcost("DEU","omv",COALte) * remind_prodSe("2020", "DEU", "pecoal", "seel", COALte))
     / sum(COALte, remind_prodSe("2020", "DEU", "pecoal", "seel", COALte)) *1.2*1e12/sm_TWa_2_MWh;

cdata("c_var_con","CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") ne 0)
    = sum(NonPeakGASte, remind_OMcost("DEU","omv",NonPeakGASte) * remind_prodSe("2020", "DEU", "pegas", "seel",NonPeakGASte))
     / sum(NonPeakGASte, remind_prodSe("2020", "DEU", "pegas", "seel",NonPeakGASte)) *1.2*1e12/sm_TWa_2_MWh;
     
cdata("c_var_con","OCGT_eff") = remind_OMcost("DEU","omv","ngt")  *1.2*1e12/sm_TWa_2_MWh;

cdata("c_var_con","bio")$(RM_postInv_prodSe_con("2020", "DEU","bio") ne 0)
    = sum(BIOte, remind_OMcost("DEU","omv",BIOte) * remind_prodSe("2020", "DEU", "pebiolc", "seel",BIOte))
     / sum(BIOte, remind_prodSe("2020", "DEU", "pebiolc", "seel",BIOte)) *1.2*1e12/sm_TWa_2_MWh;

cdata("c_var_con","ror") = remind_OMcost("DEU","omv","hydro")  *1.2*1e12/sm_TWa_2_MWh;

cdata("c_var_con","nuc")$(RM_postInv_prodSe_con("2020", "DEU","nuc") ne 0)
    = sum(NUCte, remind_OMcost("DEU","omv",NUCte) * remind_prodSe("2020", "DEU", "peur", "seel",NUCte))
     / sum(NUCte, remind_prodSe("2020", "DEU", "peur", "seel",NUCte)) *1.2*1e12/sm_TWa_2_MWh;

*if there is no generation in REMIND, then just take the average omv value over techs in one given category
cdata("c_var_con","lig")$(RM_postInv_prodSe_con("2020", "DEU","coal") eq 0) = sum(COALte, remind_OMcost("DEU","omv",COALte))/card(COALte)*1.2*1e12/sm_TWa_2_MWh;
cdata("c_var_con","hc") = cdata("c_var_con","lig");
cdata("c_var_con","CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") eq 0) = sum(NonPeakGASte, remind_OMcost("DEU","omv",NonPeakGASte))/card(NonPeakGASte)*1.2*1e12/sm_TWa_2_MWh;
cdata("c_var_con","bio")$(RM_postInv_prodSe_con("2020", "DEU","bio") eq 0) = sum(BIOte, remind_OMcost("DEU","omv",BIOte))/card(BIOte)*1.2*1e12/sm_TWa_2_MWh;
*not averaging for nuclear since fnrs is small for the most part: though this should be checked
cdata("c_var_con","nuc")$(RM_postInv_prodSe_con("2020", "DEU","nuc") eq 0) = remind_OMcost("DEU","omv","tnrs")*1.2*1e12/sm_TWa_2_MWh;

** there is no var OM cost for VRE in REMIND
*rdata("c_var_res","Solar") = remind_OMcost("DEU","omv","spv") * 1.2 * 1e12 / sm_TWa_2_MWh;
*rdata("c_var_res","Wind_on") = remind_OMcost("DEU","omv","wind") * 1.2 * 1e12 / sm_TWa_2_MWh;

p2gdata("c_var_p2g","elh2") = remind_OMcost("DEU","omv","elh2") * 1.2 * 1e12 / sm_TWa_2_MWh;

***** END of variable O&M from REMIND *****

$IFTHEN.FC3 %fuel_cost_suppc% == "no_suppcurve"
***** summing variable cost components
c_m_reg(ct,reg) = con_fuelprice_reg_yr_avg(ct,reg)/cdata("eta_con",ct) + cdata("carbon_content",ct)/cdata("eta_con",ct) * remind_flatco2("2020",reg) * 1.2 + cdata("c_var_con",ct) ;
c_m(ct) = c_m_reg(ct,"DEU");
$ENDIF.FC3

** CG: currently, suppcurve turns DIETER from LP to NLP
$IFTHEN.FC3 %fuel_cost_suppc% == "suppcurve" 
** with supply curve response in DIETER: building linear demand/price relation to help with convergence
** CG: non reactive part of the marginal cost
c_m_reg_nrp(ct,reg) = cdata("carbon_content",ct)/cdata("eta_con",ct) * remind_flatco2("2020",reg) * 1.2 + cdata("c_var_con",ct) ;
c_m_nrp(ct) = c_m_reg_nrp(ct,"DEU");
c_m_FC(ct) = con_fuelprice_reg_yr_avg(ct,"DEU")/cdata("eta_con",ct);
$ENDIF.FC3

*================================================================
*======================= FIXED COST =============================
*once REMIND starts, read in the interest rate from REMIND
r = remind_r("2020","DEU");

*===================== annuitized investment cost (calculated from full lifetime in REMIND, i.e no early retirement) ==================
*disc.fac = r * (1+r)^lifetime/(-1+(1+r)^lifetime)
*note on tech harmonization: ngcc, ngccc, gaschp have the same lifetime in REMIND, 35 years; igcc, igccc, pc, pcc, pco, coalchp all have 40 years; biochp, bioigcc, bioigccc have 40 years
*fnrs and tnrs have 50 years. So no need to harmonize the multiple techs
disc_fac_con("lig") = r * (1+r) ** remind_lifetime("lifetime", "pc") / (-1+(1+r) ** remind_lifetime("lifetime", "pc")) ;
disc_fac_con("hc") = disc_fac_con("lig");
disc_fac_con("CCGT") = r * (1+r) ** remind_lifetime("lifetime", "ngcc") / (-1+(1+r) ** remind_lifetime("lifetime", "ngcc")) ;
disc_fac_con("OCGT_eff") = r * (1+r) ** remind_lifetime("lifetime", "ngt") / (-1+(1+r) ** remind_lifetime("lifetime", "ngt")) ;
disc_fac_con("bio") = r * (1+r) ** remind_lifetime("lifetime", "bioigcc") / (-1+(1+r) ** remind_lifetime("lifetime", "bioigcc")) ;
disc_fac_con("ror") = r * (1+r) ** remind_lifetime("lifetime", "hydro") / (-1+(1+r) ** remind_lifetime("lifetime", "hydro")) ;
disc_fac_con("nuc") = r * (1+r) ** remind_lifetime("lifetime", "tnrs") / (-1+(1+r) ** remind_lifetime("lifetime", "tnrs")) ;

disc_fac_res("Solar") = r * (1+r) ** remind_lifetime("lifetime", "spv") / (-1+(1+r) ** remind_lifetime("lifetime", "spv")) ;
disc_fac_res("Wind_on") = r * (1+r) ** remind_lifetime("lifetime", "wind") / (-1+(1+r) ** remind_lifetime("lifetime", "wind")) ;

disc_fac_p2g("elh2") = r * (1+r) ** remind_lifetime("lifetime", "elh2") / (-1+(1+r) ** remind_lifetime("lifetime", "elh2")) ;
disc_fac_grid("vregrid") = r * (1+r) ** remind_lifetime("lifetime", "gridwind") / (-1+(1+r) ** remind_lifetime("lifetime", "gridwind")) ;


*===================== annuitized investment cost (calculated from discounted lifetime REMIND) ==================
*disc_fac_con("lig")$(RM_postInv_prodSe_con("2020", "DEU","coal") ne 0)
*        = sum(COALte, remind_annuity(COALte) * remind_prodSe("2020", "DEU", "pecoal", "seel", COALte))
*              / sum(COALte, remind_prodSe("2020", "DEU", "pecoal", "seel", COALte));
*disc_fac_con("hc") = disc_fac_con("lig");
*
*disc_fac_con("CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") ne 0)
*            = sum(NonPeakGASte, remind_annuity(NonPeakGASte) * remind_prodSe("2020", "DEU", "pegas", "seel",NonPeakGASte))
*              / sum(NonPeakGASte, remind_prodSe("2020", "DEU", "pegas", "seel",NonPeakGASte));
*disc_fac_con("OCGT_eff") = remind_annuity("ngt");
*disc_fac_con("bio")$(RM_postInv_prodSe_con("2020", "DEU","bio") ne 0)
*            = sum(BIOte, remind_annuity(BIOte) * remind_prodSe("2020", "DEU", "pebiolc", "seel", BIOte))
*              / sum(BIOte, remind_prodSe("2020", "DEU", "pebiolc", "seel", BIOte));
*disc_fac_con("ror") = remind_annuity("hydro");
*disc_fac_con("nuc")$(RM_postInv_prodSe_con("2020", "DEU","nuc") ne 0)
*            = sum(NUCte, remind_annuity(NUCte) * remind_prodSe("2020", "DEU", "peur", "seel", NUCte))
*              / sum(NUCte, remind_prodSe("2020", "DEU", "peur", "seel", NUCte)) * 1e6 * 1.2;
*
*disc_fac_res("Solar") = remind_annuity("spv") ;
*disc_fac_res("Wind_on") = remind_annuity("wind") ;
*disc_fac_p2g("elh2") = remind_annuity("elh2") ;
*disc_fac_grid("vregrid") = remind_annuity("gridwind") ;

              
*======= read in investment cost from remind ========
*overnight investment cost
*# *# conversion from tr USD_twothousandfive/TW to USD_twentyfifteen/MW
** split pecoal into lignite and hc for rough comparison (not finalized):  set lignite as REMIND-pc cost + 300€/kW
** weighted average of many techs in REMIND
c_i_ovnt("lig")$(RM_postInv_prodSe_con("2020", "DEU","coal") ne 0)
            = sum(COALte, remind_CapCost("2020", "DEU", COALte) * remind_prodSe("2020", "DEU", "pecoal", "seel", COALte))
              / sum(COALte, remind_prodSe("2020", "DEU", "pecoal", "seel", COALte)) * 1e6 * 1.2;
c_i_ovnt("CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") ne 0)
            = sum(NonPeakGASte, remind_CapCost("2020", "DEU", NonPeakGASte) * remind_prodSe("2020", "DEU", "pegas", "seel",NonPeakGASte))
              / sum(NonPeakGASte, remind_prodSe("2020", "DEU", "pegas", "seel",NonPeakGASte)) * 1e6 * 1.2;
c_i_ovnt("OCGT_eff") = remind_CapCost("2020", "DEU", "ngt") * 1e6 * 1.2;
c_i_ovnt("bio")$(RM_postInv_prodSe_con("2020", "DEU","bio") ne 0)
            = sum(BIOte, remind_CapCost("2020", "DEU", BIOte) * remind_prodSe("2020", "DEU", "pebiolc", "seel", BIOte))
              / sum(BIOte, remind_prodSe("2020", "DEU", "pebiolc", "seel", BIOte)) * 1e6 * 1.2;
*regardless of generation, overnight cost for hydro is the same, no downscaling              
c_i_ovnt("ror") = remind_CapCost("2020", "DEU", "hydro") * 1e6 * 1.2;
c_i_ovnt("nuc")$(RM_postInv_prodSe_con("2020", "DEU","nuc") ne 0)
            = sum(NUCte, remind_CapCost("2020", "DEU", NUCte) * remind_prodSe("2020", "DEU", "peur", "seel", NUCte))
              / sum(NUCte, remind_prodSe("2020", "DEU", "peur", "seel", NUCte)) * 1e6 * 1.2;
 
*in case no generation in remind, take the average (except nuc, nuc just uses tnrs)
c_i_ovnt("lig")$(RM_postInv_prodSe_con("2020", "DEU","coal") eq 0) = sum(COALte, remind_CapCost("2020", "DEU", COALte))/card(COALte) * 1e6 * 1.2;
c_i_ovnt("hc") = c_i_ovnt("lig") + 300000;
c_i_ovnt("CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") eq 0)
            = sum(NonPeakGASte, remind_CapCost("2020", "DEU", NonPeakGASte))/card(NonPeakGASte) * 1e6 * 1.2;
c_i_ovnt("bio")$(RM_postInv_prodSe_con("2020", "DEU","bio") eq 0)
            = sum(BIOte, remind_CapCost("2020", "DEU", BIOte))/card(BIOte) * 1e6 * 1.2;              
c_i_ovnt("nuc")$(RM_postInv_prodSe_con("2020", "DEU","nuc") eq 0)
            = remind_CapCost("2020", "DEU", "tnrs") * 1e6 * 1.2;
c_i_ovnt_res("Solar") = remind_CapCost("2020", "DEU", "spv") * 1e6 * 1.2 ;
c_i_ovnt_res("Wind_on") = remind_CapCost("2020", "DEU", "wind") * 1e6 * 1.2;

* since capacity of elh2 is in MW H2 unit (not MW_el like in DIETER, we need to multiply the efficiency of electrolyzer to obtain the capex for elh2)
c_i_ovnt_p2g("elh2") = remind_CapCost("2020", "DEU", "elh2") * 1e6 * 1.2 * remind_eta2("2020","DEU","elh2");
c_i_ovnt_grid("vregrid") = remind_CapCost("2020", "DEU", "gridwind") * 1e6 * 1.2;

*annuitized investment cost
c_i(ct) = c_i_ovnt(ct) * disc_fac_con(ct);
c_i_res(res) = c_i_ovnt_res(res) * disc_fac_res(res);
c_i_p2g(p2g) = c_i_ovnt_p2g(p2g) * disc_fac_p2g(p2g);
c_i_grid(grid) = c_i_ovnt_grid(grid) * disc_fac_grid(grid);
*================================================================
*=======read in fixed OM cost from REMIND ========
*note that omf is the proportion from overnight investment cost, not annuitized
** split pecoal into lignite and hc for rough comparison (not finalized)annuitized
** no need to harmonize many to one mapping, since omf are the same for tech in the same category
cdata("c_fix_con","lig") = remind_OMcost("DEU","omf","pc") * c_i_ovnt("lig");
cdata("c_fix_con","hc") = remind_OMcost("DEU","omf","pc") * c_i_ovnt("hc") ;
cdata("c_fix_con","CCGT") = remind_OMcost("DEU","omf","ngcc") * c_i_ovnt("CCGT");
cdata("c_fix_con","OCGT_eff") = remind_OMcost("DEU","omf","ngt") * c_i_ovnt("OCGT_eff");
cdata("c_fix_con","bio") = remind_OMcost("DEU","omf","bioigcc") * c_i_ovnt("bio");
cdata("c_fix_con","ror") = remind_OMcost("DEU","omf","hydro") * c_i_ovnt("ror");
cdata("c_fix_con","nuc") = remind_OMcost("DEU","omf","tnrs") * c_i_ovnt("nuc");

rdata("c_fix_res","Solar") = remind_OMcost("DEU","omf","spv") * c_i_ovnt_res("Solar");
rdata("c_fix_res","Wind_on") = remind_OMcost("DEU","omf","wind") * c_i_ovnt_res("Wind_on");

p2gdata("c_fix_p2g","elh2") = remind_OMcost("DEU","omf","elh2") * c_i_ovnt_p2g("elh2");
griddata("c_fix_grid","vregrid") = remind_OMcost("DEU","omf","gridwind") * c_i_ovnt_grid("vregrid");

remind_gridfac_reg = remind_gridfac("DEU");

Equations
* Objective
obj                      Objective cost minimization

* Energy balance
con1a_bal                Supply Demand Balance in case of cost minimization

* Flex load total
eq1_flexload             total P2G demand
* P2G capacity constraint
eq2_minprod_elh2         P2G capacity factor lower bound
eq2_maxprod_p2g          P2G capacity constraint

eq3_grid                 VRE grid constraint

* Load change costs
con2a_loadlevel          Load change costs: Level
con2b_loadlevelstart     Load change costs: Level for first period
con2c_flexloadlevel      Load change costs: Level
con2d_flexloadlevelstart Load change costs: Level for first period

*full-load hours
con2c_maxprodannual_conv Full load hour constraint (for non-nuclear conventional)
con2c_maxprodannual_conv_nuc Full load hour constraint (for coal)

* Capacity contraints and flexibility constraints
con3a_maxprod_conv       Capacity Constraint conventionals

con3i_maxprod_ror        Annual capacity constraint Run-of-river
eq2_capfac_ror_avg       Annual capacity constraint Run-of-river

con3k_maxprod_res        Capacity constraints renewables

* Storage constraints
con4a_stolev_start        Storage Level Dynamics Initial Condition
con4b_stolev              Storage Level Dynamics

con4c_stolev_max          Storage Power Capacity
con4d_maxin_sto           Storage maximum inflow
con4e_maxout_sto          Storage maximum outflow
*con4f_resrv_sto           Constraint on reserves (up)
*con4g_resrv_sto           Constraint on reserves (down)

con4h_maxout_lev          Maximum storage outflow - no more than level of last period
con4i_maxin_lev           Maximum storage inflow - no more than ebergy capacity minus level of last period
con4j_ending              End level equal to initial level
con4k_PHS_EtoP            Maximum E to P ratio for PHS

* Minimum restrictions for renewables and biomass
con5a_spv_share             Gross solar PV share
con5b_wind_on_share         Gross wind onshore share
con5c_wind_off_share        Gross wind offshore share
*con5d_capfacBIO             fix capacity factor of biomass energy
con5_demand
con5e_P2Gshare              Gross power to gas share

* DSM conditions: Load curtailment
*con6a_DSMcurt_duration_max       Maximum curtailment energy budget per time
*con6b_DSMcurt_max                Maximum curtailment per period

* DSM conditions: Load shifting
*con7a_DSMshift_upanddown         Equalization of upshifts and downshifts in due time
*con7b_DSMshift_granular_max      Maximum shifting in either direction per period
*con7c_DSM_distrib_up             Distribution of upshifts between wholesale and reserves
*con7d_DSM_distrib_do             Distribution of downshifts between wholesale and reserves
*con7e_DSMshift_recovery          Recovery times
*con7f_DSMshift_profile           AC profile to give DSM a time-dependant nature
*con7g_DSMshift_profile_maxACpower      Maximum AC power limit
* Maximum installation conditions
con8a_max_I_con                 Maximum installable capacity: Conventionals
con8b_max_I_res                 Maximum installable capacity: Renewables
con8c_max_I_sto_e               Maximum installable energy: Storage in MWh
con8d_max_I_sto_p               Maximum installable capacity: Storage inflow-outflow in MW
*con8e_max_I_dsm_cu              Maximum installable capacity: DSM load curtailment
*con8f_max_I_dsm_shift_pos       Maximum installable capacity: DSM load shifting
$IFTHEN.FC3 %fuel_cost_suppc% == "suppcurve"     
eq4_pref                     Calculate generation-share dependent prefactor for fuel cost supply curve
$ENDIF.FC3
;


*==========

* ---------------------------------------------------------------------------- *
*==========           Objective function *==========
* ---------------------------------------------------------------------------- *

obj..
         Z =E=

$IFTHEN.FC3 %fuel_cost_suppc% == "no_suppcurve"        
                   sum( (ct,h) , c_m(ct)*G_L(ct,h) )
$ENDIF.FC3
$IFTHEN.FC3 %fuel_cost_suppc% == "suppcurve"        
                   sum( (ct,h) , (c_m_nrp(ct) + c_m_FC(ct) * pref_FC(ct)) * G_L(ct,h) )
$ENDIF.FC3
*** ramping cost
$IFTHEN %ramping_cost% == "on"
                 + sum( (ct,h)$(ord(h)>1) , cdata("c_up",ct)*G_UP(ct,h) )
                 + sum( (ct,h) , cdata("c_do",ct)*G_DO(ct,h) )
$ENDIF

%P2G%$ontext
*** P2G ramping cost, likely do not need this, considering electrolyzers (even alkaline one) are very flexible now
$IFTHEN %ramping_cost% == "on"
*                 + sum( (p2g,h)$(ord(h)>1) , p2gdata("p2g_up",p2g) * C_P2GUP(p2g,h) )
*                 + sum( (p2g,h) , p2gdata("p2g_do",p2g)*C_P2GDO(p2g,h) )
$ENDIF
*** P2G var O&M cost
                 + sum( (p2g,h) , p2gdata("c_var_p2g",p2g) * C_P2G(p2g,h) )
$ontext
$offtext
*** curtailment cost and variable O&M cost for non-dispatchable
                 + sum( (res,h) , rdata("c_cu",res) * CU(res,h) + rdata("c_var_res",res) * G_RES(res,h))
                 + sum( (sto,h) , stodata("c_m_sto",sto) * ( STO_OUT(sto,h) + STO_IN(sto,h) ) )
%DSM%$ontext
                 + sum( (dsm_curt,h) , dsmdata_cu("c_m_dsm_cu",dsm_curt)*DSM_CU(dsm_curt,h) )
                 + sum( (dsm_shift,h) , dsmdata_shift("c_m_dsm_shift",dsm_shift) * DSM_UP_DEMAND(dsm_shift,h) )
                 + sum( (dsm_shift,h) , dsmdata_shift("c_m_dsm_shift",dsm_shift) * DSM_DO_DEMAND(dsm_shift,h) )
$ontext
$offtext
*** conventional and renewable capital and fixed O&M cost
                 + sum( ct , c_i(ct)*N_CON(ct) )
                 + sum( ct , cdata("c_fix_con",ct)*N_CON(ct) )

                 + sum( res , c_i_res(res)*P_RES(res) )
                 + sum( res , rdata("c_fix_res",res)*P_RES(res) )
%P2G%$ontext
*** P2G capital and fixed O&M cost
                 + sum( p2g , c_i_p2g(p2g)*N_P2G(p2g) )
                 + sum( p2g , p2gdata("c_fix_p2g",p2g)*N_P2G(p2g) )
$ontext
$offtext
*** grid cost (from REMIND), no var O&M grid cost
                 + sum( grid , c_i_grid(grid)*N_GRID(grid) )
                 + sum( grid , griddata("c_fix_grid",grid)*N_GRID(grid) )
                 
                 + sum( sto , c_i_sto_e(sto)*N_STO_E(sto) )
                 + sum( sto , stodata("c_fix_sto",sto)/2*(N_STO_P(sto)+N_STO_E(sto)) )
                 + sum( sto , c_i_sto_p(sto)*N_STO_P(sto) )
%DSM%$ontext
                 + sum( dsm_curt , c_i_dsm_cu(dsm_curt)*N_DSM_CU(dsm_curt) )
                 + sum( dsm_curt , dsmdata_cu("c_fix_dsm_cu",dsm_curt)*N_DSM_CU(dsm_curt) )
                 + sum( dsm_shift , c_i_dsm_shift(dsm_shift)*N_DSM_SHIFT(dsm_shift) )
                 + sum( dsm_shift , dsmdata_shift("c_fix_dsm_shift",dsm_shift)*N_DSM_SHIFT(dsm_shift) )
$ontext
$offtext
;


* ---------------------------------------------------------------------------- *
*==========           Energy balance and load levels *==========
* ---------------------------------------------------------------------------- *

* Energy balance
con1a_bal(hh)..
         d(hh)
%P2G%$ontext         
         + C_P2G("elh2",hh)
$ontext
$offtext        
         + sum( sto , STO_IN(sto,hh) )
%DSM%$ontext
         + sum( dsm_shift , DSM_UP_DEMAND(dsm_shift,hh) )

$ontext
$offtext
         =E=
         sum( ct , G_L(ct,hh)) + sum( res , G_RES(res,hh)) + sum( sto , STO_OUT(sto,hh) )
%reserves%$ontext
* Balancing Correction Factor
        + sum( ct ,
        - RP_CON('PR_up',ct,hh) * phi_reserves_call('PR_up',hh)
        - RP_CON('SR_up',ct,hh) * phi_reserves_call('SR_up',hh)
        - RP_CON('MR_up',ct,hh) * phi_reserves_call('MR_up',hh)
        + RP_CON('PR_do',ct,hh) * phi_reserves_call('PR_do',hh)
        + RP_CON('SR_do',ct,hh) * phi_reserves_call('SR_do',hh)
        + RP_CON('MR_do',ct,hh) * phi_reserves_call('MR_do',hh) )
$ontext
$offtext
%DSM%$ontext

         + sum(dsm_shift, DSM_DO_DEMAND(dsm_shift,hh))
                    + sum(dsm_curt, DSM_CU(dsm_curt,hh))
$ontext
$offtext
;

%P2G%$ontext
eq1_flexload..
         sum( h , C_P2G("elh2",h))  =E=  totFlexLoad
;
$ontext
$offtext

con2a_loadlevel(ct,h)$(ord(h) > 1)..
        G_L(ct,h) =E= G_L(ct,h-1) + G_UP(ct,h) - G_DO(ct,h)
;

con2b_loadlevelstart(ct,'h1')..
         G_L(ct,'h1') =E= G_UP(ct,'h1')
;

%P2G%$ontext
con2c_flexloadlevel(p2g,h)$(ord(h) > 1)..
        C_P2G(p2g,h) =E= C_P2G(p2g,h-1) + C_P2GUP(p2g,h) - C_P2GDO(p2g,h)
;
con2d_flexloadlevelstart(p2g,'h1')..
         C_P2G(p2g,'h1') =E= C_P2GUP(p2g,'h1')
;
$ontext
$offtext
*
* ---------------------------------------------------------------------------- *
*==========           CONSTRAINING ANNUAL FULL/LOAD HOURS FOR CONVENTIONAL TECHNOLOGIES   *==========
* ---------------------------------------------------------------------------- *
con2c_maxprodannual_conv(ct)$(non_nuc_ct(ct))..
       sum(h, G_L(ct,h) ) =L= 0.8*8760*N_CON(ct)
;

con2c_maxprodannual_conv_nuc("nuc")..
       sum(h, G_L("nuc",h) ) =L= 0.85*8760*N_CON("nuc")
;

%P2G%$ontext

* Constraints for capfac of electrolyzers (at least 30%, in line with current data)
eq2_minprod_elh2('elh2')..
        sum(h, C_P2G('elh2',h)) =G= 0.3 * N_P2G('elh2') *8760
*        sum(h, C_P2G('elh2',h)) =G= 0.3 * N_P2G('elh2')
;

$ontext
$offtext

*** q32_limitCapTeGrid eqn in REMIND
eq3_grid(grid)..
        N_GRID(grid) / remind_gridfac_reg
        =G=
*        P_RES("Solar") * remind_VRECapFac("Solar") + 1.5 * P_RES("Wind_on") * remind_VRECapFac("Wind_on")
*       + 3 * P_RES("Wind_off") * ("Wind_off")
        (sum(h,G_RES("Solar",h)) + 1.5 * sum(h,G_RES("Wind_on",h)))/8760
;

* ---------------------------------------------------------------------------- *
*==========           Hourly maximum generation caps and constraints related to reserves   *==========
* ---------------------------------------------------------------------------- *

con3a_maxprod_conv(ct,h)$(ord(ct)>1 )..
        G_L(ct,h)
        =L= N_CON(ct)
;

*** hourly generation is constrained by theoretical capfac (since we do not have a hydro time series and want to stay realistic)
con3i_maxprod_ror(h)..
        G_L('ror',h)
        =L= 0.4 * N_CON('ror')
;

* annual average capfac on run of river to be constrained by remind theoretical capfac
eq2_capfac_ror_avg("ror")..
        sum(h, G_L("ror",h) ) =L= capfac_ror * 8760 * N_CON("ror")
;

* Constraints on renewables
con3k_maxprod_res(res,h)..
        G_RES(res,h) + CU(res,h)
%reserves%$ontext
        + RP_RES('PR_up',res,h)
        + RP_RES('SR_up',res,h)
        + RP_RES('MR_up',res,h)
$ontext
$offtext
        =E= phi_res(res,h)*P_RES(res)
;

%P2G%$ontext
* Constraints on p2g
eq2_maxprod_p2g(p2g,h)..
        C_P2G(p2g,h)
        =L= N_P2G(p2g)
;
$ontext
$offtext
* ---------------------------------------------------------------------------- *
*==========           Storage constraints *==========
* ---------------------------------------------------------------------------- *

con4a_stolev_start(sto,'h1')..
        STO_L(sto,'h1') =E= stodata("phi_sto_ini",sto) * N_STO_E(sto) + STO_IN(sto,'h1')*(1+stodata("eta_sto",sto))/2 - STO_OUT(sto,'h1')/(1+stodata("eta_sto",sto))*2
;

con4b_stolev(sto,h)$( (ord(h)>1) )..
         STO_L(sto,h) =E= STO_L(sto,h-1) + STO_IN(sto,h)*(1+stodata("eta_sto",sto))/2 - STO_OUT(sto,h)/(1+stodata("eta_sto",sto))*2
%reserves%$ontext
        + sum( (reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6) ,(RP_STO_IN(reserves,sto,h) * phi_reserves_call(reserves,h) )*(1+stodata("eta_sto",sto))/2 )
        - sum( (reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5) ,(RP_STO_IN(reserves,sto,h) * phi_reserves_call(reserves,h) )*(1+stodata("eta_sto",sto))/2 )
        - sum( (reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5) ,(RP_STO_OUT(reserves,sto,h) * phi_reserves_call(reserves,h) )/(1+stodata("eta_sto",sto))*2 )
        + sum( (reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6) ,(RP_STO_OUT(reserves,sto,h) * phi_reserves_call(reserves,h) )/(1+stodata("eta_sto",sto))*2 )
$ontext
$offtext
;

con4c_stolev_max(sto,h)..
        STO_L(sto,h) =L= N_STO_E(sto)
;

con4d_maxin_sto(sto,h)..
        STO_IN(sto,h)
%reserves%$ontext
        + RP_STO_IN('PR_do',sto,h) + RP_STO_IN('SR_do',sto,h) + RP_STO_IN('MR_do',sto,h)
$ontext
$offtext
        =L= N_STO_P(sto)
;

con4e_maxout_sto(sto,h)..
        STO_OUT(sto,h)
%reserves%$ontext
        + RP_STO_OUT('PR_up',sto,h) + RP_STO_OUT('SR_up',sto,h) + RP_STO_OUT('MR_up',sto,h)
$ontext
$offtext
        =L= N_STO_P(sto)
;

con4h_maxout_lev(sto,h)..
        ( STO_OUT(sto,h)
%reserves%$ontext
        + RP_STO_OUT('PR_up',sto,h) + RP_STO_OUT('SR_up',sto,h) + RP_STO_OUT('MR_up',sto,h)
$ontext
$offtext
        ) /(1+stodata("eta_sto",sto))*2
        =L= STO_L(sto,h-1)
;

con4i_maxin_lev(sto,h)..
        ( STO_IN(sto,h)
%reserves%$ontext
        + RP_STO_IN('PR_do',sto,h) + RP_STO_IN('SR_do',sto,h) + RP_STO_IN('MR_do',sto,h)
$ontext
$offtext
        ) * (1+stodata("eta_sto",sto))/2
        =L= N_STO_E(sto) - STO_L(sto,h-1)
;

con4j_ending(sto,h)$( ord(h) = card(h) )..
         STO_L(sto,h) =E= stodata("phi_sto_ini",sto) * N_STO_E(sto)
;

con4k_PHS_EtoP('Sto5')..
        N_STO_E('Sto5') =L= stodata("etop_max",'Sto5') * N_STO_P('Sto5')
;

*** fix biomass plant capfac to 80%
*con5d_capfacBIO..
*sum(h, G_L("bio",h) ) =E= 0.8 * 8760 * N_CON("bio")
*;
* ---------------------------------------------------------------------------- *
*==========           Quotas                         *==========
* ---------------------------------------------------------------------------- *

**power to gas share
*con5e_P2Gshare..
*sum( h , STO_OUT("sto7",h) )
*        =E= p2g_share * sum(h,d(h)) ;
*


* ---------------------------------------------------------------------------- *
*==========           DSM constraints - curtailment *==========
* ---------------------------------------------------------------------------- *

*con6a_DSMcurt_duration_max(dsm_curt,h)..
*         sum( hh$( ord(hh) >= ord(h) AND ord(hh) < ord(h) + dsmdata_cu("t_off_dsm_cu",dsm_curt) ) , DSM_CU(dsm_curt,hh)
*%reserves%$ontext
*         + RP_DSM_CU('SR_up',dsm_curt,hh) * phi_reserves_call('SR_up',hh)
*         + RP_DSM_CU('MR_up',dsm_curt,hh) * phi_reserves_call('MR_up',hh)
*$ontext
*$offtext
*         )
*         =L= N_DSM_CU(dsm_curt) * dsmdata_cu("t_dur_dsm_cu",dsm_curt)
*;
*
*con6b_DSMcurt_max(dsm_curt,h)..
*        DSM_CU(dsm_curt,h)
*%reserves%$ontext
*        + RP_DSM_CU('SR_up',dsm_curt,h)
*        + RP_DSM_CU('MR_up',dsm_curt,h)
*$ontext
*$offtext
*          =L= N_DSM_CU(dsm_curt)
*;
*

* ---------------------------------------------------------------------------- *
*==========           DSM constraints - shifting *==========
* ---------------------------------------------------------------------------- *
*
*con7a_DSMshift_upanddown(dsm_shift,h)..
*    DSM_UP(dsm_shift,h) * dsmdata_shift("eta_dsm_shift",dsm_shift)
*    =E=
*   sum( hh$( ( ord(hh) >= ord(h) ) AND ( ord(hh) <= ( ord(h) + dsmdata_shift("t_dur_dsm_shift",dsm_shift) ) ) ) ,
*    DSM_DO(dsm_shift,h,hh)
*  )
*;
*
*con7b_DSMshift_granular_max(dsm_shift,h)..
*         DSM_UP_DEMAND(dsm_shift,h) + DSM_DO_DEMAND(dsm_shift,h)
*%reserves%$ontext
*         + sum( reserves$(ord(reserves) > 2) , RP_DSM_SHIFT(reserves,dsm_shift,h) )
*$ontext
*$offtext
*         =L= N_DSM_SHIFT(dsm_shift)
*;
*
*con7c_DSM_distrib_up(dsm_shift,h)..
*         DSM_UP(dsm_shift,h) =E= DSM_UP_DEMAND(dsm_shift,h)
*%reserves%$ontext
*         + RP_DSM_SHIFT('SR_do',dsm_shift,h) * phi_reserves_call('SR_do',h)
*         + RP_DSM_SHIFT('MR_do',dsm_shift,h) * phi_reserves_call('MR_do',h)
*$ontext
*$offtext
*;
*
*con7d_DSM_distrib_do(dsm_shift,h)..
*         sum( hh$( ord(hh) >= ord(h) - dsmdata_shift("t_dur_dsm_shift",dsm_shift) AND ord(hh) <= ord(h) + dsmdata_shift("t_dur_dsm_shift",dsm_shift) ) , DSM_DO(dsm_shift,hh,h) )
*                 =E=
*         DSM_DO_DEMAND(dsm_shift,h)
*%reserves%$ontext
*         + RP_DSM_SHIFT('SR_up',dsm_shift,h) * phi_reserves_call('SR_up',h)
*         + RP_DSM_SHIFT('MR_up',dsm_shift,h) * phi_reserves_call('MR_up',h)
*$ontext
*$offtext
*;
*
*con7e_DSMshift_recovery(dsm_shift,h)..
*         sum( hh$( ord(hh) >= ord(h) AND ord(hh) < ord(h) + dsmdata_shift("t_off_dsm_shift",dsm_shift) ) , DSM_UP(dsm_shift,hh))
*         =L= N_DSM_SHIFT(dsm_shift) * dsmdata_shift("t_dur_dsm_shift",dsm_shift)
*;


*con7f_DSMshift_profile(dsm_shift,h)..
*
*        DSM_DO_DEMAND(dsm_shift,h)
*
*           =L=
*
*phi_AC('2013','IND',h)* N_DSM_SHIFT(dsm_shift)
*
*;
*
*
*con7g_DSMshift_profile_maxACpower(dsm_shift,h)..
*
*        DSM_UP_DEMAND(dsm_shift,h)
*
*           =L=
*
*        1.2 * N_DSM_SHIFT(dsm_shift) -  N_DSM_SHIFT(dsm_shift)* phi_AC('2013','IND',h)
*;
*
* ---------------------------------------------------------------------------- *
*==========           Maximum installation constraints *==========
* ---------------------------------------------------------------------------- *

con8a_max_I_con(ct)..
         N_CON(ct) =L= cdata("m_con",ct)
;

con8b_max_I_res(res)..
         P_RES(res) =L= rdata("m_res",res)
;

con8c_max_I_sto_e(sto)..
         N_STO_E(sto) =L= stodata("m_sto_e",sto)
;

con8d_max_I_sto_p(sto)..
         N_STO_P(sto) =L= stodata("m_sto_p",sto)
;

*con8e_max_I_dsm_cu(dsm_curt)..
*         N_DSM_CU(dsm_curt) =L= dsmdata_cu("m_dsm_cu",dsm_curt)
*;
*
*con8f_max_I_dsm_shift_pos(dsm_shift)..
*         N_DSM_SHIFT(dsm_shift) =L= dsmdata_shift("m_dsm_shift",dsm_shift)
*;
*

$IFTHEN.FC3 %fuel_cost_suppc% == "suppcurve" 
***CG: if sum(h, G_L(ct,h)) = generation_DIETER_currentIter is larger than REMIND's last iteration gen share RM_postInv_prodSe_con for "ct" conventional tech,
***then pref_FC > 1, making the FC more expensive in current iteration DIETER, lowering its current share
eq4_pref(ct)..
          pref_FC(ct) =E=  1 + ( sum(h, G_L(ct,h)) / totLoad - sum(DT_RM(ct,ct_remind), RM_postInv_prodSe_con("2020","DEU",ct_remind))/ totLoad)
;
$ENDIF.FC3

********************************************************************************
*==========           MODEL *==========
********************************************************************************

model DIETER /
obj

con1a_bal
%P2G%$ontext
eq1_flexload
eq2_minprod_elh2
eq2_maxprod_p2g
$ontext
$offtext

eq3_grid

con2a_loadlevel
con2b_loadlevelstart

%P2G%$ontext
con2c_flexloadlevel
con2d_flexloadlevelstart 
$ontext
$offtext

con2c_maxprodannual_conv
con2c_maxprodannual_conv_nuc

con3a_maxprod_conv

*** two types of capfac constraint on ror: like dispatchable or like VRE
eq2_capfac_ror_avg
con3i_maxprod_ror

con3k_maxprod_res

con4a_stolev_start
con4b_stolev
con4c_stolev_max
con4d_maxin_sto
con4e_maxout_sto
con4h_maxout_lev
con4i_maxin_lev
con4j_ending
con4k_PHS_EtoP

$IFTHEN.FC3 %fuel_cost_suppc% == "suppcurve" 
eq4_pref
$ENDIF.FC3


*con5d_capfacBIO

*con8a_max_I_con
*con8b_max_I_res
*con8c_max_I_sto_e
*con8d_max_I_sto_p

%DSM%$ontext
*con6a_DSMcurt_duration_max
*con6b_DSMcurt_max
*
*con7a_DSMshift_upanddown
*con7b_DSMshift_granular_max
*con7c_DSM_distrib_up
*con7d_DSM_distrib_do

*con_7e_DSMshift_recovery
*con7f_DSMshift_profile
*con7g_DSMshift_profile_maxACpower

*con8e_max_I_dsm_cu
*con8f_max_I_dsm_shift_pos
$ontext
$offtext

/;

*==========
*==========           Options, fixings, report preparation *==========
*==========

*==========
*==========           SOLVER OPTIONS *==========
*==========

option lp = cplex;
option nlp = conopt;
option threads = 6;

parameter
corr_fac_con
corr_fac_res
corr_fac_sto
corr_fac_dsm_cu
corr_fac_dsm_shift
calc_maxprice
calc_minprice
p32_report4RM
hourly_price
peak_price
peak_short_term_cost
annual_load_weighted_price
annual_load_weighted_price_shaved
residual_demand
p32_reportmk_4RM
report
report_tech
report_tech_hours
report_hours
report_reserves
report_reserves_hours
report_reserves_tech
report_reserves_tech_hours
res_min
;

* Min and max for prices
calc_maxprice = 0 ;
calc_minprice = 1000 ;
res_min       = 0;

* Assign default correction factors
corr_fac_con(ct,h) = 0 ;
corr_fac_res(res,h) = 0 ;
corr_fac_sto(sto,h) = 0 ;
corr_fac_dsm_cu(dsm_curt,h) = 0 ;
corr_fac_dsm_shift(dsm_shift,h) = 0 ;

*suppress print solutions to limit lst size, placed before solve statement
option solprint = off ;

solve DIETER using lp minimizing Z ;

p32_report4RM(yr,reg,ct,'capacity') = N_CON.l(ct);
p32_report4RM(yr,reg,res,'capacity') = P_RES.l(res);

%P2G%$ontext
*multiply with efficiency of el->h2 to get REMIND unit for eletrolyzer (DIETER unit: /el_equivalent, REMIND unit: /H2_equivalent)
p32_report4RM(yr,reg,'elh2','capacity') = N_P2G.l('elh2') * remind_eta2(yr,reg,"elh2");
$ontext
$offtext
*** inflexible residual demand
*excluding VRE and hydro
*residual_demand(h) = d(h) - G_RES.l("Solar",h) - G_RES.l("Wind_On",h) - G_L.l("ror",h);
*only excluding VRE
residual_demand(h) = d(h) - G_RES.l("Solar",h) - G_RES.l("Wind_On",h); 
p32_report4RM(yr,reg,"all_te",'ResPeakDem_relFac') = SMax(h, residual_demand(h))/sum(h,d(h));

p32_report4RM(yr,reg,"all_te",'peakDem') = SMax(h, d(h));

p32_report4RM(yr,reg,ct,'capfac')$( N_CON.l(ct) ne 0 ) = sum( h , G_L.l(ct,h)) / (N_CON.l(ct) * card(h) );
*combine hc and lig together into one CF to pass onto REMIND
p32_report4RM(yr,reg,'coal','capfac')$( N_CON.l("hc") + N_CON.l("lig") ne 0 ) = sum( h , (G_L.l("hc",h) + G_L.l("lig",h))) / ((N_CON.l("hc") + N_CON.l("lig")) * card(h));
p32_report4RM(yr,reg,res,'capfac')$(P_RES.l(res) ne 0 ) = sum( h , G_RES.l(res,h)) / (P_RES.l(res) * card(h));

%P2G%$ontext
p32_report4RM(yr,reg,'elh2','capfac')$(totFlexLoad ne 0 ) = sum( h , C_P2G.l("elh2",h)) / ( N_P2G.l("elh2") * card(h));
$ontext
$offtext


p32_report4RM(yr,reg,res,'usable_generation') = sum( h , G_RES.l(res,h) );
p32_report4RM(yr,reg,ct,'usable_generation') = sum( h , G_L.l(ct,h) );

p32_report4RM(yr,reg,res,'total_generation') = sum( h , G_RES.l(res,h) +CU.l(res,h));
p32_report4RM(yr,reg,ct,'total_generation') = sum( h , G_L.l(ct,h) );
p32_report4RM(yr,reg,'el','total_consumption') = sum( h , d(h) );
%P2G%$ontext
p32_report4RM(yr,reg,'elh2','total_consumption') = sum( h , C_P2G.l("elh2",h) );
$ontext
$offtext

p32_report4RM(yr,reg,res,'gen_share') = sum( h , G_RES.l(res,h))/totLoad *1e2;
p32_report4RM(yr,reg,ct,'gen_share') = sum( h , G_L.l(ct,h))/totLoad *1e2;
p32_report4RM(yr,reg,'coal','gen_share') = sum( h , (G_L.l('hc',h) + G_L.l('lig',h)))/sum(h,d(h)) * 1e2;

%P2G%$ontext
p32_report4RM(yr,reg,p2g,'dem_share') = sum( h, C_P2G.l(p2g,h) ) / totLoad * 1e2;
$ontext
$offtext

p32_report4RM(yr,reg,'el','dem_share') = sum( h, d(h) ) / totLoad * 1e2;

*also export zero values (prevent compression)
p32_report4RM(yr,reg,ct,'capacity')$(not p32_report4RM(yr,reg,ct,'capacity')) = eps;
p32_report4RM(yr,reg,res,'capacity')$(not p32_report4RM(yr,reg,res,'capacity')) = eps;

p32_report4RM(yr,reg,ct,'capfac')$(not p32_report4RM(yr,reg,ct,'capfac')) = eps;
p32_report4RM(yr,reg,'coal','capfac')$(not p32_report4RM(yr,reg,'coal','capfac')) = eps;
p32_report4RM(yr,reg,res,'capfac')$(not p32_report4RM(yr,reg,res,'capfac')) = eps;

%P2G%$ontext
p32_report4RM(yr,reg,'elh2','capfac')$(not p32_report4RM(yr,reg,'elh2','capfac')) = eps;
$ontext
$offtext

p32_report4RM(yr,reg,ct,'usable_generation')$(not p32_report4RM(yr,reg,ct,'usable_generation')) = eps;
p32_report4RM(yr,reg,res,'usable_generation')$(not p32_report4RM(yr,reg,res,'usable_generation')) = eps;

p32_report4RM(yr,reg,ct,'total_generation')$(not p32_report4RM(yr,reg,ct,'total_generation')) = eps;
p32_report4RM(yr,reg,res,'total_generation')$(not p32_report4RM(yr,reg,res,'total_generation')) = eps;

p32_report4RM(yr,reg,ct,'gen_share')$(not p32_report4RM(yr,reg,ct,'gen_share')) = eps;
p32_report4RM(yr,reg,'coal','gen_share')$(not p32_report4RM(yr,reg,'coal','gen_share')) = eps;
p32_report4RM(yr,reg,res,'gen_share')$(not p32_report4RM(yr,reg,res,'gen_share')) = eps;

p32_report4RM(yr,reg,p2g,'dem_share')$(not p32_report4RM(yr,reg,p2g,'dem_share')) = eps;

*ratio of curtailed renewable to usable renewable generation
p32_report4RM(yr,reg,res,'curt_ratio')$(sum(h,G_RES.l(res,h)) ne 0) = sum(h,CU.l(res,h))/ sum(h,G_RES.l(res,h));
*make sure all values are there, even 0 ones, otherwise REMIND will take input values
p32_report4RM(yr,reg,res,'curt_ratio')$(not p32_report4RM(yr,reg,res,'curt_ratio')) = eps;

*** calculate multiplicative factor - markup


******************* with scarcity price shaving ****************************
*** calculate hourly price with scarcity price thrown out, i.e. setting the highest price hour prices to the price of the hour with the highest short term cost
hourly_price(h) = - con1a_bal.m(h);
peak_price = SMax(h, hourly_price(h));

$IFTHEN.PriceShave %price_shave% == "on"
if (peak_price > 5000,
*   peak_short_term_cost is the highest short-term marginal cost
    peak_short_term_cost = SMax( h$(hourly_price(h) < 5000), hourly_price(h) ); 
    hourly_price(h)$(hourly_price(h) > 5000)  = peak_short_term_cost;
);
$ENDIF.PriceShave

*** calculate market value (only in hours where there is no scarcity price)
report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)',ct)$(sum(h, G_L.l(ct,h)) ne 0 ) = sum( h, G_L.l(ct,h)*hourly_price(h))/sum( h , G_L.l(ct,h));
report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)',res)$(sum(h, G_RES.l(res,h)) ne 0 ) = sum( h, G_RES.l(res,h)*hourly_price(h))/sum( h , G_RES.l(res,h) );
report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','coal')$(sum( h, (G_L.l('lig',h) + G_L.l('hc',h)) ) ne 0 )
        = sum( h , (G_L.l('lig',h) + G_L.l('hc',h))*hourly_price(h)) / sum( h , (G_L.l('lig',h) + G_L.l('hc',h)) );

* average price for both flexible and inflexible techs, with scarcity price shaved
annual_load_weighted_price_shaved = sum(h,hourly_price(h)*d(h)) / totLoad;
%P2G%$ontext
annual_load_weighted_price_shaved = sum(h,hourly_price(h)*(d(h)+ sum(p2g,C_P2G.l(p2g,h)))) / totLoad;
$ontext
$offtext

*** if generation is not 0, pass the market value (w/ scarcity price shaved) from DIETER to REMIND, if generation is 0, pass the average annual price to REMIND
p32_reportmk_4RM(yr,reg,ct,'market_value')$(sum(h, G_L.l(ct,h)) ne 0 ) =
    report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)',ct);

p32_reportmk_4RM(yr,reg,ct,'market_value')$(sum(h, G_L.l(ct,h)) eq 0 ) = annual_load_weighted_price_shaved;
    
p32_reportmk_4RM(yr,reg,'coal','market_value')$(sum(h, G_L.l('lig',h)) ne 0 ) =
    report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','coal');

p32_reportmk_4RM(yr,reg,'coal','market_value')$(sum(h, G_L.l('lig',h)) eq 0 ) = annual_load_weighted_price_shaved;
    
p32_reportmk_4RM(yr,reg,res,'market_value')$(sum(h, G_RES.l(res,h)) ne 0 ) = 
    report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)',res);
    
p32_reportmk_4RM(yr,reg,res,'market_value')$(sum(h, G_RES.l(res,h)) eq 0 ) = annual_load_weighted_price_shaved;

******************* without scarcity price shaving ****************************
annual_load_weighted_price = -sum(h,con1a_bal.m(h)*d(h))/totLoad ;
%P2G%$ontext
annual_load_weighted_price = -sum(h,con1a_bal.m(h)*(d(h)+sum(p2g,C_P2G.l(p2g,h))))/totLoad ;
$ontext
$offtext

report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',ct)$(sum(h, G_L.l(ct,h) ne 0 )) = sum( h , G_L.l(ct,h)*(-con1a_bal.m(h)))/sum( h , G_L.l(ct,h));
report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)','coal')$(sum( h, (G_L.l('lig',h) + G_L.l('hc',h)) ) ne 0 )
        = sum( h , (G_L.l('lig',h) + G_L.l('hc',h))*(-con1a_bal.m(h))) / sum( h , (G_L.l('lig',h) + G_L.l('hc',h)) );
report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',res)$(sum(h, G_RES.l(res,h) ne 0 )) = sum( h , G_RES.l(res,h)*(-con1a_bal.m(h)))/sum( h , G_RES.l(res,h) );

p32_reportmk_4RM(yr,reg,'all_te','elec_price') = annual_load_weighted_price_shaved;


%P2G%$ontext
******************** green H2 absolute markup *****************************************
***** annual average electricity price that electrolyzer "sees", calculated using shaved off hourly price
report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','elh2')$(sum( h , C_P2G.l("elh2",h)) ne 0)
                      = sum( h, C_P2G.l("elh2",h) * hourly_price(h))/sum( h , C_P2G.l("elh2",h));
             
* if no generation at all, take annual electricity price as the market value         
report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','elh2')$(sum( h , C_P2G.l("elh2",h)) eq 0)
                      = annual_load_weighted_price_shaved;
* if there is generation but average price seen is 0 because prices in producing hours are 0, take EPS as the market value
                      
p32_reportmk_4RM(yr,reg,"elh2",'market_price') = report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','elh2');
p32_reportmk_4RM(yr,reg,"elh2",'market_price')$(report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','elh2') eq 0 ) = EPS;

*in case of too low market price for elh2, to prevent next REMIND iteration from blowing up, only take 90% of full price
*if ((p32_reportmk_4RM("2020","DEU","elh2","market_price") < 0.2 * annual_load_weighted_price_shaved),
if ((p32_reportmk_4RM("2020","DEU","elh2","market_price") < 1),
    p32_reportmk_4RM(yr,reg,"elh2",'market_price') = 0.25 * annual_load_weighted_price_shaved;
);


******************* green H2 multiplicative markup *****************************************
p32_reportmk_4RM(yr,reg,"elh2",'value_factor') = p32_reportmk_4RM(yr,reg,"elh2",'market_price')/annual_load_weighted_price_shaved;
p32_reportmk_4RM(yr,reg,"elh2",'value_factor')$(sum( h , C_P2G.l("elh2",h)) eq 0) = 1;

*in case of too low market price for elh2, to prevent next REMIND iteration from blowing up, only take 90% of full price
*if ((p32_reportmk_4RM("2020","DEU","elh2","market_price") < 0.2 * annual_load_weighted_price_shaved),
*if ((p32_reportmk_4RM("2020","DEU","elh2","value_factor") < 0.1),
*    p32_reportmk_4RM(yr,reg,"elh2",'value_factor') = 0.1;
*);



$ontext
$offtext
******************** inflexible electricity demand markdown *****************************************
***** annual average electricity price that inflexible demand "sees", calculated using shaved off hourly price
report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','el')$(totFixedLoad ne 0)
                      = sum( h, d(h) * hourly_price(h))/sum( h , d(h));
             
* if no generation at all, take annual electricity price as the market value         
report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','el')$(totFixedLoad eq 0)
                      = annual_load_weighted_price_shaved;
                      
* if there is generation but market value is 0 because prices in producing hours are 0, take EPS as the market value
p32_reportmk_4RM(yr,reg,"el",'market_price') = report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','el');
p32_reportmk_4RM(yr,reg,"el",'market_price')$(report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','el') eq 0 ) = EPS;

******************* inflexible electricity demand markdown value factor *****************************************
**** annual average electricity price that inflexible demand "sees", calculated using shaved off hourly price
                      
* if there is generation but market value is 0 because prices in producing hours are 0, take EPS as the market value
p32_reportmk_4RM(yr,reg,"el",'value_factor') = report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','el')/annual_load_weighted_price_shaved;
p32_reportmk_4RM(yr,reg,"el",'value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','el') eq 0 ) = 1;


$include reporting.gms

execute_unload "results_DIETER_y1", p32_report4RM, p32_reportmk_4RM;

execute_unload "full_DIETER_y1";
execute_unload "report_DIETER_y1", report, report_tech, report_hours, report_tech_hours;

%write_to_excel%$ontext
$include report_to_excel
$ontext
$offtext


* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *

