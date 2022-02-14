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
*P2G is prob superfluous switch
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
*load will load averaged fuel cost over 3 iterations
*fixed will load fuel cost from the last uncoupled iteration of REMIND
*linFit will load the lienar fit of fuel cost time series into DIETER
*$setglobal fuel_cost_iter load
*$setglobal fuel_cost_iter fixed (deprecated)
$setglobal fuel_cost_iter linFit
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
*$setglobal price_shave on
*$setglobal price_shave off

**** capacity bound options (bound to remind's preInvest cap)
* none = no bound
* hardLo = hard lower bound for all tech
* softLo1 = soft lower bound (maximum of 20% peak capacity and remind preInvest cap) for dispatchables, hard lower bound for VRE
* softLo2 = 80% of hard lower bound
* fixed = fix to postInvest cap in REMIND, for speeding up computation. However, this should only be turned on after a few iterations, otherwise REMIND's firm capacities are too low in later years,
*           result in infes in DIETER
*$setglobal cap_bound validation
*$setglobal cap_bound softLo
*$setglobal cap_bound none
*$setglobal cap_bound dispatch

*whether ramping cost for conventional and for electrolyzers are turned on
*$setglobal ramping_cost on
$setglobal ramping_cost off

*whether adjustment cost is included in capital cost
*$setglobal adj_cost on
*$setglobal adj_cost on_select
$setglobal adj_cost off

*consider early retirement for capex or not
*$setglobal capex_er on
*$setglobal capex_er off

*choose to print solution for debug purpose
*$setglobal debug on
$setglobal debug off

*choose to have DIETER follow REMIND in nuclear phase-out
$setglobal nucphaseout on
*$setglobal nucphaseout off

*choose to have DIETER follow REMIND in coal phase-out
$setglobal coalphaseout on
*$setglobal coalphaseout off

*this should be on as long as REMIND's windoff is semi-exogenous (like currently, because it is a share of wind_on)
$setglobal windoff_fix on
*$setglobal windoff_fix off

* to reduce the size of lst file
option limcol    = 0;
option limrow    = 0;



*==========
*==========

Sets
*============== remind sets ==================
yr          year for remind power sector             /2020/
yr_before   previous year from remind                /2015/
*all_yr      for smoothing prices                     /2005,2020,2150/
t           year from remind to be loaded


te_remind
COALte
NonPeakGASte
BIOte
NUCte

* remind technology					                /spv, wind, hydro, elh2, coalchp, gaschp, biochp, ngcc, ngccc, ngt, bioigcc, bioigccc, igcc, igccc, pc, pcc, pco, storspv, storwind, tnrs, fnrs, gridwind/
gas_remind  remind emission gases                    /co2/
pe_remind   remind primary energy                    /pegas,pecoal,pewin,pesol,pebiolc,peur,pehyd/
se_remind   remind secondary energy                  /seel,seh2/
*omf is for fixed O&M cost
char_remind remind character                         /omf, omv, lifetime/
char_remind_dataren "remind character for renewable" /nur,maxprod/
grade 	    remind grade level for technology	    /1*12/
reg         region set                               /DEU/
reg_nuc     region with nuclear phase-out            /DEU/
reg_coal    region with coal phase-out               /DEU/

*============== DIETER sets ==================
year      yearly time data                       /2011, 2012, 2013, 2013_windonsmooth,2019/
te_dieter all dieter techs                       /ror, nuc, coal, CCGT, OCGT_eff, OCGT_ineff, bio, Wind_on, Wind_off, Solar,elh2,vregrid/
all_cdata Data for Conventional Technologies     /eta_con,carbon_content,c_up,c_do,c_fix_con,c_var_con,c_inv_overnight_con,inv_lifetime_con,inv_recovery_con,inv_interest_con,m_con,m_con_e,grad_per_min/
all_rdata Data for Renewable Technologies        /c_cu,c_fix_res,c_var_res,phi_min_res,c_inv_overnight_res,inv_lifetime_res,inv_recovery_res,inv_interest_res,m_res,m_res_e/
all_p2gdata                                      /c_fix_p2g, c_var_p2g, inv_lifetime_p2g,p2g_up,p2g_do/
all_griddata                                     /c_fix_grid, inv_lifetime_grid/
ct(te_dieter)        Conventional Technologies      /ror, nuc, coal, CCGT, OCGT_eff, OCGT_ineff, bio/
non_nuc_ct(ct) Conv. Technologies except nuclear /ror, coal, CCGT, OCGT_eff, OCGT_ineff, bio/
sto       Storage technolgies                    /lith,PbS,flow,PSH,caes/
res(te_dieter)       Renewable technologies         /Wind_on, Wind_off, Solar/
p2g(te_dieter)       Sector Coupling P2G Technologies /elh2/
grid      Transmission grid cost for VRE         /vregrid/
all_dsm_cu Data for DSM curt                     /c_m_dsm_cu,c_fix_dsm_cu,c_inv_overnight_dsm_cu,inv_recovery_dsm_cu,inv_interest_dsm_cu,m_dsm_cu,t_dur_dsm_cu,t_off_dsm_cu/
all_dsm_shift Data for DSM shift                 /c_m_dsm_shift,eta_dsm_shift,c_fix_dsm_shift,c_inv_overnight_dsm_shift,inv_recovery_dsm_shift,inv_interest_dsm_shift,m_dsm_shift,t_dur_dsm_shift,t_off_dsm_shift/
all_storage Data for Storagge                    /c_m_sto,eta_sto,c_fix_sto,c_inv_overnight_sto_e,c_inv_overnight_sto_p,inv_lifetime_sto,inv_interest_sto,m_sto_e,m_sto_p,phi_sto_ini,etop_max/
all_reserve Data for Reserves                    /reserves_intercept,phi_reserves_share/
dsm_shift DSM shifting technologies              /DSM_shift1*DSM_shift5/
dsm_curt  Set of load curtailment technologies   /DSM_curt1*DSM_curt3/
reserves  Set of reserve qualities               /PR_up, PR_do, SR_up, SR_do, MR_up, MR_do/
h         hour                                   /h1*h8760/


*==========
*te(te_dieter) = ct(te_dieter) + res(te_dieter) + p2g(te_dieter);

$include dataload.gms

Sets

DT_RM(te_dieter,te_remind)   "mapping DIETER and REMIND technologies for reporting"
/
coal.pc
coal.pcc
coal.pco
coal.igcc
coal.igccc
CCGT.ngcc
CCGT.ngccc
OCGT_eff.ngt
nuc.tnrs
nuc.fnrs
bio.bioigcc
bio.bioigccc
Solar.spv
Wind_on.wind
Wind_off.windoff
ror.hydro
/

DT_RM_ct(te_dieter,te_remind)   "mapping DIETER and REMIND conventional technologies"
/
coal.pc
coal.pcc
coal.pco
coal.igcc
coal.igccc
CCGT.ngcc
CCGT.ngccc
OCGT_eff.ngt
nuc.tnrs
nuc.fnrs
bio.bioigcc
bio.bioigccc
ror.hydro
/

RM_ct_pe(te_remind,pe_remind)   "mapping DIETER and REMIND conventional technologies and primary energy in remind"
/
pc.pecoal
pcc.pecoal
pco.pecoal
igcc.pecoal
igccc.pecoal
ngcc.pegas
ngccc.pegas
ngt.pegas
tnrs.peur
fnrs.peur
bioigcc.pebiolc
bioigccc.pebiolc
hydro.pehyd
/

DT_ct_pe(ct,pe_remind)   "mapping DIETER and REMIND conventional technologies and primary energy in remind"
/
coal.pecoal
CCGT.pegas
OCGT_eff.pegas
nuc.peur
bio.pebiolc
ror.pehyd
/

;

********** COUPLED SWITCH **************
*** H2 switch for DIETER standalone testing
*remind_h2switch = 0;
*remind_h2switch = 1;

*remind_coupModeSwitch = 0;
*remind_coupModeSwitch = 1;
*remind_coupModeSwitch = 2;


*** wind offshore switch 
* there might be situation where input.gdx has no windoff as technology, in which case, skip first iter
if ((remind_wind_offshore eq 1), 
    if ((remind_iter eq 0),
        remind_wind_offshore = 0;
    );
    
    if ((remind_iter gt 0),
        remind_wind_offshore = 1;
    );
);

dieter_vremarg =0;
*remind_priceShaveSwitch = 1;


*** note: whether CHP coupling is switched on is decided in REMIND, then the sets are exported into DIETER via coupling input gdx RMdata_4DT.gdx
****************************************

Sets
*adjte_remind(te_remind)                              /wind, spv, gridwind,hydro, ngcc, ngccc, bioigcc, bioigccc, igcc, igccc, pc, pcc, pco/
*adjte_dieter(te_dieter)                                 /Wind_on, Solar, vregrid, ror, CCGT, bio, coal/
*
adjte_remind(te_remind)                              /wind, spv, gridwind, ngcc, ngccc/
adjte_dieter(te_dieter)                                 /Wind_on, Solar, vregrid,CCGT/

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
Parameter RM_preInv_prodSe_con(yr, reg, pe_remind, se_remind, te_remind) Pre investment remind prodSe for VRE gen share transfer;
Parameter RM_postInv_cap_con(yr,reg,ct) Post-investment REMIND capacity for conventional;
Parameter RM_postInv_cap_res(yr,reg,res) Post-investment REMIND capacity for renewable;
Parameter RM_postInv_cap_p2g(yr,reg,p2g) Post-investment REMIND capacity for renewable;
Parameter RM_postInv_cap_grid(yr,reg,grid) Post-investment REMIND capacity for renewable;
Parameter RM_postInv_prodSe_con(yr,reg,ct) Post-investment REMIND generation for conventional;
Parameter RM_postInv_prodSe_res_xcurt(yr,reg,res) Post-investment REMIND generation for renewables excluding curtailment;
Parameter RM_postInv_prodSe_res(yr,reg,res) Post-investment REMIND generation for renewables including curtailment;
Parameter RM_postInv_demSe(yr,reg,p2g) Post-investment REMIND demand for P2G;
Parameter RM_curt_rep(yr,reg,res) REMIND curtailment for VRE;
Parameter VRE_grid_ratio(yr,reg,res) grid ratio for reporting;
Parameter dieter_lifetime(ct)  DIETER plant lifetime;

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

if ((remind_h2switch eq 0),
totFlexLoad = 0;
);

if ((remind_h2switch eq 1),
totFlexLoad = remind_totseh2Dem("2020", "DEU", "seh2") * sm_TWa_2_MWh;
);

totFixedLoad = totLoad - totFlexLoad;
d(h) = d_y_reg('2019',"DEU",h) * totFixedLoad / DIETER_OLDtotdem;


****************
*REMIND disinvestments cap: disinvestments = (early_reti(t) - early_reti(t-1) ) * cap(t) / (1 - early_reti(t)), it doesn't go into DIETER, I am only using DIETER for reporting

earlyRetiCap_reporting(yr, reg, te_remind)$(remind_capEarlyReti(yr, reg, te_remind) ne 1) = (remind_capEarlyReti(yr, reg, te_remind) - remind_capEarlyReti2("2015", reg, te_remind) ) * remind_cap(yr, reg, te_remind, "1")
                                                                            / (1 - remind_capEarlyReti(yr, reg, te_remind)) ;

****************
Parameter
remind_VRECapFac(res)   "VRE capacity factors from REMIND"
remind_HydroCapFac      "Hydro capacity factor from REMIND"
dieter_VRECapFac(res)   "VRE capacity factors from time series input to DIETER"
*share_wind_on_CF_match  "Share of required wind onshore power to match DIETER wind CF to REMIND values"
dieter_newInvFactor(te_remind) "an investment CAPEX factor for added cap in DIETER that corresponds to potential of the still empty rlf grades in REMIND - should be equal or larger than 1"
remind_gradeMaxCap(grade,te_remind) "remind maximal capacity for each renewable grade"
remind_highest_empty_grade_LF(te_remind) "load factor of the highest remind grade with free room for new built (less than 90% maximal capacity)"
remind_average_grade_LF(te_remind) "average grade load factor - need to multiply with vm_capFac to get remind theoretical renewable capacity factor"
remind_lowest_grade_LF(te_remind) "load factor of the lowest remind grade"
;

remind_average_grade_LF(te_remind)$(remind_cap("2020", "DEU",te_remind, "1")) = sum(grade, remind_pm_dataren("DEU", "nur", grade, te_remind) * remind_vm_CapDistr("2020", "DEU", te_remind, grade)) / remind_cap("2020", "DEU",te_remind, "1");
*AO* Calculate REMIND VRE CFs from grades
remind_VRECapFac("Wind_on") = remind_CF("2020","DEU","wind") * remind_average_grade_LF("wind");

if ((remind_wind_offshore eq 1),
remind_VRECapFac("Wind_off") = remind_CF("2020","DEU","windoff") * remind_average_grade_LF("windoff");
);

remind_VRECapFac("Solar") = remind_CF("2020","DEU","spv") * remind_average_grade_LF("spv");
remind_HydroCapFac = remind_average_grade_LF("hydro");

*CG*: for VRE, calculate an investment CAPEX factor for added cap in DIETER which is
* dieter_newInvFactor = (average capfac over all remind grades) /
* (capfac of the highest rlf grade that is still empty, > 1e-8, < 0.9x maxcap = maxprod * s_twa2mwh / 8760 / nur * 1e-6)
remind_gradeMaxCap(grade,te_remind)$(remind_pm_dataren("DEU", "nur", grade, te_remind) AND remind_CF("2020","DEU",te_remind)) = remind_pm_dataren("DEU", "maxprod", grade, te_remind) / (remind_pm_dataren("DEU", "nur", grade, te_remind) * remind_CF("2020","DEU",te_remind));
remind_lowest_grade_LF(te_remind) = smin(grade$(remind_pm_dataren("DEU", "nur", grade, te_remind) ne 0), remind_pm_dataren("DEU", "nur", grade, te_remind));
*** if there are still empty grades, take the highest LF of the empty grades; if all grades are full, take the lowest grade load factor
remind_highest_empty_grade_LF("wind") = max(remind_lowest_grade_LF("wind"), SMax(grade$(remind_vm_CapDistr("2020", "DEU", "wind", grade) < (0.9 * remind_gradeMaxCap(grade,"wind"))), remind_pm_dataren("DEU", "nur", grade, "wind")));
remind_highest_empty_grade_LF("windoff") = max(remind_lowest_grade_LF("windoff"), SMax(grade$(remind_vm_CapDistr("2020", "DEU", "windoff", grade) < (0.9 * remind_gradeMaxCap(grade,"windoff"))), remind_pm_dataren("DEU", "nur", grade, "windoff")));
remind_highest_empty_grade_LF("spv") = max(remind_lowest_grade_LF("spv"), SMax(grade$(remind_vm_CapDistr("2020", "DEU", "spv", grade) < (0.9 * remind_gradeMaxCap(grade,"spv"))), remind_pm_dataren("DEU", "nur", grade, "spv")));
remind_highest_empty_grade_LF("hydro") = max(remind_lowest_grade_LF("hydro"), SMax(grade$(remind_vm_CapDistr("2020", "DEU", "hydro", grade) < (0.99 * remind_gradeMaxCap(grade,"hydro"))), remind_pm_dataren("DEU", "nur", grade, "hydro")));
dieter_newInvFactor(te_remind)$(remind_highest_empty_grade_LF(te_remind)) = remind_average_grade_LF(te_remind) / remind_highest_empty_grade_LF(te_remind);
**CG: sometimes hydro grades are both full, in which case set factor to 1
dieter_newInvFactor(te_remind)$(dieter_newInvFactor(te_remind) eq 0) = 1;

if((dieter_vremarg eq 0),
dieter_newInvFactor(te_remind) = 1;
);
*AO* Calculate DIETER VRE CFs as given by the input data
dieter_VRECapFac(res) = sum(h, phi_res_y_reg("2019", "DEU", h, res)) / card(h);

phi_res(res, h) = phi_res_y_reg("2019", "DEU", h, res) * remind_VRECapFac(res) / ( sum(hh, phi_res_y_reg("2019", "DEU", hh, res)) / card(hh));

*disable this to minimize distortion
*phi_res("Wind_on", h)$(phi_res("Wind_on", h) > 1)  = 1;
*phi_res("Solar", h)$(phi_res("Solar", h) > 1)  = 1;
*phi_res("Wind_on", h)$(phi_res("Wind_on", h) < 0)  = 0;
*
*if ((remind_wind_offshore eq 1),
*phi_res("Wind_off", h)$(phi_res("Wind_off", h) > 1)  = 1;
*phi_res("Wind_off", h)$(phi_res("Wind_off", h) < 0)  = 0;
*);
*
*
*AO* For hydro simply set CF to that of REMIND
capfac_ror = remind_HydroCapFac;

****************
*pass on VRE gen share from RM to DT instead of capacities, using the following transformation
*(total_generation X gen.share) / (cap.fac. X 8760) = capacity, where (total_generation X gen.share) = generation
*capacity = VRE_seProd / sum(h, cap.fac.(h))

* the prodSe that pre-investment REMIND sees in time step t: prodSe(t) -  pm_dt(t)/2 * prodSe(t) * (vm_deltacap(t)/vm_cap(t))
RM_preInv_prodSe_con(yr, "DEU", pe_remind, se_remind, te_remind)$(remind_cap(yr, "DEU", te_remind, "1") ne 0 ) = remind_prodSe(yr, "DEU", pe_remind, se_remind, te_remind)
                                                                       - remind_pm_dt(yr) / 2  * remind_prodSe(yr, "DEU", pe_remind, se_remind, te_remind)
                                                                       * remind_deltaCap(yr, "DEU", te_remind, "1")
                                                                       /remind_cap(yr, "DEU", te_remind, "1");

**********************************************************************
*Remind post-investment cap ( TW-> MW )
RM_postInv_cap_con(yr,reg,ct) = sum(DT_RM_ct(ct,te_remind), sum( grade, remind_cap(yr, reg, te_remind, grade))) *1e6;
RM_postInv_cap_res(yr,reg,"Solar") = sum( grade, remind_cap(yr, reg, "spv", grade)) * 1e6;
RM_postInv_cap_res(yr,reg,"Wind_on") = sum( grade, remind_cap(yr, reg, "wind", grade))* 1e6;

if ((remind_wind_offshore eq 1),
RM_postInv_cap_res(yr,reg,"Wind_off") = sum( grade, remind_cap(yr, reg, "windoff", grade))* 1e6;
);
RM_postInv_cap_con(yr,reg,"ror") = sum( grade, remind_cap(yr, reg, "hydro", grade)) * 1e6;
RM_postInv_cap_p2g(yr,reg,"elh2") = sum( grade, remind_cap(yr, reg, "elh2", grade)) * 1e6;
RM_postInv_cap_grid(yr,reg,"vregrid") = sum( grade, remind_cap(yr, reg, "gridwind", grade)) * 1e6;

* Remind post-investment gen (excluding curtailment, only usable seel energy) for reporting ( TWa-> MWh )
RM_postInv_prodSe_con(yr,reg,ct) = sum( DT_RM_ct(ct,te_remind), sum(RM_ct_pe(te_remind,pe_remind), remind_prodSe(yr,reg, pe_remind, "seel", te_remind)) ) * sm_TWa_2_MWh;
RM_postInv_prodSe_res_xcurt(yr,reg,"Solar") = remind_prodSe_Resxcurt(yr, reg, "seel", "spv")* sm_TWa_2_MWh;
RM_postInv_prodSe_res_xcurt(yr,reg,"Wind_on") = remind_prodSe_Resxcurt(yr, reg, "seel", "wind")* sm_TWa_2_MWh ;
if ((remind_wind_offshore eq 1),
RM_postInv_prodSe_res_xcurt(yr,reg,"Wind_off") = remind_prodSe_Resxcurt(yr, reg, "seel", "windoff")* sm_TWa_2_MWh ;
);
RM_postInv_prodSe_res(yr,reg,"Solar") = remind_prodSe(yr, reg, "pesol", "seel", "spv")* sm_TWa_2_MWh;
RM_postInv_prodSe_res(yr,reg,"Wind_on") = remind_prodSe(yr, reg, "pewin", "seel", "wind")* sm_TWa_2_MWh;
if ((remind_wind_offshore eq 1),
RM_postInv_prodSe_res(yr,reg,"Wind_off") = remind_prodSe(yr, reg, "pewin", "seel", "windoff")* sm_TWa_2_MWh;
);
RM_postInv_demSe(yr,reg,"elh2") = totFlexLoad;
RM_curt_rep(yr,reg,"Solar") = remind_curt(yr,reg,"spv")* sm_TWa_2_MWh ;
RM_curt_rep(yr,reg,"Wind_on") = remind_curt(yr,reg,"wind")* sm_TWa_2_MWh ;
if ((remind_wind_offshore eq 1),
RM_curt_rep(yr,reg,"Wind_off") = remind_curt(yr,reg,"windoff")* sm_TWa_2_MWh ;
);

**********************************************************************


**********************************************************************
*********************** VALIDATION MODE ******************************
***   THIS MEANS CAP FROM REMIND IS PASSED AS LOWER BOUNDS ***********
**********************************************************************
*****************
* the cap that pre-investment REMIND sees in time step t: vm_cap(t) - pm_dt(t)/2 * vm_deltaCap(t) * (1-vm_earlyRetire) (preInv_remind_cap can sometimes be negative when cap is small)
preInv_remind_cap(yr, "DEU", te_remind, grade) = max(0, remind_cap(yr, "DEU", te_remind, grade) - remind_pm_dt(yr) / 2 * remind_deltaCap(yr, "DEU", te_remind, grade) * (1 - remind_capEarlyReti(yr, "DEU", te_remind)));
added_remind_cap(yr, "DEU", te_remind, grade) = remind_pm_dt(yr)/2 * remind_deltaCap(yr, "DEU", te_remind, grade) * (1 - remind_capEarlyReti(yr, "DEU", te_remind));

**renewable upper bound is the total limit of potential grade capacity in REMIND:
P_RES.up("Solar") = sum(grade, remind_gradeMaxCap(grade,"spv"))*1e6;
P_RES.up("Wind_on") = sum(grade, remind_gradeMaxCap(grade,"wind"))*1e6;
if ((remind_wind_offshore eq 1),
P_RES.up("Wind_off") = sum(grade, remind_gradeMaxCap(grade,"windoff"))*1e6;
);
N_CON.up("ror") = sum(grade, remind_gradeMaxCap(grade,"hydro"))*1e6;

** if nuclear phase out then no nuclear should be added in DIETER
$IFTHEN %nucphaseout% == "on"
loop(reg,
    N_CON.up("nuc")$(reg_nuc(reg)) = RM_postInv_cap_con("2020", reg, "nuc"); 
    );
$ENDIF

$IFTHEN %coalphaseout% == "on"
loop(reg,
    N_CON.up("coal")$(reg_coal(reg)) = RM_postInv_cap_con("2020", reg, "coal"); 
    );
$ENDIF


** remind_coupModeSwitch=0 corresponds to validation mode, where capacities in DIETER only take lower bound (pre-invest, post-earlyreti) from REMIND
if ((remind_coupModeSwitch eq 0),
*$IFTHEN.CB %cap_bound% == "validation"
P_RES.lo("Solar") = RM_preInv_prodSe_con("2020", "DEU", "pesol", "seel", "spv") * sm_TWa_2_MWh / ( remind_VRECapFac("Solar") * card(h)) * 1;
P_RES.lo("Wind_on") = RM_preInv_prodSe_con("2020", "DEU", "pewin", "seel", "wind") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_on") * card(h)) * 1;
if ((remind_wind_offshore eq 1),
P_RES.lo("Wind_off") = RM_preInv_prodSe_con("2020", "DEU", "pewin", "seel", "windoff") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_off") * card(h)) * 1;
);

N_CON.lo(ct) =  sum(DT_RM_ct(ct,te_remind), 
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade))
                    ) * 1e6;

**CG: somehow the coupling doesn't like this bound on electrolyzers
*if ((remind_h2switch eq 1),
*N_P2G.lo("elh2") = sum(   grade, preInv_remind_cap("2020", "DEU", "elh2", grade)  ) * 1e6;
*);

*** gridwind is the only grid tech in REMIND
N_GRID.lo("vregrid") = sum(   grade, preInv_remind_cap("2020", "DEU", "gridwind", grade)  ) * 1e6;

*$ENDIF.CB
);


$IFTHEN.CB %cap_bound% == "softLo"
P_RES.lo("Solar") = RM_preInv_prodSe_con("2020", "DEU", "pesol", "seel", "spv") * sm_TWa_2_MWh / ( remind_VRECapFac("Solar") * card(h)) * 0.8;
P_RES.lo("Wind_on") = RM_preInv_prodSe_con("2020", "DEU", "pewin", "seel", "wind") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_on") * card(h)) * 0.8;
if ((remind_wind_offshore eq 1),
P_RES.lo("Wind_off") = RM_preInv_prodSe_con("2020", "DEU", "pewin", "seel", "windoff") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_off") * card(h)) * 0.8;
);


N_CON.lo(ct) =  sum(DT_RM_ct(ct,te_remind), 
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade))
                    ) * 1e6 * 0.8;

$ENDIF.CB

**********************************************************************
*********************** END OF VALIDATION MODE ***********************
**********************************************************************

**********************************************************************
*********************** Dispatch MODE ********************************
***   THIS MEANS CAP FROM REMIND IS PASSED AS FIXED BOUNDS ***********
******** (after some degree of convergence is reached) ***************
**********************************************************************
** remind_coupModeSwitch=1 corresponds to dispatch mode, where capacities in DIETER take (post-invest, post-earlyreti) capacities in REMIND
if ((remind_coupModeSwitch eq 1),
*$IFTHEN.CB %cap_bound% == "dispatch"
P_RES.lo("Solar") = RM_preInv_prodSe_con("2020", "DEU", "pesol", "seel", "spv") * sm_TWa_2_MWh / ( remind_VRECapFac("Solar") * card(h)) * 1;
P_RES.lo("Wind_on") = RM_preInv_prodSe_con("2020", "DEU", "pewin", "seel", "wind") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_on") * card(h)) * 1;
if ((remind_wind_offshore eq 1),
P_RES.lo("Wind_off") = RM_preInv_prodSe_con("2020", "DEU", "pewin", "seel", "windoff") * sm_TWa_2_MWh / (remind_VRECapFac("Wind_off") * card(h)) * 1;
);

N_CON.lo(ct) =  sum(DT_RM_ct(ct,te_remind), 
                    sum(   grade, preInv_remind_cap("2020", "DEU", te_remind, grade))
                    ) * 1e6;
                    

if ((remind_iter gt remind_dispatch_iter_vrefix),
P_RES.fx(res) = RM_postInv_cap_res("2020", "DEU",res);
);

if ((remind_iter gt remind_dispatch_iter_fix),

    P_RES.fx(res) = RM_postInv_cap_res("2020", "DEU",res);
    N_CON.fx("ror")= RM_postInv_cap_con("2020", "DEU", "ror") ;
    N_CON.fx("nuc")= RM_postInv_cap_con("2020", "DEU", "nuc") ;
    N_CON.fx("CCGT")= RM_postInv_cap_con("2020", "DEU", "CCGT") ;
*   N_CON.fx("OCGT_eff")= RM_postInv_cap_con("2020", "DEU", "OCGT_eff") ;
    N_CON.fx("bio")= RM_postInv_cap_con("2020", "DEU", "bio") ;
    N_CON.fx("coal") = RM_postInv_cap_con("2020", "DEU", "coal") ;

*N_GRID.fx(grid) = RM_postInv_cap_grid("2020", "DEU", grid);
);
*$ENDIF.CB
);

** switch off certain technologies
if ((remind_wind_offshore eq 0),
P_RES.fx(res)$sameas(res,"Wind_off") = 0; 
);

N_CON.fx("OCGT_ineff") = 0;

if ((remind_wind_offshore eq 1),
$IFTHEN.windoff_fix %windoff_fix% == "on"
P_RES.fx(res)$sameas(res,"Wind_off") = RM_postInv_cap_res("2020", "DEU","Wind_off");
$ENDIF.windoff_fix
);
**********************************************************************
*********************** END OF COUPLED MODE ***********************
**********************************************************************
*
*N_STO_P.fx("PbS") = 0 ;
*N_STO_P.fx("caes") = 0 ;
*N_STO_E.fx("PbS") = 0 ;
*N_STO_E.fx("caes") = 0 ;
*STO_IN.fx("caes",h) = 0 ;
*STO_OUT.fx("caes",h) = 0 ;
*STO_L.fx("caes",h) = 0 ;
*STO_IN.fx("PbS",h) = 0 ;
*STO_OUT.fx("PbS",h) = 0 ;
*STO_L.fx("PbS",h) = 0 ;


N_STO_P.fx(sto) = 0 ;
N_STO_E.fx(sto) = 0 ;
STO_IN.fx(sto,h) = 0 ;
STO_OUT.fx(sto,h) = 0 ;
STO_L.fx(sto,h) = 0 ;

*================================================================
*======================= VARIABLE COST =============================
*================================================================
*================ read in fuel price from remind ================


*1e12 is the conversion btw Trillion$ to $

$IFTHEN.FC  %fuel_cost_iter% == "linFit"
*** NO need for unit conversion
con_fuelprice_reg_remind("2020",ct,reg) = sum(DT_ct_pe(ct,pe_remind), remind_fuelprice("2020",reg,pe_remind)) ;
$ENDIF.FC



$IFTHEN.FC  %fuel_cost_iter% == "load"
*** need unit conversion
con_fuelprice_reg_remind("2020",ct,reg) = sum(DT_ct_pe(ct,pe_remind), remind_fuelprice("2020",reg,pe_remind)) * 1e12 / sm_TWa_2_MWh ;
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
** adding eta1 and eta2 together since sometimes eta from REMIND is stored in one parameter, sometimes the other
cdata("eta_con",ct)$(RM_postInv_prodSe_con("2020", "DEU",ct) ne 0)
    = sum(DT_RM_ct(ct,te_remind), (remind_eta1("2020","DEU", te_remind)+remind_eta2("2020","DEU", te_remind)) * sum(RM_ct_pe(te_remind,pe_remind),remind_prodSe("2020", "DEU", pe_remind, "seel", te_remind)))
     / (RM_postInv_prodSe_con("2020", "DEU",ct)/sm_TWa_2_MWh);

*if there is no generation in REMIND, then just take the average eta value of REMIND techs in one category
cdata("eta_con","coal")$(RM_postInv_prodSe_con("2020", "DEU","coal") eq 0)=sum(COALte(te_remind),(remind_eta1("2020","DEU", te_remind)+remind_eta2("2020","DEU", te_remind)))/card(COALte);
cdata("eta_con","CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") eq 0)=sum(NonPeakGASte(te_remind),(remind_eta1("2020","DEU", te_remind)+remind_eta2("2020","DEU", te_remind) ) )/card(NonPeakGASte);
cdata("eta_con","bio")$(RM_postInv_prodSe_con("2020", "DEU","bio") eq 0)=sum(BIOte(te_remind), (remind_eta1("2020","DEU", te_remind) + remind_eta2("2020","DEU", te_remind)))/card(BIOte);
*not averaging for nuclear since fnrs is small for the most part: though this should be checked
cdata("eta_con","nuc")$(RM_postInv_prodSe_con("2020", "DEU","nuc") eq 0)=remind_eta2("2020","DEU","tnrs");

***** carbon content from REMIND (average over REMIND te since CCS plants have lower carbon content) ***** 
*dieter value (tCO2/MWh) = REMIND value (GtC/TWa) * (sm_c_2_co2 * sm_Gt_2_t) / sm_TWa_2_MWh) = REMIND value * (44/12 * 1e9) / (8760000000) 
cdata("carbon_content","coal")$(RM_postInv_prodSe_con("2020", "DEU","coal") ne 0)
    = sum(COALte(te_remind), remind_carboncontent("pecoal","seel",te_remind,"co2") * remind_prodSe("2020", "DEU", "pecoal", "seel", te_remind))
     / sum(COALte(te_remind), remind_prodSe("2020", "DEU", "pecoal", "seel", te_remind)) * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;
cdata("carbon_content","CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") ne 0)
    = sum(NonPeakGASte(te_remind), remind_carboncontent("pegas","seel",te_remind,"co2") * remind_prodSe("2020", "DEU", "pegas", "seel",te_remind))
     / sum(NonPeakGASte(te_remind), remind_prodSe("2020", "DEU", "pegas", "seel",te_remind)) * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;
cdata("carbon_content","OCGT_eff") = remind_carboncontent("pegas","seel","ngt","co2") * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;

*if there is no generation in REMIND, then just take the average carbon content value of REMIND techs
cdata("carbon_content","coal")$(RM_postInv_prodSe_con("2020", "DEU","coal") eq 0)
    = sum(COALte(te_remind),remind_carboncontent("pecoal","seel", te_remind,"co2"))/card(COALte) * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;
cdata("carbon_content","CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") eq 0)
    = sum(NonPeakGASte(te_remind),remind_carboncontent("pegas","seel",te_remind,"co2"))/card(NonPeakGASte) * sm_c_2_co2 * sm_Gt_2_t / sm_TWa_2_MWh;

*omv's unit in fulldata.gdx is T$(2005)/TWa, multiply by 1e12 to get $(2005)/TWa, divides sm_TWa_2_MWh to get $(2005)/MWh

***** variable O&M from REMIND *****
cdata("c_var_con",ct)$(RM_postInv_prodSe_con("2020", "DEU",ct) ne 0)
    = sum(DT_RM_ct(ct,te_remind), remind_OMcost("DEU","omv",te_remind) * sum(RM_ct_pe(te_remind,pe_remind),remind_prodSe("2020", "DEU", pe_remind, "seel", te_remind)))
     / (RM_postInv_prodSe_con("2020", "DEU",ct)/sm_TWa_2_MWh)
     *1e12/sm_TWa_2_MWh;

*if there is no generation in REMIND, then just take the average omv value over techs in one given category
cdata("c_var_con","coal")$(RM_postInv_prodSe_con("2020", "DEU","coal") eq 0) = sum(COALte(te_remind), remind_OMcost("DEU","omv",te_remind))/card(COALte)*1e12/sm_TWa_2_MWh;
cdata("c_var_con","CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") eq 0) = sum(NonPeakGASte(te_remind), remind_OMcost("DEU","omv",te_remind))/card(NonPeakGASte)*1e12/sm_TWa_2_MWh;
cdata("c_var_con","bio")$(RM_postInv_prodSe_con("2020", "DEU","bio") eq 0) = sum(BIOte(te_remind), remind_OMcost("DEU","omv",te_remind))/card(BIOte)*1e12/sm_TWa_2_MWh;
*not averaging for nuclear since fnrs is small for the most part: though this should be checked
cdata("c_var_con","nuc")$(RM_postInv_prodSe_con("2020", "DEU","nuc") eq 0) = remind_OMcost("DEU","omv","tnrs")*1e12/sm_TWa_2_MWh;

** there is no var OM cost for VRE or VREgrid in REMIND

p2gdata("c_var_p2g","elh2") = remind_OMcost("DEU","omv","elh2")  * 1e12 / sm_TWa_2_MWh;

Display cdata;

***** END of variable O&M from REMIND *****
** note: for hydro/ror c_m_reg is 0
$IFTHEN.FC3 %fuel_cost_suppc% == "no_suppcurve"
***** summing variable cost components
c_m_reg(ct,reg)$(cdata("eta_con",ct)) = con_fuelprice_reg_yr_avg(ct,reg)/cdata("eta_con",ct) + cdata("carbon_content",ct)/cdata("eta_con",ct) * remind_co2("2020",reg)  + cdata("c_var_con",ct) ;
c_m(ct) = c_m_reg(ct,"DEU");
$ENDIF.FC3

** CG: currently, suppcurve turns DIETER from LP to NLP
$IFTHEN.FC3 %fuel_cost_suppc% == "suppcurve" 
** with supply curve response in DIETER: building linear demand/price relation to help with convergence
** CG: non reactive part of the marginal cost
c_m_reg_nrp(ct,reg) = cdata("carbon_content",ct)/cdata("eta_con",ct) * remind_co2("2020",reg)  + cdata("c_var_con",ct) ;
c_m_nrp(ct) = c_m_reg_nrp(ct,"DEU");
c_m_FC(ct) = con_fuelprice_reg_yr_avg(ct,"DEU")/cdata("eta_con",ct);
$ENDIF.FC3

*================================================================
*======================= FIXED COST =============================
*once REMIND starts, read in the interest rate from REMIND
r = remind_r("2020","DEU");

*===================== annuitized investment cost (calculated from full lifetime in REMIND, i.e no early retirement) ==================
*disc.fac = r * (1+r)^lifetime/(-1+(1+r)^lifetime)

dieter_lifetime(ct)$(RM_postInv_prodSe_con("2020", "DEU",ct) ne 0) = sum(DT_RM_ct(ct,te_remind), remind_lifetime("lifetime", te_remind) * sum(RM_ct_pe(te_remind,pe_remind),remind_prodSe("2020", "DEU", pe_remind, "seel", te_remind)))
     / (RM_postInv_prodSe_con("2020", "DEU",ct)/sm_TWa_2_MWh);
     
disc_fac_con(ct)$(dieter_lifetime(ct) ne 0) = r * (1+r) ** dieter_lifetime(ct) / (-1+(1+r) ** dieter_lifetime(ct) ) ;

disc_fac_res("Solar") = r * (1+r) ** remind_lifetime("lifetime", "spv") / (-1+(1+r) ** remind_lifetime("lifetime", "spv")) ;
disc_fac_res("Wind_on") = r * (1+r) ** remind_lifetime("lifetime", "wind") / (-1+(1+r) ** remind_lifetime("lifetime", "wind")) ;

if ((remind_wind_offshore eq 1),
disc_fac_res("Wind_off") = r * (1+r) ** remind_lifetime("lifetime", "windoff") / (-1+(1+r) ** remind_lifetime("lifetime", "windoff")) ;
);

disc_fac_p2g("elh2") = r * (1+r) ** remind_lifetime("lifetime", "elh2") / (-1+(1+r) ** remind_lifetime("lifetime", "elh2")) ;
disc_fac_grid("vregrid") = r * (1+r) ** remind_lifetime("lifetime", "gridwind") / (-1+(1+r) ** remind_lifetime("lifetime", "gridwind")) ;

c_i_sto_e(sto) = stodata("c_inv_overnight_sto_e",sto)*( r * (1+r)**(stodata("inv_lifetime_sto",sto)) )
                / ( (1+r)**(stodata("inv_lifetime_sto",sto))-1 )       ;
c_i_sto_p(sto) = stodata("c_inv_overnight_sto_p",sto)*( r * (1+r)**(stodata("inv_lifetime_sto",sto)) )
                / ( (1+r)**(stodata("inv_lifetime_sto",sto))-1 )       ;

*======= add adjustment cost from REMIND for medium and long term periods ========
*only couple adjustment cost for >=2030 due to earlier years volatility
$IFTHEN.AC %adj_cost% == "on"
remind_CapCost(yr,reg,te_remind) = remind_CapCost(yr,reg,te_remind) + remind_adjcost(yr,reg,te_remind);
$ENDIF.AC

$IFTHEN.AC %adj_cost% == "on_select"
remind_CapCost(yr,reg,te_remind)$(adjte_remind(te_remind)) = remind_CapCost(yr,reg,te_remind) + remind_adjcost(yr,reg,te_remind);
$ENDIF.AC

*======= read in investment cost from remind ========
*overnight investment cost
*# conversion from tr USD_twothousandfive/TW to USD_twentyfifteen/MW
** weighted average of many techs in REMIND
c_i_ovnt(ct)$(RM_postInv_prodSe_con("2020", "DEU",ct) ne 0)
    = sum(DT_RM_ct(ct,te_remind), remind_CapCost("2020","DEU",te_remind) * sum(RM_ct_pe(te_remind,pe_remind),remind_prodSe("2020", "DEU", pe_remind, "seel", te_remind)))
     / (RM_postInv_prodSe_con("2020", "DEU",ct)/sm_TWa_2_MWh)
     * 1e6;
     
c_i_ovnt("ror") = c_i_ovnt("ror") * dieter_newInvFactor("hydro");

*in case no generation in remind, take the average (except nuc, nuc just uses tnrs)
c_i_ovnt("coal")$(RM_postInv_prodSe_con("2020", "DEU","coal") eq 0) = sum(COALte(te_remind), remind_CapCost("2020", "DEU", te_remind))/card(COALte) * 1e6 ;
c_i_ovnt("CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") eq 0)
            = sum(NonPeakGASte(te_remind), remind_CapCost("2020", "DEU", te_remind))/card(NonPeakGASte) * 1e6 ;
c_i_ovnt("bio")$(RM_postInv_prodSe_con("2020", "DEU","bio") eq 0)
            = sum(BIOte(te_remind), remind_CapCost("2020", "DEU", te_remind))/card(BIOte) * 1e6 ;              
c_i_ovnt("nuc")$(RM_postInv_prodSe_con("2020", "DEU","nuc") eq 0)
            = remind_CapCost("2020", "DEU", "tnrs") * 1e6 ;
c_i_ovnt_res("Solar") = remind_CapCost("2020", "DEU", "spv") * dieter_newInvFactor("spv")* 1e6  ;
c_i_ovnt_res("Wind_on") = remind_CapCost("2020", "DEU", "wind") * dieter_newInvFactor("wind")* 1e6 ;

if ((remind_wind_offshore eq 1),
c_i_ovnt_res("Wind_off") = remind_CapCost("2020", "DEU", "windoff") * dieter_newInvFactor("windoff")* 1e6 ;
);


* since capacity of elh2 is in MW H2 unit (not MW_el like in DIETER, we need to multiply the efficiency of electrolyzer to obtain the capex for elh2)
c_i_ovnt_p2g("elh2") = remind_CapCost("2020", "DEU", "elh2") * 1e6  * remind_eta2("2020","DEU","elh2");
c_i_ovnt_grid("vregrid") = remind_CapCost("2020", "DEU", "gridwind") * 1e6 ;

*annuitized investment cost
c_i(ct) = c_i_ovnt(ct) * disc_fac_con(ct);
c_i_res(res) = c_i_ovnt_res(res) * disc_fac_res(res);
c_i_p2g(p2g) = c_i_ovnt_p2g(p2g) * disc_fac_p2g(p2g);
c_i_grid(grid) = c_i_ovnt_grid(grid) * disc_fac_grid(grid);

*======= adjustment cost from remind (for disaggregated reportin)========
*""overnight" adjustment cost
*# *# conversion from tr USD_twothousandfive/TW to USD_twentyfifteen/MW
** weighted average of many techs in REMIND
c_adj_ovnt(ct)$(RM_postInv_prodSe_con("2020", "DEU",ct) ne 0)
    = sum(DT_RM_ct(ct,te_remind), remind_adjcost("2020","DEU",te_remind) * sum(RM_ct_pe(te_remind,pe_remind),remind_prodSe("2020", "DEU", pe_remind, "seel", te_remind)))
     / (RM_postInv_prodSe_con("2020", "DEU",ct)/sm_TWa_2_MWh)
     * 1e6;     

*in case no generation in remind, take the average (except nuc, nuc just uses tnrs)
c_adj_ovnt("coal")$(RM_postInv_prodSe_con("2020", "DEU","coal") eq 0) = sum(COALte(te_remind), remind_adjcost("2020", "DEU", te_remind))/card(COALte) * 1e6 ;
c_adj_ovnt("CCGT")$(RM_postInv_prodSe_con("2020", "DEU","CCGT") eq 0)
            = sum(NonPeakGASte(te_remind), remind_adjcost("2020", "DEU", te_remind))/card(NonPeakGASte) * 1e6 ;
c_adj_ovnt("bio")$(RM_postInv_prodSe_con("2020", "DEU","bio") eq 0)
            = sum(BIOte(te_remind), remind_adjcost("2020", "DEU", te_remind))/card(BIOte) * 1e6 ;              
c_adj_ovnt("nuc")$(RM_postInv_prodSe_con("2020", "DEU","nuc") eq 0)
            = remind_adjcost("2020", "DEU", "tnrs") * 1e6 ;
c_adj_ovnt_res("Solar") = remind_adjcost("2020", "DEU", "spv") * 1e6  ;
c_adj_ovnt_res("Wind_on") = remind_adjcost("2020", "DEU", "wind") * 1e6 ;

if ((remind_wind_offshore eq 1),
c_adj_ovnt_res("Wind_off") = remind_adjcost("2020", "DEU", "windoff") * 1e6 ;
);

* since capacity of elh2 is in MW H2 unit (not MW_el like in DIETER, we need to multiply the efficiency of electrolyzer to obtain the capex for elh2)
c_adj_ovnt_p2g("elh2") = remind_adjcost("2020", "DEU", "elh2") * 1e6  * remind_eta2("2020","DEU","elh2");
c_adj_ovnt_grid("vregrid") = remind_adjcost("2020", "DEU", "gridwind") * 1e6 ;

*annuitized adjustment cost
c_adj(ct) = c_adj_ovnt(ct) * disc_fac_con(ct);
c_adj_res(res) = c_adj_ovnt_res(res) * disc_fac_res(res);
c_adj_p2g(p2g) = c_adj_ovnt_p2g(p2g) * disc_fac_p2g(p2g);
c_adj_grid(grid) = c_adj_ovnt_grid(grid) * disc_fac_grid(grid);

*================================================================
*=======read in fixed OM cost from REMIND ========
*note that omf is the proportion from overnight investment cost, NOT annuitized!!!
cdata("c_fix_con",ct)$(RM_postInv_prodSe_con("2020", "DEU",ct) ne 0)
    = sum(DT_RM_ct(ct,te_remind), remind_OMcost("DEU","omf",te_remind) * sum(RM_ct_pe(te_remind,pe_remind),remind_prodSe("2020", "DEU", pe_remind, "seel", te_remind)))
     / (RM_postInv_prodSe_con("2020", "DEU",ct)/sm_TWa_2_MWh)
     * c_i_ovnt(ct);

rdata("c_fix_res","Solar") = remind_OMcost("DEU","omf","spv") * c_i_ovnt_res("Solar");
rdata("c_fix_res","Wind_on") = remind_OMcost("DEU","omf","wind") * c_i_ovnt_res("Wind_on");
if ((remind_wind_offshore eq 1),
rdata("c_fix_res","Wind_off") = remind_OMcost("DEU","omf","windoff") * c_i_ovnt_res("Wind_off");
);

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
con2c_maxprodannual_conv_nuc Full load hour constraint (for nuclear)

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
*con5e_P2Gshare              Gross power to gas share

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
        ( sum(h,(G_RES("Solar",h) + CU("Solar",h)))
        + 1.5 * sum(h,(G_RES("Wind_on",h) + CU("Wind_on",h)))
        + 3 * sum(h,(G_RES("Wind_off",h) + CU("Wind_off",h))) * remind_wind_offshore
        )/8760

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
        sum(h, G_L("ror",h) ) =E= capfac_ror * 8760 * N_CON("ror")
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

con4k_PHS_EtoP('PSH')..
        N_STO_E('PSH') =L= stodata("etop_max",'PSH') * N_STO_P('PSH')
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
          pref_FC(ct) =E=  1 + ( sum(h, G_L(ct,h)) / totLoad - RM_postInv_prodSe_con("2020","DEU",ct)/ totLoad)
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
annual_load_weighted_price_wscar
annual_load_weighted_price
residual_demand
p32_reportmk_4RM
market_value
market_value_wscar
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

** debug
$IFTHEN.deb %debug% == "on"
option solprint = on;
$ENDIF.deb
*suppress print solutions to limit lst size, placed before solve statement
$IFTHEN.deb %debug% == "off"
option solprint = off ;
$ENDIF.deb

solve DIETER using lp minimizing Z ;

p32_report4RM(yr,reg,ct,'capacity') = N_CON.l(ct);
p32_report4RM(yr,reg,res,'capacity') = P_RES.l(res);

%P2G%$ontext
*multiply with efficiency of el->h2 to get REMIND unit for eletrolyzer (DIETER unit: /el_equivalent, REMIND unit: /H2_equivalent)
p32_report4RM(yr,reg,'elh2','capacity') = N_P2G.l('elh2') * remind_eta2(yr,reg,"elh2");
$ontext
$offtext

*** inflexible residual demand
******excluding VRE and hydro
residual_demand(h) = d(h) - G_RES.l("Solar",h) - G_RES.l("Wind_on",h) - G_L.l("ror",h);
******only excluding VRE
*residual_demand(h) = d(h) - G_RES.l("Solar",h) - G_RES.l("Wind_on",h);
if ((remind_wind_offshore eq 1),
residual_demand(h) = residual_demand(h) - G_RES.l("Wind_Off",h); 
);

p32_report4RM(yr,reg,"all_te",'ResPeakDem_relFac') = SMax(h, residual_demand(h))/sum(h,d(h));

p32_report4RM(yr,reg,"all_te",'peakDem') = SMax(h, d(h));

p32_report4RM(yr,reg,ct,'capfac')$( N_CON.l(ct) ne 0 ) = sum( h , G_L.l(ct,h)) / (N_CON.l(ct) * card(h) );
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

%P2G%$ontext
p32_report4RM(yr,reg,p2g,'dem_share') = sum( h, C_P2G.l(p2g,h) ) / totLoad * 1e2;
$ontext
$offtext

p32_report4RM(yr,reg,'el','dem_share') = sum( h, d(h) ) / totLoad * 1e2;

*also export zero values (prevent compression)
p32_report4RM(yr,reg,ct,'capacity')$(not p32_report4RM(yr,reg,ct,'capacity')) = eps;
p32_report4RM(yr,reg,res,'capacity')$(not p32_report4RM(yr,reg,res,'capacity')) = eps;

p32_report4RM(yr,reg,ct,'capfac')$(not p32_report4RM(yr,reg,ct,'capfac')) = eps;
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
p32_report4RM(yr,reg,res,'gen_share')$(not p32_report4RM(yr,reg,res,'gen_share')) = eps;

p32_report4RM(yr,reg,p2g,'dem_share')$(not p32_report4RM(yr,reg,p2g,'dem_share')) = eps;

*ratio of curtailed renewable to usable renewable generation
p32_report4RM(yr,reg,res,'curt_ratio')$(sum(h,G_RES.l(res,h)) ne 0) = sum(h,CU.l(res,h))/ sum(h,G_RES.l(res,h));
*make sure all values are there, even 0 ones, otherwise REMIND will take input values
p32_report4RM(yr,reg,res,'curt_ratio')$(not p32_report4RM(yr,reg,res,'curt_ratio')) = eps;

p32_report4RM(yr,reg,'el','model_status') = DIETER.modelstat ;

*** calculate multiplicative factor - markup


******************* with scarcity price shaving ****************************
*** calculate hourly price with scarcity price thrown out, i.e. setting the highest price hour prices to the price of the hour with the highest short term cost
hourly_price(h) = - con1a_bal.m(h);
peak_price = SMax(h, hourly_price(h));

*$IFTHEN.PriceShave %price_shave% == "on"
if ((remind_priceShaveSwitch = 1),
    if (peak_price > 5000,
*       peak_short_term_cost is the highest short-term marginal cost
        peak_short_term_cost = SMax( h$(hourly_price(h) < 5000), hourly_price(h) ); 
        hourly_price(h)$(hourly_price(h) > 5000)  = peak_short_term_cost;
    );
*$ENDIF.PriceShave
);
*** calculate market value (only in hours where there is no scarcity price)
market_value(ct)$(sum(h, G_L.l(ct,h)) ne 0 ) = sum( h, G_L.l(ct,h)*hourly_price(h))/sum( h , G_L.l(ct,h));
market_value(res)$(sum(h, G_RES.l(res,h)) ne 0 ) = sum( h, G_RES.l(res,h)*hourly_price(h))/sum( h , G_RES.l(res,h) );

* average price for both flexible and inflexible techs, with scarcity price shaved
annual_load_weighted_price = sum(h,hourly_price(h)*d(h)) / totLoad;
%P2G%$ontext
annual_load_weighted_price= sum(h,hourly_price(h)*(d(h)+ sum(p2g,C_P2G.l(p2g,h)))) / totLoad;
$ontext
$offtext

*****!!! note: all prices in p32_reportmk_4RM are in 2005$ value, to report, one needs to multiply by 1.2

*** if generation is not 0, pass the market value (w/ scarcity price shaved) from DIETER to REMIND, if generation is 0, pass the average annual price to REMIND
p32_reportmk_4RM(yr,reg,ct,'market_value')$(sum(h, G_L.l(ct,h)) ne 0 ) = market_value(ct);

p32_reportmk_4RM(yr,reg,ct,'market_value')$(sum(h, G_L.l(ct,h)) eq 0 ) = annual_load_weighted_price;

p32_reportmk_4RM(yr,reg,res,'market_value')$(sum(h, G_RES.l(res,h)) ne 0 ) = 
    market_value(res);
p32_reportmk_4RM(yr,reg,res,'market_value')$(sum(h, G_RES.l(res,h)) eq 0 ) = annual_load_weighted_price;

******************* without scarcity price shaving ****************************
annual_load_weighted_price_wscar = -sum(h,con1a_bal.m(h)*d(h))/totLoad ;
%P2G%$ontext
annual_load_weighted_price_wscar = -sum(h,con1a_bal.m(h)*(d(h)+sum(p2g,C_P2G.l(p2g,h))))/totLoad ;
$ontext
$offtext

******************* market values ****************************
market_value_wscar(ct)$(sum(h, G_L.l(ct,h) ne 0 )) = sum( h , G_L.l(ct,h)*(-con1a_bal.m(h)))/sum( h , G_L.l(ct,h));
market_value_wscar(res)$(sum(h, G_RES.l(res,h) ne 0 )) = sum( h , G_RES.l(res,h)*(-con1a_bal.m(h)))/sum( h , G_RES.l(res,h) );

*       if there is generation in non-scarcity hour(s), i.e. market value is non-zero, it is equal to the market value /annual electricity price
p32_reportmk_4RM(yr,reg,ct,'value_factor')$(market_value(ct) ne 0) =
    market_value(ct) / annual_load_weighted_price;
    
p32_reportmk_4RM(yr,reg,res,'value_factor')$(market_value(res) ne 0) = 
    market_value(res) / annual_load_weighted_price;
        
*       if there is no generation in non-scarcity hour(s), i.e. market value is zero, the markup is 1 (i.e no tax markup in REMIND) 
p32_reportmk_4RM(yr,reg,ct,'value_factor')$(market_value(ct) = 0) = 1;
p32_reportmk_4RM(yr,reg,res,'value_factor')$(market_value(res) = 0) = 1;

******************** demand price *****************************************

p32_reportmk_4RM(yr,reg,'all_te','elec_price') = annual_load_weighted_price;
p32_reportmk_4RM(yr,reg,'all_te','elec_price_wscar') = annual_load_weighted_price_wscar;

%P2G%$ontext
******************** green H2 absolute markup *****************************************
***** annual average electricity price that electrolyzer "sees", calculated using shaved off hourly price
market_value('elh2')$(sum( h , C_P2G.l("elh2",h)) ne 0)
                      = sum( h, C_P2G.l("elh2",h) * hourly_price(h))/sum( h , C_P2G.l("elh2",h));
             
* if no generation at all, take annual electricity price as the market value         
market_value('elh2')$(sum( h , C_P2G.l("elh2",h)) eq 0)
                      = annual_load_weighted_price;
* if there is generation but average price seen is 0 because prices in producing hours are 0, take EPS as the market value

market_value_wscar('elh2')$(sum( h , C_P2G.l("elh2",h)) ne 0)
                      = sum( h, C_P2G.l("elh2",h) * (-con1a_bal.m(h)))/sum( h , C_P2G.l("elh2",h));
            
p32_reportmk_4RM(yr,reg,"elh2",'market_price') = market_value('elh2');
p32_reportmk_4RM(yr,reg,"elh2",'market_price')$(market_value('elh2') eq 0 ) = EPS;

*in case of too low market price for elh2, to prevent next REMIND iteration from blowing up, only take 90% of full price
*if ((p32_reportmk_4RM("2020","DEU","elh2","market_price") < 0.2 * annual_load_weighted_price),
if ((p32_reportmk_4RM("2020","DEU","elh2","market_price") < 1),
    p32_reportmk_4RM(yr,reg,"elh2",'market_price') = 0.25 * annual_load_weighted_price;
);


******************* green H2 multiplicative markup *****************************************
p32_reportmk_4RM(yr,reg,"elh2",'value_factor') = p32_reportmk_4RM(yr,reg,"elh2",'market_price')/annual_load_weighted_price;
p32_reportmk_4RM(yr,reg,"elh2",'value_factor')$(p32_reportmk_4RM(yr,reg,"elh2",'market_price') eq 0) = 1;

*in case of too low market price for elh2, to prevent next REMIND iteration from blowing up, only take 90% of full price
*if ((p32_reportmk_4RM("2020","DEU","elh2","market_price") < 0.2 * annual_load_weighted_price),
*if ((p32_reportmk_4RM("2020","DEU","elh2","value_factor") < 0.1),
*    p32_reportmk_4RM(yr,reg,"elh2",'value_factor') = 0.1;
*);


$ontext
$offtext
******************** inflexible electricity demand markdown *****************************************
***** annual average electricity price that inflexible demand "sees", calculated using shaved off hourly price
market_value('el')$(totFixedLoad ne 0)
                      = sum( h, d(h) * hourly_price(h))/sum( h , d(h));
             
* if no generation at all, take annual electricity price as the market value         
market_value('el')$(totFixedLoad eq 0)
                      = annual_load_weighted_price;


market_value_wscar('el')$(sum( h , d(h)) ne 0)
                      = sum( h, d(h) * (-con1a_bal.m(h)))/sum( h , d(h));
            
                      
* if there is generation but market value is 0 because prices in producing hours are 0, take EPS as the market value
p32_reportmk_4RM(yr,reg,"el",'market_price') = market_value('el');
p32_reportmk_4RM(yr,reg,"el",'market_price')$(market_value('el') eq 0 ) = EPS;

******************* inflexible electricity demand markdown value factor *****************************************
**** annual average electricity price that inflexible demand "sees", calculated using shaved off hourly price
                      
* if there is generation but market value is 0 because prices in producing hours are 0, take EPS as the market value
p32_reportmk_4RM(yr,reg,"el",'value_factor') = p32_reportmk_4RM(yr,reg,"el",'market_price')/annual_load_weighted_price;
p32_reportmk_4RM(yr,reg,"el",'value_factor')$(p32_reportmk_4RM(yr,reg,"el",'market_price') eq 0 ) = 1;


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

