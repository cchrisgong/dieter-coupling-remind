
*================================================================================================
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.0.2, January 2016.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/dieter.
This version constitutes a minor revision of the model documented in Zerrahn, A., Schill, W.-P. (2015): A greenfield model to evaluate long-run power storage requirements for high shares of renewables. DIW Discussion Paper 1457. http://www.diw.de/documents/publikationen/73/diw_01.c.498475.de/dp1457.pdf
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
*================================================================================================


*======================================================
*===== Defines and uploads parameters      =====
*======================================================

Parameters
*=========== for loading remind output ===========
*------------------------
*Investment interest rate
R_4DT(yr, reg)
remind_r(yr, reg)
*------------------------
*capacity
vm_cap(yr, reg, te_remind, grade) 
remind_cap(yr, reg, te_remind, grade)
*-------------------------
*demand
p32_seelUsableDem(yr, reg, se_remind)
remind_totseelDem(yr, reg, se_remind)
p32_seh2elh2Dem(yr, reg, se_remind)
remind_totseh2Dem(yr, reg, se_remind)
*-------------------------
*fuel price
remind_fuelprice(all_yr,reg,pe_remind)
*for smoothing costs over 2 iterations
p32_fuelprice_avgiter(all_yr,reg,pe_remind)
*REMIND variable for fuel cost
q_balPe(all_yr,reg,pe_remind)
*-------------------------
** REMIND budget and last iteration budget (some averaging is needed as sometimes for some iterations budget goes to zero)
*qm_budget(yr,reg)
*remind_budget(all_yr,reg)
*p32_budget(yr,reg)
*remind_budget_lastiter(all_yr,reg)
*------------------------------------
* REMIND energy generated from all tech (including curtailment)
vm_prodSe(yr, reg, pe_remind, se_remind, te_remind)
remind_prodSe(yr, reg, pe_remind, se_remind, te_remind)
*------------------------------------
* REMIND usable energy generated from VRE (excluding curtailment)
vm_usableSeTe(yr, reg, se_remind, te_remind)
remind_prodSe_Resxcurt(yr, reg, se_remind, te_remind)
*------------------------------------
*fraction of OM cost over investment cost
pm_data(reg,char_remind,te_remind)
remind_OMcost(reg,char_remind,te_remind)
*------------------------------------
*investment cost in REMIND already annualized
vm_costTeCapital(yr, reg, te_remind)
remind_CapCost(yr, reg, te_remind)
*------------------------------------
*plant lifetime from REMIND
fm_dataglob(char_remind,te_remind)
remind_lifetime(char_remind, te_remind)
*------------------------------------
*fuel conversion efficiency, pm_dataeta and pm_eta_conv have etas for different te
pm_dataeta(yr,reg,te_remind)
pm_eta_conv(yr,reg,te_remind)
remind_eta1(yr,reg,te_remind)
remind_eta2(yr,reg,te_remind)
*-------------------------------------
*for the purpose of comparing the decision remind and dieter make, use instead the cap. before remind's investment at the beginning of the year, not the end
remind_pm_ts(yr)
pm_ts(yr)
remind_deltaCap(yr, reg, te_remind, grade)
vm_deltaCap(yr, reg, te_remind, grade)
remind_capEarlyReti(yr, reg, te_remind)
remind_capEarlyReti2(yr_before, reg, te_remind)
vm_capEarlyReti(yr, reg, te_remind)
earlyRetiCap_reporting(yr, reg, te_remind)
*-------------------------------------
*for passing the capacity factor of REMIND for VRE techs
remind_pm_dataren(reg, char_remind_dataren, grade,te_remind)
remind_vm_capDistr(yr, reg, te_remind, grade)
*remind_CF contain scaling that accounts for smaller turbines or panels/lower CFs in earlier years (only for wind and solar in remind)
remind_CF(yr,reg,te_remind) 
pm_cf(yr,reg,te_remind)
vm_capFac(yr,reg,te_remind)
*load grid factor (for large country it is larger, otherwise 1)
p32_grid_factor(reg)
remind_gridfac(reg)
*-------------------------------------
* passing REMIND CO2 price to DIETER, CO2 price in $ per tCO2 
remind_flatco2(yr,reg)
f21_taxCO2eqHist(yr,reg)
*-------------------------------------
sm_TWa_2_MWh Conversion factor between TWa and MWh /8760000000/
*-------------------------------------
remind_carboncontent(pe_remind, se_remind, te_remind, gas_remind)
fm_dataemiglob(pe_remind, se_remind, te_remind, gas_remind)
sm_c_2_co2 Conversion factor between weight of carbon to weight of CO2 /3.6667/
sm_Gt_2_t Conversion factor between gigaton to ton /1e9/
*-------------------------------------
*iteration from REMIND
remind_iter
sm32_tmp
*=========== for scaling dieter demand ===========
dieter_OLDtotdem   Old DIETER total demand
demConvR       Remind to Dieter Demand Conversion Ratio which is the ratio between remind_totdem and dieter total net demand sum_h dem_h
;

******* CG: for 1 DIETER run before REMIND run is initiated, based on input data for REMIND input.gdx (which is usually the result of some previous run)
*** load input.gdx before REMIND starts
$ifThen.firstIter not exist RMdata_4DT.gdx
remind_iter = 0;
$gdxin input.gdx
$load  remind_cap = vm_cap.l
*$load  remind_iter = sm32_iter
*$load  remind_r = p32_R_4DT
$load  remind_totseelDem = p32_seelUsableDem_avg
$load  remind_totseh2Dem = p32_seh2elh2Dem_avg
$load  remind_fuelprice = p32_fuelprice_avgiter
$load  remind_flatco2 = f21_taxCO2eqHist
$load  remind_OMcost = pm_data
$load  remind_CapCost = vm_costTeCapital.l
$load  remind_prodSe = vm_prodSe.l
$load  remind_prodSe_Resxcurt = vm_usableSeTe.l
$load  remind_lifetime = fm_dataglob
$load  remind_eta1 = pm_dataeta
$load  remind_eta2 = pm_eta_conv
$load  remind_gridfac = p32_grid_factor
$load  remind_pm_ts = pm_ts
$load  remind_deltaCap = vm_deltaCap.l
$load  remind_capEarlyReti = vm_capEarlyReti.l
$load  remind_capEarlyReti2 = vm_capEarlyReti.l
$load  remind_carboncontent = fm_dataemiglob
*$load  remind_CF = pm_cf
$load  remind_CF = vm_capFac.l
$load  remind_pm_dataren = pm_dataren
$load  remind_vm_capDistr = vm_capDistr.l
$gdxin
$endIf.firstIter


********
**** during REMIND run, load special data before fulldata.gdx drops for the REMIND iteration
**remember to load sets first
$ifThen.duringRun exist RMdata_4DT.gdx
$gdxin RMdata_4DT.gdx
$load  remind_cap = vm_cap.l
$load  remind_iter = sm32_iter
$load  remind_r = p32_R_4DT
$load  remind_totseelDem = p32_seelUsableDem_avg
$load  remind_totseh2Dem = p32_seh2elh2Dem_avg
*$load  remind_fuelprice = p32_fuelprice_avgiter
$load  remind_flatco2 = f21_taxCO2eqHist
$load  remind_OMcost = pm_data
$load  remind_CapCost = vm_costTeCapital.l
$load  remind_prodSe = vm_prodSe.l
$load  remind_prodSe_Resxcurt = vm_usableSeTe.l
$load  remind_lifetime = fm_dataglob
$load  remind_eta1 = pm_dataeta
$load  remind_eta2 = pm_eta_conv
$load  remind_gridfac = p32_grid_factor
$load  remind_pm_ts = pm_ts
$load  remind_deltaCap = vm_deltaCap.l
$load  remind_capEarlyReti = vm_capEarlyReti.l
$load  remind_capEarlyReti2 = vm_capEarlyReti.l
$load  remind_carboncontent = fm_dataemiglob
*$load  remind_CF = pm_cf
$load  remind_CF = vm_capFac.l
$load  remind_pm_dataren = pm_dataren
$load  remind_vm_capDistr = vm_capDistr.l
$gdxin
$endIf.duringRun

**** options to load fuel price differently
$IFTHEN.FC %fuel_cost_iter% == "fixed"
$Ifthen.duringRun exist fulldata_1.gdx
$gdxin fulldata_1.gdx
$load  remind_fuelprice = q_balPe.m
$gdxin
$endIf.duringRun
$ENDIF.FC

$IFTHEN.FC %fuel_cost_iter% == "smoothed"
$Ifthen.duringRun exist RMdata_4DT.gdx
$gdxin RMdata_4DT.gdx
$load  remind_fuelprice = p32_fuelprice_avgiter
$gdxin
$endIf.duringRun
$ENDIF.FC

Parameters

*====== Conventionals ======

*--- Generation and fixed ---*
*eta_con(ct)              Efficiency of conventional technologies
*carbon_content(ct)       CO2 emissions per fuel unit used
*c_up(ct)                 Load change costs UP in EUR per MW
*c_do(ct)                 Load change costs DOWN in EUR per MW
*c_fix_con(ct)            Annual fixed costs per MW
*c_var_con(ct)            Variable O&M costs per MWh

*--- Investment ---*
*c_inv_overnight_con(ct)  Investment costs: Overnight
*inv_lifetime_con(ct)     Investment costs: technical lifetime
*inv_recovery_con(ct)     Investment costs: Recovery period according to depreciation tables
*inv_interest_con(ct)     Investment costs: Interest rate
*m_con(ct)                Investment: maximum installable capacity per technology
*m_con_e(ct)              Investment: maximum installable energy in TWh per a

*--- Flexibility ---*
*grad_per_min(ct)         Maximum load change per minute relative to installed capacity

*====== Fuel and CO2 costs ======
*""fuel price" means without dividing by efficiency eta
*con_fuelprice(ct)        Fuel price conventionals in Euro per MWth
budget_smoothed(all_yr,reg) average budget over 2 iter calculated from REMIND

*====== Fuel and CO2 costs ======
*""fuel price" means without dividing by efficiency eta
*con_fuelprice(ct)        Fuel price conventionals in Euro per MWth
con_fuelprice_reg_remind(all_yr,ct,reg) Fuel price calculated from REMIND
con_fuelprice_reg_yr_avg(ct,reg) Fuel price can be smoothed over several years
con_fuelprice_reg_remind_reporting(ct,reg) Fuel price from REMIND for reporting
*====== Renewables ======

*--- Generation and fixed costs ---*
c_cu(res)                Hourly Curtailment costs for renewables per MW
c_fix_res(res)           Annual fixed costs per MW
phi_min_res              Upload parameter: Minimum required renewables generation

*--- Investment ---*
c_i_ovnt(ct)             Investment costs: Overnight
c_i_ovnt_res(res)        Investment costs: Overnight
c_i_ovnt_p2g(p2g)        Investment costs: Overnight
c_i_ovnt_grid(grid)      Investment costs: Overnight

c_inv_overnight_res(res) Investment costs: Overnight
inv_lifetime_res(res)    Investment costs: technical lifetime
inv_recovery_res(res)    Investment costs: Recovery period
inv_interest_res(res)    Investment costs: Interest rate
m_res(res)               Investment: maximum installable capacity
m_res_e(res)             Investment: maximum installable energy in TWh per a


*====== Time Data ======

*d_y(year,h)              Demand hour h for cost minimization for different years
*d_y_reg(year,reg,h)      Demand hour h for cost minimization for different years and specific regions
d(h)                      Demand hour h for cost minimization
*price_data(h)            Spot market prices 2012
*phi_res_y(year,res,h)    Renewables availability technology res in hour h for different years
phi_res_y_reg(year,reg,h,res)
phi_res(res,h)            Renewables availability technology res in hour h
*phi_ror(h)               Run-of-river availability in hour h

elasticity_upload         Upload parameter for demand elasticity
elasticity                Demand elasticity
*alpha(h)                 Reservation price hour h for elastic demand
*beta(h)                  Slope on linear demand curve hour h


*====== Storage ======

*--- Generation and fixed costs ---*
*c_m_sto(sto)             Marginal costs of storing in or out
*eta_sto(sto)             Storage efficiency
*eta_sto_in(sto)          Storage loading efficiency
*eta_sto_out(sto)         Storage discharging efficiency
*stodata("phi_sto_ini",sto)         Initial storage level
*etop_max(sto)                      Maximum E to P ratio of storage types
*stodata("c_fix_sto",sto)           Annual fixed costs per MW

*--- Investment ---*
*c_inv_overnight_sto_e(sto)       Investment costs for storage energy in MWh: Overnight
*c_inv_overnight_sto_p(sto)       Investment costs for storage capacity in MW: Overnight
*stodata("inv_lifetime_sto",sto)  Investment costs for storage: technical lifetime
*inv_recovery_sto(sto)            Investment costs for storage: Recovery period
*stodata("inv_interest_sto",sto)  Investment costs for storage: Interest rate
*m_sto_e(sto)                     Investment into storage: maximum installable energy in MWh
*m_sto_p(sto)                     Investment into storage: maximum installable capacity in MW


*====== DSM ======

*--- Generation and fixed costs ---*
*c_m_dsm_cu(dsm_curt)             DSM: hourly costs of load curtailment
*c_m_dsm_shift(dsm_shift)         DSM: costs for load shifting
*c_fix_dsm_cu(dsm_curt)           Annual fixed costs per MW load curtailment capacity
*c_fix_dsm_shift(dsm_shift)       Annual fixed costs per MW load shifting capacity

*--- Flexibility, efficiency, recovery ---*
*t_dur_dsm_cu(dsm_curt)           DSM: Maximum duration load curtailment
*t_off_dsm_cu(dsm_curt)           DSM: Minimum recovery time between two load curtailment instances

*t_dur_dsm_shift(dsm_shift)       DSM: Maximum duration load shifting
*t_off_dsm_shift(dsm_shift)       DSM: Minimum recovery time between two granular load upshift instances
*eta_dsm_shift(dsm_shift)         DSM: Efficiency of load shifting technologies

*--- Investment ---*
*c_inv_overnight_dsm_cu(dsm_curt)         Investment costs for DSM load curtailment: Overnight
*c_inv_overnight_dsm_shift(dsm_shift)     Investment costs for DSM load shifting: Overnight
*inv_recovery_dsm_cu(dsm_curt)            Investment costs for DSM load curtailment: Recovery period
*inv_recovery_dsm_shift(dsm_shift)        Investment costs for DSM load shifting: Recovery period
*inv_interest_dsm_cu(dsm_curt)            Investment costs for DSM load curtailment: Interest rate
*inv_interest_dsm_shift(dsm_shift)        Investment costs for DSM load shifting: Interest rate
*m_dsm_cu(dsm_curt)                       DSM: Maximum installable capacity load curtailment
*m_dsm_shift(dsm_shift)                   DSM: Maximum installable capacity load shifting



*====== Reserves ======
*%reserves%$ontext
phi_reserves_share(reserves)             Shares of SRL and MRL up and down
*reserves_intercept(reserves)
reserves_slope(reserves,res)
phi_reserves_call_y(year,h,reserves)     Hourly share of reserve provision that is actually activated
phi_reserves_call(reserves,h)            Hourly share of reserve provision that is actually activated
phi_reserves_pr                          ??? /0.05/
;
*$ontext
*$offtext

*repress printing out all the input data
$offlisting
*================================================================================================
parameter d_y_reg(year,reg,h)      "Demand hour h for cost minimization for different years and specific regions"
/
$ondelim
$include "Load_DEU_2019.csv"
*$include "Load_USA_2019.csv"
$offdelim
/;

parameter cdata(all_cdata,ct)      "Various Data for Conventional Technologies"
/
$ondelim
$include "Conventionals.csv"
$offdelim
/;

parameter p2gdata(all_p2gdata,p2g)      "Various Data for P2G Technologies"
/
$ondelim
$include "P2G.csv"
$offdelim
/;


parameter rdata(all_rdata,res)      "Various Data for Renewable Technologies"
/
$ondelim
$include "Renewables.csv"
$offdelim
/;

parameter griddata(all_griddata,grid);

*parameter con_fuelprice_reg(ct,reg)      "Fuel price conventionals in Euro per MWth for different regions"
*/
*$ondelim
*$include "FuelPrice.csv"
*$offdelim
*/;

Table t_phi_res_y_reg(year,reg,h,res)      ""
$ondelim
$include "VRE_potential_DEU_2019.csv"
*$include "VRE_potential_USA_renewNinja.csv"
$offdelim
;



phi_res_y_reg(year,reg,h,res) = t_phi_res_y_reg(year,reg,h,res);

*parameter phi_ror(year,reg,h)      "Run-of-river availability in hour h"
*/
*$ondelim
*$include "Time_Data.csv"
*$offdelim
*/;

parameter dsmdata_cu(all_dsm_cu,dsm_curt)      "Various Data for DSM"
/
$ondelim
$include "DSM_curt.csv"
$offdelim
/;

parameter dsmdata_shift(all_dsm_shift,dsm_shift)      "Various Data for DSM"
/
$ondelim
$include "DSM_shift.csv"
$offdelim
/;


*parameter AC_demand(year,reg,h)      "AC Demand hour h for cost minimization for different years and specific regions"
*/
*$ondelim
*$include "AC_demand.csv"
*$offdelim
*/;
*

*parameter phi_AC(year,reg,h)      "AC Demand profile hour h for cost minimization for different years and specific regions"
*/
*$ondelim
*$include "phi_AC.csv"
*$offdelim
*/;


parameter stodata(all_storage,sto)      "Various Data for storage"
/
$ondelim
$include "Storage.csv"
$offdelim
/;

*%reserves%$ontext
*parameter reservedata(all_reserve,reserves)      "Various Data for storage"
*/
*$ondelim
*$include "Reserves.csv"
*$offdelim
*/;
*
*Table t_reserves_slope(reserves,res)      ""
*$ondelim
*$include "Reserves2.csv"
*$offdelim
*;
*reserves_slope(reserves,res) = t_reserves_slope(reserves,res);
*
*
*Table t_phi_reserves_call_y(year,h,reserves)      "Hourly share of reserve provision that is actually activated"
*$ondelim
*$include "Reserves_hourly.csv"
*$offdelim
*;
*
*phi_reserves_call_y(year,h,reserves) = t_phi_reserves_call_y(year,h,reserves);

$onlisting


$onecho >temp.tmp

par=phi_reserves_call_y          rng=Reserves!b49:lya73  rdim=2 cdim=1
$offecho

*$ontext
*$offtext

*%skip_Excel%$call "gdxxrw Data_Input_v1.0.2.xlsx @temp.tmp o=Data_input.gdx";

*$GDXin Data_input.gdx
*$load phi_ror
*$load eta_con carbon_content c_up c_do c_fix_con c_var_con c_inv_overnight_con inv_lifetime_con inv_recovery_con inv_interest_con m_con m_con_e grad_per_min
*$load  con_CO2price
*$load c_cu c_fix_res phi_min_res c_inv_overnight_res inv_lifetime_res inv_recovery_res inv_interest_res m_res m_res_e
*$load c_m_sto eta_sto c_fix_sto c_inv_overnight_sto_e c_inv_overnight_sto_p inv_lifetime_sto inv_interest_sto m_sto_e m_sto_p phi_sto_ini etop_max
*$load c_m_dsm_shift c_fix_dsm_shift c_inv_overnight_dsm_shift inv_recovery_dsm_shift inv_interest_dsm_shift m_dsm_shift t_dur_dsm_shift eta_dsm_shift t_off_dsm_shift
*$load c_fix_dsm_cu c_inv_overnight_dsm_cu inv_recovery_dsm_cu inv_interest_dsm_cu m_dsm_cu t_dur_dsm_cu t_off_dsm_cu
*$load phi_reserves_share reserves_intercept reserves_slope phi_reserves_call_y
;

Parameters
c_m_reg(ct,reg)          Marginal production costs for conventional plants including variable O&M costs
c_m(ct)                  Marginal production costs for current region
c_i(ct)          Annualized investment costs by conventioanl plant per MW
c_i_res(res)     Annualized investment costs by renewable plant per MW
c_i_p2g(p2g)     Annualized investment costs by P2G plant per MW
c_i_grid(grid)     Annualized investment costs for grid per MW

c_i_sto_e(sto)   Annualized investment costs storage energy per MWh
c_i_sto_p(sto)   Annualized investment costs storage capacity per MW

c_i_dsm_cu(dsm_curt)     DSM: Investment costs load curtailment
c_i_dsm_shift(dsm_shift) DSM: Investment costs load shifting
;

rdata("c_cu",res)= 0;
p2gdata("p2g_do","elh2") = 10;
p2gdata("p2g_up","elh2") = 10;

*c_i(ct) = cdata("c_inv_overnight_con",ct)*( cdata("inv_interest_con",ct) * (1+cdata("inv_interest_con",ct))**(cdata("inv_lifetime_con",ct)) )
*                 / ( (1+cdata("inv_interest_con",ct))**(cdata("inv_lifetime_con",ct))-1 )       ;
*                 
*c_i_res(res) = rdata("c_inv_overnight_res",res)*(rdata("inv_interest_res",res) * (1+rdata("inv_interest_res",res))**(rdata("inv_lifetime_res",res)) )
*                 / ( (1+rdata("inv_interest_res",res))**(rdata("inv_lifetime_res",res))-1 )       ;
*
c_i_sto_e(sto) = stodata("c_inv_overnight_sto_e",sto)*( stodata("inv_interest_sto",sto) * (1+stodata("inv_interest_sto",sto))**(stodata("inv_lifetime_sto",sto)) )
                 / ( (1+stodata("inv_interest_sto",sto))**(stodata("inv_lifetime_sto",sto))-1 )       ;
c_i_sto_p(sto) = stodata("c_inv_overnight_sto_p",sto)*( stodata("inv_interest_sto",sto) * (1+stodata("inv_interest_sto",sto))**(stodata("inv_lifetime_sto",sto)) )
                 / ( (1+stodata("inv_interest_sto",sto))**(stodata("inv_lifetime_sto",sto))-1 )       ;

*c_i_dsm_cu(dsm_curt) = dsmdata_cu("c_inv_overnight_dsm_cu",dsm_curt)*( dsmdata_cu("inv_interest_dsm_cu",dsm_curt) * (1+dsmdata_cu("inv_interest_dsm_cu",dsm_curt))**(dsmdata_cu("inv_recovery_dsm_cu",dsm_curt)) )
*                 / ( (1+dsmdata_cu("inv_interest_dsm_cu",dsm_curt))**(dsmdata_cu("inv_recovery_dsm_cu",dsm_curt))-1 )       ;
*c_i_dsm_shift(dsm_shift) = dsmdata_shift("c_inv_overnight_dsm_shift",dsm_shift)*( dsmdata_shift("inv_interest_dsm_shift",dsm_shift) * (1+dsmdata_shift("inv_recovery_dsm_shift",dsm_shift))**(dsmdata_shift("inv_recovery_dsm_shift",dsm_shift)) )
*                 / ( (1+dsmdata_shift("inv_interest_dsm_shift",dsm_shift))**(dsmdata_shift("inv_recovery_dsm_shift",dsm_shift))-1 )       ;
*

%second_hour%$ontext
c_i_sto_e(sto) = c_i_sto_e(sto)*card(h)/8760 * 2 ;
dsmdata_cu("t_dur_dsm_cu",dsm_curt) = dsmdata_cu("t_dur_dsm_cu",dsm_curt) / 2 ;
dsmdata_cu("t_off_dsm_cu",dsm_curt) = dsmdata_cu("t_off_dsm_cu",dsm_curt) / 2 ;
dsmdata_shift("t_dur_dsm_shift",dsm_shift)$(ord(dsm_shift)=2 or ord(dsm_shift)=4 or ord(dsm_shift)=5) = dsmdata_shift("t_dur_dsm_shift",dsm_shift) / 2 ;
dsmdata_shift("t_dur_dsm_shift",dsm_shift)$(ord(dsm_shift)=1 or ord(dsm_shift)=3) = 2 ;
$ontext
$offtext

*cdata("c_fix_con",ct) = cdata("c_fix_con",ct)*card(h)/8760 ;
*rdata("c_fix_res",res) = rdata("c_fix_res",res)*card(h)/8760 ;
*stodata("c_fix_sto",sto) = stodata("c_fix_sto",sto)*card(h)/8760 ;
*dsmdata_cu("c_fix_dsm_cu",dsm_curt) = dsmdata_cu("c_fix_dsm_cu",dsm_curt)*card(h)/8760 ;
*dsmdata_shift("c_fix_dsm_shift",dsm_shift) = dsmdata_shift("c_fix_dsm_shift",dsm_shift)*card(h)/8760 ;
*
*cdata("m_con_e",'bio') = cdata("m_con_e",'bio')*card(h)/8760 ;
*

*parameter phi_mean_reserves_call, phi_mean_reserves_call_y ;
*phi_mean_reserves_call_y(year,reserves) = sum(h, phi_reserves_call_y(year,h,reserves) ) / card(h) + eps ;
