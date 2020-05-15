*==========
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
*==========





*Fluctuating renewable Scenarios within each loop
$include Scenario.gms

*For endogenous VRE shares
*parameter spv_share, wind_share_on, wind_share_off;



* Define net energy demand



*==========
*==========           GLOBAL OPTIONS *==========
*==========

* Set star to skip Excel upload and load data from gdx
$setglobal skip_Excel "*"

* Set star to write Excel output file
$setglobal write_to_excel ""

* Set star to activate options
$setglobal DSM "*"
$setglobal reserves ""

* Set star to select run-of-river options either as a simple exogenous parameter or as an endogenous variable including reserve provision:
* if nothing is selected, ROR capacity will be set to zero
* if parameter option is selected, set appropriate values in fix.gmx
* if variable option is selected, set appropriate bound in data_input excel
$setglobal ror_parameter "*"
$setglobal ror_variable ""

* Set star to determine loops
$setglobal solar "exogenous"
$setglobal wind "exogenous"
$setglobal p2g "endogenous"

* Set star to run test variant with each second hour
$setglobal second_hour ""

*==========

* Definition of strings for report parameters and sanity checks
* (Do not change settings below)
$setglobal res_share ""
$setglobal res_share2 ""
$setglobal em_share ""
$setglobal sec_hour "1"


$IFTHEN.solarEndog "%solar%" == "endogenous"
$setglobal res_share_I ",'0'"
$setglobal res_share2_I "'0',"
$ELSE.solarEndog
$setglobal res_share_I ",share"
$setglobal res_share2_I "share,"
$ENDIF.solarEndog

$IFTHEN.windEndog "%wind%" == "endogenous"
$setglobal res_share_II "%res_share_I%,'0','0'"
$setglobal res_share2_II "%res_share2_I%'0','0'"
$ELSE.windEndog
$setglobal res_share_II "%res_share_I%,share1,share2"
$setglobal res_share2_II "%res_share2_I%share1,share2"
$ENDIF.windEndog

$IFTHEN.p2gEndog "%p2g%" == "endogenous"
$setglobal res_share "%res_share_II%,'0'"
$setglobal res_share2 "%res_share2_II%'0'"
$ELSE.p2gEndog
$setglobal res_share "%res_share_II%,share3"
$setglobal res_share2 "%res_share2_II%share3"
$ENDIF.p2gEndog



%second_hour%$ontext
$setglobal sec_hour "8760/2208"
$ontext
$offtext

$if "%ror_parameter%" == "*" $if "%ror_variable%" == "*" $abort Choose appropriate ROR option! ;

*==========

*==========
*==========           SOLVER OPTIONS *==========
*==========

options
optcr = 0.00
reslim = 10000000
lp = cplex
;

option
dispwidth = 15,
limrow = 0,
limcol = 0,
*solprint = off,res_share_0
sysout = off ,
threads = 2 ;

*==========

Sets
year      yearly time data                       /2011, 2012, 2013, 2013_windonsmooth/
all_cdata Data for Conventional Technologies     /eta_con,carbon_content,c_up,c_do,c_fix_con,c_var_con,c_inv_overnight_con,inv_lifetime_con,inv_recovery_con,inv_interest_con,m_con,m_con_e,grad_per_min/
all_rdata Data for Renewable Technologies        /c_cu,c_fix_res,phi_min_res,c_inv_overnight_res,inv_lifetime_res,inv_recovery_res,inv_interest_res,m_res,m_res_e/
ct        Conventional Technologies              /ror, nuc, lig, hc, CCGT, OCGT_eff, OCGT_ineff, bio/
res       Renewable technologies                 /Wind_on, Wind_off, Solar/
sto       Storage technolgies                    /Sto1*Sto7/
all_dsm_cu Data for DSM curt                     /c_m_dsm_cu,c_fix_dsm_cu,c_inv_overnight_dsm_cu,inv_recovery_dsm_cu,inv_interest_dsm_cu,m_dsm_cu,t_dur_dsm_cu,t_off_dsm_cu/
all_dsm_shift Data for DSM shift                 /c_m_dsm_shift,eta_dsm_shift,c_fix_dsm_shift,c_inv_overnight_dsm_shift,inv_recovery_dsm_shift,inv_interest_dsm_shift,m_dsm_shift,t_dur_dsm_shift,t_off_dsm_shift/
all_storage Data for Storagge                    /c_m_sto,eta_sto,c_fix_sto,c_inv_overnight_sto_e,c_inv_overnight_sto_p,inv_lifetime_sto,inv_interest_sto,m_sto_e,m_sto_p,phi_sto_ini,etop_max/
all_reserve Data for Reserves                    /reserves_intercept,phi_reserves_share/
dsm_shift DSM shifting technologies              /DSM_shift1*DSM_shift5/
dsm_curt  Set of load curtailment technologies   /DSM_curt1*DSM_curt3/
reserves  Set of reserve qualities               /PR_up, PR_do, SR_up, SR_do, MR_up, MR_do/
%second_hour%h  hour                             /h1*h8760/
%second_hour%$ontext
$include second_hour.gms
$ontext
$offtext



Alias (h,hh) ;
alias (res,resres) ;
alias (reserves,reservesreserves) ;

*==========

$include dataload.gms
*$stop

*==========

Variables
Z                Value objective function
;

Positive Variables
G_L(ct,h)        Generation level in hour h in MWh
G_UP(ct,h)       Generation upshift in hour h in MWh
G_DO(ct,h)       Generation downshift in hour h in MWh

G_RES(res,h)     Generation renewables type res in hour h in MWh
CU(res,h)        Renewables curtailment technology res in hour h in MWh

STO_IN(sto,h)    Storage inflow technology sto hour h in MWh
STO_OUT(sto,h)   Storage outflow technology sto hour h in MWh
STO_L(sto,h)     Storage level technology sto hour h in MWh

N_CON(ct)        Conventional technology ct built in MW
N_RES(res)       Renewable technology built in MW
N_STO_E(sto)     Storage technology built - Energy in MWh
N_STO_P(sto)     Storage loading and discharging capacity built - Capacity in MW

DSM_CU(dsm_curt,h)       DSM: Load curtailment hour h in MWh
DSM_UP(dsm_shift,h)      DSM: Load shifting up hour h technology dsm in MWh
DSM_DO(dsm_shift,h,hh)   DSM: Load shifting down in hour hh to account for upshifts in hour h technology dsm in MWh

DSM_UP_DEMAND(dsm_shift,h)   DSM: Load shifting up active for wholesale demand in hour h of technology dsm in MWh
DSM_DO_DEMAND(dsm_shift,h)   DSM: Load shifting down active for wholesale demand in hour h of technology dsm in MWh

N_DSM_CU(dsm_curt)        DSM: Load curtailment capacity in MW
N_DSM_SHIFT(dsm_shift)    DSM: Load shifting capacity in MWh

RP_CON(reserves,ct,h)                     Reserve provision by conventionals in hour h in MW
RP_RES(reserves,res,h)                    Reserve provision by renewables in hour h in MW
RP_STO_IN(reserves,sto,h)                 Reserve provision by storage in in hour h in MW
RP_STO_OUT(reserves,sto,h)                Reserve provision by storage out in hour h in MW
RP_DSM_CU(reserves,dsm_curt,h)            Reserve provision by DSM load curtailment in hour h in MW
RP_DSM_SHIFT(reserves,dsm_shift,h)        Reserve provision by DSM load shifting in hour h in MW

demand
;

*==========


Equations
* Objective
obj                      Objective cost minimization

* Energy balance
con1a_bal                Supply Demand Balance in case of cost minimization

* Load change costs
con2a_loadlevel          Load change costs: Level
con2b_loadlevelstart     Load change costs: Level for first period

*full-load hours
con2c_maxprodannual_conv Full load hour constraint (for coal)

* Capacity contraints and flexibility constraints
con3a_maxprod_conv       Capacity Constraint conventionals
con3b_minprod_conv       Minimum production conventionals if reserves contracted

con3c_flex_PR_up        Flexibility of conventionals - provision PR up
con3d_flex_PR_do        Flexibility of conventionals - provision PR do
con3e_flex_SR_up        Flexibility of conventionals - provision SR up
con3f_flex_SR_do        Flexibility of conventionals - provision SR do
con3g_flex_MR_up        Flexibility of conventionals - provision MR up
con3h_flex_MR_do        Flexibility of conventionals - provision MR do

con3i_maxprod_ror        Capacity constraint Run-of-river
con3j_minprod_ror        Minimum production RoR if reserves contracted

con3k_maxprod_res        Capacity constraints renewables
con3l_minprod_res        Minimum production RES if reserves contracted

* Storage constraints
con4a_stolev_start        Storage Level Dynamics Initial Condition
con4b_stolev              Storage Level Dynamics

con4c_stolev_max          Storage Power Capacity
con4d_maxin_sto           Storage maximum inflow
con4e_maxout_sto          Storage maximum outflow
con4f_resrv_sto           Constraint on reserves (up)
con4g_resrv_sto           Constraint on reserves (down)

con4h_maxout_lev          Maximum storage outflow - no more than level of last period
con4i_maxin_lev           Maximum storage inflow - no more than ebergy capacity minus level of last period
con4j_ending              End level equal to initial level
con4k_PHS_EtoP            Maximum E to P ratio for PHS

* Minimum restrictions for renewables and biomass
con5a_spv_share             Gross solar PV share
con5b_wind_on_share         Gross wind onshore share
con5c_wind_off_share        Gross wind offshore share
con5d_maxBIO             Maximum yearly biomass energy
con5_demand
con5e_P2Gshare              Gross power to gas share



* DSM conditions: Load curtailment
con6a_DSMcurt_duration_max       Maximum curtailment energy budget per time
con6b_DSMcurt_max                Maximum curtailment per period

* DSM conditions: Load shifting
con7a_DSMshift_upanddown         Equalization of upshifts and downshifts in due time
con7b_DSMshift_granular_max      Maximum shifting in either direction per period
con7c_DSM_distrib_up             Distribution of upshifts between wholesale and reserves
con7d_DSM_distrib_do             Distribution of downshifts between wholesale and reserves
con7e_DSMshift_recovery          Recovery times
con7f_DSMshift_profile           AC profile to give DSM a time-dependant nature
con7g_DSMshift_profile_maxACpower      Maximum AC power limit
* Maximum installation conditions
con8a_max_I_con                 Maximum installable capacity: Conventionals
con8b_max_I_res                 Maximum installable capacity: Renewables
con8c_max_I_sto_e               Maximum installable energy: Storage in MWh
con8d_max_I_sto_p               Maximum installable capacity: Storage inflow-outflow in MW
con8e_max_I_dsm_cu              Maximum installable capacity: DSM load curtailment
con8f_max_I_dsm_shift_pos       Maximum installable capacity: DSM load shifting

* Reserves
con10a_reserve_prov             Reserve provision SR and MR
con10b_reserve_prov_PR          Reserve provision PR


;


*==========

* ---------------------------------------------------------------------------- *
*==========           Objective function *==========
* ---------------------------------------------------------------------------- *

obj..
         Z =E=
                 sum( (ct,h) , c_m(ct)*G_L(ct,h) )
                 + sum( (ct,h)$(ord(h)>1) , cdata("c_up",ct)*G_UP(ct,h) )
                 + sum( (ct,h) , cdata("c_do",ct)*G_DO(ct,h) )
                 + sum( (res,h) , rdata("c_cu",res)*CU(res,h) )
                 + sum( (sto,h) , stodata("c_m_sto",sto) * ( STO_OUT(sto,h) + STO_IN(sto,h) ) )
%DSM%$ontext
                 + sum( (dsm_curt,h) , dsmdata_cu("c_m_dsm_cu",dsm_curt)*DSM_CU(dsm_curt,h) )
                 + sum( (dsm_shift,h) , dsmdata_shift("c_m_dsm_shift",dsm_shift) * DSM_UP_DEMAND(dsm_shift,h) )
                 + sum( (dsm_shift,h) , dsmdata_shift("c_m_dsm_shift",dsm_shift) * DSM_DO_DEMAND(dsm_shift,h) )
$ontext
$offtext
                 + sum( ct , c_i(ct)*N_CON(ct) )
                 + sum( ct , cdata("c_fix_con",ct)*N_CON(ct) )

                 + sum( res , c_i_res(res)*N_RES(res) )
                 + sum( res , rdata("c_fix_res",res)*N_RES(res) )

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
%reserves%$ontext
                 - sum( (reserves,sto,h)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5) , RP_STO_IN(reserves,sto,h)* phi_reserves_call(reserves,h) * c_m_sto(sto) )
                 + sum( (reserves,sto,h)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6) , RP_STO_IN(reserves,sto,h)* phi_reserves_call(reserves,h) * c_m_sto(sto) )
                 + sum( (reserves,sto,h)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5) , RP_STO_OUT(reserves,sto,h)* phi_reserves_call(reserves,h) * c_m_sto(sto) )
                 - sum( (reserves,sto,h)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6) , RP_STO_OUT(reserves,sto,h)* phi_reserves_call(reserves,h) * c_m_sto(sto) )
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
                 + sum( (reserves,dsm_curt,h)$(ord(reserves) = 3 or ord(reserves) = 5) , RP_DSM_CU(reserves,dsm_curt,h) * phi_reserves_call(reserves,h) * dsmdata_cu("c_m_dsm_cu",dsm_curt) )
                 + sum( (reserves,dsm_shift,h)$(ord(reserves) > 2) , RP_DSM_SHIFT(reserves,dsm_shift,h) * phi_reserves_call(reserves,h) * dsmdata_shift("c_m_dsm_shift",dsm_shift) )
$ontext
$offtext
;


* ---------------------------------------------------------------------------- *
*==========           Energy balance and load levels *==========
* ---------------------------------------------------------------------------- *

* Energy balance
con1a_bal(hh)..
         d(hh) + sum( sto , STO_IN(sto,hh) )
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

con2a_loadlevel(ct,h)$(ord(h) > 1)..
        G_L(ct,h) =E= G_L(ct,h-1) + G_UP(ct,h) - G_DO(ct,h)
;

con2b_loadlevelstart(ct,'h1')..
         G_L(ct,'h1') =E= G_UP(ct,'h1')
;

* ---------------------------------------------------------------------------- *
*==========           cONSTRAINING ANNUAL FULL/LOAD HOURS FOR CONVENTIONAL TECHNOLOGIES   *==========
* ---------------------------------------------------------------------------- *
con2c_maxprodannual_conv(ct)$(ord(ct)>1 )..
       sum(h, G_L(ct,h) )
=L=  0.85*8760*N_CON(ct)
;


* ---------------------------------------------------------------------------- *
*==========           Hourly maximum generation caps and constraints related to reserves   *==========
* ---------------------------------------------------------------------------- *

con3a_maxprod_conv(ct,h)$(ord(ct)>1 )..
        G_L(ct,h)
%reserves%$ontext
        + RP_CON('PR_up',ct,h)
        + RP_CON('SR_up',ct,h)
        + RP_CON('MR_up',ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h)
$ontext
$offtext
        =L= N_CON(ct)
;

con3b_minprod_conv(ct,h)..
        RP_CON('PR_do',ct,h)
        + RP_CON('SR_do',ct,h)
        + RP_CON('MR_do',ct,h)
        =L= G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h)
;

con3c_flex_PR_up(ct,h)$(ord(ct)>1 )..
        RP_CON('PR_up',ct,h)
        =L= cdata("grad_per_min",ct) * 0.5 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3d_flex_PR_do(ct,h)$(ord(ct)>1 )..
        RP_CON('PR_do',ct,h)
        =L= cdata("grad_per_min",ct) * 0.5 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3e_flex_SR_up(ct,h)$(ord(ct)>1 )..
        RP_CON('SR_up',ct,h)
        =L= cdata("grad_per_min",ct) * 5 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3f_flex_SR_do(ct,h)$(ord(ct)>1 )..
        RP_CON('SR_do',ct,h)
        =L= cdata("grad_per_min",ct) * 5 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3g_flex_MR_up(ct,h)$(ord(ct)>1 )..
        RP_CON('MR_up',ct,h)
        =L= cdata("grad_per_min",ct) * 15 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

con3h_flex_MR_do(ct,h)$(ord(ct)>1 )..
        RP_CON('MR_do',ct,h)
        =L= cdata("grad_per_min",ct) * 15 * ( G_L(ct,h)
* Balancing Correction Factor
        - RP_CON('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do',ct,h) * phi_reserves_call('MR_do',h) )
;

* Constraints on run of river
con3i_maxprod_ror(h)..
        G_L('ror',h)
%reserves%$ontext
        + RP_CON('PR_up','ror',h)
        + RP_CON('SR_up','ror',h)
        + RP_CON('MR_up','ror',h)
* Balancing Correction Factor
        - RP_CON('PR_up','ror',h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up','ror',h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up','ror',h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do','ror',h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do','ror',h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do','ror',h) * phi_reserves_call('MR_do',h)
$ontext
$offtext
        =L= phi_ror("2013","IND",h)*N_CON('ror')
;

con3j_minprod_ror(h)..
        RP_CON('PR_do','ror',h)
        + RP_CON('SR_do','ror',h)
        + RP_CON('MR_do','ror',h)
        =L= G_L('ror',h)
* Balancing Correction Factor
        - RP_CON('PR_up','ror',h) * phi_reserves_call('PR_up',h)
        - RP_CON('SR_up','ror',h) * phi_reserves_call('SR_up',h)
        - RP_CON('MR_up','ror',h) * phi_reserves_call('MR_up',h)
        + RP_CON('PR_do','ror',h) * phi_reserves_call('PR_do',h)
        + RP_CON('SR_do','ror',h) * phi_reserves_call('SR_do',h)
        + RP_CON('MR_do','ror',h) * phi_reserves_call('MR_do',h)
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
        =E= phi_res(res,h)*N_RES(res)
;

con3l_minprod_res(res,h)..
        RP_RES('PR_do',res,h)
        + RP_RES('SR_do',res,h)
        + RP_RES('MR_do',res,h)
        =L= G_RES(res,h)
;


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

con4f_resrv_sto(sto,h)..
        RP_STO_IN('PR_up',sto,h) + RP_STO_IN('SR_up',sto,h) + RP_STO_IN('MR_up',sto,h)
        =L= STO_IN(sto,h)
;

con4g_resrv_sto(sto,h)..
        RP_STO_OUT('PR_do',sto,h) + RP_STO_OUT('SR_do',sto,h) + RP_STO_OUT('MR_do',sto,h)
        =L= STO_OUT(sto,h)
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


* ---------------------------------------------------------------------------- *
*==========           Quotas for renewables and biomass *==========
* ---------------------------------------------------------------------------- *

*sum( h , N_RES("Solar") * phi_res("Solar",h ) )
*       =E= spv_share * net_energy_demand ;
*sum( h , N_RES("Wind_on") * phi_res("Wind_on",h )+N_RES("Wind_off") * phi_res("Wind_off",h ) )
*      =E= wind_share_on * net_energy_demand;




con5a_spv_share..
sum( h , G_RES("Solar",h) + CU("Solar",h) )
        =E= spv_share * sum(h,d(h)) ;

con5b_wind_on_share..
sum( h , (G_RES("Wind_on",h)+CU("Wind_on",h) ))
        =E= wind_share_on * sum(h,d(h));


con5c_wind_off_share..
        sum( h , (G_RES("Wind_off",h)+CU("Wind_off",h) ))
        =E= wind_share_off * sum(h,d(h));


con5d_maxBIO..
         sum( h , G_L('bio',h) ) =L= cdata("m_con_e",'bio')
;


*power to gas share
con5e_P2Gshare..
sum( h , STO_OUT("sto7",h) )
        =E= p2g_share * sum(h,d(h)) ;



* ---------------------------------------------------------------------------- *
*==========           DSM constraints - curtailment *==========
* ---------------------------------------------------------------------------- *

con6a_DSMcurt_duration_max(dsm_curt,h)..
         sum( hh$( ord(hh) >= ord(h) AND ord(hh) < ord(h) + dsmdata_cu("t_off_dsm_cu",dsm_curt) ) , DSM_CU(dsm_curt,hh)
%reserves%$ontext
         + RP_DSM_CU('SR_up',dsm_curt,hh) * phi_reserves_call('SR_up',hh)
         + RP_DSM_CU('MR_up',dsm_curt,hh) * phi_reserves_call('MR_up',hh)
$ontext
$offtext
         )
         =L= N_DSM_CU(dsm_curt) * dsmdata_cu("t_dur_dsm_cu",dsm_curt)
;

con6b_DSMcurt_max(dsm_curt,h)..
        DSM_CU(dsm_curt,h)
%reserves%$ontext
        + RP_DSM_CU('SR_up',dsm_curt,h)
        + RP_DSM_CU('MR_up',dsm_curt,h)
$ontext
$offtext
          =L= N_DSM_CU(dsm_curt)
;


* ---------------------------------------------------------------------------- *
*==========           DSM constraints - shifting *==========
* ---------------------------------------------------------------------------- *

con7a_DSMshift_upanddown(dsm_shift,h)..
    DSM_UP(dsm_shift,h) * dsmdata_shift("eta_dsm_shift",dsm_shift)
    =E=
   sum( hh$( ( ord(hh) >= ord(h) ) AND ( ord(hh) <= ( ord(h) + dsmdata_shift("t_dur_dsm_shift",dsm_shift) ) ) ) ,
    DSM_DO(dsm_shift,h,hh)
  )
;

con7b_DSMshift_granular_max(dsm_shift,h)..
         DSM_UP_DEMAND(dsm_shift,h) + DSM_DO_DEMAND(dsm_shift,h)
%reserves%$ontext
         + sum( reserves$(ord(reserves) > 2) , RP_DSM_SHIFT(reserves,dsm_shift,h) )
$ontext
$offtext
         =L= N_DSM_SHIFT(dsm_shift)
;

con7c_DSM_distrib_up(dsm_shift,h)..
         DSM_UP(dsm_shift,h) =E= DSM_UP_DEMAND(dsm_shift,h)
%reserves%$ontext
         + RP_DSM_SHIFT('SR_do',dsm_shift,h) * phi_reserves_call('SR_do',h)
         + RP_DSM_SHIFT('MR_do',dsm_shift,h) * phi_reserves_call('MR_do',h)
$ontext
$offtext
;

con7d_DSM_distrib_do(dsm_shift,h)..
         sum( hh$( ord(hh) >= ord(h) - dsmdata_shift("t_dur_dsm_shift",dsm_shift) AND ord(hh) <= ord(h) + dsmdata_shift("t_dur_dsm_shift",dsm_shift) ) , DSM_DO(dsm_shift,hh,h) )
                 =E=
         DSM_DO_DEMAND(dsm_shift,h)
%reserves%$ontext
         + RP_DSM_SHIFT('SR_up',dsm_shift,h) * phi_reserves_call('SR_up',h)
         + RP_DSM_SHIFT('MR_up',dsm_shift,h) * phi_reserves_call('MR_up',h)
$ontext
$offtext
;

con7e_DSMshift_recovery(dsm_shift,h)..
         sum( hh$( ord(hh) >= ord(h) AND ord(hh) < ord(h) + dsmdata_shift("t_off_dsm_shift",dsm_shift) ) , DSM_UP(dsm_shift,hh))
         =L= N_DSM_SHIFT(dsm_shift) * dsmdata_shift("t_dur_dsm_shift",dsm_shift)
;
con7f_DSMshift_profile(dsm_shift,h)..

        DSM_DO_DEMAND(dsm_shift,h)

           =L=

phi_AC('2013','IND',h)* N_DSM_SHIFT(dsm_shift)

;


con7g_DSMshift_profile_maxACpower(dsm_shift,h)..

        DSM_UP_DEMAND(dsm_shift,h)

           =L=

        1.2 * N_DSM_SHIFT(dsm_shift) -  N_DSM_SHIFT(dsm_shift)* phi_AC('2013','IND',h)
;

* ---------------------------------------------------------------------------- *
*==========           Maximum installation constraints *==========
* ---------------------------------------------------------------------------- *

con8a_max_I_con(ct)..
         N_CON(ct) =L= cdata("m_con",ct)
;

con8b_max_I_res(res)..
         N_RES(res) =L= rdata("m_res",res)
;

con8c_max_I_sto_e(sto)..
         N_STO_E(sto) =L= stodata("m_sto_e",sto)
;

con8d_max_I_sto_p(sto)..
         N_STO_P(sto) =L= stodata("m_sto_p",sto)
;

con8e_max_I_dsm_cu(dsm_curt)..
         N_DSM_CU(dsm_curt) =L= dsmdata_cu("m_dsm_cu",dsm_curt)
;

con8f_max_I_dsm_shift_pos(dsm_shift)..
         N_DSM_SHIFT(dsm_shift) =L= dsmdata_shift("m_dsm_shift",dsm_shift)
;

* ---------------------------------------------------------------------------- *
*==========          Reserve constraints *==========
* ---------------------------------------------------------------------------- *

con10a_reserve_prov(reserves,h)$( ord(reserves) > 2)..
        sum(ct, RP_CON(reserves,ct,h))
        + sum(res, RP_RES(reserves,res,h))
        + sum(sto, RP_STO_IN(reserves,sto,h) + RP_STO_OUT(reserves,sto,h))
%DSM%$ontext
        + sum(dsm_curt, RP_DSM_CU(reserves,dsm_curt,h))$( ord(reserves) = 3 OR ord(reserves) = 5 )
        + sum(dsm_shift , RP_DSM_SHIFT(reserves,dsm_shift,h) )
$ontext
$offtext
        =E= (
            1000 * phi_reserves_share(reserves) * (
            reservedata("reserves_intercept",reserves) + sum( res , reserves_slope(reserves,res) * N_RES(res)/1000 ) ) )$(ord(h) > 1)
;

con10b_reserve_prov_PR(reserves,h)$( ord(reserves) < 3)..
        sum(ct, RP_CON(reserves,ct,h))
        + sum(res, RP_RES(reserves,res,h))
        + sum(sto, RP_STO_IN(reserves,sto,h) + RP_STO_OUT(reserves,sto,h) )
         =E= phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2), 1000 * phi_reserves_share(reservesreserves) * (
            reservedata("reserves_intercept",reservesreserves) + sum( res , reserves_slope(reservesreserves,res) * N_RES(res)/1000 ) ) )$(ord(h) > 1)
;


********************************************************************************
*==========           MODEL *==========
********************************************************************************

model DIETER /
obj

con1a_bal
con2a_loadlevel
con2b_loadlevelstart

con2c_maxprodannual_conv

con3a_maxprod_conv
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

$IFTHEN.solarEndog NOT "%solar%" == "endogenous"
con5a_spv_share
$ENDIF.solarEndog

$IFTHEN.windEndog NOT "%wind%" == "endogenous"
con5b_wind_on_share
con5c_wind_off_share
$ENDIF.windEndog

$IFTHEN.p2gEndog NOT "%p2g%" == "endogenous"
con5e_P2Gshare
$ENDIF.p2gEndog

con5d_maxBIO

con8a_max_I_con
con8b_max_I_res
con8c_max_I_sto_e
con8d_max_I_sto_p

%DSM%$ontext
con6a_DSMcurt_duration_max
con6b_DSMcurt_max

con7a_DSMshift_upanddown
con7b_DSMshift_granular_max
con7c_DSM_distrib_up
con7d_DSM_distrib_do
* con_7e_DSMshift_recovery
con7f_DSMshift_profile
con7g_DSMshift_profile_maxACpower

con8e_max_I_dsm_cu
con8f_max_I_dsm_shift_pos
$ontext
$offtext

%reserves%$ontext
con3b_minprod_conv
con3c_flex_PR_up
con3d_flex_PR_do
con3e_flex_SR_up
con3f_flex_SR_do
con3g_flex_MR_up
con3h_flex_MR_do
con3j_minprod_ror
con3l_minprod_res

con4f_resrv_sto
con4g_resrv_sto

con10a_reserve_prov
con10b_reserve_prov_PR
$ontext
$offtext
/;

*==========
*==========           Options, fixings, report preparation *==========
*==========

* Solver options
$onecho > cplex.opt
lpmethod 4
threads 2
parallelmode -1
$offecho

dieter.OptFile = 1;
dieter.holdFixed = 1 ;

* Fixings
$include fix.gms

* Parameters for the report file
parameter
corr_fac_con
corr_fac_res
corr_fac_sto
corr_fac_dsm_cu
corr_fac_dsm_shift
gross_energy_demand
calc_maxprice
calc_minprice
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

* Parameters for default base year
*d(h) = d_y('2013',h) ;
*phi_res(res,h) = phi_res_y('2013',res,h) ;
phi_reserves_call(reserves,h) = phi_reserves_call_y('2013',h,reserves) ;
phi_mean_reserves_call(reserves) = phi_mean_reserves_call_y('2013',reserves) ;



*==========
*==========           Solve *==========
*==========

* Default 100% RES if no loop over RES shares
*%renewable_share%phi_min_res= 1 ;
*%renewable_share%spv_share = 0.1;
*%renewable_share%wind_share_on = 0.2;
*%renewable_share%wind_share_off = 0.1;
*%renewable_share%$goto skip_loop_res_share


* Loop over regions
loop(reg_solve(reg),

         d(h) = d_y_reg('2013',reg,h) ;
         phi_res(res,h) = phi_res_y_reg('2013',reg,h,res) ;
         c_m(ct) = c_m_reg(ct,reg);


* Loop over res shares

$IFTHEN.solarEndog NOT "%solar%" == "endogenous"
         loop(share$loop_phi_spv(share),
                 spv_share = phi_spv(share);
                 display spv_share;
$ENDIF.solarEndog

                
$IFTHEN.windEndog NOT "%wind%" == "endogenous"
                 loop(share1$loop_phi_wind_on(share1),
                         loop(share2$loop_phi_wind_off(share2),
                                 wind_share_on = phi_wind_on(share1);
                                 wind_share_off = phi_wind_off(share2);
                                 display wind_share_on,wind_share_off ;
$ENDIF.windEndog

$IFTHEN.p2gEndog NOT "%p2g%" == "endogenous"
loop(share3$loop_phi_p2g(share3),
        p2g_share = phi_p2g(share3);
        display p2g_share;
$ENDIF.p2gEndog

*loop( loop_res_share ,
*        phi_min_res= 0$(ord(loop_res_share) = 1)
*         + 0.6$(ord(loop_res_share) = 2)
*         + 0.7$(ord(loop_res_share) = 3)
*         + 0.8$(ord(loop_res_share) = 4)
*         + 0.9$(ord(loop_res_share) = 5)
*         + 1$(ord(loop_res_share) = 6) ;
*$label skip_loop_res_share

* Fix again within each loop
$include fix.gms

* Solve model
solve DIETER using lp minimizing Z ;

* Assign default correction factors
corr_fac_con(ct,h) = 0 ;
corr_fac_res(res,h) = 0 ;
corr_fac_sto(sto,h) = 0 ;
corr_fac_dsm_cu(dsm_curt,h) = 0 ;
corr_fac_dsm_shift(dsm_shift,h) = 0 ;



* Define gross energy demand for reporting, egual to equation 5a
gross_energy_demand = sum( h , d(h) + sum( sto , STO_IN.l(sto,h) - STO_OUT.l(sto,h) )
%DSM%$ontext
         - sum( dsm_curt , DSM_CU.l(dsm_curt,h) )
$ontext
$offtext
%reserves%$ontext
        + phi_mean_reserves_call('PR_up') * phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum( res , reserves_slope(reserves,res) * N_RES.l(res)/1000 ) ) )
        + phi_mean_reserves_call('SR_up') *( 1000 * phi_reserves_share('SR_up') * (reserves_intercept('SR_up') + sum( res , reserves_slope('SR_up',res) * N_RES.l(res)/1000 ) ) )
        + phi_mean_reserves_call('MR_up') *( 1000 * phi_reserves_share('MR_up') * (reserves_intercept('MR_up') + sum( res , reserves_slope('MR_up',res) * N_RES.l(res)/1000 ) ) )
        - phi_mean_reserves_call('PR_do') * phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum( res , reserves_slope(reserves,res) * N_RES.l(res)/1000 ) ) )
        - phi_mean_reserves_call('SR_do') *( 1000 * phi_reserves_share('SR_do') * (reserves_intercept('SR_do') + sum( res , reserves_slope('SR_do',res) * N_RES.l(res)/1000 ) ) )
        - phi_mean_reserves_call('MR_do') *( 1000 * phi_reserves_share('MR_do') * (reserves_intercept('MR_do') + sum( res , reserves_slope('MR_do',res) * N_RES.l(res)/1000 ) ) )

       + sum( sto ,
       + RP_STO_IN.l('PR_do',sto,h)*phi_reserves_call('PR_do',h) + RP_STO_IN.l('SR_do',sto,h)*phi_reserves_call('SR_do',h) + RP_STO_IN.l('MR_do',sto,h)*phi_reserves_call('MR_do',h)
       - RP_STO_IN.l('PR_up',sto,h)*phi_reserves_call('PR_up',h) - RP_STO_IN.l('SR_up',sto,h)*phi_reserves_call('SR_up',h) - RP_STO_IN.l('MR_up',sto,h)*phi_reserves_call('MR_up',h)
       - RP_STO_OUT.l('PR_up',sto,h)*phi_reserves_call('PR_up',h) - RP_STO_OUT.l('SR_up',sto,h)*phi_reserves_call('SR_up',h) - RP_STO_OUT.l('MR_up',sto,h)*phi_reserves_call('MR_up',h)
       + RP_STO_OUT.l('PR_do',sto,h)*phi_reserves_call('PR_do',h) + RP_STO_OUT.l('SR_do',sto,h)*phi_reserves_call('SR_do',h) + RP_STO_OUT.l('MR_do',sto,h)*phi_reserves_call('MR_do',h) )
$ontext
$offtext
%DSM%$ontext
%reserves%$ontext
       - sum( dsm_curt , RP_DSM_CU.l('SR_up',dsm_curt,h) * phi_reserves_call('SR_up',h) )
       - sum( dsm_curt , RP_DSM_CU.l('MR_up',dsm_curt,h) * phi_reserves_call('MR_up',h) )
$ontext
$offtext
)
;

* Determine correction factors
%reserves%$ontext
        corr_fac_con(ct,h) =
        - RP_CON.l('PR_up',ct,h) * phi_reserves_call('PR_up',h)
        - RP_CON.l('SR_up',ct,h) * phi_reserves_call('SR_up',h)
        - RP_CON.l('MR_up',ct,h) * phi_reserves_call('MR_up',h)
        + RP_CON.l('PR_do',ct,h) * phi_reserves_call('PR_do',h)
        + RP_CON.l('SR_do',ct,h) * phi_reserves_call('SR_do',h)
        + RP_CON.l('MR_do',ct,h) * phi_reserves_call('MR_do',h)
;
        corr_fac_res(res,h) =
        - RP_RES.l('PR_up',res,h) * phi_reserves_call('PR_up',h)
        - RP_RES.l('SR_up',res,h) * phi_reserves_call('SR_up',h)
        - RP_RES.l('MR_up',res,h) * phi_reserves_call('MR_up',h)
        + RP_RES.l('PR_do',res,h) * phi_reserves_call('PR_do',h)
        + RP_RES.l('SR_do',res,h) * phi_reserves_call('SR_do',h)
        + RP_RES.l('MR_do',res,h) * phi_reserves_call('MR_do',h)
;
        corr_fac_sto(sto,h) =
        - RP_STO_IN.l('PR_up',sto,h) * phi_reserves_call('PR_up',h)
        - RP_STO_IN.l('SR_up',sto,h) * phi_reserves_call('SR_up',h)
        - RP_STO_IN.l('MR_up',sto,h) * phi_reserves_call('MR_up',h)
        - RP_STO_OUT.l('PR_up',sto,h) * phi_reserves_call('PR_up',h)
        - RP_STO_OUT.l('SR_up',sto,h) * phi_reserves_call('SR_up',h)
        - RP_STO_OUT.l('MR_up',sto,h) * phi_reserves_call('MR_up',h)
        + RP_STO_IN.l('PR_do',sto,h) * phi_reserves_call('PR_do',h)
        + RP_STO_IN.l('SR_do',sto,h) * phi_reserves_call('SR_do',h)
        + RP_STO_IN.l('MR_do',sto,h) * phi_reserves_call('MR_do',h)
        + RP_STO_OUT.l('PR_do',sto,h) * phi_reserves_call('PR_do',h)
        + RP_STO_OUT.l('SR_do',sto,h) * phi_reserves_call('SR_do',h)
        + RP_STO_OUT.l('MR_do',sto,h) * phi_reserves_call('MR_do',h)
;
%DSM%$ontext
        corr_fac_dsm_cu(dsm_curt,h) =
        - RP_DSM_CU.l('SR_up',dsm_curt,h) * phi_reserves_call('SR_up',h)
        - RP_DSM_CU.l('MR_up',dsm_curt,h) * phi_reserves_call('MR_up',h)
;
        corr_fac_dsm_shift(dsm_shift,h) =
        - RP_DSM_SHIFT.l('SR_up',dsm_shift,h) * phi_reserves_call('SR_up',h)
        - RP_DSM_SHIFT.l('MR_up',dsm_shift,h) * phi_reserves_call('MR_up',h)
        + RP_DSM_SHIFT.l('SR_do',dsm_shift,h) * phi_reserves_call('SR_do',h)
        + RP_DSM_SHIFT.l('MR_do',dsm_shift,h) * phi_reserves_call('MR_do',h)
;
$ontext
$offtext

* Report files
*        report: 'model','scenario','year','state','region','variable','solar_share','wind_on_share', 'wind_off_share','p2g_share', 'value'
        report('DIETER',reg,'net_energy_demand'%res_share%%em_share%) = sum( h , d(h));
        report('DIETER',reg,'model status'%res_share%%em_share%) = DIETER.modelstat ;
        report('DIETER',reg,'solve time'%res_share%%em_share%) = DIETER.resUsd ;
        report('DIETER',reg,'obj value'%res_share%%em_share%) = Z.l * %sec_hour% ;

        report_tech('DIETER',reg,'net shares'%res_share%%em_share%,res) = sum( h , G_RES.l(res,h))/sum(h,d(h)) ;

        report_tech('DIETER',reg,'capacities conventional'%res_share%%em_share%,ct)$(ord(ct) > 1 and not ord(ct)=card(ct)) =  N_CON.l(ct) ;
        report_tech('DIETER',reg,'capacities renewable'%res_share%%em_share%,res) =  N_RES.l(res) ;
        report_tech('DIETER',reg,'capacities renewable'%res_share%%em_share%,'bio') =  N_CON.l('bio') ;
        report_tech('DIETER',reg,'capacities renewable'%res_share%%em_share%,'ror') = 0 + N_CON.l('ror') ;
        report_tech('DIETER',reg,'capacities storage MW'%res_share%%em_share%,sto) =  N_STO_P.l(sto) ;
        report_tech('DIETER',reg,'capacities storage MWh'%res_share%%em_share%,sto) =  N_STO_E.l(sto) * %sec_hour% ;
        report('DIETER',reg,'gross_energy_demand'%res_share%%em_share%) = gross_energy_demand ;
        report_tech('DIETER',reg,'conshares'%res_share%%em_share%,ct)$(not ord(ct)=card(ct)) = sum( h, G_L.l(ct,h) ) / gross_energy_demand ;
        report('DIETER',reg,'conshare total'%res_share%%em_share%) = sum(ct, report_tech('DIETER',reg,'conshares'%res_share%%em_share%,ct) ) ;
        report_tech('DIETER',reg,'renshares'%res_share%%em_share%,res) = sum( h, G_RES.l(res,h) - corr_fac_res(res,h))/ gross_energy_demand ;
        report_tech('DIETER',reg,'renshares'%res_share%%em_share%,'bio') = sum( h, G_L.l('bio',h) ) / gross_energy_demand ;
        report_tech('DIETER',reg,'renshares'%res_share%%em_share%,'ror') = sum( h, G_L.l('ror',h) ) / gross_energy_demand ;
        report('DIETER',reg,'renshare total'%res_share%%em_share%) = sum(res,report_tech('DIETER',reg,'renshares'%res_share%%em_share%,res)) + report_tech('DIETER',reg,'renshares'%res_share%%em_share%,'bio') + report_tech('DIETER',reg,'renshares'%res_share%%em_share%,'ror') ;
        report_tech('DIETER',reg,'curtailment of fluct res absolute'%res_share%%em_share%,res) =  sum(h,CU.l(res,h)) * %sec_hour% ;
        report_tech('DIETER',reg,'curtailment of fluct res relative'%res_share%%em_share%,res)$report_tech('DIETER',reg,'curtailment of fluct res absolute'%res_share%%em_share%,res) =  sum(h,CU.l(res,h))/ (sum(h,G_RES.l(res,h) - corr_fac_res(res,h) ) + sum(h,CU.l(res,h)) ) ;
        report_tech_hours('DIETER',reg,'generation conventional'%res_share%%em_share%,ct,h) =  G_L.l(ct,h) + corr_fac_con(ct,h) ;
        report_tech_hours('DIETER',reg,'generation renewable'%res_share%%em_share%,'ror',h) = G_L.l('ror',h) ;
        report_tech_hours('DIETER',reg,'generation renewable'%res_share%%em_share%,res,h) = G_RES.l(res,h) ;
        report_tech_hours('DIETER',reg,'curtailment of fluct res'%res_share%%em_share%,res,h) =  CU.l(res,h) ;
        report_tech_hours('DIETER',reg,'generation storage'%res_share%%em_share%,sto,h) =  STO_OUT.l(sto,h) ;
        report_tech_hours('DIETER',reg,'storage loading'%res_share%%em_share%,sto,h) =  STO_IN.l(sto,h) ;
        report_tech_hours('DIETER',reg,'storage level'%res_share%%em_share%,sto,h) =  STO_L.l(sto,h) ;
        report_hours('DIETER',reg,'demand'%res_share%%em_share%,h) = d(h) ;
        report('DIETER',reg,'curtailment of fluct res absolute'%res_share%%em_share%) = sum((res,h),CU.l(res,h)) * %sec_hour% ;
        report('DIETER',reg,'curtailment of fluct res relative'%res_share%%em_share%)$report('DIETER',reg,'curtailment of fluct res absolute'%res_share%%em_share%) = sum((res,h),CU.l(res,h))/( sum((res,h),G_RES.l(res,h) - corr_fac_res(res,h) ) + sum((res,h),CU.l(res,h)) ) ;
        report('DIETER',reg,'bio not utilized absolute'%res_share%%em_share%)$(cdata("m_con_e",'bio')) = (cdata("m_con_e",'bio') - sum(h,G_L.l('bio',h))) * %sec_hour% ;
        report('DIETER',reg,'bio not utilized relative'%res_share%%em_share%)$(cdata("m_con_e",'bio')) = (cdata("m_con_e",'bio') - sum(h,G_L.l('bio',h)))/cdata("m_con_e",'bio') ;

        report_hours('DIETER',reg,'price'%res_share%%em_share%,h) = -con1a_bal.m(h);
        loop(h,
                if(-con1a_bal.m(h) > calc_maxprice,
                calc_maxprice = -con1a_bal.m(h) ;);
                report('DIETER',reg,'max price'%res_share%%em_share%) = calc_maxprice ;
        );
        report('DIETER',reg,'mean price'%res_share%%em_share%) = -sum(h,con1a_bal.m(h))/card(h) ;
        loop(h,
                if(-con1a_bal.m(h) < calc_minprice,
                calc_minprice = -con1a_bal.m(h) ;);
                report('DIETER',reg,'min price'%res_share%%em_share%) = calc_minprice ;
        );

report_tech('DIETER',reg,'load-weighted price'%res_share%%em_share%,res) = -sum(h,con1a_bal.m(h)*d(h))/sum(h,d(h)) ;


report_tech('DIETER',reg,'market value'%res_share%%em_share%,res)$(sum(h, G_RES.l(res,h) ne 0 ))
  = sum( h , G_RES.l(res,h)*(-con1a_bal.m(h)))/sum( h , G_RES.l(res,h) + CU.l(res,h) );


report_tech('DIETER',reg,'market value'%res_share%%em_share%,ct)$(sum(h, G_L.l(ct,h) ne 0 ))
  = sum( h , G_L.l(ct,h)*(-con1a_bal.m(h)))/sum( h , G_L.l(ct,h));

report_tech('DIETER',reg,'market value_discharge'%res_share%%em_share%,sto)$(sum(h, STO_OUT.l(sto,h) ne 0 ))
  = sum( h , STO_OUT.l(sto,h)*(-con1a_bal.m(h)))/sum( h , STO_OUT.l(sto,h));

report_tech('DIETER',reg,'market value_charge'%res_share%%em_share%,sto)$(sum(h, STO_IN.l(sto,h) ne 0 ))
  = sum( h , STO_IN.l(sto,h)*(-con1a_bal.m(h)))/sum( h , STO_IN.l(sto,h));


report_tech('DIETER',reg,'market value_DSM_UP'%res_share%%em_share%,dsm_shift)$(sum(h, DSM_UP_DEMAND.l(dsm_shift,h) ne 0 ))
  = sum( h , DSM_UP_DEMAND.l(dsm_shift,h)*(-con1a_bal.m(h)))/sum( h , DSM_UP_DEMAND.l(dsm_shift,h));

report_tech('DIETER',reg,'market value_DSM_DO'%res_share%%em_share%,dsm_shift)$(sum(h, DSM_DO_DEMAND.l(dsm_shift,h) ne 0 ))
  = sum( h , DSM_DO_DEMAND.l(dsm_shift,h)*(-con1a_bal.m(h)))/sum( h , DSM_DO_DEMAND.l(dsm_shift,h));











report_tech('DIETER',reg,'base value factor'%res_share%%em_share%,res)$( sum(h, G_RES.l(res,h)) ne 0 )
  = (report_tech('DIETER',reg,'market value'%res_share%%em_share%,res)$( sum(h, G_RES.l(res,h)) ne 0 ))/ report('DIETER',reg,'mean price'%res_share%%em_share%);

 report_tech('DIETER',reg,'load-weighted value factor'%res_share%%em_share%,res)$( sum(h, G_RES.l(res,h)) ne 0 )
  = (report_tech('DIETER',reg,'market value'%res_share%%em_share%,res)$( sum(h, G_RES.l(res,h)) ne 0 ))/report_tech('DIETER',reg,'load-weighted price'%res_share%%em_share%,res);







        report('DIETER',reg,'Capacity total'%res_share%%em_share%) = sum( ct , N_CON.l(ct) ) + sum( res , N_RES.l(res) ) + sum( sto , N_STO_P.l(sto) )
%DSM%$ontext
        + sum( dsm_curt , N_DSM_CU.l(dsm_curt) ) + sum( dsm_shift , N_DSM_SHIFT.l(dsm_shift) )
$ontext
$offtext
;
        report_tech('DIETER',reg,'Capacity share'%res_share%%em_share%,ct) = N_CON.l(ct) / report('DIETER',reg,'Capacity total'%res_share%%em_share%) + 1e-9 ;
        report_tech('DIETER',reg,'Capacity share'%res_share%%em_share%,'ror') = N_CON.l('ror') / report('DIETER',reg,'Capacity total'%res_share%%em_share%) + 1e-9 ;
        report_tech('DIETER',reg,'Capacity share'%res_share%%em_share%,res) = N_RES.l(res) / report('DIETER',reg,'Capacity total'%res_share%%em_share%) + 1e-9 ;
        report_tech('DIETER',reg,'Capacity share'%res_share%%em_share%,sto) = N_STO_P.l(sto) / report('DIETER',reg,'Capacity total'%res_share%%em_share%) + 1e-9 ;
        report('DIETER',reg,'Energy total'%res_share%%em_share%) = (sum( h , d(h) ) + sum( (sto,h) , STO_IN.l(sto,h) )) * %sec_hour%
%DSM%$ontext
        + sum( (dsm_shift,h) , DSM_UP_DEMAND.l(dsm_shift,h) ) * %sec_hour%
$ontext
$offtext
%reserves%$ontext
       + sum( h$(ord(h) > 1) , phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum( res , reserves_slope(reserves,res) * N_RES.l(res)/1000 ) ) ) * phi_reserves_call('PR_up',h) )
       + sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('SR_up') * ( reserves_intercept('SR_up') + sum( res , reserves_slope('SR_up',res) * N_RES.l(res)/1000 ) ) * phi_reserves_call('SR_up',h) )
       + sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('MR_up') * ( reserves_intercept('MR_up') + sum( res , reserves_slope('MR_up',res) * N_RES.l(res)/1000 ) ) * phi_reserves_call('MR_up',h) )
       - sum( h$(ord(h) > 1) , phi_reserves_pr* sum( reserves$( ord(reserves) > 2) , 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum( res , reserves_slope(reserves,res) * N_RES.l(res)/1000 ) ) ) * phi_reserves_call('PR_do',h) )
       - sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('SR_do') * ( reserves_intercept('SR_do') + sum( res , reserves_slope('SR_do',res) * N_RES.l(res)/1000 ) ) * phi_reserves_call('SR_do',h) )
       - sum( h$(ord(h) > 1) , 1000 * phi_reserves_share('MR_do') * ( reserves_intercept('MR_do') + sum( res , reserves_slope('MR_do',res) * N_RES.l(res)/1000 ) ) * phi_reserves_call('MR_do',h) )
$ontext
$offtext
;
        report_tech('DIETER',reg,'Energy share'%res_share%%em_share%,ct) = sum( h , G_L.l(ct,h) ) / report('DIETER',reg,'Energy total'%res_share%%em_share%) * %sec_hour% + 1e-9 ;
        report_tech('DIETER',reg,'Energy share'%res_share%%em_share%,'ror') = sum( h , G_L.l('ror',h) ) / report('DIETER',reg,'Energy total'%res_share%%em_share%) * %sec_hour% + 1e-9 ;
        report_tech('DIETER',reg,'Energy share'%res_share%%em_share%,res) = sum( h , G_RES.l(res,h) - corr_fac_res(res,h) ) / report('DIETER',reg,'Energy total'%res_share%%em_share%) * %sec_hour% + 1e-9 ;
        report_tech('DIETER',reg,'Energy share'%res_share%%em_share%,sto) = sum( h , STO_OUT.l(sto,h) - corr_fac_sto(sto,h) ) / report('DIETER',reg,'Energy total'%res_share%%em_share%) * %sec_hour% + 1e-9 ;
        report_tech('DIETER',reg,'Total Renewable Generation'%res_share%%em_share%,res) = sum( h , G_RES.l(res,h)) ;
        report_tech('DIETER',reg,'Total Renewable Curtailment'%res_share%%em_share%,res) = sum( h , CU.l(res,h)) ;
        report_tech('DIETER',reg,'Storage out total wholesale'%res_share%%em_share%,sto) = sum(h, report_tech_hours('DIETER',reg,'generation storage'%res_share%%em_share%,sto,h) ) * %sec_hour% ;
        report_tech('DIETER',reg,'Storage in total wholesale'%res_share%%em_share%,sto) = sum(h, report_tech_hours('DIETER',reg,'storage loading'%res_share%%em_share%,sto,h) ) * %sec_hour% ;
%reserves%$ontext
        report_tech('DIETER',reg,'Storage positive reserves activation by storage in'%res_share%%em_share%,sto) = sum( (h,reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5 ), RP_STO_IN.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour% ;
        report_tech('DIETER',reg,'Storage negative reserves activation by storage in'%res_share%%em_share%,sto) = sum( (h,reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6 ), RP_STO_IN.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour% ;
        report_tech('DIETER',reg,'Storage positive reserves activation by storage out'%res_share%%em_share%,sto) = sum( (h,reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5 ), RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour% ;
        report_tech('DIETER',reg,'Storage negative reserves activation by storage out'%res_share%%em_share%,sto) = sum( (h,reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6 ), RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour% ;
$ontext
$offtext
%reserves%        report_tech('DIETER',reg,'Storage FLH'%res_share%%em_share%,sto)$N_STO_P.l(sto) = report_tech('DIETER',reg,'Storage out total wholesale'%res_share%%em_share%,sto) / N_STO_P.l(sto) ;
%reserves%        report_tech('DIETER',reg,'Storage cycles'%res_share%%em_share%,sto)$N_STO_E.l(sto) = report_tech('DIETER',reg,'Storage out total wholesale'%res_share%%em_share%,sto) / N_STO_E.l(sto) * %sec_hour% ;
%reserves%$ontext
        report_tech('DIETER',reg,'Storage FLH'%res_share%%em_share%,sto)$N_STO_P.l(sto) = ( report_tech('DIETER',reg,'Storage out total wholesale'%res_share%%em_share%,sto)
                        + sum( (h,reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5 ), RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour%
                        - sum( (h,reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6 ), RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour%
                        ) / N_STO_P.l(sto) ;
        report_tech('DIETER',reg,'Storage cycles'%res_share%%em_share%,sto)$N_STO_E.l(sto) = ( report_tech('DIETER',reg,'Storage out total wholesale'%res_share%%em_share%,sto)
                        + sum( (h,reserves)$(ord(reserves) = 1 or ord(reserves) = 3 or ord(reserves) = 5 ), RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour%
                        - sum( (h,reserves)$(ord(reserves) = 2 or ord(reserves) = 4 or ord(reserves) = 6 ), RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h)) * %sec_hour%
                        ) / N_STO_E.l(sto) * %sec_hour% ;
$ontext
$offtext
        report_tech('DIETER',reg,'Storage EP-ratio'%res_share%%em_share%,sto)$N_STO_P.l(sto) = N_STO_E.l(sto) * %sec_hour% / N_STO_P.l(sto) ;

%reserves%$ontext
         report_reserves_tech('Reserves provision ratio'%res_share%%em_share%,reserves,ct)$(N_CON.l(ct)) = sum( h , RP_CON.l(reserves,ct,h) ) / sum( h , G_L.l(ct,h) + corr_fac_con(ct,h) ) ;
         report_reserves_tech('Reserves activation ratio'%res_share%%em_share%,reserves,ct)$(N_CON.l(ct)) = sum( h , RP_CON.l(reserves,ct,h) * phi_reserves_call(reserves,h)) / sum( h , G_L.l(ct,h) + corr_fac_con(ct,h) ) ;
         report_reserves_tech('Reserves provision ratio'%res_share%%em_share%,reserves,res)$(N_RES.l(res)) = sum( h , RP_RES.l(reserves,res,h) ) / sum( h , G_RES.l(res,h) ) ;
         report_reserves_tech('Reserves activation ratio'%res_share%%em_share%,reserves,res)$(N_RES.l(res)) = sum( h , RP_RES.l(reserves,res,h) * phi_reserves_call(reserves,h) ) / sum( h , G_RES.l(res,h) ) ;
         report_reserves_tech('Reserves provision ratio (storage out positive reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 1 OR ord(reserves) = 3 OR ord(reserves) = 5 ) ) = sum( h , RP_STO_OUT.l(reserves,sto,h) ) / sum( h , STO_OUT.l(sto,h) ) ;
         report_reserves_tech('Reserves provision ratio (storage out negative reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 2 OR ord(reserves) = 4 OR ord(reserves) = 6 ) ) = sum( h , RP_STO_OUT.l(reserves,sto,h) ) / sum( h , STO_OUT.l(sto,h) ) ;
         report_reserves_tech('Reserves provision ratio (storage in positive reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 1 OR ord(reserves) = 3 OR ord(reserves) = 5 ) ) = sum( h , RP_STO_IN.l(reserves,sto,h) ) / sum( h , STO_IN.l(sto,h) ) ;
         report_reserves_tech('Reserves provision ratio (storage in negative reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 2 OR ord(reserves) = 4 OR ord(reserves) = 6 ) ) = sum( h , RP_STO_IN.l(reserves,sto,h) ) / sum( h , STO_IN.l(sto,h) ) ;
         report_reserves_tech('Reserves activation ratio (storage out positive reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 1 OR ord(reserves) = 3 OR ord(reserves) = 5 ) ) = sum( h , RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h) ) / sum( h , STO_OUT.l(sto,h) ) ;
         report_reserves_tech('Reserves activation ratio (storage out negative reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 2 OR ord(reserves) = 4 OR ord(reserves) = 6 ) ) = sum( h , RP_STO_OUT.l(reserves,sto,h) * phi_reserves_call(reserves,h) ) / sum( h , STO_OUT.l(sto,h) ) ;
         report_reserves_tech('Reserves activation ratio (storage in positive reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 1 OR ord(reserves) = 3 OR ord(reserves) = 5 ) ) = sum( h , RP_STO_IN.l(reserves,sto,h) * phi_reserves_call(reserves,h) ) / sum( h , STO_IN.l(sto,h) ) ;
         report_reserves_tech('Reserves activation ratio (storage in negative reserves)'%res_share%%em_share%,reserves,sto)$( N_STO_P.l(sto) AND ( ord(reserves) = 2 OR ord(reserves) = 4 OR ord(reserves) = 6 ) ) = sum( h , RP_STO_IN.l(reserves,sto,h) * phi_reserves_call(reserves,h) ) / sum( h , STO_IN.l(sto,h) ) ;
$ontext
$offtext

%DSM%$ontext
        report('DIETER',reg,'load curtailment absolute (non-reserves)'%res_share%%em_share%) =  sum((dsm_curt,h), DSM_CU.l(dsm_curt,h)) * %sec_hour% ;
        report('DIETER',reg,'load shift pos absolute (non-reserves)'%res_share%%em_share%) =  sum((dsm_shift,h), DSM_UP_DEMAND.l(dsm_shift,h)) * %sec_hour% ;
        report('DIETER',reg,'load shift neg absolute (non-reserves)'%res_share%%em_share%) =  sum((dsm_shift,h), DSM_DO_DEMAND.l(dsm_shift,h)) * %sec_hour% ;
        report_tech('DIETER',reg,'capacities load curtailment'%res_share%%em_share%,dsm_curt) =  N_DSM_CU.l(dsm_curt) ;
        report_tech('DIETER',reg,'capacities load shift'%res_share%%em_share%,dsm_shift) =  N_DSM_SHIFT.l(dsm_shift) ;
        report_tech_hours('DIETER',reg,'load curtailment (non-reserves)'%res_share%%em_share%,dsm_curt,h) = DSM_CU.l(dsm_curt,h) ;
        report_tech_hours('DIETER',reg,'load shift pos (non-reserves)'%res_share%%em_share%,dsm_shift,h) = DSM_UP_DEMAND.l(dsm_shift,h) ;
        report_tech_hours('DIETER',reg,'load shift neg (non-reserves)'%res_share%%em_share%,dsm_shift,h) = DSM_DO_DEMAND.l(dsm_shift,h) ;
        report_tech('DIETER',reg,'Capacity share'%res_share%%em_share%,dsm_curt) = N_DSM_CU.l(dsm_curt) / report('DIETER',reg,'Capacity total'%res_share%%em_share%) + 1e-9 ;
        report_tech('DIETER',reg,'Capacity share'%res_share%%em_share%,dsm_shift) = N_DSM_SHIFT.l(dsm_shift) / report('DIETER',reg,'Capacity total'%res_share%%em_share%) + 1e-9 ;
        report_tech('DIETER',reg,'Energy share'%res_share%%em_share%,dsm_curt) = sum( h , DSM_CU.l(dsm_curt,h) - corr_fac_dsm_cu(dsm_curt,h) )  / report('DIETER',reg,'Energy total'%res_share%%em_share%) * %sec_hour% + 1e-9 ;
        report_tech('DIETER',reg,'Energy share'%res_share%%em_share%,dsm_shift) = sum( h , DSM_DO_DEMAND.l(dsm_shift,h) - corr_fac_dsm_shift(dsm_shift,h) ) / report('DIETER',reg,'Energy total'%res_share%%em_share%) * %sec_hour% + 1e-9 ;
        report_tech('DIETER',reg,'Load shift pos absolute (total)'%res_share%%em_share%,dsm_shift) = sum( h , DSM_UP.l(dsm_shift,h) ) ;
        report_tech('DIETER',reg,'Load shift neg absolute (total)'%res_share%%em_share%,dsm_shift) = sum( (h,hh) , DSM_DO.l(dsm_shift,h,hh) ) ;
        report_tech('DIETER',reg,'Load shift pos absolute (wholesale)'%res_share%%em_share%,dsm_shift) = sum( h , DSM_UP_DEMAND.l(dsm_shift,h) ) ;
        report_tech('DIETER',reg,'Load shift neg absolute (wholesale)'%res_share%%em_share%,dsm_shift) = sum( h , DSM_DO_DEMAND.l(dsm_shift,h) ) ;
        report_tech('DIETER',reg,'Load shift pos absolute (reserves)'%res_share%%em_share%,dsm_shift) = report_tech('DIETER',reg,'Load shift pos absolute (total)'%res_share%%em_share%,dsm_shift) - report_tech('DIETER',reg,'Load shift pos absolute (wholesale)'%res_share%%em_share%,dsm_shift) ;
        report_tech('DIETER',reg,'Load shift neg absolute (reserves)'%res_share%%em_share%,dsm_shift) = report_tech('DIETER',reg,'Load shift neg absolute (total)'%res_share%%em_share%,dsm_shift) - report_tech('DIETER',reg,'Load shift neg absolute (wholesale)'%res_share%%em_share%,dsm_shift) ;
$ontext
$offtext

%reserves%$ontext
        report_reserves('reserve provision requirements'%res_share%%em_share%,reserves)$(ord(reserves) < 3) = phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  (1000 * phi_reserves_share(reservesreserves) * (reservedata("reserves_intercept",reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000) )) ) ;
        report_reserves('reserve provision requirements'%res_share%%em_share%,reserves)$(ord(reserves) > 2) = 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) ) ;
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,ct)$(ord(reserves) < 3) = sum( h , RP_CON.l(reserves,ct,h)) / (phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  (card(h) * 1000 * phi_reserves_share(reservesreserves) * (reservedata("reserves_intercept",reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000) )) )) ;
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,res)$(ord(reserves) < 3) = sum( h , RP_RES.l(reserves,res,h)) / (phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  (card(h) * 1000 * phi_reserves_share(reservesreserves) * (reservedata("reserves_intercept",reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000) )) )) ;
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,sto)$(ord(reserves) < 3) = sum( h , RP_STO_IN.l(reserves,sto,h) + RP_STO_OUT.l(reserves,sto,h)) / (phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2 ) ,  (card(h) * 1000 * phi_reserves_share(reservesreserves) * (reservedata("reserves_intercept",reservesreserves) + sum(resres,reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000) )) ));
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,ct)$( ord(reserves) > 2 ) = sum( h , RP_CON.l(reserves,ct,h)) / (card(h) * 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) ) ) ;
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,res)$( ord(reserves) > 2 ) = sum( h , RP_RES.l(reserves,res,h)) / (card(h) * 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,sto)$( ord(reserves) > 2 ) = sum( h , RP_STO_IN.l(reserves,sto,h) + RP_STO_OUT.l(reserves,sto,h)) / (card(h) * 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,ct)$(ord(reserves) < 3) = sum(h,RP_CON.l(reserves,ct,h)*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reservedata("reserves_intercept",reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000 ) ) )   ) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,res)$(ord(reserves) < 3) = sum(h,RP_RES.l(reserves,res,h)*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reservedata("reserves_intercept",reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000 ) ) )   ) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,sto)$(ord(reserves) < 3) = sum(h,(RP_STO_IN.l(reserves,sto,h) + RP_STO_OUT.l(reserves,sto,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) *  phi_reserves_pr* sum( reservesreserves$( ord(reservesreserves) > 2) , 1000 * phi_reserves_share(reservesreserves) * ( reservedata("reserves_intercept",reservesreserves) + sum(resres , reserves_slope(reservesreserves,resres) * N_RES.l(resres)/1000 ) ) )   );
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,ct)$( ord(reserves) > 2 ) = sum(h,RP_CON.l(reserves,ct,h)*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,res)$( ord(reserves) > 2 ) = sum(h,RP_RES.l(reserves,res,h)*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,sto)$( ord(reserves) > 2 ) = sum(h,(RP_STO_IN.l(reserves,sto,h) + RP_STO_OUT.l(reserves,sto,h))*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech_hours('Reserves provision'%res_share%%em_share%,reserves,'required',h) = report_reserves('reserve provision requirements'%res_share%%em_share%,reserves) ;
        report_reserves_tech_hours('Reserves activation'%res_share%%em_share%,reserves,'required',h) = phi_reserves_call(reserves,h) * report_reserves('reserve provision requirements'%res_share%%em_share%,reserves) ;
        report_reserves_tech_hours('Reserves provision'%res_share%%em_share%,reserves,ct,h) = RP_CON.l(reserves,ct,h) ;
        report_reserves_tech_hours('Reserves activation'%res_share%%em_share%,reserves,ct,h) = RP_CON.l(reserves,ct,h)*phi_reserves_call(reserves,h) ;
        report_reserves_tech_hours('Reserves provision'%res_share%%em_share%,reserves,res,h) = RP_RES.l(reserves,res,h) ;
        report_reserves_tech_hours('Reserves activation'%res_share%%em_share%,reserves,res,h) = RP_RES.l(reserves,res,h)*phi_reserves_call(reserves,h) ;
        report_reserves_tech_hours('Reserves provision'%res_share%%em_share%,reserves,sto,h) = RP_STO_IN.l(reserves,sto,h) + RP_STO_OUT.l(reserves,sto,h) ;
        report_reserves_tech_hours('Reserves activation'%res_share%%em_share%,reserves,sto,h) = (RP_STO_IN.l(reserves,sto,h) + RP_STO_OUT.l(reserves,sto,h))*phi_reserves_call(reserves,h) ;
$ontext
$offtext

%DSM%$ontext
%reserves%$ontext
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,dsm_curt)$( ord(reserves) > 2 ) = sum( h , RP_DSM_CU.l(reserves,dsm_curt,h)) / ( card(h) * 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve provision shares'%res_share%%em_share%,reserves,dsm_shift)$( ord(reserves) > 2 ) = sum( h , RP_DSM_SHIFT.l(reserves,dsm_shift,h)) / ( card(h) * 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,dsm_curt)$( ord(reserves) > 2 ) = sum( h , RP_DSM_CU.l(reserves,dsm_curt,h)*phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech('reserve activation shares'%res_share%%em_share%,reserves,dsm_shift)$( ord(reserves) > 2 ) = sum( h , RP_DSM_SHIFT.l(reserves,dsm_shift,h) * phi_reserves_call(reserves,h)) / sum( h , phi_reserves_call(reserves,h) * 1000 * phi_reserves_share(reserves) * (reservedata("reserves_intercept",reserves) + sum(resres,reserves_slope(reserves,resres) * N_RES.l(resres)/1000) )) ;
        report_reserves_tech_hours('Reserves provision'%res_share%%em_share%,reserves,dsm_shift,h)$(ord(reserves) > 2) = RP_DSM_SHIFT.l(reserves,dsm_shift,h) ;
        report_reserves_tech_hours('Reserves activation'%res_share%%em_share%,reserves,dsm_shift,h)$(ord(reserves) > 2) = RP_DSM_SHIFT.l(reserves,dsm_shift,h)*phi_reserves_call(reserves,h) ;
        report_reserves_tech_hours('Reserves provision'%res_share%%em_share%,reserves,dsm_curt,h)$(ord(reserves) > 2) = RP_DSM_CU.l(reserves,dsm_curt,h) ;
        report_reserves_tech_hours('Reserves activation'%res_share%%em_share%,reserves,dsm_curt,h)$(ord(reserves) > 2) = RP_DSM_CU.l(reserves,dsm_curt,h)*phi_reserves_call(reserves,h) ;
$ontext
$offtext

* Clear variables and equations to speed up calculation
*$include clear.gms

* Close loop_res_share

$IFTHEN.p2gEndog NOT "%p2g%" == "endogenous"
                        );
$ENDIF.p2gEndog

$IFTHEN.windEndog NOT "%wind%" == "endogenous"
                 );
         );
$ENDIF.windEndog

$IFTHEN.solarEndog NOT "%solar%" == "endogenous"
);
$ENDIF.solarEndog

*region loop close
);


* Read out solutions
%reserves%execute_unload "results", report, report_tech, report_tech_hours, report_hours ;
%reserves%$ontext
execute_unload "results", report, report_tech, report_tech_hours, report_hours, report_reserves, report_reserves_tech, report_reserves_tech_hours ;
$ontext
$offtext
execute_unload "results_full";

%write_to_excel%$ontext
$include report_to_excel
$ontext
$offtext


* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
