***!!!! reporting variable name should obey the format ''NAME (UNIT)'. ANYTHING IN THE PARENTHESIS WILL BE TREATED AS UNIT
*******************************************************************************************************
********************************* reporting for hourly data *******************************************
*     generation MWh -> TWh
        report_tech_hours('DIETER',yr,reg,'generation (GWh)',ct,h) =  ( G_L.l(ct,h) + corr_fac_con(ct,h) )/1e3;
        report_tech_hours('DIETER',yr,reg,'generation (GWh)',res,h) = G_RES.l(res,h) /1e3;
        report_tech_hours('DIETER',yr,reg,'curtailment renewable (GWh)',res,h) =  CU.l(res,h)/1e3 ;
*     generation MWh -> TWh
        report_tech_hours('DIETER',yr,reg,'storage generation (MWh)',sto,h) =  STO_OUT.l(sto,h) ;
        report_tech_hours('DIETER',yr,reg,'storage loading (MWh)',sto,h) =  STO_IN.l(sto,h) ;
        report_tech_hours('DIETER',yr,reg,'storage level (MWh)',sto,h) =  STO_L.l(sto,h) ;
        report_tech_hours('DIETER',yr,reg,'consumption (GWh)','el',h) = d(h) /1e3;

%P2G%$ontext
        report_tech_hours('DIETER',yr,reg,'consumption (GWh)','elh2',h) = C_P2G.l("elh2",h) /1e3;
$ontext
$offtext
        
*        report_hours('DIETER',yr,reg,'fixed demand (MWh)',h) = d(h) ;
        
*model's unit is in $(2005), multiply by 1.2 to $(2015)
        report_hours('DIETER',yr,reg,'hourly wholesale price ($/MWh)',h) = -con1a_bal.m(h) * 1.2;

*******************************************************************************************************
********************************* reporting for annual system data ************************************
*       Report files: cap. transformed into GW (divide by 1e3), generation transformed into TWh (divide by 1e6), costs to $/kW, share to *100%

***     generation MWh -> TWh
        report('DIETER',yr,reg,'energy demand (TWh)') = sum( h , d(h)) /1e6;
%P2G%$ontext
        report('DIETER',yr,reg,'sector coupling green H2 demand (TWh)') = sum( h , C_P2G.l("elh2",h)) /1e6;
$ontext
$offtext
        
        report('DIETER',yr,reg,'model status') = DIETER.modelstat ;
        report('DIETER',yr,reg,'solve time') = DIETER.resUsd ;
*       transform into BillionUSD
        report('DIETER',yr,reg,'obj value (BillionUSD)') = Z.l /1e9;
        report('DIETER',yr,reg,'CO2 price ($/tCO2)') = remind_co2(yr,reg); 
        report('DIETER',yr,reg,'price for fixed demand - with scarcity price ($/MWh)') = -sum(h,con1a_bal.m(h)*d(h))/sum(h,d(h)) * 1.2;
        report('DIETER',yr,reg,'price for total demand - with scarcity price ($/MWh)') = -sum(h,con1a_bal.m(h)*(d(h)+sum(p2g,C_P2G.l(p2g,h))))/sum(h,(d(h)+sum(p2g,C_P2G.l(p2g,h)))) * 1.2;
        report('DIETER',yr,reg,'price for fixed demand ($/MWh)') = sum(h,hourly_price(h)*d(h))/sum(h,d(h)) * 1.2;
        report('DIETER',yr,reg,'price for total demand ($/MWh)') = sum(h,hourly_price(h)*(d(h)+sum(p2g,C_P2G.l(p2g,h))))/sum(h,(d(h)+sum(p2g,C_P2G.l(p2g,h)))) * 1.2 ;        
            
        report('DIETER',yr,reg,'investment interest rate') = r * 1e2;
*       Define gross energy demand for reporting, egual to equation 5a      
        report('DIETER',yr,reg,'total energy demand (TWh)') = totLoad / 1e6;
        report('DIETER',yr,reg,'total energy demand and curtailment (TWh)') = (totLoad + sum(res, sum( h , CU.l(res,h))) )/1e6 ;
        
***********************************************************************************************************
********************************* reporting for annual technology data ************************************

*-------------------------------- reporting for generation and capacities ---------------------------------
        report_tech('DIETER',yr,reg,'Total generation (TWh)',ct) = sum( h , G_L.l(ct,h)) /1e6 ;
        report_tech('DIETER',yr,reg,'Total generation (TWh)',res) = sum( h , G_RES.l(res,h)) /1e6 ;
        report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',res) = sum( h , (G_RES.l(res,h)+CU.l(res,h))) /1e6 ;        
        report_tech('DIETER',yr,reg,'Total renewable curtailment (TWh)',res) = sum( h , CU.l(res,h)) /1e6 ;
        
***     capacity MW -> GW

        report_tech('DIETER',yr,reg,'DIETER pre-investment capacities (GW)',ct) = N_CON.lo(ct) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER pre-investment capacities (GW)',res) = P_RES.lo(res) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER pre-investment capacities (GW)','coal') = (N_CON.lo('lig') + N_CON.lo('hc'))/ 1e3 ;
        
        report_tech('DIETER',yr,reg,'DIETER post-investment capacities (GW)',ct) =  N_CON.l(ct) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER post-investment capacities (GW)',res) =  P_RES.l(res) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER post-investment capacities (GW)','coal') = (N_CON.l('lig') + N_CON.l('hc'))/ 1e3 ;
        
***     reporting on remind run (can be used as a check to reporting in R or as validation)
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities (GW)','coal') = sum(te_remind, sum(grade, preInv_remind_cap(yr,reg,te_remind, grade)$(COALte(te_remind))) ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities (GW)','nuc') = sum(te_remind, sum(grade, preInv_remind_cap(yr,reg,te_remind, grade)$(NUCte(te_remind))) ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities (GW)','CCGT') = sum(te_remind, sum(grade, preInv_remind_cap(yr,reg,te_remind, grade)$(NonPeakGASte(te_remind)))  ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities (GW)','OCGT_eff') = sum(grade, preInv_remind_cap(yr,reg,'ngt', grade)) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities (GW)','bio') = sum(te_remind, sum(grade, preInv_remind_cap(yr,reg,te_remind, grade)$(BIOte(te_remind))   )  ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities (GW)','hc') = sum(te_remind, sum(grade, preInv_remind_cap(yr,reg,te_remind, grade)$(COALte(te_remind))) ) * 1e3/2;
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities (GW)','lig') = sum(te_remind, sum(grade, preInv_remind_cap(yr,reg,te_remind, grade)$(COALte(te_remind))) ) * 1e3/2;
        
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities (GW)','ror') = preInv_remind_cap(yr, reg, 'hydro', '1') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities (GW)','Solar') = preInv_remind_cap(yr, reg, 'spv', '1') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities (GW)','Wind_on') = preInv_remind_cap(yr, reg, 'wind', '1') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND pre-investment capacities (GW)','Wind_off') = preInv_remind_cap(yr, reg, 'windoff', '1') * 1e3;
   
*       MW -GW
        report_tech('REMIND',yr,reg,'REMIND post-investment capacities (GW)',ct_remind) = RM_postInv_cap_con(yr,reg,ct_remind)/ 1e3;
        report_tech('REMIND',yr,reg,'REMIND post-investment capacities (GW)',res) = RM_postInv_cap_res(yr,reg,res)/ 1e3;

***     generation MWh -> TWh
        report_tech('REMIND',yr,reg,'REMIND post-investment generation (TWh)',ct_remind) = RM_postInv_prodSe_con(yr,reg,ct_remind) /1e6;
***     generation from VRE excluding curtailment 
        report_tech('REMIND',yr,reg,'REMIND post-investment generation (TWh)',res) = RM_postInv_prodSe_res_xcurt(yr,reg,res) /1e6;
***     generation from VRE including curtailment   
        report_tech('REMIND',yr,reg,'REMIND post-investment generation including curt (TWh)',res) = RM_postInv_prodSe_res(yr,reg,res) /1e6;



*-------------------------------- reporting for costs, added capacities and capacity factor  --------------------------------------------

*                  ========== report cost ============ REMIND ============
**note: only report when there is (post-investment) capacity in REMIND
**coal
*        report_tech('REMIND',yr,reg,'annualized investment cost ($/MWh)','coal')$(RM_postInv_prodSe_con(yr,reg,'coal') ne 0) = c_i('lig')  *  RM_postInv_cap_con(yr,reg,'coal') / RM_postInv_prodSe_con(yr,reg,'coal');
*        report_tech('REMIND',yr,reg,'O&M cost ($/MWh)','coal')$(RM_postInv_prodSe_con(yr,reg,'coal') ne 0) = cdata('c_fix_con','lig') * RM_postInv_cap_con(yr,reg,'coal') / RM_postInv_prodSe_con(yr,reg,'coal');
$IFTHEN.ACon not %adj_cost% == "off"
*conventional 
        report_tech('REMIND',yr,reg,'annualized investment cost ($/MWh)',ct_remind)$(RM_postInv_prodSe_con(yr,reg,ct_remind) ne 0) = ( sum(DT_RM(ct,ct_remind),(c_i(ct)-c_adj(ct)))) * RM_postInv_cap_con(yr,reg,ct_remind) / RM_postInv_prodSe_con(yr,reg,ct_remind)  * 1.2;
        report_tech('REMIND',yr,reg,'O&M cost ($/MWh)',ct_remind)$(RM_postInv_prodSe_con(yr,reg,ct_remind) ne 0) =( sum(DT_RM(ct,ct_remind),cdata('c_fix_con',ct))) * RM_postInv_cap_con(yr,reg,ct_remind) / RM_postInv_prodSe_con(yr,reg,ct_remind)  * 1.2;
*renewable
        report_tech('REMIND',yr,reg,'annualized investment cost ($/MWh)',res)$(RM_postInv_prodSe_res_xcurt(yr,reg,res) ne 0) = (c_i_res(res)-c_adj_res(res))*  RM_postInv_cap_res(yr,reg,res) / RM_postInv_prodSe_res_xcurt(yr,reg,res)  * 1.2;
        report_tech('REMIND',yr,reg,'O&M cost ($/MWh)',res)$(RM_postInv_prodSe_res_xcurt(yr,reg,res) ne 0) =  rdata('c_fix_res',res) * RM_postInv_cap_res(yr,reg,res) / RM_postInv_prodSe_res_xcurt(yr,reg,res)  * 1.2;
$ENDIF.ACon

$IFTHEN.ACoff %adj_cost% == "off"
*conventional 
        report_tech('REMIND',yr,reg,'annualized investment cost ($/MWh)',ct_remind)$(RM_postInv_prodSe_con(yr,reg,ct_remind) ne 0) = ( sum(DT_RM(ct,ct_remind),c_i(ct))) * RM_postInv_cap_con(yr,reg,ct_remind) / RM_postInv_prodSe_con(yr,reg,ct_remind) * 1.2;
        report_tech('REMIND',yr,reg,'O&M cost ($/MWh)',ct_remind)$(RM_postInv_prodSe_con(yr,reg,ct_remind) ne 0) =( sum(DT_RM(ct,ct_remind),cdata('c_fix_con',ct))) * RM_postInv_cap_con(yr,reg,ct_remind) / RM_postInv_prodSe_con(yr,reg,ct_remind) * 1.2;
*renewable
        report_tech('REMIND',yr,reg,'annualized investment cost ($/MWh)',res)$(RM_postInv_prodSe_res_xcurt(yr,reg,res) ne 0) = c_i_res(res)*  RM_postInv_cap_res(yr,reg,res) / RM_postInv_prodSe_res_xcurt(yr,reg,res)  * 1.2;
        report_tech('REMIND',yr,reg,'O&M cost ($/MWh)',res)$(RM_postInv_prodSe_res_xcurt(yr,reg,res) ne 0) =  rdata('c_fix_res',res) * RM_postInv_cap_res(yr,reg,res) / RM_postInv_prodSe_res_xcurt(yr,reg,res)  * 1.2;
$ENDIF.ACoff

*P2G
        report_tech('REMIND',yr,reg,'annualized investment cost ($/MWh)',p2g)$(totFlexLoad ne 0) = c_i_p2g(p2g)/0.75 * RM_postInv_cap_p2g(yr,reg,p2g) / (RM_postInv_demSe(yr,reg,p2g)*0.75)  * 1.2;
        report_tech('REMIND',yr,reg,'O&M cost ($/MWh)',p2g)$(totFlexLoad ne 0) = p2gdata('c_fix_p2g',p2g)/0.75 * RM_postInv_cap_p2g(yr,reg,p2g) / (RM_postInv_demSe(yr,reg,p2g)*0.75)  * 1.2 ;

        report_tech('REMIND',yr,reg,'fuel cost - divided by eta ($/MWh)',ct_remind)$(RM_postInv_prodSe_con(yr,reg,ct_remind) ne 0) = sum(DT_RM(ct,ct_remind), con_fuelprice_reg_remind_reporting(ct,reg)/cdata('eta_con',ct)) * 1.2;
        report_tech('REMIND',yr,reg,'CO2 cost ($/MWh)',ct_remind)$(RM_postInv_prodSe_con(yr,reg,ct_remind) ne 0) = sum(DT_RM(ct,ct_remind), cdata('carbon_content',ct)/cdata('eta_con',ct) * remind_co2(yr,reg)) * 1.2 ;
         
        report_tech('REMIND',yr,reg,'primary energy price ($/MWh)',ct_remind)$(RM_postInv_prodSe_con(yr,reg,ct_remind) ne 0) = sum(DT_RM(ct,ct_remind), con_fuelprice_reg_remind_reporting(ct,reg)) * 1.2;
        
***     LCOE (for investment c_i 'lig' is the same as 'coal' ) LCOE = (IC+OM) * cap /gen + CO2 + FC
*       IC cost: $/MW, CAP: MW, PRODSE: MWh
***     This is LCOE for REMIND iteration before (so if this outputs to report_DIETER_i5.gdx, it reports REMIND_LCOE of fulldata_5.gdx run, even though fulldata_5 is produced after DIETER gdxes)
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)',ct_remind)$(RM_postInv_prodSe_con(yr,reg,ct_remind) ne 0) = ( sum(DT_RM(ct,ct_remind),c_i(ct)) + sum(DT_RM(ct,ct_remind),cdata('c_fix_con',ct)) )
                                                                                    *  RM_postInv_cap_con(yr,reg,ct_remind) / RM_postInv_prodSe_con(yr,reg,ct_remind)
                                                                                    + sum(DT_RM(ct,ct_remind), con_fuelprice_reg_yr_avg(ct,reg)/cdata('eta_con',ct) + cdata('carbon_content',ct)/cdata('eta_con',ct) * remind_co2(yr,reg))  * 1.2;
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)',res)$(RM_postInv_prodSe_res_xcurt(yr,reg,res) ne 0) = ( c_i_res(res) + rdata('c_fix_res',res) ) *  RM_postInv_cap_res(yr,reg,res) / RM_postInv_prodSe_res_xcurt(yr,reg,res)  * 1.2;
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)',p2g)$(totFlexLoad ne 0) = ( c_i_p2g(p2g) + p2gdata('c_fix_p2g',p2g) ) * RM_postInv_cap_p2g(yr,reg,p2g) / RM_postInv_demSe(yr,reg,p2g)  * 1.2;

*                  ========== divestment and investment capacities ============ REMIND ============
***     TW -> GW
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','lig') = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(COALte(te_remind)) ) * 1e3 ;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','coal') = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(COALte(te_remind)) ) * 1e3 ;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','CCGT') = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(NonPeakGASte(te_remind)) ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','OCGT_eff') = earlyRetiCap_reporting(yr, reg, 'ngt') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','bio') = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(BIOte(te_remind)) ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','nuc') = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(NUCte(te_remind)) ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','ror') = earlyRetiCap_reporting(yr, reg, 'hydro') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','Solar') = earlyRetiCap_reporting(yr, reg, 'spv') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','Wind_on') = earlyRetiCap_reporting(yr, reg, 'wind') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','Wind_off') = earlyRetiCap_reporting(yr, reg, 'windoff') * 1e3;
        
***     TW -> GW; coal is split for easy comparison
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','coal') = sum(te_remind, added_remind_cap(yr, reg, te_remind, '1')$(COALte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','lig') = sum(te_remind, added_remind_cap(yr, reg, te_remind, '1')$(COALte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','CCGT') = sum(te_remind, added_remind_cap(yr, reg, te_remind, '1')$(NonPeakGASte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','OCGT_eff') = added_remind_cap(yr, reg, 'ngt', '1') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','bio') = sum(te_remind, added_remind_cap(yr, reg, te_remind, '1')$(BIOte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','ror') = added_remind_cap(yr, reg, 'hydro', '1') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','nuc') = sum(te_remind, added_remind_cap(yr, reg, te_remind, '1')$(NUCte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','Solar') = added_remind_cap(yr, reg, 'spv', '1') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','Wind_on') = added_remind_cap(yr, reg, 'wind', '1') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','Wind_off') = added_remind_cap(yr, reg, 'windoff', '1') * 1e3;
        
*                  ========== capacity factors ============ REMIND ============
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','coal') = remind_CF(yr,reg,'pc')*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','lig') = remind_CF(yr,reg,'pc')*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','CCGT') = remind_CF(yr,reg,'ngcc')*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','OCGT_eff') = remind_CF(yr,reg,'ngt')*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','bio') = remind_CF(yr,reg,'bioigcc')*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','nuc') = remind_CF(yr,reg,'tnrs')*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','ror') = sum(grade, remind_pm_dataren(reg, 'nur', grade, 'hydro') * remind_vm_CapDistr(yr, reg, 'hydro', grade) / remind_cap(yr, reg, 'hydro', '1'))*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)',res) = remind_VRECapFac(res) * 1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','elh2') = remind_CF(yr,reg,'elh2')*1e2;
        report_tech('REMIND',yr,reg,'REMIND real CapFac (%)','Solar') = remind_realVRECF(yr,reg,"spv");
        report_tech('REMIND',yr,reg,'REMIND real CapFac (%)','Wind_on') = remind_realVRECF(yr,reg,"wind");
        report_tech('REMIND',yr,reg,'REMIND real CapFac (%)','Wind_off') = remind_realVRECF(yr,reg,"windoff");
   
***     ^^^ reporting on remind stuff 
        report_tech('DIETER',yr,reg,'DIETER added capacities (GW)',ct) =  (N_CON.l(ct) - N_CON.lo(ct)) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER added capacities (GW)',res) =  (P_RES.l(res) - P_RES.lo(res)) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER added capacities (GW)','coal') =  (N_CON.l('lig') + N_CON.l('hc') - N_CON.lo('lig') - N_CON.lo('hc')) / 1e3 ;
        
        report_tech('DIETER',yr,reg,'capacities storage (GW)',sto) =  N_STO_P.l(sto) / 1e3 ;
        report_tech('DIETER',yr,reg,'capacities storage (TWh)',sto) =  N_STO_E.l(sto) /1e6;
        report_tech('DIETER',yr,reg,'genshares (%)',ct) = sum( h, G_L.l(ct,h) ) / totLoad  * 1e2;
        report_tech('DIETER',yr,reg,'genshares (%)',res) = sum( h, G_RES.l(res,h) ) / totLoad  * 1e2;
        report_tech('DIETER',yr,reg,'genshares (%)','coal') = sum( h, (G_L.l('hc',h) + G_L.l('lig',h)) ) / totLoad  * 1e2;

        report_tech('REMIND',yr,reg,'genshares (%)','coal') = sum(te_remind, remind_genshare(yr,reg,te_remind)$(COALte(te_remind)));
        report_tech('REMIND',yr,reg,'genshares (%)','lig') = sum(te_remind, remind_genshare(yr,reg,te_remind)$(COALte(te_remind)));
        report_tech('REMIND',yr,reg,'genshares (%)','CCGT') = sum(te_remind, remind_genshare(yr,reg,te_remind)$(NonPeakGASte(te_remind)));
        report_tech('REMIND',yr,reg,'genshares (%)','OCGT_eff') = remind_genshare(yr,reg,'ngt');
        report_tech('REMIND',yr,reg,'genshares (%)','bio') = sum(te_remind, remind_genshare(yr,reg,te_remind)$(BIOte(te_remind)));
        report_tech('REMIND',yr,reg,'genshares (%)','nuc') = sum(te_remind, remind_genshare(yr,reg,'tnrs')$(NUCte(te_remind)));
        report_tech('REMIND',yr,reg,'genshares (%)','ror') = remind_genshare(yr,reg,'hydro');
        report_tech('REMIND',yr,reg,'genshares (%)','Solar') = remind_genshare(yr,reg,'spv');
        report_tech('REMIND',yr,reg,'genshares (%)','Wind_on') = remind_genshare(yr,reg,'wind');
        report_tech('REMIND',yr,reg,'genshares (%)','Wind_off') = remind_genshare(yr,reg,'windoff');
*                
*                  ========== capacity factors and revenues ============ DIETER ============
** capacity factor of average plant in the system 
        report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct)$(N_CON.l(ct) ne 0 ) = sum( h , G_L.l(ct,h)) / (N_CON.l(ct) * card(h)) * 1e2;
* real/post-curtailment and theoretical/pre-curtailment capfac for VRE of average grade     
        report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',res)$(P_RES.l(res) ne 0 ) = sum( h , (G_RES.l(res,h) + CU.l(res,h))) / (P_RES.l(res) * card(h)) * 1e2;
        report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',res)$(P_RES.l(res) ne 0 ) = sum( h , G_RES.l(res,h)) / (P_RES.l(res) * card(h)) * 1e2;
        report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g)$(totFlexLoad ne 0 ) = sum( h , C_P2G.l(p2g,h)) / ( N_P2G.l(p2g) * card(h)) * 1e2;

*** annual average of VRE hourly capacity factor/potential, this should equal to both "REMIND CapFac (%)" and "DIETER avg CapFac (%)"
        report_tech('DIETER',yr,reg,'DIETER annual VRE potential avg (%)',res) = sum( h, phi_res(res, h) ) / card(h) * 1e2;
     
** capacity factor of marginal plant: i.e. only suitable for renewables; i.e. for the one additional plant added, it can only be added in the lower rang of still empty VRE potential grade, determined in REMIND

** deprecated: don't think this way of calculating marginal plant is correct
*        report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res)$(P_RES.l(res) ne 0 )
*          = sum( h$(G_RES.l(res,h) = P_RES.l(res)) , G_RES.l(res,h)) / (P_RES.l(res) * card(h)) * 1e2;        

* real/post-curtailment and theoretical/pre-curtailment capfac for VRE of marginal grade           
        report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',"Wind_on") = report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',"Wind_on") * remind_highest_empty_grade_LF("wind")/remind_average_grade_LF("wind");
        report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',"Wind_off")$(remind_average_grade_LF("windoff")) = report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',"Wind_off") * remind_highest_empty_grade_LF("windoff")/remind_average_grade_LF("windoff");
        report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',"Solar") = report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',"Solar") * remind_highest_empty_grade_LF("spv")/remind_average_grade_LF("spv");
        report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',"ror") = report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',"ror") * remind_highest_empty_grade_LF("hydro")/remind_average_grade_LF("hydro");

        report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',"Wind_on") = remind_VRECapFac("Wind_on")*1e2 * remind_highest_empty_grade_LF("wind")/remind_average_grade_LF("wind");
        report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',"Wind_off")$(remind_average_grade_LF("windoff")) = remind_VRECapFac("Wind_off")*1e2 * remind_highest_empty_grade_LF("windoff")/remind_average_grade_LF("windoff");
        report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',"Solar") = remind_VRECapFac("Solar")*1e2 * remind_highest_empty_grade_LF("spv")/remind_average_grade_LF("spv");
        report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',"ror") = remind_highest_empty_grade_LF("hydro") *1e2;

        report_tech('DIETER',yr,reg,'DIETER Revenue (billionUSD)',ct) = sum( h, G_L.l(ct,h)*(-con1a_bal.m(h)))/1e9  * 1.2;
        report_tech('DIETER',yr,reg,'DIETER Revenue (billionUSD)',res) = sum( h, G_RES.l(res,h)*(-con1a_bal.m(h)))/1e9 * 1.2;
        
        report_tech('DIETER',yr,reg,'DIETER Marginal Revenue (billionUSD)',ct) = sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h)*(-con1a_bal.m(h)) )/1e9 * 1.2;
        report_tech('DIETER',yr,reg,'DIETER Marginal Revenue (billionUSD)',res) = sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h)*(-con1a_bal.m(h)) )/1e9 * 1.2;
        
        report_tech('DIETER',yr,reg,'DIETER Revenue marginal plant (millionUSD)',ct) = sum( h$(G_L.l(ct,h) = N_CON.l(ct) ),(-con1a_bal.m(h)) )/1e6 * 1.2;
        report_tech('DIETER',yr,reg,'DIETER Revenue marginal plant (millionUSD)',res) = sum( h$(G_RES.l(res,h) = P_RES.l(res) ), (-con1a_bal.m(h)) )/1e6 * 1.2;
      
*                  ========== report cost ============ DIETER ============
*       efficiency -> %
        report_tech('DIETER',yr,reg,'fuel efficiency (%)', ct) = cdata('eta_con',ct) * 1e2;
*       investment cost ($/MW -> $/kW)
        report_tech('DIETER',yr,reg,'annualized investment cost ($/kW)',ct) = c_i(ct) /1e3 * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost ($/kW)',res) = c_i_res(res) / 1e3 * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost ($/kW)',p2g) = c_i_p2g(p2g) / 1e3 * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost ($/kW)',grid) = c_i_grid(grid) / 1e3 * 1.2;
* for VRE investment cost here is divided by theoretical capfac (pre-curtailment)

* report IC in DIETER and adjustment costs in both models
$IFTHEN.ACon not %adj_cost% == "off"
** investment cost (avg and marginal) for DIETER: avg and marginal IC for non-VRE are the same, theoretical capfac (pre-curtailment) are used in each cases
** only for plants that are dispatched
        report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',ct)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct) ne 0) = (c_i(ct)-c_adj(ct)) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',res) ne 0) = (c_i_res(res)-c_adj_res(res)) / (card(h) * report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',res)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',p2g)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g) ne 0) = (c_i_p2g(p2g)-c_adj_p2g(p2g)) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g)/1e2) * 1.2 ;   
        report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',grid) = c_i_grid(grid) * N_GRID.L(grid) / totLoad  * 1.2;

        report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',ct) = report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',ct) * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',"ror")$(report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',"ror") ne 0) = (c_i("ror")-c_adj("ror")) / (card(h) * report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',"ror")/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res) ne 0) = (c_i_res(res)-c_adj_res(res)) / (card(h) * report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',p2g) = report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',p2g) * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',grid) = report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',grid) * 1.2;
 
** for plants that exist in the system, regardless whether or not they are dispatched (in $ terms)
        report_tech('DIETER',yr,reg,'total annual investment cost ($)',ct) = (c_i(ct)-c_adj(ct)) * N_CON.l(ct)  * 1.2;
            
** adjustment cost (avg and marginal) for DIETER: avg and marginal AC for non-VRE are the same
        report_tech('DIETER',yr,reg,'annualized adjustment cost - avg ($/MWh)',ct)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct) ne 0) = c_adj(ct) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'annualized adjustment cost - avg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',res) ne 0) = c_adj_res(res) /  (card(h) * report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',res)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'annualized adjustment cost - avg ($/MWh)',p2g)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g) ne 0) = c_adj_p2g(p2g) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g)/1e2)  * 1.2;   
        report_tech('DIETER',yr,reg,'annualized adjustment cost - avg ($/MWh)',grid) = c_adj_grid(grid) * N_GRID.L(grid) / totLoad * 1.2;
        
        report_tech('DIETER',yr,reg,'annualized adjustment cost - marg ($/MWh)',ct) = report_tech('DIETER',yr,reg,'annualized adjustment cost - avg ($/MWh)',ct) * 1.2;
        report_tech('DIETER',yr,reg,'annualized adjustment cost - marg ($/MWh)',"ror")$(report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',"ror") ne 0) = c_adj("ror") / (card(h) * report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',"ror")/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'annualized adjustment cost - marg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res) ne 0) = c_adj_res(res) / (card(h) * report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'annualized adjustment cost - marg ($/MWh)',p2g) = report_tech('DIETER',yr,reg,'annualized adjustment cost - avg ($/MWh)',p2g) * 1.2;
        report_tech('DIETER',yr,reg,'annualized adjustment cost - marg ($/MWh)',grid) = report_tech('DIETER',yr,reg,'annualized adjustment cost - avg ($/MWh)',grid) * 1.2;
        
** adjustment cost (avg) for REMIND (so one sees how much influence this can have on DIETER if techX is coupled): 
        report_tech('REMIND',yr,reg,'annualized adjustment cost - avg ($/MWh)',ct)$(report_tech('REMIND',yr,reg,'REMIND CapFac (%)',ct) ne 0) = c_adj(ct) / (card(h) * report_tech('REMIND',yr,reg,'REMIND CapFac (%)',ct)/1e2) * 1.2;
        report_tech('REMIND',yr,reg,'annualized adjustment cost - avg ($/MWh)',res)$(report_tech('REMIND',yr,reg,'REMIND real CapFac (%)',res) ne 0) = c_adj_res(res) / (card(h) * report_tech('REMIND',yr,reg,'REMIND real CapFac (%)',res)/1e2) * 1.2;
        report_tech('REMIND',yr,reg,'annualized adjustment cost - avg ($/MWh)',p2g)$(report_tech('REMIND',yr,reg,'REMIND CapFac (%)',p2g) ne 0) = c_adj_p2g(p2g) / (card(h) * report_tech('REMIND',yr,reg,'REMIND CapFac (%)',p2g)/1e2) * 1.2;
        report_tech('REMIND',yr,reg,'annualized adjustment cost - avg ($/MWh)',grid) = c_adj_grid(grid) * RM_postInv_cap_grid(yr,reg,grid) / totLoad * 1.2;

** adjustment cost (marg) for REMIND (use DIETER's marg capfac, which should be the same as REMIND marg capfac): 
        report_tech('REMIND',yr,reg,'annualized adjustment cost - marg ($/MWh)',ct) = report_tech('REMIND',yr,reg,'annualized adjustment cost - avg ($/MWh)',ct) * 1.2;
*** DIETER real marg CapFac is the same as REMIND real marg CapFac
        report_tech('REMIND',yr,reg,'annualized adjustment cost - marg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res) ne 0) = c_adj_res(res) / (card(h) * report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res)/1e2) * 1.2;
        report_tech('REMIND',yr,reg,'annualized adjustment cost - marg ($/MWh)',p2g) = report_tech('REMIND',yr,reg,'annualized adjustment cost - avg ($/MWh)',p2g) * 1.2;
        report_tech('REMIND',yr,reg,'annualized adjustment cost - marg ($/MWh)',grid) = report_tech('REMIND',yr,reg,'annualized adjustment cost - avg ($/MWh)',grid) * 1.2;

*       Grid cost (if adj cost for vregrid is coupled)
        report_tech('DIETER',yr,reg,'grid LCOE ($/MWh)',grid) = (griddata("c_fix_grid",grid) +  c_i_grid(grid) ) * N_GRID.L(grid) / totLoad  * 1.2;
                                                        
*only report those adjustment costs that are coupled
$IFTHEN.ACon2 %adj_cost% == "on_select"
        report_tech('DIETER',yr,reg,'annualized adjustment cost - avg ($/MWh)',all_te)$(not adjte_dieter(all_te)) = 0;
        report_tech('DIETER',yr,reg,'annualized adjustment cost - marg ($/MWh)',all_te)$(not adjte_dieter(all_te)) = 0;
        
$ENDIF.ACon2

$ENDIF.ACon

$IFTHEN.ACoff %adj_cost% == "off"
        report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',ct)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct) ne 0) = c_i(ct) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',res) ne 0) = c_i_res(res) / (card(h) * report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',res)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',p2g)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g) ne 0) = c_i_p2g(p2g) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g)/1e2) * 1.2 ;   
        report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',grid) = c_i_grid(grid) * N_GRID.L(grid) / totLoad * 1.2;
        
        report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',ct) = report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',ct) * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res) ne 0) = c_i_res(res) / (card(h) * report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',p2g) = report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',p2g) * 1.2;
        report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',grid) = report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',grid) * 1.2;
 
*       Grid cost (if adj cost for vregrid is not coupled)
        report_tech('DIETER',yr,reg,'grid LCOE ($/MWh)',grid) = (griddata("c_fix_grid",grid) +  c_i_grid(grid)) * N_GRID.L(grid) / totLoad * 1.2;
         
$ENDIF.ACoff
 
*** for simplicity we don't report adj cost separately of grid for "solar" and "wind", it is only report for the tech "grid"
*       Grid cost for each VRE type
        VRE_grid_ratio(yr,reg,"Solar") = report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',"Solar")/
            (report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',"Solar") + 1.5*report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',"Wind_on")
            + 3*report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',"Wind_off"));
        VRE_grid_ratio(yr,reg,"Wind_on") = 1.5* report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',"Wind_on")/
            (report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',"Solar") + 1.5*report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',"Wind_on")
            + 3*report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',"Wind_off"));            
        VRE_grid_ratio(yr,reg,"Wind_off") = 3.0* report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',"Wind_off")/
            (report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',"Solar") + 1.5*report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',"Wind_on")
            + 3*report_tech('DIETER',yr,reg,'Total renewable generation w/ curt (TWh)',"Wind_off"));            

*** note: in remind2/R/reportLCOE.R, grid cost is divided by total usable energy not just renewable generation, because it was calculated as system LCOE for grid, here we calculate the tech LCOE for VRE, then
**  later it can be multiplied with generation share and add up to system LCOE same as other tech LCOE components
        report_tech('DIETER',yr,reg,'grid cost ($/MWh)',"Solar") = sum(grid, (griddata("c_fix_grid",grid) +  c_i_grid("vregrid")) * N_GRID.L(grid) ) / (report_tech('DIETER',yr,reg,'Total generation (TWh)',"Solar")*1e6)
                                                                    * VRE_grid_ratio(yr,reg,"Solar") * dieter_newInvFactor("spv") * 1.2;
        report_tech('DIETER',yr,reg,'grid cost ($/MWh)',"Wind_on") = sum(grid, (griddata("c_fix_grid",grid) +  c_i_grid("vregrid")) * N_GRID.L(grid)) / (report_tech('DIETER',yr,reg,'Total generation (TWh)',"Wind_on")*1e6)
                                                                    * VRE_grid_ratio(yr,reg,"Wind_on") * dieter_newInvFactor("wind") * 1.2;    
        report_tech('DIETER',yr,reg,'grid cost ($/MWh)',"Wind_off")$(report_tech('DIETER',yr,reg,'Total generation (TWh)',"Wind_off")) = sum(grid, (griddata("c_fix_grid",grid) +  c_i_grid("vregrid")) * N_GRID.L(grid)) / (report_tech('DIETER',yr,reg,'Total generation (TWh)',"Wind_off")*1e6)
                                                                    * VRE_grid_ratio(yr,reg,"Wind_off") * dieter_newInvFactor("windoff") * 1.2;    
        
*       OM cost
        report_tech('DIETER',yr,reg,'O&M fixed cost ($/kW)',ct) = cdata('c_fix_con',ct) / 1e3 * 1.2;
        report_tech('DIETER',yr,reg,'O&M fixed cost ($/kW)',res) = rdata('c_fix_res',res) / 1e3 * 1.2;
        report_tech('DIETER',yr,reg,'O&M fixed cost ($/kW)',p2g) = p2gdata('c_fix_p2g',p2g) / 1e3 * 1.2;
        report_tech('DIETER',yr,reg,'O&M fixed cost ($/kW)',grid) = griddata("c_fix_grid",grid) / 1e3 * 1.2;
        
        report_tech('DIETER',yr,reg,'O&M fixed cost - avg ($/MWh)',ct)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct) ne 0) = cdata('c_fix_con',ct)  / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'O&M fixed cost - avg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',res) ne 0) = rdata('c_fix_res',res) / (card(h) * report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',res)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'O&M fixed cost - avg ($/MWh)',p2g)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g) ne 0) = p2gdata('c_fix_p2g',p2g) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g)/1e2)  * 1.2;
        report_tech('DIETER',yr,reg,'O&M fixed cost - avg ($/MWh)',grid) = griddata('c_fix_grid',grid) * N_GRID.L(grid) / totLoad * 1.2;
        
        report_tech('DIETER',yr,reg,'O&M fixed cost - marg ($/MWh)',ct) = report_tech('DIETER',yr,reg,'O&M fixed cost - avg ($/MWh)',ct) * 1.2;
        report_tech('DIETER',yr,reg,'O&M fixed cost - marg ($/MWh)',"ror")$(report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',"ror") ne 0) = cdata('c_fix_con',"ror") / (card(h) * report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',"ror")/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'O&M fixed cost - marg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res) ne 0) = rdata('c_fix_res',res) / (card(h) * report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'O&M fixed cost - marg ($/MWh)',p2g) = report_tech('DIETER',yr,reg,'O&M fixed cost - avg ($/MWh)',p2g) * 1.2;
        report_tech('DIETER',yr,reg,'O&M fixed cost - marg ($/MWh)',grid) = report_tech('DIETER',yr,reg,'O&M fixed cost - avg ($/MWh)',grid) * 1.2;
        
** No OM var cost for vre or vregrid
        report_tech('DIETER',yr,reg,'O&M var cost ($/MWh)',ct)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct) ne 0) = cdata("c_var_con",ct) * 1.2;
        report_tech('DIETER',yr,reg,'O&M var cost ($/MWh)',p2g)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g) ne 0) = p2gdata("c_var_p2g","elh2")  * 1.2;
        
        

*       fuel cost
        report_tech('DIETER',yr,reg,'primary energy price ($/MWh)',ct) = con_fuelprice_reg_yr_avg(ct,reg) * 1.2;
        report_tech('DIETER',yr,reg,'fuel cost - divided by eta ($/MWh)',ct)$(sum(h, G_L.l(ct,h)) ne 0 )  = con_fuelprice_reg_yr_avg(ct,reg)/cdata('eta_con',ct) * 1.2;
*       CO2 cost
        report_tech('DIETER',yr,reg,'CO2 cost ($/MWh)',ct)$(sum(h, G_L.l(ct,h)) ne 0 ) = cdata('carbon_content',ct)/cdata('eta_con',ct) * remind_co2(yr,reg) * 1.2;
        

        report_tech('DIETER',yr,reg,'DIETER LCOE_avg ($/MWh)',ct)$(sum(h, G_L.l(ct,h) ne 0 )) = (report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',ct) + report_tech('DIETER',yr,reg,'O&M fixed cost - avg ($/MWh)',ct) 
                                                                                + con_fuelprice_reg_yr_avg(ct,reg)/cdata('eta_con',ct) + cdata('carbon_content',ct)/cdata('eta_con',ct) * remind_co2(yr,reg) ) * 1.2;
        report_tech('DIETER',yr,reg,'DIETER LCOE_avg ($/MWh)',res)$(sum(h, G_RES.l(res,h) ne 0 )) = (report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',res) + report_tech('DIETER',yr,reg,'O&M fixed cost - avg ($/MWh)',res)) * 1.2 ;

*       convert capacity elh2 to remind unit by multiplying eta
        report_tech('DIETER',yr,reg,'DIETER LCOE_avg ($/MWh)',p2g)$(totFlexLoad ne 0 ) =
            (report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',p2g) + report_tech('DIETER',yr,reg,'O&M fixed cost - marg ($/MWh)',p2g)) * remind_eta2(yr,reg,"elh2") * 1.2;
                                                                                
*       LCOE_marg $/MWh
        report_tech('DIETER',yr,reg,'DIETER LCOE_marg ($/MWh)',ct)$(sum(h, G_L.l(ct,h) ne 0 )) = (report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',ct) + report_tech('DIETER',yr,reg,'O&M fixed cost - marg ($/MWh)',ct) 
                                                                                + con_fuelprice_reg_yr_avg(ct,reg)/cdata('eta_con',ct) + cdata('carbon_content',ct)/cdata('eta_con',ct) * remind_co2(yr,reg)) * 1.2 ;

        report_tech('DIETER',yr,reg,'DIETER LCOE_marg ($/MWh)',res)$(sum(h, G_RES.l(res,h) ne 0 )) = (report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',res) + report_tech('DIETER',yr,reg,'O&M fixed cost - marg ($/MWh)',res) ) * 1.2;


*                  ========== market values and prices ============ DIETER ============
*        report_tech('DIETER',yr,reg,'DIETER Marginal market value ($/MWh)',ct)$(sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h)) ne 0 )
*            = sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h)*(-con1a_bal.m(h))) / sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h));
*        report_tech('DIETER',yr,reg,'DIETER Marginal market value ($/MWh)',res)$(sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h)) ne 0 )
*            = sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h)*(-con1a_bal.m(h)) )/ sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h));

*       shadow price of capacity bound from REMIND, calculated using average capacity factor
        report_tech('DIETER',yr,reg,'shadow price of capacity bound from REMIND - avg ($/MWh)',ct)$(sum(h, G_L.l(ct,h)) ne 0 ) = N_CON.m(ct) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'shadow price of capacity bound from REMIND - avg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',res) ne 0) = P_RES.m(res) / (card(h) * report_tech('DIETER',yr,reg,'DIETER real avg CapFac (%)',res)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'shadow price of capacity bound from REMIND - avg ($/MWh)',grid) = N_GRID.m(grid) * N_GRID.L(grid) / totLoad * 1.2;
        report('DIETER',yr,reg,'total system shadow price of capacity bound - avg ($/MWh)') = sum(all_te, report_tech('DIETER',yr,reg,'shadow price of capacity bound from REMIND - avg ($/MWh)',all_te)* report_tech('DIETER',yr,reg,'genshares (%)',all_te)/1e2);

*       shadow price of capacity bound from REMIND, calculated using marginal capacity factor
        report_tech('DIETER',yr,reg,'shadow price of capacity bound from REMIND - marg ($/MWh)',ct)$(sum(h, G_L.l(ct,h)) ne 0 ) = N_CON.m(ct) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'shadow price of capacity bound from REMIND - marg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res) ne 0) = P_RES.m(res) / (card(h) * report_tech('DIETER',yr,reg,'DIETER real marg CapFac (%)',res)/1e2) * 1.2;
        report_tech('DIETER',yr,reg,'shadow price of capacity bound from REMIND - marg ($/MWh)',grid) = N_GRID.m(grid) * N_GRID.L(grid) / totLoad * 1.2;
        report('DIETER',yr,reg,'total system shadow price of capacity bound - marg ($/MWh)') = sum(all_te, report_tech('DIETER',yr,reg,'shadow price of capacity bound from REMIND - marg ($/MWh)',all_te)* report_tech('DIETER',yr,reg,'genshares (%)',all_te)/1e2);

        report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',ct) = market_value(ct) * 1.2;
        report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',res) = market_value(res) * 1.2;
        report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)','coal') = market_value('coal') * 1.2;
        report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)','elh2') = market_value('elh2') * 1.2;
        report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)','el') = market_value('el') * 1.2;
        
        report_tech('DIETER',yr,reg,'DIETER Market value with scarcity price ($/MWh)',ct) = market_value_wscar(ct) * 1.2;
        report_tech('DIETER',yr,reg,'DIETER Market value with scarcity price ($/MWh)','coal') = market_value_wscar('coal') * 1.2;
        report_tech('DIETER',yr,reg,'DIETER Market value with scarcity price ($/MWh)',res) = market_value_wscar(res) * 1.2;
        

*       if there is generation in non-scarcity hour(s), i.e. market value is non-zero, it is equal to the market value /annual electricity price
        p32_reportmk_4RM(yr,reg,ct,'value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',ct) ne 0) =
            report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',ct) / (annual_load_weighted_price * 1.2);
            
        p32_reportmk_4RM(yr,reg,'coal','value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)','coal') ne 0) =
            report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)','coal') / (annual_load_weighted_price * 1.2);
            
        p32_reportmk_4RM(yr,reg,res,'value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',res) ne 0) = 
            report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',res) / (annual_load_weighted_price * 1.2);
        
*       if there is no generation in non-scarcity hour(s), i.e. market value is zero, the markup is 1 (i.e no tax markup in REMIND) 
        p32_reportmk_4RM(yr,reg,ct,'value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',ct) = 0) = 1;
        p32_reportmk_4RM(yr,reg,'coal','value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)','coal') = 0)  = 1;    
        p32_reportmk_4RM(yr,reg,res,'value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',res) = 0) = 1;

        report_tech('DIETER',yr,reg,'DIETER Value factor (%)',ct) = p32_reportmk_4RM(yr,reg,ct,'value_factor') * 1e2;
        report_tech('DIETER',yr,reg,'DIETER Value factor (%)',res) = p32_reportmk_4RM(yr,reg,res,'value_factor') * 1e2;
        report_tech('DIETER',yr,reg,'DIETER Value factor (%)','coal') = p32_reportmk_4RM(yr,reg,'coal','value_factor') * 1e2;

        report_tech('DIETER',yr,reg,'Storage out total wholesale (TWh)',sto) = sum(h, report_tech_hours('DIETER',yr,reg,'storage generation (MWh)',sto,h) )   /1e6 ;
        report_tech('DIETER',yr,reg,'Storage in total wholesale (TWh)',sto) = sum(h, report_tech_hours('DIETER',yr,reg,'storage loading (MWh)',sto,h) )   /1e6;
