***!!!! reporting variable name should obey the format ''NAME (UNIT)'. ANYTHING IN THE PARENTHESIS WILL BE TREATED AS UNIT

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
        

        report_hours('DIETER',yr,reg,'hourly wholesale price ($/MWh)',h) = -con1a_bal.m(h);

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
        report('DIETER',yr,reg,'CO2 price ($/tCO2)') = remind_flatco2(yr,reg); 
        report('DIETER',yr,reg,'load-weighted price for fixed demand ($/MWh)') = -sum(h,con1a_bal.m(h)*d(h))/sum(h,d(h)) ;
        report('DIETER',yr,reg,'price w/ scarcity price shaved ($/MWh)') = annual_load_weighted_price_shaved;
        
*       Define gross energy demand for reporting, egual to equation 5a      
        report('DIETER',yr,reg,'gross energy demand (TWh)') = totLoad /1e6;

*       report('DIETER',yr,reg,'curtailment of fluct res absolute') = sum((res,h),CU.l(res,h))  /1e6 ;
*       report('DIETER',yr,reg,'curtailment of fluct res relative')$report('DIETER',yr,reg,'curtailment of fluct res absolute') = sum((res,h),CU.l(res,h))/sum((res,h),G_RES.l(res,h)) ;

***     capacity MW -> GW

        report_tech('DIETER',yr,reg,'DIETER pre-investment capacities (GW)',ct) = N_CON.lo(ct) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER pre-investment capacities (GW)',res) = P_RES.lo(res) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER pre-investment capacities (GW)','coal') = (N_CON.lo('lig') + N_CON.lo('hc'))/ 1e3 ;
        
        report_tech('DIETER',yr,reg,'DIETER post-investment capacities (GW)',ct) =  N_CON.l(ct) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER post-investment capacities (GW)',res) =  P_RES.l(res) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER post-investment capacities (GW)','coal') = (N_CON.l('lig') + N_CON.l('hc'))/ 1e3 ;
        
***     reporting on remind run but only every 5 iterations (can be used as a check to reporting in R or as validation)
        
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
   
*       MW -GW
        report_tech('REMIND',yr,reg,'REMIND post-investment capacities (GW)',ct_remind) = RM_postInv_cap_con(yr,reg,ct_remind)/ 1e3;
        report_tech('REMIND',yr,reg,'REMIND post-investment capacities (GW)',res) = RM_postInv_cap_res(yr,reg,res)/ 1e3;

***     generation MWh -> TWh
        report_tech('REMIND',yr,reg,'REMIND post-investment generation (TWh)',ct_remind) = RM_postInv_prodSe_con(yr,reg,ct_remind) /1e6;
***     generation from VRE excluding curtailment   
        report_tech('REMIND',yr,reg,'REMIND post-investment generation (TWh)',res) = RM_postInv_prodSe_res_xcurt(yr,reg,res) /1e6;


*       ========== report cost ============REMIND============
**coal
*        report_tech('REMIND',yr,reg,'annualized investment cost ($/MWh)','coal')$(RM_postInv_prodSe_con(yr,reg,'coal') ne 0) = c_i('lig')  *  RM_postInv_cap_con(yr,reg,'coal') / RM_postInv_prodSe_con(yr,reg,'coal');
*        report_tech('REMIND',yr,reg,'O&M cost ($/MWh)','coal')$(RM_postInv_prodSe_con(yr,reg,'coal') ne 0) = cdata('c_fix_con','lig') * RM_postInv_cap_con(yr,reg,'coal') / RM_postInv_prodSe_con(yr,reg,'coal');
*conventional 
        report_tech('REMIND',yr,reg,'annualized investment cost ($/MWh)',ct_remind)$(RM_postInv_prodSe_con(yr,reg,ct_remind) ne 0) = ( sum(DT_RM(ct,ct_remind),c_i(ct))) * RM_postInv_cap_con(yr,reg,ct_remind) / RM_postInv_prodSe_con(yr,reg,ct_remind);
        report_tech('REMIND',yr,reg,'O&M cost ($/MWh)',ct_remind)$(RM_postInv_prodSe_con(yr,reg,ct_remind) ne 0) =( sum(DT_RM(ct,ct_remind),cdata('c_fix_con',ct))) * RM_postInv_cap_con(yr,reg,ct_remind) / RM_postInv_prodSe_con(yr,reg,ct_remind);
*renewable
        report_tech('REMIND',yr,reg,'annualized investment cost ($/MWh)',res)$(RM_postInv_prodSe_res_xcurt(yr,reg,res) ne 0) = c_i_res(res)*  RM_postInv_cap_res(yr,reg,res) / RM_postInv_prodSe_res_xcurt(yr,reg,res) ;
        report_tech('REMIND',yr,reg,'O&M cost ($/MWh)',res)$(RM_postInv_prodSe_res_xcurt(yr,reg,res) ne 0) =  rdata('c_fix_res',res) * RM_postInv_cap_res(yr,reg,res) / RM_postInv_prodSe_res_xcurt(yr,reg,res) ;
*P2G
        report_tech('REMIND',yr,reg,'annualized investment cost ($/MWh)',p2g)$(totFlexLoad ne 0) = c_i_p2g(p2g)/0.75 * RM_postInv_cap_p2g(yr,reg,p2g) / (RM_postInv_demSe(yr,reg,p2g)*0.75) ;
        report_tech('REMIND',yr,reg,'O&M cost ($/MWh)',p2g)$(totFlexLoad ne 0) = p2gdata('c_fix_p2g',p2g)/0.75 * RM_postInv_cap_p2g(yr,reg,p2g) / (RM_postInv_demSe(yr,reg,p2g)*0.75)  ;
                                                  
        report_tech('REMIND',yr,reg,'fuel cost - divided by eta ($/MWh)',ct_remind) = sum(DT_RM(ct,ct_remind), con_fuelprice_reg_remind_reporting(ct,reg)/cdata('eta_con',ct));
        report_tech('REMIND',yr,reg,'CO2 cost ($/MWh)',ct_remind) = sum(DT_RM(ct,ct_remind), cdata('carbon_content',ct)/cdata('eta_con',ct) * remind_flatco2(yr,reg)) ;
         
        report_tech('REMIND',yr,reg,'primary energy price ($/MWh)',ct_remind) = sum(DT_RM(ct,ct_remind), con_fuelprice_reg_remind_reporting(ct,reg));
        
***     LCOE (for investment c_i 'lig' is the same as 'coal' ) LCOE = (IC+OM) * cap /gen + CO2 + FC
*       IC cost: $/MW, CAP: MW, PRODSE: MWh
***     This is LCOE for REMIND iteration before (so if this outputs to report_DIETER_i5.gdx, it reports REMIND_LCOE of fulldata_5.gdx run, even though fulldata_5 is produced after DIETER gdxes)
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)',ct_remind)$(RM_postInv_prodSe_con(yr,reg,ct_remind) ne 0) = ( sum(DT_RM(ct,ct_remind),c_i(ct)) + sum(DT_RM(ct,ct_remind),cdata('c_fix_con',ct)) )
                                                                                    *  RM_postInv_cap_con(yr,reg,ct_remind) / RM_postInv_prodSe_con(yr,reg,ct_remind)
                                                                                    + sum(DT_RM(ct,ct_remind), con_fuelprice_reg_yr_avg(ct,reg)/cdata('eta_con',ct) + cdata('carbon_content',ct)/cdata('eta_con',ct) * remind_flatco2(yr,reg)) ;
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)',res)$(RM_postInv_prodSe_res_xcurt(yr,reg,res) ne 0) = ( c_i_res(res) + rdata('c_fix_res',res) ) *  RM_postInv_cap_res(yr,reg,res) / RM_postInv_prodSe_res_xcurt(yr,reg,res) ;
        report_tech('REMIND',yr,reg,'REMIND LCOE ($/MWh)',p2g)$(totFlexLoad ne 0) = ( c_i_p2g(p2g) + p2gdata('c_fix_p2g',p2g) ) * RM_postInv_cap_p2g(yr,reg,p2g) / RM_postInv_demSe(yr,reg,p2g) ;

***     TW -> GW
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','lig') = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(COALte(te_remind)) ) * 1e3 /2 ;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','hc') = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(COALte(te_remind)) ) * 1e3 /2 ;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','coal') = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(COALte(te_remind)) ) * 1e3 ;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','CCGT') = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(NonPeakGASte(te_remind)) ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','OCGT_eff') = earlyRetiCap_reporting(yr, reg, 'ngt') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','bio') = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(BIOte(te_remind)) ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','nuc') = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(NUCte(te_remind)) ) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','ror') = earlyRetiCap_reporting(yr, reg, 'hydro') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','Solar') = earlyRetiCap_reporting(yr, reg, 'spv') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND divestment (GW)','Wind_on') = earlyRetiCap_reporting(yr, reg, 'wind') * 1e3;
        
***     TW -> GW; coal is split for easy comparison
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','coal') = sum(te_remind, added_remind_cap(yr, reg, te_remind, '1')$(COALte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','lig') = sum(te_remind, added_remind_cap(yr, reg, te_remind, '1')$(COALte(te_remind))) * 1e3 /2;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','hc') = sum(te_remind, added_remind_cap(yr, reg, te_remind, '1')$(COALte(te_remind))) * 1e3 /2;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','CCGT') = sum(te_remind, added_remind_cap(yr, reg, te_remind, '1')$(NonPeakGASte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','OCGT_eff') = added_remind_cap(yr, reg, 'ngt', '1') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','bio') = sum(te_remind, added_remind_cap(yr, reg, te_remind, '1')$(BIOte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','ror') = added_remind_cap(yr, reg, 'hydro', '1') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','nuc') = sum(te_remind, added_remind_cap(yr, reg, te_remind, '1')$(NUCte(te_remind))) * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','Solar') = added_remind_cap(yr, reg, 'spv', '1') * 1e3;
        report_tech('REMIND',yr,reg,'REMIND added capacities (GW)','Wind_on') = added_remind_cap(yr, reg, 'wind', '1') * 1e3;
        

        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','coal') = remind_CF(yr,reg,'pc')*1e2;
*        report_tech('REMIND',yr,reg,'REMIND CapFac (%) 2','coal') = RM_postInv_prodSe_con(yr,reg,"coal") /( RM_postInv_cap_con(yr,reg,"coal") *8760) *1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','CCGT') = remind_CF(yr,reg,'ngcc')*1e2;
*        report_tech('REMIND',yr,reg,'REMIND CapFac (%) 2','CCGT') = RM_postInv_prodSe_con(yr,reg,"CCGT") /( RM_postInv_cap_con(yr,reg,"CCGT") *8760) *1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','OCGT_eff') = remind_CF(yr,reg,'ngt')*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','bio') = remind_CF(yr,reg,'bioigcc')*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','nuc') = remind_CF(yr,reg,'tnrs')*1e2;
*        report_tech('REMIND',yr,reg,'REMIND CapFac (%) 2','nuc') = RM_postInv_prodSe_con(yr,reg,"nuc") /( RM_postInv_cap_con(yr,reg,"nuc") *8760) *1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','ror') = sum(grade, remind_pm_dataren(reg, 'nur', grade, 'hydro') * remind_vm_CapDistr(yr, reg, 'hydro', grade) / remind_cap(yr, reg, 'hydro', '1'))*1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','Solar') = sum( h, phi_res('Solar', h) ) / card(h) * 1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','Wind_on') = sum( h, phi_res('Wind_on', h) ) / card(h) * 1e2;
        report_tech('REMIND',yr,reg,'REMIND CapFac (%)','elh2') = remind_CF(yr,reg,'elh2')*1e2;
        

***     ^^^ reporting on remind stuff
 
        report_tech('DIETER',yr,reg,'DIETER added capacities (GW)',ct) =  (N_CON.l(ct) - N_CON.lo(ct)) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER added capacities (GW)',res) =  (P_RES.l(res) - P_RES.lo(res)) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER added capacities (GW)','coal') =  (N_CON.l('lig') + N_CON.l('hc') - N_CON.lo('lig') - N_CON.lo('hc')) / 1e3 ;
        
        report_tech('DIETER',yr,reg,'capacities storage (GW)',sto) =  N_STO_P.l(sto) / 1e3 ;
        report_tech('DIETER',yr,reg,'capacities storage (TWh)',sto) =  N_STO_E.l(sto) /1e6;
        report_tech('DIETER',yr,reg,'genshares (%)',ct) = sum( h, G_L.l(ct,h) ) / totLoad  * 1e2;
        report_tech('DIETER',yr,reg,'genshares (%)',res) = sum( h, G_RES.l(res,h) ) / totLoad  * 1e2;
        report_tech('DIETER',yr,reg,'genshares (%)','coal') = sum( h, (G_L.l('hc',h) + G_L.l('lig',h)) ) / totLoad  * 1e2;
*        report_tech('DIETER',yr,reg,'curtailment of fluct res relative',res) =  sum(h,CU.l(res,h))/ (sum(h,G_RES.l(res,h) - corr_fac_res(res,h) ) + sum(h,CU.l(res,h)) )  * 1e2;
        
*       report_tech('DIETER',yr,reg,'load-weighted price for flex demand', flexTe) = -sum(h,con1a_bal.m(h)*C_P2G.l("elh2",h))/sum(h,C_P2G.l("elh2",h)) ;
        
*       ===================================

        report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct)$(N_CON.l(ct) ne 0 ) = sum( h , G_L.l(ct,h)) / (N_CON.l(ct) * card(h)) * 1e2;
        report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',res)$(P_RES.l(res) ne 0 ) = sum( h , G_RES.l(res,h)) / (P_RES.l(res) * card(h)) * 1e2;
        report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)','elh2')$(totFlexLoad ne 0 ) = sum( h , C_P2G.l("elh2",h)) / ( N_P2G.l("elh2") * card(h)) * 1e2;

        report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',ct)$(N_CON.l(ct) ne 0)
          = sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h)) / (N_CON.l(ct) * card(h)) * 1e2;
        report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',res)$(P_RES.l(res) ne 0 )
          = sum( h$(G_RES.l(res,h) = P_RES.l(res)) , G_RES.l(res,h)) / (P_RES.l(res) * card(h)) * 1e2;

        report_tech('DIETER',yr,reg,'DIETER Revenue (billionUSD)',ct) = sum( h, G_L.l(ct,h)*(-con1a_bal.m(h)))/1e9;
        report_tech('DIETER',yr,reg,'DIETER Revenue (billionUSD)',res) = sum( h, G_RES.l(res,h)*(-con1a_bal.m(h)))/1e9;
        
        report_tech('DIETER',yr,reg,'DIETER Marginal Revenue (billionUSD)',ct) = sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h)*(-con1a_bal.m(h)) )/1e9;
        report_tech('DIETER',yr,reg,'DIETER Marginal Revenue (billionUSD)',res) = sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h)*(-con1a_bal.m(h)) )/1e9;
          
        report_tech('DIETER',yr,reg,'DIETER Revenue marginal plant (millionUSD)',ct) = sum( h$(G_L.l(ct,h) = N_CON.l(ct) ),(-con1a_bal.m(h)) )/1e6;
        report_tech('DIETER',yr,reg,'DIETER Revenue marginal plant (millionUSD)',res) = sum( h$(G_RES.l(res,h) = P_RES.l(res) ), (-con1a_bal.m(h)) )/1e6;
        
*        report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',ct)$(sum(h, G_L.l(ct,h) ne 0 )) = sum( h , G_L.l(ct,h)*(-con1a_bal.m(h)))/sum( h , G_L.l(ct,h));
*        report_tech('DIETER',yr,reg,'DIETER Market value ($/MWh)',res)$(sum(h, G_RES.l(res,h) ne 0 )) = sum( h , G_RES.l(res,h)*(-con1a_bal.m(h)))/sum( h , G_RES.l(res,h) );

*========== report cost ============DIETER============
*       efficiency -> %
        report_tech('DIETER',yr,reg,'fuel efficiency (%)', ct) = cdata('eta_con',ct) * 1e2;
*       investment cost ($/MW -> $/kW)
        report_tech('DIETER',yr,reg,'annualized investment cost ($/kW)',ct) = c_i(ct) /1e3;
        report_tech('DIETER',yr,reg,'annualized investment cost ($/kW)',res) = c_i_res(res) / 1e3;
        report_tech('DIETER',yr,reg,'annualized investment cost ($/kW)',p2g) = c_i_p2g(p2g) / 1e3;
        
        report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',ct)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct) ne 0) = c_i(ct) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct)/1e2);
        report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',res) ne 0) = c_i_res(res) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',res)/1e2);
        report_tech('DIETER',yr,reg,'annualized investment cost - avg ($/MWh)',p2g)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g) ne 0) = c_i_p2g(p2g) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g)/1e2) ;   
       
        report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',ct)$(report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',ct) ne 0) = c_i(ct) / (card(h) * report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',ct)/1e2);
        report_tech('DIETER',yr,reg,'annualized investment cost - marg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',res) ne 0) = c_i_res(res) / (card(h) * report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',res)/1e2);
        report_tech('DIETER',yr,reg,'annualized investment cost - marg($/MWh)',p2g)$(report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',p2g) ne 0) = c_i_p2g(p2g) / (card(h) * report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',p2g)/1e2) ;
        
*       Grid cost
        report_tech('DIETER',yr,reg,'grid cost ($/MWh)',grid) = (griddata("c_fix_grid",grid) +  c_i_grid("vregrid")) * N_GRID.L(grid) / totLoad;
*       Grid cost for each VRE type        
*        report_tech('DIETER',yr,reg,'grid cost ($/MWh)',"Solar") = sum(grid, (griddata("c_fix_grid",grid) +  c_i_grid("vregrid")) * N_GRID.L(grid) ) / totLoad * ( sum(h, G_RES.l("Solar",h) ) /( sum( h, G_RES.l("Solar",h) ) + 1.5 * sum(h, G_RES.l("Wind_on",h))));
*        report_tech('DIETER',yr,reg,'grid cost ($/MWh)',"Wind_on") = sum(grid, (griddata("c_fix_grid",grid) +  c_i_grid("vregrid")) * N_GRID.L(grid)) / totLoad * ( 1.5 * sum(h, G_RES.l("Wind_on",h) ) /( sum( h, G_RES.l("Solar",h) ) + 1.5 * sum(h, G_RES.l("Wind_on",h))));
        report_tech('DIETER',yr,reg,'grid cost ($/MWh)',"Solar") = sum(grid, (griddata("c_fix_grid",grid) +  c_i_grid("vregrid")) * N_GRID.L(grid) ) / (sum(h, G_RES.l("Solar",h) ) ) * (  sum(h, G_RES.l("Solar",h) ) /( sum( h, G_RES.l("Solar",h) ) + 1.5 * sum(h, G_RES.l("Wind_on",h))));
        report_tech('DIETER',yr,reg,'grid cost ($/MWh)',"Wind_on") = sum(grid, (griddata("c_fix_grid",grid) +  c_i_grid("vregrid")) * N_GRID.L(grid)) / sum(h, G_RES.l("Wind_on",h) ) * ( 1.5 * sum(h, G_RES.l("Wind_on",h) ) /( sum( h, G_RES.l("Solar",h) ) + 1.5 * sum(h, G_RES.l("Wind_on",h))));
        
*       OM cost
        report_tech('DIETER',yr,reg,'O&M fixed cost ($/kW)',ct) = cdata('c_fix_con',ct) / 1e3;
        report_tech('DIETER',yr,reg,'O&M fixed cost ($/kW)',res) = rdata('c_fix_res',res) / 1e3;
        report_tech('DIETER',yr,reg,'O&M fixed cost ($/kW)',p2g) = p2gdata('c_fix_p2g',p2g) / 1e3;
        report_tech('DIETER',yr,reg,'O&M fixed cost ($/kW)',grid) = griddata("c_fix_grid",grid) / 1e3;
        
        report_tech('DIETER',yr,reg,'O&M fixed cost - avg ($/MWh)',ct)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct) ne 0) = cdata('c_fix_con',ct)  / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct)/1e2);
        report_tech('DIETER',yr,reg,'O&M fixed cost - avg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',res) ne 0) = rdata('c_fix_res',res) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',res)/1e2);
        report_tech('DIETER',yr,reg,'O&M fixed cost - avg ($/MWh)',p2g)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g) ne 0) = p2gdata('c_fix_p2g',p2g) / (card(h) * report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g)/1e2) ;
        
        report_tech('DIETER',yr,reg,'O&M fixed cost - marg ($/MWh)',ct)$(report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',ct) ne 0) = cdata('c_fix_con',ct)  / (card(h) * report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',ct)/1e2);
        report_tech('DIETER',yr,reg,'O&M fixed cost - marg ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',res) ne 0) = rdata('c_fix_res',res) / (card(h) * report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',res)/1e2);
        report_tech('DIETER',yr,reg,'O&M fixed cost - marg ($/MWh)',p2g)$(report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',p2g) ne 0) = p2gdata('c_fix_p2g',p2g) / (card(h) * report_tech('DIETER',yr,reg,'DIETER marg CapFac (%)',p2g)/1e2) ;
  
        report_tech('DIETER',yr,reg,'O&M var cost ($/MWh)',ct)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',ct) ne 0) = cdata("c_var_con",ct);
        report_tech('DIETER',yr,reg,'O&M var cost ($/MWh)',res)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',res) ne 0) = rdata("c_var_res",res) ;
        

        report_tech('DIETER',yr,reg,'O&M var cost ($/MWh)',p2g)$(report_tech('DIETER',yr,reg,'DIETER avg CapFac (%)',p2g) ne 0) = p2gdata("c_var_p2g","elh2") ;
        
        
*       LCOE_marg $/MWh
        report_tech('DIETER',yr,reg,'DIETER LCOE_marg ($/MWh)',ct)$(sum( h$(G_L.l(ct,h) = N_CON.l(ct) ) , G_L.l(ct,h)) ne 0) = ( c_i(ct) + cdata('c_fix_con',ct) ) *  N_CON.l(ct) / sum( h$(G_L.l(ct,h) = N_CON.l(ct) ) , G_L.l(ct,h) )
                                                                                + con_fuelprice_reg_yr_avg(ct,reg)/cdata('eta_con',ct) + cdata('carbon_content',ct)/cdata('eta_con',ct) * remind_flatco2(yr,reg) ;



*       fuel cost
        report_tech('DIETER',yr,reg,'primary energy price ($/MWh)',ct) = con_fuelprice_reg_yr_avg(ct,reg);
        report_tech('DIETER',yr,reg,'fuel cost - divided by eta ($/MWh)',ct)$(N_CON.l(ct) ne 0 )  = con_fuelprice_reg_yr_avg(ct,reg)/cdata('eta_con',ct);
*       CO2 cost
        report_tech('DIETER',yr,reg,'CO2 cost ($/MWh)',ct) = cdata('carbon_content',ct)/cdata('eta_con',ct) * remind_flatco2(yr,reg);
        

        report_tech('DIETER',yr,reg,'DIETER LCOE_avg ($/MWh)',ct)$(sum(h, G_L.l(ct,h) ne 0 )) = ( c_i(ct) + cdata('c_fix_con',ct) ) *  N_CON.l(ct) / sum( h , G_L.l(ct,h))
                                                                                + con_fuelprice_reg_yr_avg(ct,reg)/cdata('eta_con',ct) + cdata('carbon_content',ct)/cdata('eta_con',ct) * remind_flatco2(yr,reg) ;
        report_tech('DIETER',yr,reg,'DIETER LCOE_avg ($/MWh)',res)$(sum(h, G_RES.l(res,h) ne 0 )) = ( c_i_res(res) + rdata('c_fix_res',res) ) * P_RES.l(res) / sum( h , G_RES.l(res,h)) ;

*       convert capacity elh2 to remind unit by multiplying eta
        report_tech('DIETER',yr,reg,'DIETER LCOE_avg ($/MWh)',p2g)$(totFlexLoad ne 0 ) =  ( c_i_p2g(p2g) + p2gdata('c_fix_p2g',p2g) ) * N_P2G.l(p2g) * remind_eta2(yr,reg,"elh2") / sum( h , C_P2G.l(p2g,h)) ;
                                                                                
*       LCOE_marg $/MWh
        report_tech('DIETER',yr,reg,'DIETER LCOE_marg ($/MWh)',ct)$(sum( h$(G_L.l(ct,h) = N_CON.l(ct) ) , G_L.l(ct,h)) ne 0) = ( c_i(ct) + cdata('c_fix_con',ct) ) *  N_CON.l(ct) / sum( h$(G_L.l(ct,h) = N_CON.l(ct) ) , G_L.l(ct,h) )
                                                                                + con_fuelprice_reg_yr_avg(ct,reg)/cdata('eta_con',ct) + cdata('carbon_content',ct)/cdata('eta_con',ct) * remind_flatco2(yr,reg) ;

        report_tech('DIETER',yr,reg,'DIETER LCOE_marg ($/MWh)',res)$(sum( h$(G_RES.l(res,h) = P_RES.l(res) ) , G_RES.l(res,h)) ne 0) = ( c_i_res(res) + rdata('c_fix_res',res) ) * P_RES.l(res) / sum( h$(G_RES.l(res,h) = P_RES.l(res) ) , G_RES.l(res,h));


*       report_tech('DIETER',yr,reg,'marginal generation',ct)$(sum(h, G_L.l(ct,h) ne 0 )) = sum( h$(G_L.l(ct,h) = N_CON.l(ct)) , G_L.l(ct,h)) /1e6 ;
*       report_tech('DIETER',yr,reg,'marginal generation',res)$(sum(h, G_RES.l(res,h) ne 0 )) = sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h)) /1e6 ;
*        
        report_tech('DIETER',yr,reg,'DIETER Marginal market value ($/MWh)',ct)$(sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h)) ne 0 )
            = sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h)*(-con1a_bal.m(h))) / sum( h$(G_L.l(ct,h) = N_CON.l(ct)), G_L.l(ct,h));
        report_tech('DIETER',yr,reg,'DIETER Marginal market value ($/MWh)',res)$(sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h)) ne 0 )
            = sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h)*(-con1a_bal.m(h)) )/ sum( h$(G_RES.l(res,h) = P_RES.l(res) ), G_RES.l(res,h));

*       if there is generation in non-scarcity hour(s), i.e. market value is non-zero, it is equal to the market value /annual electricity price
        p32_reportmk_4RM(yr,reg,ct,'value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)',ct) ne 0) =
            report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)',ct) / annual_load_weighted_price_shaved;
            
        p32_reportmk_4RM(yr,reg,'coal','value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','coal') ne 0) =
            report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','coal') / annual_load_weighted_price_shaved;
            
        p32_reportmk_4RM(yr,reg,res,'value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)',res) ne 0) = 
            report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)',res) / annual_load_weighted_price_shaved;
        
*       if there is no generation in non-scarcity hour(s), i.e. market value is zero, the markup is 1 (i.e no tax markup in REMIND) 
        p32_reportmk_4RM(yr,reg,ct,'value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)',ct) = 0) = 1;
        p32_reportmk_4RM(yr,reg,'coal','value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)','coal') = 0)  = 1;    
        p32_reportmk_4RM(yr,reg,res,'value_factor')$(report_tech('DIETER',yr,reg,'DIETER Market value w/ scarcity price shaved ($/MWh)',res) = 0) = 1;

        report_tech('DIETER',yr,reg,'DIETER Value factor (%)',ct) = p32_reportmk_4RM(yr,reg,ct,'value_factor') * 1e2;
        report_tech('DIETER',yr,reg,'DIETER Value factor (%)',res) = p32_reportmk_4RM(yr,reg,res,'value_factor') * 1e2;
        report_tech('DIETER',yr,reg,'DIETER Value factor (%)','coal') = p32_reportmk_4RM(yr,reg,'coal','value_factor') * 1e2;


        report_tech('DIETER',yr,reg,'Total Generation (TWh)',ct) = sum( h , G_L.l(ct,h)) /1e6 ;
        report_tech('DIETER',yr,reg,'Total Generation (TWh)',res) = sum( h , G_RES.l(res,h)) /1e6 ;
        report_tech('DIETER',yr,reg,'Total Renewable Curtailment (TWh)',res) = sum( h , CU.l(res,h)) /1e6 ;
        report_tech('DIETER',yr,reg,'Storage out total wholesale (TWh)',sto) = sum(h, report_tech_hours('DIETER',yr,reg,'storage generation (MWh)',sto,h) )   /1e6 ;
        report_tech('DIETER',yr,reg,'Storage in total wholesale (TWh)',sto) = sum(h, report_tech_hours('DIETER',yr,reg,'storage loading (MWh)',sto,h) )   /1e6;
