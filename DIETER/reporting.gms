

* Define gross energy demand for reporting, egual to equation 5a
gross_energy_demand = sum( h , d(h) + sum( sto , STO_IN.l(sto,h) - STO_OUT.l(sto,h) ))
;


        report_tech_hours('DIETER',yr,reg,'generation',ct,h) =  G_L.l(ct,h) + corr_fac_con(ct,h) ;
        report_tech_hours('DIETER',yr,reg,'generation',res,h) = G_RES.l(res,h) ;
        report_tech_hours('DIETER',yr,reg,'curtailment of fluct res',res,h) =  CU.l(res,h) ;
        report_tech_hours('DIETER',yr,reg,'generation storage',sto,h) =  STO_OUT.l(sto,h) ;
        report_tech_hours('DIETER',yr,reg,'storage loading',sto,h) =  STO_IN.l(sto,h) ;
        report_tech_hours('DIETER',yr,reg,'storage level',sto,h) =  STO_L.l(sto,h) ;
        report_hours('DIETER',yr,reg,'fixed demand',h) = d(h) ;
*       report_hours('DIETER',yr,reg,'flexible demand',h) = d2.l(h) ;

        report_hours('DIETER',yr,reg,'price',h) = -con1a_bal.m(h);
        loop(h,
                if(-con1a_bal.m(h) > calc_maxprice,
                calc_maxprice = -con1a_bal.m(h) ;);
                report('DIETER',yr,reg,'max price') = calc_maxprice ;
        );


* Report files: cap. transformed into GW (divide by 1e3), generation transformed into TWh (divide by 1e6), costs to $/kW, share to *100%
   
        report('DIETER',yr,reg,'net_energy_demand') = sum( h , d(h)) /1e6;
        report('DIETER',yr,reg,'model status') = DIETER.modelstat ;
        report('DIETER',yr,reg,'solve time') = DIETER.resUsd ;
*       transform into BillionUSD
        report('DIETER',yr,reg,'obj value') = Z.l /1e9;
        report('DIETER',yr,reg,'CO2 price') = con_CO2price; 
        report('DIETER',yr,reg,'load-weighted price for fixed demand') = -sum(h,con1a_bal.m(h)*d(h))/sum(h,d(h)) ;
        report('DIETER',yr,reg,'gross_energy_demand') = gross_energy_demand /1e6;

*       report('DIETER',yr,reg,'curtailment of fluct res absolute') = sum((res,h),CU.l(res,h))  /1e6 ;
*       report('DIETER',yr,reg,'curtailment of fluct res relative')$report('DIETER',yr,reg,'curtailment of fluct res absolute') = sum((res,h),CU.l(res,h))/( sum((res,h),G_RES.l(res,h) - corr_fac_res(res,h) ) + sum((res,h),CU.l(res,h)) ) ;
*       report('DIETER',yr,reg,'bio not utilized absolute')$(cdata("m_con_e",'bio')) = (cdata("m_con_e",'bio') - sum(h,G_L.l('bio',h)))  ;
*       report('DIETER',yr,reg,'bio not utilized relative')$(cdata("m_con_e",'bio')) = (cdata("m_con_e",'bio') - sum(h,G_L.l('bio',h)))/cdata("m_con_e",'bio') ;

        report('DIETER',yr,reg,'mean price') = -sum(h,con1a_bal.m(h))/card(h) ;        

        report('DIETER',yr,reg,'Capacity total') = (sum( ct , N_CON.l(ct) ) + sum( res , P_RES.l(res) ) + sum( sto , N_STO_P.l(sto) )) /1e3
%DSM%$ontext
        + (sum( dsm_curt , N_DSM_CU.l(dsm_curt) ) + sum( dsm_shift , N_DSM_SHIFT.l(dsm_shift) ))/1e3
$ontext
$offtext
        ;
        
        
        report_tech('DIETER',yr,reg,'net shares',res) = sum( h , G_RES.l(res,h))/sum(h,d(h)) * 1e2;
        report_tech('DIETER',yr,reg,'capacities',ct) =  N_CON.l(ct) / 1e3 ;
        report_tech('DIETER',yr,reg,'capacities',res) =  P_RES.l(res) / 1e3 ;
        
        report_tech('DIETER',yr,reg,'DIETER pre-investment capacities',ct) = N_CON.lo(ct) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER pre-investment capacities',res) = P_RES.lo(res) / 1e3 ;
        
        report_tech('DIETER',yr,reg,'REMIND pre-investment capacities',"lig") = N_CON.lo("lig") / 1e3 ;
        report_tech('DIETER',yr,reg,'REMIND pre-investment capacities',"hc") = N_CON.lo("hc") / 1e3 ;
        report_tech('DIETER',yr,reg,'REMIND pre-investment capacities',"CCGT") = N_CON.lo("CCGT") / 1e3 ;
        report_tech('DIETER',yr,reg,'REMIND pre-investment capacities',"OCGT_eff") = N_CON.lo("OCGT_eff") / 1e3 ;
        report_tech('DIETER',yr,reg,'REMIND pre-investment capacities',"bio") = N_CON.lo("bio") / 1e3 ;
        report_tech('DIETER',yr,reg,'REMIND pre-investment capacities',"nuc") = N_CON.lo("nuc") / 1e3 ;
        report_tech('DIETER',yr,reg,'REMIND pre-investment capacities',"ror") = preInv_remind_cap(yr, "DEU", "hydro", "1") * 1e3;
        report_tech('DIETER',yr,reg,'REMIND pre-investment capacities',"Solar") = preInv_remind_cap(yr, "DEU", "spv", "1") * 1e3;
        report_tech('DIETER',yr,reg,'REMIND pre-investment capacities',"Wind_on") = preInv_remind_cap(yr, "DEU", "wind", "1") * 1e3;
        
***       reporting on remind stuff, not result of DIETER run      (GW)  
        
        report_tech('DIETER',yr,reg,'REMIND divestment',"lig") = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(COALte(te_remind)) ) * 1e3 /2 ;
        report_tech('DIETER',yr,reg,'REMIND divestment',"hc") = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(COALte(te_remind)) ) * 1e3 /2 ;
        report_tech('DIETER',yr,reg,'REMIND divestment',"CCGT") = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(NonPeakGASte(te_remind)) ) * 1e3;
        report_tech('DIETER',yr,reg,'REMIND divestment',"OCGT_eff") = earlyRetiCap_reporting(yr, reg, "ngt") * 1e3;
        report_tech('DIETER',yr,reg,'REMIND divestment',"bio") = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(BIOte(te_remind)) ) * 1e3;
        report_tech('DIETER',yr,reg,'REMIND divestment',"ror") = earlyRetiCap_reporting(yr, reg, "hydro") * 1e3;
        report_tech('DIETER',yr,reg,'REMIND divestment',"nuc") = sum(te_remind, earlyRetiCap_reporting(yr, reg, te_remind)$(NUCte(te_remind)) ) * 1e3;
        report_tech('DIETER',yr,reg,'REMIND divestment',"Solar") = earlyRetiCap_reporting(yr, reg, "spv") * 1e3;
        report_tech('DIETER',yr,reg,'REMIND divestment',"Wind_on") = earlyRetiCap_reporting(yr, reg, "wind") * 1e3;
        
        report_tech('DIETER',yr,reg,'REMIND added capacities from DIETER pre-investment',"lig") = sum(te_remind, remind_cap(yr, reg, te_remind, "1")$(COALte(te_remind)) ) * 1e3 /2 - N_CON.lo("lig")/ 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from DIETER pre-investment',"hc") = sum(te_remind, remind_cap(yr, reg, te_remind, "1")$(COALte(te_remind)) ) * 1e3 /2 - N_CON.lo("hc")/ 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from DIETER pre-investment',"CCGT") = sum(te_remind, remind_cap(yr, reg, te_remind, "1")$(NonPeakGASte(te_remind)) ) * 1e3 - N_CON.lo("CCGT")/ 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from DIETER pre-investment',"OCGT_eff") = remind_cap(yr, reg, "ngt", "1") * 1e3 - N_CON.lo("OCGT_eff")/ 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from DIETER pre-investment',"bio") = sum(te_remind, remind_cap(yr, reg, te_remind, "1")$(BIOte(te_remind)) ) * 1e3 - N_CON.lo("bio")/ 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from DIETER pre-investment',"ror") = remind_cap(yr, reg, "hydro", "1") * 1e3 - N_CON.lo("ror")/ 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from DIETER pre-investment',"nuc") = sum(te_remind, remind_cap(yr, reg, te_remind, "1")$(NUCte(te_remind)) ) * 1e3 - N_CON.lo("nuc")/ 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from DIETER pre-investment',"Solar") = remind_cap(yr, reg, "spv", "1") * 1e3 - P_RES.lo("Solar")/ 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from DIETER pre-investment',"Wind_on") = remind_cap(yr, reg, "wind", "1") * 1e3 - P_RES.lo("Wind_on")/ 1e3;
        
        report_tech('DIETER',yr,reg,'REMIND added capacities from REMIND pre-investment',"lig") = sum(te_remind, added_remind_cap(yr, "DEU", te_remind, "1")$(COALte(te_remind))) * 1e3 /2;
        report_tech('DIETER',yr,reg,'REMIND added capacities from REMIND pre-investment',"hc") = sum(te_remind, added_remind_cap(yr, "DEU", te_remind, "1")$(COALte(te_remind))) * 1e3 /2;
        report_tech('DIETER',yr,reg,'REMIND added capacities from REMIND pre-investment',"CCGT") = sum(te_remind, added_remind_cap(yr, "DEU", te_remind, "1")$(NonPeakGASte(te_remind))) * 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from REMIND pre-investment',"OCGT_eff") = added_remind_cap(yr, "DEU", "ngt", "1") * 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from REMIND pre-investment',"bio") = sum(te_remind, added_remind_cap(yr, "DEU", te_remind, "1")$(BIOte(te_remind))) * 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from REMIND pre-investment',"ror") = added_remind_cap(yr, "DEU", "hydro", "1") * 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from REMIND pre-investment',"nuc") = sum(te_remind, added_remind_cap(yr, "DEU", te_remind, "1")$(NUCte(te_remind))) * 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from REMIND pre-investment',"Solar") = added_remind_cap(yr, "DEU", "spv", "1") * 1e3;
        report_tech('DIETER',yr,reg,'REMIND added capacities from REMIND pre-investment',"Wind_on") = added_remind_cap(yr, "DEU", "wind", "1") * 1e3;
        
***     reporting on remind stuff
 
        report_tech('DIETER',yr,reg,'DIETER added capacities',ct) =  (N_CON.l(ct) - N_CON.lo(ct)) / 1e3 ;
        report_tech('DIETER',yr,reg,'DIETER added capacities',res) =  (P_RES.l(res) - P_RES.lo(res)) / 1e3 ;
        
        report_tech('DIETER',yr,reg,'capacities storage GW',sto) =  N_STO_P.l(sto) / 1e3 ;
        report_tech('DIETER',yr,reg,'capacities storage TWh',sto) =  N_STO_E.l(sto) /1e6;
        report_tech('DIETER',yr,reg,'conshares',ct) = sum( h, G_L.l(ct,h) ) / gross_energy_demand  * 1e2;
        report_tech('DIETER',yr,reg,'renshares',res) = sum( h, G_RES.l(res,h) - corr_fac_res(res,h))/ gross_energy_demand  * 1e2;
        report_tech('DIETER',yr,reg,'curtailment of fluct res relative',res) =  sum(h,CU.l(res,h))/ (sum(h,G_RES.l(res,h) - corr_fac_res(res,h) ) + sum(h,CU.l(res,h)) )  * 1e2;
        report_tech('DIETER',yr,reg,'fuel efficiency', ct) = cdata("eta_con",ct) * 1e2;
        
*       report_tech('DIETER',yr,reg,'load-weighted price for flex demand', flexTe) = -sum(h,con1a_bal.m(h)*d2.l(h))/sum(h,d2.l(h)) ;
*       ========== report cost ============
*       investment cost ($/MW -> $/kW)
        report_tech('DIETER',yr,reg,'annualized investment cost',ct) = c_i(ct) /1e3;
        report_tech('DIETER',yr,reg,'annualized investment cost',res) = c_i_res(res) / 1e3;
*       OM cost
        report_tech('DIETER',yr,reg,'O&M cost',ct) = cdata("c_fix_con",ct) / 1e3;
        report_tech('DIETER',yr,reg,'O&M cost',res) = rdata("c_fix_res",res) / 1e3;
*       fuel cost
        report_tech('DIETER',yr,reg,'primary energy price',ct) = con_fuelprice_reg(ct,reg);
        report_tech('DIETER',yr,reg,'fuel cost (divided by eta)',ct) = con_fuelprice_reg(ct,reg)/cdata("eta_con",ct) ;
*       CO2 cost
        report_tech('DIETER',yr,reg,'CO2 cost',ct) = cdata("carbon_content",ct)/cdata("eta_con",ct) * con_CO2price ;

*       ===================================
    Parameter max_ctGen(ct) ;
    Parameter max_resGen(res) ;
    
    max_ctGen(ct) = SMax(h, G_L.l(ct,h));
    max_resGen(res) =SMax(h, G_RES.l(res,h));

        report_tech('DIETER',yr,reg,'avg capacity factor',ct)$(N_CON.l(ct) ne 0 ) = sum( h , G_L.l(ct,h)) / (N_CON.l(ct) * card(h)) * 1e2;
        report_tech('DIETER',yr,reg,'avg capacity factor',res)$(P_RES.l(res) ne 0 ) = sum( h , G_RES.l(res,h)) / (P_RES.l(res) * card(h)) * 1e2;

        report_tech('DIETER',yr,reg,'full load capacity factor',ct)$(N_CON.l(ct) ne 0)
          = sum( h$(G_L.l(ct,h) = max_ctGen(ct)), G_L.l(ct,h)) / (N_CON.l(ct) * card(h)) * 1e2;
        report_tech('DIETER',yr,reg,'full load capacity factor',res)$(P_RES.l(res) ne 0 )
          = sum( h$(G_RES.l(res,h) = max_resGen(res)) , G_RES.l(res,h)) / (P_RES.l(res) * card(h)) * 1e2;

        report_tech('DIETER',yr,reg,'Revenue',ct) = sum( h, G_L.l(ct,h)*(-con1a_bal.m(h)))/1e9;
        report_tech('DIETER',yr,reg,'Revenue',res) = sum( h, G_RES.l(res,h)*(-con1a_bal.m(h)))/1e9;
        
        report_tech('DIETER',yr,reg,'Revenue full load',ct) = sum( h$(G_L.l(ct,h) = max_ctGen(ct)), G_L.l(ct,h)*(-con1a_bal.m(h)) )/1e9;
        report_tech('DIETER',yr,reg,'Revenue full load',res) = sum( h$(G_RES.l(res,h) = max_resGen(res) ), G_RES.l(res,h)*(-con1a_bal.m(h)) )/1e9;
          
        report_tech('DIETER',yr,reg,'Revenue marginal plant per MW',ct) = sum( h$(G_L.l(ct,h) = max_ctGen(ct) ),(-con1a_bal.m(h)) );
        report_tech('DIETER',yr,reg,'Revenue marginal plant per MW',res) = sum( h$(G_RES.l(res,h) = max_resGen(res) ), (-con1a_bal.m(h)) );
        
        report_tech('DIETER',yr,reg,'market value',ct)$(sum(h, G_L.l(ct,h) ne 0 )) = sum( h , G_L.l(ct,h)*(-con1a_bal.m(h)))/sum( h , G_L.l(ct,h));
        report_tech('DIETER',yr,reg,'market value',res)$(sum(h, G_RES.l(res,h) ne 0 )) = sum( h , G_RES.l(res,h)*(-con1a_bal.m(h)))/sum( h , G_RES.l(res,h) );
        
        report_tech('DIETER',yr,reg,'market value full load',ct)$(sum(h, G_L.l(ct,h) ne 0 ))
            = sum( h$(G_L.l(ct,h) = max_ctGen(ct)), G_L.l(ct,h)*(-con1a_bal.m(h))) / sum( h$(G_L.l(ct,h) = max_ctGen(ct)), G_L.l(ct,h));
        report_tech('DIETER',yr,reg,'market value full load',res)$(sum(h, G_RES.l(res,h) ne 0 ))
            = sum( h$(G_RES.l(res,h) = max_resGen(res) ), G_RES.l(res,h)*(-con1a_bal.m(h)) )/ sum( h$(G_RES.l(res,h) = max_resGen(res) ), G_RES.l(res,h));
        
        report_tech('DIETER',yr,reg,'Total Generation',ct) = sum( h , G_L.l(ct,h)) /1e6 ;
        report_tech('DIETER',yr,reg,'Total Renewable Generation',res) = sum( h , G_RES.l(res,h)) /1e6 ;
        report_tech('DIETER',yr,reg,'Total Renewable Curtailment',res) = sum( h , CU.l(res,h)) /1e6 ;
        report_tech('DIETER',yr,reg,'Storage out total wholesale',sto) = sum(h, report_tech_hours('DIETER',yr,reg,'generation storage',sto,h) )   /1e6 ;
        report_tech('DIETER',yr,reg,'Storage in total wholesale',sto) = sum(h, report_tech_hours('DIETER',yr,reg,'storage loading',sto,h) )   /1e6;
