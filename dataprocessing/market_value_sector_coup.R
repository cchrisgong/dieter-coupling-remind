# import library
source("library_import.R")

#####################################################
market_value_SectorCoup_lineplot_mWindShare <- function(techfile){
  # read out necessary data
  # techfile = "~/DIETER/myFirstDIETER/DIETER/results_mWS_exoSS_PtG-ONLY.gdx"
    report_tech_table = read.gdx(techfile,"report_tech", colNames = c("country", "variable", "pv_share", "wind_on_share", 
                                                            "wind_off_share", "technology", "value"), factors = FALSE)
  
    report_table = read.gdx(techfile,"report", colNames = c("country", "variable", "pv_share", "wind_on_share", 
                                                            "wind_off_share", "value"), factors = FALSE)
    
    report_tech_hours_table = read.gdx(techfile,"report_tech_hours", colNames = c("country", "variable", "pv_share", "wind_on_share", 
                                                                    "wind_off_share", "technology", "hour","value"), factors = FALSE)
  
    marketvalues0<- report_tech_table %>% 
    filter(variable == "market value_charge") %>% 
    mutate(variable = str_replace(variable, "market value_charge", "market_value_charge")) %>% 
    filter(technology== "Sto7") %>%  select("country", "variable", "pv_share", "wind_on_share", "technology","value")
    
    StorageIn_annual <- report_tech_table %>% 
      filter(variable == "Storage in total wholesale") %>% 
      mutate(variable = str_replace(variable, "Storage in total wholesale", "stor_in_annual")) %>% 
      filter(technology== "Sto7") %>%  select("country", "pv_share", "wind_on_share","value")%>%
      dplyr::rename(storin = value)
    # mutate(scenarioWS =  paste0("S", pv_share,"W", wind_share))
    
    Demand_annual <- report_table %>% 
      filter(variable == "gross_energy_demand")%>%  
      select("country", "pv_share", "wind_on_share","value") %>% 
      dplyr::rename(demand = value)

    StorIn_share = list(StorageIn_annual, Demand_annual) %>%
      reduce(full_join) %>% 
      mutate(storin = storin / demand) %>% 
      dplyr::rename(storin_share = storin)%>% 
      replace(is.na(.), 0)%>% 
      mutate(wind_on_share = as.numeric(wind_on_share))
    
    marketvalues <- marketvalues0 %>% 
      group_by(country, variable, pv_share, technology ) %>% 
      complete(wind_on_share = paste0(seq(10, 90, by = 10))) %>% 
      replace(is.na(.), 0) %>%
      ungroup(country, variable, pv_share, technology) %>% 
      mutate(wind_on_share = as.numeric(wind_on_share))
    
    StorCap <- report_tech_table %>% 
      filter(variable == "capacities storage MW")%>% 
      filter(technology== "Sto7") %>%  
      select("pv_share", "wind_on_share","value") %>% 
      dplyr::rename(StorCap = value)

    StorageIn_hourly <- report_tech_hours_table %>% 
      filter(variable == "storage loading") %>% 
      filter(technology== "Sto7") %>%  select("pv_share", "wind_on_share","value") %>% 
      dplyr::rename(storin_hrly = value) %>% 
      group_by(pv_share, wind_on_share) %>%
      summarise(storin_hrly = sum(storin_hrly) )%>% 
      ungroup(pv_share, wind_on_share) %>% 
      dplyr::rename(storin_annual = storin_hrly)

    Capacity_Factor = list(StorCap, StorageIn_hourly) %>%
      reduce(full_join) %>% 
      mutate(storin_annual = storin_annual / (StorCap * 8760)) %>% 
      dplyr::rename(cap_factor = storin_annual)%>% 
      replace(is.na(.), 0)%>% 
      mutate(wind_on_share = as.numeric(wind_on_share))
    
    ##########################################################################################
    secAxisScale = 1
    
    p <- ggplot() + geom_line(data = marketvalues, mapping = aes_string(x = "wind_on_share", y = "value"), color = "red", alpha = 0.5, group = 1) +
      geom_point(data=marketvalues, mapping = aes_string(x="wind_on_share", y = "value"), color = "red", group = 1, size = 5) +
    xlim(0, 100)+  ylim(0, 30) + ggtitle(paste0("exo Solar Share = 11,", "varying exo Wind Share"))
  
    # p <- p + geom_line(data=StorIn_share, mapping = aes(x=wind_on_share, y=storin_share * 100 * secAxisScale, group = 2), color = "blue")
    # p <- p + scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = "Storin Share of Total Demand (%) (blue)"))

    p <- p + geom_line(data=Capacity_Factor, mapping = aes(x=wind_on_share, y=cap_factor * 100 * secAxisScale, group = 3), color = "green")+
      geom_point(data=Capacity_Factor, mapping = aes(x=wind_on_share, y=cap_factor * 100 * secAxisScale), color = "green", group = 3, size = 5) 
    p <- p + scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = "Capacity factor (%) (green)"))
    
    # modifying colours and theme options
    p <- p + scale_colour_manual(values = c("green", "red"))
    p <- p + labs(y = "Relative price (Euro/mWh) (red)",
                  x = "Wind Share On Shore",
                  colour = "")
    
  plotfilename = paste0("Marketvalue_line_mWS_exoSS_PtG-ONLY.png")
    
  ggsave(plotfilename, width = 7, height = 5)
  
}

market_value_SectorCoup_lineplot_mSolarShare <- function(techfile){
  # read out necessary data
  techfile = "~/DIETER/myFirstDIETER/DIETER/results_mSS_exoWS_PtG-ONLY.gdx"
  report_tech_table = read.gdx(techfile,"report_tech", colNames = c("model","country", "variable", "pv_share", "wind_on_share", 
                                                                    "wind_off_share", "p2g_share","technology", "value"), factors = FALSE)
  
  report_table = read.gdx(techfile,"report", colNames = c("model","country", "variable", "pv_share", "wind_on_share", 
                                                          "wind_off_share", "p2g_share", "value"), factors = FALSE)
  
  report_tech_hours_table = read.gdx(techfile,"report_tech_hours", colNames = c("model","country", "variable", "pv_share", "wind_on_share", 
                                                                                "wind_off_share", "p2g_share","technology", "hour", "value"), factors = FALSE)
  
  marketvalues0<- report_tech_table %>% 
    filter(variable == "market value_charge") %>% 
    mutate(variable = str_replace(variable, "market value_charge", "market_value_charge")) %>% 
    filter(technology== "Sto7") %>%  select("country", "variable", "pv_share", "wind_on_share", "technology","value")
  
  StorageIn_annual <- report_tech_table %>% 
    filter(variable == "Storage in total wholesale") %>% 
    mutate(variable = str_replace(variable, "Storage in total wholesale", "stor_in_annual")) %>% 
    filter(technology== "Sto7") %>%  select("country", "pv_share", "wind_on_share","value")%>%
    dplyr::rename(storin = value)
  
  Demand_annual <- report_table %>% 
    filter(variable == "gross_energy_demand")%>%  
    select("country", "pv_share", "wind_on_share","value") %>% 
    dplyr::rename(demand = value)
  
  StorIn_share = list(StorageIn_annual, Demand_annual) %>%
    reduce(full_join) %>% 
    mutate(storin = storin / demand) %>% 
    dplyr::rename(storin_share = storin)%>% 
    replace(is.na(.), 0)%>% 
    mutate(pv_share = as.numeric(pv_share))
  
  marketvalues <- marketvalues0 %>% 
    group_by(country, variable, pv_share, technology ) %>% 
    complete(pv_share = paste0(seq(10, 60, by = 10))) %>% 
    replace(is.na(.), 0) %>%
    ungroup(country, variable, pv_share, technology) %>% 
    mutate(pv_share = as.numeric(pv_share))
  
  StorCap <- report_tech_table %>% 
    filter(variable == "capacities storage MW")%>% 
    filter(technology== "Sto7") %>%  
    select("pv_share", "wind_on_share","value") %>% 
    dplyr::rename(StorCap = value)
  
  StorageIn_hourly <- report_tech_hours_table %>% 
    filter(variable == "storage loading") %>% 
    filter(technology== "Sto7") %>%  select("pv_share", "wind_on_share","hour","value") %>% 
    mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
    group_by(pv_share, wind_on_share ) %>% 
    complete(hour = paste0("",1:8760)) %>% 
    replace(is.na(.), 0) %>%
    ungroup(pv_share, wind_on_share) %>% 
    mutate(hour = as.numeric(hour))%>% 
    arrange(hour) %>% 
    dplyr::rename(storin_hrly = value)
  
  StorageIn_annual <- StorageIn_hourly %>% 
    group_by(pv_share, wind_on_share) %>%
    summarise(storin_hrly = sum(storin_hrly) )%>% 
    ungroup(pv_share, wind_on_share) %>% 
    dplyr::rename(storin_annual = storin_hrly)
  
  Capacity_Factor = list(StorCap, StorageIn_hourly) %>%
    reduce(full_join) %>% 
    mutate(storin_annual = storin_annual / (StorCap * 8760)) %>% 
    dplyr::rename(cap_factor = storin_annual) %>% 
    replace(is.na(.), 0) %>% 
    mutate(pv_share = as.numeric(pv_share))
  
  secAxisScale = 1
  
  p <- ggplot() + geom_line(data = marketvalues, mapping = aes_string(x = "pv_share", y = "value"), color = "red", alpha = 0.5, group = 1) +
    geom_point(data=marketvalues, mapping = aes_string(x="pv_share", y = "value"), color = "red", group = 1, size = 5) +
    xlim(0, 60)+  ylim(0, 30) + ggtitle(paste0("exo Wind Share = 11,", "varying exo Solar Share"))
  
  p <- p + geom_line(data=Capacity_Factor, mapping = aes(x=pv_share, y=cap_factor * 100 * secAxisScale, group = 3), color = "green")+
    geom_point(data=Capacity_Factor, mapping = aes(x=pv_share, y=cap_factor * 100 * secAxisScale), color = "green", group = 3, size = 5) 
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./secAxisScale, name = "Capacity factor (%) (green)"))
  
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c("green", "red"))
  p <- p + labs(y = "Relative price (Euro/mWh) (red)",
                x = "Solar PV Share",
                colour = "")
  
  plotfilename = paste0("Marketvalue_line_mSS_exoWS_PtG-ONLY.png")
  ggsave(plotfilename, width = 7, height = 5)
  
  StorageIn_hrlyPV60 <- StorageIn_hourly %>% 
    filter(pv_share == 60)
  
  p1 <- ggplot() + geom_line(data = StorageIn_hrlyPV60, mapping = aes_string(x = "hour", y = "storin_hrly"), color = "black", size = 0.5, alpha = 0.5, group = 1) +
    xlim(0, 8760)
  
  print(p1)
  
  ggsave("storin_hrly.png", width = 20, height = 5)
  
}


market_value_SectorCoup_lineplot_mG2Pshare <- function(techfile){
  # read out necessary data
  # techfile = "~/DIETER/myFirstDIETER/DIETER/results_mP2GS_exoWSSS_PtG-ONLY.gdx"
  results = read.gdx(techfile,"report_tech", colNames = c("country", "variable", "pv_share", "wind_on_share", 
                                                          "wind_off_share", "ptg_share", "technology", "value"), factors = FALSE)
  
  marketvalues0 <- results %>% 
    filter(variable == "market value_charge") %>% 
    mutate(variable = str_replace(variable, "market value_charge", "market_value_charge")) %>% 
    filter(technology== "Sto7") %>%  select("variable", "ptg_share", "value")
  
  marketvalues <- marketvalues0 %>% 
    # group_by(country, variable, wind_on_share, ptg_share, technology ) %>% 
    # # complete(ptg_share = paste0(seq(10, 90, by = 10))) %>% 
    # # replace(is.na(.), 0) %>%
    # ungroup(country, variable, wind_on_share, ptg_share, technology) %>% 
    mutate(ptg_share = as.numeric(ptg_share))
    
  # print(marketvalues$wind_share)
  
  p <- ggplot() + geom_line(data = marketvalues, mapping = aes_string(x = "ptg_share", y = "value"), alpha = 0.5, group = 1) +
    geom_point(data=marketvalues, mapping = aes_string(x="ptg_share", y = "value"), color = "red", group = 2, size = 5) +
    xlim(0, 100)+  ylim(0, 100)+ xlab("P2G Share") + ylab("$/MWh") + 
    ggtitle(paste0("P2G market value (charge in): exoWS = 50, exoSS = 50"))
  
  plotfilename = paste0("Marketvalue_line_mP2GS_exoWSSS_PtG-ONLY.png")
  ggsave(plotfilename, width = 7, height = 5)
  
}

techfile1 = "~/DIETER/myFirstDIETER/DIETER/results_mWS_exoSS_PtG-ONLY.gdx"
techfile2 = "~/DIETER/myFirstDIETER/DIETER/results_mSS_exoWS_PtG-ONLY.gdx"
techfile3 = "~/DIETER/myFirstDIETER/DIETER/results_mP2GS_exoWSSS_PtG-ONLY.gdx"

# market_value_SectorCoup_lineplot_mWindShare(techfile1)
market_value_SectorCoup_lineplot_mSolarShare(techfile2)
# market_value_SectorCoup_lineplot_mG2Pshare(techfile3)




