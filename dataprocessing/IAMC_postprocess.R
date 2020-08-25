# import library
require(quitte)
require(gdxrrw)
require(tidyverse)
source("config.R")

files_in_loc <- list.files("~/DIETER/myFirstDIETER/DIETER")

out_hourly <- NULL
out_annual0 <- NULL

#####################################################
IAMC_rewrite <- function(techfile){
  # techfile = "~/DIETER/myFirstDIETER/DIETER/results_mWS_endSS.gdx"
  
  # IAMC data format from Karthik:  ("country","variable", "solar_pv", "wind_on", "wind_off", "value")
  # read out necessary data (# em_share = emission share, now obsolete in Murtaza's DIETER version 20.4.2020)
  rep = read.gdx(techfile,"report_hours", colNames = c("country","variable","pv_share", "wind_share","em_share","hour","value"), factor = FALSE)
  
  # complete the table (zero entries which were not read by read.gdx is now filled in)
  out_h <- rep %>% 
    select(country, variable, pv_share, wind_share, hour, value) %>% 
    group_by(country, variable, pv_share, wind_share ) %>% 
    complete(hour = paste0("h",1:8760)) %>% 
    replace(is.na(.), 0) %>%
    ungroup(country, variable, pv_share, wind_share) %>% 
    # mutate(MODEL = model, SCENARIO = paste0("baseline_",solar_pv,"_",wind_on,"_",wind_off,"_",all_tech,"_",co2_price,"_",system_type), 
    #        REGION = country) %>%
    mutate(MODEL = model, SCENARIO = paste0("baseline_",pv_share,"_",wind_share), YEAR = 2013, 
         REGION = country, PERIOD = hour, VARIABLE = variable, TECHNOLOGY = "all", VALUE = value) %>% 
    mutate(VARIABLE = paste0(as.vector(unlist(variable_dict[variable])), "|UNDEFINED"),
           UNIT = as.vector(unlist(unit_dict[variable]))) %>%
    select(MODEL, SCENARIO, YEAR, REGION, PERIOD, VARIABLE, TECHNOLOGY, VALUE, UNIT) 
  
    #################################################################
    out_annual <- rbind(out_annual0, out_h)
    
    write.table(out_annual, "annual_report.csv", sep = ",", row.names = F)
  
  # rep = read.gdx(gdxName = paste0(file_loc, file), requestList = 'report_tech_hours', factors = FALSE)
  # 
  # names(rep) <- c("country","variable", "solar_pv", "wind_on", "wind_off", "tech", "hour", "value")
  # out_th <- rep %>%
  #   select(variable, hour, tech, value, solar_pv, wind_on, wind_off, country) %>%
  #   mutate(tech = as.character(tech)) 
  # %>%
  #   group_by(variable, solar_pv, wind_on, wind_off, country, tech) %>%
  #   complete(hour = paste0("h",1:8760)) 
  # %>%
  # replace(is.na(.), 0) %>%
  # spread(key = hour, value = value) %>%
  # ungroup(tech, solar_pv, wind_on, wind_off, country) %>%
  # mutate(MODEL = model, SCENARIO = paste0("baseline_",solar_pv,"_",wind_on,"_",wind_off,"_",all_tech,"_",co2_price,"_",system_type), 
  #        REGION = country, 
  #        TECH = as.vector(unlist(tech_dict[tech]))) %>%
  # ungroup(variable) %>% 
  # mutate(VARIABLE = paste0(as.vector(unlist(variable_dict[variable])),"|", TECH), 
  #        UNIT = as.vector(unlist(unit_dict[variable]))) %>%
  # select(MODEL, SCENARIO, REGION, VARIABLE, UNIT, paste0("h",1:8760))
  
  #################################################################
  # out_hourly <- rbind(out_hourly, out_th)
  # out_hourly <- rbind(out_hourly, out_h)
  # 
  # ###########################################################################################################################
  # results = read.gdx(techfile,"report_tech", colNames = c("country","variable","pv_share", "wind_share","em_share",
  #                                                         "technology","value"), factors = FALSE)
  # 
  # names(rep) <- c("country","variable", "solar_pv", "wind_on", "wind_off", "value")
  # out <- rep %>%
  #   mutate(MODEL = model, SCENARIO = paste0("baseline_",solar_pv,"_",wind_on,"_",wind_off,"_",all_tech,"_",co2_price,"_",system_type), 
  #          REGION = country, YEAR = year, VALUE = value, 
  #          TECH = "UNDEFINED", VARIABLE = paste0(as.vector(unlist(variable_dict[variable])),"|", TECH), 
  #          UNIT = as.vector(unlist(unit_dict[variable]))) %>%
  #   select(VARIABLE, MODEL, SCENARIO, REGION, UNIT, YEAR, VALUE)
  # 
  # #################################################################
  # rep = read.gdx(gdxName = paste0(file_loc, file), requestList = 'report_tech', factors = FALSE)
  # 
  # names(rep) <- c("country","variable", "solar_pv", "wind_on", "wind_off", "tech", "value")
  # out_t <- rep %>%
  #   mutate(MODEL = model, SCENARIO = paste0("baseline_",solar_pv,"_",wind_on,"_",wind_off,"_",all_tech,"_",co2_price,"_",system_type), 
  #          REGION = country, YEAR = year, VALUE = value, 
  #          TECH = as.vector(unlist(tech_dict[tech])), VARIABLE = paste0(as.vector(unlist(variable_dict[variable])),"|", TECH), 
  #          UNIT = as.vector(unlist(unit_dict[variable]))) %>%
  #   select(VARIABLE, MODEL, SCENARIO, REGION, UNIT, YEAR, VALUE)
  
  
}


EEtype_total = c("exo", "end") # endogenous or exogenous
VREtype_total = c("SS", "WS")
VREtype_NAME_total = c("Solar", "Wind")
EEtype_NAME_total = c("Exogenous", "Endogenous")
xname = c("pv_share", "wind_share")

for (i in c(0,1)){
  print(i+1)
  print((i+1)%%2+1)
  print(xname[i+1])
  print(VREtype_total[i+1])
  for (j in c(0,1)){
        #plot for one varying wind or solar share
        techfile = paste0("~/DIETER/myFirstDIETER/DIETER/results_m", VREtype_total[i+1], "_", EEtype_total[j+1], VREtype_total[(i+1)%%2+1], ".gdx")
        print(techfile)
        IAMC_rewrite(techfile)
    }
}


