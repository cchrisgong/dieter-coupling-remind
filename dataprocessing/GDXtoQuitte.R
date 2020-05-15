# get data from config.R file
#file: config.R
require(quitte)
require(gdxrrw)
require(tidyverse)

# IGNORE THE WARNING WHICH APPEARS AFTER EXECUTION OF THE FILE.. 

#set current working directory
#
source("~/DIETER/myFirstDIETER/dataprocessing/config.R")


gdxToQuitte <- function(file){
  out_hourly <- NULL
  out_annual <- NULL
  
  print(file)
  
  ######################################################################################################################## 
  rep_hrs = read.gdx(gdxName = file, requestList = 'report_hours', factors = FALSE)
  names(rep_hrs) <- c("model", "country", "variable", "solar_pv", "wind_on", "wind_off", "p2g", "hour", "value")
  
  out_h <- rep_hrs %>% 
    group_by( model, country, variable, solar_pv, wind_on, wind_off, p2g ) %>%
    complete(hour = paste0("h",1:8760)) %>%
    mutate(hour = hour[order(nchar(hour), hour)]) %>% 
    replace(is.na(.), 0) %>%
    ungroup( model, country, variable, solar_pv, wind_on, wind_off, p2g ) %>% 
    mutate(MODEL = model, SCENARIO = paste0("baseline_PV",solar_pv,"_WO",wind_on,"_WF",wind_off,"_PG",p2g), REGION = country,
           HOUR = hour, TECH = "all Tech") %>% 
    mutate(VARIABLE = as.vector(unlist(variable_dict[variable])), PERIOD = year, STATE=state,
           UNIT = as.vector(unlist(unit_dict[variable])), VALUE = value) %>%
    select(MODEL, SCENARIO, PERIOD, HOUR, STATE, REGION, VARIABLE, TECH, VALUE, UNIT)
  
  ###################################################################
  rep_techHrs = read.gdx(gdxName = file, requestList = 'report_tech_hours', factors = FALSE)
  
  names(rep_techHrs) <- c("model","country","variable", "solar_pv", "wind_on", "wind_off", "p2g", "tech", "hour", "value")
  out_th <- rep_techHrs %>%
  mutate(tech = as.character(tech)) %>%
  group_by(model, variable, country, variable, solar_pv, wind_on, wind_off, p2g, tech)%>%
  complete(hour = paste0("h",1:8760)) %>%
  mutate(hour = hour[order(nchar(hour), hour)]) %>%
  replace(is.na(.), 0) %>%
  ungroup(model, variable, country, variable, solar_pv, wind_on, wind_off, p2g, tech) %>%
  mutate(MODEL = model, SCENARIO = paste0("baseline_PV",solar_pv,"_WO",wind_on,"_WF",wind_off,"_PG",p2g),  REGION = country,
         HOUR = hour,TECH = as.vector(unlist(tech_dict[tech]))) %>%
  mutate(VARIABLE = as.vector(unlist(variable_dict[variable])), PERIOD = year, STATE=state,
         UNIT = as.vector(unlist(unit_dict[variable])), VALUE = value) %>%
  select(MODEL, SCENARIO, PERIOD, HOUR, STATE, REGION, VARIABLE, TECH, VALUE, UNIT)
  
  #################################################################
  
  out_hourly <- rbind(out_hourly, out_h)
  out_hourly <- rbind(out_hourly, out_th)
  
  ###########################################################################################################################
  rep = read.gdx(gdxName = file, requestList = 'report', factors = FALSE)
  
  names(rep) <- c("model","country","variable", "solar_pv", "wind_on", "wind_off", "p2g", "value")
  out <- rep %>% 
    mutate(MODEL = model, SCENARIO = paste0("baseline_PV",solar_pv,"_WO",wind_on,"_WF",wind_off,"_PG",p2g), 
           REGION = country, YEAR = year, VALUE = value, 
           TECH = "all Tech",
           VARIABLE = as.vector(unlist(variable_dict[variable])),
           UNIT = as.vector(unlist(unit_dict[variable])), PERIOD = "annual", STATE = state) %>%
    select(MODEL, SCENARIO, YEAR, STATE, REGION, PERIOD, VARIABLE, TECH, VALUE, UNIT)
  
  #################################################################
  rep_Tech = read.gdx(gdxName = file, requestList = 'report_tech', factors = FALSE)
  
  names(rep_Tech) <- c("model","country","variable", "solar_pv", "wind_on", "wind_off", "p2g", "tech", "value")
  out_t <- rep_Tech %>%
    mutate(tech = as.character(tech)) %>%
    mutate(MODEL = model, SCENARIO = paste0("baseline_PV",solar_pv,"_WO",wind_on,"_WF",wind_off,"_PG",p2g), 
           REGION = country, YEAR = year, VALUE = value, 
           TECH = as.vector(unlist(tech_dict[tech])), 
           VARIABLE = as.vector(unlist(variable_dict[variable])), 
           UNIT = as.vector(unlist(unit_dict[variable])), PERIOD = "annual", STATE = state) %>%
    select(MODEL, SCENARIO, YEAR, STATE, REGION, PERIOD, VARIABLE, TECH, VALUE, UNIT)
  
  #################################################################
  out_annual <- rbind(out_annual, out)
  out_annual <- rbind(out_annual, out_t)
  
  write.table(out_annual, "~/DIETER/myFirstDIETER/dataprocessing/annual_report.csv", sep = ",", row.names = F)
  write.table(out_hourly, "~/DIETER/myFirstDIETER/dataprocessing/hourly_report.csv", sep = ",", row.names = F)
  print(paste0("Tables saved to", "~/DIETER/myFirstDIETER/dataprocessing"))
}



# file = "~/DIETER/myFirstDIETER/DIETER/results_mSS_exoWS_PtG-ONLY.gdx"
# gdxToQuitte(file)
