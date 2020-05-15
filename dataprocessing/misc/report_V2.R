# get data from config.R file
#file: config.R
require(quitte)
require(gdxrrw)
require(tidyverse)

# IGNORE THE WARNING WHICH APPEARS AFTER EXECUTION OF THE FILE.. 

#set current working directory
#
setwd("/home/chengong/DIETER/myFirstDIETER/dataprocessing/")

source("config.R")
files_in_loc <- list.files(file_loc)

out_hourly <- NULL
out_annual <- NULL

for(file in files_in_loc){
  print(file)
  
  file_name = gsub(".gdx","", file)
  co2_price = paste0(scenario_desc$CO2.price[which(scenario_desc$Scenario.No. == file_name)],"CO2")
  all_tech = scenario_desc$Alltech[which(scenario_desc$Scenario.No. == file_name)]
  system_type = scenario_desc$Greenfield.brownfield[which(scenario_desc$Scenario.No. == file_name)]
  
  ######################################################################################################################## 
  rep = read.gdx(gdxName = paste0(file_loc, file), requestList = 'report_hours', factors = FALSE)
  
  names(rep) <- c("country", "variable", "solar_pv", "wind_on", "wind_off", "hour", "value")
  
  out_h <- rep %>% 
    select(variable, solar_pv, wind_on, wind_off, hour, value, country) %>%
    group_by(variable, solar_pv, wind_on, wind_off, country) %>%
    complete(hour = paste0("h",1:8760)) %>%
    replace(is.na(.), 0) %>%
    spread(key = hour, value = value) %>%
    ungroup(variable, solar_pv, wind_on, wind_off, country) %>%
    mutate(MODEL = model, SCENARIO = paste0("baseline_",solar_pv,"_",wind_on,"_",wind_off,"_",all_tech,"_",co2_price,"_",system_type), 
           REGION = country) %>%
    mutate(VARIABLE = paste0(as.vector(unlist(variable_dict[variable])), "|UNDEFINED"), 
           UNIT = as.vector(unlist(unit_dict[variable]))) %>%
    select(MODEL, SCENARIO, REGION, VARIABLE, UNIT, paste0("h",1:8760))
  
  ###################################################################
  rep = read.gdx(gdxName = paste0(file_loc, file), requestList = 'report_tech_hours', factors = FALSE)
  
  names(rep) <- c("country","variable", "solar_pv", "wind_on", "wind_off", "tech", "hour", "value")
  out_th <- rep %>%
    select(variable, hour, tech, value, solar_pv, wind_on, wind_off, country) %>%
    mutate(tech = as.character(tech))   %>%
    group_by(variable, solar_pv, wind_on, wind_off, country, tech) %>%
    complete(hour = paste0("h",1:8760))
  %>%
  replace(is.na(.), 0) %>%
  spread(key = hour, value = value) %>%
  ungroup(tech, solar_pv, wind_on, wind_off, country) %>%
  mutate(MODEL = model, SCENARIO = paste0("baseline_",solar_pv,"_",wind_on,"_",wind_off,"_",all_tech,"_",co2_price,"_",system_type),
         REGION = country,
         TECH = as.vector(unlist(tech_dict[tech]))) %>%
  ungroup(variable) %>%
  mutate(VARIABLE = paste0(as.vector(unlist(variable_dict[variable])),"|", TECH),
         UNIT = as.vector(unlist(unit_dict[variable]))) %>%
  select(MODEL, SCENARIO, REGION, VARIABLE, UNIT, paste0("h",1:8760))
  
  #################################################################
  out_hourly <- rbind(out_hourly, out_th)
  out_hourly <- rbind(out_hourly, out_h)
  
  ###########################################################################################################################
  rep = read.gdx(gdxName = paste0(file_loc, file), requestList = 'report', factors = FALSE)
  
  names(rep) <- c("country","variable", "solar_pv", "wind_on", "wind_off", "value")
  out <- rep %>%
    mutate(MODEL = model, SCENARIO = paste0("baseline_",solar_pv,"_",wind_on,"_",wind_off,"_",all_tech,"_",co2_price,"_",system_type), 
           REGION = country, YEAR = year, VALUE = value, 
           TECH = "UNDEFINED", VARIABLE = paste0(as.vector(unlist(variable_dict[variable])),"|", TECH), 
           UNIT = as.vector(unlist(unit_dict[variable]))) %>%
    select(VARIABLE, MODEL, SCENARIO, REGION, UNIT, YEAR, VALUE)
  
  #################################################################
  rep = read.gdx(gdxName = paste0(file_loc, file), requestList = 'report_tech', factors = FALSE)
  
  names(rep) <- c("country","variable", "solar_pv", "wind_on", "wind_off", "tech", "value")
  out_t <- rep %>%
    mutate(MODEL = model, SCENARIO = paste0("baseline_",solar_pv,"_",wind_on,"_",wind_off,"_",all_tech,"_",co2_price,"_",system_type), 
           REGION = country, YEAR = year, VALUE = value, 
           TECH = as.vector(unlist(tech_dict[tech])), VARIABLE = paste0(as.vector(unlist(variable_dict[variable])),"|", TECH), 
           UNIT = as.vector(unlist(unit_dict[variable]))) %>%
    select(VARIABLE, MODEL, SCENARIO, REGION, UNIT, YEAR, VALUE)
  
  #################################################################
  out_annual <- rbind(out_annual, out)
  out_annual <- rbind(out_annual, out_t)
}

write.table(out_annual, "annual_report.csv", sep = ",", row.names = F)
write.table(out_hourly, "hourly_report.csv", sep = ",", row.names = F)

