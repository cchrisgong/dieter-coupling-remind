# get data from config.R file
#file: config.R
require(quitte)
require(gdxrrw)
require(tidyverse)

# IGNORE THE WARNING WHICH APPEARS AFTER EXECUTION OF THE FILE.. 

mypath = "~/DIETER/myFirstParallelDIETER/dataprocessing/"
mydatapath = "~/DIETER/myFirstParallelDIETER/DIETER/"

source(paste0(mypath, "library_import.R"))
source("~/DIETER/myFirstParallelDIETER/dataprocessing/config.R")

gdxToQuitte <- function(file){
  file = "results_DIETER_1.gdx"
  out_hourly <- NULL
  out_annual <- NULL
  
  ######################################################################################################################## 
  rep_hrs = read.gdx(gdxName = file, requestList = 'report_hours', factors = FALSE)
  names(rep_hrs) <- c("model", "year", "country", "variable", "hour", "value")
  
  out_h <- rep_hrs %>% 
    group_by(model, year, variable, country) %>%
    complete(hour = paste0("h",1:8760)) %>%
    mutate(hour = sort(as.character(hour))) %>% 
    replace(is.na(.), 0) %>%
    ungroup(model, year, variable, country) %>% 
    mutate(MODEL = model, SCENARIO = paste0("baseline"), REGION = country,
           HOUR = hour, TECH = "all Tech") %>% 
    mutate(VARIABLE = as.vector(unlist(variable_dict[variable])), PERIOD = year,
           UNIT = as.vector(unlist(unit_dict[variable])), VALUE = value) %>%
    select(MODEL, SCENARIO, PERIOD, HOUR, REGION, VARIABLE, TECH, VALUE, UNIT)
  
  ###################################################################
  rep_techHrs = read.gdx(gdxName = paste0(mydatapath, file), requestList = 'report_tech_hours', factors = FALSE)
  
  names(rep_techHrs) <- c("model","year", "country","variable", "tech", "hour", "value")
  
  out_th <- rep_techHrs %>%
  mutate(tech = as.character(tech)) %>%
  group_by(model, year, variable, country,tech)%>%
  complete(hour = paste0("h",1:8760)) %>%
  mutate(hour = sort(as.character(hour))) %>%
  replace(is.na(.), 0) %>%
  ungroup(model, year, variable, country,tech) %>%
  mutate(MODEL = model, SCENARIO = paste0("baseline"),  REGION = country,
         HOUR = hour,TECH = as.vector(unlist(tech_dict[tech]))) %>%
  mutate(VARIABLE = as.vector(unlist(variable_dict[variable])), PERIOD = year,
         UNIT = as.vector(unlist(unit_dict[variable])), VALUE = value) %>% 
  select(MODEL, SCENARIO, PERIOD, HOUR, REGION, VARIABLE, TECH, VALUE, UNIT)
  
  #################################################################
  
  out_hourly <- rbind(out_hourly, out_h)
  out_hourly <- rbind(out_hourly, out_th)
    
  ###########################################################################################################################
  rep = read.gdx(gdxName = paste0(mydatapath, file), requestList = 'report', factors = FALSE)
  
  names(rep) <- c("model","year", "country","variable", "value")
  out <- rep %>% 
    mutate(MODEL = model, SCENARIO = paste0("baseline"), 
           REGION = country, YEAR = year, VALUE = value, 
           TECH = "all Tech",
           VARIABLE = as.vector(unlist(variable_dict[variable])),
           UNIT = as.vector(unlist(unit_dict[variable])), PERIOD = "annual") %>%
    select(MODEL, SCENARIO, YEAR, REGION, PERIOD, VARIABLE, TECH, VALUE, UNIT)
  
  #################################################################
  rep_Tech = read.gdx(gdxName = paste0(mydatapath, file), requestList = 'report_tech', factors = FALSE)
  
  names(rep_Tech) <- c("model","year", "country","variable", "tech", "value")
  out_t <- rep_Tech %>%
    mutate(tech = as.character(tech)) %>%
    mutate(MODEL = model, SCENARIO = paste0("baseline"), 
           REGION = country, YEAR = year, VALUE = value, 
           TECH = as.vector(unlist(tech_dict[tech])), 
           VARIABLE = as.vector(unlist(variable_dict[variable])), 
           UNIT = as.vector(unlist(unit_dict[variable])), PERIOD = "annual") %>%
    select(MODEL, SCENARIO, YEAR, REGION, PERIOD, VARIABLE, TECH, VALUE, UNIT)
  
  #################################################################
  out_annual <- rbind(out_annual, out)
  out_annual <- rbind(out_annual, out_t)
  print(str_sub(file,1,-5))
  
  write.table(out_annual, paste0(mypath, str_sub(file,1,-5), "_annualreport.csv"), sep = ";", row.names = F)
  write.table(out_hourly, paste0(mypath, str_sub(file,1,-5), "_hourlyreport.csv"), sep = ";", row.names = F)
  print(paste0("csv table saved to", mypath))
}


# file = "~/DIETER/myFirstDIETER/DIETER/results_mSS_exoWS_PtG-ONLY.gdx"
# gdxToQuitte(file)
