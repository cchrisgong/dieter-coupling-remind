### code to do a regression on data points of oscillatory fuel price from REMIND
library(tidyverse)
library(gdxrrw)
library(data.table)
library(quitte)
library(nnls)

# mydatapath = paste0("~/DIETER/dieter-coupling-remind/DIETER/")
# igdx("/opt/gams/gams30.2_linux_x64_64_sfx")
mydatapath = "./"
# should read cYears and cReg from the gdx in the future
cReg <- "DEU"

#"RMdata_4DT.gdx" is the in-between iteration gdx produced by REMIND (before fulldata.gdx is produced)
gdx <- paste0(mydatapath,"RMdata_4DT.gdx")


peTypeList_linear = c("pebiolc", "pecoal", "pegas", "peur")
seTypeList_linear = c("seh2")

sm_TWa_2_MWh = 8760000000
mycolors =  c("fitted FP" = "#999959", "raw FP" = "#0c0c0c")

iter<- read.gdx(gdx, "sm32_iter", squeeze = FALSE)
cYears <- pull(read.gdx(gdx, "tDT32", factors = FALSE))
cReg <- pull(read.gdx(gdx, "regDTCoup", factors = FALSE))

fitFuelPrice_linear <- function(peType){
  # peType = "peur"
  
  rawFuelPrice <- read.gdx(gdx, "p32_fuelprice_avgiter", factors = FALSE, colNames = c('t', 'regi','fuel','value')) %>%
    filter(t %in% cYears) %>% 
    filter(regi %in% cReg) %>% 
    filter(fuel %in% c(peType)) %>% 
    mutate(t = as.numeric(t)) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh) %>%  # unit conversion
    mutate(label = "raw FP") 
  
  #### only fit if data contains at least 2 points
  if (length(rawFuelPrice$value) > 1){
  
  rawFuelPrice[rawFuelPrice < 0] <- 0 # ensure positive price
  
  x = unlist(rawFuelPrice$t)
  y = unlist(rawFuelPrice$value)
  
  # fit to a linear function
  model <- lm(y ~ 1 + x) 
  coeff <- summary(model)$coefficients[, "Estimate"]
  
  fittedFuelPrice <- rawFuelPrice %>% 
    mutate(fittedValue = (coeff[[1]] + coeff[[2]] * t )) %>% 
    mutate(label = "fitted FP") %>% 
    select(t,regi,fuel,value = fittedValue,label)
  }
  
  ### if the remind data consists of one data point, simply copy the value for other years as well
  if (length(as.list(rawFuelPrice$value)) == 1){
    fittedFuelPrice <- rawFuelPrice %>% 
      select(-t) 
    
    fittedFuelPrice <- fittedFuelPrice %>%
      expand(fittedFuelPrice, t = cYears) %>% 
      mutate(t = as.numeric(t)) %>% 
      mutate(label = "fitted FP") 
  }
  
  ### if the remind data consists of no data point, fill it with 10
  if (length(as.list(rawFuelPrice$value)) == 0){
    t = cYears
    regi = cReg
    fuel = c(peType)
    fittedFuelPrice <- data.frame( 
      t,
      regi ,
      fuel ,
      # label = "fitted FP",
      value = 10 
    )
    FP<-fittedFuelPrice %>% 
      mutate(value = round(as.numeric(value), digits = 2))%>% 
      mutate(label = "fitted FP") 
      
  }
  
  if (!length(as.list(rawFuelPrice$value)) == 0){
  FP <- list(rawFuelPrice, fittedFuelPrice) %>% 
    reduce(full_join) %>% 
    mutate(value = round(as.numeric(value), digits = 2))
  }
  
  return(FP)
}

fitSeFuelPrice_linear <- function(seType){
  # seType = "seh2"
  
  rawFuelPrice <- read.gdx(gdx, "pm_SEPrice", factors = FALSE, colNames = c('t', 'regi','fuel','value')) %>%
    filter(t %in% cYears) %>% 
    filter(regi %in% cReg) %>% 
    filter(fuel %in% c(seType)) %>% 
    mutate(t = as.numeric(t)) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh) %>%  # unit conversion
    mutate(label = "raw FP") 
  
  #### only fit if data contains at least 2 points
  if (length(rawFuelPrice$value) > 1){
    
    rawFuelPrice[rawFuelPrice < 0] <- 0 # ensure positive price
    
    x = unlist(rawFuelPrice$t)
    y = unlist(rawFuelPrice$value)
    
    # fit to a linear function
    model <- lm(y ~ 1 + x) 
    coeff <- summary(model)$coefficients[, "Estimate"]
    
    fittedFuelPrice <- rawFuelPrice %>% 
      mutate(fittedValue = (coeff[[1]] + coeff[[2]] * t )) %>% 
      mutate(label = "fitted FP") %>% 
      mutate(t = as.numeric(t)) %>% 
      select(t,regi,fuel,value = fittedValue,label)
    
    fittedFuelPrice <- fittedFuelPrice%>%
      dplyr::group_by(regi,fuel,label) %>%
      complete(t = as.numeric(cYears), fill = list(value = 0)) %>% 
      dplyr::ungroup(regi,fuel,label) 
      
  }
  
  ### if the remind data consists of one data point, simply copy the value for other years as well
  if (length(as.list(rawFuelPrice$value)) == 1){
    fittedFuelPrice <- rawFuelPrice %>% 
      select(-t) 
    
    fittedFuelPrice <- fittedFuelPrice %>%
      expand(fittedFuelPrice, t = cYears) %>% 
      mutate(t = as.numeric(t)) %>% 
      mutate(label = "fitted FP") 
  }
  
  ### if the remind data consists of no data point, fill it with 10
  if (length(as.list(rawFuelPrice$value)) == 0){
    t = cYears
    regi = cReg
    fuel = c(seType)
    fittedFuelPrice <- data.frame( 
      t,
      regi ,
      fuel ,
      # label = "fitted FP",
      value = 10 
    )
    FP<-fittedFuelPrice %>% 
      mutate(value = round(as.numeric(value), digits = 2))%>% 
      mutate(label = "fitted FP") 
    
  }
  
  if (!length(as.list(rawFuelPrice$value)) == 0){
    FP <- list(rawFuelPrice, fittedFuelPrice) %>% 
      reduce(full_join) %>% 
      mutate(value = round(as.numeric(value), digits = 2))
  }
  
  return(FP)
}

fittedPeFuelPrice0 <- lapply(peTypeList_linear, fitFuelPrice_linear)
fittedPeFuelPrice <- rbindlist(fittedPeFuelPrice0)
fittedSeFuelPrice0 <- lapply(seTypeList_linear, fitSeFuelPrice_linear)
fittedSeFuelPrice <- rbindlist(fittedSeFuelPrice0)

fittedFuelPrice <- list(fittedPeFuelPrice, fittedSeFuelPrice) %>%
  reduce(full_join)

#plot raw fuel price and fitted fuel price for each iteration
p<-ggplot() +
  geom_line(data = fittedFuelPrice, aes(x = t, y = value, color = label), size = 1.2, alpha = 1) +
  scale_color_manual(name = "label", values = mycolors)+
  coord_cartesian(ylim = c(0, 80))+
  theme(axis.text=element_text(size=5), axis.title=element_text(size=5,face="bold"))+
  facet_wrap(~fuel)
ggsave(filename = paste0(mydatapath, "checkFittedFC_i=", iter,".png"), p, width = 8, height = 5, units = "in", dpi = 120)

#output only fitted fuel price to csv
fittedFuelPrice <- fittedFuelPrice %>% 
  filter(label =="fitted FP") %>% 
  select(-label)
       

  write.table(fittedFuelPrice, paste0(mydatapath, "FittedFuelPrice.csv"), sep = ",", row.names = F, col.names = F)
  

