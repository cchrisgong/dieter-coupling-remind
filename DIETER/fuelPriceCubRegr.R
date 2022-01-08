### code to fit a monotonous cubic equation to data points of oscillatory fuel price from REMIND
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

# fitType = "coalGasCubic"
fitType = "allLinear"

# averageOverIter = 1
averageOverIter = 0
  
if (fitType == "coalGasCubic"){
peTypeList_cubic = c("pecoal", "pegas", "peur")
peTypeList_linear = c("pebiolc")
}

if (fitType == "allLinear"){
peTypeList_cubic = c()
peTypeList_linear = c("pebiolc", "pecoal", "pegas", "peur")
}

sm_TWa_2_MWh = 8760000000
mycolors =  c("fitted FP" = "#999959", "raw FP" = "#0c0c0c")

iter<- read.gdx(gdx, "sm32_iter", squeeze = FALSE)
cYears <- pull(read.gdx(gdx, "tDT32", factors = FALSE))
cReg <- pull(read.gdx(gdx, "regDTCoup", factors = FALSE))


fitFuelPrice_linear <- function(peType){
  # peType = "pebiolc"
  
  rawFuelPrice <- read.gdx(gdx, "p32_fuelprice_avgiter", factors = FALSE, colNames = c('t', 'regi','fuel','value')) %>%
    filter(t %in% cYears) %>% 
    filter(regi %in% cReg) %>% 
    filter(fuel %in% c(peType)) %>% 
    mutate(t = as.numeric(t)) %>% 
    mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>%  # unit conversion
    mutate(label = "raw FP")
  
  rawFuelPrice[rawFuelPrice < 0] <- 0 # ensure positive price
  
  x = unlist(rawFuelPrice$t)
  y = unlist(rawFuelPrice$value)
  
  # fit to a cubic function
  model <- lm(y ~ 1 + x) 
  coeff <- summary(model)$coefficients[, "Estimate"]
  
  fittedFuelPrice <- rawFuelPrice %>% 
    mutate(fittedValue = (coeff[[1]] + coeff[[2]] * t )) %>% 
    mutate(label = "fitted FP") %>% 
    select(t,regi,fuel,value = fittedValue,label)
  
  FP <- list(rawFuelPrice, fittedFuelPrice) %>% 
    reduce(full_join) %>% 
    mutate(value = round(as.numeric(value), digits = 3))
  
  return(FP)
}

fitFuelPrice_cubic <- function(peType){
  # peType = "pegas"
  
  rawFuelPrice <- read.gdx(gdx, "p32_fuelprice_avgiter", factors = FALSE, colNames = c('t', 'regi','fuel','value')) %>%
  filter(t %in% cYears) %>% 
  filter(regi %in% cReg) %>% 
  filter(fuel %in% c(peType)) %>% 
  mutate(t = as.numeric(t)) %>% 
  mutate(value = value * 1e12 / sm_TWa_2_MWh * 1.2) %>%  # unit conversion
  mutate(label = "raw FP")
  
  rawFuelPrice[rawFuelPrice < 0] <- 0 # ensure positive price
  
  x = unlist(rawFuelPrice$t)
  y = unlist(rawFuelPrice$value)
  
  # fit to a cubic function
  model <- lm(y ~ 1 + x + I(x^2) + I(x^3)) 
  coeff <- summary(model)$coefficients[, "Estimate"]
  
  fittedFuelPrice <- rawFuelPrice %>% 
    mutate(fittedValue = (coeff[[1]] + coeff[[2]]*t + coeff[[3]] * t^2 +coeff[[4]] * t^3 )) %>% 
    mutate(label = "fitted FP") %>% 
    select(t,regi,fuel,value=fittedValue,label)
  
  FP <- list(rawFuelPrice, fittedFuelPrice) %>% 
    reduce(full_join) %>% 
    mutate(value = round(as.numeric(value), digits = 3))
    
  return(FP)
}

if (fitType == "coalGasCubic"){
fittedFuelPrice0 <- lapply(peTypeList_cubic, fitFuelPrice_cubic)
fittedFuelPrice1 <- rbindlist(fittedFuelPrice0)
}

fittedFuelPrice0 <- lapply(peTypeList_linear, fitFuelPrice_linear)
fittedFuelPrice2 <- rbindlist(fittedFuelPrice0)


if (fitType == "coalGasCubic"){
fittedFuelPrice <- list(fittedFuelPrice1, fittedFuelPrice2) %>%
  reduce(full_join)
}

if (fitType == "allLinear"){
 fittedFuelPrice <- fittedFuelPrice2
}

#plot raw fuel price and fitted fuel price for each iteration
p<-ggplot() +
  geom_line(data = fittedFuelPrice, aes(x = t, y = value, color = label), size = 1.2, alpha = 1) +
  scale_color_manual(name = "label", values = mycolors)+
  coord_cartesian(ylim = c(0, 80))+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  facet_wrap(~fuel)
ggsave(filename = paste0(mydatapath, "checkFittedFC_",fitType,"_i=", iter,".png"), p, width = 8, height = 5, units = "in", dpi = 120)

#output only fitted fuel price to csv
fittedFuelPrice <- fittedFuelPrice %>% 
  filter(label =="fitted FP") %>% 
  select(-label)
         
write.table(fittedFuelPrice, paste0(mydatapath, "FittedFuelPrice_i=", iter, ".csv"), sep = ",", row.names = F, col.names = F)


#=================== now average over all previous iterations  ====================00
if (averageOverIter == 1){
FC_csv <- list.files(mydatapath, pattern="FittedFuelPrice_i=[0-9]+\\.csv")

id <- NULL
for(fname in FC_csv){
  idx <- as.numeric(str_extract(fname, "[0-9]+"))
  id = c(id, idx)
}
id = sort(id)

sorted_files <- paste0(mydatapath, "FittedFuelPrice_i=", id, ".csv")

df_FC_list = NULL
for (i in id){
  csv = paste0(mydatapath, "FittedFuelPrice_i=", i, ".csv")
  df_FC = read.csv(csv, sep = ",", header = T, stringsAsFactors = F)
  df_FC$iter <- i
  df_FC_list <- rbind(df_FC_list, df_FC)
}

df_FC_avg <- df_FC_list %>% 
  dplyr::group_by(t,regi,fuel) %>%
  dplyr::summarise( value = mean(value), .groups = "keep" ) %>% 
  dplyr::ungroup(t,regi,fuel)

write.table(fittedFuelPrice, paste0(mydatapath, "FittedFuelPrice.csv"), sep = ",", row.names = F, col.names = F)
}

if (averageOverIter == 0){
write.table(fittedFuelPrice, paste0(mydatapath, "FittedFuelPrice.csv"), sep = ",", row.names = F, col.names = F)
}
