
mypath = "~/DIETER/myFirstDIETER/dataprocessing/"

# import library

source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)

# testfile = "~/DIETER/myFirstDIETER/DIETER/results70_flex.gdx"
# gdxToQuitte(testfile)
# hourly_reportCSV = read.csv(paste0(mypath, "hourly_report70_flex.csv"), sep = ';', header = T, stringsAsFactors = F)

testfile = "~/DIETER/myFirstDIETER/DIETER/results70.gdx"
gdxToQuitte(testfile)
hourly_reportCSV = read.csv(paste0(mypath, "hourly_report70.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
QUITTobj2 = hourly_reportQUITT 
    
prices_DIETER<- QUITTobj2 %>% 
  filter(variable == "Wholesale electricity price") %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(value = as.double( value * (value > 0))) %>%
  select(value) %>% 
  dplyr::rename(PRICE = value)
  
 
prices_2016         <- xlsx::read.xlsx(file = paste0(mypath,"Prices_2016DEU.xlsx"), sheetName = "Sheet1") # 
prices_2017         <- xlsx::read.xlsx(file = paste0(mypath,"Prices_2017DEU.xlsx"), sheetName = "Sheet1") #
prices_2018         <- xlsx::read.xlsx(file = paste0(mypath,"Prices_2018DEU.xlsx"), sheetName = "Sheet1") #

prices_16<-prices_2016%>% 
  mutate(PRICE = as.numeric(PRICE)) %>% 
  mutate(PRICE = PRICE*(PRICE>0)) 

prices_18<-prices_2018%>% 
  mutate(PRICE = as.numeric(PRICE)) %>% 
  mutate(PRICE = PRICE*(PRICE>0)) 

prices_16$year       <- "2016"
# prices_2017$year       <- "2017"
prices_18$year       <- "2018"
prices_DIETER$year <- "DIETER (VRE70% wo/stor)"

mean_price <- ddply(electricity_price, "year", summarise, grp.mean=mean(PRICE))
  
  ggplot() +
    geom_histogram(data=prices_16, aes(PRICE, fill = year, color = year), binwidth = 15, alpha = 0.5)+
    geom_histogram(data=prices_18, aes(PRICE, fill = year, color = year), binwidth = 15, alpha = 0.5)+
    geom_histogram(data=prices_DIETER, aes(PRICE, fill = year, color = year), binwidth = 15, alpha = 0.5)+
    geom_vline(aes(xintercept=mean(prices_16$PRICE), color = prices_16$year), linetype="dashed", size=1)+
    geom_vline(aes(xintercept=mean(prices_18$PRICE), color = prices_18$year), linetype="dashed", size=1)+
    geom_vline(aes(xintercept=mean(prices_DIETER$PRICE), color = prices_DIETER$year), linetype="dashed", size=1)+
    # annotate(geom = "text", x = 150, y = 4500, label = "70%VRE", color = "blue",  size=5)+
    ggtitle("Electricity price distribution (dashed line: mean)")+
    xlim(c(-10,200)) +
    # ylim(c(0,100))+
    xlab("electricity price (EURO/MWh)") + ylab("number of hours in a year")+
    theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) 
  
  ggsave(filename =paste0(mypath, "Price_frequency_wHistdata_woStor.png"),  width = 5, height =5, units = "in", dpi = 220)
  

