
mypath = "~/DIETER/myFirstDIETER/dataprocessing/"
  
# import library

source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)

#===========================================
testfile = "~/DIETER/myFirstDIETER/DIETER/results70_flex.gdx"
gdxToQuitte(testfile)
hourly_reportCSV = read.csv(paste0(mypath, "hourly_report70_flex.csv"), sep = ';', header = T, stringsAsFactors = F)

# testfile = "~/DIETER/myFirstDIETER/DIETER/results70.gdx"
# gdxToQuitte(testfile)
# hourly_reportCSV = read.csv(paste0(mypath, "hourly_report70.csv"), sep = ';', header = T, stringsAsFactors = F)

# testfile = "~/DIETER/myFirstDIETER/DIETER/results70.gdx"
# gdxToQuitte(testfile)
# hourly_reportCSV = read.csv(paste0(mypath, "hourly_report70.csv"), sep = ';', header = T, stringsAsFactors = F)

hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
QUITTobj2 = hourly_reportQUITT

price_hr_2<- QUITTobj2 %>% 
  filter(variable == "Wholesale electricity price") %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  mutate(value = as.double( value * (value > 0))) %>%
  select(value, hour) %>% 
  arrange(desc(value)) 

price_hr_2$sorted_x <- seq(1, 8760)

prices_2016         <- xlsx::read.xlsx(file = paste0(mypath,"Prices_2016DEU.xlsx"), sheetName = "Sheet1") # 
prices_2018         <- xlsx::read.xlsx(file = paste0(mypath,"Prices_2018DEU.xlsx"), sheetName = "Sheet1") #

prices_16<-prices_2016%>% 
  mutate(PRICE = as.numeric(PRICE)) %>% 
  mutate(PRICE = PRICE*(PRICE>0))  %>% 
  arrange(desc(PRICE)) 
prices_16$sorted_x <- seq(1, 8760)

prices_18<-prices_2018%>% 
  mutate(PRICE = as.numeric(PRICE)) %>% 
  mutate(PRICE = PRICE*(PRICE>0)) %>% 
  arrange(desc(PRICE)) 
prices_18$sorted_x <- seq(1, 8760)

prices_16$year       <- "2016"
prices_18$year       <- "2018"
price_hr_2$year <- "DIETER 70%VRE (w/ stor)"

ylim <- c(0,140)

p <- ggplot() +
  geom_line(data = prices_16, aes(x = sorted_x, y = PRICE, color = year), group = year, size = 3.2, alpha = 0.8) +
  geom_line(data = prices_18, aes(x = sorted_x, y = PRICE, color = year), group = year, size = 3.2, alpha = 0.8) +
  geom_line(data = price_hr_2, aes(x = sorted_x, y = value, color = year), group = year, size = 3.2, alpha = 0.8) +
  theme(plot.title =element_text(size=22), axis.text=element_text(size=25), axis.title=element_text(size= 25,face="bold"), 
        legend.text = element_text(size = 25), legend.title = element_text(size = 25)) +
  xlab("sorted hour") + ylab("electricity price (EURO/MWh)")+ggtitle("Price duration curves")+
  coord_cartesian( ylim = ylim, xlim = c(0, 8760))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank()) +
  theme(plot.title =element_text(size=35), axis.text=element_text(size=25), axis.title=element_text(size= 25,face="bold"), 
        legend.text = element_text(size = 25)) 


ggsave(filename =paste0(mypath, "Duration_curve_lines_wStor.png"),  width = 10, height =10, units = "in", dpi = 120)
  
