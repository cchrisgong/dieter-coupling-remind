
mypath = "~/DIETER/myFirstDIETER/dataprocessing/"
mydatapath = "~/DIETER/myFirstDIETER/DIETER/"

# import library

source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)
# specify output file

testfile = "results70.gdx"
# testfile = "results70_flex.gdx"
gdxToQuitte(testfile)
hourly_reportCSV = read.csv(paste0(mypath, "results70_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
annual_reportCSV = read.csv(paste0(mypath, "results70_annualreport.csv"), sep = ';', header = T, stringsAsFactors = F)
# hourly_reportCSV = read.csv(paste0(mypath, "results70_flex_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
# annual_reportCSV = read.csv(paste0(mypath, "results70_flex_annualreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
annual_reportQUITT <- as.quitte(annual_reportCSV) 
QUITTobj2 = hourly_reportQUITT
QUITTobj_a2 = annual_reportQUITT

load_2 <- QUITTobj2 %>% 
  filter(variable == "Net electricity demand") %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  select(hour, value) %>% 
  dplyr::rename(load = value)

price_hr_2<- QUITTobj2 %>% 
  filter(variable == "Wholesale electricity price") %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  select(hour, value) %>%
  mutate(value = value*(value>0))

load_price2 =list(load_2, price_hr_2) %>% 
  reduce(full_join) 

load_price2$weighted <- sum(load_price2$load * load_price2$value/sum(load_price2$load))

# sum_h (price(h) * dem(h)/ total demand)

load_price2$hrRevenue <- load_price2$load * load_price2$value

marketValue2 <- QUITTobj_a2 %>% 
  filter(variable == "Market value") %>% 
  filter(tech %in% c("Wind_on","Solar PV")) %>% 
  dplyr::rename(MarketValue = value)%>% 
  select(tech, MarketValue) 

VREgen2 <- QUITTobj_a2 %>% 
  filter(variable == "Total Renewable Generation") %>% 
  filter(tech %in% c("Wind_on","Solar PV")) %>% 
  dplyr::rename(TotalGen = value)%>% 
  select(tech, TotalGen) 

marketValue2$LCOE <- c(56, 85)

marketValue2$subsidy <- marketValue2$LCOE - marketValue2$MarketValue

totalSub2 = list(marketValue2, VREgen2) %>% 
  reduce(full_join) 

totalSub2$TotalSub <- totalSub2$TotalGen * totalSub2$subsidy

#hourly multiplicative subsidy, mark up factor is calculated by  formula:
#total subsidy = SUM(h:1:8760)[(markup factor - 1) * price(h) * total_hourly_demand(h)]

totalSub2$mkupfactor <- sum(totalSub2$TotalSub) / sum(load_price2$hrRevenue) + 1


  
#===============================================================================================
#===============================================================================================
testfile = "results50.gdx"
# testfile = "results50_flex.gdx"
gdxToQuitte(testfile)
hourly_reportCSV = read.csv(paste0(mypath, "results50_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
annual_reportCSV = read.csv(paste0(mypath, "results50_annualreport.csv"), sep = ';', header = T, stringsAsFactors = F)
# hourly_reportCSV = read.csv(paste0(mypath, "results50_flex_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
# hourly_reportCSV = read.csv(paste0(mypath, "results50_flex_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
annual_reportQUITT <- as.quitte(annual_reportCSV) 
QUITTobj3 = hourly_reportQUITT
QUITTobj_a3 = annual_reportQUITT

load_3 <- QUITTobj3 %>% 
  filter(variable == "Net electricity demand") %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  select(hour, value) %>% 
  dplyr::rename(load = value)

price_hr_3<- QUITTobj3 %>% 
  filter(variable == "Wholesale electricity price") %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  select(hour, value) %>%
  mutate(value = value*(value>0))

load_price3 =list(load_3, price_hr_3) %>% 
  reduce(full_join) 

load_price3$weighted <- sum(load_price3$load * load_price3$value/sum(load_price3$load))
  
load_price3$hrRevenue <- load_price3$load * load_price3$value

marketValue3 <- QUITTobj_a3 %>% 
  filter(variable == "Market value") %>% 
  filter(tech %in% c("Wind_on","Solar PV")) %>% 
  dplyr::rename(MarketValue = value)%>% 
  select(tech, MarketValue) 

VREgen3 <- QUITTobj_a3 %>% 
  filter(variable == "Total Renewable Generation") %>% 
  filter(tech %in% c("Wind_on","Solar PV")) %>% 
  dplyr::rename(TotalGen = value)%>% 
  select(tech, TotalGen) 

marketValue3$LCOE <- c(56, 85)

marketValue3$subsidy <- marketValue3$LCOE - marketValue3$MarketValue

totalSub3 = list(marketValue3, VREgen3) %>% 
  reduce(full_join) 

totalSub3$TotalSub <- totalSub3$TotalGen * totalSub3$subsidy

#hourly multiplicative subsidy, mark up factor is calculated by  formula:
#total subsidy = SUM(h:1:8760)[(markup factor - 1) * price(h) * total_hourly_demand(h)]

totalSub3$mkupfactor <- sum(totalSub3$TotalSub) / sum(load_price3$hrRevenue) + 1
#===============================================================================================
#===============================================================================================
  testfile = "results25.gdx"
  # testfile = "results25_flex.gdx"
  gdxToQuitte(testfile)
  hourly_reportCSV = read.csv(paste0(mypath, "results25_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
  annual_reportCSV = read.csv(paste0(mypath, "results25_annualreport.csv"), sep = ';', header = T, stringsAsFactors = F)
  # hourly_reportCSV = read.csv(paste0(mypath, "results25_flex_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
  # hourly_reportCSV = read.csv(paste0(mypath, "results25_flex_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
  hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  QUITTobj4 = hourly_reportQUITT
  QUITTobj_a4 = annual_reportQUITT
  
  load_4 <- QUITTobj4 %>% 
    filter(variable == "Net electricity demand") %>% 
    mutate(hour = str_replace(hour, hour, str_sub(hour,2, -1))) %>% 
    mutate(hour = as.numeric(hour))%>% 
    select(hour, value) %>% 
    dplyr::rename(load = value)
  
  price_hr_4<- QUITTobj4 %>% 
    filter(variable == "Wholesale electricity price") %>% 
    mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
    mutate(hour = as.numeric(hour))%>% 
    select(hour, value) %>%
    mutate(value = value*(value>0))
  
  load_price4 =list(load_4, price_hr_4) %>% 
    reduce(full_join) 
  
  load_price4$weighted <- sum(load_price4$load * load_price4$value/sum(load_price4$load))
  load_price4$hrRevenue <- load_price4$load * load_price4$value
  
  marketValue4 <- QUITTobj_a4 %>% 
    filter(variable == "Market value") %>% 
    filter(tech %in% c("Wind_on","Solar PV")) %>% 
    dplyr::rename(MarketValue = value)%>% 
    select(tech, MarketValue) 
  
  VREgen4 <- QUITTobj_a4 %>% 
    filter(variable == "Total Renewable Generation") %>% 
    filter(tech %in% c("Wind_on","Solar PV")) %>% 
    dplyr::rename(TotalGen = value)%>% 
    select(tech, TotalGen) 
  
  marketValue4$LCOE <- c(56, 85)
  
  marketValue4$subsidy <- marketValue4$LCOE - marketValue4$MarketValue
  
  totalSub4 = list(marketValue4, VREgen4) %>% 
    reduce(full_join) 
  
  totalSub4$TotalSub <- totalSub4$TotalGen * totalSub4$subsidy
  
  #hourly multiplicative subsidy, mark up factor is calculated by  formula:
  #total subsidy = SUM(h:1:8760)[(markup factor - 1) * price(h) * total_hourly_demand(h)]
  
  totalSub4$mkupfactor <- sum(totalSub4$TotalSub) / sum(load_price4$hrRevenue) + 1
  
  
  
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  n = 3
  cols = gg_color_hue(n)

  ymin4 = sort(price_hr_4$value)[2]
  ymin3 = sort(price_hr_3$value)[2]
  ymin2 = sort(price_hr_2$value)[2]
  
  ymax4 = sort(price_hr_4$value)[length(price_hr_4$value)-1]
  ymax3 = sort(price_hr_3$value)[length(price_hr_3$value)-1]
  ymax2 = sort(price_hr_2$value)[length(price_hr_2$value)-1]

  xlim = c(min(ymin2,ymin3,ymin4),200)
  # xlim = c(-10,130)
  print(xlim)

  price_hr_4$share<- "VRE25%"
  price_hr_3$share<- "VRE50%"
  price_hr_2$share<- "VRE70%"
  
  ggplot(data=price_hr_4) +
    geom_histogram(aes(value, fill = share, color= share), binwidth = 15, alpha = 0.5, boundary=0)+
    geom_vline(aes(xintercept = load_price4$weighted[[1]], color = share), linetype="dashed", size=1)+
    ggtitle("Electricity price distribution (dashed line: demand weighted mean)")+
    xlab("electricity price (USD/MWh)") + ylab("number of hours in a year")+
    scale_x_continuous(limits=xlim)+
    scale_y_continuous(limits=c(0,6000))+
    scale_fill_manual(values = cols)+
    # xlim(c(-10,200)) +
    # ylim(c(0,6000))+
    theme(plot.title =element_text(size=9), legend.title = element_blank(),legend.position = c(.85, .55)) 
  
  ggsave(filename =paste0(mypath, "Price_frequency1_wstor.png"),  width = 4.5, height =5.5, units = "in", dpi = 220)
 
  ggplot() +
    geom_histogram(data=price_hr_4, aes(value, fill = share, color= share), binwidth = 15, alpha = 0.5, boundary=0)+
    geom_histogram(data=price_hr_3, aes(value, fill = share, color= share), binwidth = 15, alpha = 0.5, boundary=0)+
    geom_vline(data=price_hr_4, aes(xintercept=load_price4$weighted[[1]], color = share), linetype="dashed", size=1)+
    geom_vline(data=price_hr_3, aes(xintercept=load_price3$weighted[[1]], color = share), linetype="dashed", size=1)+
    ggtitle("Electricity price distribution (dashed line: demand weighted mean)")+
    scale_x_continuous(limits=xlim)+
    scale_y_continuous(limits=c(0,6000))+
    scale_fill_manual(values = cols)+
    xlab("electricity price (USD/MWh)") + ylab("number of hours in a year")+
    theme(plot.title =element_text(size=9), legend.title = element_blank(),legend.position = c(.85, .55))
    
  ggsave(filename =paste0(mypath, "Price_frequency2_wstor.png"),  width = 4.5, height =5.5, units = "in", dpi = 220)
  
  ggplot() +
    geom_histogram(data=price_hr_4, aes(value, fill = share, color= share), binwidth = 15, alpha = 0.5, boundary=0)+
    geom_histogram(data=price_hr_3, aes(value, fill = share, color= share), binwidth = 15, alpha = 0.5, boundary=0)+
    geom_histogram(data=price_hr_2, aes(value, fill = share, color= share), binwidth = 15, alpha = 0.5, boundary=0)+
    geom_vline(data=price_hr_4, aes(xintercept=load_price4$weighted[[1]], color = share), linetype="dashed", size=1)+
    geom_vline(data=price_hr_3, aes(xintercept=load_price3$weighted[[1]], color = share), linetype="dashed", size=1)+
    geom_vline(data=price_hr_2, aes(xintercept=load_price2$weighted[[1]], color = share), linetype="dashed", size=1)+
    ggtitle("Electricity price distribution (dashed line: demand weighted mean)")+
    scale_x_continuous(limits=xlim)+
    scale_y_continuous(limits=c(0,6000))+
    scale_fill_manual(values = cols)+
    xlab("electricity price (USD/MWh)") + ylab("number of hours in a year")+
    theme(plot.title =element_text(size=9), legend.title = element_blank(),legend.position = c(.85, .55)) 
  
  ggsave(filename =paste0(mypath, "Price_frequency3_wstor.png"),  width = 4.5, height =5.5, units = "in", dpi = 220)

  
  # dfnames <- c("price_hr_1", "price_hr_2", "price_hr_3", "price_hr_4")
  # varshares <- c(pWindOnshare1, pWindOnshare2, pWindOnshare3, pWindOnshare4)
  # 
  # filterdf <- function(varshare) {
  #   d <- price_hr %>% 
  #     filter(PVshare == pPVshar) %>% 
  #     filter(WindOnshare == varshare) %>% 
  #     select("hour", "value")
  #   
  #   hour_col <- d["hour"]
  #   d["DayNight"] <- ifelse(and(hour_col%%24 >=7, hour_col%%24 <19), "DAY", "NIGHT")
  #   d <- d %>% arrange(desc(value))
  #   d["sorted_x"] <- seq(1, 8760)
  #   d
  # }
  # 
  # data_main <- lapply(varshares, filterdf)
  # names(data_main) <- dfnames