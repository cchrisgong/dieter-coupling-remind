mypath = "~/DIETER/myFirstDIETER/dataprocessing/"
mydatapath = "~/DIETER/myFirstDIETER/DIETER/"

# import library
source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)

# storTag = 0
storTag = 1

VREshares <- c(25,50,70)
dftot <- list()
dftot2 <- list()
TableList <-list()
i = 1

while(i < 4) {
  
  VREshare = VREshares[[i]]
  
  if (storTag == 0) {
    file = paste0("results", VREshares,".gdx")
    # gdxToQuitte(file)
    # VREshare = 70
    hourly_reportCSV = read.csv(paste0(mypath, "results", VREshare,"_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
    annual_reportCSV = read.csv(paste0(mypath, "results", VREshare,"_annualreport.csv"), sep = ';', header = T, stringsAsFactors = F)
  }
  
  if (storTag == 1) {
    file = paste0("results", VREshare,"_flex.gdx")
    # gdxToQuitte(file)
    # VREshare = 70
    hourly_reportCSV = read.csv(paste0(mypath, "results", VREshare,"_flex_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
    annual_reportCSV = read.csv(paste0(mypath, "results", VREshare,"_flex_annualreport.csv"), sep = ';', header = T, stringsAsFactors = F)
  }
  
  hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
  annual_reportQUITT <- as.quitte(annual_reportCSV) 
  QUITTobj = hourly_reportQUITT
  QUITTobj_a = annual_reportQUITT
  
  load <- QUITTobj %>% 
    filter(variable == "Net electricity demand") %>% 
    mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
    mutate(hour = as.numeric(hour))%>% 
    select(hour, value) %>% 
    dplyr::rename(load = value)
 
  price_hr0 <- QUITTobj %>% 
    filter(variable == "Wholesale electricity price") %>% 
    mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
    mutate(hour = as.numeric(hour))%>% 
    select(hour, value) 
  
  windgen_hr <- QUITTobj %>% 
    filter(variable == "Hourly renewable generation") %>% 
    filter(tech %in% c("Wind_on")) %>% 
    select(hour, value) 

  pvgen_hr<- QUITTobj %>% 
    filter(variable == "Hourly renewable generation") %>% 
    filter(tech %in% c("Solar PV")) %>% 
    select(hour, value) 
  
  price_hr <- price_hr0    %>%
    mutate(value = value * (value > 0))
  
  marketValue_wind = sum(price_hr$value * windgen_hr$value) / (sum(windgen_hr$value))
  marketValue_solar = sum(price_hr$value * pvgen_hr$value) / (sum(pvgen_hr$value))
  
  load_price = list(load, price_hr) %>%
    reduce(full_join)
  
  load_price$hrRevenue <- load_price$load * load_price$value
  
  VREgen <- QUITTobj_a %>% 
    filter(variable == "Total Renewable Generation") %>% 
    filter(tech %in% c("Wind_on","Solar PV")) %>% 
    dplyr::rename(TotalGen = value) %>% 
    select(tech, TotalGen)

  # CU_VRE <- QUITTobj_a %>% 
  #   filter(variable == "Absolute VRE curtailment") %>% 
  #   filter(tech %in% c("Wind_on","Solar PV"))%>% 
  #   select(tech, value) %>% 
  #   dplyr::rename(curt = value)
  
  #LCOE from literature
  VREgen$LCOE <- c(56, 85)
  VREgen$MarketValue <- c(marketValue_wind, marketValue_solar)
  
  VREgen$subsidy <- VREgen$LCOE - VREgen$MarketValue
  
  # totalSub = list(VREgen, CU_VRE) %>% 
  #   reduce(full_join)
  # 
  # totalSub[is.na(totalSub)] <- 0
  
  VREgen$TotalSub <- VREgen$TotalGen * VREgen$subsidy
  
  #hourly multiplicative subsidy, mark up factor is calculated by formula:
  #total subsidy = SUM(h:1:8760)[(markup factor - 1) * price(h) * total_hourly_demand(h)]
  
  VREgen$mkupfactor <- sum( VREgen$TotalSub ) / sum(load_price$hrRevenue) + 1
  
  print( VREgen$mkupfactor[[1]] )
  
  price_hr2 <- price_hr %>% 
    mutate( value = value * VREgen$mkupfactor[[1]] )
  
  load_price2 = list(load, price_hr2) %>%
    reduce(full_join)
  
  #load weighted average of old price with neg price
  avg_price_0 <- sum(load$load * price_hr0$value/sum(load$load))
  #load weighted average of old price without neg price
  avg_price_00 <- sum(load$load * price_hr$value/sum(load$load))
  
  #load weighted average of new price
  avg_price_i <- sum(load$load * load_price2$value/sum(load$load))
  # sum_h (price(h) * dem(h)/ total demand)
  
  load2 <- QUITTobj %>% 
    filter(variable == "Net electricity demand") %>% 
    select(hour, value) %>% 
    dplyr::rename(load = value)
  
  PV<- QUITTobj %>% 
    filter(variable == "Hourly renewable generation") %>% 
    filter(tech == "Solar PV") %>% 
    select(hour, value) %>% 
    dplyr::rename(solgen = value)
  
  Wind<- QUITTobj %>% 
    filter(variable == "Hourly renewable generation") %>% 
    filter(tech == "Wind_on")%>% 
    select(hour, value) %>% 
    dplyr::rename(windgen = value)
  
  LDC = list(load2, PV, Wind) %>% 
    reduce(full_join) 
  
  LDC[is.na(LDC)] <- 0
  
  LDC$ldc <- LDC$load - LDC$solgen - LDC$windgen
  
  #residual load weighted price
  resload_weighted <- sum( LDC$ldc * price_hr$value / sum(LDC$ldc) )
  
  avg_price_ii =  (1 - VREshare/100) * resload_weighted + VREshare/200 * VREgen$LCOE[[1]] + VREshare/200 * VREgen$LCOE[[2]]
  #to check for correctness of the calculation of load_weighted
  #average price (with subsidies) = (1-VRE share) * specific residual load price + VRE share * LCOE_VRE
  
  dftot[[i]] <- price_hr2
  dftot2[[i]] <- avg_price_i
  
  windShare <- c(VREshare/2)
  solarShare <- c(VREshare/2)
  MVwind <- c(VREgen$MarketValue[[1]])
  MVsolar <- c(VREgen$MarketValue[[2]])
  LCOEwind <- c(VREgen$LCOE[[1]])
  LCOEsolar <- c(VREgen$LCOE[[2]])
  avg_pr_wNegpr_woSUB <- c(avg_price_0)
  avg_pr_woNegpr_woSUB <- c(avg_price_00)
  totSUB <- c( round(sum(VREgen$TotalSub),digits=2))
  totGEN <- c( round(sum(VREgen$TotalGen),digits=2))
  avg_pr1 <- c(avg_price_i)
  avg_pr2 <- c(avg_price_ii)
  resload_weighted_price <- resload_weighted
  
  displayTable <- data.frame(windShare, solarShare, MVwind, MVsolar, LCOEwind, LCOEsolar, avg_pr_wNegpr_woSUB, avg_pr_woNegpr_woSUB, totSUB, totGEN, avg_pr1, avg_pr2, resload_weighted_price)
  
  TableList[[i]] <- displayTable
  
  i=i+1
}

Table <- rbind(TableList[[1]], TableList[[2]], TableList[[3]])
if (storTag == 0) {
write.table(Table, paste0(mypath,"price_result_list_woStor.csv"), sep = ";", row.names = F)
}
if (storTag == 1) {
  write.table(Table, paste0(mypath,"price_result_list_wStor.csv"), sep = ";", row.names = F)
}
price_hr_1 = dftot[[1]]
price_hr_2 = dftot[[2]]
price_hr_3 = dftot[[3]]

load_price1 = dftot2[[1]]
load_price2 = dftot2[[2]]
load_price3 = dftot2[[3]]

  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  n = 3
  cols = gg_color_hue(n)

  ymin1 = sort(price_hr_1$value)[2]
  ymin2 = sort(price_hr_2$value)[2]
  ymin3 = sort(price_hr_3$value)[2]
  
  ymax1 = sort(price_hr_1$value)[length(price_hr_1$value)-1]
  ymax2 = sort(price_hr_2$value)[length(price_hr_2$value)-1]
  ymax3 = sort(price_hr_3$value)[length(price_hr_3$value)-1]

  xlim = c(min(ymin1,ymin2,ymin3)-10,200)
  # xlim = c(-10, 130)
  print(xlim)

  price_hr_1$share<- "VRE25%"
  price_hr_2$share<- "VRE50%"
  price_hr_3$share<- "VRE70%"
  
  ggplot(data=price_hr_1) +
    geom_histogram(aes(value, fill = share, color= share), binwidth = 15, alpha = 0.5)+
    geom_vline(aes(xintercept = load_price1, color = share), linetype="dashed", size=1)+
    ggtitle("Electricity price distribution (dashed line: average price)")+
    xlab("electricity price (USD/MWh)") + ylab("number of hours in a year")+
    scale_x_continuous(limits=xlim)+
    scale_y_continuous(limits=c(0,6000))+
    scale_fill_manual(values = cols)+
    # xlim(c(-10,200)) +
    # ylim(c(0,6000))+
    theme(plot.title =element_text(size=9), legend.title = element_blank(),legend.position = c(.85, .55)) 
  
  if (storTag == 0) {
    ggsave(filename =paste0(mypath, "Price_frequency1_wostormkupwNP.png"),  width = 4.5, height =5.5, units = "in", dpi = 220)
  }
  
  if (storTag == 1) {
    ggsave(filename =paste0(mypath, "Price_frequency1_wstormkupwNP.png"),  width = 4.5, height =5.5, units = "in", dpi = 220)
  }
  
  ggplot() +
    geom_histogram(data=price_hr_1, aes(value, fill = share, color= share), binwidth = 15, alpha = 0.5)+
    geom_histogram(data=price_hr_2, aes(value, fill = share, color= share), binwidth = 15, alpha = 0.5)+
    geom_vline(data=price_hr_1, aes(xintercept=load_price1, color = share), linetype="dashed", size=1)+
    geom_vline(data=price_hr_2, aes(xintercept=load_price2, color = share), linetype="dashed", size=1)+
    ggtitle("Electricity price distribution (dashed line: average price)")+
    scale_x_continuous(limits=xlim)+
    scale_y_continuous(limits=c(0,6000))+
    scale_fill_manual(values = cols)+
    xlab("electricity price (USD/MWh)") + ylab("number of hours in a year")+
    theme(plot.title =element_text(size=9), legend.title = element_blank(),legend.position = c(.85, .55))
  
  
  if (storTag == 0) {
    ggsave(filename =paste0(mypath, "Price_frequency2_wostormkupwNP.png"),  width = 4.5, height =5.5, units = "in", dpi = 220)
  }
  
  if (storTag == 1) {
    ggsave(filename =paste0(mypath, "Price_frequency2_wstormkupwNP.png"),  width = 4.5, height =5.5, units = "in", dpi = 220)
  }
  
  ggplot() +
    geom_histogram(data=price_hr_1, aes(value, fill = share, color= share), binwidth = 15, alpha = 0.5)+
    geom_histogram(data=price_hr_2, aes(value, fill = share, color= share), binwidth = 15, alpha = 0.5)+
    geom_histogram(data=price_hr_3, aes(value, fill = share, color= share), binwidth = 15, alpha = 0.5)+
    geom_vline(data=price_hr_1, aes(xintercept=load_price1, color = share), linetype="dashed", size=1)+
    geom_vline(data=price_hr_2, aes(xintercept=load_price2, color = share), linetype="dashed", size=1)+
    geom_vline(data=price_hr_3, aes(xintercept=load_price3, color = share), linetype="dashed", size=1)+
    ggtitle("Electricity price distribution (dashed line: average price)")+
    scale_x_continuous(limits=xlim)+
    scale_y_continuous(limits=c(0,6000))+
    scale_fill_manual(values = cols)+
    xlab("electricity price (USD/MWh)") + ylab("number of hours in a year")+
    theme(plot.title =element_text(size=9), legend.title = element_blank(),legend.position = c(.85, .55)) 
  
  if (storTag == 0) {
  ggsave(filename =paste0(mypath, "Price_frequency3_wostormkupwNP.png"),  width = 4.5, height =5.5, units = "in", dpi = 220)
  }
  
  if (storTag == 1) {
    ggsave(filename =paste0(mypath, "Price_frequency3_wstormkupwNP.png"),  width = 4.5, height =5.5, units = "in", dpi = 220)
  }
  
  
  
  price_hr_plot_1 <- price_hr_1 %>% arrange(desc(value)) 
  price_hr_plot_1$sorted_x <- seq(1, 8760)
  
  price_hr_plot_2 <- price_hr_2 %>% arrange(desc(value)) 
  price_hr_plot_2$sorted_x <- seq(1, 8760)
  
  price_hr_plot_3 <- price_hr_3 %>% arrange(desc(value)) 
  price_hr_plot_3$sorted_x <- seq(1, 8760)  
  
  ylim <- c(-10,200)
  
  price_hr_plot_1$share       <- "VRE25%"
  price_hr_plot_2$share       <- "VRE50%"
  price_hr_plot_3$share <- "VRE70%"
  
  p <- ggplot() +
    geom_line(data = price_hr_plot_1, aes(x = sorted_x, y = value, color = share), size = 3.2, alpha = 0.8) +
    theme(plot.title =element_text(size=22), axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold"), 
          legend.text = element_text(size = 15),
          legend.key.size = unit(1.5, "cm"),legend.title = element_blank(),legend.position = c(.85, .85)) +
    xlab("sorted hour") + ylab("electricity price (USD/MWh)")+ggtitle("Price duration curves for various shares of Variable REnewables")+
    coord_cartesian( ylim = ylim, xlim = c(0, 8760))
  
  ggsave(filename = paste0(mypath, "Duration_curve_line_wstor1.png"),  width = 10, height =10, units = "in", dpi = 120)
  
  p <- ggplot() +
    geom_line(data = price_hr_plot_1, aes(x = sorted_x, y = value, color = share), size = 3.2, alpha = 0.8) +
    geom_line(data = price_hr_plot_2, aes(x = sorted_x, y = value, color = share), size = 3.2, alpha = 0.8) +
    theme(plot.title =element_text(size=22), axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold"), 
          legend.text = element_text(size = 15),
          legend.key.size = unit(1.5, "cm"),legend.title = element_blank(),legend.position = c(.85, .85)) +
    xlab("sorted hour") + ylab("electricity price (USD/MWh)")+ggtitle("Price duration curves for various shares of Variable REnewables")+
    coord_cartesian( ylim = ylim, xlim = c(0, 8760))
  
  ggsave(filename = paste0(mypath, "Duration_curve_line_wstor2.png"),  width = 10, height =10, units = "in", dpi = 120)
  
  p <- ggplot() +
    geom_line(data = price_hr_plot_1, aes(x = sorted_x, y = value, color = share), size = 3.2, alpha = 0.8) +
    geom_line(data = price_hr_plot_2, aes(x = sorted_x, y = value, color = share), size = 3.2, alpha = 0.8) +
    geom_line(data = price_hr_plot_3, aes(x = sorted_x, y = value, color = share), size = 3.2, alpha = 0.8) +
    theme(plot.title =element_text(size=22), axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold"), 
          legend.text = element_text(size = 15),
          legend.key.size = unit(1.5, "cm"),legend.title = element_blank(),legend.position = c(.85, .85)) +
    xlab("sorted hour") + ylab("electricity price (USD/MWh)")+ggtitle("Price duration curves for various shares of Variable REnewables")+
    coord_cartesian( ylim = ylim, xlim = c(0, 8760))
  
  ggsave(filename = paste0(mypath, "Duration_curve_line_wstor3.png"),  width = 10, height =10, units = "in", dpi = 120)
  
  