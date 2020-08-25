
mypath = "~/DIETER/myFirstDIETER/dataprocessing/"
mydatapath = "~/DIETER/myFirstDIETER/DIETER/"

# import library

source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)
# specify output file

#===========================================

# testfile = "results70.gdx"
testfile = "results70_flex.gdx"
# hourly_reportCSV = read.csv(paste0(mypath, "results70_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportCSV = read.csv(paste0(mypath, "results70_flex_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
QUITTobj2 = hourly_reportQUITT 

price_hr_2<- QUITTobj2 %>% 
  filter(variable == "Wholesale electricity price") %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))%>% 
  mutate(value = value*(value>0))

# price_hr_2$DayNight <- ifelse(and(price_hr_2$hour%%24 >=7, price_hr_2$hour%%24 <19), "DAY", "NIGHT")
price_Hr_plot2 <- price_hr_2 %>% arrange(desc(value)) 
price_Hr_plot2$sorted_x <- seq(1, 8760)

#===========================================
# testfile = "results50.gdx"
testfile = "results50_flex.gdx"
# hourly_reportCSV = read.csv(paste0(mypath, "results50_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportCSV = read.csv(paste0(mypath, "results50_flex_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
QUITTobj3 = hourly_reportQUITT

price_hr_3 <- QUITTobj3 %>% 
  filter(variable == "Wholesale electricity price") %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour))  %>% 
  mutate(value = value*(value>0))

price_hr_3$DayNight <- ifelse(and(price_hr_3$hour%%24 >=7, price_hr_3$hour%%24 <19), "DAY", "NIGHT")
price_Hr_plot3 <- price_hr_3 %>% arrange(desc(value)) 
price_Hr_plot3$sorted_x <- seq(1, 8760)

#===========================================
# testfile = "results25.gdx"
testfile = "results25_flex.gdx"
# hourly_reportCSV = read.csv(paste0(mypath, "results25_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportCSV = read.csv(paste0(mypath, "results25_flex_hourlyreport.csv"), sep = ';', header = T, stringsAsFactors = F)
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 
QUITTobj4 = hourly_reportQUITT

price_hr_4<- QUITTobj4 %>% 
  filter(variable == "Wholesale electricity price") %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour)) 
price_hr_4$DayNight <- ifelse(and(price_hr_4$hour%%24 >=7, price_hr_4$hour%%24 <19), "DAY", "NIGHT")
price_Hr_plot4 <- price_hr_4 %>% arrange(desc(value)) 
price_Hr_plot4$sorted_x <- seq(1, 8760)

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
  
  rainbow.pal <- rev(rainbow(100, start = 0.63, end = 0.62))
  
  ylim <- c(-10,140)

  price_Hr_plot4$share       <- "VRE25%"
  price_Hr_plot3$share       <- "VRE50%"
  price_Hr_plot2$share <- "VRE70%"
  
    p <- ggplot() +
      geom_line(data = price_Hr_plot4, aes(x = sorted_x, y = value, color = share), size = 3.2, alpha = 0.8) +
      # scale_fill_gradientn(colours  = rainbow.pal,
      #                      limits = c(1, 8760),
      #                      breaks = (c(1,7,13,19,23)+.3) * 8760 / 24,
      #                      labels = c("Jan", "Apr", "Jul", "Oct", "Dec"),
      #                      name = "") +
      theme(plot.title =element_text(size=22), axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold"), 
            legend.text = element_text(size = 15),
            legend.key.size = unit(1.5, "cm"),legend.title = element_blank(),legend.position = c(.85, .85)) +
      xlab("sorted hour") + ylab("electricity price (USD/MWh)")+ggtitle("Price duration curves for various shares of Variable REnewables")+
      coord_cartesian( ylim = ylim, xlim = c(0, 8760))
      
  ggsave(filename = paste0(mypath, "Duration_curve_line_wstor1.png"),  width = 10, height =10, units = "in", dpi = 120)

  p <- ggplot() +
    geom_line(data = price_Hr_plot4, aes(x = sorted_x, y = value, color = share), size = 3.2, alpha = 0.8) +
    geom_line(data = price_Hr_plot3, aes(x = sorted_x, y = value, color = share), size = 3.2, alpha = 0.8) +
    theme(plot.title =element_text(size=22), axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold"), 
          legend.text = element_text(size = 15),
          legend.key.size = unit(1.5, "cm"),legend.title = element_blank(),legend.position = c(.85, .85)) +
    xlab("sorted hour") + ylab("electricity price (USD/MWh)")+ggtitle("Price duration curves for various shares of Variable REnewables")+
    coord_cartesian( ylim = ylim, xlim = c(0, 8760))
  
  ggsave(filename = paste0(mypath, "Duration_curve_line_wstor2.png"),  width = 10, height =10, units = "in", dpi = 120)
  
  p <- ggplot() +
    geom_line(data = price_Hr_plot4, aes(x = sorted_x, y = value, color = share), size = 3.2, alpha = 0.8) +
    geom_line(data = price_Hr_plot3, aes(x = sorted_x, y = value, color = share), size = 3.2, alpha = 0.8) +
    geom_line(data = price_Hr_plot2, aes(x = sorted_x, y = value, color = share), size = 3.2, alpha = 0.8) +
    theme(plot.title =element_text(size=22), axis.text=element_text(size=20), axis.title=element_text(size= 20,face="bold"), 
          legend.text = element_text(size = 15),
          legend.key.size = unit(1.5, "cm"),legend.title = element_blank(),legend.position = c(.85, .85)) +
    xlab("sorted hour") + ylab("electricity price (USD/MWh)")+ggtitle("Price duration curves for various shares of Variable REnewables")+
    coord_cartesian( ylim = ylim, xlim = c(0, 8760))
  
  ggsave(filename = paste0(mypath, "Duration_curve_line_wstor3.png"),  width = 10, height =10, units = "in", dpi = 120)

#   p <- ggplot() +
#     geom_line(data = price_Hr_plot4, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "purple") +
#     geom_col(data = price_Hr_plot4, aes(x = sorted_x, y = value, fill = DayNight ), alpha = 0.8, position = "identity", width = 0.2) +
#     annotate(geom = "text", x = 7000, y = 120, label = "25%VRE", color = "purple",  size=20)+
#     # geom_line(data = price_Hr_plot3, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "dark green") +
#     # geom_col(data = price_Hr_plot3, aes(x = sorted_x, y = value, fill = DayNight ), alpha = 0.8, position = "identity", width = 0.2) +
#     # annotate(geom = "text", x = 7000, y = 120, label = "50%VRE", color = "dark green",  size=20)+
#     # geom_line(data = price_Hr_plot2, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "blue") +
#     # geom_col(data = price_Hr_plot2, aes(x = sorted_x, y = value, fill = DayNight ), alpha = 0.8, position = "identity", width = 0.2) +
#     # annotate(geom = "text", x = 7000, y = 120, label = "70%VRE", color = "blue",  size=20)+
#     # geom_line(data = price_Hr_plot1, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "orange") +
#     # geom_col(data = price_Hr_plot1, aes(x = sorted_x, y = value, fill = DayNight ), alpha = 0.8, position = "identity", width = 0.2) +
#     # annotate(geom = "text", x = 7000, y = 120, label = "90%VRE", color = "orange",  size=20)+
#     
#     scale_fill_brewer(palette="Set1")+
#     theme(plot.title =element_text(size=45), axis.text=element_text(size=40), axis.title=element_text(size= 40,face="bold"), 
#           legend.text = element_text(size = 28),
#           legend.title = element_text(size = 28),
#           legend.key.size = unit(1.5, "cm")) +
#     xlab("sorted hour") + ylab("electricity price (USD/MWh)")+ggtitle("Price duration curves for various shares of Variable REnewables")+
#     coord_cartesian( ylim = ylim, xlim = c(0, 8760))
#   
# ggsave(filename =paste0(mypath, "Duration_curve_DAYNIGHT_1.png"),  width = 25, height =25, units = "in", dpi = 120)

# p <- ggplot() +
#   geom_line(data = price_Hr_plot4, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "purple") +
#   annotate(geom = "text", x = 8000, y = 120, label = "25%VRE", color = "purple",  size=10)+
#   geom_line(data = price_Hr_plot3, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "dark green") +
#   annotate(geom = "text", x = 6000, y = 120, label = "50%VRE", color = "dark green",  size=10)+
#   geom_line(data = price_Hr_plot2, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "blue") +
#   annotate(geom = "text", x = 4000, y = 120, label = "70%VRE", color = "blue",  size=10)+
#   geom_line(data = price_Hr_plot1, aes(x = sorted_x, y = value), size = 3.2, alpha = 0.8, color = "orange") +
#   annotate(geom = "text", x = 2000, y = 120, label = "90%VRE", color = "orange",  size=10)+
#   
#   theme(plot.title =element_text(size=22), axis.text=element_text(size=25), axis.title=element_text(size= 25,face="bold"), 
#         legend.text = element_text(size = 15), legend.title = element_text(size = 25)) +
#   xlab("sorted hour") + ylab("electricity price (USD/MWh)")+ggtitle("Price duration curves for various shares of Variable REnewables")+
#   coord_cartesian( ylim = ylim, xlim = c(0, 8760))
# 
# ggsave(filename =paste0(mypath, "Duration_curve_lines.png"),  width = 10, height =10, units = "in", dpi = 120)
