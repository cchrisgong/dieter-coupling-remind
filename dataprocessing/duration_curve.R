
mypath = "~/DIETER/myFirstDIETER/dataprocessing/"
  
# import library

source(paste0(mypath, "library_import.R"))
source(paste0(mypath, "GDXtoQuitte.R"))
library(readr)
# specify output file
testfile = "~/DIETER/myFirstDIETER/DIETER/results_mSS_exoWS_PtG-ONLY.gdx"
gdxToQuitte(testfile)
# annual_reportCSV = read.csv("/home/chengong/DIETER/myFirstDIETER/dataprocessing/annual_report.csv", sep = ',', header = T, stringsAsFactors = F)

hourly_reportCSV = read.csv(paste0(mypath, "hourly_report.csv", sep = ',', header = T, stringsAsFactors = F))
hourly_reportQUITT <- as.quitte(hourly_reportCSV) 

QUITTobj = hourly_reportQUITT

#####################################################
#plot hourly price duration curve, along with the sorted variable (VARkey) curve of one technology "TECH" in quitte object "QUITTobj", for PV share, Wind On share, Wind Off share, and other parameters
duration_curve_Production_curve <- function(QUITTobj, VARkey, TECHkey, pPVshar = 0, pWindOnshare = 0, pWindOffshare = 0, pTECHshare = 0){

  pPVshar = 30; pWindOnshare = 11; pWindOffshare = 0; pTECHshare = 0; VARkey = "Hourly storage demand"; TECHkey = "Power-to-gas"
  
  # select price
  price_hr <- QUITTobj %>% 
  filter(variable == "Wholesale electricity price") %>% 
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour)) %>%
  select("variable", "scenario", "period", "hour", "unit", "tech", "value") %>%
  # decompose scenarios into various tech shares, and make them into numeric values and filter by the parameter shares
  separate(scenario, c("scenario", "PVshare", "WindOnshare", "WindOffshare", "P2Gshare"), "_") %>% 
  mutate(PVshare = parse_number(PVshare)) %>% mutate(WindOnshare = parse_number(WindOnshare)) %>% 
  mutate(WindOffshare = parse_number(WindOffshare)) %>% mutate(P2Gshare = parse_number(P2Gshare)) %>% 
  mutate(PVshare = as.numeric(PVshare)) %>%
  mutate(WindOnshare = as.numeric(WindOnshare))%>% 
  filter(PVshare == pPVshar) %>% 
  filter(WindOnshare == pWindOnshare)
  # complete the hours (since in case of zero entries, not read by gdx reader)
  
  # select production variable and technology
  techProd_hr <- QUITTobj %>% 
  filter(variable == VARkey) %>%
  filter(tech == TECHkey) %>%
  mutate(hour = str_replace(hour, hour, str_sub(hour, 2, -1))) %>% 
  mutate(hour = as.numeric(hour)) %>%
  select("variable", "scenario", "period", "hour", "unit", "tech", "value") %>%
  # decompose scenarios into various tech shares, and make them into numeric values and filter by the parameter shares
  separate(scenario, c("scenario", "PVshare", "WindOnshare", "WindOffshare", "P2Gshare"), "_") %>% 
  mutate(PVshare = parse_number(PVshare)) %>% mutate(WindOnshare = parse_number(WindOnshare)) %>% 
  # complete the table (zero entries which were not read by read.gdx is now filled in)
  mutate(PVshare = as.numeric(PVshare)) %>%
  mutate(WindOnshare = as.numeric(WindOnshare))%>% 
  filter(PVshare == pPVshar) %>% 
  filter(WindOnshare == pWindOnshare)

price_hr$DayNight <- ifelse(and(price_hr$hour%%24 >=7, price_hr$hour%%24 <19), "DAY", "NIGHT")

price_hr$tech_Prod <- techProd_hr$value

price_Hr_plot <- price_hr  %>% arrange(desc(value)) 
price_Hr_plot$sorted_x <- seq(1, 8760)
max_price <- max(price_Hr_plot$value)
mean_price= mean(price_Hr_plot$value)

max_techProd= max(techProd_hr$value)



rainbow.pal <- rev(rainbow(100, start = 0.63, end = 0.62))
max_price_plot = 140

p1<-ggplot(data = price_Hr_plot) +
  geom_line(aes(x = sorted_x, y = value ), size = 1.2, alpha = 0.5, color = "red") +
  coord_cartesian(expand = FALSE,  ylim = c(0, max_price_plot)) +
  geom_col(aes(x = sorted_x, y = value, fill = hour ), position = "identity", width = 0.2) +
  scale_fill_gradientn(colours  = rainbow.pal,
                       limits = c(1, 8760),
                       breaks = (c(1,7,13,19,23)+.3) * 8760 / 24,
                       labels = c("Jan", "Apr", "Jul", "Oct", "Dec"),
                       name = "") +
  geom_point(aes(x = sorted_x, y = 0.9 * max_price_plot * tech_Prod/max_techProd), color = "green", size = 5) +
  annotate(geom = "text", x = 2000, y = 0.7 * max_price_plot, label = paste0("mean price = ",round(mean_price)), color = "purple",  size=15) +
  annotate(geom = "text", x = 2000, y = 0.9 * max_price_plot , label = paste0("max", VARkey," =", round(max_techProd)), color = "green",  size=15) +
  scale_y_continuous(sec.axis = sec_axis(~.*max_techProd/(0.9 * max_price_plot), name = "GW")) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10,face="bold")) +
  xlab("hour") + ylab("electricity price")

ggsave(filename = paste0("Duration_curve_", pPVshar, "_", pWindOnshare, "_", pWindOffshare, "_", pTECHshare, ".png"),  width = 25, height =25, units = "in", dpi = 120)


p2<-ggplot(data = price_Hr_plot) +
  geom_line(aes(x = sorted_x, y = value ), size = 1.2, alpha = 0.5, color = "red") +
  coord_cartesian(expand = FALSE,  ylim = c(0, max_price_plot)) +
  geom_col(aes(x = sorted_x, y = value, fill = DayNight,), position = "identity", width = 0.2) +
  scale_fill_brewer(palette="Set1")+
  geom_point(aes(x = sorted_x, y = 0.9 * max_price_plot * tech_Prod/max_techProd), color = "green", size = 5) +
  annotate(geom = "text", x = 2000, y = 0.7 * max_price_plot, label = paste0("mean price = ", round(mean_price)), color = "purple",  size=15) +
  annotate(geom = "text", x = 2000, y = 0.8 * max_price_plot, label = paste0("max", VARkey," =", round(max_techProd)), color = "blue",  size=15) +
  scale_y_continuous(sec.axis = sec_axis(~.*max_techProd/(0.9*max_price_plot), name = "GW")) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size= 10, face="bold")) +
  xlab("hour") + ylab("electricity price")
  
ggsave(filename =paste0(mypath, "Duration_curve_", pPVshar, "_", pWindOnshare, "_", pWindOffshare, "_", pTECHshare, "DAYNIGHT.png"),  width = 25, height =25, units = "in", dpi = 120)

}
