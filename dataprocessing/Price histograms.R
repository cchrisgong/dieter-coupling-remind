#---- INITIALISATION ----------


setwd("~/PhD/Modelling/Histogram plotting")


# install.packages("tidyr")
# install.packages("dplyr")
# install.packages ("ggplot2")
# install.packages ("data.table")
# install.packages("xlsx")
# install.packages("readxl")
# install.packages("tibble")
#install.packages("ggpubr")

library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(xlsx)
library(readxl)
library(tibble)
library(reshape2)
library(gdx)
library(quitte)
library(zoo)
library(stringr)
library(dygraphs)
library(plyr)
library(ggpubr)
theme_set(theme_pubr())

#---- FUNCTIONS ----------


#---------READ DATA ----------



prices_2016         <- read.xlsx(file = "Prices_2016.xlsx", sheetName = "Sheet1") # 
prices_2017         <- read.xlsx(file = "Prices_2017.xlsx", sheetName = "Sheet1") #
prices_2018         <- read.xlsx(file = "Prices_2018.xlsx", sheetName = "Sheet1") #
prices_DIETER   <- read.xlsx(file = "Prices_2040_India.xlsx", sheetName = "Sheet1") #


colours_data <- c('2016' = 'red',
                  '2017' = 'blue',
                  '2018' = 'green',
                  'DIETER' = 'brown'
)
                 

prices_2016$year       <- "2016"
prices_2017$year       <- "2017"
prices_2018$year       <- "2018"
prices_DIETER$year <- "DIETER"

electricity_price <- rbind(prices_2016,
                           prices_2017,
                           prices_2018,
                           prices_DIETER
                           
)
  

mean_price <- ddply(electricity_price, "year", summarise, grp.mean=mean(PRICE))






p= ggplot(electricity_price, aes(PRICE, fill = year)) + 
  geom_density(alpha = 0.6)+
  #geom_histogram(aes(y =..density.., fill = year), binwidth = 1, alpha = 0.7) +
  labs(title="Histogram for Electricity Price", x="Price EUR/MWh", y="Density") + 
  geom_vline(aes(xintercept= 31.6 ),linetype="dashed", size=1)+
  annotate(geom="text", x=5, y=0.05, label="Base price = 31.6 (day-ahead)",        size=6)+
  xlim(c(-50,100)) + 
  ylim(c(0,0.05))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(text = element_text(size=20))

ggsave(filename = 'price hist.png', plot = p)
  
 

