library(readr)
library(lubridate)
library(zoo)
library(dplyr)
library(ggplot2)

#Read Zentra data file
Precip <- read.csv('C:/Users/aalvarado55/OneDrive - University of California Merced/00_Alvarado_ESF Junior Specialist/03_Data/Zentra/Alvarado_Zentra_2023.11.23_vers2.csv')[,c(1,3)]

#Set Working Directory so that files save back in folder
setwd('C:/Users/aalvarado55/OneDrive - University of California Merced/00_Alvarado_ESF Junior Specialist/03_Data/Zentra/')

#Set Timestamp column as readable date format
Precip$ï..Timestamps <- as.Date(Precip$ï..Timestamps, "%m/%d/%Y")

#Organize points by day and calculate daily precipitation
Precip_daily <- Precip %>%
  group_by(`ï..Timestamps`) %>%
  summarize(`mm.Precipitation` = sum(`mm.Precipitation`))

#Organize points by month and calculate monthly precipitation
Precip_monthly <- Precip_daily %>%
  mutate(Month = as.yearmon(`ï..Timestamps`, "%m/%Y")) %>%
  group_by(Month) %>%
  summarize(`Precipitation` = sum(`mm.Precipitation`))

#Create line plot
lineplot <- ggplot(Precip_monthly, aes(x = Month, y= Precipitation)) +
  geom_line(color = "blue") + labs(title = "Monthly Precipitation", y = "Precipitation (mm/month)", x = "") +
  scale_y_continuous(expand = c(0,0)) +
  #scale_x_continuous(expand = c(0,0)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave(lineplot, filename =  "MonthlyPrecipitation_20231123.png", dpi = 300,units="in", width=8,height=5)
