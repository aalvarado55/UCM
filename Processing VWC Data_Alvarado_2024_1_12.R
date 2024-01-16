# Libraries --------------------------------------------------------------------
library(readr)
library(lubridate)
library(zoo)
library(dplyr)
library(ggplot2)
library(readxl)
library(float)
library(XLConnect)
library(reshape)
library(reshape2)
library(MASS)
# Creating list of files-------------------------------------------------------

#Set Working Directory so that files save back in folder
setwd('C:/Users/aalvarado55/OneDrive - University of California Merced/00_Alvarado_ESF Junior Specialist/03_Data/02_Soil Moisture_Volumetric Water Content/')

soil_Em501 <- list.files("RawData/", pattern = "^Em50-1")
soil_Em501

soil_Em502 <- list.files("RawData/", pattern = "^Em50-2")
soil_Em502


# Reading and modifying files EM501---------------------------------------------
moist_Em501 <- data.frame() #empty dataframe for saving each for loop result
for(a in soil_Em501){
#a <-soil_Em501[2]
whatwewant <-  readxl::read_excel(paste("RawData/", a,sep=""),sheet = 1,col_names = F)
portanames <- whatwewant[1,]
ports_yes <- which(portanames != "Port 1") #All ports that are not port 1
whatwewant<- whatwewant[,ports_yes] #selecting ports columns in the file
moist_cols <- which(whatwewant[3,]== "m³/m³ VWC")
whatwewant2 <- whatwewant[,c(1,moist_cols)] #selecting the columns with moisture data

#Transforming the data frame into the three column format
moist_data <- data.frame(whatwewant2)
colnames(moist_data)<- moist_data[1,]
moist_data2 <- melt(moist_data[-c(1:3),],id = c("Em50-1"))
colnames(moist_data2) <- c("Date","Port","Moisture")
#Changing the data format from the excel file
moist_data2[,1] <- as.Date(as.numeric(moist_data2$Date), origin = "1899-12-30")
#Saving info from file and joining to the other ones
moist_Em501<- rbind(moist_Em501,moist_data2)
}


# Reading and modifying files EM502---------------------------------------------
moist_Em502 <- data.frame() #empty dataframe for saving each for loop result
for(a in soil_Em502){

  whatwewant <-  readxl::read_excel(paste("RawData/", a,sep=""),sheet = 1,col_names = F)
  portanames <- whatwewant[1,]
  ports_yes <- which(portanames != "Port 1") #All ports that are not port 1
  whatwewant<- whatwewant[,ports_yes] #selecting ports columns in the file
  moist_cols <- which(whatwewant[3,]== "m³/m³ VWC")
  whatwewant2 <- whatwewant[,c(1,moist_cols)] #selecting the columns with moisture data
  
  #Transforming the data frame into the three column format
  moist_data <- data.frame(whatwewant2)
  colnames(moist_data)<- moist_data[1,]
  moist_data2 <- melt(moist_data[-c(1:3),],id = c("Em50-1"))
  colnames(moist_data2) <- c("Date","Port","Moisture")
  #Changing the data format from the excel file
  moist_data2[,1] <- as.Date(as.numeric(moist_data2$Date), origin = "1899-12-30")
  #Saving info from file and joining to the other ones
  moist_Em502<- rbind(moist_Em502,moist_data2)
}


# Reformat data frame to calculate daily mean VWC values for EM501--------------

whatweneed <- cast(moist_Em501,Date~Port, mean)



#FOR REFERENCE: melt(moist_data[-c(1:3),],id = c("Em50-1"))


#-------------------------------------------------------------




#Organize points by day and calculate daily precipitation
moist_Em501_reshape <- dcast(moist_Em501,Date~Port)

soilmoist_daily <- moist_Em501 %>%
  group_by(Date, Port) %>%
  summarise(mean = mean(Moisture))
     

soilmoist_daily <- summarize(moist_Em501, .by = c(d) )




# %>%
#   reshape2::melt(., id = c("Em50.2"))
# soilmoist_daily