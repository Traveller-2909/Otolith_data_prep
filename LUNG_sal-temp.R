#General dealing with large LUNG datafiles

library(here)
library(tidyverse)
here::i_am("LUNG_sal-temp.R")

Lungdata<- read.delim("LUNG_Temp_Rügensche_Bodden_1970-2018.txt", header = T, stringsAsFactors = F)

Lungdata20 <- read.csv2("LUNG-Daten_Tw_Sal_2020.CSV", header = T, stringsAsFactors = F)

Lungdata_T_S <- Lungdata20%>%
  filter(Parameter=="Wassertemperatur"|Parameter == "Salzgehalt")

Lungdata_T_S[27002,11] <- mean(Lungdata_T_S[27002,11], Lungdata_T_S[27004,11])
Lungdata_T_S[27003,11] <- mean(Lungdata_T_S[27003,11],Lungdata_T_S[27005,11])
Lungdata_T_S <- Lungdata_T_S[-27005,]
Lungdata_T_S <- Lungdata_T_S[-27004,]

Lungdata_T_S <- spread(Lungdata_T_S, Parameter, Endergebnis)

Lungdata_T_S$Salzgehalt <- as.numeric(Lungdata_T_S$Salzgehalt)
Lungdata_T_S$Wassertemperatur <- as.numeric(Lungdata_T_S$Wassertemperatur)

Lungdata_T_S_clean <- Lungdata_T_S%>%
  group_by(Probenahmedatum, Tiefeninformation)%>%
  mutate(temperature = mean(Wassertemperatur, na.rm = T),
         salinity = mean(Salzgehalt, na.rm = T))%>%
  ungroup()%>%
  transmute(mstnr = PN.Stellennr.,
            station = substr(PN.Stellenbezeichnung,1,4),
            temperature =temperature,
            salinity = salinity,
            unit = Einheit,
            depth = Tiefeninformation,
            Lat = Lat,
            Long = Long)%>%
  distinct()

Lungdata20mean <- Lungdata_T_S_clean %>%
  group_by(station)%>%
  summarise(temperature = round(mean(temperature),2),
            salinity = round(mean(salinity),2),
            Lat = Lat,
            Long = Long)%>%
  distinct()

write.csv2(Lungdata20mean, file = "Mean_T_S_2020.csv", row.names = F)

Lungdata_T_S_clean[grepl("DB", Lungdata_T_S_clean$station),]$turbidity <- Lungdata_T_S_clean[grepl("DB", Lungdata_T_S_clean$station),]$Long
Lungdata_T_S_clean[grepl("DB", Lungdata_T_S_clean$station),]$Long <- Lungdata_T_S_clean[grepl("DB", Lungdata_T_S_clean$station),]$Lat
Lungdata_T_S_clean[grepl("DB", Lungdata_T_S_clean$station),]$Lat <- Lungdata_T_S_clean[grepl("DB", Lungdata_T_S_clean$station),]$depth
Lungdata_T_S_clean[grepl("DB", Lungdata_T_S_clean$station),]$depth <- as.numeric(Lungdata_T_S_clean[grepl("DB", Lungdata_T_S_clean$station),]$unit)
Lungdata_T_S_clean[grepl("DB", Lungdata_T_S_clean$station),]$unit <- paste("°C")

Lungdata_T_S_clean[grepl("GB1", Lungdata_T_S_clean$station),]$Lat <- 54.18833
Lungdata_T_S_clean[grepl("GB1", Lungdata_T_S_clean$station),]$Long <- 13.775

Lungdata_T_S_month <- Lungdata_T_S_clean%>%
  group_by(station, year, month, depth)%>%
  summarise(year = year,
            temperature = mean(temperature),
            salinity = mean(salinity),
            depth = factor(ifelse(depth > 1.5, 0, 1), levels = c(0,1), labels = c("bottom", "surface")),
            Lat = first(Lat),
            Long = first(Long),
            turbidity = mean(turbidity))%>%
  ungroup()%>%
  distinct()

Lungdata_T_S_month2 <- Lungdata_T_S_month%>%
  group_by(station, year, month, depth)%>%
  summarise(temperature = mean(temperature),
            salinity = mean(salinity),
            Lat = first(Lat),
            Long = first(Long))%>%
  distinct()%>%
  ungroup()

Months_WRB <- Lungdata_T_S_month2%>%
  filter(Lat >= 54.40167 & Lat <= 54.62333)%>%
  filter(Long > 13.05 & Long < 13.17833)%>%
  filter(year >= 2020-15)%>%
  group_by(station, month, depth)%>%
  summarise(temperature = median(temperature),
            salinity = median(salinity),
            Lat = Lat,
            Long = Long,
            depth = depth,
            area = "WRB")%>%
  distinct()

Months_NRB <- Lungdata_T_S_month2%>%
  filter(Lat >= 54.50667 & Lat <= 54.56333)%>%
  filter(Long >= 13.17833 & Long <= 13.49)%>%
  filter(year >= 2020-15)%>%
  group_by(station, month, depth)%>%
  group_by(station, month, depth)%>%
  summarise(temperature = median(temperature),
            salinity = median(salinity),
            Lat = Lat,
            Long = Long,
            depth = depth,
            area = "WRB")%>%
  distinct()

Months_GB <- Lungdata_T_S_month2%>%
  filter(Lat >= 54.15 & Lat <= 54.26667)%>%
  filter(Long >= 13.48333  & Long <= 13.775 )%>%
  filter(year >= 2020-15)%>%
  group_by(station, month, depth)%>%
  summarise(temperature = median(temperature),
            salinity = median(salinity),
            Lat = Lat,
            Long = Long,
            depth = depth,
            area = "WRB")%>%
  distinct()

Months_areas <- rbind(Months_GB, Months_NRB, Months_WRB)%>%
  group_by(area, month, depth)%>%
  summarise(temperature = mean(temperature),
            salinity = mean(salinity))

