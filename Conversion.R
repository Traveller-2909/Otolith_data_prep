#General dealing with large LUNG datafiles
library(lubridate)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(grid)

Lungdata<- read.delim("C:/Users/timor/Nextcloud2/Timo/BODDENHECHT/Chapter 2/Salinity Bernhard/LUNG_Temp_Rügensche_Bodden_1970-2018.txt", header = T, stringsAsFactors = F)

Lungdata_T_S <- Lungdata%>%
  filter(Param_kurz == "W-T"|Parameter=="Wassertemperatur"|Param_kurz == "SAL"|Parameter == "Salzgehalt")

Lungdata_T_S[27002,11] <- mean(Lungdata_T_S[27002,11], Lungdata_T_S[27004,11])
Lungdata_T_S[27003,11] <- mean(Lungdata_T_S[27003,11],Lungdata_T_S[27005,11])
Lungdata_T_S <- Lungdata_T_S[-27005,]
Lungdata_T_S <- Lungdata_T_S[-27004,]

Lungdata_T_S <- spread(Lungdata_T_S, Parameter, WERT_berechnet)

Lungdata_T_S_clean <- Lungdata_T_S%>%
  group_by(DATUM_Uhrzeit, TIEFE)%>%
  mutate(temperature = mean(Wassertemperatur, na.rm = T),
         salinity = mean(Salzgehalt, na.rm = T))%>%
  ungroup()%>%
  transmute(mstnr = MstNr,
            station = substr(Messstelle,1,3),
            year = Jahr,
            day = Tag,
            month = Monat,
            temperature =temperature,
            salinity = salinity,
            unit = Einheit,
            depth = TIEFE,
            Lat = HW_GEO,
            Long = RW_GEO,
            turbidity = SICHTTIEFE)%>%
  distinct()

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


Pike_d18O_pred <- read.delim("C:/Users/timor/Nextcloud2/Timo/BODDENHECHT/Chapter 2/Stats/Otolith visuals/d18O_values_Kloster.txt",
                             header = T, stringsAsFactors = F)

Pike_d18O_pred <- Pike_d18O_pred%>%
  transmute(date = Sampling.date,
            salinity = Salinity,
            temperature = Water.temp...C.,
            d18O = X.18O....,
            Lat = Lat,
            Long = Long)

WRB_average <- Months_WRB%>%
  ungroup()%>%
  group_by(month)%>%
  summarise(temperature = mean(temperature),
            salinity = mean(salinity))

Pike_d18O_pred$month <- lubridate::month(parse_date_time(Pike_d18O_pred$date, orders = c("dmy")))
Pike_d18O_pred$date <- lubridate::as_date(parse_date_time(Pike_d18O_pred$date, orders = c("dmy")))



Pike_d18O_pred <- Pike_d18O_pred %>% inner_join(WRB_average, by = "month")

Pike_d18O_pred$alpha1 = exp((15.99*1000*((Pike_d18O_pred$temperature.x+273.15)^-1)-24.25)/1000)
Pike_d18O_pred$alpha2 = exp((15.99*1000*((Pike_d18O_pred$temperature.y+273.15)^-1)-24.25)/1000)

Pike_d18O_pred$d18O_Oto_1 = 0.97001*(Pike_d18O_pred$alpha1*(1000+Pike_d18O_pred$d18O)-1000)-29.99
Pike_d18O_pred$d18O_Oto_2 = 0.97001*(Pike_d18O_pred$alpha2*(1000+Pike_d18O_pred$d18O)-1000)-29.99

Pike_d18O_pred$alpha3 = exp((18.56*1000*((Pike_d18O_pred$temperature.x+273.15)^-1)-33.49)/1000)
Pike_d18O_pred$alpha4 = exp((18.56*1000*((Pike_d18O_pred$temperature.y+273.15)^-1)-33.49)/1000)


Pike_d18O_pred$d18O_Oto_3 = 0.97001*(Pike_d18O_pred$alpha3*(1000+Pike_d18O_pred$d18O)-1000)-29.99
Pike_d18O_pred$d18O_Oto_4 = 0.97001*(Pike_d18O_pred$alpha4*(1000+Pike_d18O_pred$d18O)-1000)-29.99

Aragonite_prediction <- Pike_d18O_pred%>%
  transmute(month = month,
            date = date,
            salinity_shore = salinity.x,
            temperature_shore = temperature.x,
            salinity_open = salinity.y,
            temperature_open = temperature.y,
            d18O_station = d18O,
            Prediction_Geffen_station_VPDB = d18O_Oto_1,
            Prediction_Patterson_station_VPDB = d18O_Oto_3,
            Prediction_Geffen_WRB_VPDB = d18O_Oto_2,
            Prediction_Patterson_WRB_VPDB = d18O_Oto_4,
            Lat_station = Lat,
            Long_station = Long)%>%
  drop_na()

#Sensitivity & range
alpha = exp((18.56*1000*((20+273.15)^-1)-33.49)/1000)
(0.97001*(alpha*(1000-6.5)-1000)-29.99) - (0.97001*(alpha*(1000-6.5+sd(Aragonite_prediction$d18O_station))-1000)-29.99)

alpha = exp((15.99*1000*((20+273.15)^-1)-24.25)/1000)
(0.97001*(alpha*(1000-6.5)-1000)-29.99) - (0.97001*(alpha*(1000-6.5+sd(Aragonite_prediction$d18O_station))-1000)-29.99)

alpha = exp((18.56*1000*((20+273.15)^-1)-33.49)/1000)
alpha2 = exp((18.56*1000*((20+sd(Aragonite_prediction$temperature_open)+273.15)^-1)-33.49)/1000)
(0.97001*(alpha*(1000-6.5)-1000)-29.99) - (0.97001*(alpha2*(1000-6.5)-1000)-29.99)

alpha = exp((15.99*1000*((20+273.15)^-1)-24.25)/1000)
alpha2 = exp((15.99*1000*((20+sd(Aragonite_prediction$temperature_open)+273.15)^-1)-24.25)/1000)
(0.97001*(alpha*(1000-6.5)-1000)-29.99) - (0.97001*(alpha2*(1000-6.5)-1000)-29.99)

range(Aragonite_prediction$d18O_station)
range(Aragonite_prediction$temperature_open)

alpha = exp((18.56*1000*((15+273.15)^-1)-33.49)/1000)
alpha2 = exp((18.56*1000*((4+273.15)^-1)-33.49)/1000)
(0.97001*(alpha*(1000-5.3)-1000)-29.99) - (0.97001*(alpha2*(1000-5.3)-1000)-29.99)

(0.97001*(alpha*(1000-6.5)-1000)-29.99) - (0.97001*(alpha*(1000-6.5+(-4.2+6.5))-1000)-29.99)

mypallette2 <- brewer.pal(2, "Dark2")

ggplot(Aragonite_prediction)+
  geom_smooth(aes(date, Prediction_Geffen_station_VPDB, color = "Equation 1", lty = "Shore"), alpha = 0)+
  geom_smooth(aes(date, Prediction_Patterson_station_VPDB, color = "Equation 2", lty = "Shore"), alpha = 0)+
  geom_smooth(aes(date, Prediction_Geffen_WRB_VPDB, color = "Equation 1", lty = "Open"), alpha = 0)+
  geom_smooth(aes(date, Prediction_Patterson_WRB_VPDB, color = "Equation 2", lty = "Open"), alpha = 0)+
  ggtitle("Theoretical d18O values in Pike otoliths")+
  scale_x_continuous(breaks = c(seq(1,12,1), seq(1,12,1)))+
  ylab("Predicted d180 relatve to VPDB")+
  scale_color_manual("", breaks = c("Equation 1", "Equation 2"), values = mypallette2)+
  scale_linetype_manual("", breaks = c("Shore", "Open"), values = c("solid", "dotdash"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
  theme_minimal()

Environment <- ggplot(Aragonite_prediction)+
  geom_line(aes(date, salinity_open*2), color = "black")+
  geom_point(aes(date, salinity_open*2), size = 4, color = "darkgrey", pch = 18)+
  geom_line(aes(date, temperature_open), size = 2, color = "black")+
  geom_point(aes(date, temperature_open), size = 4)+
  scale_y_continuous(name = "Temperature [°C]", sec.axis = sec_axis(~./2, name = "Salinity [PSU]"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.y = element_line(size = 1, colour = "black", linetype = 1), axis.title.y = element_text(size = 15),
        axis.ticks= element_line(size = 2), axis.text.y = element_text(size = 15),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())


d18OW <- ggplot(Aragonite_prediction)+
  xlab("Date") + ylab("d18O Water")+
  geom_line(aes(date, d18O_station), size = 2)+
  geom_point(aes(date, d18O_station), size = 4)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(size = 1, colour = "black", linetype = 1), axis.title.y = element_text(size = 15),
        axis.ticks= element_line(size = 2), axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 15), axis.ticks.x = element_line(size = 2), axis.title.x = element_text(size = 15))
  
  
Aragonite <- ggplot(Aragonite_prediction)+
  geom_line(aes(date, Prediction_Geffen_WRB_VPDB), color = "black", size = 1)+
  geom_point(aes(date, Prediction_Geffen_WRB_VPDB), size = 4, pch = 18)+
  geom_line(aes(date, Prediction_Patterson_WRB_VPDB), color = "black", size = 1)+
  geom_point(aes(date, Prediction_Patterson_WRB_VPDB), size = 4)+
  ylab("Predicted d180 relative to VPDB")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.y = element_line(size = 1, colour = "black", linetype = 1), axis.title.y = element_text(size = 10),
        axis.ticks= element_line(size = 2), axis.text.y = element_text(size = 10),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())

Plot_d18O <- ggarrange(Environment, Aragonite, d18OW, ncol = 1, align = "v")
Plot_d18O


text_2020 <- textGrob(expression(bold("2020")), gp=gpar(fontsize=20))
text_2021 <- textGrob(expression(bold("2021")), gp=gpar(fontsize=20))

ggplot(Aragonite_prediction)+
  geom_line(aes(date, Prediction_Geffen_WRB_VPDB), color = "black", size = 1)+
  geom_point(aes(date, Prediction_Geffen_WRB_VPDB, shape = "Marine"), size = 3)+
  geom_line(aes(date, Prediction_Patterson_WRB_VPDB), color = "black", size = 1)+
  geom_point(aes(date, Prediction_Patterson_WRB_VPDB, shape = "Freshwater"), size = 3)+
  scale_y_continuous(limits = c(-6,-1.5), breaks = seq(-6, -1.5, .5))+
  coord_cartesian(clip = "off")+
  annotation_custom(text_2020, xmin = as.Date("2020-05-13"), xmax = as.Date("2020-05-13"), ymin = -6.5,ymax = -7.5) + 
  annotation_custom(text_2021, xmin = as.Date("2021-01-27"), xmax = as.Date("2021-01-27"), ymin = -6.5,ymax = -7.5)+
  scale_x_date(date_breaks = "1 month", date_labels = c("Mar", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb"))+
  scale_shape_manual(name = "Equation:" ,breaks = c("Marine", "Freshwater"), values = c(15, 17))+
  ylab(expression(bold("Modelled araonite"~delta^18*"O"~"(VPDB \211)")))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(size = 1, colour = "black", linetype = 1), axis.title.y = element_text(size = 20, face = "bold"),
        axis.ticks= element_line(size = 2), axis.text.y = element_text(size = 20, colour = "black"),
        axis.text.x = element_text(size = 20, colour = "black", angle = 90), axis.ticks.x = element_line(size = 2), 
        axis.title.x = element_text(size = 20, colour = "white"),
        legend.position = "bottom", legend.text = element_text(size = 20), legend.title = element_text(size = 20))
par(mfrow=c(1,2))
plot(Prediction_Patterson_WRB_VPDB~temperature_open, data = Aragonite_prediction, col = "blue")
plot(Prediction_Patterson_WRB_VPDB~d18O_station, data = Aragonite_prediction, col = "red")

#stats

center_scale <- function(x){
  scale(x, scale = F)
}

Aragonite_prediction$Tcenter <- center_scale(Aragonite_prediction$temperature_open)
Aragonite_prediction$Dcenter <- center_scale(Aragonite_prediction$d18O_station)

mR <- lmer(Prediction_Patterson_WRB_VPDB~Dcenter+Tcenter+(1|month), data = Aragonite_prediction)
m0 <- lm(Prediction_Patterson_WRB_VPDB~Tcenter+Dcenter, data = Aragonite_prediction)

anova(mR, m0)

m1.1 <- lmer(Prediction_Patterson_WRB_VPDB~Dcenter+(1|month), data = Aragonite_prediction)
m1.2 <- lmer(Prediction_Patterson_WRB_VPDB~Tcenter+(1|month), data = Aragonite_prediction)

anova(mR, m1.1) #T needs to stay
anova(mR, m1.2) #d18O needs to stay

summary(mR)

plot(Prediction_Patterson_WRB_VPDB~Tcenter, data = Aragonite_prediction, col = "blue")
points(Prediction_Patterson_WRB_VPDB~Dcenter, data = Aragonite_prediction, col = "red", add = T)

save(Months_GB, Months_NRB, Months_WRB, Aragonite_prediction, 
     file = "C:/Users/timor/Nextcloud2/Timo/BODDENHECHT/Courses/Scientific writing/Data/Environmental_data2.RData")
