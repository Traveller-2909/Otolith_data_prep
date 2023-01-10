# set directory & load file
here::i_am("Cleanup.R")
library(here)

Oto_all <- read.csv2("anadromous_all.csv", header = TRUE, stringsAsFactors = FALSE)
Oto_all <- read.delim("Oto_all.txt", header = TRUE, stringsAsFactors = FALSE, sep = "\t")

Oto_core <- read.csv2("anadromous_all_core.CSV", header = T, stringsAsFactors = F)

# packages needed---------------------------------------------------------------

library(tidyverse)
library(lubridate)

# Data cleanup------------------------------------------------------------------

Oto_all <- Oto_all[Oto_all$Type..Tr.Pt. != "Pt",]

Otopike <- Oto_all %>%
  transmute(measurement = as.numeric(substr(Analysis, nchar(Analysis)-1, nchar(Analysis))),
            ID = substr(Oto_all$Analysis, 5,6),
            ID2 = Otolith.number,
            date = lubridate::as_date(parse_date_time(Date, orders = c("dmy"))),
            d18O = 0.97001 * as.numeric(d18O.SMOW) -29.99,
            x = east.west..um.,
            y = north.south..um.,
            SE_permill = X1SE....,
            SE_percent = X1SE.....1,
            OH = X16O1H.16O.measured)

#plots only core

Otocore <- Oto_core %>%
  transmute(measurement = as.numeric(substr(Analysis, nchar(Analysis)-1, nchar(Analysis))),
            ID = substr(Analysis, 0, 1),
            ID2 = Otolith.number,
            date = lubridate::as_date(parse_date_time(Date, orders = c("dmy"))),
            d18O = 0.97001 * as.numeric(d18O.SMOW) -29.99,
            x = east.west..um.,
            y = north.south..um.,
            SE_permill = X1SE....,
            SE_percent = X1SE.....1,
            OH = X16O1H.16O.measured)


#Proofreading

is.na(Otopike)
apply(is.na(Otopike), 2, which)

apply(is.na(Otocore), 2, which)

#calculation of distances-------------------------------------------------------
#for loop

IDOto <- unique(Otopike$ID)
#IDOto <- unique(Otocore$ID)

#check length
length(IDOto)

Dist <- NULL

for (i in 1:length(IDOto)){
  d <- sqrt(((sqrt(Otopike[which(Otopike$ID == IDOto[i]),]$x^2)) - 
               (sqrt((Otopike[which(Otopike$ID == IDOto[i] & Otopike$ID2 == "core"),]$x)^2)))^2 +
              ((sqrt(Otopike[which(Otopike$ID == IDOto[i]),]$y^2)) - 
                 (sqrt((Otopike[which(Otopike$ID == IDOto[i] & Otopike$ID2 == "core"),]$y)^2)))^2)

  Dist <- c(Dist, d)
}

Otopike$Distance_um <- Dist

#Core

Dist <- NULL

for (i in 1:length(IDOto)){
  d <- sqrt(((sqrt(Otocore[which(Otocore$ID == IDOto[i]),]$x^2)) - 
               (sqrt((Otocore[which(Otocore$ID == IDOto[i] & Otocore$ID2 == "core"),]$x)^2)))^2 +
              ((sqrt(Otocore[which(Otocore$ID == IDOto[i]),]$y^2)) - 
                 (sqrt((Otocore[which(Otocore$ID == IDOto[i] & Otocore$ID2 == "core"),]$y)^2)))^2)
  
  Dist <- c(Dist, d)
}

Otocore$Distance_um <- Dist

#check for errors, all have to be zero

Otopike[Otopike$ID2 == "core",]$Distance_um

Otocore[Otocore$ID2 == "core",]$Distance_um

#final form

Oto_distances <- Otopike %>%
  transmute(ID = ID,
            ID2 = ID2,
            date = date,
            Distance_um = Distance_um,
            d18O = d18O,
            OH = OH,
            SE_permill = SE_permill,
            SE_percent = SE_percent)

# Core clean

Otocore_distances <- Otocore %>%
  transmute(ID = ID,
            ID2 = ID2,
            date = date,
            Distance_um = Distance_um,
            d18O = d18O,
            OH = OH,
            SE_permill = SE_permill,
            SE_percent = SE_percent)

#write into file

write.table(Oto_distances, file = "Otolith_data_anadromous_clean_with_distance_um.CSV", dec = ".", sep = ";", row.names = FALSE)

write.table(Otocore, file = "Otolith_data_anadromous_clean_core_with_distance_um.CSV", dec = ".", sep = ";", row.names = F)

#Backcheck strange Distance measures--------------------------------------------

sqrt(((sqrt(Otopike[which(Otopike$ID == "#G"),]$x^2)) - 
        (sqrt((Otopike[which(Otopike$ID == "#G" & Otopike$ID2 == "core"),]$x)^2)))^2 +
       ((sqrt(Otopike[which(Otopike$ID == "#G"),]$y^2)) - 
          (sqrt((Otopike[which(Otopike$ID == "#G" & Otopike$ID2 == "core"),]$y)^2)))^2)

SIMS_pike <- unique(Otopike[which(Otopike$ID2 != "core" & Otopike$ID2 != "margin" & Otopike$ID2 != "Margin"),]$ID2)

SIMS_pike_Haupt <- data.frame(c(1:69), SIMS_pike)
write_delim(SIMS_pike_Haupt, "SIMS_pike_Haupt+Pilot.txt", delim = "\t")

#Filter specific otolith (for checking outliers etc.)

View <- Oto_distances[Oto_distances$ID=="77",]
View$Number <- seq(1, length(View$Distance_um), 1)
View

#search for number
Oto_distances[Oto_distances$ID=="06",]$ID2

#Otopike[Otopike$ID2 == "BH-01855",]

#Whereshitgoingdown <- Otopike %>%
#  group_by(ID) %>%
#  filter(row_number()==2)

#unique(Whereshitgoingdown$ID2)

#dim(Whereshitgoingdown[duplicated(Whereshitgoingdown$ID2),])

#unique(SIMS_pike)
#Add zipper---------------------------------------------------------------------
 Oto_zipper <- read.csv2("C:/Users/timor/Nextcloud/Timo/BODDENHECHT/Chapter 1/Stats/Otolith_stats/Box1 Mount B/Box1 MountB d18O Jul2021 zipper.CSV",
                         header = TRUE, stringsAsFactors = FALSE)

#clean zipper

Otozipper <- Oto_zipper %>%
  transmute(measurement = as.numeric(substr(Analysis, nchar(Analysis)-1, nchar(Analysis))),
            ID = substr(Oto_zipper$Analysis, 5, 6),
            ID2 = Otolith.number,
            date = lubridate::as_date(parse_date_time(Date, orders = c("dmy"))),
            d18O = 0.97001 * as.numeric(d18O.SMOW) -29.99,
            x = east.west..?m.,
            y = north.south..?m.,
            SE_permill = X1SE....,
            SE_percent = X1SE.....1,
            OH = X16O1H.16O.measured,
            site = Area)

#distance zipper

IDZipper <- unique(Otozipper$ID)

#check length
length(IDZipper)

ZDist <- NULL

for (i in 1:length(IDZipper)){
  d <- sqrt(((sqrt(Otozipper[which(Otozipper$ID == IDZipper[i]),]$x^2)) - 
               (sqrt((Otozipper[which(Otozipper$ID == IDZipper[i] & Otozipper$ID2 == "core"),]$x)^2)))^2 +
              ((sqrt(Otozipper[which(Otozipper$ID == IDZipper[i]),]$y^2)) - 
                 (sqrt((Otozipper[which(Otozipper$ID == IDZipper[i] & Otozipper$ID2 == "core"),]$y)^2)))^2)
  
  ZDist <- c(ZDist, d)
}

Otozipper$Distance_um <- ZDist

#check for errors, all have to be zero

Otozipper[Otozipper$ID2 == "core",]$Distance_um

#Add distance

Otozipper$Distance_um <- Otozipper$Distance_um + 15

#Join zipper profiles to normal profile

Zipper_distances <- Otozipper %>%
  transmute(ID = ID,
            ID2 = ID2,
            ID3 = "Zipper",
            date = date,
            Distance_um = Distance_um,
            d18O = d18O,
            OH = OH,
            SE_permill = SE_permill,
            SE_percent = SE_percent)

Oto_distances$ID3 <- "Transect"

Otozipper_total <- rbind(Oto_distances[Oto_distances$ID %in% IDZipper,], Zipper_distances)


#Plot alone---------------------------------------------------------------------

ggplot(Otozipper_total[Otozipper_total$ID == 18 & Otozipper_total$ID3 == "Transect",], 
       aes(Distance_um, d18O)) + geom_line() + geom_point() + theme_minimal()

#plot with zipper

ggplot(Otozipper_total[Otozipper_total$ID == 18,], aes(Distance_um, d18O)) + geom_line() + geom_point() + theme_minimal()
