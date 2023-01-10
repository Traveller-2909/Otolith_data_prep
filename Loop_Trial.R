setwd("C:/Users/timor/Nextcloud/Timo/BODDENHECHT/Chapter 1/Stats/Otolith_stats")
library(scales)
library(ggplot2)
library(car)
library(tidyr)
library(pastecs)


#Otoliths ABDEJK d18O

Oto_d18O <- read.csv2("Feb2021-18O-samplesABDEJK.CSV", header = TRUE, stringsAsFactors = FALSE)
Oto_d18O$Analysis <- sub("@","0",Oto_d18O$Analysis)
Oto_d18O$Order <- as.numeric(substr(Oto_d18O$Analysis, nchar(Oto_d18O$Analysis)-1, nchar(Oto_d18O$Analysis)))
Oto_d18O <- Oto_d18O[order(Oto_d18O$Order),]

#?
Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),"Analysis"]
substr(Oto_d18O$Analysis, nchar(Oto_d18O$Analysis)-1, nchar(Oto_d18O$Analysis))

#Otoliths I-F d18O

Oto2_d18O <- read.delim("Feb2021-18O- samples-I-F.txt", header = TRUE, stringsAsFactors = FALSE)
Oto2_d18O$Analysis <- sub("@","0",Oto2_d18O$Analysis)
Oto2_d18O$Order <- as.numeric(substr(Oto2_d18O$Analysis, nchar(Oto2_d18O$Analysis)-1, nchar(Oto2_d18O$Analysis)))
Oto2_d18O <- Oto2_d18O[order(Oto2_d18O$Order),]

#Otoliths DEGHJK d13C

Oto_d13C <- read.csv2("Feb2021-13C-samplesDEGHJK.CSV", header = TRUE, stringsAsFactors = FALSE)
Oto_d13C$ID <- sub("@","0",Oto_d13C$ID)
Oto_d13C$Order <- as.numeric(substr(Oto_d13C$ID, nchar(Oto_d13C$ID)-1, nchar(Oto_d13C$ID)))
Oto_d13C <- Oto_d13C[order(Oto_d13C$Order),]


  #Otlith A transect selection sequence
  Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),]
  
  #Otolith A selection of core measurement
  Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis))
           [nrow(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),])],]
  
#Working a & b distance
(sqrt(as.numeric(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),"east...west..µm."])^2))-
  (sqrt(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis))
                 [nrow(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),])],"east...west..µm."]^2))

(sqrt(as.numeric(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),"north...south..µm."])^2))-
  (sqrt(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis))
                 [nrow(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),])],"north...south..µm."]^2))

#Working Pythagoras
  sqrt((((sqrt(as.numeric(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),"east...west..µm."])^2))-
    (sqrt(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis))
         [nrow(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),])],"east...west..µm."]^2)))^2)+
    (((sqrt(as.numeric(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),"north...south..µm."])^2))-
       (sqrt(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis))
                 [nrow(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),])],"north...south..µm."]^2)))^2))

#Converting SMOW to VPDB
  0.97001 *as.numeric(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),"d18O.SMOW"])-29.99

#Plotting A transect of d18O PDB against distance from core
png("OtoA.png", width = 960)
  plot(
  (sqrt((((sqrt(as.numeric(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),"east...west..µm."])^2))-
         (sqrt(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis))
                        [nrow(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),])],"east...west..µm."]^2)))^2)+
       (((sqrt(as.numeric(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),"north...south..µm."])^2))-
           (sqrt(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis))
                          [nrow(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),])],"north...south..µm."]^2)))^2))),
  (0.97001 *as.numeric(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),"d18O.SMOW"])-29.99),
  main = "", xlab =  "", ylab = "", 
  type = "o", col = "black", lwd = 3, axes = FALSE)
axis(1, col = "black", col.ticks = "black", col.axis = "black", cex.axis = 2)
axis(2, col = "black", col.ticks = "black", col.axis = "black", cex.axis = 2)
mtext("Distance from Core [\U03BC]", side = 1, line = 3, col = "black", cex = 2)
mtext("\211 relative to VPDB", side = 2, line = 3, col = "black", cex = 2)
grid(nx = NULL, ny = NA, col = "black", lwd = 2)
#Adding manual points

points(
  (sqrt((((sqrt(as.numeric(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis)),"east...west..µm."])^2))-
            (sqrt(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis))
                           [nrow(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis)),])],"east...west..µm."]^2)))^2)+
          (((sqrt(as.numeric(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis)),"north...south..µm."])^2))-
              (sqrt(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis))
                             [nrow(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis)),])],"north...south..µm."]^2)))^2))),
  (0.97001 *as.numeric(Oto_d18O[which(grepl("A", Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis)),"d18O.SMOW"])-29.99),
  pch = 17, cex = 2, col = "#006600")
dev.off()

#Adding d13C (only for DEGHJK)

png("OtoK_d13C.png", width = 960)
plot(
  (sqrt((((sqrt(as.numeric(Oto_d13C[which(grepl("K", Oto_d13C$ID)),"east.west"])^2))-
            (sqrt(Oto_d13C[which(grepl("K", Oto_d13C$ID))
                           [nrow(Oto_d13C[which(grepl("K", Oto_d13C$ID)),])],"east.west"]^2)))^2)+
          (((sqrt(as.numeric(Oto_d13C[which(grepl("K", Oto_d13C$ID)),"north.south"])^2))-
              (sqrt(Oto_d13C[which(grepl("K", Oto_d13C$ID))
                             [nrow(Oto_d13C[which(grepl("K", Oto_d13C$ID)),])],"north.south"]^2)))^2))),
  Oto_d13C[which(grepl("K", Oto_d13C$ID)),"VPDB"],
  main = "", xlab =  "", ylab = "", type = "o", col = "#00FF00", lwd = 3, axes = FALSE)
axis(1, col = "#FFFF00", col.ticks = "#FFFF00", col.axis = "#FFFF00", cex.axis = 2)
axis(2, col = "#FFFF00", col.ticks = "#FFFF00", col.axis = "#FFFF00", cex.axis = 2)
mtext("Distance from Core [\U03BC]", side = 1, line = 3, col = "#FFFF00", cex = 2)
mtext("\211 relative to VPDB", side = 2, line = 3, col = "#FFFF00", cex = 2)
grid(nx = NULL, ny = NA, col = "#FFFF00", lwd = 2)
dev.off()

mean(0.97001 *as.numeric(Oto_d18O[which(grepl("J", Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis)),"d18O.SMOW"])-29.99, na.rm = TRUE)
