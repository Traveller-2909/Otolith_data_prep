setwd("C:/Users/timor/Nextcloud/Timo/BODDENHECHT/Chapter 1/Stats/Otolith_stats")
library(scales)
library(ggplot2)
library(car)
library(tidyr)
library(pastecs)


#Otoliths ABDEJK d18O

Oto_d18O <- read.csv2("Feb2021-18O-samplesABDEJK.CSV", header = TRUE)
#Convert SMOW into VPDB for d18O
Oto_d18O$VPDB <- 0.97001 * as.numeric(Oto_d18O$d18O.SMOW) - 29.99

#Separate otolith transect from add. points
# Otolith A
OtoA_O <- Oto_d18O[grepl("A", Oto_d18O$Analysis), , drop = FALSE]
OtoA_OPt <- OtoA_O[grepl("P", OtoA_O$Analysis), , drop = FALSE]
OtoA_OTrans <- OtoA_O[grepl("trans", OtoA_O$Analysis), , drop = FALSE]
# Otolith D
OtoD_O <- Oto_d18O[grepl("D", Oto_d18O$Analysis), , drop = FALSE]
OtoD_OPt <- OtoD_O[grepl("P", OtoD_O$Analysis), , drop = FALSE]
OtoD_OTrans <- OtoD_O[grepl("trans", OtoD_O$Analysis), , drop = FALSE]

#Phytagoras for Distance from Core d18O
#A
OtoA_OTrans$EastWestDist <- sqrt(OtoA_OTrans$east...west..µm.**2) - 1273
OtoA_OTrans$NorthSouthDist <- OtoA_OTrans$north...south..µm. - 6252

OtoA_OTrans$DistanceFromCore <- sqrt(OtoA_OTrans$EastWestDist **2 + OtoA_OTrans$NorthSouthDist **2)

#D
OtoD_OTrans$EastWestDist <- sqrt(OtoD_OTrans$east...west..µm.**2) - 3658
OtoD_OTrans$NorthSouthDist <- sqrt(OtoD_OTrans$north...south..µm.**2) - 3873

OtoD_OTrans$DistanceFromCore <- sqrt(OtoD_OTrans$EastWestDist **2 + OtoD_OTrans$NorthSouthDist **2)


#Distance for Points
#A
OtoA_OPt$EastWestDist <- sqrt(OtoA_OPt$east...west..µm.**2) - 1273
OtoA_OPt$NorthSouthDist <- OtoA_OPt$north...south..µm. - 6252

OtoA_OPt$DistanceFromCore <- sqrt(OtoA_OPt$EastWestDist **2 + OtoA_OPt$NorthSouthDist **2)

#D
OtoD_OPt$EastWestDist <- sqrt(OtoD_OPt$east...west..µm.**2) - 3388
OtoD_OPt$NorthSouthDist <- sqrt(OtoD_OPt$north...south..µm.**2) - 4323

OtoD_OPt$DistanceFromCore <- sqrt(OtoD_OPt$EastWestDist **2 + OtoD_OPt$NorthSouthDist **2)

#Plot
#A
Transectplot_A <- ggplot(OtoA_OTrans, aes(DistanceFromCore, VPDB, color = "Transect"))
Transectplot_A + geom_point(aes(DistanceFromCore, VPDB, color = "Transect"), shape = 16, size = 3, fill = "blue")+
  geom_line(size = 1, linetype = 1)+
  geom_point(data = OtoA_OPt, aes(DistanceFromCore, VPDB, color = "Additional Points"), 
             size = 4, colour = "darkgreen", shape = 2, stroke = 1.1, fill = "darkgreen")+
  scale_color_manual("", limits = c("Transect", "Additional Points"), values = c("blue", "darkgreen"))+
  guides(colour = guide_legend(override.aes = list(pch = c (16, 2), fill = c("blue", "darkgreen"), linetype = 0)))+
  labs(x = "Distance from core [\U03BC]", y = "\211 relative to VPDB")+ #Unicode U+03BC ohne Plus für Erfolg
  ggtitle("Otolith A")+
  theme(axis.title = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust = 0.5),
        panel.background = element_blank(), axis.text = element_text(face = "bold"), 
        axis.line = element_line(colour = "darkgrey"))
#D
Transectplot_D <- ggplot(OtoD_OTrans, aes(DistanceFromCore, VPDB, color = "Transect O"))
Transectplot_D + geom_point(aes(DistanceFromCore, VPDB, color = "Transect"), shape = 16, size = 3, fill = "blue")+
  geom_line(size = 1, linetype = 1)+
  geom_point(data = OtoD_OPt, aes(DistanceFromCore, VPDB, color = "Additional Points"), 
             size = 4, colour = "darkgreen", shape = 2, stroke = 1.1, fill = "darkgreen")+
  scale_color_manual("", limits = c("Transect", "Additional Points"), values = c("blue", "darkgreen"))+
  guides(colour = guide_legend(override.aes = list(pch = c (16, 2), fill = c("blue", "darkgreen"), linetype = 0)))+
  labs(x = "Distance from core [\U03BC]", y = "\211 relative to VPDB")+ #Unicode U+03BC ohne Plus für Erfolg
  ggtitle("Otolith D")+
  theme(axis.title = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust = 0.5),
        panel.background = element_blank(), axis.text = element_text(face = "bold"), 
        axis.line = element_line(colour = "darkgrey"))  

#Otoliths DEGHJK d13C
Oto_d13C <- read.delim("Feb2021-13C-samplesDEGHJK.txt", header = TRUE)
# Otolith D
OtoD_C <- Oto_d13C[grepl("D", Oto_d13C$X), , drop = FALSE]

#Phytagoras for Distance from Core d13C
#D
OtoD_C$EastWestDist <- sqrt(OtoD_C$east.west **2) - 3582
OtoD_C$NorthSouthDist <- sqrt(OtoD_C$north.south **2) - 3726

OtoD_C$DistanceFromCore <- sqrt(OtoD_C$EastWestDist **2 + OtoD_C$NorthSouthDist **2)

#Add to d18O plot (?)
Transectplot_D + geom_point(aes(DistanceFromCore, VPDB, color = "Transect O"), shape = 16, size = 3, fill = "blue")+
  geom_line(size = 1, linetype = 1)+
  geom_point(data = OtoD_OPt, aes(DistanceFromCore, VPDB, color = "Additional Points"), 
             size = 4, colour = "darkgreen", shape = 2, stroke = 1.1, fill = "darkgreen")+
  geom_point(data = OtoD_C, aes(DistanceFromCore, VPDB, color = "Transect C"), size = 4, colour = "black", shape = 18)+
  scale_color_manual("", limits = c("Transect O", "Additional Points", "Transect C"), values = c("blue", "darkgreen", "darkgrey"))+
  guides(colour = guide_legend(override.aes = list(pch = c (16, 2, 18), fill = c("blue", "darkgreen", "darkgrey"), linetype = 0)))+
  labs(x = "Distance from core [\U03BC]", y = "\211 relative to VPDB")+ #Unicode U+03BC ohne Plus für Erfolg
  ggtitle("Otolith D")+
  theme(axis.title = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust = 0.5),
        panel.background = element_blank(), axis.text = element_text(face = "bold"), 
        axis.line = element_line(colour = "darkgrey"))  


#Transect d13C
#D 
Transectplot_C_D <- ggplot(OtoD_C, aes(DistanceFromCore,VPDB, color = "Transect"))
Transectplot_C_D + geom_point(shape = 16, size = 3, fill = "darkgrey")+
  geom_line(size = 1, linetype = 1)+
  scale_color_manual("", limits = c("Transect"), values = c("darkgrey"))+
  guides(colour = guide_legend(override.aes = list(pch = c (16), fill = c("darkgrey"), linetype = 0)))+
  labs(x = "Distance from core [\U03BC]", y = "\211 relative to VPDB")+ #Unicode U+03BC ohne Plus für Erfolg
  ggtitle("Otolith D")+
  theme(axis.title = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust = 0.5),
        panel.background = element_blank(), axis.text = element_text(face = "bold"), 
        axis.line = element_line(colour = "darkgrey"))
