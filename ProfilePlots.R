# set directory & load file

here::i_am("ProfilePlots.R")
library(here)
library(tidyverse)
library(ggplot2)
library(knitr)

#Load profiles
Oto_profiles <- read.csv2("Otolith_data_clean_with_distance_um.CSV", header = TRUE, dec = ".", sep = ";")

Otocore_profiles_ana <- read.csv2("Otolith_data_anadromous_clean_core_with_distance_um.CSV", 
                                  header = T, stringsAsFactors = F, sep = ";")
Otocore_profiles_ana <- Otocore_profiles_ana %>% 
  transmute(ID = ID,
            ID2 = ID2,
            d18O = d18O,
            Distance_um = Distance_um)%>%
  drop_na()


Otocore_profiles <- read.csv2("Otolith_data_clean_core_with_distance_um.CSV", 
                              header = T, stringsAsFactors = F, sep = ";")
Otocore_profiles <- Otocore_profiles %>%
  transmute(ID = ID,
            ID2 = ID2,
            d18O = d18O,
            Distance_um = Distance_um)


Otocore_profiles <- rbind(Otocore_profiles, Otocore_profiles_ana)
Otocore_profiles <- Otocore_profiles %>% drop_na()
Otocore_profiles$d18O <- as.numeric(Otocore_profiles$d18O)
Otocore_profiles$Distance_um <- as.numeric(Otocore_profiles$Distance_um)

Otocore_profiles <- Otocore_profiles %>%
  transmute(ID = ID,
            ID2 = ID2,
            d18O = d18O,
            Distance_um = round(Distance_um,1))%>%
  group_by(ID) %>%
  arrange(ID, Distance_um)

tiff(filename = "d18OvsOH.tiff", width = 3000, height = 3000, 
     units = "px", res = 600)
ggplot(Oto_profiles, aes(scale(d18O), scale(OH)))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_minimal()
dev.off()

Otocore_profiles <- as.data.frame(Otocore_profiles)

Oto_sizes <- read.csv2("Oto_sizes_all.csv", header = TRUE, stringsAsFactors = F)
Oto_sizes$Size.x. <- as.numeric(Oto_sizes$Size.x.)
Oto_sizes$ID <- as.character(substr(Oto_sizes$Otolith, 1,2)) #If OtoID is 01, 02, 03, 04 etc
Oto_sizes$ID <- as.numeric(Oto_sizes$ID) #If OtoID is 1, 2, 3, 4 etc

#Otocore_profiles$ID <- as.numeric(Otocore_profiles$ID)
#Oto_profiles$ID <- as.numeric(Oto_profiles$ID)

Oto_profiles <- Oto_profiles %>% drop_na()


#Test plot----------------------------------------------------------------------

png(paste("Oto_A.png"), bg = "transparent")
plot(Oto_profiles[Oto_profiles$ID == "A",]$Distance_um, Oto_profiles[Oto_profiles$ID == "A",]$d18O, main = "", 
     xlab =  "", ylab = "", xlim = c(0, max(Oto_profiles[Oto_profiles$ID == "A",]$Distance_um)), 
     col.axis = "black", cex.axis = 1.7, type = "b", col = "black", lwd = 3, fg = "black", axes = FALSE)
axis(1, pos = median(Oto_profiles[Oto_profiles$ID == "A",]$d18O), at = c(0, max(Oto_profiles[Oto_profiles$ID == "A",]$Distance_um)),
     labels = FALSE, lwd.ticks = 5, tck = 0)
axis(2, pos = 0, at = round(c(min(Oto_profiles[Oto_profiles$ID == "A",]$d18O), max(Oto_profiles[Oto_profiles$ID == "A",]$d18O)), 1))
dev.off()

# Working plot
png(paste("48.png"), width = Oto_sizes[which(Oto_sizes$ID == "48"), "Size.x.",], 
    height = 0.5 * Oto_sizes[which(Oto_sizes$ID == "48"), "Size.x.",], units = "px", bg = "black")
par(xaxs = "i", mar = rep(0,4))
plot(Oto_profiles[which(Oto_profiles$ID == "48"), "Distance_um",], Oto_profiles[Oto_profiles$ID == "48",]$d18O, main = "", 
     xlab =  "", ylab = "", xlim = c(0, max(Oto_profiles[Oto_profiles$ID == "48",]$Distance_um)), 
     col.axis = "black", cex.axis = 1.7, type = "b", col = "white", lwd = 3, fg = "black", cex = 4, axes = FALSE)
dev.off()  

# Loop--------------------------------------------------------------------------
Oto_sizes$Size.x.[2] <- 4543

IDOto <- unique(Oto_sizes$ID)
IDOto <- "2"

IDOto2 <- unique(Oto_profiles$ID)

for (i in 1:length(IDOto)) {
  png(paste0("C:/Users/timor/Nextcloud2/Timo/BODDENHECHT/Geschwafel/Graphic repository/",IDOto[i], "_SIMS_2.png"), 
      width = Oto_sizes[which(Oto_sizes$ID == IDOto[i]), "Size.x.",]+585, #Change "LA-Plots" for whatever 
      height = 0.5 * (Oto_sizes[which(Oto_sizes$ID == IDOto[i]), "Size.x.",]+687), units = "px", bg = "black",
      res = 300)    #path needed 
  par(xaxs = "i", mar = c(12,10,0,0), font.axis=2, font.lab=2)
  plot(Oto_profiles[which(Oto_profiles$ID == IDOto[i]), "Distance_um",], 
       Oto_profiles[which(Oto_profiles$ID == IDOto[i]), "d18O",], main = "", 
       xlab =  "", ylab = "", xlim = c(0, max(Oto_profiles[which(Oto_profiles$ID == IDOto[i]), "Distance_um",], na.rm = T)), 
       col.axis = "white", cex.axis = 1.7, type = "l", col = "white", lwd = 10, fg = "white", cex = 2, axes = FALSE, pch = 19)
  # axis(1, pos = mean(Oto_profiles[Oto_profiles$ID == IDOto[i],]$d18O), 
  #      at = c(0, max(Oto_profiles[Oto_profiles$ID == IDOto[i],]$Distance_um)),
  #      labels = FALSE, lwd.ticks = 5, tck = 0, col = "white", lty = 1, lwd = 5)
  # axis(1, pos = mean(Oto_profiles$d18O), at = c(0, max(Oto_profiles$Distance_um)),
  #      labels = FALSE, lwd.ticks = 5, tck = 0, col = "white", lty = 3, lwd = 5)
  axis(2, pos = 0, las = 2, mgp = c(0, 2, 2), cex.axis = 3, lwd = 10, col = "white", col.axis = "white",
       at = seq(round(min(Oto_profiles[which(Oto_profiles$ID == IDOto[i]), "d18O",], na.rm = T),0),
                round(max(Oto_profiles[which(Oto_profiles$ID == IDOto[i]), "d18O",], na.rm = T),0),1),
       labels = seq(round(min(Oto_profiles[which(Oto_profiles$ID == IDOto[i]), "d18O",], na.rm = T),0),
                round(max(Oto_profiles[which(Oto_profiles$ID == IDOto[i]), "d18O",], na.rm = T),0),1))
  axis(1, pos = -7.8, las = 2, mgp = c(0, 2, 2), cex.axis = 3, lwd = 10, col = "white", col.axis = "white",
       at = seq(0,round(max(Oto_profiles[which(Oto_profiles$ID == IDOto[i]), "Distance_um",], na.rm = T),0)+500,500),
       labels = seq(0, round(max(Oto_profiles[which(Oto_profiles$ID == IDOto[i]), "Distance_um",], na.rm = T),0)+500,500))
  title(main = "", ylab = expression(bold("\u03B4"^18*"O"~"otolith"~("VPDB"))),
        cex.lab = 3.5, col.lab = "white", line = 5, font=2)
  title(xlab = expression(bold("Distance from core"~("\u03bc"~"m"))),
        cex.lab = 3.5, col.lab = "white", line = 11, font=2)
  dev.off()  
}


#Graphical adjustments

par(xaxs = "i", mar = rep(2, 4))
plot(Oto_profiles[which(Oto_profiles$ID == "O"), "Distance_um",], Oto_profiles[Oto_profiles$ID == "O",]$d18O, main = "", 
     xlab =  "", ylab = "", xlim = c(0, max(Oto_profiles[Oto_profiles$ID == "O",]$Distance_um)),
     col.axis = "black", cex.axis = 1.7, type = "b", col = "black", lwd = 3, fg = "black", cex = 4, axes = FALSE)
axis(2, pos = 0, tcl = 0.2, at = c(min(Oto_profiles[Oto_profiles$ID == "O",]$d18O, na.rm = T), 
                                    max(Oto_profiles[Oto_profiles$ID == "O",]$d18O, na.rm = T)),
     labels = c(ceiling(min(Oto_profiles[Oto_profiles$ID == "O",]$d18O, na.rm = T)), 
                floor(max(Oto_profiles[Oto_profiles$ID == "O",]$d18O, na.rm = T))),
     las = 2, mgp = c(0, -2, 0), cex.axis = 2, lwd = 3)

# Only plots--------------------------------------------------------------------

for (i in 1:length(IDOto)) {
  png(paste(IDOto[i], ".png"), width = Oto_sizes[which(Oto_sizes$ID == IDOto[i]), "Size.x.",], 
      height = 0.5 * Oto_sizes[which(Oto_sizes$ID == IDOto[i]), "Size.x.",], units = "px", bg = "white")
  par(xaxs = "i", mar = rep(5,4))
  plot(Otocore_profiles[which(Otocore_profiles$ID == IDOto[i]), "Distance_um",], 
       Otocore_profiles[which(Otocore_profiles$ID == IDOto[i]), "d18O",], main = "", 
       xlab =  "Distance from Core [\U03BCm]", ylab = "\211 relative to VPDB", 
       col.axis = "black", cex.axis = 3, type = "b", col = "black", lwd = 4, fg = "black", cex = 3, frame.plot = F, cex.lab = 2)
  box(bty = "l")
  dev.off()  
}

#Graphics-----------------------------------------------------------------------

par(xaxs = "i", mar = rep(5, 4))
plot(Oto_profiles[which(Oto_profiles$ID == IDOto[i] & Oto_profiles$Distance_um > 200), "Distance_um",], 
     Oto_profiles[which(Oto_profiles$ID == IDOto[i] & Oto_profiles$Distance_um > 200), "d18O"], main = "", 
     xlab =  "Distance from Core [\U03BCm]", ylab = "\211 relative to VPDB",
     col.axis = "black", cex.axis = 2, type = "b", col = "black", lwd = 3, fg = "black", cex = 2, frame.plot = F, cex.lab = 2)
box(bty = "l")

#Isorange
IDOto <- unique(Oto_profiles$ID)
Maxiso <- NULL
for (i in 1:length(IDOto)){
 M <- max(Oto_profiles[which(Oto_profiles$ID == IDOto[i]), "d18O",])
 Maxiso <- c(Maxiso, M)
}

Miniso <- NULL
for (i in 1:length(IDOto)){
  m <- min(Oto_profiles[which(Oto_profiles$ID == IDOto[i]), "d18O",])
  Miniso <- c(Miniso, m)
}
 Iso <- data.frame("Min" = Miniso, "Max" = Maxiso, "Range" = Miniso - Maxiso)
mean(Iso$Range, na.rm = T)


ggplot() + geom_point(aes(Oto_profiles[which(Oto_profiles$ID == "O"), "Distance_um",], Oto_profiles[Oto_profiles$ID == "O",]$d18O))+
  geom_line(aes(Oto_profiles[which(Oto_profiles$ID == "O"), "Distance_um",], Oto_profiles[Oto_profiles$ID == "O",]$d18O))+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1), 
        axis.text = element_text(colour = "black", size = 18), axis.title = element_text(colour = "black", size = 18),
        panel.grid.major.y = element_line(colour = "grey", size = 1, linetype = "dotted"))+
  scale_y_continuous(limits = c(-8, -2), breaks = c(-7:-2))+
  xlab("Distance from core ([\U03BCm])") + ylab("\211 relative to VPDB")+
  geom_smooth(span = 1)

# Automated peak detection

IDOto <- unique(Otocore_profiles$ID)
IDOto <- c("O")
#2
library(ggpmisc)

# Maxima
IDOto <- "D"
IDOto <- unique(Otocore_profiles$ID)
Data <- data.frame(IDS = character(), BH = character(), Ages = integer())

for (i in 1:length(IDOto)) {
  
  V <- rle(Otocore_profiles[which(Otocore_profiles$ID == IDOto[i]), "d18O"])$values
  A <- length(V[ggpmisc:::find_peaks(V, ignore_threshold = 0.5, 
                                     span = ifelse(length(V[ggpmisc:::find_peaks(V, ignore_threshold = 0.5, span = 5)])>=10, 5, 5))])
  Data[i,"IDS"] <- IDOto[i]
  Data[i, "BH"] <- Otocore_profiles[Otocore_profiles$ID == IDOto[i],][[2,2]]
  Data[i,"Ages"] <- A
}

Otocore_profiles[Otocore_profiles$ID == IDOto[i],][[2,2]]

#Peak finder
#remove duplicates

Vector_17 <- rle(Vector_17)
Vector_17 <- Vector_17$values

#Count peaks

length(Otocore_profiles$d18O[Otocore_profiles$ID == "U"][ggpmisc:::find_peaks
                                                           (Otocore_profiles$d18O[Otocore_profiles$ID == "U"],
                                                             ignore_threshold = 0.5, span = 3)])

# Plot
ggplot(Otocore_profiles[Otocore_profiles$ID == "#E",], aes(Distance_um, d18O)) + 
  geom_line() + stat_peaks(col = "red", ignore_threshold = 0.5, span = 5) + stat_valleys(col = "green")


write.table(Data, "Automated_count.txt", sep = "\t", dec = ".", row.names = F)

