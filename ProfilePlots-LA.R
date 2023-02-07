# set directory & load file

here::i_am("ProfilePlots-LA.R")
library(here)
library(tidyverse)
library(ggplot2)
library(knitr)

#Load profiles
Oto_profilesLA <- read.delim("Otolith_LA-data_all_with_distance.txt", header = TRUE, stringsAsFactors = F)

Otocore_profiles <- read.csv2("Otolith_data_anadromous_clean_core_with_distance_um.CSV", header = T, stringsAsFactors = F, sep = ";")

Oto_sizes <- read.csv2("Oto_sizes_all.csv", header = TRUE, stringsAsFactors = F)
Oto_sizes$Size.x. <- as.numeric(Oto_sizes$Size.x.)
#Oto_sizes$ID <- as.character(substr(Oto_sizes$Otolith, 1,2)) #If OtoID is 01, 02, 03, 04 etc
#Oto_sizes$ID <- as.numeric(Oto_sizes$ID) #If OtoID is 1, 2, 3, 4 etc

#Otocore_profiles$ID <- as.numeric(Otocore_profiles$ID)
#Oto_profiles$ID <- as.numeric(Oto_profiles$ID)

Otocore_profiles <- Otocore_profiles %>% drop_na()

Oto_profiles <- Oto_profiles %>% drop_na()


#Test plot----------------------------------------------------------------------

png(paste("Oto_A.png"), bg = "transparent")
plot(Oto_profiles[Oto_profiles$SIMS_ID == "A",]$Distance_um, Oto_profiles[Oto_profiles$SIMS_ID == "A",]$Sr.Ca, main = "", 
     xlab =  "", ylab = "", xlim = c(0, max(Oto_profiles[Oto_profiles$SIMS_ID == "A",]$Distance_um)), 
     col.axis = "black", cex.axis = 1.7, type = "b", col = "black", lwd = 3, fg = "black", axes = F)
axis(1, pos = mean(Oto_profiles[Oto_profiles$SIMS_ID == "A",]$Sr.Ca), at = c(0, max(Oto_profiles[Oto_profiles$SIMS_ID == "A",]$Distance_um)),
     labels = FALSE, lwd.ticks = 5, tck = 0)
axis(2, pos = 0, tcl = 0.2, las = 2, mgp = c(0, -7, 0), cex.axis = 2, lwd = 1, col = "black", col.axis = "black",
     at = c(0,.003), labels = c(0,0.003))
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

#IDOto <- unique(Oto_sizes$ID)
IDOto <- "O"

Oto_profiles <- Oto_profiles %>% filter(Mount != "Pilot")

IDOto <- unique(Oto_profiles$SIMS_ID)

IDOto <- c("19", "002", "O")

for (i in 1:length(IDOto)) {
  png(paste0("LA-Plots/", IDOto[i], "_LA.png"), width = Oto_sizes[which(Oto_sizes$ID == IDOto[i]), "Size.x.",]+288, 
      height = 0.5 * (Oto_sizes[which(Oto_sizes$ID == IDOto[i]), "Size.x.",]+288), units = "px", bg = "black")
  par(xaxs = "i", mar = c(0,40,0,4))
  plot(Oto_profilesLA[which(Oto_profilesLA$SIMS_ID == IDOto[i]), "Distance_um",], 
       Oto_profilesLA[which(Oto_profilesLA$SIMS_ID == IDOto[i]), "Sr.Ca",], main = "", 
       xlab =  "", ylab = "", xlim = c(0, max(Oto_profilesLA[which(Oto_profilesLA$SIMS_ID == IDOto[i]), "Distance_um",], na.rm = T)), 
       col.axis = "black", cex.axis = 1.7, type = "b", col = "orange", lwd = 15, fg = "black", cex = 2, axes = FALSE, pch = 19)
  axis(1, pos = mean(Oto_profilesLA[Oto_profilesLA$SIMS_ID == IDOto[i],]$Sr.Ca), 
       at = c(0, max(Oto_profilesLA[Oto_profilesLA$SIMS_ID == IDOto[i],]$Distance_um)),
       labels = FALSE, lwd.ticks = 5, tck = 0, col = "orange", lty = 1, lwd = 5)
  axis(1, pos = mean(Oto_profilesLA$Sr.Ca), at = c(0, max(Oto_profilesLA$Distance_um)),
       labels = FALSE, lwd.ticks = 5, tck = 0, col = "orange", lty = 3, lwd = 5)
  axis(2, pos = 0, tcl = 0.2,
       at = seq(round(min(Oto_profilesLA[Oto_profilesLA$SIMS_ID == IDOto[i],]$Sr.Ca),3), 
                round(max(Oto_profilesLA[Oto_profilesLA$SIMS_ID == IDOto[i],]$Sr.Ca),3), 0.001), 
       labels = seq(round(min(Oto_profilesLA[Oto_profilesLA$SIMS_ID == IDOto[i],]$Sr.Ca),3), 
                    round(max(Oto_profilesLA[Oto_profilesLA$SIMS_ID == IDOto[i],]$Sr.Ca),3), 0.001),
       las = 2, mgp = c(0, 2, 2), cex.axis = 10, lwd = 5, col = "orange", col.axis = "orange")
  title(main = "", ylab = "Sr/Ca", cex.lab = 15, col.lab = "orange", line = 30)
  
  dev.off()  
}

#Graphical adjustments

par(xaxs = "i", mar = c(0, 0, 0, 5))
plot(Oto_profiles[which(Oto_profiles$SIMS_ID == "O"), "Distance_um",], Oto_profiles[Oto_profiles$SIMS_ID == "O",]$Sr.Ca, main = "", 
     xlab =  "", ylab = "", xlim = c(0, max(Oto_profiles[Oto_profiles$SIMS_ID == "O",]$Distance_um)),
     col.axis = "black", cex.axis = 1.7, type = "o", col = "black", lwd = 3, fg = "black", cex = 1, axes = FALSE)
axis(1, pos = mean(Oto_profiles[Oto_profiles$SIMS_ID == "O",]$Sr.Ca), at = c(0, max(Oto_profiles[Oto_profiles$SIMS_ID == "O",]$Distance_um)),
     labels = FALSE, lwd.ticks = 5, tck = 0)
axis(1, pos = mean(Oto_profiles$Sr.Ca), at = c(0, max(Oto_profiles$Distance_um)),
     labels = FALSE, lwd.ticks = 5, tck = 0)
axis(2, pos = max(Oto_profiles[which(Oto_profiles$SIMS_ID == "O"), "Distance_um",]), tcl = 0.2,
     at = seq(round(min(Oto_profiles[Oto_profiles$SIMS_ID == "O",]$Sr.Ca),3), round(max(Oto_profiles[Oto_profiles$SIMS_ID == "O",]$Sr.Ca),3), 0.001), 
     labels = seq(round(min(Oto_profiles[Oto_profiles$SIMS_ID == "O",]$Sr.Ca),3), round(max(Oto_profiles[Oto_profiles$SIMS_ID == "O",]$Sr.Ca),3), 0.001),
     las = 2, mgp = c(0, -5, 2), cex.axis = 2, lwd = 3)


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

ggplot() + geom_point(aes(Oto_profiles[which(Oto_profiles$ID == "19"), "Distance_um",], Oto_profiles[Oto_profiles$ID == "19",]$d18O))+
  geom_line(aes(Oto_profiles[which(Oto_profiles$ID == "19"), "Distance_um",], Oto_profiles[Oto_profiles$ID == "19",]$d18O))+
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1), 
        axis.text = element_text(colour = "black", size = 18), axis.title = element_text(colour = "black", size = 18),
        panel.grid.major.y = element_blank())+
  scale_y_continuous(limits = c(-8, -2), breaks = c(-7:-2))+
  geom_hline(yintercept = mean(Oto_profiles[Oto_profiles$ID == "19",]$d18O))+
  geom_hline(yintercept = mean(Oto_profiles$d18O), linetype = "dotted")+
  xlab("Distance from core ([\U03BCm])") + ylab("\211 relative to VPDB")



