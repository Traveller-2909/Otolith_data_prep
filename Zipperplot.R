setwd("C:/Users/timor/Nextcloud/Timo/BODDENHECHT/Chapter 1/Stats/Otolith_stats/Box1 Mount B")

Oto_d18O <- read.csv2("Box1 MountB d18O Jun2021.CSV", header = TRUE, stringsAsFactors = FALSE)
Oto_d18O_Z <- read.csv2("Box1 MountB d18O Jul2021 zipper.CSV", header = TRUE, stringsAsFactors = FALSE)


Oto_d18O$Order <- as.numeric(substr(Oto_d18O$Analysis, nchar(Oto_d18O$Analysis)-1, nchar(Oto_d18O$Analysis)))
Oto_d18O_Z$Order <- as.numeric(substr(Oto_d18O_Z$Analysis, nchar(Oto_d18O_Z$Analysis)-1, nchar(Oto_d18O_Z$Analysis)))
Oto_d18O <- Oto_d18O[order(Oto_d18O$Order),]
Oto_d18O_Z <- Oto_d18O_Z[order(Oto_d18O_Z$Order),]


Oto_d18O <- subset(Oto_d18O, select = c(Analysis, Otolith.number, east.west..µm., north.south..µm., d18O.SMOW, Order))
Oto_d18O_Z <- subset(Oto_d18O_Z, select = c(Analysis, Otolith.number, east.west..µm., north.south..µm., d18O.SMOW, Order))

Oto_tot <- rbind(Oto_d18O[which(!grepl(17, substr(Oto_d18O$Analysis, 5, 6))),], Oto_d18O_Z)

IDOto <- unique(substr(Oto_d18O$Analysis, 5, 6))
IDOto_Z <- unique(substr(Oto_d18O_Z$Analysis, 5, 6))

Dist <- NULL

for(i in 1:length(IDOto)){
  d <- sqrt(
    (((sqrt(as.numeric(Oto_tot[which(grepl(IDOto_Z[i],substr(Oto_tot$Analysis, 5, 6))),"east.west..µm."])^2))-
        (sqrt(Oto_tot[which(grepl(IDOto_Z[i], substr(Oto_tot$Analysis, 5, 6)))
                      [nrow(Oto_tot[which(grepl(IDOto_Z[i], substr(Oto_tot$Analysis, 5, 6))),])],"east.west..µm."]^2)))^2)+
      (((sqrt(as.numeric(Oto_tot[which(grepl(IDOto_Z[i], substr(Oto_tot$Analysis, 5, 6))),"north.south..µm."])^2))-
          (sqrt(Oto_tot[which(grepl(IDOto_Z[i], substr(Oto_tot$Analysis, 5, 6)))
                        [nrow(Oto_tot[which(grepl(IDOto_Z[i], substr(Oto_tot$Analysis, 5, 6))),])],"north.south..µm."]^2)))^2))
  Dist <- c(Dist, c)
}
Oto_tot <- Oto_tot[order(substr(Oto_tot$Analysis, 5, 6), decreasing = FALSE),]
Oto_tot$Dist <- Dist
#Problem with distances: Core not 0 for non-zippers