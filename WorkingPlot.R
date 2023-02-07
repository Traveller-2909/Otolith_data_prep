setwd("C:/Users/timor/Nextcloud/Timo/BODDENHECHT/Chapter 1/Stats/Otolith_stats/Box1 Mount B")

Oto_d18O <- read.csv2("Box1 MountB d18O Jun2021.CSV", header = TRUE, stringsAsFactors = FALSE)
#Oto_d18O$Analysis <- sub("@","0",Oto_d18O$Analysis)
Oto_d18O$Order <- as.numeric(substr(Oto_d18O$Analysis, nchar(Oto_d18O$Analysis)-1, nchar(Oto_d18O$Analysis)))
Oto_d18O <- Oto_d18O[order(Oto_d18O$Order),]

IDOto <- unique(substr(Oto_d18O$Analysis, 5, 6))
for(i in 1:length(IDOto)){
png(paste(IDOto[i], "plot.png", sep = "_"), width = 1200)
par(mar = c(5,6,1,1))
plot(
  sqrt(
    (((sqrt(as.numeric(Oto_d18O[which(grepl(IDOto[i],substr(Oto_d18O$Analysis, 5, 6))),"east.west..?m."])^2))-
            (sqrt(Oto_d18O[which(grepl(IDOto[i], substr(Oto_d18O$Analysis, 5, 6)))
                           [nrow(Oto_d18O[which(grepl(IDOto[i], substr(Oto_d18O$Analysis, 5, 6))),])],"east.west..?m."]^2)))^2)+
          (((sqrt(as.numeric(Oto_d18O[which(grepl(IDOto[i], substr(Oto_d18O$Analysis, 5, 6))),"north.south..?m."])^2))-
              (sqrt(Oto_d18O[which(grepl(IDOto[i], substr(Oto_d18O$Analysis, 5, 6)))
                             [nrow(Oto_d18O[which(grepl(IDOto[i], substr(Oto_d18O$Analysis, 5, 6))),])],"north.south..?m."]^2)))^2)
    ),
  0.97001 *as.numeric(Oto_d18O[which(grepl(IDOto[i], substr(Oto_d18O$Analysis, 5, 6))),"d18O.SMOW"])-29.99,
  main = "", xlab =  "", ylab = "", col.axis = "black", cex.axis = 1.7,
  type = "b", col = "#33FF33", lwd = 3, fg = "black"
  )
mtext("Distance from Core [\U03BCm]", side = 1, line = 3, col = "black", cex = 2)
mtext("\211 relative to VPDB", side = 2, line = 3, col = "black", cex = 2)
grid(nx = NULL, ny = NULL, col = "indianred", lwd = 0.2)
dev.off()}


#Adding manual points

points(
  (sqrt((((sqrt(as.numeric(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis)),"east...west..µm."])^2))-
            (sqrt(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis))
                           [nrow(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis)),])],"east...west..µm."]^2)))^2)+
          (((sqrt(as.numeric(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis)),"north...south..µm."])^2))-
              (sqrt(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis))
                             [nrow(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis)),])],"north...south..µm."]^2)))^2))),
  (0.97001 *as.numeric(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("P", Oto_d18O$Analysis)),"d18O.SMOW"])-29.99),
  pch = 17, cex = 2, col = "Blue")
dev.off()


#Original script
plot(
  (sqrt((((sqrt(as.numeric(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis, ignore.case = TRUE)),"east...west..µm."])^2))-
            (sqrt(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis, ignore.case = TRUE))
                           [nrow(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis, ignore.case = TRUE)),])],"east...west..µm."]^2)))^2)+
          (((sqrt(as.numeric(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis, ignore.case = TRUE)),"north...south..µm."])^2))-
              (sqrt(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis, ignore.case = TRUE))
                             [nrow(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis, ignore.case = TRUE)),])],"north...south..µm."]^2)))^2))),
  (0.97001 *as.numeric(Oto_d18O[which(grepl(IDOto[i], Oto_d18O$Analysis) & grepl("trans", Oto_d18O$Analysis, ignore.case = TRUE)),"d18O.SMOW"])-29.99),
  main = "", xlab =  "", ylab = "", 
  type = "o", col = "black", lwd = 3, axes = FALSE)
axis(1, col = "black", col.ticks = "black", col.axis = "black", cex.axis = 2)
axis(2, col = "black", col.ticks = "black", col.axis = "black", cex.axis = 2)
mtext("Distance from Core [\U03BC]", side = 1, line = 3, col = "black", cex = 2)
mtext("\211 relative to VPDB", side = 2, line = 3, col = "black", cex = 2)
grid(nx = NULL, ny = NA, col = "black", lwd = 2)

#Test plot new samples
plot(
  sqrt(
    (((sqrt(as.numeric(Oto_d18O[which(grepl(17,substr(Oto_d18O$Analysis, 5, 6))),"east.west..µm."])^2))-
        (sqrt(Oto_d18O[which(grepl(17, substr(Oto_d18O$Analysis, 5, 6)))
                       [nrow(Oto_d18O[which(grepl(17, substr(Oto_d18O$Analysis, 5, 6))),])],"east.west..µm."]^2)))^2)+
      (((sqrt(as.numeric(Oto_d18O[which(grepl(17, substr(Oto_d18O$Analysis, 5, 6))),"north.south..µm."])^2))-
          (sqrt(Oto_d18O[which(grepl(17, substr(Oto_d18O$Analysis, 5, 6)))
                         [nrow(Oto_d18O[which(grepl(17, substr(Oto_d18O$Analysis, 5, 6))),])],"north.south..µm."]^2)))^2)
  ),
  0.97001 *as.numeric(Oto_d18O[which(grepl(17, substr(Oto_d18O$Analysis, 5, 6))),"d18O.SMOW"])-29.99,
  main = "", xlab =  "", ylab = "", xlim = c(0,1500), ylim = c(-10, -3),
  type = "o", col = "black", lwd = 3)

axis(1, col = "black", col.ticks = "black", col.axis = "black", cex.axis = 2)
axis(2, col = "black", col.ticks = "black", col.axis = "black", cex.axis = 2)
mtext("Distance from Core [\U03BC]", side = 1, line = 3, col = "black", cex = 2)
mtext("\211 relative to VPDB", side = 2, line = 3, col = "black", cex = 2)
grid(nx = NULL, ny = NULL, col = "black", lwd = 2)

#Zipper profiles manual
Oto_d18O <- read.csv2("Box1 MountB d18O Jun2021.CSV", header = TRUE, stringsAsFactors = FALSE)
Oto_d18O_Z = read.csv2("Box1 MountB d18O Jul2021 zipper.CSV", header = TRUE, stringsAsFactors = FALSE)

Oto_d18O$Order <- as.numeric(substr(Oto_d18O$Analysis, nchar(Oto_d18O$Analysis)-1, nchar(Oto_d18O$Analysis)))
Oto_d18O_Z$Order <- as.numeric(substr(Oto_d18O_Z$Analysis, nchar(Oto_d18O_Z$Analysis)-1, nchar(Oto_d18O_Z$Analysis)))
Oto_d18O <- Oto_d18O[order(Oto_d18O$Order),]
Oto_d18O_Z <- Oto_d18O_Z[order(Oto_d18O_Z$Order),]

IDOto <- unique(substr(Oto_d18O$Analysis, 5, 6))
IDOto_Z <- unique(substr(Oto_d18O_Z$Analysis, 5, 6))

Otozipper <- Oto_d18O %<>%
  transmute(
 ID = substr(Oto_d18O$Analysis, 5, 6),
 ID2 = Otolith.number)

#Normal disances manual

Oto23_dist <- sqrt(
     (((sqrt(as.numeric(Oto_d18O[which(grepl(23,substr(Oto_d18O$Analysis, 5, 6))),"east.west..µm."])^2))-
         (sqrt(Oto_d18O[which(grepl(23, substr(Oto_d18O$Analysis, 5, 6)))
                        [nrow(Oto_d18O[which(grepl(23, substr(Oto_d18O$Analysis, 5, 6))),])],"east.west..µm."]^2)))^2)+
       (((sqrt(as.numeric(Oto_d18O[which(grepl(23, substr(Oto_d18O$Analysis, 5, 6))),"north.south..µm."])^2))-
           (sqrt(Oto_d18O[which(grepl(23, substr(Oto_d18O$Analysis, 5, 6)))
                          [nrow(Oto_d18O[which(grepl(23, substr(Oto_d18O$Analysis, 5, 6))),])],"north.south..µm."]^2)))^2)
   )
Oto23_order <- seq(length(Oto23_dist), 1)
Oto23_d18O <- 0.97001 *as.numeric(Oto_d18O[which(grepl(23, substr(Oto_d18O$Analysis, 5, 6))),"d18O.SMOW"])-29.99
Oto23_frame = data.frame(ID = Oto23_order, Distance = Oto23_dist, d18O = Oto23_d18O)

#Zipper distances manual

Oto23_Zdist <- sqrt(
  (((sqrt(as.numeric(Oto_d18O_Z[which(grepl(23,substr(Oto_d18O_Z$Analysis, 5, 6))),"east.west..µm."])^2))-
      (sqrt(Oto_d18O_Z[which(grepl(23, substr(Oto_d18O_Z$Analysis, 5, 6)))
                     [nrow(Oto_d18O_Z[which(grepl(23, substr(Oto_d18O_Z$Analysis, 5, 6))),])],"east.west..µm."]^2)))^2)+
    (((sqrt(as.numeric(Oto_d18O_Z[which(grepl(23, substr(Oto_d18O_Z$Analysis, 5, 6))),"north.south..µm."])^2))-
        (sqrt(Oto_d18O_Z[which(grepl(23, substr(Oto_d18O_Z$Analysis, 5, 6)))
                       [nrow(Oto_d18O_Z[which(grepl(23, substr(Oto_d18O_Z$Analysis, 5, 6))),])],"north.south..µm."]^2)))^2)
)
Oto23_Zorder <- seq(length(Oto23_Zdist), 1)
Oto23_Zd18O <- 0.97001 *as.numeric(Oto_d18O_Z[which(grepl(23, substr(Oto_d18O_Z$Analysis, 5, 6))),"d18O.SMOW"])-29.99
Oto23_Zframe = data.frame(ID = Oto23_Zorder, Distance = Oto23_Zdist, d18O = Oto23_Zd18O)

#bind
Oto23_Profile$ID = sequence(length(Oto23_Profile$Distance),1)

Oto23_Zframe$Distance = Oto23_Zframe$Distance - 0.0001

Oto23_Profile <- rbind(Oto23_frame, Oto23_Zframe)

ggplot(Oto23_frame, aes(Distance, d18O)) + geom_line() + geom_point() + theme_minimal()
ggplot(Oto23_Profile, aes(Distance, d18O), fill = ) + geom_line() + geom_point() + theme_minimal()

for(i in 1:length(IDOto)){
  png(paste(IDOto[i], "plot.png", sep = "_"), width = 1200)
  par(mar = c(5,6,1,1))
  plot(
    sqrt(
      (((sqrt(as.numeric(Oto_d18O[which(grepl(IDOto[i],substr(Oto_d18O$Analysis, 5, 6))),"east.west..?m."])^2))-
          (sqrt(Oto_d18O[which(grepl(IDOto[i], substr(Oto_d18O$Analysis, 5, 6)))
                         [nrow(Oto_d18O[which(grepl(IDOto[i], substr(Oto_d18O$Analysis, 5, 6))),])],"east.west..?m."]^2)))^2)+
        (((sqrt(as.numeric(Oto_d18O[which(grepl(IDOto[i], substr(Oto_d18O$Analysis, 5, 6))),"north.south..?m."])^2))-
            (sqrt(Oto_d18O[which(grepl(IDOto[i], substr(Oto_d18O$Analysis, 5, 6)))
                           [nrow(Oto_d18O[which(grepl(IDOto[i], substr(Oto_d18O$Analysis, 5, 6))),])],"north.south..?m."]^2)))^2)
    ),
    0.97001 *as.numeric(Oto_d18O[which(grepl(IDOto[i], substr(Oto_d18O$Analysis, 5, 6))),"d18O.SMOW"])-29.99,
    main = "", xlab =  "", ylab = "", col.axis = "black", cex.axis = 1.7,
    type = "b", col = "#33FF33", lwd = 3, fg = "black"
  )
  mtext("Distance from Core [\U03BCm]", side = 1, line = 3, col = "black", cex = 2)
  mtext("\211 relative to VPDB", side = 2, line = 3, col = "black", cex = 2)
  grid(nx = NULL, ny = NULL, col = "indianred", lwd = 0.2)
  dev.off()}
