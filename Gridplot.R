here::i_am("Gridplot.R")

# Load packages
library(tidyverse)
library(ggmap)
library(stringr)
library(viridis)
library(heatmaply)
library(akima)

#load data (might need to add µ)

gridpattern <- read.csv2("Grid/Grid Otolith BH-01588.CSV", 
                         header = T, stringsAsFactors = F)
gridpattern$d18O.SMOW <- as.numeric(gridpattern$d18O.SMOW)
gridpattern$X16O1H.16O.measured <- as.numeric(gridpattern$X16O1H.16O.measured)

gridpattern1 <- gridpattern_elim %>%
  transmute(analysis = Analysis,
            lat = north.south..um.,
            long = east.west..um.,
            OH = scale(as.numeric(X16O1H.16O.measured)),
            d18O = scale(0.97001 * as.numeric(d18O.SMOW) -29.99))%>%
  drop_na()

#plot scatter

ggplot() + geom_point(data = gridpattern1, aes(long, lat, fill = d18O))

ggplot() + stat_summary_hex(data = gridpattern1, aes(x = long, y = lat, z = d18O), 
                        geom = "hex")+
  scale_fill_viridis_c()

#heatmaply(gridpattern)

#try contour

#get outliers
outliers1 <- boxplot(gridpattern1$d18O, plot=F)$out
outliers2 <- boxplot(gridpattern1$OH, plot=F)$out
gridpattern2 <- gridpattern1[-which(gridpattern1$d18O %in% outliers1),]
gridpattern3 <- gridpattern2[-which(gridpattern2$OH %in% outliers2)]

grid_interp <- interp(x=gridpattern3$long, y=gridpattern3$lat, z=gridpattern3$OH,
                     nx = 1000, ny=1000)

newgrid <- as.data.frame(interp2xyz(grid_interp))

names(newgrid) <- c("x", "Y", "Z")

colors = c('deeppink', 'blue3', 'aquamarine', 'chartreuse', 'yellow', 'darkorange1', 'red')

tiff(filename = "Gridplot_OH-center.tiff", width = 7225, height = 7150, 
     units = "px", res = 2000)
ggplot(newgrid, aes(x, Y, z = Z)) +
  stat_summary_2d(geom = "tile", bins = 1000)+
  scale_fill_viridis(direction = 1)+
  theme(panel.grid = element_blank(), plot.background = element_blank(),
        panel.background = element_blank(), axis.text = element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom", plot.margin = unit(c(0,0,0,0), "null"))
dev.off()
  
  #Kann man machen, sieht aber schei?e aus...
  #scale_color_gradient2(low = "red", mid = "blue", high = "darkblue", 
                        #midpoint = -5, space = "Lab", guide = "colourbar",
                        #aesthetics = "fill")


