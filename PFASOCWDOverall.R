library(ggplot2)
library(ggmap)
library(grid)
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(ggpubr)
library(scales)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(splitstackshape)
library("data.table")
library(reshape2)
library(readxl)
library(maps)
library(mapproj)
library(RColorBrewer)
library("ggbeeswarm")
library(readr)
library(RColorBrewer)
library(reshape2)
library(purrr)
library(cowplot)
library(ggsn)
library(geosphere)
library(sp)
library(gstat)
library(raster)
library(rgdal)



#PLotting
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))


# Data import and Processing 
PFAS <- read_excel("PFAS.xlsx")
# Filter the Raw PFAS Data 
PFAS2 <- PFAS %>% transmute(District, `System Name`, Latitude, Longitude, Species, Concentration)  



vars2 <- c("CITY OF ANAHEIM", "EAST ORANGE COUNTY WD - RZ", "CITY OF FULLERTON", "CITY OF GARDEN GROVE", "IRVINE RANCH WATER DISTRICT", 
           "CITY OF ORANGE", "CITY OF SANTA ANA", "SERRANO WATER DISTRICT", "YORBA LINDA WATER DISTRICT")

PFAS5 <- PFAS %>% filter(`System Name` %in% vars2)
PFAS5 <- PFAS %>% filter(Latitude < 34.2 & Latitude >33.795525 & Longitude < -117.1 & Longitude > -118.2)
Covina <- data.frame(x = 34.09 , y= -117.8903 )

PFAS6 <- PFAS5 %>% filter(Species %in% c("PFOA","PFOS"))

PFAS7 <- PFAS5 %>% filter(Species %in% c("PFBS", "PFDA", "PFHpA", "PFHxS" , "PFHxA", "PFNA" , "PFOA","PFOS" ))


#statistics 
 summary2 <- PFAS7 %>% group_by(Species) %>% summarise(mean = mean(Concentration), sd = sd(Concentration), count = n())


#### PLOT 2: MAP
# Optimal Scales
buffer = 0.02
#range <- PFAS6 %>% transmute(Longmin = min(Longitude)-0.003, Latmin = min(Latitude)-0.003, Longmax = max(Longitude) +0.003, Latmax = max(Latitude)+0.003)
range <- PFAS6 %>% transmute(Longmin = min(Longitude) - buffer, Latmin = min(Latitude)-buffer, Longmax = max(Longitude)+buffer, Latmax = max(Latitude)+buffer) # tight
#range <- PFAS6 %>% transmute(Longmin = min(Longitude) - buffer, Latmin = 33.75525, Longmax = max(Longitude)+buffer, Latmax = 31.1) # tight
range <- distinct(range)   # order (Longmin, Latmin,Longmax,Latmax)

# #Pull Map 
# register_google(key = "AIzaSyDJx-TfEQfeJzEAdMFlM43bp_mFCIqxcGo")
# has_google_key()

# #Individual Producers Location 
myLocation <- as.numeric(range)
la <- get_map(location=myLocation,  maptype="toner", source = "stamen" , crop = TRUE) #maptype="terrain-lines"
gg <- ggmap(la, extent = "device", darken = 0,  legend = "bottom") +ggsn:::scalebar(x.min = myLocation[1], x.max = myLocation[3],
                                                                                    y.min = myLocation[2], y.max = myLocation[4], 
                                                                                    dist = 0.5, dist_unit = "mi",
                                                                                    st.bottom = FALSE, st.color = "black",
                                                                                    transform = TRUE, model = "WGS84", anchor=c(x = myLocation[3]-buffer/3, y = myLocation[2]+buffer/3))



PFAS8 <-  PFAS6 %>%  filter(Species == 'PFOA' | Species == 'PFOS') %>% transmute(`Sampling Point Name`, Latitude, Longitude,Concentration)

PFAS8 <-  PFAS6 %>%  filter(Species == 'PFOA') %>% transmute(`Sampling Point Name`, Latitude, Longitude,Concentration)

PFASFull <- PFAS8 %>% group_by(Latitude) %>%  mutate(Longitude = Longitude, `Sampling Point Name` = `Sampling Point Name`) %>% summarise(Concentration = sum(Concentration))

PFAS8.1 <- distinct(PFAS8,Latitude, .keep_all = TRUE)
PFAS8.1 <- PFAS8.1[-4]

PFAS8.1 <-left_join(PFAS8.1,PFASFull, by = "Latitude")
PFAS8.1 <- PFAS8.1[-1]

# get the min/max range for lat/long to make an empty grid
x.range <- c(myLocation[1], myLocation[3])  # min/max longitude of the interpolation area
y.range <- c(myLocation[2], myLocation[4]) # min/max latitude of the interpolation area


x<-seq(x.range[1], x.range[2], length.out=20)
y<-seq(y.range[1], y.range[2], length.out=20)
# from the range, exapnd the coordinates to make a regular grid
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], length.out = 100),
                   y = seq(from = y.range[1], to = y.range[2], length.out = 100))  # expand

coordinates(grd) <- ~x + y
coordinates(PFAS8.1) = ~Longitude+Latitude
sp::gridded(grd) <- TRUE


UTM32n <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")


proj4string(PFAS8.1) <- WGS84
proj4string(grd) <- WGS84


#dat.idw <- idw(PFAS8$Concentration ~ 1, locations = PFAS8, newdata = grd)
dat.idw <- idw(formula=Concentration ~ 1, locations = PFAS8.1, newdata = grd, idp = 2.5)

# grab output of IDW for plotting
idw.output = as.data.frame(dat.idw)  # output is defined as a data table
#TRIM data
library(rgeos)
mb_hull_trans <- spTransform(PFAS8.1, UTM32n)
mb_hull <- gConvexHull(mb_hull_trans) # convex hull
mb_buff <- gBuffer(mb_hull, width = 1000) # arbitrary 25km buffer
# tranforms back to WGS84
mb_buff_WGS84 <- spTransform(mb_buff, WGS84)
plot(mb_buff_WGS84)
points(PFAS8.1)


## use raster packaage to turn IDW into raster object
idw_raster <- rasterFromXYZ(idw.output[,1:3])
# mask IDW raster to confine to region that we have sites for
idw_raster_crop <- mask(idw_raster, mb_buff_WGS84)
# create contours of the IDW
#idw_contour <- rasterToContour(idw_raster_crop, nlevels = 15)
idw.contour = as.data.frame(idw_raster_crop)
idw.contour2 <- cbind(idw.output,idw.contour)
colnames(idw.contour2)[5] <- "raster"



xy <- xyFromCell(idw_raster_crop, 1:ncell(idw_raster_crop))
v <- as.data.frame(idw_raster_crop)
xyv <- data.frame(xy, v)

xyv$var1.pred<- as.numeric(as.character(xyv$var1.pred))


  #Actual Plotting 
  ggclusters <- gg +
  
  #Concentrations
  geom_point(data = PFAS8,aes(x=Longitude, y=Latitude, fill = Concentration), alpha = 1, size = 5, stroke = 2, shape = 21)+
  geom_tile(data = xyv, aes(x = x, y = y, fill = var1.pred), alpha = 0.5)+
  geom_point(data = Covina, aes(x=y, y = x), size = 10, shape = 23, fill = "black", color = "black", stroke = 2)+
    
  labs(fill=expression(Sigma*"PFOA + PFOS (ppb)"))+
  scale_fill_gradientn(colours = myPalette(100), limits=c(0, 30),  na.value = "transparent" ,  #ceiling(max(PFAS6$Concentration)/5)*5)
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"),
                         draw.ulim = F,
                         title.position = 'top',
                         # some shifting around
                         title.hjust = 0.5,
                         label.hjust = 0.5
                       ))+
  
  theme(legend.direction = "horizontal", legend.background = element_blank(), legend.text=element_text(size=10, face = "bold"), legend.title=element_text(size=12, face = "bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=2))


 plot(ggclusters)
