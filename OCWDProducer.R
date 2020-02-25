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
library(viridis)




#PLotting
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))


# Data import and Processing 
PFAS <- read_excel("PFAS_TreatmentPlanningProject.xlsx", sheet = "Sheet1" ,range = cell_cols("A:I"))

# Filter the Raw PFAS Data 
colnames(PFAS) <- c("Producer", "Well", "Latitude", "Longitude", "Parameter", "Species", "Concentration", "StDev", "Units")

varsSpecies <- c("ALK", "As", "Al", "B", "Ba", "Ca", "Cl", "CLO4", "F", "Fe", "TOC", "TURB", "TDS", "SO4", "pH", "NO3" , "NH3" , "Mn" , "Mg", "K" ,
                 "EtFOSA", "MeFOSA" , "PFBS", "PFHpA" , "PFHxS" , "PFNA" , "PFOS" , "PFOA" , "PFDA" , "PFDoA" , "PFHxA" , "PFTA" , "PFTrDA" , "PFUnA" , "PFOAOS" )


#Categories                
ShortChain <- c("EtFOSA", "MeFOSA" , "PFBS", "PFHpA" , "PFHxS" , "PFNA" , "PFDA" , "PFDoA" , "PFHxA" , "PFTA" , "PFTrDA" , "PFUnA")
PFC<- c(ShortChain, "PFOS" , "PFOA") #Doesnt include sum
MCL <- c("As", "NO3", "NH3", "CLO4", "B", "TDS")
Particulate <- c("Fe", "Mn", "TURB")
Bulk <- c("ALK", "pH", "TOC")
Other <- c("Al", "Ba", "Ca", "Cl", "Mg", "K", "SO4", "F")


PFAS2 <- PFAS %>% transmute(Producer, Well, Latitude, Longitude, Species, Concentration)  
PFAS2 <- PFAS2 %>% filter(Species %in% varsSpecies)  
PFAS2 <- PFAS2 %>% group_by(Producer, Well, Latitude, Longitude) %>% summarize( Concentration = sum(Concentration[Species %in% ShortChain])) %>%  mutate(Species = 'ShortChain') %>% bind_rows(PFAS2,.) 
PFAS2 <- PFAS2 %>% group_by(Species) %>% mutate(Value = Concentration/max(Concentration)) %>% arrange(Species)
PFAS3 <- PFAS2 %>% filter(Species %in% c("PFOAOS"))
PFAS2 <- PFAS2 %>% filter(!Species %in% c(ShortChain , "PFOAOS"))

PFAS2 <- PFAS2 %>% filter(!(Concentration == 0 & Species =="ShortChain" ))

# PFAS2$Concentration[PFAS2$Concentration == 0 & PFAS2$Species =="ShortChain"] <- NA
# PFAS2$Value[is.na(PFAS2$Concentration)] <- NA


### DOMAINS ###

PFAS2 <- PFAS2 %>% mutate(Category = lapply(Species, function(x) {
    if (x %in% PFC| Species == 'ShortChain') {
      'PFAS'
    } else if (x %in% MCL) {
      'MCLs'
    } else if (x %in% Particulate) {
      'Colloidal'
    } else if (x %in% Bulk) {
      'Bulk'
    } else if (x %in% Other) {
      'Other Ions'
    }
  }))

PFAS2$Category <- unlist(PFAS2$Category)



#### HEATMAP ###### Doesnt Include the Wells that we have to interpolate on 
heatmap <- ggplot(subset(PFAS2, Producer == "Serrano WD"), aes(x = Well, y = Species, fill = Value)) +
  geom_tile(aes(alpha = 0.8), colour = "white", size = 1) +
  geom_text(aes(label=sprintf("%0.1f", Concentration)), fontface = "bold") + 
  xlab(label = NULL) +
  facet_grid(Category~., switch = "y", scales = "free_y", space = "free") +
  scale_fill_viridis() +
  scale_x_discrete(expand = c(0, 0)) +
  theme_minimal() +
  theme(strip.placement = "outside",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.ticks = element_blank(), 
        axis.text = element_text(face = "bold")) +
  ggtitle(label = "Serrano WD")


# plot(heatmap)



#### PLOT 2: MAP

# #Pull Map 
# register_google(key = "AIzaSyDJx-TfEQfeJzEAdMFlM43bp_mFCIqxcGo")
# has_google_key()
  
# Optimal Scales for ALL the Producers
buffer = 0.02

range <- PFAS3 %>% transmute(Longmin = min(Longitude) - buffer, Latmin = min(Latitude)-buffer, Longmax = max(Longitude)+buffer, Latmax = max(Latitude)+buffer) # tight

### TO PLOT ONLY MAP FOR PRODUCER
#range <- PFAS3 %>% filter(Producer == "Serrano WD")  %>%  transmute(Longmin = min(Longitude) - buffer, Latmin = min(Latitude)-buffer, Longmax = max(Longitude)+buffer, Latmax = max(Latitude)+buffer) # per producer

range <- distinct(range)   # order (Longmin, Latmin,Longmax,Latmax)
range <- range[-1]


# #Individual Producers Location 
myLocation <- as.numeric(range)
la <- get_map(location=myLocation,  maptype="toner", source = "stamen" , crop = TRUE) #maptype="terrain-lines"
gg <- ggmap(la, extent = "device", darken = 0,  legend = "bottom") +ggsn:::scalebar(x.min = myLocation[1], x.max = myLocation[3],
                                                                                    y.min = myLocation[2], y.max = myLocation[4], 
                                                                                    dist = 0.5, dist_unit = "mi",
                                                                                    st.bottom = FALSE, st.color = "black",
                                                                                    transform = TRUE, model = "WGS84", anchor=c(x = myLocation[3]-buffer/3, y = myLocation[2]+buffer/3))




## New packages for plotting
library(ggsn)
library(geosphere)
library(sp)
library(gstat)
library(raster)
library(rgdal)

#Simplify the dataset
PFAS8 <-  PFAS3 %>% transmute(Latitude, Longitude,Concentration)
PFAS8 <-  PFAS8[-1]
PFAS8.1 <- distinct(PFAS8,Latitude, .keep_all = TRUE)

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
# plot(mb_buff_WGS84)
# points(PFAS8.1)


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


### Interpolating Certain Wells 
PFAS4 <- PFAS %>% transmute(Well, Latitude, Longitude, Producer)  
#PFAS4 <- PFAS4[-1]
PFAS4 <- distinct(PFAS4)

PFAS5 <- PFAS4 %>% dplyr::left_join(dplyr::select(PFAS3, Concentration, Latitude),  by = "Latitude")

#Find the wells thats dont have data, find min distance to known LatLon
PFAS5<- PFAS5 %>% filter(is.na(Concentration))
d <- pointDistance(PFAS5[,3:2], idw.output[,1:2], lonlat=TRUE, allpairs=T) 
i <- apply(d, 1, which.min)
PFAS5$Lat2 = idw.output$y[i]
PFAS5$Lon2 = idw.output$x[i]

#Fill in the original dataset 
PFAS6 <- merge(PFAS5, idw.output, by.x = c("Lat2", "Lon2"), by.y = c("y", "x"), all.x=TRUE)
PFAS6<- PFAS6 %>% dplyr::transmute(Well, Latitude, Longitude, Producer, Species = "PFOAOS", Concentration = var1.pred)

#Merge the 2 dataset to get values for all 66 wells
PFASComplete <- bind_rows(PFAS3[,-7],PFAS6)

##### DELETE IF WANT FULL MAP!
#PFAS8.Producer <-  PFAS3 %>% filter(Producer == "Serrano WD") %>% transmute(Latitude, Longitude,Concentration) 

#Actual Plotting 
ggclusters <- gg +
  
  #Concentrations
  geom_point(data = PFAS8,aes(x=Longitude, y=Latitude, fill = Concentration), alpha = 1, size = 5, stroke = 2, shape = 21)+
  geom_point(data = PFAS6,aes(x=Longitude, y=Latitude, fill = Concentration), color = "red", alpha = 1, size = 5, stroke = 2, shape = 21)+
  geom_tile(data = xyv, aes(x = x, y = y, fill = var1.pred), alpha = 0.5)+
  
  labs(fill=expression(Sigma*"PFOA + PFOS (ppb)"))+
  scale_fill_gradientn(colours = myPalette(100), limits=c(0, ceiling(max(PFAS3$Concentration)/5)*5),  na.value = "transparent" ,
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

# 
# 
# 
