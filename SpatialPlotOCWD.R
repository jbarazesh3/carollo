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

rm(list = ls())

#PLotting
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))


# Data import and Processing 
PFAS <- read_excel("PFAS_TreatmentPlanningProject.xlsx", sheet = "Sheet1" , range = cell_cols("A:I"))

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


#### PLOT 2: MAP

# #Pull Map 
register_google(key = "AIzaSyDJx-TfEQfeJzEAdMFlM43bp_mFCIqxcGo")
has_google_key()

PFAS4 <- PFAS2 %>% transmute(Well, Latitude, Longitude, Producer)  
PFAS4 <- PFAS4[-1]
PFAS4 <- distinct(PFAS4)

# Optimal Scales
buffer = 0.005
range <- PFAS4 %>% transmute(Longmin = min(Longitude) - buffer, Latmin = min(Latitude)-buffer, Longmax = max(Longitude)+buffer, Latmax = max(Latitude)+buffer) # tight
range <- distinct(range)   # order (Longmin, Latmin,Longmax,Latmax)


# #Individual Producers Location (start with just anaheim)
myLocation <- as.numeric(range)
la <- get_map(location=myLocation,  maptype="toner", source = "stamen" , crop = FALSE)
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
library(rgeos)
UTM32n <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 

ProducerSelect <- unique(PFAS3$Producer)[5] # "Serrano WD", "Santa Ana", "Yorba Linda WD" "EOCWD" "Fullerton" "Garden Grove" "IRWD" "Orange" "Anaheim" 
PFAS8.3 <- PFAS4 %>% filter(Producer == ProducerSelect)

coordinates(PFAS8.3) = ~Longitude+Latitude
proj4string(PFAS8.3) <- WGS84

mb_hull_trans <- spTransform(PFAS8.3, UTM32n)
mb_hull <- gConvexHull(mb_hull_trans) # convex hull
mb_buff <- gBuffer(mb_hull, width = 250) # arbitrary 25km buffer
# tranforms back to WGS84
mb_buff_WGS84 <- spTransform(mb_buff, WGS84)
yourshapefile_df <- fortify(mb_buff_WGS84, region ="id")


#### K means Centralization 
################### Centralized ALternatives #### 

#To find the best number of clusters 
library(clValid)
express <- as.data.frame(PFAS4[,2:3])
#rownames(express) <- PFAS4$Well
intern <- clValid(express, nClust = 2:(dim(PFAS4)[1]-1),  clMethods = c("hierarchical","kmeans","pam"), validation = "internal")

Optimalcluster <- as.numeric(as.character(optimalScores(intern)[1,3]))
####
clusternum = 16 #Optimalcluster
clusters <- kmeans(PFAS4[,2:3], clusternum)
PFAS4$Cluster <- as.factor(clusters$cluster)
Centroid <- as.data.frame(clusters$centers)
colnames(Centroid) <- c("xcluster","ycluster")
Centroid$ID <- seq.int(nrow(Centroid))
PFAS8.2<- merge(PFAS4, Centroid, by.x = c("Cluster"), by.y = c("ID"), all.x=TRUE)
#PFAS8.2$Distance <- sapply(PFAS8.2, distHaversine(Longitude,Latitude,ycluster,xcluster))
PFAS8.2 <- PFAS8.2 %>% rowwise() %>%
  mutate(distance = distHaversine(c(Longitude,Latitude),c(ycluster,xcluster)))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

ggclusters <- gg +
  # Producers
  geom_point(data = PFAS4, aes(x=Longitude, y=Latitude, fill = Producer, shape =Producer), alpha = 1, size = 5, stroke = 2)+
  scale_shape_manual(values = rep(21:25, 4))+
  geom_polygon(data = yourshapefile_df, aes(x=long, y=lat), fill = gg_color_hue(8)[4],color = "black", alpha = 0.2)+
  
 # geom_point(data = Centroid, aes(x=ycluster, y = xcluster), size = 5, shape = 23, fill = "black", color = "red", stroke = 2)+
  #geom_segment(data = PFAS8.2, aes(x= ycluster, y = xcluster, xend=Longitude, yend=Latitude, color = Producer), size = 1, alpha = 0.8)+
 
  theme(legend.direction = "horizontal", legend.background = element_blank(), legend.text=element_text(size=10, face = "bold"), legend.title=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=2))
plot(ggclusters)

#write_xlsx(PFAS8.2, path = "FullCentralization.xlsx",  col_names = TRUE, format_headers = TRUE)