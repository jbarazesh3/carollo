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
# library(knitr)
# library(kableExtra)

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
# register_google(key = "AIzaSyDJx-TfEQfeJzEAdMFlM43bp_mFCIqxcGo")
# has_google_key()

### NEED TO RUN OCWDProducer FIRST to get PFASCOMPLETE
#Select Specific Producer
source('~/OCWDProducer.R')
ProducerSelect <- unique(PFAS3$Producer)[5] # "Serrano WD", "Santa Ana", "Yorba Linda WD" "EOCWD" "Fullerton" "Garden Grove" "IRWD" "Orange" "Anaheim" 
PFAS3 <- PFASComplete %>% dplyr::filter(Producer == ProducerSelect)

# Optimal Scales
buffer = 0.005
range <- PFAS3 %>% transmute(Longmin = min(Longitude) - buffer, Latmin = min(Latitude)-buffer, Longmax = max(Longitude)+buffer, Latmax = max(Latitude)+buffer) # tight
range <- distinct(range)   # order (Longmin, Latmin,Longmax,Latmax)
range <- range[-1]


# #Individual Producers Location (start with just anaheim)
myLocation <- as.numeric(range)
la <- get_map(location=myLocation,  maptype="toner", source = "stamen" , crop = TRUE) #maptype="terrain-lines"
gg <- ggmap(la, extent = "device", darken = 0,  legend = "bottom") +ggsn:::scalebar(x.min = myLocation[1], x.max = myLocation[3],
                                                                                    y.min = myLocation[2], y.max = myLocation[4], 
                                                                                    dist = 0.5, dist_unit = "mi",
                                                                                    st.bottom = FALSE, st.color = "black",
                                                                                    transform = TRUE, model = "WGS84", anchor=c(x = myLocation[3]-buffer/3, y = myLocation[2]+buffer/3))


PFAS8 <-  PFAS3
PFAS8.1 <- distinct(PFAS8,Latitude, .keep_all = TRUE)

################### Centralized ALternatives #### 

#To find the best number of clusters 
library(clValid)
express <- as.data.frame(PFAS8[,3:4])
#rownames(express) <- PFAS8$Well
intern <- clValid(express, nClust = 2:(dim(PFAS8)[1]-1),  clMethods = c("hierarchical","kmeans","pam"), validation = "internal", neighbSize = (dim(PFAS8)[1]-1))

Optimalcluster <- as.numeric(as.character(optimalScores(intern)[1,3]))
####
clusternum = 2 #Optimalcluster
clusters <- kmeans(PFAS8.1[,3:4], clusternum)
PFAS8.1$Cluster <- as.factor(clusters$cluster)
Centroid <- as.data.frame(clusters$centers)
colnames(Centroid) <- c("xcluster","ycluster")
Centroid$ID <- seq.int(nrow(Centroid))
PFAS8.2<- merge(PFAS8.1, Centroid, by.x = c("Cluster"), by.y = c("ID"), all.x=TRUE)
#PFAS8.2$Distance <- sapply(PFAS8.2, distHaversine(Longitude,Latitude,ycluster,xcluster))
PFAS8.2 <- PFAS8.2 %>% rowwise() %>%
  mutate(distance = distHaversine(c(Longitude,Latitude),c(ycluster,xcluster)))

# library(gmapsdistance)
# PFAS8.2 <- PFAS8.2 %>% rowwise() %>%
#   mutate(distance2 = gmapsdistance(origin = c(Longitude,Latitude),c(ycluster,xcluster)))
# 
# TotalDistance = sum(PFAS8.2$distance)
# 
# coordinates(grd) <- ~x + y
# coordinates(PFAS8.1) = ~Longitude+Latitude
# 
ggclusters <- gg +

  #Concentrations
  geom_point(data = PFAS8,aes(x=Longitude, y=Latitude, fill = Concentration), alpha = 1, size = 7, stroke = 2, shape = 21)+
  geom_text(data=PFAS8, aes(x=Longitude, y=Latitude, label=Well, vjust = -1), color="blue", fontface="bold",size=3) +
  # geom_tile(data = xyv, aes(x = x, y = y, fill = var1.pred), alpha = 0.5)+
  # geom_raster(data = idw_raster_crop2, aes(fill = value), alpha = 0.5)+
  
 # geom_point(data = Centroid, aes(x=ycluster, y = xcluster), size = 10, shape = 23, fill = "black", color = "purple", stroke = 2)+
 # geom_circle(data = Centroid, aes(x0=ycluster, y0 = xcluster, r = 0.5), stroke = 2)+
 # geom_segment(data = PFAS8.2, aes(x= ycluster, y = xcluster, xend=Longitude, yend=Latitude), size = 2)+
  
  labs(fill="PFAS Concentration (ppb)")+
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
# theme(legend.direction = "horizontal", legend.background = element_blank(), legend.text=element_text(size=10, face = "bold"), legend.title=element_blank())
plot(ggclusters)
#

## Export to Excel
library(writexl)
write_xlsx(PFAS8.2, path = "Anaheim5.xlsx",  col_names = TRUE, format_headers = TRUE)
