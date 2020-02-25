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


rm(list = ls())


#PLotting
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))


# Data import and Processing 
PFAS <- read_excel("PFAS_TreatmentPlanningProject.xlsx", sheet = "Sheet1" , range = cell_cols("A:I"))

# Filter the Raw PFAS Data 
colnames(PFAS) <- c("Producer", "Well", "Latitude", "Longitude", "Parameter", "Species", "Concentration", "StDev", "Units")

varsSpecies <- c("ALK", "As", "Al", "B", "Ba", "Ca", "Cl", "CLO4", "F", "Fe", "TOC", "TURB", "TDS", "SO4", "pH", "NO3" , "NH3" , "Mn" , "Mg", "K" ,
                 "EtFOSA", "MeFOSA" , "PFBS", "PFHpA" , "PFHxS" , "PFNA" , "PFOS" , "PFOA" , "PFDA" , "PFDoA" , "PFHxA" , "PFTA" , "PFTrDA" , "PFUnA" , "PFOAOS" )


varsSpecies2 <- c("TOTALK", "As", "Al","Ba", "Ca", "Cl", "F", "Fe","Mn" , "Mg", "SO4",  "NO3" , "pH", "PO4-P" ,"TOC", "TDS",
                  "EtFOSA", "MeFOSA" , "PFBS", "PFDA" ,  "PFHpA" , "PFHxS" , "PFNA" , "PFOS" , "PFOA" , "PFDoA" , "PFHxA" , "PFTA" , "PFTrDA" , "PFUnA" , "PFOAOS",
                  "TTHMs", "TCE", "PCE", "Cl3F3E")


#Categories                
ShortChain <- c("EtFOSA", "MeFOSA" , "PFBS", "PFHpA" , "PFHxS" , "PFNA" , "PFDA" , "PFDoA" , "PFHxA" , "PFTA" , "PFTrDA" , "PFUnA")
PFC<- c(ShortChain, "PFOS" , "PFOA") #Doesnt include sum
MCL <- c("As", "NO3", "TDS")
Particulate <- c("Fe", "Mn", "TURB")
Bulk <- c("ALK", "pH", "TOC")
Other <- c("Al", "Ba", "Ca", "Cl", "Mg","SO4", "F")
VOC <- c("TTHMs", "TCE", "PCE", "Cl3F3E")


PFAS2 <- PFAS %>% transmute(Producer, Well, Latitude, Longitude, Species, Concentration)  
PFAS2 <- PFAS2 %>% filter(Species %in% varsSpecies2)  
PFAS3 <- PFAS2 %>% transmute(Producer, Well, Species, Concentration)

#PFAS4<- melt(PFAS3, id.vars = c("Producer", "Well"),  variable.name = "Species", value.name = "Concentration")
#PFAS5 <- dcast(PFAS3, Producer + Well ~ Species, value.var = "Concentration")
PFAS5 <- recast(PFAS3, Producer + Well ~ Species, measure.var = "Concentration")



# Filling in missing data 
PFAS6 <- PFAS5
  #Groups with all ND
PFAS6$PFDA = 0.4
PFAS6$PFDoA = 0.4
PFAS6$PFTA = 0.4
PFAS6$PFTrDA = 0.4
PFAS6$PFUnA = 0.4

  #Remaining PFAS SPecies 
# #Pull Map 
register_google(key = "AIzaSyDJx-TfEQfeJzEAdMFlM43bp_mFCIqxcGo")
has_google_key()

# Optimal Scales for OCWD
range <- PFAS2 %>% transmute(Longmin = min(Longitude), Latmin = min(Latitude), Longmax = max(Longitude), Latmax = max(Latitude)) # tight
myLocation<- as.numeric(distinct(range))   # order (Longmin, Latmin,Longmax,Latmax)

## New packages for plotting
library(ggsn)
library(geosphere)
library(sp)
library(gstat)
library(raster)
library(rgdal)

#Simplify the dataset
Select = "PFOS"
PFAS8.1 <-  PFAS2 %>% filter(Species == Select) %>% transmute(Latitude, Longitude,Concentration)

#Full dataSet
#PFAS8.1 <-  PFAS2 %>% transmute(Latitude, Longitude,Concentration)

PFAS8.2 <- PFAS8.1

PFAS8.1 <- PFAS8.1[-48,]

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

dat.idw <- idw(formula=Concentration ~ 1, locations =PFAS8.1, newdata = grd, idp = 2.5)

# grab output of IDW for plotting
idw.output = as.data.frame(dat.idw)  # output is defined as a data table

### Interpolating Certain Wells 
PFASAll <- PFAS2 %>% transmute(Well, Latitude, Longitude, Producer)  
PFASAll <- distinct(PFASAll)

PFASWell <- PFASAll %>% dplyr::left_join(dplyr::select(PFAS8.2, Concentration, Latitude),  by = "Latitude")

#Find the wells thats dont have data, find min distance to known LatLon
PFASWell2<- PFASWell %>% filter(is.na(Concentration))
d <- pointDistance(PFASWell2[,3:2], idw.output[,1:2], lonlat=TRUE, allpairs=T) 
i <- apply(d, 1, which.min)
PFASWell2$Lat2 = idw.output$y[i]
PFASWell2$Lon2 = idw.output$x[i]

#Fill in the original dataset 
PFASWell3 <- merge(PFASWell2, idw.output, by.x = c("Lat2", "Lon2"), by.y = c("y", "x"), all.x=TRUE)
PFASWell3<- PFASWell3 %>% dplyr::transmute(Well, Latitude, Longitude, Producer, Species = Select, Concentration = var1.pred)

#Merge the 2 dataset to get values for all 66 wells
PFASComplete <- bind_rows(PFAS2,PFASWell3)

write_xlsx(PFASWell3, path = sprintf("%s.xlsx", Select),  col_names = TRUE, format_headers = TRUE)


#PFAS2 <- PFAS2 %>% group_by(Producer, Well, Latitude, Longitude) %>% summarize( Concentration = sum(Concentration[Species %in% ShortChain])) %>%  mutate(Species = 'ShortChain') %>% bind_rows(PFAS2,.) 



# Just PFAS
# PFAS3 <- PFAS2 %>% filter(Species %in% c("PFOAOS"))
# PFAS2 <- PFAS2 %>% filter(!Species %in% c(ShortChain , "PFOAOS"))
# PFAS2 <- PFAS2 %>% filter(!(Concentration == 0 & Species =="ShortChain" ))






# 
# 
# 
# ### DOMAINS ###
# 
# PFAS2 <- PFAS2 %>% mutate(Category = lapply(Species, function(x) {
#   if (x %in% PFC| Species == 'ShortChain') {
#     'PFAS'
#   } else if (x %in% MCL) {
#     'MCLs'
#   } else if (x %in% Particulate) {
#     'Colloidal'
#   } else if (x %in% Bulk) {
#     'Bulk'
#   } else if (x %in% Other) {
#     'Other Ions'
#   } else if (x %in% VOC) {
#     'VOC'
# }}))
# 
# PFAS2$Category <- unlist(PFAS2$Category)
# 
# 
# #### HEATMAP ###### Doesnt Include the Wells that we have to interpolate on 
# heatmap <- ggplot(subset(PFAS2, Producer == "Serrano WD"), aes(x = Well, y = Species, fill = Value)) +
#   geom_tile(aes(alpha = 0.8), colour = "white", size = 1) +
#   geom_text(aes(label=sprintf("%0.1f", Concentration)), fontface = "bold") + 
#   xlab(label = NULL) +
#   facet_grid(Category~., switch = "y", scales = "free_y", space = "free") +
#   scale_fill_viridis() +
#   scale_x_discrete(expand = c(0, 0)) +
#   theme_minimal() +
#   theme(strip.placement = "outside",
#         plot.title = element_text(hjust = 0.5),
#         axis.title.y = element_blank(),
#         strip.background = element_rect(fill = "white", color = "white"),
#         strip.text.y = element_text(size = 12, face = "bold"),
#         axis.ticks = element_blank(), 
#         axis.text = element_text(face = "bold")) +
#   ggtitle(label = "Serrano WD")
# 
# 
# # plot(heatmap)
