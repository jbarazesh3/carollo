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
library(ggsn)

###Plot Themse 


# myLocation <- c(-118.03,33.75,-117.8,33.87)

#Choose a City
city = 6

#PLotting
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))


# Data import and Processing 
   PFAS <- read_excel("PFAS.xlsx")
  # Filter the Raw PFAS Data 
     PFAS2 <- PFAS %>% transmute(District, `System Name`, Latitude, Longitude, Species, Concentration)
   
   vars2 <- c("CITY OF ANAHEIM", "EAST ORANGE COUNTY WD - RZ", "CITY OF FULLERTON", "CITY OF GARDEN GROVE", "IRVINE RANCH WATER DISTRICT", 
             "CITY OF ORANGE", "CITY OF SANTA ANA", "SERRANO WATER DISTRICT", "YORBA LINDA WATER DISTRICT")
   
   PFAS5 <- PFAS %>% filter(`System Name` %in% vars2)
   
   PFAS6 <- PFAS5 %>% filter(Species %in% c("PFOA","PFOS"))
   
   PFAS7 <- PFAS5 %>% filter(Species %in% c("PFBS", "PFDA", "PFHpA", "PFHxS" , "PFHxA", "PFNA" , "PFOA","PFOS" ))
   
   
  #statistics 
    summary3 <- PFAS6 %>% group_by(Species,  `System Name`) %>% summarise(mean = mean(Concentration), sd = sd(Concentration), count = n())
    
    summary4 <- PFAS6 %>%  group_by(`System Name`,Species) %>%
                dplyr::summarise(count = n(), zero = sum(Concentration ==0), exceed =
                                 if (Species == "PFOA") {exceed= sum(Concentration > 5.1)}
                                 else {exceed= sum(Concentration > 6.5)})  %>% mutate(percentExceed= 100*exceed / count)
    
    PFAS6.1 <- PFAS6 %>% filter(`System Name` == vars2[city]) # eventually turn into lapply 
    summaryPie <- PFAS6.1 %>%  group_by(`System Name`,Species) %>%
      dplyr::summarise(count = n(), 'ND' = sum(Concentration ==0),
                       'Compliance' = if (Species == "PFOA") {sum(Concentration > 0 & Concentration < 5.1)}
                       else {sum(Concentration > 0 & Concentration < 6.5)}, 
                       '>NL' = if (Species == "PFOA") {sum(Concentration >= 5.1 & Concentration < 10)}
                       else {sum(Concentration >= 6.5 & Concentration < 40)}, 
                       '>RL' =  if (Species == "PFOA") {sum(Concentration >= 10)}
                       else {sum(Concentration >= 40)}) 
    
#### PLOT 1: PIE CHART    
    
    summaryPie.m <- summaryPie %>% mutate( count = NULL) %>% melt(id.vars=c('Species', 'System Name')) # this makes plotting easier by puting all variables in a single column

     summaryPie.m <- summaryPie.m %>% group_by(Species) %>%
      mutate(end = 2 * pi * cumsum(value)/sum(value),
             start = lag(end, default = 0),
             middle = 0.5 * (start + end),
             hjust = ifelse(middle > pi, 1, 0),
             vjust = ifelse(middle < pi/2 | middle > 3 * pi/2, 0, 1))

    library(ggforce) # for 'geom_arc_bar'
   Pie <-  ggplot(data = summaryPie.m) +
      geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = 1,
                       start = start, end = end, fill = variable), width=1, color="white", alpha = 0.5, size=2) +
      facet_grid ( .~ Species)+
      geom_text(data=subset(summaryPie.m, value != 0), aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = paste0(value, " (", scales::percent(value / (sum(value)/2)),")"),
                    hjust = hjust, vjust = vjust), size = 5) +
      coord_fixed() +
      scale_x_continuous(limits = c(-1.5, 1.5),  # Adjust so labels are not cut off
                         name = "", breaks = NULL, labels = NULL) +
      scale_y_continuous(limits = c(-1, 1.1),      # Adjust so labels are not cut off
                         name = "", breaks = NULL, labels = NULL)+
      labs(fill = NULL, x = NULL, y = NULL, title = NULL) +
      theme_void() + theme(legend.position="bottom",
                           legend.text = element_text(size = 14),
                           strip.text.x = element_text(size=14, face="bold"))
      # print(Pie)
    # 

      
#### PLOT 2: MAP
      # Optimal Scales
      range <- PFAS6.1 %>% group_by(`System Name`) %>% transmute(Longmin = min(Longitude)-0.003, Latmin = min(Latitude)-0.003, Longmax = max(Longitude) +0.003, Latmax = max(Latitude)+0.003)
      range <- distinct(range)   # order (Longmin, Latmin,Longmax,Latmax)
      
      # #Pull Map 
      # register_google(key = "AIzaSyDJx-TfEQfeJzEAdMFlM43bp_mFCIqxcGo")
      # has_google_key()
      # # 
      # #Individual Producers Location (start with just anaheim)
      myLocation <- as.numeric(range[1,-1])
      la <- get_map(location=myLocation,  maptype="toner", source = "stamen" , crop = FALSE)
      gg <- ggmap(la, extent = "device", darken = 0,  legend = "bottom") +ggsn:::scalebar(x.min = myLocation[1], x.max = myLocation[3],
                                                                                          y.min = myLocation[2], y.max = myLocation[4], 
                                                                                          dist = 1, dist_unit = "mi",
                                                                                          st.bottom = TRUE, st.color = "black", box.fill = "grey",
                                                                                          transform = TRUE, model = "WGS84", location="topleft")
      
      
# Clustering
PFAS8 <-  PFAS6 %>% transmute(District, `System Name`, `Sampling Point Name`, Latitude, Longitude)
PFAS8 <- distinct(PFAS8, PFAS8$`Sampling Point Name`, .keep_all = TRUE)
PFAS9 <- PFAS8 %>% group_by(`System Name`) %>% tally()

PFAS8.1 <- PFAS8 %>% filter(`System Name` == vars2[city]) 



clusternum = 3
clusters <- kmeans(PFAS8.1[,4:5], clusternum)
PFAS8.1$Cluster <- as.factor(clusters$cluster)
Centroid <- as.data.frame(clusters$centers)
colnames(Centroid) <- c("xcluster","ycluster")
Centroid$ID <- seq.int(nrow(Centroid))
PFAS8.2<- merge(PFAS8.1, Centroid, by.x = c("Cluster"), by.y = c("ID"), all.x=TRUE)
#PFAS8.2$Distance <- sapply(PFAS8.2, distHaversine(Longitude,Latitude,ycluster,xcluster))
PFAS8.2 <- PFAS8.2 %>% rowwise() %>% 
  mutate(distance = distHaversine(c(Longitude,Latitude),c(ycluster,xcluster)))

TotalDistance = sum(PFAS8.2$distance)
# for (i in 1:clusternum){ #
# xcluster[i] = as.numeric(clusters$centers[i,2])  # Possibly here add this to PFAS8.1
# ycluster[i] = as.numeric(clusters$centers[i,1])
# PFAS8.1$LatCenter <- as.factor(clusters$cluster)
# }

ggclusters <- gg +
  geom_point(data = subset(PFAS6.1, `Species`=='PFOS'),aes(x=Longitude, y=Latitude, fill = Concentration), alpha = 1, size = 5, stroke = 2, shape = 21)+

   # geom_point(data = Centroid, aes(x=ycluster, y = xcluster), size = 5, shape = 23, fill = "darkred", color = "black", stroke = 1.5)+
   # geom_segment(data = PFAS8.2, aes(x= ycluster, y = xcluster, xend=Longitude, yend=Latitude), size = 1)+
  
  
  labs(fill="PFOS Concentration (ppb)")+
  scale_fill_gradientn(colours = myPalette(100), limits=c(0, ceiling(max(PFAS6$Concentration)/5)*5),
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(25, units = "mm"),
                         draw.ulim = F,
                         title.position = 'top',
                         # some shifting around
                         title.hjust = 0.5,
                         label.hjust = 0.5
                       ))+
  theme(legend.direction = "horizontal", legend.background = element_blank(), legend.text=element_text(size=12), legend.title=element_text(size=16))


 
geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)

            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)

          },

          draw_group = function(data, panel_scales, coord, draw_quantiles = NULL) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))

            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))

            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])


            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },

          draw_key = draw_key_polygon,

          default_aes = aes(weight = 1, colour = "black", fill = "white", size = 1,
                            alpha = NA, linetype = "solid"),

          required_aes = c("x", "y")
  )

Jamestheme <- function() { theme(axis.line = element_line(size=1,colour = "black"),
                                 axis.text.y = element_text(colour="black",size = 18, angle = 90, vjust=1, hjust = 0), panel.background = element_blank(),
                                 axis.text.x = element_text(colour="black",size = 16),
                                 axis.title.x.bottom = element_text(colour="black",size = 20),
                                 axis.title.y = element_blank(),
                                 strip.text = element_text(colour="black",size = 16, face = "bold"),
                                 panel.border = element_blank(),
                                 strip.background = element_blank(),
                                 legend.position = "none",
                                 legend.text = element_text(color = "black", size = 18),
                                 legend.title = element_blank(),
                                 legend.key = element_rect(colour = "transparent", fill = "transparent")

     
)}

p3 <- ggplot(PFAS6.1,aes(x=Species,y=Concentration, fill = Species))+
  geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust = 1, alpha = 0.3, draw_quantiles = 0)+
 # geom_segment(aes(x=0, xend = 5, y = 5.1, yend = 10), position = position_nudge(x = 0.2), width = 0) +

    # geom_hline(data = Z, aes(yintercept=Z1), color="black", linetype="dashed", size=3)+
    # geom_hline(data = ZZ, aes(yintercept=Z2), color="black", linetype="dotted", size=3)+


  scale_shape_manual(values=1:length(unique(PFAS7$`System Name`)))+
  ylab('Concentration (ppb)')+coord_flip()+guides(fill = FALSE)+

  geom_boxplot(width = .1, outlier.alpha = 0.3,  alpha = 0.3, size=1) + #guides = FALSE, outlier.shape = NA,
  geom_jitter(aes(color = Species), shape=16, size = 3, position=position_jitterdodge(0.1), alpha = 0.8)+
  labs(fill = NULL, title = NULL) +
  scale_y_continuous(limits = c(0,max(PFAS6.1$Concentration)), expand = c(0.025,0.025))+
  Jamestheme()
  # theme_light() + theme(legend.position="bottomright",
  #                      legend.text = element_text(size = 20),
  #                      strip.text.x = element_text(size=20),
  #                      axis.text = element_text(size = 16),
  #                      axis.text.y = element_text(angle = 90, hjust = 1),
  #                      axis.title.x = element_text(size = 20, face = "bold"),
  #                      axis.title.y = element_blank()             )

  # geom_errorbar(data = summary_loud, aes(x = genre, y = Mean, ymin = Mean-ci, ymax = Mean+ci), position = position_nudge(.25), colour = "BLACK", width = 0.1, size = 0.8)+


 # plot(p3)

grid.arrange(Pie,p3,ggclusters, ncol = 2, nrow = 2, layout_matrix = rbind(c(1,1), c(2,3)),top = textGrob(as.character(vars2[city]),gp=gpar(fontsize=28,font=3)))

