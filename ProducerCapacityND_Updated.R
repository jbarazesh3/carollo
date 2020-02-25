
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
library(data.table)


rm(list = ls())

#PLotting
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
options(stringsAsFactors=F) 

# Data import and Processing 
producer <- "Yorba Linda"
string <- paste(producer,"Alternatives Template.xlsx", sep = " ")
string2 <- paste(producer,"Alternatives Template2.xlsx", sep = " ")

Orange <- read_excel(string, sheet = "Vessel Calcs" ,range = cell_cols("A:J"))

Orange <- Orange %>% mutate (PFOSblend = PFOS * WellCapacity, PFOAblend = PFOA * WellCapacity, 
                             Onlineblend = if("Online" %in% colnames(Orange)) { Online * WellCapacity} else {0.8 * WellCapacity})

Orange$Land <- ifelse(Orange$Land == "Purchase", 1, 0)
Orange$Land[is.na(Orange$Land)] <- 0 

WellSums2 <- Orange %>% group_by(Alternative, Location) %>% summarise(WellCapacity = sum(as.numeric(WellCapacity)), Wells = length(Location),
                                                                      PFOS = sum(as.numeric(PFOSblend))/sum(as.numeric(WellCapacity)),  PFOA = sum(as.numeric(PFOAblend))/sum(as.numeric(WellCapacity)),
                                                                      Pipeline = sum(CostTreatment,CostDistribution, na.rm=TRUE),
                                                                      Land = sum(Land),
                                                                      Online = sum(as.numeric(Onlineblend))/sum(as.numeric(WellCapacity)))
# Flow = sum(as.numeric(WellC)))

WellSums2 <- WellSums2 %>% mutate (Well = NULL) %>% arrange(Alternative)

#WellSums2 <- WellSums2 %>% mutate (Vessels = ceiling(WellCapacity/525), EffectiveFlow = WellCapacity/Vessels)


#### Data manipulation

Vessel<- read_excel("Vessel and Media Calcs OCWD.xlsx", sheet = "Vessel and Media Cost" ,range = cell_cols("B:D"))
Vessel2 <- Vessel %>% slice(7:26)
Vessel2 <-Vessel2[c(1:8,12,13,15:19),] 
rownames(Vessel2) <- Vessel2$Parameter
Vessel2$Parameter <- NULL
Vessel2 <-as.data.frame(t(as.matrix(Vessel2)))
names(Vessel2) <- c("Cost", "Carbon", "CarbonVol", "IXVol", "CarbonCost", "ResinCost","FlowGAC", "FlowIX", "ReplaceGAC","ReplaceGACgpm", "ReplaceIX", "ReplaceIXgpm", "BackwashTanks", "SiteGAC", "SiteIX" )
Vessel2 <- melt(as.matrix(Vessel2))
names(Vessel2) <- c("Vessel", "Parameter", "Value")
Vessel2$Vessel <- as.character(Vessel2$Vessel)
Vessel2$Parameter <- as.character(Vessel2$Parameter)
Vessel2$Value <- as.numeric(Vessel2$Value)

#### Parameters #### 
GAC_Vessel_S = "HP1220"
GAC_Vessel_L = "HP1240"
#IX_Vessel_S = "HP1020"  #apparently no small
IX_Vessel_L = "HP1220"

GAC_HP1220_Flow= Vessel2$Value[Vessel2$Vessel == GAC_Vessel_S & Vessel2$Parameter == "FlowGAC"]
GAC_HP1240_Flow = Vessel2$Value[Vessel2$Vessel == GAC_Vessel_L & Vessel2$Parameter == "FlowGAC"]
#IX_HP1020_Flow= Vessel2$Value[Vessel2$Vessel == IX_Vessel_S & Vessel2$Parameter == "FlowIX"]
IX_HP1220_Flow = Vessel2$Value[Vessel2$Vessel == IX_Vessel_L & Vessel2$Parameter == "FlowIX"]

Flows = c(GAC_HP1220_Flow,GAC_HP1220_Flow,GAC_HP1240_Flow,GAC_HP1240_Flow,IX_HP1220_Flow)

# Max Flow Limits
Tolerance = 1.1 #10%
MaxFlow= Tolerance * Flows


WellSums3 <- WellSums2 %>% mutate ( GAC_HP1220_Parallel= ceiling(WellCapacity/GAC_HP1220_Flow),
                                    GAC_HP1220_LeadLag = 2* GAC_HP1220_Parallel,
                                    GAC_HP1240_Parallel = ceiling(WellCapacity/GAC_HP1240_Flow),
                                    GAC_HP1240_LeadLag = 2* GAC_HP1240_Parallel,
                                    IX_HP1220_LeadLag = 2*ceiling(WellCapacity/IX_HP1220_Flow))

TotalSystems <- WellSums3 %>% group_by(Alternative) %>% summarise (GAC_HP1220_Parallel= sum(2*ceiling(GAC_HP1220_Parallel/2))/2,
                                                                   GAC_HP1220_LeadLag =sum(2 *ceiling (GAC_HP1220_LeadLag/2))/2,
                                                                   GAC_HP1240_Parallel = sum(2 * ceiling( GAC_HP1240_Parallel/2))/2,
                                                                   GAC_HP1240_LeadLag = sum(2 *ceiling (GAC_HP1240_LeadLag/2))/2,
                                                                   IX_HP1220_LeadLag = sum(2 * ceiling (IX_HP1220_LeadLag/2))/2)

ParallelFunctionNormal <- function(x,y) {
  x/(2*ceiling(y/2))
}

ParallelFunctionLess <- function(x,y) {
  if (y >2) {x/ceiling(y+((y %% 2)-2))}
  else {x}
}

LeadLagFunctionNormal <- function(x,y) {
  x/(y/2)
}

LeadLagFunctionLess <- function(x,y) {
  if (y >2) {x/((y/2)-1)}
  else{x}
}

####OPtimized
WellSumsOptimized <- WellSums3 %>% mutate (GAC_HP1220_Parallel_Normal = mapply(ParallelFunctionNormal, WellCapacity, GAC_HP1220_Parallel),
                                           GAC_HP1220_Parallel_Less= mapply(ParallelFunctionLess, WellCapacity, GAC_HP1220_Parallel),
                                           GAC_HP1220_LeadLag_Normal = mapply(LeadLagFunctionNormal, WellCapacity, GAC_HP1220_LeadLag),
                                           GAC_HP1220_LeadLag_Less= mapply(LeadLagFunctionLess, WellCapacity, GAC_HP1220_LeadLag),
                                           
                                           GAC_HP1240_Parallel_Normal = mapply(ParallelFunctionNormal, WellCapacity, GAC_HP1240_Parallel),
                                           GAC_HP1240_Parallel_Less= mapply(ParallelFunctionLess, WellCapacity, GAC_HP1240_Parallel),
                                           GAC_HP1240_LeadLag_Normal = mapply(LeadLagFunctionNormal, WellCapacity, GAC_HP1240_LeadLag),
                                           GAC_HP1240_LeadLag_Less= mapply(LeadLagFunctionLess, WellCapacity, GAC_HP1240_LeadLag),

                                           IX_HP1220_LeadLag_Normal = mapply(LeadLagFunctionNormal, WellCapacity, IX_HP1220_LeadLag),
                                           IX_HP1220_LeadLag_Less= mapply(LeadLagFunctionLess, WellCapacity, IX_HP1220_LeadLag))



WellSumsOptimized2 <- WellSumsOptimized  %>% transmute( Location, Wells, PFOS, PFOA, WellCapacity, Land, Pipeline)

Parallel <- function(x,y,z) {
  if (x > y | z <= 2) {ceiling(z/2)}
  else {ceiling((z-2)/2)}
}

WellSumsOptimized2 <- WellSumsOptimized  %>% transmute( Location, Wells, PFOS, PFOA, WellCapacity, Land, Pipeline,
                                                        GAC_HP1220_Parallel_Opt_System = mapply(Parallel, GAC_HP1220_Parallel_Less, MaxFlow[1],GAC_HP1220_Parallel),
                                                        GAC_HP1220_Parallel_Opt_Flow = WellCapacity/ (2* GAC_HP1220_Parallel_Opt_System),
                                                        GAC_HP1220_LeadLag_Opt_System = mapply(Parallel, GAC_HP1220_LeadLag_Less, MaxFlow[2],GAC_HP1220_LeadLag),
                                                        GAC_HP1220_LeadLag_Opt_Flow = WellCapacity/ (GAC_HP1220_LeadLag_Opt_System),

                                                        GAC_HP1240_Parallel_Opt_System = mapply(Parallel, GAC_HP1240_Parallel_Less, MaxFlow[3],GAC_HP1240_Parallel),
                                                        GAC_HP1240_Parallel_Opt_Flow = WellCapacity/ (2* GAC_HP1240_Parallel_Opt_System),
                                                        GAC_HP1240_LeadLag_Opt_System = mapply(Parallel, GAC_HP1240_LeadLag_Less, MaxFlow[4],GAC_HP1240_LeadLag),
                                                        GAC_HP1240_LeadLag_Opt_Flow = WellCapacity/ (GAC_HP1240_LeadLag_Opt_System),

                                                        IX_HP1220_LeadLag_Opt_System = mapply(Parallel, IX_HP1220_LeadLag_Less, MaxFlow[5],IX_HP1220_LeadLag),
                                                        IX_HP1220_LeadLag_Opt_Flow = WellCapacity/ (IX_HP1220_LeadLag_Opt_System))


TotalSystems2 <- WellSumsOptimized2 %>% group_by(Alternative) %>% summarise (GAC_HP1220_Parallel= sum(GAC_HP1220_Parallel_Opt_System),
                                                                   GAC_HP1220_LeadLag =sum(GAC_HP1220_LeadLag_Opt_System),
                                                                   GAC_HP1240_Parallel = sum(GAC_HP1240_Parallel_Opt_System),
                                                                   GAC_HP1240_LeadLag = sum(GAC_HP1240_LeadLag_Opt_System),
                                                                   IX_HP1220_LeadLag = sum(IX_HP1220_LeadLag_Opt_System))


################ COst Matrics

CostAssumptions1 <- read_excel("Vessel and Media Calcs OCWD.xlsx", sheet = "Cost Assumptions" ,range = "B9:C15",  col_names = FALSE)
CostAssumptions2<- read_excel("Vessel and Media Calcs OCWD.xlsx", sheet = "Cost Assumptions" ,range = "B37:C48" ,  col_names = FALSE)

C3 = bind_rows(CostAssumptions1,CostAssumptions2)
C3 = drop_na(C3)
names(C3) <- c("Parameter", "Value")

# 
# ##### Alternative Cost Estimate 
Media <- "GAC"
Model <- c("HP1220","HP1240")
Config <- c("Parallel", "LeadLag")

nms2 <- grep("Opt_Flow",names(WellSumsOptimized2))
OptFlowPlace <- as.vector(WellSumsOptimized2[,nms2])
OptFlowPlace<-as.data.frame(t(as.matrix(OptFlowPlace)))
OptFlowPlace <- tibble::rownames_to_column(OptFlowPlace, "name")

for (i in 1:length(Model)) {
  for (j in 1:length(Config)) {

String <- sprintf("%s_%s_%s", Media,Model[i],Config[j])

Column1 <- paste(String,"Opt_System", sep ='_')
Column2 <- paste(String,"Opt_Flow", sep ='_')

### Build it out by creating a string with sprint f for all the vessel and media options

Headloss = 20# Need to optimize at later point

nms3 <- grep(paste("Location", "WellCapacity", "Land" , "Alternative", "Pipeline" , String, sep='|'), names(WellSumsOptimized2))
AlternativeCost <- ungroup(WellSumsOptimized2[,nms3])
AlternativeCost <- AlternativeCost %>% mutate(VesselCosts = .[[6]]* Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "Cost"],
                                              MediaCost = 2 * .[[6]] * Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "CarbonCost"],
                                              BoosterPump = (Headloss/14.7*33.9)*WellCapacity/3960/0.7*C3$Value[C3$Parameter == "Booster Pump Station"],
                                             
                                              

                                              
                                              BackwashTanks =  if (Media== "GAC") { Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "BackwashTanks"]} else {0},
                                              BackwashPumps =  if (Media== "GAC") {  50000} else {0},
                                              Prefilter = if (Media== "GAC") {0} else {C3$Value[C3$Parameter == "Pre-filter"]*(ceiling(WellCapacity/2300)+1)} ,
                                              
                                              Site = (VesselCosts + MediaCost +BackwashTanks + BoosterPump+BackwashPumps + Prefilter) / (1-(Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "SiteGAC"]+C3$Value[C3$Parameter == "Structural"] +C3$Value[C3$Parameter == "E&IC"]))*Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "SiteGAC"],
                                              Structural = (VesselCosts + MediaCost +BackwashTanks + BoosterPump+ BackwashPumps + Prefilter) / (1-(Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "SiteGAC"]+C3$Value[C3$Parameter == "Structural"] +C3$Value[C3$Parameter == "E&IC"]))*C3$Value[C3$Parameter == "Structural"],
                                              EIC = (VesselCosts + MediaCost +BackwashTanks + BackwashPumps+BoosterPump+ Prefilter) / (1-(Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "SiteGAC"]+C3$Value[C3$Parameter == "Structural"] +C3$Value[C3$Parameter == "E&IC"]))*C3$Value[C3$Parameter == "E&IC"],
                                              LandCost = .[[6]]*Land*1500/43560 * C3$Value[C3$Parameter == "Land Acquisition"],
                                              `Total Direct Costs` = VesselCosts + MediaCost +BackwashTanks + BackwashPumps + BoosterPump +Prefilter + Site + Structural + EIC + LandCost +Pipeline,
                                              Contingency = as.numeric(C3[2,2])* `Total Direct Costs`,
                                              SubTotal_1 = `Total Direct Costs`  + Contingency,
                                              GeneralConditions =  as.numeric(C3[4,2])* SubTotal_1,
                                              SubTotal_2 = GeneralConditions +SubTotal_1,
                                              `Contractor Overhead and Profit` = as.numeric(C3[5,2])*SubTotal_2,
                                              Subtotal_3 = SubTotal_2 + `Contractor Overhead and Profit` ,
                                              SalesTax =   (SubTotal_1/1.5)*as.numeric(C3[6,2]),
                                              `Total Capital Cost` = Subtotal_3 + SalesTax,
                                              ProjectCosts = `Total Capital Cost` * as.numeric(C3[7,2]),
                                              `Total Project Costs` = `Total Capital Cost` + ProjectCosts,
                                              MediaChangeOut = if (Media == "GAC" & Config[j] == "Parallel") {(.[[7]]/Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "FlowGAC"])*Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "ReplaceGAC"]/0.75}
                                              else if (Media == "GAC" & Config[j] == "LeadLag") {(.[[7]]/Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "FlowGAC"])*Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "ReplaceGAC"]}
                                            
                                  
                                              else if (Media == "IX" & Config[j] == "Parallel") {(.[[7]]/Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "FlowGAC"])*Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "ReplaceIX"]/0.75}
                                              else {(.[[7]]/Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "FlowGAC"])*Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "ReplaceIX"]},
                                              
                                              ReplacementPrice = if (Config[j] =="Parallel") {2*.[[6]]*MediaChangeOut * Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "CarbonCost"]}
                                              else {1* .[[6]]*MediaChangeOut * Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "CarbonCost"]},
                                                
                                              Labor = as.numeric(C3[13,2]),
                                              Power = (Headloss/14.7*33.9)*WellCapacity/3960/0.7*0.75*365*24*as.numeric(C3[14,2]),
                                              OtherMaintenance = (VesselCosts + BackwashTanks + BackwashPumps + Prefilter) * as.numeric(C3[16,2]),
                                              Analytical = .[[6]]*12*2*as.numeric(C3[15,2]),
                                              Contingency2 = (ReplacementPrice + Labor + Power +OtherMaintenance + Analytical) * as.numeric(C3[17,2]),
                                              OnlineFactor = if("Online" %in% colnames(WellSums2))
                                                    { WellSums2$Online} else {0.9},
                                              `Annual O&M` = (ReplacementPrice + Labor + Power +OtherMaintenance + Analytical + Contingency2)*OnlineFactor,
                                              `30 yr CapitalCost` =  10*floor((`Total Project Costs` + 30*`Annual O&M`)/10)

)

AlternativeCost[is.na(AlternativeCost)] <- 0

AlternativeCost_summary <- AlternativeCost %>%  group_by(Alternative) %>% summarise(`Total Capital Cost` = sum(`Total Capital Cost`),
                                                                                    `Total Project Costs` = sum( `Total Project Costs`),
                                                                                    `Annual O&M` = sum ( `Annual O&M`),
                                                                                    `30 yr CapitalCost` = sum( `30 yr CapitalCost`))

AlternativeCost_summary$`Total Capital Cost` <- dollar(AlternativeCost_summary$`Total Capital Cost`)
AlternativeCost_summary$`Total Project Costs`<- dollar(AlternativeCost_summary$`Total Project Costs`)
AlternativeCost_summary$`Annual O&M` <- dollar(AlternativeCost_summary$`Annual O&M`)
AlternativeCost_summary$`30 yr CapitalCost`<- dollar(AlternativeCost_summary$`30 yr CapitalCost`)

# create a data frame to hold results
assign(sprintf("Cost_%s_%s_%s",Media, Model[i],Config[j]),AlternativeCost_summary)

# Alt Cost Summary BY LOCATION to hold results 
colnames(AlternativeCost)[6] <- "Vessel"
colnames(AlternativeCost)[7] <- "Flow"
assign(sprintf("LocationCost_%s_%s_%s",Media, Model[i],Config[j]),AlternativeCost)
  }}



##### Alternative Cost Estimate -----IX
Media <- "IX"
Model <- c("HP1220")
Config <- c("LeadLag")

for (i in 1:length(Model)) {
  for (j in 1:length(Config)) {

    String <- sprintf("%s_%s_%s", Media,Model[i],Config[j])
    
    Column1 <- paste(String,"Opt_System", sep ='_')
    Column2 <- paste(String,"Opt_Flow", sep ='_')


    ## For ND%: column 3 (Well capacity), column 6 (Vessels), Column 7 (effective flow)

    nms3 <- grep(paste("Location", "WellCapacity", "Land" , "Alternative", "Pipeline" , String, sep='|'), names(WellSumsOptimized2))
    AlternativeCost <- ungroup(WellSumsOptimized2[,nms3])
    AlternativeCost <- AlternativeCost %>% mutate(VesselCosts = .[[6]]* Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "Cost"],
                                                  MediaCost = 2 * .[[6]] * Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "ResinCost"],
                                                  BoosterPump = (Headloss/14.7*33.9)*WellCapacity/3960/0.7*C3$Value[C3$Parameter == "Booster Pump Station"],
                                                  
                                                  
                                                  
                                                  
                                                  BackwashTanks =  if (Media== "GAC") { Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "BackwashTanks"]} else {0},
                                                  BackwashPumps =  if (Media== "GAC") {  50000} else {0},
                                                  Prefilter = if (Media== "GAC") {0} else {C3$Value[C3$Parameter == "Pre-filter"]*(ceiling(WellCapacity/2300)+1)} ,
                                                  
                                                  Site = (VesselCosts + MediaCost +BackwashTanks + BoosterPump+BackwashPumps + Prefilter) / (1-(Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "SiteIX"]+C3$Value[C3$Parameter == "Structural"] +C3$Value[C3$Parameter == "E&IC"]))*Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "SiteIX"],
                                                  Structural = (VesselCosts + MediaCost +BackwashTanks + BoosterPump+ BackwashPumps + Prefilter) / (1-(Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "SiteIX"]+C3$Value[C3$Parameter == "Structural"] +C3$Value[C3$Parameter == "E&IC"]))*C3$Value[C3$Parameter == "Structural"],
                                                  EIC = (VesselCosts + MediaCost +BackwashTanks + BackwashPumps+BoosterPump+ Prefilter) / (1-(Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "SiteIX"]+C3$Value[C3$Parameter == "Structural"] +C3$Value[C3$Parameter == "E&IC"]))*C3$Value[C3$Parameter == "E&IC"],
                                                  LandCost = .[[6]]*Land*1500/43560 * C3$Value[C3$Parameter == "Land Acquisition"],
                                                  `Total Direct Costs` = VesselCosts + MediaCost +BackwashTanks + BackwashPumps + BoosterPump +Prefilter + Site + Structural + EIC + LandCost +Pipeline,
                                                  Contingency = as.numeric(C3[2,2])* `Total Direct Costs`,
                                                  SubTotal_1 = `Total Direct Costs`  + Contingency,
                                                  GeneralConditions =  as.numeric(C3[4,2])* SubTotal_1,
                                                  SubTotal_2 = GeneralConditions +SubTotal_1,
                                                  `Contractor Overhead and Profit` = as.numeric(C3[5,2])*SubTotal_2,
                                                  Subtotal_3 = SubTotal_2 + `Contractor Overhead and Profit` ,
                                                  SalesTax =   (SubTotal_1/1.5)*as.numeric(C3[6,2]),
                                                  `Total Capital Cost` = Subtotal_3 + SalesTax,
                                                  ProjectCosts = `Total Capital Cost` * as.numeric(C3[7,2]),
                                                  `Total Project Costs` = `Total Capital Cost` + ProjectCosts,
                                                  MediaChangeOut = (.[[7]]/Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "FlowIX"])*Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "ReplaceIX"],
                                           
                                                  
                                                  ReplacementPrice = if (Config[j] =="Parallel") {2* .[[6]]*MediaChangeOut * Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "ResinCost"]}
                                                  else {1 * .[[6]]*MediaChangeOut * Vessel2$Value[Vessel2$Vessel == Model[i] & Vessel2$Parameter == "ResinCost"]},
                                                
                                                  Labor = as.numeric(C3[13,2]),
                                                  Power = (Headloss/14.7*33.9)*WellCapacity/3960/0.7*0.75*365*24*as.numeric(C3[14,2]),
                                                  OtherMaintenance = (VesselCosts + BackwashTanks + BackwashPumps + Prefilter) * as.numeric(C3[16,2]),
                                                  Analytical = .[[6]]*12*2*as.numeric(C3[15,2]),
                                                  Contingency2 = (ReplacementPrice + Labor + Power +OtherMaintenance + Analytical) * as.numeric(C3[17,2]),
                                                  OnlineFactor = if("Online" %in% colnames(WellSums2))
                                                  { WellSums2$Online} else {0.9},
                                                  `Annual O&M` = (ReplacementPrice + Labor + Power +OtherMaintenance + Analytical + Contingency2)*OnlineFactor,
                                                  `30 yr CapitalCost` =  10*floor((`Total Project Costs` + 30*`Annual O&M`)/10)
                                
    )

    AlternativeCost[is.na(AlternativeCost)] <- 0


    AlternativeCost_summary <- AlternativeCost %>%  group_by(Alternative) %>% summarise(`Total Capital Cost` = sum(`Total Capital Cost`),
                                                                                        `Total Project Costs` = sum( `Total Project Costs`),
                                                                                        `Annual O&M` = sum ( `Annual O&M`),
                                                                                        `30 yr CapitalCost` = sum( `30 yr CapitalCost`))

    AlternativeCost_summary$`Total Capital Cost` <- dollar(AlternativeCost_summary$`Total Capital Cost`)
    AlternativeCost_summary$`Total Project Costs`<- dollar(AlternativeCost_summary$`Total Project Costs`)
    AlternativeCost_summary$`Annual O&M` <- dollar(AlternativeCost_summary$`Annual O&M`)
    AlternativeCost_summary$`30 yr CapitalCost`<- dollar(AlternativeCost_summary$`30 yr CapitalCost`)

    # create a data frame to hold results
    assign(sprintf("Cost_%s_%s_%s",Media, Model[i],Config[j]),AlternativeCost_summary)

    # Alt Cost Summary BY LOCATION to hold results
    colnames(AlternativeCost)[6] <- "Vessel"
    colnames(AlternativeCost)[7] <- "Flow"
    assign(sprintf("LocationCost_%s_%s_%s",Media, Model[i],Config[j]),AlternativeCost)
  }}


AppendMe <- function(dfNames) {
  do.call(rbind, lapply(dfNames, function(x) {
    cbind(get(x), source = x)
  }))
}


df3 <- AppendMe(c("Cost_GAC_HP1220_Parallel","Cost_GAC_HP1220_LeadLag", "Cost_GAC_HP1240_Parallel", "Cost_GAC_HP1240_LeadLag",  "Cost_IX_HP1220_LeadLag"))
df4 <- AppendMe(c("LocationCost_GAC_HP1220_Parallel","LocationCost_GAC_HP1220_LeadLag", "LocationCost_GAC_HP1240_Parallel", "LocationCost_GAC_HP1240_LeadLag", "LocationCost_IX_HP1220_LeadLag"))
df4$source <- gsub("LocationCost_","", df4$source)
df4$Location <- paste(df4$Location,df4$Alternative, sep = ".Alt")
df4 <- df4 %>% mutate (Alternative = NULL)%>% arrange(Location)

df5 <- df4  %>% filter(!grepl("Parallel",source))

df4sum <- df4[,c(1,2,25,36:38)] 
df4sum <- df4sum %>% arrange(Location)


df5 <- df5 %>%
  group_by(Location) %>%
  slice(which.min(`30 yr CapitalCost`))

df5 <- df5 %>% ungroup(Location) %>% transmute( Location, WellCapacity, Vessel,`Total Capital Cost` , `Annual O&M`, `30 yr CapitalCost` , source )

library(stringr)
out <- str_split_fixed(df5$Location, ".Alt", 2)
df5 <- cbind(out,df5[,-1])
colnames(df5)[2] <- "Alternative"
colnames(df5)[1] <- "Location"
df5 <- df5[,c(2,1,3,4,5,6,7,8)]

df5 <- df5 %>% arrange(Alternative)


CostbyAlternative <- df5 %>%  group_by(Alternative) %>% summarise(`Total Capital Cost` = sum(`Total Capital Cost`),
                                                                   `Annual O&M` = sum ( `Annual O&M`),
                                                                  `30 yr CapitalCost` = sum( `30 yr CapitalCost`))

CostbyAlternative[,-1] <- apply(CostbyAlternative[,-1], 2, dollar)
df5[,5:7] <- apply(df5[,5:7], 2, dollar)

WellSums3 <- WellSums3 %>% mutate (Pipeline = NULL, Land = NULL)
WellSumsOptimized2 <- WellSumsOptimized2 %>% mutate (Pipeline = NULL, Land = NULL)


 library(writexl)
write_xlsx(CostbyAlternative, path = paste(producer,"CostbyAlternative.xlsx", sep = " "),  col_names = TRUE, format_headers = TRUE)
write_xlsx(df5, path = paste(producer,"CostbyLocation.xlsx", sep = " "),  col_names = TRUE, format_headers = TRUE)


 write_xlsx(df4 , path = paste(producer,"Full.xlsx", sep = " "),  col_names = TRUE, format_headers = TRUE)
 write_xlsx(df4sum , path = paste(producer,"Summary .xlsx", sep = " "), col_names = TRUE, format_headers = TRUE)
 write_xlsx(WellSums3 , path = paste(producer,"Well Sums.xlsx", sep = " "), col_names = TRUE, format_headers = TRUE)
 write_xlsx(WellSumsOptimized2, path = paste(producer,"Well Sums Opt.xlsx", sep = " "), col_names = TRUE, format_headers = TRUE)
 write_xlsx(TotalSystems, path = paste(producer,"Systems.xlsx", sep = " "),col_names = TRUE, format_headers = TRUE)
 write_xlsx(TotalSystems2, path = paste(producer,"Systems Opt.xlsx", sep = " "), col_names = TRUE, format_headers = TRUE)

