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
Orange <- read_excel("Anaheim Alternatives Template.xlsx", sheet = "Vessel Calcs" ,range = cell_cols("A:I"))

Orange <- Orange %>% transmute( Well, WellCapacity, PFOS, PFOA)
Orange <- unique(Orange)

cols.num <- c( "PFOS" , "PFOA" )
Orange[cols.num] <- apply(Orange[cols.num], 2, as.numeric) 

Orange2 <- Orange %>% mutate(Limiting = if(PFOS < 32 & PFOA < 8) {0} 
                             else if((1-32/PFOS) >= (1-8/PFOA)) {1-32/PFOS}
                             else {1-8/PFOA} ,
                             Species= if((1-32/PFOS) >= (1-8/PFOA)) {"PFOS"} else {"PFOA"})

Orange2[,5][Orange2[,5] <0] <- 0

Orange2 <- Orange2 %>% mutate (BlendPercent = 100*(1 - Limiting ),
                               EffectiveBlend = WellCapacity*(1-Limiting),
                               Import = WellCapacity - EffectiveBlend )


library(writexl)
write_xlsx(Orange2, path = "Blend.xlsx", col_names = TRUE, format_headers = TRUE)