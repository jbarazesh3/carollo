
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

path <- "OCWD Producer Alternative Prescreen 3.xlsx"
sheetnames <- excel_sheets(path)
mylist <- lapply(sheetnames, function(x) read_excel(path, sheet = x, range = "J2:M10"))
mylist[[1]] <- NULL
# name the dataframes
names(mylist) <- sheetnames[-1]

df_doCall <- do.call("rbind", mylist)

df_doCall <- tibble::rownames_to_column(df_doCall, "Producer")

df_doCall$Producer <- gsub('[[:digit:]]+|\\W', '', df_doCall$Producer )

df_doCall  <- df_doCall %>% drop_na()


cols.num <- c(  "Total Capital Cost" ,"Annual O&M"  ,       "30 yr CapitalCost" )


df_doCall[cols.num] <- apply(df_doCall[cols.num], 2, function(x) as.numeric(gsub('\\D', '', x))) 

#df_doCall <- df_doCall  %>% filter(!grepl("Parallel",source))

OPt <- df_doCall %>%
  group_by(Producer) %>%
  slice(which.min(`30 yr CapitalCost`))


AndrewGay <- as.data.frame(t(colSums(OPt[sapply(OPt, is.numeric)], na.rm = TRUE)))
rownames(AndrewGay) <- NULL
AndrewFinalGay <- dplyr::bind_rows(OPt, AndrewGay)

AndrewFinalGay <- AndrewFinalGay %>% drop_na()

library(writexl)
# library(rJava)
# library(xlsx)
write_xlsx(AndrewFinalGay , path = "OCWD Producer Alternative Prescreen2.xlsx", col_names = TRUE, format_headers = TRUE)
#write.xlsx(AndrewFinalGay, file="CWD Producer Alternative Prescreen.xlsx", sheetName="USA Arrests")
  
