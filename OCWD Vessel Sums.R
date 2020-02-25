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
mylist <- lapply(sheetnames, function(x) read_excel(path, sheet = x, range = "A2:H50"))
mylist[[1]] <- NULL
# name the dataframes
names(mylist) <- sheetnames[-1]

df_doCall <- do.call("rbind", mylist)

df_doCall  <- df_doCall %>% transmute(Systems, source) %>% drop_na()

summary <- df_doCall %>% group_by(source) %>% summarise(count = sum(Systems))

library(writexl)

write_xlsx(summary , path = "OCWD Vessel Summary.xlsx", col_names = TRUE, format_headers = TRUE)
