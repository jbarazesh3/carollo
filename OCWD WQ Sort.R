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
library(RColorBrewer)
library(readr)
library(reshape2)
library(purrr)
library(cowplot)
library(viridis)
library(data.table)

WQ <- read_excel("OCWD Producer Water Quality.xlsx", sheet = "MasterWQ" , range = cell_cols("A:AK"))

WQ <- WQ[-1,]

WQ[, c(-1,-2)] <- sapply(WQ[, c(-1,-2)], as.numeric)

WQ2 <- WQ %>% arrange(desc(PFOAOS), desc(TOC), desc(TDS), desc(Cl), desc(SO4))


library(writexl)
write_xlsx(WQ2, path = "OCWD WQ sort.xlsx",   col_names = TRUE, format_headers = TRUE)