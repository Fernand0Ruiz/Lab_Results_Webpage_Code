library(tidyverse)
library(dplyr)
library(lubridate)
library(googlesheets4)
library("stringr") 

rm(list = ls())
cat("\f")

RSO_report <- read.csv("./Results/RSO_Report.csv", na.strings=c("","NA"))
RSO_report <- RSO_report[, -1]

base_report <- read.csv("./Results/Base_Report.csv", na.strings=c("","NA"))
base_report <- base_report[, -1]

save_report <- base_report[!(base_report$Package.ID %in% RSO_report$Package.ID),]
ab = rbind(save_report, RSO_report)

path <- "./RESULTS/"
write.csv(ab, file.path(path, "Base_Report2.csv"))

#RSO_report <- base_report[str_detect(base_report$Product, "RSO"),]

#path <- "./RESULTS/"
#write.csv(RSO_report, file.path(path, "RSO_Report.csv"))