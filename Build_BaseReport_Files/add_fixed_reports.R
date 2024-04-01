library(tidyverse)
library(dplyr)
library(DescTools)

#clear console
rm(list = ls())
#clear environ. 
cat("\f")

#Read in CSV in the folder COA, make all empty values NA
#From Looker
COA_Base_Report <- read.csv("Final_WebExport_CLEAN.csv", na.strings=c("","NA"))
#From Dutchie 
add_export <- read.csv("COA_ADD.csv", na.strings=c("","NA"))
         
         
add_export$Package.ID <- format(add_export$Package.ID, scientific = FALSE)

add_export %>%
  mutate(Package.ID = as.character(add_export$Package.ID))

add_export$Package.ID <- cbind(StrAlign(add_export$Package.ID, sep="\\l"))

View(add_export)

COA_Base_Report <- COA_Base_Report[, -1]
View(COA_Base_Report)
combined_report <- rbind(COA_Base_Report, add_export)

combined_report$Date.of.Manufacture <- mdy(combined_report$Date.of.Manufacture)

combined_report <- combined_report[order(combined_report$Date.of.Manufacture, decreasing = TRUE),]

combined_report$Date.of.Manufacture <- format(ymd(combined_report$Date.of.Manufacture), format = "%m/%d/%Y")
combined_report <- unique(combined_report)
n_occur <- data.frame(table(combined_report$Package.ID))

write.csv(combined_report, "Base_Report.csv")


weekly <- read.csv("download.csv", na.strings=c("","NA"))