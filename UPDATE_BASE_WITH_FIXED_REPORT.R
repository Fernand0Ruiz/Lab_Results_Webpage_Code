library(tidyverse)
library(dplyr)
library(DescTools)

#clear console
rm(list = ls())
#clear environ. 
cat("\f")

#read in base report and remove count column
base_report <- read.csv("./Results/Base_Report.csv", na.strings=c("","NA"))
base_report <- base_report[, -1]

#grab sys date
CurrentDate <- format(Sys.time(),'%A,_%B_%d,_%Y_%H-%M-%S')
#record a copy of the basereport
path <- "./Base_Report_history/"
write.csv(base_report, file.path(path, paste0("Base_Report",CurrentDate,".csv")))

#read in the fixed report to be added to the base report
add_export <- read.csv("./ADD_FIXED_REPORTS_HERE/Fixed_Report.csv", na.strings=c("","NA"))
#format the package ID to not be in Sci. notation 
add_export$Package.ID <- format(add_export$Package.ID, scientific = FALSE)

add_export %>%
  mutate(Package.ID = as.character(add_export$Package.ID))

add_export$Package.ID <- cbind(StrAlign(add_export$Package.ID, sep="\\l"))

#record a copy of the fixed report
path <- "./Fixed_Report_history/"
write.csv(add_export, file.path(path, paste0("Fixed_Report",CurrentDate,".csv")))

#combine basereport with addreport
combined_report <- rbind(base_report, add_export)
#format the manu. date to a date format type, so that it can be ordered by decreasing order
combined_report$Date.of.Manufacture <- mdy(combined_report$Date.of.Manufacture)
combined_report <- combined_report[order(combined_report$Date.of.Manufacture, decreasing = TRUE),]

#convert date format type back to a string
combined_report$Date.of.Manufacture <- format(ymd(combined_report$Date.of.Manufacture), format = "%m/%d/%Y")
#get rid of duplicates. 
combined_report <- unique(combined_report)

#write the new basereport to the results folder. 
path <- "./RESULTS/"
write.csv(combined_report, file.path(path, "Base_Report.csv"))