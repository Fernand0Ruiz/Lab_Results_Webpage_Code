library(tidyverse)
library(dplyr)
library(DescTools)

#clear console
rm(list = ls())
#clear environ. 
cat("\f")

base_report <- read.csv("./Results/Base_Report.csv", na.strings=c("","NA"))
base_report <- base_report[, -1]

CurrentDate <- format(Sys.time(),'%A,_%B_%d,_%Y_%H-%M-%S')
path <- "./Base_Report_history/"
write.csv(base_report, file.path(path, paste0("Base_Report",CurrentDate,".csv")))

add_export <- read.csv("./ADD_FIXED_REPORTS_HERE/Fixed_Report.csv", na.strings=c("","NA"))

add_export$Package.ID <- format(add_export$Package.ID, scientific = FALSE)

add_export %>%
  mutate(Package.ID = as.character(add_export$Package.ID))

add_export$Package.ID <- cbind(StrAlign(add_export$Package.ID, sep="\\l"))

path <- "./Fixed_Report_history/"
write.csv(add_export, file.path(path, paste0("Fixed_Report",CurrentDate,".csv")))

combined_report <- rbind(base_report, add_export)

combined_report$Date.of.Manufacture <- mdy(combined_report$Date.of.Manufacture)

combined_report <- combined_report[order(combined_report$Date.of.Manufacture, decreasing = TRUE),]

combined_report$Date.of.Manufacture <- format(ymd(combined_report$Date.of.Manufacture), format = "%m/%d/%Y")
#test <- combined_report[duplicated(combined_report), ]
#test2 <- intersect(base_report, add_export)
combined_report <- unique(combined_report)

#n_occur <- data.frame(table(combined_report$Package.ID))

path <- "./RESULTS/"
write.csv(combined_report, file.path(path, "Base_Report.csv"))