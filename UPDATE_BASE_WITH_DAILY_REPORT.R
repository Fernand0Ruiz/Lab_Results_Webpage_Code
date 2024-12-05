library(tidyverse)
library(dplyr)
library(lubridate)
library(googlesheets4)

#Clear console
rm(list = ls())
#clear environ.
cat("\f")

#Take sys date to append to filename for recording history.
CurrentDate <- format(Sys.time(),'%A,_%B_%d,_%Y_%H-%M-%S')

#Read in DailyReport from Looker, this is emailed downloaded and dropped in the folder DROP_DAILY_REPORT_HERE
daily_report <- read.csv("./DROP_DAILY_REPORT_HERE/daily_report.csv", na.strings=c("","NA"))

#Record Daily Report, goes to folder Dropped_Report_history
path <- "./Dropped_Report_history/"
write.csv(daily_report, file.path(path, paste0("daily_report",CurrentDate,".csv")))

#fcn used to populate Producer name if empty, defaults to our License 
producer_fill <- function(T) {
  val <- unname(T[9])
  if(is.na(val)) {
    return("Forever 46, LLC - 00000057DCHF00477864")
  } 
  return(val)
}

#Populate empty producer using above fcn
daily_report$Inventory.Producer.Name <- apply(daily_report, 1, FUN = function(T) producer_fill(T))

#adds column with the link to POS
daily_report = daily_report %>%
  mutate(Point.of.Intended.Sale = "https://ilava.com/point-of-intended-sale/")

#adds column with the link to Extraction Methods
daily_report = daily_report %>%
  mutate(Extraction.Method = "https://ilava.com/extraction-methods/")

#rename Columns of the dailyreport from Looker/Bi-tools
daily_report = daily_report %>% 
  rename(
    "Brand" = Products.Brand.Name,
    "Product" = Inventory.Conversion.Inputs.Outputs.Output.Product,
    "Strain" = Batch.Strain.Name, 
    "Batch" = Inventory.Conversions.Batch.Name,
    Package.ID = Inventory.Conversion.Inputs.Outputs.Output.Package.ID,
    Distribution.Chain = Products.Vendor.Name, 
    Cultivated.By = Inventory.Producer.Name,
    Date.of.Harvest = Batch.Harvest.Date, 
    Laboratory.Testing.Results = Batch.Lab.Results.Lab.Result.URL, 
    Date.of.Manufacture = Manufacturing.Date
  )

#Reorganize the columns for aethetics for the website
daily_report <- select(daily_report, 
                            Package.ID,
                            "Brand", 
                            "Product", 
                            "Batch", 
                            "Strain", 
                            Distribution.Chain, 
                            Date.of.Harvest,
                            Date.of.Manufacture, 
                            Cultivated.By, 
                            Laboratory.Testing.Results,
                            Point.of.Intended.Sale, 
                            Extraction.Method
)

#make the date columns of the daily report a date format to be binded with the basereport
#doing this to prevent any format compatibility issues
daily_report$Date.of.Harvest <- format(ymd(daily_report$Date.of.Harvest), format = "%m/%d/%Y")
daily_report$Date.of.Manufacture <- format(ymd(daily_report$Date.of.Manufacture), format = "%m/%d/%Y")
#apply unique
daily_report <- unique(daily_report)

#Create a table of daily_report entries that need to be fixed
fix_report <- daily_report %>% filter(is.na(Date.of.Harvest) | is.na(Date.of.Manufacture) | is.na(Laboratory.Testing.Results))
path <- "./Fix_Report_history/"
write.csv(fix_report, file.path(path, paste0("Fix_Report",CurrentDate,".csv")))
#Create a table of daily_report entries that are not missing data and will be added to the basereport. 
add_report <- daily_report %>% filter(!is.na(Date.of.Harvest) & !is.na(Date.of.Manufacture) & !is.na(Laboratory.Testing.Results))

#read in the base report
base_report <- read.csv("./Results/Base_Report.csv", na.strings=c("","NA"))
#remove the column count column
base_report <- base_report[, -1]

#record a copy of the base report
path <- "./Base_Report_history/"
write.csv(base_report, file.path(path, paste0("Base_Report",CurrentDate,".csv")))

#add the valid entrees to the basereport
base_report_new <- rbind(base_report,add_report)

#set manu. date formating type so that it can be organized in decreasing order.
base_report_new$Date.of.Manufacture <- mdy(base_report_new$Date.of.Manufacture)
base_report_new <- base_report_new[order(base_report_new$Date.of.Manufacture, decreasing = TRUE),]
#set to date format string
base_report_new$Date.of.Manufacture <- format(ymd(base_report_new$Date.of.Manufacture), format = "%m/%d/%Y")
#get rid of duplicates
base_report_new <- unique(base_report_new)

#Work is all done, now we write the results to the Results folder
#record a copy of the fixed report
path <- "./Fix_Report_history/"
write.csv(fix_report, file.path(path, paste0("Fix_Report",CurrentDate,".csv")))

#write the fix report and basereport to the results. 
#fixed report is fixed manually and basereport is website ready.
path <- "./RESULTS/"
write.csv(base_report_new, file.path(path, "Base_Report.csv"))
write.csv(fix_report, file.path(path, "Fix_Report.csv"))

