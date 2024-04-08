library(tidyverse)
library(dplyr)
library(lubridate)
library(googlesheets4)

rm(list = ls())
cat("\f")

CurrentDate <- format(Sys.time(),'%A,_%B_%d,_%Y_%H-%M-%S')
daily_report <- read.csv("./DROP_DAILY_REPORT_HERE/daily_report.csv", na.strings=c("","NA"))

path <- "./Dropped_Report_history/"
write.csv(daily_report, file.path(path, paste0("daily_report",CurrentDate,".csv")))


producer_fill <- function(T) {
  val <- unname(T[9])
  if(is.na(val)) {
    return("Forever 46, LLC - 00000057DCHF00477864")
  } 
  return(val)
}

#Apply producer fill in if NA
daily_report$Inventory.Producer.Name <- apply(daily_report, 1, FUN = function(T) producer_fill(T))

daily_report = daily_report %>%
  mutate(Point.of.Intended.Sale = "https://ilava.com/point-of-intended-sale/")

daily_report = daily_report %>%
  mutate(Extraction.Method = "https://ilava.com/extraction-methods/")

daily_report = daily_report %>% 
  rename(
    "Brand" = Products.Brand.Name,
    "Product" = Inventory.Conversion.Inputs.Outputs.Output.Product,
    "Strain" = Strain.Name, 
    "Batch" = Inventory.Conversions.Batch.Name,
    Package.ID = Inventory.Conversion.Inputs.Outputs.Output.Package.ID,
    Distribution.Chain = Products.Vendor.Name, 
    Cultivated.By = Inventory.Producer.Name,
    Date.of.Harvest = Batch.Harvest.Date, 
    Laboratory.Testing.Results = Batch.Lab.Results.Lab.Result.URL, 
    Date.of.Manufacture = Manufacturing.Date
  )

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

daily_report$Date.of.Harvest <- format(ymd(daily_report$Date.of.Harvest), format = "%m/%d/%Y")
daily_report$Date.of.Manufacture <- format(ymd(daily_report$Date.of.Manufacture), format = "%m/%d/%Y")
daily_report <- unique(daily_report)

fix_report <- daily_report %>% filter(is.na(Date.of.Harvest) | is.na(Date.of.Manufacture) | is.na(Laboratory.Testing.Results))
add_report <- daily_report %>% filter(!is.na(Date.of.Harvest) & !is.na(Date.of.Manufacture) & !is.na(Laboratory.Testing.Results))

base_report <- read.csv("./Results/Base_Report.csv", na.strings=c("","NA"))
base_report <- base_report[, -1]
#base_report <- unique(base_report)
#test <- base_report[duplicated(base_report), ]
#base_report2 <- unique(base_report)
path <- "./Base_Report_history/"
write.csv(base_report, file.path(path, paste0("Base_Report",CurrentDate,".csv")))

base_report_new <- rbind(base_report,add_report)
#test <- base_report[duplicated(base_report), ]
base_report_new <- unique(base_report_new)
base_report_new$Date.of.Manufacture <- mdy(base_report_new$Date.of.Manufacture)
base_report_new <- base_report_new[order(base_report_new$Date.of.Manufacture, decreasing = TRUE),]
base_report_new$Date.of.Manufacture <- format(ymd(base_report_new$Date.of.Manufacture), format = "%m/%d/%Y")
base_report_new <- unique(base_report_new)
path <- "./Fix_Report_history/"
write.csv(fix_report, file.path(path, paste0("Fix_Report",CurrentDate,".csv")))

path <- "./RESULTS/"
write.csv(base_report_new, file.path(path, "Base_Report.csv"))
write.csv(fix_report, file.path(path, "Fix_Report.csv"))

