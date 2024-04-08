library(tidyverse)
library(dplyr)
library(googlesheets4)

#clear console
rm(list = ls())
#clear environ. 
cat("\f")



web_export_weekly <- read.csv("./DROP_REPORT_HERE/weekly_export.csv", na.strings=c("","NA"))
CurrentDate <- Sys.Date()
path <- "./Weekly_Records/"
write.csv(web_export_weekly, file.path(path, paste0("web_export_weekly",CurrentDate,".csv")))


producer_fill <- function(T) {
  val <- unname(T[9])
  if(is.na(val)) {
    return("Forever 46, LLC - 00000057DCHF00477864")
  } 
  return(val)
}

#Apply producer fill in if NA
web_export_weekly$Inventory.Producer.Name <- apply(web_export_weekly, 1, FUN = function(T) producer_fill(T))

web_export_weekly = web_export_weekly %>%
  mutate(Point.of.Intended.Sale = "https://ilava.com/point-of-intended-sale/")

web_export_weekly = web_export_weekly %>%
  mutate(Extraction.Method = "https://ilava.com/extraction-methods/")

web_export_weekly = web_export_weekly %>% 
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

web_export_weekly <- select(web_export_weekly, 
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

web_export_weekly$Date.of.Harvest <- format(ymd(web_export_weekly$Date.of.Harvest), format = "%m/%d/%Y")
web_export_weekly$Date.of.Manufacture <- format(ymd(web_export_weekly$Date.of.Manufacture), format = "%m/%d/%Y")
web_export_weekly <- unique(web_export_weekly)

fix_report <- web_export_weekly %>% filter(is.na(Date.of.Harvest) | is.na(Date.of.Manufacture) | is.na(Laboratory.Testing.Results))
add_report <- web_export_weekly %>% filter(!is.na(Date.of.Harvest) & !is.na(Date.of.Manufacture) & !is.na(Laboratory.Testing.Results))

base_report <- read.csv("./Results/Base_Report.csv", na.strings=c("","NA"))
base_report <- base_report[, -1]
#save a copy for records
CurrentDate <- Sys.Date()
path <- "./Report_Records/"
write.csv(base_report, file.path(path, paste0("Base_Report",CurrentDate,".csv")))

base_report <- rbind(base_report,add_report)
base_report <- unique(base_report)
base_report$Date.of.Manufacture <- mdy(base_report$Date.of.Manufacture)
base_report <- base_report[order(base_report$Date.of.Manufacture, decreasing = TRUE),]
base_report$Date.of.Manufacture <- format(ymd(base_report$Date.of.Manufacture), format = "%m/%d/%Y")

path <- "./RESULTS/"
write.csv(base_report, file.path(path, "Base_Report.csv"))
write.csv(fix_report, file.path(path, "Fix_Report.csv"))


