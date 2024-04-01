library(tidyverse)
library(dplyr)
library(googlesheets4)

#clear console
rm(list = ls())
#clear environ. 
cat("\f")



web_export_weekly <- read.csv("./Weekly_Reports/web_export_weekly_4wks.csv", na.strings=c("","NA"))

#web_export_weekly$Batch.Harvest.Date <- ymd(web_export_weekly$Batch.Harvest.Date)
#web_export_weekly$Manufacturing.Date <- ymd(web_export_weekly$Manufacturing.Date)

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
add_report <-  web_export_weekly %>% filter(!is.na(Date.of.Harvest) & !is.na(Date.of.Manufacture) & !is.na(Laboratory.Testing.Results))

Base_Report <- read.csv("Base_Report.csv", na.strings=c("","NA"))
Base_Report <- Base_Report[, -1]

report_test <- rbind(Base_Report,add_report)

report_test <- unique(report_test)

report_test$Date.of.Manufacture <- mdy(report_test$Date.of.Manufacture)

report_test <- report_test[order(report_test$Date.of.Manufacture, decreasing = TRUE),]

report_test$Date.of.Manufacture <- format(ymd(report_test$Date.of.Manufacture), format = "%m/%d/%Y")
write.csv(report_test, "Base_Report2.csv")
View(report_test)