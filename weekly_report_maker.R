library(tidyverse)
library(dplyr)
library(googlesheets4)

#clear console
rm(list = ls())
#clear environ. 
cat("\f")

web_export_weekly <- read.csv("./Weekly_Reports/web_export_weekly.csv", na.strings=c("","NA"))

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
  mutate("Point of Intended Sale" = "https://ilava.com/point-of-intended-sale/")

web_export_weekly = web_export_weekly %>%
  mutate("Extraction Method" = "https://ilava.com/extraction-methods/")

web_export_weekly = web_export_weekly %>% 
  rename(
    "Brand" = Products.Brand.Name,
    "Product" = Inventory.Conversion.Inputs.Outputs.Output.Product,
    "Strain" = Strain.Name, 
    "Batch" = Inventory.Conversions.Batch.Name,
    "Package ID" = Inventory.Conversion.Inputs.Outputs.Output.Package.ID,
    "Distribution Chain" = Products.Vendor.Name, 
    "Cultivated By" = Inventory.Producer.Name,
    Date.of.Harvest = Batch.Harvest.Date, 
    Laboratory.Testing.Results = Batch.Lab.Results.Lab.Result.URL, 
    Date.of.Manufacture = Manufacturing.Date
  )

web_export_weekly <- select(web_export_weekly, 
    "Package ID",
    "Brand", 
    "Product", 
    "Batch", 
    "Strain", 
    "Distribution Chain", 
    Date.of.Harvest,
    Date.of.Manufacture, 
    "Cultivated By", 
    Laboratory.Testing.Results,
    "Point of Intended Sale", 
    "Extraction Method"
)

web_export_weekly <- unique(web_export_weekly)

web_export_weekly$Date.of.Manufacture <- format(as.Date(web_export_weekly$Date.of.Manufacture), "%m/%d/%Y")
web_export_weekly$Date.of.Harvest <- format(as.Date(web_export_weekly$Date.of.Harvest), "%m/%d/%Y")

report_empty <- web_export_weekly %>% filter(is.na(Date.of.Harvest) | is.na(Date.of.Manufacture) | is.na(Laboratory.Testing.Results))
web_export_weekly <-  web_export_weekly %>% filter(!is.na(Date.of.Harvest) & !is.na(Date.of.Manufacture) & !is.na(Laboratory.Testing.Results))

x <- read_sheet('https://docs.google.com/spreadsheets/d/1VJ8YKwPgI75YzDPI36L7KNHmLjsoBfOzowItWZWnNhQ/edit?usp=sharing')
sheet_append("1VJ8YKwPgI75YzDPI36L7KNHmLjsoBfOzowItWZWnNhQ", web_export_weekly, sheet = 1)

