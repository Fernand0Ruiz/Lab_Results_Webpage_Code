library(tidyverse)
library(dplyr)

#clear console
rm(list = ls())
#clear environ. 
cat("\f")

#Read in CSV in the folder COA, make all empty values NA
#From Looker
BI_export <- read.csv("./COA/BI_PULL.csv", na.strings=c("","NA"))
#From Dutchie 
cult_export <- read.csv("./COA/Cultivation.csv", na.strings=c("","NA"))
downtown_export <- read.csv("./COA/Downtown.csv", na.strings=c("","NA"))
wholesale_export <- read.csv("./COA/Wholesale.csv", na.strings=c("","NA"))

#Select the package ID to serve as the merging value.
#Select the observation Manufacture Dates to perform Table calculation.
cult_table <- cult_export %>% select(Package.ID,Date.of.manufacture)
names(cult_table) <- c("package_ID","Date.of.manufacture.Cult")
downtown_table <- downtown_export %>% select(Package.ID,Date.of.manufacture)
names(downtown_table) <- c("package_ID","Date.of.manufacture.Downtown")
wholesale_table <- wholesale_export %>% select(Package.ID,Date.of.manufacture)
names(wholesale_table) <- c("package_ID","Date.of.manufacture.Wholesale")

#convert all Manufacture Dates to class Date
#This is necessary for the func. date_selector picks the oldest date. 
BI_export$Manufacturing.Date <- mdy(BI_export$Manufacturing.Date)
cult_table$Date.of.manufacture.Cult <- mdy(cult_table$Date.of.manufacture.Cult)
downtown_table$Date.of.manufacture.Downtown <- mdy(downtown_table$Date.of.manufacture.Downtown)
wholesale_table$Date.of.manufacture.Wholesale<- mdy(wholesale_table$Date.of.manufacture.Wholesale)

#Join Looker export with the Dutchie locations. 
#Used temp vars. to maintain the BI_export as the primary table
join_cult <- left_join(BI_export, cult_table, by="package_ID")
join_downtown <- left_join(join_cult, downtown_table, by="package_ID")
Final_WebExport <- unique(left_join(join_downtown, wholesale_table, by="package_ID"))

#function that takes in any amount of params and checks for equality. 
AllIdentical <- function(...){
  lst <- list(...)
  for(i in seq_along(lst)[-1]){
    if(!identical(lst[[i]], lst[[1]])){
      return(FALSE)
    }
  }
  return(TRUE)
}

#function to decide which date to select based on every possible occurrence
date_selector <- function(T) {
  A <- unname(T[11])
  B <- unname(T[12])
  C <- unname(T[13])
  if (AllIdentical(A, B, C)) {
    #if all empty returns NA
    #if not returns the val because all are equal
    return (A)
  } else if(AllIdentical(A, B) && is.na(C)){
    return (A)
  } else if(AllIdentical(B, C) && is.na(A)) {
    return (B)
  } else if(AllIdentical(A, C) && is.na(B)) {
    return (C)
  } else if (is.na(A) == FALSE && is.na(B) && is.na(C)) {
    return (A)
  } else if (is.na(A) && is.na(B) == FALSE && is.na(C)) {
    return (B)
  } else if (is.na(A) && is.na(B) && is.na(C) == FALSE) {
    return (C)
  } else {
    #Met none of the conditions so must have different Manu. Dates across the locations
    return ("CONFLICT")
  }
}

#main function handler for deciding which Manu.Date gets selected.
date_merger <- function(T) {
  main <- unname(T[10])#Looker date
  A <- unname(T[11])#cult date
  B <- unname(T[12])#downtown date
  C <- unname(T[13])#wholesale date
  
  if(is.na(main)){
    #main was empty so we call date_selector to decide which date we will use in final col.
    ret <- date_selector(T)
    if (AllIdentical(ret, "CONFLICT")) {
      #conflict was detected so we use the oldest Manu. Date available. 
      min_date <- min(main, A, B, C, na.rm = TRUE)
      return (min_date)
    } else {
      #No conflict a Manu. Date was selected. 
      return(ret)
    }
  } else {
    #Looker (BI_export) already has a Manu. Date and
    #doesn't require comparison amongst the other locations
    return (main)
  }
}

#function handles NA for Producer, setting them to default license. 
producer_fill <- function(T) {
  val <- unname(T[9])
  if(is.na(val)) {
    return("Forever 46, LLC - 00000057DCHF00477864")
  } 
  return(val)
}

merge_row <- function(x) {
  min_ <- min(x, na.rm = TRUE)
  if(is.infinite(min_ )){
    return (NA)
  }
  return (min_)
}


#Apply Manu. Date selection process
Final_WebExport$Manu.Date.Merge <- apply(Final_WebExport, 1, FUN = function(T) date_merger(T)) 

#Apply producer fill in if NA
Final_WebExport$Inventory.Producer.Name <- apply(Final_WebExport, 1, FUN = function(T) producer_fill(T))

#Remove Manu. Date columns from other locations used in Manu. Date Selection. 
Final_WebExport_CLEAN <- unique(Final_WebExport %>% select(-c(Manufacturing.Date,Date.of.manufacture.Cult, Date.of.manufacture.Downtown, Date.of.manufacture.Wholesale)))

Final_WebExport_CLEAN <- Final_WebExport_CLEAN[order(Final_WebExport_CLEAN$Manu.Date.Merge, decreasing = TRUE),]

#solves the many-to-many relationship by combining rows that match package_IDs using the min() of the values. 
Final_WebExport_CLEAN <- Final_WebExport_CLEAN %>%
  group_by(Products.Brand.Name, Inventory.Conversion.Inputs.Outputs.Output.Product, Strain.Name, Inventory.Conversions.Batch.Name,package_ID, Products.Vendor.Name, Inventory.Producer.Name) %>%
  summarise(across(c(Batch.Harvest.Date, Batch.Lab.Results.Lab.Result.URL, Manu.Date.Merge), list(merge_row)))

#FIX_THESE_BEFORE<- BI_export  %>% filter(is.na(Batch.Harvest.Date) | is.na(Manufacturing.Date) | is.na(Batch.Lab.Results.Lab.Result.URL))
#FIX_THESE_AFTER <- Final_WebExport_CLEAN  %>% filter(is.na(Batch.Harvest.Date_1) | is.na(Manu.Date.Merge_1) | is.na(Batch.Lab.Results.Lab.Result.URL_1))

#_____________________________________
#fixed_export <- read.csv("./COA/fixed_coas.csv", na.strings=c("","NA"))
#should add 212 results
#Final_WebExport_CLEAN <- rbind(Final_WebExport_CLEAN, fixed_export)
#Final_WebExport_CLEAN <- Final_WebExport_CLEAN  %>% filter(is.na(Batch.Harvest.Date_1) == FALSE & is.na(Manu.Date.Merge_1) == FALSE & is.na(Batch.Lab.Results.Lab.Result.URL_1) == FALSE)
#Final_WebExport_CLEAN <- unique(Final_WebExport_CLEAN)
#____________________________________


Final_WebExport_CLEAN = Final_WebExport_CLEAN %>%
  mutate("Point of Intended Sale" = "https://ilava.com/point-of-intended-sale/")

Final_WebExport_CLEAN = Final_WebExport_CLEAN %>%
  mutate("Extraction Method" = "https://ilava.com/extraction-methods/")

Final_WebExport_CLEAN = Final_WebExport_CLEAN %>% 
  rename(
    "Brand" = Products.Brand.Name,
    "Product" = Inventory.Conversion.Inputs.Outputs.Output.Product,
    "Strain" = Strain.Name, 
    "Batch" = Inventory.Conversions.Batch.Name,
    "Package ID" = package_ID,
    "Distribution Chain" = Products.Vendor.Name, 
    "Cultivated By" = Inventory.Producer.Name,
    Date.of.Harvest = Batch.Harvest.Date_1, 
    Laboratory.Testing.Results = Batch.Lab.Results.Lab.Result.URL_1, 
    Date.of.Manufacture = Manu.Date.Merge_1
  )

Final_WebExport_CLEAN <- select(Final_WebExport_CLEAN, 
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

#filter an rows that have an empty value
Final_WebExport_CLEANER <-  Final_WebExport_CLEAN %>% filter(!is.na(Date.of.Harvest) & !is.na(Date.of.Manufacture) & !is.na(Laboratory.Testing.Results))
#set manufacture dates to desc order, to handle duplicate IDs with different min dates, so the min date is still selected. 
Final_WebExport_CLEANER <- Final_WebExport_CLEANER[order(Final_WebExport_CLEANER$Date.of.Manufacture, decreasing = TRUE),]
#convert manu. date back into a string in the normal format.
Final_WebExport_CLEANER$Date.of.Manufacture <- format(ymd(Final_WebExport_CLEANER$Date.of.Manufacture), format = "%m/%d/%Y")

#Writes the Final cleaned up version of Looker export with the dates processed and producer values filled in. 
write.csv(Final_WebExport_CLEAN, "Final_WebExport.csv")

#write.csv(FIX_THESE_AFTER, "COA_FIX.csv")