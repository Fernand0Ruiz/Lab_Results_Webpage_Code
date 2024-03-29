library(tidyverse)
library(dplyr)
library(googlesheets4)

base_report      <- read_sheet('https://docs.google.com/spreadsheets/d/1VJ8YKwPgI75YzDPI36L7KNHmLjsoBfOzowItWZWnNhQ/edit?usp=sharing', sheet = 1)
weekly_report    <- read_sheet('https://docs.google.com/spreadsheets/d/1VJ8YKwPgI75YzDPI36L7KNHmLjsoBfOzowItWZWnNhQ/edit?usp=sharing', sheet = 2)
attention_report <- read_sheet('https://docs.google.com/spreadsheets/d/1VJ8YKwPgI75YzDPI36L7KNHmLjsoBfOzowItWZWnNhQ/edit?usp=sharing', sheet = 3)


main <- function() {
  #clear console
  rm(list = ls())
  #clear environ. 
  cat("\f")
  
  print("Howdy, thanks for choosing to run this R Program!\n")
  print("This program was created by Fernando Ruiz (fernando@ilava.com).\n")
  print("Send email if you encounter any errors or have any suggestions!\n")
  print("We will begin the COA Update...")
  print("LOADING... (est: 10 seconds)")
  Sys.sleep(10)
  
  
  print("Okay let's check if you have any data in the sheet titled 'attention!_' to see if we need to add any previously invalid rows!\n")
  print("checking...\n")
  Sys.sleep(5)
  if(length(attention_report) == 0) {
    print("Awesome, we found no rows in in the sheet titled 'attention!_' that need to be added!\n")
  } else {
    attention_invalid <- attention_report %>% filter(is.na(Date.of.Harvest) | is.na(Date.of.Manufacture) | is.na(Laboratory.Testing.Results))
    attention_valid <- attention_report %>% filter(!is.na(Date.of.Harvest) & !is.na(Date.of.Manufacture) & !is.na(Laboratory.Testing.Results))
    
    if(length(attention_valid) > 0) {
      #base_report <- rbind(base_report, attention_valid)
      #base_report <- unique(base_report)
      #base_report <- base_report[order(base_report$"Date of Manufacture", decreasing = TRUE),]
      sheet_append("1VJ8YKwPgI75YzDPI36L7KNHmLjsoBfOzowItWZWnNhQ",  attention_valid , sheet = 3)
      print("We added ")
      print(length(attention_valid))
      print(" rows to the base_report.\n")
    }
    
    if(length(attention_invalid) > 0) {
      sheet_append("1VJ8YKwPgI75YzDPI36L7KNHmLjsoBfOzowItWZWnNhQ",  attention_invalid , sheet = 3)
      print("We found ")
      print(length(attention_invalid))
      print(" that need to still be fixed.\n")
    }
    
    #sheet_write("1VJ8YKwPgI75YzDPI36L7KNHmLjsoBfOzowItWZWnNhQ", attention_invalid , sheet = 3)
  }
  
  
  print("Okay let's check if a Weekly Report needs to be added to the Base Report.\n")
  Sys.sleep(5)
  if(length(weekly_report) == 0) {
    print("Weekly Report is EMPTY please populate the googlesheet titled 'weekly_report' with the report from Looker.\n")
  } else {
    #save copy of previous report as History
    weekly_report_append <- set_up_weekly_report(weekly_report)
    attention_found <- weekly_report_append %>% filter(is.na(Date.of.Harvest) | is.na(Date.of.Manufacture) | is.na(Laboratory.Testing.Results))
    weekly_report_append <-  weekly_report_append %>% filter(!is.na(Date.of.Harvest) & !is.na(Date.of.Manufacture) & !is.na(Laboratory.Testing.Results))

    #base_report <- rbind(base_report, weekly_report)
    #base_report <- unique(base_report)
    #base_report <- base_report[order(base_report$"Date of Manufacture", decreasing = TRUE),]

    #range_clear("1VJ8YKwPgI75YzDPI36L7KNHmLjsoBfOzowItWZWnNhQ", sheet = 1, reformat = FALSE)
    sheet_append("1VJ8YKwPgI75YzDPI36L7KNHmLjsoBfOzowItWZWnNhQ",  weekly_report_append , sheet = 1)
    
    #print("We have added the valid rows of the Weekly Report to the Base Report.\n")
    #print("Removed duplicates and sorted in Desc. Order. by Manufacture Date.\n")
    
    if(length(attention_found) > 0) {
      print("ATTENTION!!!")
      print("We founds some rows that need Attention from the Weekly Report!\n")
      print("Please see google sheet titled 'attention!_report', and fix the blank cells!\n")
      print("Run this program again and we'll get those rows added to the Base Report!\n")
      sheet_append("1VJ8YKwPgI75YzDPI36L7KNHmLjsoBfOzowItWZWnNhQ", attention_found , sheet = 3)
    }
  }
  
  print("Thank you, have a nice day!\n")
  Sys.sleep(2)
  print("ENDING PROGRAM...\n")
  Sys.sleep(5)
  print("END")
}

producer_fill <- function(T) {
  val <- unname(T[9])
  if(is.na(val)) {
    return("Forever 46, LLC - 00000057DCHF00477864")
  } 
  return(val)
}
  

set_up_weekly_report <- function(T) {
  T = T %>% 
    rename(
      "Brand" = colnames(T)[1],
      "Product" = colnames(T)[2],
      "Strain" = colnames(T)[3], 
      "Batch" = colnames(T)[4],
      "Package ID" = colnames(T)[5],
      Date.of.Harvest = colnames(T)[6], 
      Laboratory.Testing.Results = colnames(T)[7], 
      "Distribution Chain" = colnames(T)[8], 
      "Cultivated By" = colnames(T)[9],
      Date.of.Manufacture = colnames(T)[10]
    )
  
  T$Inventory.Producer.Name <- apply(T, 1, FUN = function(T) producer_fill(T))
  
  T = T %>%
    mutate("Point of Intended Sale" = "https://ilava.com/point-of-intended-sale/")
  
  T = T %>%
    mutate("Extraction Method" = "https://ilava.com/extraction-methods/")
  
  T <- select(T, 
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
  
  T$Date.of.Manufacture <- format(as.Date(T$Date.of.Manufacture), "%m/%d/%Y")
  return (T)
}


main()