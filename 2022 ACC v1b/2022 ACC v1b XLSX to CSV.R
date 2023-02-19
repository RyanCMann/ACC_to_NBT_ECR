#### Script Description Header ####

# File Name: 2022 ACC v1b XLSX to CSV.R

# File Location: "~/Desktop/Avoided Cost Calculator to Net Billing Tariff Export Compensation Rate/2022 ACC v1b"

# Project: Avoided Cost Calculator to Net Billing Tariff Export Compensation Rate

# Description: Cleans and converts 2022 ACC v1b macro XLSX output
# ("$/MWh export of selected components to Excel") to smaller CSV outputs.

# These XLSX files are 48 MB, 41 MB, and 29 MB for PG&E, SCE, and SDG&E respectively.

# This exceeds GitHub maximum file upload size:
# https://docs.github.com/en/repositories/working-with-files/managing-large-files/about-large-files-on-github


#### Load Packages
library(tidyverse)
library(lubridate)
library(openxlsx)

# Disable Scientific Notation
options(scipen = 999)

# Set Working Directories
setwd("~/Desktop/Avoided Cost Calculator to Net Billing Tariff Export Compensation Rate/2022 ACC v1b")
Code_WD <- getwd()


# Iterate through PG&E, SCE, and SDG&E ACC Outputs
Utility_Names <- c("PG&E", "SCE", "SDG&E")

for(Utility_Name_Iter in Utility_Names){
  
  ACC_Output_Filename <- paste0(Utility_Name_Iter, " All.xlsx")
  
  ACC_Output_SheetNames <- getSheetNames(ACC_Output_Filename)
  
  ACC_Sheet_CSV_Filepath <- file.path(Code_WD, 
                                      paste0("2022 ACC v1b Outputs - ", 
                                             Utility_Name_Iter))
  
  for(ACC_Output_SheetName in ACC_Output_SheetNames){
    
    # Load spreadsheet and clean up datetime column.
    # Timestamps are in Pacific Standard Time,
    # and include Leap Day 2020 but not Dec. 31, so as to have 365 days.
    ACC_Output_Sheet <- read.xlsx(ACC_Output_Filename, 
                                  sheet = ACC_Output_SheetName, 
                                  startRow = 2, 
                                  rows = 2:8762) %>%
      rename(Date_Time = X1) %>%
      select(-`2022`) %>% # 2022 ACC values not used as Export Compensation Rate values.
      mutate(Date_Time = seq.POSIXt(as.POSIXct("2020-01-01 00:00", tz = "Etc/GMT+8"),
                                    as.POSIXct("2020-12-30 23:00", tz = "Etc/GMT+8"),
                                    by = "1 hour")) 
    
    # Label "CZ" files as corresponding to Distribution Capacity avoided cost component.
    if(grepl("CZ", ACC_Output_SheetName)){ # "CZ" is included in spreadsheet tab name.
      ACC_Sheet_CSV_Filename <- paste0(Utility_Name_Iter, " DCap ", 
                                       ACC_Output_SheetName, ".csv")
    }else{ # "CZ" is not included in spreadsheet tab name.
      ACC_Sheet_CSV_Filename <- paste0(Utility_Name_Iter, " ", 
                                       ACC_Output_SheetName, ".csv")
    }
    
    write.csv(ACC_Output_Sheet,
              file.path(ACC_Sheet_CSV_Filepath, ACC_Sheet_CSV_Filename),
              row.names = F)
    
  }
  
}
