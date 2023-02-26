#### Script Description Header ####

# File Name: SDG&E EV-TOU-5 Tariff Vectors Creator.R
# File Location: "ACC_to_NBT_ECR/Retail Rate Creation/SDG&E EV-TOU-5"
# Project: Avoided Cost Calculator to Net Billing Tariff Export Compensation Rate
# Description: Converts SDG&E EV-TOU-5 rate into time-series vectors.

#### User Inputs ####

# This script generates a time-series vector for the user's choice of year.
# This value must be input by the user to match the desired year to be modeled.

Data_Year <- 2023

Data_Timestep_Length = 60 # Timestep length, in minutes


#### Load Packages

# When you first begin using R, packages must be installed.
# This process only needs to be performed once.

# install.packages("tidyverse")
# install.packages("readr")
# install.packages("lubridate")

# Packages must be loaded to the library every time you open up RStudio.

library(tidyverse)
library(readr)
library(lubridate)

# Disable Scientific Notation

options(scipen = 999)

#### Define Working Directories ####

# Set working directory to project directory or source file location manually
# using Session -> Set Working Directory before running these.

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Ryan's Stuff/2023/ACC_to_NBT_ECR/Retail Rate Creation/SDG&E EV-TOU-5")
Code_WD <- getwd()
Clean_Rate_Data_WD <- file.path(Code_WD, Data_Year)

#### Load  & Clean Load Interval Data from Excel File ####

Start_Date_Time <- as.POSIXct(paste0(Data_Year, "-01-01 00:00:00"), tz = "Etc/GMT+8")
End_Date_Time <- as.POSIXct(paste0(Data_Year, "-12-31 23:55:00"), tz = "Etc/GMT+8")

SDGE_EV_TOU_5_Cost_Vectors <- data.frame(Date_Time = seq.POSIXt(Start_Date_Time, 
                                                                End_Date_Time, 
                                                                by = paste(Data_Timestep_Length, "min")))


#### Create Vectors for Energy and Demand Charges ####

# Values are for SDG&E EV-TOU-5, in US $.

Summer_On_Peak_Rate = 0.81629
Summer_Off_Peak_Rate = 0.48129
Summer_Super_Off_Peak_Rate = 0.15351

Winter_On_Peak_Rate = 0.51149
Winter_Off_Peak_Rate = 0.44775
Winter_Super_Off_Peak_Rate = 0.14520

# Bills are assumed to be monthly, and fall at the end of every month, 
# so there are no billing periods that fall into both winter and summer periods.

# Create Summer vs. Winter Binary and Decimal Hour Variables
# Dates between June 1 and October 31 are considered "Summer"

SDGE_EV_TOU_5_Cost_Vectors <- SDGE_EV_TOU_5_Cost_Vectors %>%
  mutate(Month = month(Date_Time)) %>%
  mutate(Day = day(Date_Time)) %>%
  mutate(Season = ifelse(Month %in% c(6:10), "Summer", "Winter")) %>% 
  mutate(Hour_Beginning = hour(Date_Time) + minute(Date_Time)/60) # ex. 8:30 am = 8.5

# SDG&E Holiday List 2023: https://www.sdge.com/sites/default/files/2023%20Billing%20Cycle%20Schedule_1.pdf
SDGE_Holidays_2023 <- read.csv(file.path(Code_WD,
                                         "SDG&E_Holidays_2023.csv")) %>%
  pivot_longer(New.Years.Day:Christmas, "Holiday_Name", values_to = "Holiday_Date") %>%
  mutate(Holiday_Date = as.Date(Holiday_Date, tz = "America/Los_Angeles")) %>%
  mutate(Holiday_Flag = TRUE) %>%
  select(-Holiday_Name)

SDGE_EV_TOU_5_Cost_Vectors <- SDGE_EV_TOU_5_Cost_Vectors %>%
  mutate(Date = as.Date(Date_Time, tz = "America/Los_Angeles")) %>%
  mutate(Day_of_Week = weekdays(Date_Time)) %>%
  mutate(Weekend_Flag = Day_of_Week %in% c("Saturday", "Sunday")) %>%
  left_join(SDGE_Holidays_2023, by = c("Date" = "Holiday_Date")) %>% 
  mutate(Holiday_Flag = replace_na(Holiday_Flag, FALSE)) %>%
  mutate(DayType = ifelse(Weekend_Flag | Holiday_Flag, "Weekend & Holiday", "Weekday")) %>% 
  select(-Date, -Day_of_Week, -Weekend_Flag, -Holiday_Flag)

rm(SDGE_Holidays_2023)


# Summer On-Peak
# Summer On-Peak = June-October, 4:00 pm-9:00 pm

SDGE_EV_TOU_5_Cost_Vectors <- SDGE_EV_TOU_5_Cost_Vectors %>%
  mutate(Summer_On_Peak_Binary = ifelse(Season == "Summer" & 
                                          Hour_Beginning >= (12+4) & 
                                          Hour_Beginning < (12+9), 
                                        1, 0))

# Summer Super Off-Peak
# Summer Super Off-Peak Weekdays = 12:00 am to 6:00 am
# Summer Super Off-Peak Weekends/Holidays = 12:00 am to 2:00 pm

SDGE_EV_TOU_5_Cost_Vectors <- SDGE_EV_TOU_5_Cost_Vectors %>%
  mutate(Summer_Super_Off_Peak_Binary = ifelse(Season == "Summer" & 
                                                 ((DayType == "Weekday" &
                                                    Hour_Beginning < 6) |
                                                 (DayType == "Weekend & Holiday" &
                                                    Hour_Beginning < (12 + 2))), 
                                               1, 0))

# Summer Off-Peak
# Summer Off-Peak Weekdays = 6:00 am to 4:00 pm, 9:00 pm to 12:00 am
# Summer Off-Peak Weekends/Holidays = 2:00 pm to 4:00 pm, 9:00 pm to 12:00 am
# In other words, if it's summer and not on-peak or super-off-peak, it's off-peak.

SDGE_EV_TOU_5_Cost_Vectors <- SDGE_EV_TOU_5_Cost_Vectors %>%
  mutate(Summer_Off_Peak_Binary = ifelse(Season == "Summer" &
                                           Summer_On_Peak_Binary == 0 &
                                           Summer_Super_Off_Peak_Binary == 0, 
                                         1, 0))

# Winter On-Peak
# Winter On-Peak = November-May, 4:00 pm-9:00 pm

SDGE_EV_TOU_5_Cost_Vectors <- SDGE_EV_TOU_5_Cost_Vectors %>%
  mutate(Winter_On_Peak_Binary = ifelse(Season == "Winter" & 
                                          Hour_Beginning >= (12+4) & 
                                          Hour_Beginning < (12+9), 
                                        1, 0))

# Winter Super Off-Peak
# Winter Super Off-Peak Weekdays = 12:00 am to 6:00 am, 10:00 am to 2:00 pm in March and April)
# Winter Super Off-Peak Weekends/Holidays = 12:00 am to 2:00 pm

SDGE_EV_TOU_5_Cost_Vectors <- SDGE_EV_TOU_5_Cost_Vectors %>%
  mutate(Winter_Super_Off_Peak_Binary = ifelse(Season == "Winter" & 
                                                 ((DayType == "Weekday" &
                                                    Hour_Beginning < 6) |
                                                 (DayType == "Weekday" &
                                                    Month %in% c(3,4) &
                                                    Hour_Beginning >= 10 &
                                                    Hour_Beginning < (12+2)) |
                                                 (DayType == "Weekend & Holiday" &
                                                    Hour_Beginning < (12 + 2))), 
                                               1, 0))

# Winter Off-Peak
# Winter Off-Peak Weekdays = 6:00 am to 4:00 pm (excluding 10:00 am to 2:00 pm in March and April), 9:00 pm to 12:00 am
# Winter Off-Peak Weekends/Holidays = 2:00 pm to 4:00 pm, 9:00 pm to 12:00 am
# In other words, if it's winter and not on-peak or super-off-peak, it's off-peak.

SDGE_EV_TOU_5_Cost_Vectors <- SDGE_EV_TOU_5_Cost_Vectors %>%
  mutate(Winter_Off_Peak_Binary = ifelse(Season == "Winter" &
                                           Winter_On_Peak_Binary == 0 &
                                           Winter_Super_Off_Peak_Binary == 0, 
                                         1, 0))


# Check that all timesteps are accounted for, 
# and that no timestep has a value of 1 in multiple columns

SDGE_EV_TOU_5_Cost_Vector_Check <- SDGE_EV_TOU_5_Cost_Vectors %>%
  mutate(Check_Sum = Summer_On_Peak_Binary + Summer_Off_Peak_Binary + Summer_Super_Off_Peak_Binary + 
           Winter_On_Peak_Binary + Winter_Off_Peak_Binary + Winter_Super_Off_Peak_Binary) %>%
  filter(Check_Sum != 1)
# This dataframe should be empty (0 observations).

rm(SDGE_EV_TOU_5_Cost_Vector_Check)


#### Energy Rates ####

SDGE_EV_TOU_5_Cost_Vectors <- SDGE_EV_TOU_5_Cost_Vectors %>%
  mutate(Retail_Rate = (Summer_On_Peak_Binary * Summer_On_Peak_Rate) + 
           (Summer_Off_Peak_Binary * Summer_Off_Peak_Rate) +
           (Summer_Super_Off_Peak_Binary * Summer_Super_Off_Peak_Rate) +
           (Winter_On_Peak_Binary * Winter_On_Peak_Rate) + 
           (Winter_Off_Peak_Binary * Winter_Off_Peak_Rate) +
           (Winter_Super_Off_Peak_Binary * Winter_Super_Off_Peak_Rate))


#### Export Rate Data as CSV Outputs ####

saveRDS(SDGE_EV_TOU_5_Cost_Vectors, 
        file.path(Clean_Rate_Data_WD, 
                  paste0(Data_Timestep_Length, "-Minute Data"), "Dataframe Format", 
                  paste0(Data_Year, "_SDGE_EV_TOU_5_Cost_Dataframe.rds")))

write.csv(SDGE_EV_TOU_5_Cost_Vectors, 
          file.path(Clean_Rate_Data_WD, 
                    paste0(Data_Timestep_Length, "-Minute Data"), "Dataframe Format", 
                    paste0(Data_Year, "_SDGE_EV_TOU_5_Cost_Dataframe.csv")),
          row.names = FALSE)
