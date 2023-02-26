#### Script Description Header ####

# File Name: SCE TOU-D-PRIME Tariff Vectors Creator.R
# File Location: "ACC_to_NBT_ECR/Retail Rate Creation/SCE TOU-D-PRIME"
# Project: Avoided Cost Calculator to Net Billing Tariff Export Compensation Rate
# Description: Converts SCE TOU-D-PRIME rate into time-series vectors.

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

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Ryan's Stuff/2023/ACC_to_NBT_ECR/Retail Rate Creation/SCE TOU-D-PRIME")
Code_WD <- getwd()
Clean_Rate_Data_WD <- file.path(Code_WD, Data_Year)


#### Load  & Clean Load Interval Data from Excel File ####

Start_Date_Time <- as.POSIXct(paste0(Data_Year, "-01-01 00:00:00"), tz = "Etc/GMT+8")
End_Date_Time <- as.POSIXct(paste0(Data_Year, "-12-31 23:55:00"), tz = "Etc/GMT+8")

SCE_TOU_D_PRIME_Cost_Vectors <- data.frame(Date_Time = seq.POSIXt(Start_Date_Time, 
                                                                  End_Date_Time, 
                                                                  by = paste(Data_Timestep_Length, "min")))


#### Create Vectors for Energy and Demand Charges ####

# Values are for SCE TOU-D-PRIME, in US $.

Summer_On_Peak_Rate = 0.22296 + 0.40014 + 0.00090
Summer_Mid_Peak_Rate = 0.22296 + 0.14240 + 0.00090
Summer_Off_Peak_Rate = 0.14797 + 0.09507 + 0.00090

Winter_Mid_Peak_Rate = 0.22859 + 0.33707 + 0.00090
Winter_Off_Peak_Rate = 0.14152 + 0.08074 + 0.00090
Winter_Super_Off_Peak_Rate = 0.14152 + 0.08074 + 0.00090

# Bills are assumed to be monthly, and fall at the end of every month, 
# so there are no billing periods that fall into both winter and summer periods.

# Create Summer vs. Winter Binary and Decimal Hour Variables
# Dates between June 1 and September 30 are considered "Summer"

SCE_TOU_D_PRIME_Cost_Vectors <- SCE_TOU_D_PRIME_Cost_Vectors %>%
  mutate(Month = month(Date_Time)) %>%
  mutate(Day = day(Date_Time)) %>%
  mutate(Season = ifelse(Month %in% c(6:9), "Summer", "Winter")) %>% 
  mutate(Hour_Beginning = hour(Date_Time) + minute(Date_Time)/60) # ex. 8:30 am = 8.5

# SCE Holiday List 2023:
# "Holidays are
# New Year’s Day (January 1),
# Presidents’ Day (third Monday in February),
# Memorial Day (last Monday in May),
# Independence Day (July 4),
# Labor Day (first Monday in September),
# Veterans Day (November 11),
# Thanksgiving Day (fourth Thursday in November),
# and Christmas (December 25).
# When any holiday listed above falls on Sunday,
# the following Monday will be recognized as a holiday.
# No change will be made for holidays falling on Saturday."
SCE_Holidays_2023 <- read.csv(file.path(Code_WD,
                                        "SCE_Holidays_2023.csv")) %>%
  pivot_longer(New.Years.Day:Christmas, "Holiday_Name", values_to = "Holiday_Date") %>%
  mutate(Holiday_Date = as.Date(Holiday_Date, tz = "America/Los_Angeles")) %>%
  mutate(Holiday_Flag = TRUE) %>%
  select(-Holiday_Name)

SCE_TOU_D_PRIME_Cost_Vectors <- SCE_TOU_D_PRIME_Cost_Vectors %>%
  mutate(Date = as.Date(Date_Time, tz = "America/Los_Angeles")) %>%
  mutate(Day_of_Week = weekdays(Date_Time)) %>%
  mutate(Weekend_Flag = Day_of_Week %in% c("Saturday", "Sunday")) %>%
  left_join(SCE_Holidays_2023, by = c("Date" = "Holiday_Date")) %>% 
  mutate(Holiday_Flag = replace_na(Holiday_Flag, FALSE)) %>%
  mutate(DayType = ifelse(Weekend_Flag | Holiday_Flag, "Weekend & Holiday", "Weekday")) %>% 
  select(-Date, -Day_of_Week, -Weekend_Flag, -Holiday_Flag)

rm(SCE_Holidays_2023)


# Summer On-Peak
# Summer On-Peak = June-September, 4:00 pm-9:00 pm on Weekdays

SCE_TOU_D_PRIME_Cost_Vectors <- SCE_TOU_D_PRIME_Cost_Vectors %>%
  mutate(Summer_On_Peak_Binary = ifelse(Season == "Summer" &
                                          DayType == "Weekday" &
                                          Hour_Beginning >= (12+4) & 
                                          Hour_Beginning < (12+9), 
                                        1, 0))

# Summer Mid-Peak
# Summer Mid-Peak = 4:00 pm-9:00 pm on Weekends & Holidays

SCE_TOU_D_PRIME_Cost_Vectors <- SCE_TOU_D_PRIME_Cost_Vectors %>%
  mutate(Summer_Mid_Peak_Binary = ifelse(Season == "Summer" &
                                           DayType == "Weekend & Holiday" &
                                           Hour_Beginning >= (12+4) & 
                                           Hour_Beginning < (12+9), 
                                         1, 0))

# Summer Off-Peak
# Summer Off-Peak = 9:00 pm to 4:00 pm
# In other words, if it's summer and not on-peak or mid-peak, it's off-peak.

SCE_TOU_D_PRIME_Cost_Vectors <- SCE_TOU_D_PRIME_Cost_Vectors %>%
  mutate(Summer_Off_Peak_Binary = ifelse(Season == "Summer" &
                                           Summer_On_Peak_Binary == 0 &
                                           Summer_Mid_Peak_Binary == 0, 
                                         1, 0))

# Winter Mid-Peak
# Winter Mid-Peak = October-May, 4:00 pm-9:00 pm

SCE_TOU_D_PRIME_Cost_Vectors <- SCE_TOU_D_PRIME_Cost_Vectors %>%
  mutate(Winter_Mid_Peak_Binary = ifelse(Season == "Winter" & 
                                           Hour_Beginning >= (12+4) & 
                                           Hour_Beginning < (12+9), 
                                         1, 0))

# Winter Off-Peak
# Winter Off-Peak = 9:00 pm to 8:00 am

SCE_TOU_D_PRIME_Cost_Vectors <- SCE_TOU_D_PRIME_Cost_Vectors %>%
  mutate(Winter_Off_Peak_Binary = ifelse(Season == "Winter" &
                                           (Hour_Beginning >= (12+9) |
                                              Hour_Beginning < 8), 
                                         1, 0))


# Winter Super-Off-Peak
# Winter Super-Off-Peak = 8:00 am to 4:00 pm

SCE_TOU_D_PRIME_Cost_Vectors <- SCE_TOU_D_PRIME_Cost_Vectors %>%
  mutate(Winter_Super_Off_Peak_Binary = ifelse(Season == "Winter" & 
                                                 Hour_Beginning >= 8 &
                                                 Hour_Beginning < (12 + 4), 
                                               1, 0))

# Check that all timesteps are accounted for, 
# and that no timestep has a value of 1 in multiple columns

SCE_TOU_D_PRIME_Cost_Vector_Check <- SCE_TOU_D_PRIME_Cost_Vectors %>%
  mutate(Check_Sum = Summer_On_Peak_Binary + Summer_Mid_Peak_Binary + Summer_Off_Peak_Binary + 
           Winter_Mid_Peak_Binary + Winter_Off_Peak_Binary + Winter_Super_Off_Peak_Binary) %>%
  filter(Check_Sum != 1)
# This dataframe should be empty (0 observations).

rm(SCE_TOU_D_PRIME_Cost_Vector_Check)


#### Energy Rates ####

SCE_TOU_D_PRIME_Cost_Vectors <- SCE_TOU_D_PRIME_Cost_Vectors %>%
  mutate(Retail_Rate = (Summer_On_Peak_Binary * Summer_On_Peak_Rate) + 
           (Summer_Mid_Peak_Binary * Summer_Mid_Peak_Rate) +
           (Summer_Off_Peak_Binary * Summer_Off_Peak_Rate) +
           (Winter_Mid_Peak_Binary * Winter_Mid_Peak_Rate) + 
           (Winter_Off_Peak_Binary * Winter_Off_Peak_Rate) +
           (Winter_Super_Off_Peak_Binary * Winter_Super_Off_Peak_Rate))


#### Export Rate Data as CSV Outputs ####

saveRDS(SCE_TOU_D_PRIME_Cost_Vectors, 
        file.path(Clean_Rate_Data_WD, 
                  paste0(Data_Timestep_Length, "-Minute Data"), "Dataframe Format", 
                  paste0(Data_Year, "_SCE_TOU_D_PRIME_Cost_Dataframe.rds")))

write.csv(SCE_TOU_D_PRIME_Cost_Vectors, 
          file.path(Clean_Rate_Data_WD, 
                    paste0(Data_Timestep_Length, "-Minute Data"), "Dataframe Format", 
                    paste0(Data_Year, "_SCE_TOU_D_PRIME_Cost_Dataframe.csv")),
          row.names = FALSE)
