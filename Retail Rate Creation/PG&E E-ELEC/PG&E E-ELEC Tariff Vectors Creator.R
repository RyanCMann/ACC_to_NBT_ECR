#### Script Description Header ####

# File Name: PG&E E-ELEC Tariff Vectors Creator.R
# File Location: "ACC_to_NBT_ECR/Retail Rate Creation/PG&E E-ELEC"
# Project: Avoided Cost Calculator to Net Billing Tariff Export Compensation Rate
# Description: Converts PG&E E-ELEC rate into time-series vectors.

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

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Ryan's Stuff/2023/ACC_to_NBT_ECR/Retail Rate Creation/PG&E E-ELEC")
Code_WD <- getwd()
Clean_Rate_Data_WD <- file.path(Code_WD, Data_Year)

#### Load  & Clean Load Interval Data from Excel File ####

Start_Date_Time <- as.POSIXct(paste0(Data_Year, "-01-01 00:00:00"), tz = "Etc/GMT+8")
End_Date_Time <- as.POSIXct(paste0(Data_Year, "-12-31 23:55:00"), tz = "Etc/GMT+8")

PGE_E_ELEC_Cost_Vectors <- data.frame(Date_Time = seq.POSIXt(Start_Date_Time, 
                                                                End_Date_Time, 
                                                                by = paste(Data_Timestep_Length, "min")))


#### Create Vectors for Energy and Demand Charges ####

# Values are for PG&E E-ELEC, in US $.

Summer_Peak_Rate = 0.52979
Summer_Partial_Peak_Rate = 0.36791
Summer_Off_Peak_Rate = 0.31123

Winter_Peak_Rate = 0.29828
Winter_Partial_Peak_Rate = 0.27619
Winter_Off_Peak_Rate = 0.26233

# Bills are assumed to be monthly, and fall at the end of every month, 
# so there are no billing periods that fall into both winter and summer periods.

# Create Summer vs. Winter Binary and Decimal Hour Variables
# Dates between June 1 and September 30 are considered "Summer"

PGE_E_ELEC_Cost_Vectors <- PGE_E_ELEC_Cost_Vectors %>%
  mutate(Month = month(Date_Time)) %>%
  mutate(Day = day(Date_Time)) %>%
  mutate(Season = ifelse(Month %in% c(6:9), "Summer", "Winter")) %>% 
  mutate(Hour_Beginning = hour(Date_Time) + minute(Date_Time)/60) # ex. 8:30 am = 8.5

# PG&E Holiday List 2023: https://www.pge.com/tariffs/toudates.shtml
PGE_Holidays_2023 <- read.csv(file.path(Code_WD,
                                   "PG&E_Holidays_2023.csv")) %>%
  pivot_longer(New.Years.Day:Christmas, "Holiday_Name", values_to = "Holiday_Date") %>%
  mutate(Holiday_Date = as.Date(Holiday_Date, tz = "America/Los_Angeles")) %>%
  mutate(Holiday_Flag = TRUE) %>%
  select(-Holiday_Name)

PGE_E_ELEC_Cost_Vectors <- PGE_E_ELEC_Cost_Vectors %>%
  mutate(Date = as.Date(Date_Time, tz = "America/Los_Angeles")) %>%
  mutate(Day_of_Week = weekdays(Date_Time)) %>%
  mutate(Weekend_Flag = Day_of_Week %in% c("Saturday", "Sunday")) %>%
  left_join(PGE_Holidays_2023, by = c("Date" = "Holiday_Date")) %>% 
  mutate(Holiday_Flag = replace_na(Holiday_Flag, FALSE)) %>%
  mutate(DayType = ifelse(Weekend_Flag | Holiday_Flag, "Weekend & Holiday", "Weekday")) %>% 
  select(-Date, -Day_of_Week, -Weekend_Flag, -Holiday_Flag)


# Summer Peak
# Summer Peak = June-September, 4:00 pm-9:00 pm

PGE_E_ELEC_Cost_Vectors <- PGE_E_ELEC_Cost_Vectors %>%
  mutate(Summer_Peak_Binary = ifelse(Season == "Summer" & 
                                     Hour_Beginning >= (12+4) & 
                                       Hour_Beginning < (12+9), 
                                     1, 0))

# Summer Partial-Peak
# Summer Partial Peak = 3:00 pm to 4:00 pm, 9:00 pm to 12:00 am

PGE_E_ELEC_Cost_Vectors <- PGE_E_ELEC_Cost_Vectors %>%
  mutate(Summer_Partial_Peak_Binary = ifelse(Season == "Summer" & 
                                               ((Hour_Beginning >= (12+3) & 
                                                   Hour_Beginning < (12+4)) |
                                                  (Hour_Beginning >= (12 + 9) & 
                                                     Hour_Beginning < (12 + 12))), 
                                             1, 0))

# Summer Off-Peak
# Summer Off Peak = 12:00 am to 3:00 pm
# In other words, if it's summer and not peak or partial peak, it's off-peak.

PGE_E_ELEC_Cost_Vectors <- PGE_E_ELEC_Cost_Vectors %>%
  mutate(Summer_Off_Peak_Binary = ifelse(Season == "Summer" &
                                           Summer_Peak_Binary == 0 &
                                           Summer_Partial_Peak_Binary == 0, 
                                         1, 0))

# Winter Peak
# Winter Peak = October-May, 4:00 pm-9:00 pm

PGE_E_ELEC_Cost_Vectors <- PGE_E_ELEC_Cost_Vectors %>%
  mutate(Winter_Peak_Binary = ifelse(Season == "Winter" & 
                                       Hour_Beginning >= (12+4) & 
                                       Hour_Beginning < (12+9), 
                                     1, 0))

# Winter Partial-Peak
# Winter Partial Peak = 3:00 pm to 4:00 pm, 9:00 pm to 12:00 am

PGE_E_ELEC_Cost_Vectors <- PGE_E_ELEC_Cost_Vectors %>%
  mutate(Winter_Partial_Peak_Binary = ifelse(Season == "Winter" & 
                                               ((Hour_Beginning >= (12+3) & 
                                                   Hour_Beginning < (12+4)) |
                                                  (Hour_Beginning >= (12 + 9) & 
                                                     Hour_Beginning < (12 + 12))), 
                                             1, 0))

# Winter Off-Peak
# Winter Off Peak = 12:00 am to 3:00 pm
# In other words, if it's winter and not peak or partial peak, it's off-peak.

PGE_E_ELEC_Cost_Vectors <- PGE_E_ELEC_Cost_Vectors %>%
  mutate(Winter_Off_Peak_Binary = ifelse(Season == "Winter" &
                                           Winter_Peak_Binary == 0 &
                                           Winter_Partial_Peak_Binary == 0, 
                                         1, 0))


# Check that all timesteps are accounted for, 
# and that no timestep has a value of 1 in multiple columns

PGE_E_ELEC_Cost_Vector_Check <- PGE_E_ELEC_Cost_Vectors %>%
  mutate(Check_Sum = Summer_Peak_Binary + Summer_Partial_Peak_Binary + Summer_Off_Peak_Binary + 
           Winter_Peak_Binary + Winter_Partial_Peak_Binary + Winter_Off_Peak_Binary) %>%
  filter(Check_Sum != 1)
# This dataframe should be empty (0 observations).

rm(PGE_E_ELEC_Cost_Vector_Check)


#### Energy Rates ####

PGE_E_ELEC_Cost_Vectors <- PGE_E_ELEC_Cost_Vectors %>%
  mutate(Retail_Rate = (Summer_Peak_Binary * Summer_Peak_Rate) + 
           (Summer_Partial_Peak_Binary * Summer_Partial_Peak_Rate) +
           (Summer_Off_Peak_Binary * Summer_Off_Peak_Rate) +
           (Winter_Peak_Binary * Winter_Peak_Rate) + 
           (Winter_Partial_Peak_Binary * Winter_Partial_Peak_Rate) +
           (Winter_Off_Peak_Binary * Winter_Off_Peak_Rate))


#### Export Rate Data as CSV Outputs ####

saveRDS(PGE_E_ELEC_Cost_Vectors, 
        file.path(Clean_Rate_Data_WD, 
                  paste0(Data_Timestep_Length, "-Minute Data"), "Dataframe Format", 
                  paste0(Data_Year, "_PGE_E_ELEC_Cost_Dataframe.rds")))

write.csv(PGE_E_ELEC_Cost_Vectors, 
          file.path(Clean_Rate_Data_WD, 
                    paste0(Data_Timestep_Length, "-Minute Data"), "Dataframe Format", 
                    paste0(Data_Year, "_PGE_E_ELEC_Cost_Dataframe.csv")),
          row.names = FALSE)
