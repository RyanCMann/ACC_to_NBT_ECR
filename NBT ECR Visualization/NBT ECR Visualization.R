#### Script Description Header ####

# File Name: NBT ECR Visualization.R
# File Location: "ACC_to_NBT_ECR/Retail Rate Creation/PG&E E-ELEC"
# Project: Avoided Cost Calculator to Net Billing Tariff Export Compensation Rate
# Description: Creates Export Compensation Rate visualization plots for Net Billing Tariff

#### Load Packages ####
library(tidyverse)
library(lubridate)
library(scales)

# Disable Scientific Notation.
options(scipen = 999)

# Turn off a confusing message when summarizing data.
options(dplyr.summarise.inform = FALSE)

# Set Working Directories
# Note: Set working directory to source file location before running script.
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Ryan's Stuff/2023/ACC_to_NBT_ECR/NBT ECR Visualization")
Code_WD <- getwd()

setwd("/Users/ryanmann/Library/Mobile Documents/com~apple~CloudDocs/Ryan's Stuff/2023/ACC_to_NBT_ECR/Retail Rate Creation")
Retail_Rate_WD <- getwd()


#### User Inputs for Single Plot ####

# ECR_Customer_Segment <- "Residential General Market" # "Residential General Market", "Residential Low-Income", "Residential New Home/Change of Party", "Non-Residential"
# Retail_Rate_Customer_Segment <- "No Discount" # "No Discount", "CARE", "FERA"
# Utility_Name <- "PG&E" # "PG&E", "SCE", "SDG&E"
# Rate_Season <- "Summer" # "Summer", "Winter", "Spring" (Note: "Spring" only applies to SDG&E)
# Day_Type <- "Weekday" # "Weekday", "Weekend & Holiday"
# ECR_Year <- 2025 # Simulation Year in Avoided Cost Calculator (not vintage of ACC spreadsheet) 2023 . . . 2052
# IX_App_Year <- 2025 # Final Interconnection Application Year 2023 . . . 2026


ECR_Plot <- function(ECR_Customer_Segment, Retail_Rate_Customer_Segment, Utility_Name, Rate_Season, Day_Type, ECR_Year, IX_App_Year){
  
  #### Input Mapping ####
  
  if(IX_App_Year %in% c(2023, 2024)){
    ACC_Vintage = 2022
  }else if(IX_App_Year %in% c(2025, 2026)){
    ACC_Vintage = 2024
  }
  
  if(Utility_Name == "PG&E"){
    NBT_ECR_File <- paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                           "Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20Calculation/", ACC_Vintage, "%20ACC%20NBT%20ECRs/",
                           "PG%26E%20Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20-%20Simple%20Average%20DCap%20-%20Bundled.csv")
    Winter_Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Oct", "Nov", "Dec")
    Spring_Months <- NA
    Summer_Months <- c("Jun", "Jul", "Aug", "Sep")
  } else if(Utility_Name == "SCE"){
    NBT_ECR_File <- paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                           "Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20Calculation/", ACC_Vintage, "%20ACC%20NBT%20ECRs/",
                           "SCE%20Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20-%20Simple%20Average%20DCap%20-%20Bundled.csv")
    Winter_Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Oct", "Nov", "Dec")
    Spring_Months <- NA
    Summer_Months = c("Jun", "Jul", "Aug", "Sep")
  } else if(Utility_Name == "SDG&E"){
    NBT_ECR_File <- paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                           "Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20Calculation/", ACC_Vintage, "%20ACC%20NBT%20ECRs/",
                           "SDG%26E%20Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20-%20Simple%20Average%20DCap%20-%20Bundled.csv")
    Winter_Months <- c("Jan", "Feb", "May", "Nov", "Dec")
    Spring_Months <- c("Mar", "Apr")
    Summer_Months <- c("Jun", "Jul", "Aug", "Sep", "Oct")
  }
  
  
  #### Load and Process ACC Plus Adder Data ####
  
  ACC_Plus_Adders <- read.csv(file.path(Code_WD, "ACC Plus Adders by Year.csv")) %>%
    gather(key = "IX.App.Year", value = "Adder", X2023:X2030) %>%
    mutate(IX.App.Year = gsub("X", "", IX.App.Year)) %>%
    filter(Utility_Name == Utility,
           Customer.Segment == ECR_Customer_Segment,
           IX.App.Year == as.character(IX_App_Year))
  
  ACC_Plus_Adder <- ACC_Plus_Adders$Adder
  
  rm(ACC_Plus_Adders)
  
  
  #### Load Export Compensation Rate Data, Set Y-Axis Bounds for Plot ####
  
  # Combine ACC with ACC Plus Adder to get final Export Compensation Rate.
  # This ECR is in download-ready MIDAS format with ACC Plus Adder.
  Export_Compensation_Rates <- read.csv(NBT_ECR_File) %>%
    mutate(Value = Value + ACC_Plus_Adder)
  
  # Get Month from DateStart for Season filter.
  Export_Compensation_Rates <- Export_Compensation_Rates %>%
    mutate(DateStart = as.Date(DateStart, tz = "America/Los_Angeles")) %>%
    mutate(Month = month(DateStart)) %>%
    mutate(Month = factor(month.abb[Month], levels = month.abb)) %>%
    mutate(Rate = Value)
  
  # Filter to relevant Rate Season.
  if(Utility_Name != "SDG&E" && Rate_Season == "Spring"){
    stop("Selected spring rate season for a utility that does not have spring-specific rates.")
  }
  
  if(Rate_Season == "Winter"){
    Export_Compensation_Rates <- Export_Compensation_Rates %>%
      filter(Month %in% Winter_Months)
  }else if(Rate_Season == "Spring"){
    Export_Compensation_Rates <- Export_Compensation_Rates %>%
      filter(Month %in% Spring_Months)
  }else if(Rate_Season == "Summer"){
    Export_Compensation_Rates <- Export_Compensation_Rates %>%
      filter(Month %in% Summer_Months)
  }
  
  rm(Winter_Months, Spring_Months, Summer_Months)
  
  # Save maximum ECR value to be used to set plot y-axis upper limit.
  Max_ECR <- max(Export_Compensation_Rates$Rate)
  
  # Filter to ACC Year and Day-type,
  # convert remaining columns to plot-ready format
  Export_Compensation_Rates <- Export_Compensation_Rates %>%
    filter(year(DateStart) == ECR_Year) %>%
    mutate(DayType = ifelse(DayTypeStart == 6 & DayTypeEnd == 8, "Weekend & Holiday", "Weekday")) %>%
    filter(DayType == Day_Type) %>%
    mutate(Hour_Beginning = as.numeric(str_sub(TimeStart, 1, 2))) %>%
    select(Month, DayType, Hour_Beginning, Rate)
  
  rm(ACC_Plus_Adder, NBT_ECR_File)
  
  
  #### Convert Retail Rates to plot-ready format ####
  # Note: only including retail rate comparison for 2023 ACC Year,
  # and for Residential General Market customer segment.
  # Retail rate data is still loaded for ACC Years beyond 2023,
  # because the maximum value is used to set the y-axis upper limit.
  
  # Did not plot retail rates for post-2023 ACC years
  # because future retail rate values are not available.
  # Alternative approach would be to apply a
  # 4 Percent average escalation of residential retail rates
  # https://docs.cpuc.ca.gov/PublishedDocs/Published/G000/M343/K979/343979448.docx
  # (pg. 13)
  # so that post-2023 Export Compensation Rates
  # can be compared to estimated post-2023 retail rate values.
  
  # It's worth noting that there are some customers who are classified as low-income
  # with respect to the Net Billing Tariff ACC Plus Adder,
  # but who do not receive either the CARE or FERA discount on their retail rates.
  # "For purposes of the net billing tariff, 
  # low-income customers are defined as one or more of the following:
  # (i) residential customers enrolled in California Alternate Rates for Energy
  # and the Family Electric Rates Assistance programs; 
  # (ii) resident-owners of single-family homes living in disadvantaged communities 
  # (as defined in Decision (D.) 18-06-027); 
  # and (iii) residential customers who live in California Indian Country 
  # (as defined in D.20-12-003)."
  # (pg. 238 of 2022-12-15 DECISION REVISING NET ENERGY METERING TARIFF AND SUBTARIFFS)
  
  # Did not plot retail rates for Non-Residential customer segment
  # because there are a wide variety of different rate tariffs
  # for non-residential customers.
  
  if(Utility_Name == "PG&E"){
    Retail_Rates <- readRDS(file.path(Retail_Rate_WD,
                                      "PG&E E-ELEC",
                                      "2025",
                                      "60-Minute Data",
                                      "Dataframe Format",
                                      "2025_PGE_E_ELEC_Cost_Dataframe.rds"))
    Retail_Rate_Name <- "E-ELEC"
    
  }else if(Utility_Name == "SCE"){
    
    Retail_Rates <- readRDS(file.path(Retail_Rate_WD,
                                      "SCE TOU-D-PRIME",
                                      "2025",
                                      "60-Minute Data",
                                      "Dataframe Format",
                                      "2025_SCE_TOU_D_PRIME_Cost_Dataframe.rds"))
    Retail_Rate_Name <- "TOU-D-PRIME"
    
  }else if(Utility_Name == "SDG&E"){
    Retail_Rates <- readRDS(file.path(Retail_Rate_WD,
                                      "SDG&E EV-TOU-5",
                                      "2025",
                                      "60-Minute Data",
                                      "Dataframe Format",
                                      "2025_SDGE_EV_TOU_5_Cost_Dataframe.rds"))
    Retail_Rate_Name <- "EV-TOU-5"
    
  }
  
  
  #### Apply Low-Income Discount to Retail Rates ####
  
  # Load Discount Data
  Low_Income_Discounts <- read.csv(file.path(Retail_Rate_WD,
                                             "Low-Income Discounts",
                                             "Low Income Discount Percentages.csv"))
  
  # Get discount rate from the low-income discount data
  discount_data <- Low_Income_Discounts %>%
    filter(Utility == Utility_Name,
           Discount.Program == Retail_Rate_Customer_Segment)
  
  discount_rate <- discount_data$Discount.Rate[1]
  
  # Apply Discount
  Retail_Rates_With_Discount <- Retail_Rates %>%
    mutate(Retail_Rate = Retail_Rate * (1 - discount_rate))
  
  
  # Save maximum retail rate value to be used to set plot y-axis upper limit.
  Retail_Rates_With_Discount <- Retail_Rates_With_Discount %>%
    filter(month.abb[Month] %in% unique(Export_Compensation_Rates$Month)) %>%
    rename(Rate = Retail_Rate)
  
  Max_Retail_Rate <- max(Retail_Rates_With_Discount$Rate)
  
  
  if(ECR_Year == 2025 && ECR_Customer_Segment %in% c("Residential General Market", "Residential Low-Income", "Residential New Home/Change of Party")){
    
    Retail_Rate_Overlay <- TRUE
    
    # Retail rates are the same every day
    # for all days in a given season and Day-Type,
    # so the average is being taken across identical values.
    # Make the retail rate name the first month of the year
    # (before Jan) so that it shows up first on plot legend.
    Retail_Rates_With_Discount <- Retail_Rates_With_Discount %>%
      filter(DayType == Day_Type) %>%
      group_by(DayType, Hour_Beginning) %>% 
      summarize(Rate = mean(Rate)) %>% 
      ungroup() %>%
      mutate(Month = factor(Retail_Rate_Name, levels = c(Retail_Rate_Name, month.abb))) %>%
      select(Month, DayType, Hour_Beginning, Rate)
    
    Export_Compensation_Rates <- rbind(Retail_Rates_With_Discount,
                                       Export_Compensation_Rates)
    
  }else{
    Retail_Rate_Overlay <- FALSE
  }
  
  rm(Retail_Rates, Retail_Rate_Name, Retail_Rates_With_Discount)
  
  
  # Calculate Y-Axis Upper Limit
  # Round Summer values up to the nearest increment of $1.00/kWh,
  # round Winter/Spring values up up to the nearest increment of $0.10/kWh
  # Note: Y-Axis upper limits vary between Summer and Winter,
  # but are constant across ACC Years.
  Max_Rate <- max(Max_ECR, Max_Retail_Rate)
  Y_Axis_Upper_Limit <- if(Rate_Season == "Summer") ceiling(Max_Rate) else ceiling(Max_Rate * 10)/10
  Y_Axis_Breaks <- if(Rate_Season == "Summer") 1 else 0.1
  rm(Max_ECR, Max_Retail_Rate, Max_Rate)
  
  
  ##### Plot Export Compensation Rates ####
  
  Plot_Title <- paste(ECR_Customer_Segment,
                      Retail_Rate_Customer_Segment,
                      Utility_Name,
                      paste0("IX", IX_App_Year),
                      Rate_Season, 
                      Day_Type,
                      ECR_Year,
                      "Export Compensation Rate Comparison")
  
  # Optional - abbreviations and acronyms for shorter plot titles
  # Plot_Title <- gsub("Residential", "Resi", Plot_Title)
  Plot_Title <- gsub("No Discount ", "", Plot_Title)
  # Plot_Title <- gsub("General Market", "GM", Plot_Title)
  # Plot_Title <- gsub("Low-Income", "LI", Plot_Title)
  # Plot_Title <- gsub("New Home/Change of Party", "NH/CoP", Plot_Title)
  # Plot_Title <- gsub("Export Compensation Rate", "ECR", Plot_Title)
  
  Plot_Filepath <- file.path(Code_WD,
                             ECR_Customer_Segment,
                             Retail_Rate_Customer_Segment,
                             Utility_Name,
                             paste0("IX", IX_App_Year),
                             Rate_Season,
                             Day_Type)
  
  # Create folders if one does not exist already
  if(!dir.exists(Plot_Filepath)){
    dir.create(Plot_Filepath, recursive = TRUE)
  }
  
  rm(Utility_Name, ECR_Year, ECR_Customer_Segment, Rate_Season, Day_Type)
  
  ECR_Plot_Object <- ggplot(Export_Compensation_Rates) +
    geom_step(aes(x = Hour_Beginning, y = Rate,
                  color = Month),
              linewidth = 1, linetype = "solid") +
    scale_x_continuous(breaks = seq(0, 24, 2),
                       labels = paste0(seq(0, 24, 2), ":00"),
                       expand = c(0,0)) +
    scale_y_continuous(breaks = seq(0, Y_Axis_Upper_Limit, Y_Axis_Breaks),
                       limits = c(0, Y_Axis_Upper_Limit),
                       labels = scales::dollar_format()) +
    labs(title = Plot_Title,
         x = "Hour Beginning",
         y = "Rate ($/kWh)", color = "Legend") +
    theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = c(0.02, 0.95),
          legend.justification = c("left", "top"),
          legend.box.just = "left",
          legend.background = element_rect(fill = "white", color = "black"))
  
  rm(Y_Axis_Upper_Limit, Y_Axis_Breaks)
  
  # If applicable, show retail rate in black, use standard ggplot colors for ECRs.
  if(Retail_Rate_Overlay == TRUE){
    Manual_Plot_Colors <- c("#000000", hue_pal()(length(unique(Export_Compensation_Rates$Month)) - 1))
  }else if(Retail_Rate_Overlay == FALSE){
    Manual_Plot_Colors <- hue_pal()(length(unique(Export_Compensation_Rates$Month)))
  }
  
  ECR_Plot_Object <- ECR_Plot_Object +
    scale_color_manual(values = Manual_Plot_Colors)
  
  rm(Retail_Rate_Overlay, Manual_Plot_Colors)
  
  
  ggsave(filename = file.path(Plot_Filepath, paste0(Plot_Title,
                                                    ".png")),
         plot = ECR_Plot_Object,
         width = 13.333, height = 7.5, units = "in")
  
  rm(Export_Compensation_Rates, Plot_Title, Plot_Filepath, ECR_Plot_Object)
  
}


#### Iterate Through All Inputs ####
# Warning: this will generate many gigabytes of plots if iterating through all combinations of inputs.

# ECR_Customer_Segments <- c("Residential General Market", "Residential Low-Income", "Residential New Home/Change of Party", "Non-Residential")
# Utility_Names <- c("PG&E", "SCE", "SDG&E")
# Day_Types <- c("Weekday", "Weekend & Holiday")
# ECR_Years <- seq(2023, 2052)
# IX_App_Years <- seq(2023, 2026)

for(ECR_Customer_Segment in ECR_Customer_Segments){
  
  if(ECR_Customer_Segment %in% c("Residential General Market", "Non-Residential")) {
    Retail_Rate_Customer_Segments <- "No Discount"
  } else if(ECR_Customer_Segment == "Residential Low-Income") {
    Retail_Rate_Customer_Segments <- c("No Discount", "CARE", "FERA") # It's possible to be considered low-income by NBT but not receive CARE/FERA discount on retail rates.
  } else if(ECR_Customer_Segment == "Residential New Home/Change of Party") {
    Retail_Rate_Customer_Segments <- c("No Discount", "CARE", "FERA")
  }
  
  for(Retail_Rate_Customer_Segment in Retail_Rate_Customer_Segments){
    
    for(Utility_Name in Utility_Names){
      
      Rate_Seasons <- if(Utility_Name == "SDG&E") c("Summer", "Winter", "Spring") else c("Summer", "Winter")
      
      for(Rate_Season in Rate_Seasons){
        for(Day_Type in Day_Types){
          for(ECR_Year in ECR_Years){
            
            ECR_Plot(ECR_Customer_Segment, Retail_Rate_Customer_Segment, Utility_Name, Rate_Season, Day_Type, ECR_Year, IX_App_Year)
            
          }
        }
      }
    }
  }
}

