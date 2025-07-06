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
# IX_App_Year <- 2025 # Final Interconnection Application Year 2023 . . . 2026
# Rate_Season <- "Summer" # "Summer", "Winter", "Spring" (Note: "Spring" only applies to SDG&E)
# Day_Type <- "Weekday" # "Weekday", "Weekend & Holiday"
# ECR_Year <- 2025 # Simulation Year in Avoided Cost Calculator (not vintage of ACC spreadsheet) 2023 . . . 2054
# Rate_Components <- "All Components" # "All Components", "Delivery Only", "Generation Only"

ECR_Plot <- function(ECR_Customer_Segment, Retail_Rate_Customer_Segment, Utility_Name, IX_App_Year, Rate_Season, Day_Type, ECR_Year, Rate_Components){
  
  #### Input Mapping ####
  
  if(IX_App_Year %in% c(2023, 2024)){
    ACC_Vintage = 2022
  }else if(IX_App_Year %in% c(2025, 2026)){
    ACC_Vintage = 2024
  }
  
  # Map Rate_Components to URL component
  if(Rate_Components == "All Components"){
    URL_Component <- "Bundled"
  } else if(Rate_Components == "Delivery Only"){
    URL_Component <- "Unbundled%20Delivery"
  } else if(Rate_Components == "Generation Only"){
    URL_Component <- "Unbundled%20Generation"
  }
  
  URL_Utility <- gsub("&", "%26", Utility_Name)
  
  NBT_ECR_File <- paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                         "Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20Calculation/", ACC_Vintage, "%20ACC%20NBT%20ECRs/", URL_Utility,
                         "%20Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20-%20Simple%20Average%20DCap%20-%20", URL_Component, ".csv")
  
  if(Utility_Name == "PG&E"){
    Winter_Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Oct", "Nov", "Dec")
    Spring_Months <- NA
    Summer_Months <- c("Jun", "Jul", "Aug", "Sep")
  } else if(Utility_Name == "SCE"){
    Winter_Months <- c("Jan", "Feb", "Mar", "Apr", "May", "Oct", "Nov", "Dec")
    Spring_Months <- NA
    Summer_Months = c("Jun", "Jul", "Aug", "Sep")
  } else if(Utility_Name == "SDG&E"){
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
  
  
  #### Load Retail Rates and Convert to Plot-Ready Format ####
  # Note: only including retail rate comparison for 2025 ACC Year,
  # and for the residential Export Compensation Rate customer segments
  # (including low-income segments receiving discounted retail rates).
  # Retail rate data is still loaded for ACC Years beyond 2025,
  # because the maximum value is used to set the y-axis upper limit.
  
  # Did not plot retail rates for post-2025 ACC years
  # because future retail rate values are not available.
  # Alternative approach would be to apply a
  # 4 Percent average escalation of residential retail rates
  # https://docs.cpuc.ca.gov/PublishedDocs/Published/G000/M343/K979/343979448.docx
  # (pg. 13)
  # so that post-2025 Export Compensation Rates
  # can be compared to estimated post-2025 retail rate values.
  
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
  
  # Load the Retail Rate Library
  Retail_Rate_Library <- read.csv(file.path(Retail_Rate_WD,
                                            "Retail Rates",
                                            "Retail Rate Library.csv"))
  
  # Filter for the specified parameters and CONSUMPTION charges only
  Filtered_Rates <- Retail_Rate_Library %>%
    filter(Delivery.Utility == Utility_Name, # TODO: For CCAs, switch to filtering on Delivery Utility and Generation Supplier and Rate Schedule
           Rate.Season == Rate_Season,
           Charge.Type == "CONSUMPTION") %>%
    # Handle day type filtering - if Day.Type is empty, it applies to all days
    filter(is.na(Day.Type) | Day.Type == "" | Day.Type == Day_Type)
  
  rm(Retail_Rate_Library)
  
  # Initialize hourly rates vector (24 hours, 0-23)
  Hourly_Rates <- rep(NA, 24)
  
  # Process each TOU band
  for(i in 1:nrow(Filtered_Rates)) {
    from_hour <- Filtered_Rates$From.Hour[i]
    to_hour <- Filtered_Rates$To.Hour[i]

    # Select appropriate rate based on Rate_Components parameter
    if(Rate_Components == "All Components"){
      rate <- Filtered_Rates$Total.Rate[i]
    } else if(Rate_Components == "Delivery Only"){
      rate <- Filtered_Rates$Delivery.Rate[i]
    } else if(Rate_Components == "Generation Only"){
      rate <- Filtered_Rates$Generation.Rate[i]
    }
    
    
    # Normalize 24 to 0 (both represent midnight)
    if(!is.na(from_hour) && from_hour == 24) from_hour <- 0
    if(!is.na(to_hour) && to_hour == 24) to_hour <- 0
    
    # Generate sequence of hours for this band
    if(is.na(from_hour) & is.na(to_hour)) {
      # All hours case: both From Hour and To Hour are blank
      hours <- 0:23
    } else if(to_hour > from_hour) {
      # Normal case: from_hour to to_hour-1
      hours <- seq(from_hour, to_hour - 1)
    } else if(to_hour < from_hour) {
      # Wraparound case: from_hour to 23, then 0 to to_hour-1
      if(to_hour == 0) {
        # Special case: to_hour is 0, so only go from from_hour to 23
        hours <- seq(from_hour, 23)
      } else {
        # Normal wraparound: from_hour to 23, then 0 to to_hour-1
        hours <- c(seq(from_hour, 23), seq(0, to_hour - 1))
      }
    } else {
      # Edge case: from_hour == to_hour (shouldn't occur)
      hours <- integer(0)  # Empty vector
      warning("This rate tariff includes a TOU band where the From Hour = To Hour. The TOU range is not inclusive of the To Hour, so the To Hour should be equal to the Hour Ending.")
    }
    
    # Assign rate to corresponding hours
    for(hour in hours) {
      Hourly_Rates[hour + 1] <- rate  # +1 because R uses 1-based indexing
    }
  }
  
  Retail_Rate_Name = Filtered_Rates$Rate.Schedule[1]
  
  rm(Filtered_Rates)
  
  # Create dataframe in format compatible with existing script
  Retail_Rates <- data.frame(
    Season = Rate_Season,
    DayType = Day_Type,
    Hour_Beginning = 0:23,
    Retail_Rate = Hourly_Rates
  )
  
  rm(Hourly_Rates)
  
  # Check for any missing rates (hours not covered by TOU bands)
  if(any(is.na(Retail_Rates$Rate))) {
    Missing_Hours <- which(is.na(Retail_Rates$Rate)) - 1  # Convert back to 0-based
    warning(paste("Missing rates for hours:", paste(Missing_Hours, collapse = ", ")))
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
    rename(Rate = Retail_Rate)
  
  Max_Retail_Rate <- max(Retail_Rates_With_Discount$Rate)
  
  rm(Low_Income_Discounts, discount_data)
  
  
  if(ECR_Year == 2025 && ECR_Customer_Segment %in% c("Residential General Market", "Residential Low-Income", "Residential New Home/Change of Party")){
    
    Retail_Rate_Overlay <- TRUE
    
    # Retail rates are the same every day
    # for all days in a given season and Day-Type,
    # so the average is being taken across identical values.
    # Make the retail rate name the first month of the year
    # (before Jan) so that it shows up first on plot legend.
    Retail_Rates_With_Discount <- Retail_Rates_With_Discount %>%
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
                      Rate_Components,
                      "Cost & Credits")
  
  # Optional - abbreviations and acronyms for shorter plot titles
  Plot_Title <- gsub("Residential", "Resi", Plot_Title)
  Plot_Title <- gsub("No Discount ", "", Plot_Title)
  Plot_Title <- gsub("General Market", "GM", Plot_Title)
  Plot_Title <- gsub("Low-Income", "LI", Plot_Title)
  Plot_Title <- gsub("New Home/Change of Party", "NH/CoP", Plot_Title)
  Plot_Title <- gsub("Export Compensation Rate", "ECR", Plot_Title)
  Plot_Title <- gsub("All Components", "Total", Plot_Title)
  Plot_Title <- gsub("Delivery Only", "Delivery", Plot_Title)
  Plot_Title <- gsub("Generation Only", "Generation", Plot_Title)
  
  Plot_Filepath <- file.path(Code_WD,
                             ECR_Customer_Segment,
                             Retail_Rate_Customer_Segment,
                             Utility_Name,
                             paste0("IX", IX_App_Year),
                             Rate_Season,
                             Day_Type,
                             Rate_Components)
  
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
# IX_App_Years <- seq(2023, 2026)
# Day_Types <- c("Weekday", "Weekend & Holiday")
# Rate_Components_Options <- c("All Components", "Delivery Only", "Generation Only")

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
      
      for(IX_App_Year in IX_App_Years){
        
        Rate_Seasons <- if(Utility_Name == "SDG&E") c("Summer", "Winter", "Spring") else c("Summer", "Winter")
        
        for(Rate_Season in Rate_Seasons){
          for(Day_Type in Day_Types){
            
            if(IX_App_Year %in% c(2023, 2024)){
              ECR_Years <- seq(IX_App_Year, 2052) # 2022 ACC includes 2023-2052. ECR Year should always be >= IX App Year.
            }else if(IX_App_Year %in% c(2025, 2026)){
              ECR_Years <- seq(IX_App_Year, 2054) # 2024 ACC includes 2024-2054. ECR Year should always be >= IX App Year.
            }
            
            for(ECR_Year in ECR_Years){
              
              for(Rate_Components in Rate_Components_Options){
                
                ECR_Plot(ECR_Customer_Segment, Retail_Rate_Customer_Segment, Utility_Name, IX_App_Year, Rate_Season, Day_Type, ECR_Year, Rate_Components)
                
              }
              
            }
          }
        }
      }
    }
  }
}

