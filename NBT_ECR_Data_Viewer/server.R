# Dynamic Filtering Shiny App based on "Shiny App (R) integrating a filter with Multiple Dynamic Conditions" by Lee Rock
# https://www.linkedin.com/pulse/shiny-app-r-integrating-filter-multiple-dynamic-conditions-lee-rock/

# Load Packages
library(tidyverse)
library(lubridate)
library(scales)
library(shiny)
library(plotly)

# Disable Scientific Notation.
options(scipen = 999)

# Turn off a confusing message when summarizing data.
options(dplyr.summarise.inform = FALSE)


shinyServer(function(input, output, session) {
  
  
  #### Create User Input Dropdowns ####
  output$ECR_Customer_Segment_List <- renderUI({
    ECR_Customer_Segment_List <- c("Residential General Market", "Residential Low-Income", "Residential New Home/Change of Party", "Non-Residential")
    selectizeInput("ECR_Customer_Segment_Choose", "Export Compensation Rate Customer Segment:", ECR_Customer_Segment_List)
  })
  
  # Retail Rate Customer Segment Dropdown with Conditional Options
  output$Retail_Rate_Customer_Segment_List <- renderUI({
    req(input$ECR_Customer_Segment_Choose)
    
    if(input$ECR_Customer_Segment_Choose %in% c("Residential General Market", "Non-Residential")) {
      choices <- c("No Discount")
      selected <- "No Discount"
    } else if(input$ECR_Customer_Segment_Choose == "Residential Low-Income") {
      choices <- c("No Discount", "CARE", "FERA") # It's possible to be considered low-income by NBT but not receive CARE/FERA discount on retail rates. See below.
      selected <- "CARE"
    } else if(input$ECR_Customer_Segment_Choose == "Residential New Home/Change of Party") {
      choices <- c("No Discount", "CARE", "FERA")
      selected <- "No Discount"
    }
    
    selectizeInput("Retail_Rate_Customer_Segment_Choose", "Retail Rate Customer Segment:", 
                   choices = choices, selected = selected)
  })
  
  output$Utility_Name_List <- renderUI({
    Utility_Name_List <- c("PG&E", "SCE", "SDG&E")
    selectizeInput("Utility_Name_Choose", "Utility Name:", Utility_Name_List)
  }) 
  
  output$IX_App_Year_List <- renderUI({
    IX_App_Year_List <- seq(2023, 2026)
    selectizeInput("IX_App_Year_Choose", "Final Interconnection Application Year:", choices = IX_App_Year_List, selected = 2025)
  })
  
  output$Rate_Season_List <- renderUI({
    Rate_Season_List <- c("Summer", "Winter", "Spring")
    selectizeInput("Rate_Season_Choose", "Rate Season:", Rate_Season_List)
  }) 
  
  output$Day_Type_List <- renderUI({
    Day_Type_List <- c("Weekday", "Weekend & Holiday")
    selectizeInput("Day_Type_Choose", "Day Type:", Day_Type_List)
  }) 
  
  output$ECR_Year_List <- renderUI({
    ECR_Year_List <- seq(2023, 2054)
    selectizeInput("ECR_Year_Choose", "Export Compensation Rate Year:", choices = ECR_Year_List, selected = 2025)
  })
  
  output$Rate_Components_List <- renderUI({
    Rate_Components_List <- c("All Components", "Delivery Only", "Generation Only")
    selectizeInput("Rate_Components_Choose", "Rate Components:", Rate_Components_List, selected = "All Components")
  })

  
  
  #### Load and Process ACC Plus Adder Data ####
  ACC_Plus_Adders <- read.csv(paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                                     "NBT%20ECR%20Visualization/ACC%20Plus%20Adders%20by%20Year.csv")) %>%
    gather(key = "IX.App.Year", value = "Adder", X2023:X2030) %>%
    mutate(IX.App.Year = gsub("X", "", IX.App.Year))
  
  ACC_Plus_Adder <- reactive({
    
    req(input$ECR_Customer_Segment_Choose)
    req(input$Utility_Name_Choose)
    req(input$IX_App_Year_Choose)
    req(input$Rate_Components_Choose)
    
    ACC_Plus_Adders_Filtered <- ACC_Plus_Adders %>%
      filter(Customer.Segment == input$ECR_Customer_Segment_Choose,
             Utility == input$Utility_Name_Choose,
             IX.App.Year == as.character(input$IX_App_Year_Choose))
    
    ACC_Plus_Adders_Filtered$Adder
    
  })
  
  
  #### Load Export Compensation Rates for Selected Utility ####
  Export_Compensation_Rates <- reactive({
    
    req(input$Utility_Name_Choose)
    
    req(input$IX_App_Year_Choose)
    
    if(input$IX_App_Year_Choose %in% c(2023, 2024)){
      ACC_Vintage <- 2022
    }else if(input$IX_App_Year_Choose %in% c(2025, 2026)){
      ACC_Vintage <- 2024
    }
    
    # Map Rate_Components to URL component
    if(input$Rate_Components_Choose == "All Components"){
      URL_Component <- "Bundled"
    } else if(input$Rate_Components_Choose == "Delivery Only"){
      URL_Component <- "Unbundled%20Delivery"
    } else if(input$Rate_Components_Choose == "Generation Only"){
      URL_Component <- "Unbundled%20Generation"
    }
    
    URL_Utility <- gsub("&", "%26", input$Utility_Name_Choose)
    
    NBT_ECRs_Directory <- paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                                 "Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20Calculation/", ACC_Vintage, "%20ACC%20NBT%20ECRs/")
    
    NBT_ECR_File <- paste0(NBT_ECRs_Directory, URL_Utility,
                           "%20Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20-%20Simple%20Average%20DCap%20-%20", URL_Component, ".csv")
    
    read.csv(NBT_ECR_File) %>%
      mutate(Value = Value + ACC_Plus_Adder())
    
  })
  
  
  #### Load Rate Season Data for Selected Utility ####
  Winter_Months <- reactive({
    req(input$Utility_Name_Choose)
    if(input$Utility_Name_Choose == "PG&E"){
      c("Jan", "Feb", "Mar", "Apr", "May", "Oct", "Nov", "Dec")
    } else if(input$Utility_Name_Choose == "SCE"){
      c("Jan", "Feb", "Mar", "Apr", "May", "Oct", "Nov", "Dec")
    } else if(input$Utility_Name_Choose == "SDG&E"){
      c("Jan", "Feb", "May", "Nov", "Dec")
    }
  })
  
  Spring_Months <- reactive({
    req(input$Utility_Name_Choose)
    if(input$Utility_Name_Choose == "PG&E"){
      NA
    } else if(input$Utility_Name_Choose == "SCE"){
      NA
    } else if(input$Utility_Name_Choose == "SDG&E"){
      c("Mar", "Apr")
    }
  })
  
  Summer_Months <- reactive({
    req(input$Utility_Name_Choose)
    if(input$Utility_Name_Choose == "PG&E"){
      c("Jun", "Jul", "Aug", "Sep")
    } else if(input$Utility_Name_Choose == "SCE"){
      c("Jun", "Jul", "Aug", "Sep")
    } else if(input$Utility_Name_Choose == "SDG&E"){
      c("Jun", "Jul", "Aug", "Sep", "Oct")
    }
  })
  
  
  #### Create Additional Columns for Use In Filtering ECR Data ####
  Mutated_Export_Compensation_Rates <- reactive({
    
    req(input$Utility_Name_Choose)
    
    Export_Compensation_Rates() %>%
      mutate(DateStart = as.Date(DateStart, tz = "America/Los_Angeles")) %>%
      mutate(Month = month(DateStart)) %>%
      mutate(Month = factor(month.abb[Month], levels = month.abb)) %>%
      mutate(Rate = Value) %>%
      mutate(Rate_Season = ifelse(Month %in% Summer_Months(), "Summer",
                                  ifelse(Month %in% Winter_Months(), "Winter",
                                         "Spring"))) %>%
      mutate(DayType = ifelse(DayTypeStart == 6 & DayTypeEnd == 8, "Weekend & Holiday", "Weekday")) %>%
      mutate(ECR_Year = year(DateStart)) %>%
      mutate(Hour_Beginning = as.numeric(str_sub(TimeStart, 1, 2)))
    
  })
  
  
  #### Dynamically Update Season Dropdown Options Based On Available Data ####
  observeEvent(Mutated_Export_Compensation_Rates(), {
    Rate_Season_List_Update <- sort(unique(as.vector(Mutated_Export_Compensation_Rates()$Rate_Season)), decreasing = FALSE)
    updateSelectizeInput(session = session, inputId = "Rate_Season_Choose", choices = Rate_Season_List_Update, selected = input$Rate_Season_Choose)
  })
  
  #### Dynamically Update Export Compensation Rate Year Dropdown Options Based On Available Data ####
  observeEvent(Mutated_Export_Compensation_Rates(), {
    ECR_Year_List_Available <- sort(unique(as.vector(Mutated_Export_Compensation_Rates()$ECR_Year)), decreasing = FALSE)
    
    # Further filter based on interconnection year constraint
    if (!is.null(input$IX_App_Year_Choose)) {
      ECR_Year_List_Available <- ECR_Year_List_Available[ECR_Year_List_Available >= as.numeric(input$IX_App_Year_Choose)]
    }
    
    # Only update if the available years have changed
    current_choices <- as.numeric(isolate(input$ECR_Year_Choose))
    if (!identical(sort(ECR_Year_List_Available), sort(as.numeric(names(isolate(input$ECR_Year_Choose)))))) {
      
      # Preserve current selection if still valid
      if (!is.null(input$ECR_Year_Choose) && as.numeric(input$ECR_Year_Choose) %in% ECR_Year_List_Available) {
        selected_year <- input$ECR_Year_Choose
      } else {
        selected_year <- ECR_Year_List_Available[1]
      }
      
      updateSelectizeInput(session = session, 
                           inputId = "ECR_Year_Choose", 
                           choices = ECR_Year_List_Available, 
                           selected = selected_year)
    }
  })
  
  
  #### Filter ECR Data Based on Rate Season ####
  Season_Filtered_Export_Compensation_Rates <- reactive({
    req(input$Rate_Season_Choose)
    Mutated_Export_Compensation_Rates() %>%
      filter(Rate_Season == input$Rate_Season_Choose)
  })
  
  
  # Save maximum ECR value to be used to set plot y-axis upper limit.
  Max_ECR <- reactive({
    max(Season_Filtered_Export_Compensation_Rates()$Rate)
  })
  
  
  #### Filter ECR Data Based on Day-Type and ACC Year ####
  Fully_Filtered_Export_Compensation_Rates <- reactive({
    
    req(input$Day_Type_Choose)
    req(input$ECR_Year_Choose)
    
    Season_Filtered_Export_Compensation_Rates() %>%
      filter(DayType == input$Day_Type_Choose) %>%
      filter(ECR_Year == input$ECR_Year_Choose) %>%
      select(Month, DayType, Hour_Beginning, Rate)
    
  })
  
  
  #### Load Retail Rates and Convert to Plot-Ready Format ####
  # Note: only including retail rate comparison for 2025 ACC Year,
  # and for the residential Export Compensation Rate customer segments
  # (including low-income segments receiving discounted retail rates).
  # Retail rate data is still loaded for ACC Years beyond 2025,
  # because the maximum value is used to set the y-axis upper limit.
  
  # Did not plot retail rates for post-2025 ACC years
  # because future retail rate values are not available.
  # Alternative approach would be to apply a
  # 4-percent average escalation of residential retail rates
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
  
  # Determine Retail Rate Overlay Status
  Retail_Rate_Overlay <- reactive({
    req(input$ECR_Year_Choose)
    req(input$ECR_Customer_Segment_Choose)
    req(input$Rate_Components_Choose)
    # Show retail rates for 2025 and residential customer segments
    if(input$ECR_Year_Choose == 2025 && 
       input$ECR_Customer_Segment_Choose %in% c("Residential General Market", "Residential Low-Income", "Residential New Home/Change of Party")){
      TRUE
    }else{
      FALSE
    }
  })
  
  # Load Retail Rate Library
  Retail_Rate_Library <- reactive({
    read.csv("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/refs/heads/main/Retail%20Rate%20Creation/Retail%20Rates/Retail%20Rate%20Library.csv")
  })
  
  # Filter to Selected Retail Rate and Season, Convert from Bands Format to Hourly Rates Vector
  Retail_Rates_Hourly <- reactive({
    
    req(input$Utility_Name_Choose)
    req(input$Rate_Season_Choose)
    req(input$Day_Type_Choose)
    
    # Filter for the specified parameters and CONSUMPTION charges only
    Filtered_Rates <- Retail_Rate_Library() %>%
      filter(Delivery.Utility == input$Utility_Name_Choose, # TODO: For CCAs, switch to filtering on Delivery Utility and Generation Supplier and Rate Schedule
             Rate.Season == input$Rate_Season_Choose,
             Charge.Type == "CONSUMPTION") %>%
      # Handle day type filtering - if Day.Type is empty, it applies to all days
      filter(is.na(Day.Type) | Day.Type == "" | Day.Type == input$Day_Type_Choose)
    
    # Initialize hourly rates vector (24 hours, 0-23)
    Hourly_Rates <- rep(NA, 24)
    
    # Process each TOU band
    for(i in 1:nrow(Filtered_Rates)) {
      from_hour <- Filtered_Rates$From.Hour[i]
      to_hour <- Filtered_Rates$To.Hour[i]
      
      # Select appropriate rate based on Rate_Components parameter
      if(input$Rate_Components_Choose == "All Components"){
        rate <- Filtered_Rates$Total.Rate[i]
      } else if(input$Rate_Components_Choose == "Delivery Only"){
        rate <- Filtered_Rates$Delivery.Rate[i]
      } else if(input$Rate_Components_Choose == "Generation Only"){
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
    
    # Create dataframe in format compatible with existing script
    Retail_Rates <- data.frame(
      Season = input$Rate_Season_Choose,
      DayType = input$Day_Type_Choose,
      Hour_Beginning = 0:23,
      Retail_Rate = Hourly_Rates
    )
    
    # Check for any missing rates (hours not covered by TOU bands)
    if(any(is.na(Retail_Rates$Retail_Rate))) {
      Missing_Hours <- which(is.na(Retail_Rates$Retail_Rate)) - 1  # Convert back to 0-based
      warning(paste("Missing rates for hours:", paste(Missing_Hours, collapse = ", ")))
    }
    
    return(Retail_Rates)
    
  })
  
  
  #### Load Retail Rate Low-Income Discount Data ####
  Low_Income_Discounts <- reactive({
    read.csv("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/refs/heads/main/Retail%20Rate%20Creation/Low-Income%20Discounts/Low%20Income%20Discount%20Percentages.csv")
  })
  
  #### Apply Low-Income Discount to Retail Rates ####
  Retail_Rates_With_Discount <- reactive({
    
    req(input$Utility_Name_Choose)
    req(input$Retail_Rate_Customer_Segment_Choose)

    # Get discount rate from the low-income discount data
    discount_data <- Low_Income_Discounts() %>%
      filter(Utility == input$Utility_Name_Choose,
             Discount.Program == input$Retail_Rate_Customer_Segment_Choose)
    
    discount_rate <- if(nrow(discount_data) > 0) discount_data$Discount.Rate[1] else 0
    
    # Apply Discount
    Retail_Rates_With_Discount <- Retail_Rates_Hourly() %>%
      mutate(Retail_Rate = Retail_Rate * (1 - discount_rate)) %>%
      rename(Rate = Retail_Rate)
    
    return(Retail_Rates_With_Discount)
    
  })
  
  # Get Retail Rate Name for Plot Legend
  Retail_Rate_Name <- reactive({
    req(input$Utility_Name_Choose)
    if(input$Utility_Name_Choose == "PG&E"){
      "E-ELEC"
    }else if(input$Utility_Name_Choose == "SCE"){
      "TOU-D-PRIME"
    }else if(input$Utility_Name_Choose == "SDG&E"){
      "EV-TOU-5"
    }
  })
  
  # Save maximum retail rate value to be used to set plot y-axis upper limit.
  Max_Retail_Rate <- reactive({
    max(Retail_Rates_With_Discount()$Rate)
  })
  
  
  # Convert Retail Rates to Plot-Ready Format
  Retail_Rate_Plot_Ready_Format <- reactive({
    
    req(input$Day_Type_Choose)
    
    # Make the retail rate name the first month of the year
    # (before Jan) so that it shows up first on plot legend.
    Retail_Rates_With_Discount() %>%
      mutate(Month = factor(Retail_Rate_Name(), levels = c(Retail_Rate_Name(), month.abb))) %>%
      select(Month, DayType, Hour_Beginning, Rate)
    
  })
  
  
  #### Create Final Plot-Ready Rate Dataframe ####
  # Concatenate retail rates, if applicable.
  Plot_Ready_Rates <- reactive({
    if(Retail_Rate_Overlay() == TRUE){
      rbind(Retail_Rate_Plot_Ready_Format(),
            Fully_Filtered_Export_Compensation_Rates())
    }else{
      Fully_Filtered_Export_Compensation_Rates()
    }
  })
  
  
  #### Calculate Y-Axis Upper Limit and Breaks ####
  # Round Summer values up to the nearest increment of $1.00/kWh,
  # round Winter/Spring values up up to the nearest increment of $0.10/kWh
  # Note: Y-Axis upper limits vary between Summer and Winter,
  # but are constant across ACC Years.
  
  Y_Axis_Upper_Limit <- reactive({
    req(input$Rate_Season_Choose)
    Max_Rate <- max(Max_ECR(), Max_Retail_Rate())
    if(input$Rate_Season_Choose == "Summer") ceiling(Max_Rate) else ceiling(Max_Rate * 10)/10
  })
  
  Y_Axis_Breaks <- reactive({
    req(input$Rate_Season_Choose)
    if(input$Rate_Season_Choose == "Summer") 1 else 0.1
  })
  
  
  #### Create ECR Plot ####
  output$ECR_Plot <- renderPlotly({
    
    Plot_Title <- paste(input$ECR_Customer_Segment_Choose,
                        input$Retail_Rate_Customer_Segment_Choose,
                        input$Utility_Name_Choose,
                        paste0("IX", input$IX_App_Year_Choose),
                        input$Rate_Season_Choose,
                        input$Day_Type_Choose,
                        input$ECR_Year_Choose,
                        "ECR Comparison -",
                        input$Rate_Components_Choose)
    
    Plot_Title <- gsub("Residential", "Resi", Plot_Title)
    Plot_Title <- gsub("No Discount ", "", Plot_Title)
    Plot_Title <- gsub("General Market", "GM", Plot_Title)
    Plot_Title <- gsub("Low-Income", "LI", Plot_Title)
    Plot_Title <- gsub("New Home/Change of Party", "NH/CoP", Plot_Title)
    
    ECR_Plot_Object <- ggplot(Plot_Ready_Rates(),
                              aes(group = 1,
                                  text = paste("Hour Beginning: ", paste0(Hour_Beginning, ":00"),
                                               "<br>Rate: $", paste0(Rate, "/kWh"),
                                               "<br>Legend: ", Month))) +
      geom_step(aes(x = Hour_Beginning, y = Rate,
                    color = Month), 
                linewidth = 1, linetype = "solid") +
      scale_x_continuous(breaks = seq(0, 24, 2),
                         labels = paste0(seq(0, 24, 2), ":00"),
                         expand = c(0,0)) +
      scale_y_continuous(breaks = seq(0, Y_Axis_Upper_Limit(), Y_Axis_Breaks()),
                         limits = c(0, Y_Axis_Upper_Limit()),
                         labels = scales::dollar_format()) +
      labs(title = Plot_Title,
           x = "Hour Beginning",
           y = "Rate ($/kWh)", color = "Legend") +
      theme(text = element_text(size = 15), plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = c(0.01, 0.99),
            legend.justification = c("left", "top"),
            legend.box.just = "left",
            legend.background = element_rect(fill = "white", color = "black"))
    
    # If applicable, show retail rate in black, use standard ggplot colors for ECRs.
    if(Retail_Rate_Overlay() == TRUE){
      Manual_Plot_Colors <- c("#000000", hue_pal()(length(unique(Plot_Ready_Rates()$Month)) - 1))
    }else{
      Manual_Plot_Colors <- hue_pal()(length(unique(Plot_Ready_Rates()$Month)))
    }
    
    ECR_Plot_Object <- ECR_Plot_Object +
      scale_color_manual(values = Manual_Plot_Colors)
    
    ECR_Plot_Object <- ggplotly(ECR_Plot_Object, tooltip = "text", height = 600) %>% 
      config(displayModeBar = F)
    
    print(ECR_Plot_Object)
    
  })
  
  
  #### Display ACC Plus Adder Value ####
  output$ACC_Plus_Adder_Display <- renderText({
    req(ACC_Plus_Adder())
    paste("<b>ACC Plus Adder:</b>", 
          scales::dollar(ACC_Plus_Adder(), accuracy = 0.001), 
          "per kWh")
  })
  
  # Make the text output larger
  outputOptions(output, "ACC_Plus_Adder_Display", suspendWhenHidden = FALSE)
  
  
  #### Download Non-Filtered ECR Data as CSV ####
  # Includes only one utility, customer segment, and final interconnection application year,
  # but includes all rate seasons, day-types, and ACC years.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$ECR_Customer_Segment_Choose,
            input$Utility_Name_Choose,
            input$Rate_Components_Choose,
            "NBT Export Compensation Rates.csv")
    },
    content = function(file) {
      write.csv(Export_Compensation_Rates(), file, row.names = FALSE)
    }
  )
  
})