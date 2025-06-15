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
  output$Customer_Segment_List <- renderUI({
    Customer_Segment_List <- c("Residential General Market", "Residential Low-Income", "Residential New Home/Change of Party", "Non-Residential")
    selectizeInput("Customer_Segment_Choose", "Customer Segment:", Customer_Segment_List)
  })
  
  output$Utility_Name_List <- renderUI({
    Utility_Name_List <- c("PG&E", "SCE", "SDG&E")
    selectizeInput("Utility_Name_Choose", "Utility Name:", Utility_Name_List)
  }) 
  
  output$Rate_Season_List <- renderUI({
    Rate_Season_List <- c("Summer", "Winter", "Spring")
    selectizeInput("Rate_Season_Choose", "Rate Season:", Rate_Season_List)
  }) 
  
  output$Day_Type_List <- renderUI({
    Day_Type_List <- c("Weekday", "Weekend & Holiday")
    selectizeInput("Day_Type_Choose", "Day Type:", Day_Type_List)
  }) 
  
  output$ACC_Year_List <- renderUI({
    ACC_Year_List <- seq(2023, 2052)
    selectizeInput("ACC_Year_Choose", "ACC Year:", ACC_Year_List)
  })
  
  output$IX_App_Year_List <- renderUI({
    IX_App_Year_List <- seq(2023, 2030)
    selectizeInput("IX_App_Year_Choose", "Final Interconnection Application Year:", IX_App_Year_List)
  })
  
  
  #### Load and Process ACC Plus Adder Data ####
  ACC_Plus_Adders <- read.csv(paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                                     "NBT%20ECR%20Visualization/ACC%20Plus%20Adders%20by%20Year.csv")) %>%
    gather(key = "IX.App.Year", value = "Adder", X2023:X2030) %>%
    mutate(IX.App.Year = gsub("X", "", IX.App.Year))
  
  ACC_Plus_Adder <- reactive({
    
    req(input$Customer_Segment_Choose)
    req(input$Utility_Name_Choose)
    req(input$IX_App_Year_Choose)
    
    ACC_Plus_Adders_Filtered <- ACC_Plus_Adders %>%
      filter(Customer.Segment == input$Customer_Segment_Choose,
             Utility == input$Utility_Name_Choose,
             IX.App.Year == as.character(input$IX_App_Year_Choose))
    
    ACC_Plus_Adders_Filtered$Adder
    
  })
  
  
  #### Load Export Compensation Rates for Selected Utility ####
  Export_Compensation_Rates <- reactive({
    
    req(input$Utility_Name_Choose)
    
    if(input$Utility_Name_Choose == "PG&E"){
      read.csv(paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                      "Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20Calculation/2022%20ACC%20NBT%20ECRs/",
                      "PG%26E%20Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20-%20Simple%20Average%20DCap%20-%20Bundled.csv")) %>%
        mutate(Value = Value + ACC_Plus_Adder())
      
    } else if(input$Utility_Name_Choose == "SCE"){
      read.csv(paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                      "Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20Calculation/2022%20ACC%20NBT%20ECRs/",
                      "SCE%20Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20-%20Simple%20Average%20DCap%20-%20Bundled.csv")) %>%
        mutate(Value = Value + ACC_Plus_Adder())
      
    } else if(input$Utility_Name_Choose == "SDG&E"){
      read.csv(paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                      "Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20Calculation/2022%20ACC%20NBT%20ECRs/",
                      "SDG%26E%20Net%20Billing%20Tariff%20Export%20Compensation%20Rate%20-%20Simple%20Average%20DCap%20-%20Bundled.csv")) %>%
        mutate(Value = Value + ACC_Plus_Adder())
    }
    
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
      mutate(ACC_Year = year(DateStart)) %>%
      mutate(Hour_Beginning = as.numeric(str_sub(TimeStart, 1, 2)))
    
  })
  
  
  #### Dynamically Update Season Dropdown Options Based On Available Data ####
  observeEvent(Mutated_Export_Compensation_Rates(), {
    Rate_Season_List_Update <- sort(unique(as.vector(Mutated_Export_Compensation_Rates()$Rate_Season)), decreasing = FALSE)
    updateSelectizeInput(session = session, inputId = "Rate_Season_Choose", choices = Rate_Season_List_Update, selected = input$Rate_Season_Choose)
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
    req(input$ACC_Year_Choose)
    
    Season_Filtered_Export_Compensation_Rates() %>%
      filter(DayType == input$Day_Type_Choose) %>%
      filter(ACC_Year == input$ACC_Year_Choose) %>%
      select(Month, DayType, Hour_Beginning, Rate)
    
  })
  
  
  #### Load Retail Rates and Convert to Plot-Ready Format ####
  # Note: only including retail rate comparison for 2023 ACC Year,
  # and for Residential General Market customer segment.
  # Retail rate data is still loaded for ACC Years beyond 2023,
  # because the maximum value is used to set the y-axis upper limit.
  
  # Did not plot retail rates for post-2023 ACC years
  # because future retail rate values are not available.
  # Alternative approach would be to apply a
  # 4-percent average escalation of residential retail rates
  # https://docs.cpuc.ca.gov/PublishedDocs/Published/G000/M343/K979/343979448.docx
  # (pg. 13)
  # so that post-2023 Export Compensation Rates
  # can be compared to estimated post-2023 retail rate values.
  
  # Did not plot retail rates for Residential Low-Income customer segment
  # because some low-income customers are receiving the CARE discount,
  # some are receiving the FERA discount,
  # and some are not receiving either discount.
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
  
  Retail_Rate_Overlay <- reactive({
    req(input$ACC_Year_Choose)
    req(input$Customer_Segment_Choose)
    if(input$ACC_Year_Choose == 2023 && input$Customer_Segment_Choose == "Residential General Market"){
      TRUE
    }else{
      FALSE
    }
  })
  
  # Load Retail Rates, Filter to Rate Season
  Retail_Rates_Raw <- reactive({
    
    req(input$Utility_Name_Choose)
    
    if(input$Utility_Name_Choose == "PG&E"){
      read.csv(paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                      "Retail%20Rate%20Creation/PG%26E%20E-ELEC/2025/60-Minute%20Data/Dataframe%20Format/",
                      "2025_PGE_E_ELEC_Cost_Dataframe.csv")) %>%
        filter(month.abb[Month] %in% unique(Fully_Filtered_Export_Compensation_Rates()$Month)) %>%
        rename(Rate = Retail_Rate)
      
    } else if(input$Utility_Name_Choose == "SCE"){
      read.csv(paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                      "Retail%20Rate%20Creation/SCE%20TOU-D-PRIME/2025/60-Minute%20Data/Dataframe%20Format/",
                      "2025_SCE_TOU_D_PRIME_Cost_Dataframe.csv")) %>%
        filter(month.abb[Month] %in% unique(Fully_Filtered_Export_Compensation_Rates()$Month)) %>%
        rename(Rate = Retail_Rate)
      
    } else if(input$Utility_Name_Choose == "SDG&E"){
      read.csv(paste0("https://raw.githubusercontent.com/RyanCMann/ACC_to_NBT_ECR/main/",
                      "Retail%20Rate%20Creation/SDG%26E%20EV-TOU-5/2025/60-Minute%20Data/Dataframe%20Format/",
                      "2025_SDGE_EV_TOU_5_Cost_Dataframe.csv")) %>%
        filter(month.abb[Month] %in% unique(Fully_Filtered_Export_Compensation_Rates()$Month)) %>%
        rename(Rate = Retail_Rate)
    }
    
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
    max(Retail_Rates_Raw()$Rate)
  })
  
  
  # Convert Retail Rates to Plot-Ready Format
  Retail_Rate_Plot_Ready_Format <- reactive({
    
    req(input$Day_Type_Choose)
    
    # Retail rates are the same every day
    # for all days in a given season and Day-Type,
    # so the average is being taken across identical values.
    # Make the retail rate name the first month of the year
    # (before Jan) so that it shows up first on plot legend.
    Retail_Rates_Raw() %>%
      filter(DayType == input$Day_Type_Choose) %>%
      group_by(DayType, Hour_Beginning) %>% 
      summarize(Rate = mean(Rate)) %>% 
      ungroup() %>%
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
    
    Plot_Title <- paste(input$Customer_Segment_Choose,
                        input$Utility_Name_Choose,
                        input$Rate_Season_Choose,
                        input$Day_Type_Choose,
                        input$ACC_Year_Choose,
                        "ECR Comparison")
    
    Plot_Title <- gsub("Residential", "Resi", Plot_Title)
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
  
  
  #### Download Non-Filtered ECR Data as CSV ####
  # Includes only one utility and customer segment (ACC Plus Adder Value),
  # but includes all rate seasons, day-types, and ACC years.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$Customer_Segment_Choose,
            input$Utility_Name_Choose,
            "Net Billing Tariff Export Compensation Rates.csv")
    },
    content = function(file) {
      write.csv(Export_Compensation_Rates(), file, row.names = FALSE)
    }
  )
  
})