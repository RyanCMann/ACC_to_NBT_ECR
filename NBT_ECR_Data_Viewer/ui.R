# Dynamic Filtering Shiny App based on "Shiny App (R) integrating a filter with Multiple Dynamic Conditions" by Lee Rock
# https://www.linkedin.com/pulse/shiny-app-r-integrating-filter-multiple-dynamic-conditions-lee-rock/

library(plotly)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Net Billing Tariff Export Compensation Rate Data Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("ECR_Customer_Segment_List"),
      
      uiOutput("Retail_Rate_Customer_Segment_List"),
      
      uiOutput("Utility_Name_List"),
      
      uiOutput("IX_App_Year_List"),
      
      uiOutput("Rate_Season_List"),

      uiOutput("Day_Type_List"),

      uiOutput("ECR_Year_List"),
      
      uiOutput("Rate_Components_List"),
      
      downloadButton("downloadData", label = "Download NBT ECR in CEC MIDAS TOU Format")
    ),
    
    mainPanel(
      plotlyOutput("ECR_Plot", height = '100%', width = 'auto'),
      br(),
      div(style = "font-size: 18px;",
          htmlOutput("Fixed_Charge_Display")
      ),
      div(style = "font-size: 18px;",
          htmlOutput("ACC_Plus_Adder_Display")
      ),
      div(style = "font-size: 18px;",
          htmlOutput("NSC_Rate_Display")
      ),
      div(style = "font-size: 18px;",
          HTML('<a href="https://github.com/RyanCMann/ACC_to_NBT_ECR" target="_blank">GitHub Repository - All Code and Data</a>')
      )
    )
  )
))