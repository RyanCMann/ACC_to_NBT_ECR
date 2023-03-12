# Dynamic Filtering Shiny App based on "Shiny App (R) integrating a filter with Multiple Dynamic Conditions" by Lee Rock
# https://www.linkedin.com/pulse/shiny-app-r-integrating-filter-multiple-dynamic-conditions-lee-rock/

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Net Billing Tariff Export Compensation Rate Data Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("Customer_Segment_List"),
      
      uiOutput("Utility_Name_List"),
      
      uiOutput("Rate_Season_List"),

      uiOutput("Day_Type_List"),

      uiOutput("ACC_Year_List"),
      
      downloadButton("downloadData", label = "Download NBT ECR in CEC MIDAS TOU Format")
    ),
    
    mainPanel(
      plotOutput("ECR_Plot"),
    )
  )
))