library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(zoo)
library(shinythemes)

##########################Data cleaning##################################



#==================== Data Merging and Subseting ========================

#load the original data sets
License_Applications <- read.csv("../data/License_Applications.csv")
Legally_Operating_Businesses <- read.csv("../data/Legally_Operating_Businesses.csv")

#get the latest data
#License_Applications <- read.csv("https://data.cityofnewyork.us/api/views/ptev-4hud/rows.csv?accessType=DOWNLOAD")
#Legally_Operating_Businesses<- read.csv("https://data.cityofnewyork.us/api/views/w7w3-xahh/rows.csv?accessType=DOWNLOAD")

#colnames(Legally_Operating_Businesses)

#merge two datasets and subset useful rows&columns
Legal <- Legally_Operating_Businesses[,c('DCA.License.Number','Address.Borough')]

License_merged <- left_join(License_Applications, Legal,by=c('License.Number' = 'DCA.License.Number'),multiple="all")
NY <- c('ny', 'NY', 'New York')
License_merged_Ny <- filter(License_merged, `State` %in% NY)

unique(License_merged_Ny$State) #check

#clean data
License_merged_Ny$`Start.Date` <- mdy(License_merged_Ny$Start.Date)
License_merged_Ny$`End.Date` <- mdy(License_merged_Ny$`End.Date`)
month <- month(License_merged_Ny$Start.Date)
year <- year(License_merged_Ny$`Start.Date`)
License_merged_Ny <- mutate(License_merged_Ny, Start.Month= format_ISO8601(as.Date(Start.Date), precision = "ym"))
License_merged_Ny <- mutate(License_merged_Ny, Start.Year= year)
License_merged_Ny$End.year <- year(License_merged_Ny$End.Date)
License_merged_Ny <- mutate(License_merged_Ny, Processing.time = as.numeric(difftime(License_merged_Ny$`End.Date`,License_merged_Ny$`Start.Date`,units = 'days')))
License_merged_Ny_sub <- License_merged_Ny[,c('License.Type','License.Category','Start.Date','End.Date','End.year','Start.Month','Start.Year','Processing.time','Longitude','Latitude','Status','Application.or.Renewal', 'Address.Borough')]
License_merged_Ny_sub$Processing.time <- ifelse(License_merged_Ny_sub$Processing.time<0,NA,License_merged_Ny_sub$Processing.time)
License_merged_clean <- na.omit(License_merged_Ny_sub)
License_merged_clean <- filter(License_merged_clean,Start.Year>=2014)
License_merged_clean$Address.Borough <- toupper(License_merged_clean$Address.Borough)
#after omiting NA, there are only License type of business, no more individual

#write out a csv with the above cleaned data
#write.csv(License_merged_clean, file = "License_merged_clean.csv", row.names = FALSE)


#load the cleaned data
#License_merged_clean <- read.csv("../output/License_merged_clean.csv")
head(License_merged_clean)
unique(License_merged_clean$License.Type)





#==================== Re-categorize License Category ===========================

#categorize the data

#business
Service <- c("Construction Labor Provider","Storage Warehouse","Home Improvement Contractor","Debt Collection Agency","Laundries","Car Wash","Laundry","Employment Agency","Electronic & Appliance Service","Scale Dealer Repairer"
             ,"Process Serving Agency","Scrap Metal Processor")

Entertainment <-c("Cabaret","Bingo Game Operator","Pool or Billiard Room","Amusement Device Temporary","Gaming Cafe","Games of Chance","Amusement Device Permanent","Amusement Device Portable","Sidewalk Cafe")

Retail <- c("Ticket Seller Business","Dealer In Products","Stoop Line Stand","Electronics Store","Newsstand","Special Sale","Laundry Jobber","Secondhand Dealer - General","Secondhand Dealer - Auto","Electronic Cigarette Dealer","Tobacco Retail Dealer","Pawnbroker","Catering Establishment","Auction House Premises")

Trans <- c("Horse Drawn Cab Owner","Parking Lot","Tow Truck Company","Garage and Parking Lot","Pedicab Business","Garage")

License_merged_clean$New.Category <- ifelse(License_merged_clean$License.Category %in% Service,"Service (Business)",License_merged_clean$License.Category)
License_merged_clean$New.Category <- ifelse(License_merged_clean$License.Category %in% Entertainment,"Entertainment",License_merged_clean$New.Category)
License_merged_clean$New.Category <- ifelse(License_merged_clean$License.Category %in% Retail,"Retail",License_merged_clean$New.Category)
License_merged_clean$New.Category <- ifelse(License_merged_clean$License.Category %in% Trans,"Trans",License_merged_clean$New.Category)
#License_merged_clean$New.Category <- ifelse(License_merged_clean$License.Category %in% Service_1,"Service (Individual)",License_merged_clean$New.Category)
#License_merged_clean$New.Category <- ifelse(License_merged_clean$License.Category %in% Skilled_Wokers,"Skilled_Wokers",License_merged_clean$New.Category)

head(License_merged_clean)
unique(License_merged_clean$New.Category)


#individual
#Service_1 <- c('Tow Truck Driver','Pedicab Driver','Horse Drawn Driver','Home Improvement Salesperson','Temporary Street Fair Vendor','General Vendor','Ticket Seller','Sightseeing Guide','Auctioneer')

#Skilled_Wokers<-c('Process Server Individual','Motion Picture Projectionist','Locksmith Apprentice','Locksmith')



############################# UI ########################################


ui <- dashboardPage(
#=========================Dashboard Title================================

  dashboardHeader(title = "NYC Business License Application",
                  titleWidth = 400),

#==========================SideBar Tabs==================================

  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "Map", icon = icon("map")),
      menuItem("Service (Business)", tabName = "Service-Business", icon = icon("dashboard")),
      menuItem("Entertainment", icon = icon("heart"), tabName = "Entertainment"),
      menuItem("Retail", icon = icon("circle-info"), tabName = "Retail"),
      menuItem("Transportation", icon = icon("car"), tabName = "Transportation"),
      menuItem("Appendix", icon = icon("receipt"), tabName = "Appendix")
    )
  ),

#======================Body for Inputs/Outputs============================

  dashboardBody(
    #----------------------------- theme --------------------------------
    theme = shinythemes::shinytheme('cerulean'),
    
    tabItems(
      #---------------------- Map Tab -----------------------------------
      tabItem(
        tabName = "Map",
        h2("No. of Applications Submitted: 
              804 days before COVID vs 804 days after COVID "),
        fluidRow(
          box(
            width=12,
            title = "Map",
            selectInput("Map_district","Map District:", choices = c('Bronx','Brooklyn','Queens','Manhattan','Staten Island'))
          ),
          box(
            width=6,
            title = "Map",
            imageOutput('Map_1')
          ),
          box(
            width=6,
            title = "Map",
            imageOutput('Map_2')
          )
        )
      ),
      
      #--------------------- Service Tab ---------------------------------
      tabItem(
        tabName = "Service-Business",
              h2("Service tab content"),
              fluidRow(
                box(
                  width=3,
                  title = "Select type of business",
                  selectInput("business_type_service","Business Type:", choices = c("ALL",Service))
                ),
                box(
                  width=3,
                  title = "Select type of application",
                  selectInput("application_type_service","New vs Renewal:", choices = c("Application","Renewal"))
                ),
                box(
                  width=3,
                  title = "Select borough",
                  selectInput("borough_service","Borough:", choices = unique(License_merged_clean$Address.Borough))
                ),
                box(
                  width=3,
                  title = "Select time period",
                  dateRangeInput("dateRange_service","Time period:", 
                                 start=min(License_merged_clean$Start.Date),
                                 end = max(License_merged_clean$Start.Date),
                                 min=min(License_merged_clean$Start.Date),
                                 max=max(License_merged_clean$Start.Date),
                                 startview = 'year')
                ),
                box(
                  width=12,
                  plotOutput("service_plot1", height = 250)
                  ),
                box(
                  width=12,
                  plotOutput("service_plot2", height = 250)
                ),
                box(
                  width=12,
                  plotOutput("service_plot3", height = 250)
                ),
                box(
                  width=12,
                  plotOutput("service_plot4", height = 250)
                )
                
              )
      ),
      
      #---------------------- Entertainment Tab ----------------------------
      tabItem(tabName = "Entertainment",
              h2("Entertainment tab content"),
              fluidRow(
                box(
                  width=3,
                  title = "Select type of business",
                  selectInput("business_type_entertain","Business Type:", choices = c("ALL",Entertainment))
                ),
                box(
                  width=3,
                  title = "Select type of application",
                  selectInput("application_type_entertain","New vs Renewal:", choices = c("Application","Renewal"))
                ),
                box(
                  width=3,
                  title = "Select borough",
                  selectInput("borough_entertain","Borough:", choices = unique(License_merged_clean$Address.Borough))
                ),
                box(
                  width=3,
                  title = "Select time period",
                  dateRangeInput("dateRange_entertain","Time period:", 
                                 start=min(License_merged_clean$Start.Date),
                                 end = max(License_merged_clean$Start.Date),
                                 min=min(License_merged_clean$Start.Date),max=max(License_merged_clean$Start.Date),startview = 'year')
  
                ),
                box(
                  width=12,
                  plotOutput("entertainment_plot1", height = 250)
                ),
                box(
                  width=12,
                  plotOutput("entertainment_plot2", height = 250)
                ),
                box(
                  width=12,
                  plotOutput("entertainment_plot3", height = 250)
                ),
                box(
                  width=12,
                  plotOutput("entertainment_plot4", height = 250)
                )
      )
    ),
    
    # ------------------------ Retail Tab --------------------------------
    tabItem(tabName = "Retail",
            h2("Retail tab content"),
            fluidRow(
              box(
                width=3,
                title = "Select type of business",
                selectInput("business_type_retail","Business Type:", choices = c("ALL",Retail))
              ),
              box(
                width=3,
                title = "Select type of application",
                selectInput("application_type_retail","New vs Renewal:", choices = c("Application","Renewal"))
              ),
              box(
                width=3,
                title = "Select borough",
                selectInput("borough_retail","Borough:", choices = unique(License_merged_clean$Address.Borough))
              ),
              box(
                width=3,
                title = "Select time period",
                dateRangeInput("dateRange_retail","Time period:", 
                               start=min(License_merged_clean$Start.Date),
                               end = max(License_merged_clean$Start.Date),
                               min=min(License_merged_clean$Start.Date),max=max(License_merged_clean$Start.Date),startview = 'year')
                
              ),
              box(
                width=12,
                plotOutput("retail_plot1", height = 250)
              ),
              box(
                width=12,
                plotOutput("retail_plot2", height = 250)
              ),
              box(
                width=12,
                plotOutput("retail_plot3", height = 250)
              ),
              box(
                width=12,
                plotOutput("retail_plot4", height = 250)
              )
            )
    ),
    
    #--------------------- Transportation tab ----------------------------
    tabItem(tabName = "Transportation",
            h2("Transportation tab content"),
            fluidRow(
              box(
                width=3,
                title = "Select type of business",
                selectInput("business_type_trans","Business Type:", choices = c("ALL",Trans))
              ),
              box(
                width=3,
                title = "Select type of application",
                selectInput("application_type_trans","New vs Renewal:", choices = c("Application","Renewal"))
              ),
              box(
                width=3,
                title = "Select borough",
                selectInput("borough_trans","Borough:", choices = unique(License_merged_clean$Address.Borough))
              ),
              box(
                width=3,
                title = "Select time period",
                dateRangeInput("dateRange_trans","Time period:", 
                               start=min(License_merged_clean$Start.Date),
                               end = max(License_merged_clean$Start.Date),
                               min=min(License_merged_clean$Start.Date),max=max(License_merged_clean$Start.Date),startview = 'year')
                
              ),
              box(
                width=12,
                plotOutput("trans_plot1", height = 250)
              ),
              box(
                width=12,
                plotOutput("trans_plot2", height = 250)
              ),
              box(
                width=12,
                plotOutput("trans_plot3", height = 250)
              ),
              box(
                width=12,
                plotOutput("trans_plot4", height = 250)
              )
            )
    ),
    #-------------------------- Appendix Tab ---------------------------------
    tabItem(
      tabName = "Appendix",
      tags$h2(
        "Data Sources"
      ),
      tags$a(
        href = "https://data.cityofnewyork.us/Business/License-Applications/ptev-4hud",
        "data.cityofnewyork.us/Business/License-Applications", 
      ),br(),
      tags$a(
        href = "https://data.cityofnewyork.us/Business/Legally-Operating-Businesses/w7w3-xahh",
        "data.cityofnewyork.us/Business/Legally-Operating-Businesses", 
      ),br(),
      tags$a(
        href = "https://data.cityofnewyork.us/Health/COVID-19-Daily-Counts-of-Cases-Hospitalizations-an/rc75-m7u3",
        "data.cityofnewyork.us/Health/COVID-19-Daily-Counts-of-Cases-Hospitalizations-an", 
      ),br(),
      tags$a(
        href = "https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data",
        "data.cityofnewyork.us/Business/Zip-Code-Boundaries", 
      ),br(),
      tags$h2(
        "Contacts:"
      ),
      tags$p(
        "If you have any questions, please contact :"
      ),
      tags$p(
        "1. Arceneaux Luke (lpa2114@columbia.edu)"
      ),
      tags$p(
        "2. Ouyang Hongju (ho2307@columbia.edu)"
      ),
      tags$p(
        "3. Tao Ranran (rt2796@columbia.edu)"
      ),
      tags$p(
        "4. Xu Mingze (mx2269@columbia.edu)"
      ),
      tags$p(
        "5. Xue Fei (fx2189@columbia.edu)"
      ),
      tags$p(
        "6. Zhang Haoyu (hz2826@columbia.edu)"
      ),
      tags$h2(
        "GitHub Repository"
      ),
      tags$a(
        href = "https://github.com/TZstatsADS/ads-spring2023-project2-group5",
        "github.com/TZstatsADS/ads-spring2023-project2-group5"
      ),
      tags$h2(
        "FIPS code for boroughs"
      ),
      tags$a(
        href = "https://transition.fcc.gov/oet/info/maps/census/fips/fips.txt",
        "transition.fcc.gov/oet/info/maps/census/fips/fips.txt"
      ),
      tags$h2(
        "Shiny tutorial"
      ),
      tags$a(
        href = "http://tzstatsads.github.io/tutorials/wk3_Tutorial2.html",
        "tzstatsads.github.io/tutorials/wk3_Tutorial2.html"
      ),
      tags$h2(
        "Libraries used"
      ),
      tags$a(
        "leaflet,ggplot2,reshape2,dtplyr,dplyr,DT,lubridate,devtools,shiny,shinydashboard"
      )
#      fluidRow(
#        box(
#          width=12,
#          title = "Appendix",
#          textOutput("text")
#        )
#      )
    )
    
  )
)
)






############################### Server ##################################


server <- function(input, output) {
  
  #==============================Map plot ==================================
  output$Map_1 <- renderImage({
    #'D:/source/Stats 5243/',
    list(src = paste('../output/map/',input$Map_district,'_Before.png',sep=""),width='100%')
    
  },deleteFile=FALSE)
  
  output$Map_2 <- renderImage({
    #'D:/source/Stats 5243/',
    list(src = paste('../output/map/',input$Map_district,'_After.png',sep=""),width='100%')
    
  },deleteFile=FALSE)
  
  #============================= Functions for plot ==========================
  
  level <- seq(ymd("2014/01/01"),ymd("2023/02/01"),"months")
  levels <- paste(year(level),month(level),sep="-")
  
  plot_byDate <- function(business_type,application_type,borough,dateRange){
    
    License_merged_clean %>% 
      filter(License.Category %in% business_type) %>% #business category
      filter(Application.or.Renewal == application_type) %>%  #"Application" or "Renewal"
      filter(Address.Borough == borough) %>% #borough
      filter(Start.Date>=dateRange[1] & Start.Date<=dateRange[2]) %>% #date range
      group_by(Start.Date) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=Start.Date,y=count),levels=levels)+geom_col()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      xlab("Application Submission Date")+ylab("Number of Applications")+
      ggtitle("Number of Applications vs Submission Date")
    
  }
  
  plot_byMonth <- function(business_type,application_type,borough,dateRange){
    
    License_merged_clean %>% 
      filter(License.Category %in% business_type) %>% #business category
      filter(Application.or.Renewal == application_type) %>%  #"Application" or "Renewal"
      filter(Address.Borough == borough) %>% #borough
      filter(Start.Date>=dateRange[1] & Start.Date<=dateRange[2]) %>% #date range
      group_by(Start.Month) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=Start.Month,y=count))+geom_col()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      xlab("Application Submission Month")+ylab("Number of Applications")+
      ggtitle("Number of Applications vs Submission Month")
    
  }
  
  plot_ProcessTime <- function(business_type,application_type,borough,dateRange){
    
    License_merged_clean %>% 
      filter(License.Category %in% business_type) %>% #business category
      filter(Application.or.Renewal == application_type) %>%  #"Application" or "Renewal"
      filter(Address.Borough == borough) %>% #borough
      filter(Start.Date>=dateRange[1] & Start.Date<=dateRange[2]) %>% #date range
      group_by(Start.Month,Processing.time) %>% 
      ggplot(aes(x=Start.Month,y=Processing.time))+geom_point()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      xlab("Application Submission Month")+ylab("Processing Time of Applications")+
      ggtitle("Processing Time of Applications vs Submission Month")
    
  }
  
  plot_status <- function(business_type,application_type,borough,dateRange){
    
    table_plot <- License_merged_clean %>% 
      filter(Status != "Withdrawn") %>% #Withdrawn has very few cases, so drop withdrawn
      filter(License.Category %in% business_type) %>% #business category
      filter(Application.or.Renewal == application_type) %>%  #"Application" or "Renewal"
      filter(Address.Borough == borough) %>% #borough
      filter(Start.Date>=dateRange[1] & Start.Date<=dateRange[2]) %>% #date range
      group_by(End.year,Status) %>% 
      summarise(count = n()) %>% 
      mutate(prop = count/sum(count))
    
    return(ggplot(table_plot,aes(End.year,prop,fill=Status))+
             geom_col()+
             facet_wrap(~Status,scales = "free_y")+
             xlab("Year")+ylab("Application Status Proportion")+
             ggtitle("Application Status Proportion vs Year"))
           
    
  }
  
  
  
  #============================== Service plot ==================================
  output$service_plot1 <- renderPlot({
    if(input$business_type_service == "ALL"){
      plot_byDate(Service,input$application_type_service, input$borough_service,input$dateRange_service)
    }
    else{
      plot_byDate(input$business_type_service,input$application_type_service, input$borough_service,input$dateRange_service)
    }
    
  })
  output$service_plot2 <- renderPlot({
    if(input$business_type_service == "ALL"){
      plot_byMonth(Service,input$application_type_service, input$borough_service,input$dateRange_service)
    }
    else{
      plot_byMonth(input$business_type_service,input$application_type_service, input$borough_service,input$dateRange_service)

    }
  })
  output$service_plot3 <- renderPlot({
    if(input$business_type_service == "ALL"){
      plot_ProcessTime(Service,input$application_type_service, input$borough_service,input$dateRange_service)
    }
    else{
      plot_ProcessTime(input$business_type_service,input$application_type_service, input$borough_service,input$dateRange_service)
    }
  })
  output$service_plot4 <- renderPlot({
    if(input$business_type_service == "ALL"){
      plot_status(Service,input$application_type_service, input$borough_service,input$dateRange_service)
      
    }
    else{
      plot_status(input$business_type_service,input$application_type_service, input$borough_service,input$dateRange_service)
      
    }
  })
  
  #============================== Entertainment plot ==================================
  output$entertainment_plot1 <- renderPlot({
    if(input$business_type_entertain=="ALL"){
      plot_byDate(Entertainment,input$application_type_entertain,
                  input$borough_entertain,input$dateRange_entertain)
    }
    else{
      plot_byDate(input$business_type_entertain,input$application_type_entertain,
                  input$borough_entertain,input$dateRange_entertain)
    }
   
  })
  output$entertainment_plot2 <- renderPlot({
    if(input$business_type_entertain=="ALL"){
      plot_byMonth(Entertainment,input$application_type_entertain,
                   input$borough_entertain,input$dateRange_entertain)
    }
    else{
      plot_byMonth(input$business_type_entertain,input$application_type_entertain,
                   input$borough_entertain,input$dateRange_entertain)
    }
  })
  output$entertainment_plot3 <- renderPlot({
    if(input$business_type_entertain=="ALL"){
      plot_ProcessTime(Entertainment,input$application_type_entertain,
                       input$borough_entertain,input$dateRange_entertain)
    }
    else{
      plot_ProcessTime(input$business_type_entertain,input$application_type_entertain,
                       input$borough_entertain,input$dateRange_entertain)
    }
    
  })
  output$entertainment_plot4 <- renderPlot({
    if(input$business_type_entertain=="ALL"){
      plot_status(Entertainment,input$application_type_entertain,
                  input$borough_entertain,input$dateRange_entertain)
    }
    else{
      plot_status(input$business_type_entertain,input$application_type_entertain,
                  input$borough_entertain,input$dateRange_entertain)
    }
    
  })
  
  #============================== Retail plot ==================================
  output$retail_plot1 <- renderPlot({
    if(input$business_type_retail == "ALL"){
      plot_byDate(Retail,input$application_type_retail, input$borough_retail,input$dateRange_retail)
    }
    else{
      plot_byDate(input$business_type_retail,input$application_type_retail, input$borough_retail,input$dateRange_retail)
    }
  })
  output$retail_plot2 <- renderPlot({
    if(input$business_type_retail == "ALL"){
      plot_byMonth(Retail,input$application_type_retail, input$borough_retail,input$dateRange_retail)
    }
    else{
      plot_byMonth(input$business_type_retail,input$application_type_retail, input$borough_retail,input$dateRange_retail)
    }
  })
  output$retail_plot3 <- renderPlot({
    if(input$business_type_retail == "ALL"){
      plot_ProcessTime(Retail,input$application_type_retail, input$borough_retail,input$dateRange_retail)
    }
    else{
      plot_ProcessTime(input$business_type_retail,input$application_type_retail, input$borough_retail,input$dateRange_retail)
    }
  })
  output$retail_plot4 <- renderPlot({
    if(input$business_type_retail == "ALL"){
      plot_status(Retail,input$application_type_retail, input$borough_retail,input$dateRange_retail)
    }
    else{
      plot_status(input$business_type_retail,input$application_type_retail, input$borough_retail,input$dateRange_retail)
    }
  })
  
  #============================== Transportation plot ==================================
  output$trans_plot1 <- renderPlot({
    if(input$business_type_trans == "ALL"){
      plot_byDate(Trans,input$application_type_trans, input$borough_trans,input$dateRange_trans)
    }
    else{
      plot_byDate(input$business_type_trans,input$application_type_trans, input$borough_trans,input$dateRange_trans)
    }
  })
  
  output$trans_plot2 <- renderPlot({
    if(input$business_type_trans == "ALL"){
      plot_byMonth(Trans,input$application_type_trans, input$borough_trans,input$dateRange_trans)
    }
    else{
      plot_byMonth(input$business_type_trans,input$application_type_trans, input$borough_trans,input$dateRange_trans)
    }
  })
  output$trans_plot3 <- renderPlot({
    if(input$business_type_trans == "ALL"){
      plot_ProcessTime(Trans,input$application_type_trans, input$borough_trans,input$dateRange_trans)
    }
    else{
      plot_ProcessTime(input$business_type_trans,input$application_type_trans, input$borough_trans,input$dateRange_trans)
      
    }
  })
  output$trans_plot4 <- renderPlot({
    if(input$business_type_trans == "ALL"){
      plot_status(Trans,input$application_type_trans, input$borough_trans,input$dateRange_trans)
    }
    else{
      plot_status(input$business_type_trans,input$application_type_trans, input$borough_trans,input$dateRange_trans)
      
    }
  })
  
  #=============================== Appendix Text ==========================
  
#  output$text <- renderText({
#    paste("hello hello hellokwfiweafebfdfv rnfvdsjfsjakhewhgdsjkfnjsdkfdshfkj")
#  })
  
  
}

shinyApp(ui, server)


