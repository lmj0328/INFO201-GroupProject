library(shinythemes)
library(shiny)

source("../Script/ReadData.R")


shinyUI(tagList(
  navbarPage(
    theme = "paper",
    "Washington Guide",
    
    #Mengjiao's Panel
    tabPanel(
      "Statistic of Income Tax (Mengjiao)",
      
      tabsetPanel(
        
        #tab1 introduction
        tabPanel(
          "Introduction",
          h3("A Brief Description of Tax Statistics"),
          h5("Description")
        ),
        
        #tab2 compare selected counties
        tabPanel(
          "Explore Selected Counties Across Years",
          h4("Chose your interested counties to see the trend for past years."),
          sidebarPanel(
            conditionalPanel(
              'input.subpanels2 === "Bar Plot"',
              selectInput(
                "selectDataset2", 
                label = h3("Select Your Interested Data"),
                choices = list("Number of Returns" = "N1",
                               "Taxable Income Amount" = "A04800",
                               "Salaries and Wages Amount" = "A00200"),
                selected = "N1"
              )
            ),
            
            #SELECTBOX FOR SALARY
            selectInput("selectSalary2", label = h3("Select Adjusted Gross Income"),
                        choices = list("$1 under $10,000" = 2,
                                       "$10,000 under $25,000" = 3,
                                       "$25,000 under $50,000" = 4,
                                       "$50,000 under $75,000" = 5,
                                       "$75,000 under $100,000" = 6,
                                       "$100,000 under $200,000" = 7,
                                       "$200,000 or more" = 8),
                        selected = 2),
            
            #CHECKBOX FOR COUNTY
            checkboxGroupInput("selectCounty", label = h3("Select County"), 
                               choices = ListOfCounties$COUNTYNAME,
                               selected = ListOfCounties$COUNTYNAME),
            actionButton("UncheckCounty", label = "Check/Uncheck County")
            
          ),
          
          mainPanel(
            tabsetPanel(
              id = 'subpanels2',
              tabPanel(
                "Bar Plot",
                plotOutput("yearBarPlot", width = "100%")),
              tabPanel(
                "View Data",
                DT::dataTableOutput("chartTable2"))
            )
          )
        ),
        
        #tab3 compare different counties
        tabPanel(
          "Compare Your Interested Data Across Counties",
          h4("Put in your interested salary level and year to see how the tax level differ between different counties in washington state."),
          sidebarPanel(
            # SELECTBOX FOR DATASET
            conditionalPanel(
              'input.subpanels === "Washington State Map"',
              selectInput(
                "selectDataset", 
                label = h3("Select Your Interested Data"),
                choices = list("Number of Returns" = "N1",
                               "Taxable Income Amount" = "A04800",
                               "Salaries and Wages Amount" = "A00200"),
                selected = "N1"
              )
            ),
            
            # SELECTBOX FOR SALARY
            selectInput("selectSalary", label = h3("Select Adjusted Gross Income"),
                        choices = list("$1 under $10,000" = 2,
                                       "$10,000 under $25,000" = 3,
                                       "$25,000 under $50,000" = 4,
                                       "$50,000 under $75,000" = 5,
                                       "$75,000 under $100,000" = 6,
                                       "$100,000 under $200,000" = 7,
                                       "$200,000 or more" = 8),
                        selected = 2),
            
            # SELECTBOX FOR YEAR
            selectInput("selectYear", label = h3("Select Year"), 
                        choices = list("2012" = 2012, 
                                       "2013" = 2013, 
                                       "2014" = 2014, 
                                       "2015" = 2015, 
                                       "2016" = 2016), 
                        selected = 2012),
            
            # SELECTBOX FOR RANGE
            conditionalPanel(
              'input.subpanels === "Washington State Map"',
              sliderInput("rangeSlider", label = h3("Enter Range"), min = 0,
                          max = 250000, value = c(0, 250000)
              )
            )
          ),
          
          mainPanel(
            tabsetPanel(
              id = 'subpanels',
              tabPanel(
                "Washington State Map", 
                plotOutput("yearSalary",  width = "100%"),
                textOutput("chosedDataset"),
                textOutput("chosedSalary"),
                textOutput("chosedYear"),
                textOutput("chosedRange")
              ),
              tabPanel(
                "View Data",
                DT::dataTableOutput("chartTable")
              )
            )
          )
        )
      )  
    ),   
    
    tabPanel(
      "Statistics of Monthly Housing payment (Cindy & Jennifer)", 
      sidebarPanel(
        ## SELECTBOX FOR BEDROOM
        selectInput("bdrmSelect",
                    label = "Select interested type of unit",
                    choices = list ("0 bedroom" = "0_bedroom", 
                                    "1 bedroom" = "1_bedroom",
                                    "2 bedroom" = "2_bedroom",
                                    "3 bedroom" = "3_bedroom",
                                    "4 bedroom" = "4_bedroom")),
        ## SELECTBOX FOR YEAR
        selectInput("yearSelect",
                    label = "Select interested year",
                    choices = list ("2011" = 2011,
                                    "2012" = 2012,
                                    "2013" = 2013,
                                    "2014" = 2014,
                                    "2015" = 2015,
                                    "2016" = 2016)
                    
        )
      ),
      
      mainPanel(
        plotOutput("distPlot")
      )
    ),
    
    
    tabPanel(
      "Jim", 
      sidebarPanel(
        # copy&paste your code here
      ),
      
      mainPanel(
        # copy&paste your code here
      )
    )
  )
))
