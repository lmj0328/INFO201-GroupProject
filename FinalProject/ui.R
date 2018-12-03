library(shinythemes)
library(shiny)

source("../Script/ReadData.R")

shinyUI(tagList(
  navbarPage(
    theme = "paper",
    "Washington Guide",
    tabPanel(
      "Statistic of Income Tax (Mengjiao)",
      sidebarPanel(
        # selectInput("selectCounty", label = h3("Select County"),
        #             choices = ListOfCounties,
        #             selected = 1),
  
        selectInput("selectSalary", label = h3("Select Your Current Salary"),
                    choices = list("$1 under $10,000" = 2,
                                   "$10,000 under $25,000" = 3,
                                   "$25,000 under $50,000" = 4,
                                   "$50,000 under $75,000" = 5,
                                   "$75,000 under $100,000" = 6,
                                   "$100,000 under $200,000" = 7,
                                   "$200,000 or more" = 8),
                    selected = 2),
        
        selectInput("selectYear", label = h3("Select Your Interested Year"), 
                    choices = list("2012" = 2012, 
                                   "2013" = 2013, 
                                   "2014" = 2014, 
                                   "2015" = 2015, 
                                   "2016" = 2016), 
                    selected = 2012)
        
        # selectInput("selectTax", label = h3("Select Your Interested Data"),
        #             choices = list("Number of Returns" = "N1",
        #                            "Income Tax Amount" = "A06500",
        #                            "Taxable Income Amount" = "A04800",
        #                            "Tax Due at Time of Filing" = "A11901",
        #                            "Salaries and Wages Amount" = "A00200"),
        #             selected = "N1")
        
      ),

      mainPanel(
        tabsetPanel(
          tabPanel(
            "Intro",
            h3("A Brief Description of Tax Statistics"),
            h5("Description")
          ),
          
          tabPanel(
            "Map Plot", 
            plotOutput("yearSalary",  width = "100%")
          ),
          
          tabPanel("Histogram Plot", "This panel is intentionally left blank")
        )
      )
    ),   
    
    tabPanel(
      "Cindy & Jennifer", 
      sidebarPanel(
        # copy&paste your code here
      ),
      
      mainPanel(
        # copy&paste your code here
      )
    ),
    
    tabPanel(
      "Washington Poverty Data (Jim)", 
      sidebarPanel(
        selectInput("selectPovYear", label = h3("Select Your Interested Year"), 
                    choices = list("2012" = 2012, 
                                   "2013" = 2013, 
                                   "2014" = 2014, 
                                   "2015" = 2015, 
                                   "2016" = 2016), 
                    selected = 2012)
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Intro",
            h3("Poverty Data for Washington"),
            h5("Description")
          ),
          
          tabPanel(
            "Map Plot", 
            plotOutput("yearPOV",  width = "100%")
          ),
          
          tabPanel("Histogram Plot", "This panel is intentionally left blank")
        )
      )
      )
    )
  )
)
