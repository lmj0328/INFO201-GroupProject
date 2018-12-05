library(shinythemes)
library(shiny)
library(markdown)
library(plotly)

source("Script/ReadData.R")


shinyUI(tagList(
  navbarPage(theme = shinytheme("yeti"),
    "Washington Guide",
    tabPanel(
      "Introduction",
      includeMarkdown("Script/IntroductionPage.Rmd")
    ),

    #Mengjiao's Panel
    tabPanel(
      "Statistic of Income Tax",
      tabsetPanel(
        #tab1 introduction
        tabPanel(
          "Introduction",
          includeMarkdown("Script/TaxMarkDown.Rmd")
        ),

        #tab2 compare selected counties
        tabPanel(
          "Compare Selected Counties Aross Year",
          h4("Chose your interested counties to see the trend for past years."),
          # sidebar panel
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
          # main panel
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
          "Explore Your Interested Data Across Counties",
          h4("Put in your interested salary level and year to see how the tax level differ between different counties in washington state."),
          # side bar panel
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
          # main panel
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
    
    # Jennifer's Code
    tabPanel(
      "Statistics of Monthly Housing payment",
      tabsetPanel(
       
         #tab1 introduction
        tabPanel(
          "Introduction",
          includeMarkdown("Script/HousingIntroduction.Rmd")
        ),
        
        #tabPanel 2
        tabPanel(
          "Monthly Rent in Each County by Year",
          sidebarPanel(
            width = 350,
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
            plotlyOutput("distPlot")
          )
        ),
        #tabPanel3
        tabPanel(
          "Monthly Rent Trends by County",
          sidebarPanel(
            width = 350,
            ## SELECTBOX FOR COUNTY
            selectInput("countySelect",
                        label = "Select interested county",
                        choices = list ("Adams County", "Asotin County",
                                        "Benton County", "Chelan County",
                                        "Clallam County", "Clark County",
                                        "Columbia County", "Cowlitz County",
                                        "Douglas County", "Ferry County",
                                        "Franklin County", "Garfield County",
                                        "Grant County", "Grays Harbor County",
                                        "Island County", "Jefferson County",
                                        "King County", "Kitsap County",
                                        "Kittitas County", "Klickitat County",
                                        "Lewis County", "Lincoln County",
                                        "Mason County", "Okanogan County",
                                        "Pacific County", "Pend Oreille County",
                                        "Pierce County", "San Juan County",
                                        "Skagit County", "Skamania County",
                                        "Snohomish County", "Spokane County",
                                        "Stevens County", "Thurston County",
                                        "Wahkiakum County", "Walla Walla County",
                                        "Whatcom County", "Whitman County",
                                        "Yakima County")),

            selectInput("bdrmSelect2",
                        label = "Select interested type of unit",
                        choices = list ("0 bedroom" = "0_bedroom",
                                        "1 bedroom" = "1_bedroom",
                                        "2 bedroom" = "2_bedroom",
                                        "3 bedroom" = "3_bedroom",
                                        "4 bedroom" = "4_bedroom"))

          ),

          mainPanel(
            plotlyOutput("trendPlot")
          )

        )
      )

    ),
    
    
    # Jim's Code
    tabPanel(
      "Washington Poverty Data",
      ##    sidebarPanel(
      
      ##    ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Introduction",
            includeMarkdown("Script/Poverty.Rmd")
          ),
          tabPanel(
            "Data Table",
            h3("Poverty Data for Washington From 2012-2016"),
            h5("Filter by County or Year in Table 'Search' Box"),
            tabPanel("Text View", DT::dataTableOutput("povertyTable"))
            
          ),
          
          tabPanel(
            "Map Plot",
            selectInput("selectPovYear", label = h3("Select Your Interested Year"),
                        choices = list("2012" = 2012,
                                       "2013" = 2013,
                                       "2014" = 2014,
                                       "2015" = 2015,
                                       "2016" = 2016),
                        ##"All" = "All"),
                        selected = 2012),
            plotOutput("yearPOV",  width = "100%")
          ),
          
          tabPanel(
            "Histogram Plot",
            selectInput("selectBarYear", label = h3("Select Your Interested Year"),
                        choices = list("2012" = 2012,
                                       "2013" = 2013,
                                       "2014" = 2014,
                                       "2015" = 2015,
                                       "2016" = 2016),
                        ##"All" = "All"),
                        selected = 2012),
            plotOutput("povertyPlot", width = "100%")
          )
        )
      )
    ),
  
      #tab5 Conclusion
      tabPanel(
        "Conclusion",
        includeMarkdown("Script/Conclusion.Rmd")
      )
    )
  )
)
