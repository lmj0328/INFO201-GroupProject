library(shinythemes)
library(shiny)
library(markdown)

source("../Script/ReadData.R")


shinyUI(tagList(
  navbarPage(theme = shinytheme("sandstone"),
    "Washington Guide",
    tabPanel(
      "Introduction",
      h1("edited later")
    ),

    #Mengjiao's Panel
    tabPanel(
      "Statistic of Income Tax",

      tabsetPanel(

        #tab1 introduction
        tabPanel(
          "Introduction",
          includeMarkdown("../Script/TaxMarkDown.Rmd")
        ),

        #tab2 compare selected counties
        tabPanel(
          "Compare Selected Counties Aross Year",
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
            actionButton("UncheckCounty", label = "Check/Uncheck County", class = "btn-primary")

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
          "Explore Your Interested Data Across Counties",
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
      "Statistics of Monthly Housing payment",
      tabsetPanel(
        #tabPanel 1
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
            plotOutput("distPlot")
          )
        ),
        #tabPanel2
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
            plotOutput("trendPlot")
          )

        )
      )

    ),

    tabPanel(
      "Washington Poverty Data (Jim)",
  ##    sidebarPanel(

  ##    ),

      mainPanel(
        tabsetPanel(
          tabPanel(
            "Intro",
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

            


            plotOutput("povertyPlot", width = "100%"))
        )
      )
      )
    )
    
  # tabPanel(
  #     "Conclusion",
  #     h5(
  #       "Our project would be useful to anyone who are thinking about moving to another county in Washington.
  #       They are able to estimate their adjusted gross income and see which county would be a fit for them which they are
  #       able to also select their desire population. Some people would rather live somewhere where there’s not a lot of
  #       people and others might like to live somewhere with a lot of people (urban vs suburbs). After they are able to see
  #       which county may be a possible fit for them, they can also check housing affordability in that county.
  #       When moving, people often plan for the future and their family. Budgeting is a big part of moving to a new place
  #       and it would be beneficial for users to have an idea of how much rent would cost by the desire bedrooms in
  #       the areas they are looking to move to.  Lastly, poverty rate can give very basic insight into school, living
  #       condition and quality of life in a certain area. These trends usually correlate to lower qualities of life with
  #       high poverty rates / low income neighborhoods. High poverty rates are also likely to influence lower cost of
  #       land/property.  Although our project only measure for poverty rate and doesn’t take in other factors to
  #       account for measuring quality of life, new homeowners or anyone who wants to move may take this into
  #       consideration for the county economic progression throughout the years."
  #     )
  #   )
  )
)
