library(dplyr)
library(shiny)
library(networkD3)
library(visNetwork)
library(igraph)

fuzzyPartyPanel <-  tabPanel("Fuzzy Party Search",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("fuzzyPartySelect", "Party", c('Conservatives', 'Labour', 'Liberal Democrats') , 
                                        selectize = TRUE, width = NULL, size = NULL)
                          ),
                          mainPanel(
                            dataTableOutput('fuzzyPartyTable')
                          )
                        ))

fuzzyPanel <-  tabPanel("Fuzzy Search",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("years", label = 'Year range ', min = 1980, max = 2016, value = c(1980,2011)),br()
                          ),
                          mainPanel(
                            dataTableOutput('fuzzyTable')
                          )
                        ))


exactSearchPanel <-  tabPanel("Exact Search",
                              sidebarLayout(
                                sidebarPanel(
                                  sliderInput("exactYears", label = 'Year range ', min = 1980, max = 2011, value = c(1980,2011)),
                                  br(),
                                  textInput("exactArg1", label = "Head", value='^.*$'),
                                  textInput("exactArg2", label = "Dependent",value='^.*$'),
                                  textInput("exactEither", label = "Either",value='^.*$'),
                                  actionButton("showExact", "Search"),
                                  p("$ - end of word"), br(),
                                  p("^ - start of word"), br(),
                                  p(". - any character"), br(),
                                  p("* - repeat"), br(),
                                  p("[] - any of"), br(),
                                  p("e.g.: ^[Uu]nivers.*"), br()
                                ),
                                mainPanel(
                                  dataTableOutput('exactTable')
                                )
                              ))

exactPartyPanel <-  tabPanel("Exact Party Search",
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("exactParty", "Party", c('Conservatives', 'Labour', 'Liberal Democrats') , 
                                              selectize = TRUE, width = NULL, size = NULL),
                                  br(),
                                  textInput("exactPartyArg1", label = "Head", value='^.*$'),
                                  textInput("exactPartyArg2", label = "Dependent",value='^.*$'),
                                  textInput("exactPartyEither", label = "Either",value='^.*$'),
                                  actionButton("showExactParty", "Search"),
                                  p("$ - end of word"), br(),
                                  p("^ - start of word"), br(),
                                  p(". - any character"), br(),
                                  p("* - repeat"), br(),
                                  p("[] - any of"), br(),
                                  p("e.g.: ^[Uu]nivers.*"), br()
                                ),
                                mainPanel(
                                  dataTableOutput('exactTableParty')
                                )
                              ))

networkVisPartyPanel <-  tabPanel("NetworkVisParty",
                             sidebarLayout(
                               sidebarPanel(
                                 selectInput("networkPartySelect", "Party", c('Conservatives', 'Labour', 'Liberal Democrats') , 
                                             selectize = TRUE, width = NULL, size = NULL),
                                 br(),
                                 textInput("networkArg1VisParty", label = "Head", value='^.*$'),
                                 textInput("networkArg2VisParty", label = "Dependent",value='^.*$'),
                                 textInput("networkEitherVisParty", label = "Either",value='^.*$'),
                                 actionButton("showNetParty", "Search"),
                                 sliderInput("scoreThresholdParty", label = 'Frequency threshold ', min = 0, max = 30, value = 2, step = 1),
                                 p("Green: Subject"), 
                                 p("Red : Object"), 
                                 p("Black : Conjunction"), 
                                 p("Yellow : Adjective ")
                               ),
                               mainPanel(
                                 visNetworkOutput('networkVisParty', width = '100%', height = '800')
                               )
                             )
)

networkVisPanel <-  tabPanel("Network Vis Year",
                             sidebarLayout(
                               sidebarPanel(
                                 sliderInput("networkYears", label = 'Year range ', min = 1980, max = 2011, value = c(1980,2011)),
                                 br(),
                                 textInput("networkArg1Vis", label = "Head", value='^.*$'),
                                 textInput("networkArg2Vis", label = "Dependent",value='^.*$'),
                                 textInput("networkEitherVis", label = "Either",value='^.*$'),
                                 actionButton("showNet", "Search"),
                                 sliderInput("scoreThreshold", label = 'Frequency threshold ', min = 0, max = 30, value = 2, step = 1),
                                 p("Green: Subject"), 
                                 p("Red : Object"), 
                                 p("Black : Conjunction"), 
                                 p("Yellow : Adjective ")
                               ),
                               mainPanel(
                                 visNetworkOutput('networkVis', width = '100%', height = '800')
                               )
                             )
)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Show a tabset that includes a plot, summary, and
  # table view of the generated distribution
  mainPanel(
    tabsetPanel(
      exactSearchPanel,
      fuzzyPanel,
      networkVisPanel,
      exactPartyPanel,
      fuzzyPartyPanel,
      networkVisPartyPanel
    ), width = 12
  )
))
