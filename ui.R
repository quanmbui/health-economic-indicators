#ui for assignment 9

library(shiny)
library(markdown)

shinyUI(navbarPage("",
                   tabPanel("Explore Global Data Summaries",
                            titlePanel("Exploring Global Health and Economic Indicators Over Time"),
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Use the select options below to explore data on life expectancy, infant mortality, and GDP from around the world over time."),
                                selectInput("country1", "Choose your country:", as.character(GDP[,1]), selected = "United States", multiple = FALSE, selectize = FALSE),
                                sliderInput("year1", "Choose a year to to view health indicators over time:", min= 1961, max = 2011, value = 2000, step = 1),
                                helpText("Data shown has been sourced from the World Bank."),
                                helpText("Authors: Anjani Kapadia and Quan Bui")   
                              ),
                              mainPanel(
                      
                                  plotOutput("plotGDP"),
                                  plotOutput("plotLifeExp"),
                                  plotOutput("plotInfMort") 
                                  
                                
                              )
                            )
                   ),
                   tabPanel("Explore Data Relationships",
                            titlePanel("Finding Relationships Between Health and Economic Indicators"),
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Use the select options below to view the relationship between life expectancy, GDP, and infant mortality from around the world in your selected year."),
                                sliderInput("year2", "Choose a year to to view health indicators over time:", min= 1981, max = 2011, value = 2000, step = 1),
                                selectInput("country2", "Choose a country:", countries, selected = "United States", multiple = FALSE, selectize = FALSE),
                                checkboxInput("comparecheck2", "Check this box if you would like to use options below to select and display individualized data.", value = FALSE),
                                sliderInput("GDPinput2", "Compare a GDP:", min=100, max = 60000, value = NULL, step = 1),
                                sliderInput("LE2", "Compare a life expectancy:", min=48, max=85, value = NULL, step = 1),
                                helpText("Note: Your selected country will be indicated by the red cross bars. The results of your selected data will appear as an orange triangle on the plot."),
                                helpText("Data shown has been sourced from the World Bank."),
                                helpText("Authors: Anjani Kapadia and Quan Bui")
                              ),
                              mainPanel(
                                plotOutput("Scatter2011"),
                                br(),
                                textOutput("ScatterText2"),
                                br(),
                                textOutput("ScatterText")
                                
                               
                              )
                            )
                   ),
                   tabPanel("Explore Data Based Predictions",
                            titlePanel("Predicting Income Level Based on Health Indicators"),
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Based on data from the year 2011, predict your hypothetical country's income level based on a given life expectancy and infant mortality rate categorization. "),
                                sliderInput("LifeExpinput", "Choose a life expectancy:", min=45, max = 85, value = 50, step = 1),
                                checkboxInput("InfMortCheck", "Low infant mortality rate country", value = FALSE),
                                helpText("Note: A low infant mortality rate country is defined as a country with an infant mortality rate of less than 27 deaths per 1,000 live births."),
                                helpText("Data shown has been sourced from the World Bank."),
                                helpText("Authors: Anjani Kapadia and Quan Bui")
                              ),
                              mainPanel(
                                plotOutput("multivariate"),
                                textOutput("multivariatetext2"),
                                br(),
                                textOutput("multivariatetext3"),
                                br(),
                                textOutput("multivariatetext")
                              )    
                            )
                            
                   )
))


