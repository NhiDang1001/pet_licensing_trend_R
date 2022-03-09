library("shiny")
library("rbokeh")
library("ggplot2")
library("dplyr")
library("plotly")
library(tidyverse)
library(bslib)
library("rbokeh")
library(lubridate)



seattle_pet_licenses <- read.csv('https://raw.githubusercontent.com/info-201b-wi22/final-project-anhdang1/main/seattle_pet_licenses.csv?token=GHSAT0AAAAAABR5WKO56ZL6JPWWNHZGYMO6YRP2Q2A')
seattle_pet_licenses_year <- seattle_pet_licenses %>% group_by(year = year(license_issue_date))
count_range <- range(seattle_pet_licenses_year$year)

ui <- fluidPage(
  
  navbarPage("Seattle Pet Licenses",
             tabPanel(
               "Introduction",
               HTML('<a href = "https://raw.githubusercontent.com/info-201b-wi22/final-project-anhdang1/main/seattle_pet_licenses.csv?token=GHSAT0AAAAAABR5WKO56ZL6JPWWNHZGYMO6YRP2Q2A" >Here\'s a Direct Link to the Raw Data Set</a>'),
               fluidPage(theme = bs_theme(bootswatch = "minty"),
                         p("Welcome to Seattle Pet Licenses app!"),
                         mainPanel(
                           textOutput("Text01"),
                           textOutput("Value")
                         )
               )
             ),
             tabPanel("Trends", 
                      # Side bar layout
                      sidebarLayout(
                        sidebarPanel(
                          # Allow user to input range
                          selectInput(
                            inputId = "user_category",
                            label = "Select Species",
                            choices = draft$Species,
                            selected = "Dogs",
                            multiple = TRUE),
                          sliderInput("year", "Choose year range:",
                                      min = count_range[1],
                                      max = count_range[2],
                                      value = count_range,
                                      step = 1)
                        ),
                        # Main Panel
                        mainPanel(
                          textOutput("Text_trends"),
                          # display Bokeh output
                          rbokehOutput("Trends", width = "100%", height = "auto")
                        ) 
                      )),
             tabPanel("Month Trends",
                      # Side bar layout
                      sidebarLayout( 
                        sidebarPanel(
                          # Allow user to input range
                          selectInput(
                            inputId = "user_types", 
                            label = "Select Pet Types",
                            choices = pivot_data$Types,
                            selected = "Dogs",
                            multiple = TRUE),
                          sliderInput("month", label = "Month slider",
                                      min = min(pivot_data$month),
                                      max = max(pivot_data$month),
                                      value = c(3, 9),
                                      sep = "",
                                      step = 1)
                          
                        ),
                        # Main Panel3
                        mainPanel(
                          textOutput("Tab2_trends"),
                          # display Bokeh output3
                          plotlyOutput(outputId = "pet_comparisonPlot"),
                          
                        ) 
                      )),
            
             tabPanel("Zip Code Trends",
                      #Side Bar layout
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("substring_zip_code", label = "Zip Code Slider",
                                      min = min(as.numeric(zip_data$substring_zip_code)),
                                      max = max(as.numeric(zip_data$substring_zip_code)),
                                      value = c(9810, 98225),
                                      sep = "",
                                      step = 10)
                          
                        ),
                        mainPanel(
                          textOutput("Tab3_trends"),
                          # plotly output for chart
                          plotlyOutput(outputId = "zip_comparisonPlot"),
                          
                        ))
             ),
  )
)
