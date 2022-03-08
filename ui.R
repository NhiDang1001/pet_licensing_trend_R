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
             tabPanel("Tab 2"),
             mainPanel(
               selectInput(inputId = "animal",
                           label = "Choose Animal",
                           list("Cat", "Dog")),
               plotlyOutput(outputId = "Chart1_Plot")
             ),
             tabPanel("Tab 3")
  )
)