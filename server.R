library(ggplot2)
library(stringr)
library(pivottabler)
library(plotly)
library(tidyverse)
library(lubridate)
library(dplyr)




seattle_pet_licenses <- read.csv('https://raw.githubusercontent.com/info-201b-wi22/final-project-anhdang1/main/seattle_pet_licenses.csv?token=GHSAT0AAAAAABR5WKO56ZL6JPWWNHZGYMO6YRP2Q2A')

#Trend of licensed cats and dogs between 2005 - 2016
dogs_year <- seattle_pet_licenses %>%  filter (species == "Dog") %>% group_by(year = year(license_issue_date)) %>% summarize(Dogs = n())
cats_year <- seattle_pet_licenses %>%  filter (species == "Cat") %>% group_by(year = year(license_issue_date)) %>% summarize(Cats = n())

species_year <- merge(dogs_year,cats_year, by="year")

draft <- species_year %>%
  pivot_longer(!year, names_to = "Species",
               values_to = "Licensed_pet"
  )


#Trend of total pet, dog, cat licenses by Month
cat_by_month <- seattle_pet_licenses %>% filter(species == "Cat") %>% group_by(month = month(license_issue_date)) %>% summarize(Cats = n())
dog_by_month <- seattle_pet_licenses %>%  filter (species == "Dog") %>% group_by(month = month(license_issue_date)) %>% summarize(Dogs = n())
total_by_month <- seattle_pet_licenses %>% group_by(month = month(license_issue_date)) %>% summarize(Total_Pets = n())

species_month <- merge(cat_by_month,dog_by_month, by="month")

trend_data <- left_join(species_month, total_by_month, by = "month")

pivot_data <- pivot_longer(trend_data, 2:4, names_to = "Types", values_to = "number_of_pets")


hello <- ggplot(data = pivot_data) +
  geom_line(mapping= aes(x=month, y = number_of_pets, color = Types)) +
  labs(title = 'Trend of total pet, dog, cat licenses by Month', x='Month', y='Number of pets') 
ggplotly(hello)

#SELECT MONTH(license_issue_date), count(*)
#FROM seattle_pet_licenses
#WHERE license_number is not null AND species = 'Cat'
#GROUP BY MONTH(license_issue_date)
#ORDER BY COUNT(*);





#chart_script 


server <- function(input, output) {
  
  output$Trends <- renderRbokeh({
    
    # Allow user to filter by multiple year
    
    draft <- draft %>% filter(Species %in% input$user_category) %>% filter(year <= input$year[2],
                                                                           year >= input$year[1])
    
    figure(title = "Trend of licensed cats and dogs between 2005 - 2016") %>%
      ly_lines(year, Licensed_pet, data = draft, color = Species) %>%
      x_axis(label = "Year") %>%
      y_axis(label = "Licensed pet")

  })
  
  output$pet_comparisonPlot <- renderPlotly({
    pivot_data <- pivot_data %>% filter(Types %in% input$user_types) %>% filter(month <= input$month[2],
                                                                               month >= input$month[1])
    
    hello <- ggplot(data = pivot_data) +
      geom_line(mapping= aes(x=month, y = number_of_pets, color = Types)) +
      labs(title = 'Trend of total pet, dog, cat licenses by Month', x='Month', y='Number of pets') 
    ggplotly(hello)
    
  })
  

  
  
  output$Text_trends <- renderText({"This chart shows us how drastically the licenses of cats and dogs increased throughout 2005-2016 and the difference between cats and dogs.
    As we use this graph in our project, we can see that there is a drastic difference between the cats and dogs, but we also notice that during 2015 and
    2016 there was a spike in the data. Meaning for some reason more people started to get their pets licensed. We can also tell by the chart
    that as the years progresses, more and more pets are being licensed, which is a great thing."})
}



#Draft for gg plot
# ggplot(data = draft, aes(x = year, y = Licensed_pet, color = Species)) + 
#   # ggtitle("How the Data Progressed") +
#   geom_line() +
#   labs(title = "Trend of licensed cats and dogs between 2005 - 2016",
#        subtitle = "Data drawn from Seattle Animal Shelter - Brought to Kaggle.com by AaronSchlegel",
#        x = "Year",
#        y = "Licensed pet",
#        fill = "Species") +
#   # scale_fill_manual(values = custom_colors) +
#   theme(legend.position = "right",
#         legend.title = element_text(face = "bold"))

