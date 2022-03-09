library(ggplot2)
library(stringr)
library(pivottabler)
library(plotly)
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)



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


#Licensed count of Dogs and Cat by Zipcode
cat_zip <- seattle_pet_licenses %>% filter(species == "Cat") %>% filter(zip_code >0 ) %>% group_by(zip_code) %>% summarize(Cats = n())
dog_zip <- seattle_pet_licenses %>%  filter (species == "Dog") %>% filter(zip_code >0 ) %>% group_by(zip_code) %>% summarize(Dogs = n())

species_zip <- merge(cat_zip,dog_zip, by="zip_code", na.rm=T)

zip_complete <- species_zip[!(is.na(species_zip$zip_code) & species_zip$zip_code==" "), ]


zip_data <- pivot_longer(zip_complete, 2:3, names_to = "Types", values_to = "number_of_pets")

zip_data$substring_zip_code = str_sub(zip_data$zip_code,1,5)




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
  
  output$zip_comparisonPlot <- renderPlotly({
    zip_data <- zip_data %>% filter(substring_zip_code <= as.numeric(input$substring_zip_code[2]),substring_zip_code >= as.numeric(input$substring_zip_code[1]))
    
    compare <- ggplot(data = zip_data) +
      geom_col(mapping= aes(x=substring_zip_code, y = number_of_pets, fill = Types), position = "dodge") +
      theme(axis.text.x = element_text(angle = 45))+
      labs(title = 'Licensed count of Dogs and Cat by Zip code', x='Zip Code', y='Number of pets') 
    ggplotly(compare)  })
  

  
  output$introduction<- renderText({"For our group, we decided to work with the pet licensing data, to inform Seattleites on pet ownership trends and encouraged the people of Seattle to adopt unlicensed pets at the animal shelters. 
    Our goal is to acknowledge people who have gotten their pets licensed in the City of Seattle and raise awareness of adopting pets, such as cats and dogs. 
    There are several benefits to licensing one's pet, including a return ride home if your pet is lost, and easier contact from a veterinarian if your pet is unfortunately injured. 
    One of the most significant impacts is that this could be an essential source to plan on marketing and outreach campaigns targeting cat and dog owners who haven't considered licensing their pets. 
    The major questions we are asking are, “What is the ratio between licensed cats and dogs?”, “When did people start to get their pets licensed?”, and “How has the population of licensed pets changed throughout the years?”.
    The data came from Seattle Animal Shelter, Aaron Schlegel then brought it to the Kaggle website. 
    When it comes to limitations to this dataset, there are null outcomes for license code variable, this means that it includes other types of animals besides dogs and cats. 
    On top of that, this is a subset of larger pet ownership, the records shows that there are 65% dog and 35% cat in Seattle. However, we do not know if this is broadly representative of pet ownership in Seattle. 
    More verification is needed here."})
  
  output$Text_trends <- renderText({"This chart shows us how drastically the licenses of cats and dogs increased throughout 2005-2016 and the difference between cats and dogs.
    As we use this graph in our project, we can see that there is a drastic difference between the cats and dogs, but we also notice that during 2015 and
    2016 there was a spike in the data. Meaning for some reason more people started to get their pets licensed. We can also tell by the chart
    that as the years progresses, more and more pets are being licensed, which is a great thing."})
  
  
  output$Tab2_trends <- renderText({"This chart shows the changes in pet licensing amounts over the course of the year, broken down by months. 
    Looking at the three categories displayed in the chart (Dogs, Cats, and Total Pets), a visible pattern corresponding to each included 
    variable comes into perspective. They all appear to follow an upwards trend, experiencing two slight dip, with the second dip being 
    slightly larger and finally ending in an increase in the final month, December. Looking at the data more closely, it would appear that 
    the greatest increase appears between the months of September and December, with a 169% increase, hence the upwards trend in the data. 
    A decrease of 53% is seen between the months of July and September. Summer seems to be the least popular season for license, versus 
    Autumn which is the most popular, indicating a potential seasonality trend. September contains the greatest dip in the data which might 
    indicate that people return some of their pets after taking care of them in the summer or even potentially fostering animals, and adopt 
    in the winter once again."})
  
  output$Tab3_trends <- renderText({"Looking at this data visualization, a couple of trends begin to appear. First of all, it seems like 
    across all zipcodes, dog licenses are more popular than cat licenses with the highest dog value being 3818 pets, and the highest cat 
    zipcode value being 2171, which is surprising since this is around a 1,600 pet difference. The data appears to be multimodial between 
    the two variables, Dog and Cat. The two modes center around the zipcodes 98103, and the second mode at 98115 which might indicate geographic 
    association with pet licencing values since both dog and cat values spike at those locations. People might live in areas that are more 
    accepting of animals, or palces which let animals live within housing. On the other hand, looking at the lowest points, these might 
    resonate with areas which are not accepting of pets, or a coincidence regarding the people who wish not to own pets, but that would 
    take more research to reach a proper conclusion on the definite trends."})
  
  output$conclusion <- renderText({"One of the main take away from the three graphs is seeing how the number of people licensing their pets has drastically increased throughout the years. 
  This indicates that as the years goes on, the amount of pets getting licensed has gone up. Which is a great thing because it is beneficial for the owners and the pets themselves. 
  Another takeaway is  seeing the difference between how many cats get licensed and how many dogs get licensed. 
  When seeing this, we can assume that there are not as many cat owners as there are dog owners, or that there are less people who get their pets licensed if their pet is a cat. 
  The final takeaway is seeing which year the number licensed pets increased the most for both pets and the visualization of how there is a huge gap between licensed cats and dogs during each year. 
  For the exploration analysis, we were able to figure out the correlation between the number of pet licenses with the zipcodes in Seattle, which zipcode has the highest and lowest amount of licensed pets. 
  We were able to figure out the top 5 primary-breeds in dogs and cats. With all of this information, we were able to answer most of our questions and gather more information to teach other people about the 
  licensed pets data and encourage the people who still havent licensed their pets, to do so, or maybe in the future when they do adopt their pets they would remember to get them licensed."})
}





