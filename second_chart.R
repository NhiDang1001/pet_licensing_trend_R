seattle_pet_licenses <- read.csv('https://raw.githubusercontent.com/info-201b-wi22/final-project-anhdang1/main/seattle_pet_licenses.csv?token=GHSAT0AAAAAABR5WKO56ZL6JPWWNHZGYMO6YRP2Q2A')
library(ggplot2)
library(stringr)
library(pivottabler)
library("plotly")
library(tidyverse)



seattle_pets_licenses_subset <- subset(seattle_pet_licenses, species!="Livestock") %>% group_by(year = year(license_issue_date)) %>% group_by(species) %>% summarize(number_of_licenses = n())
year <- str_sub(seattle_pets_licenses_subset$license_issue_date, 1, 4)

dogs_year <- seattle_pet_licenses %>%  filter (species == "Dog") %>% group_by(year = year(license_issue_date)) %>% summarize(Dogs = n())
cats_year <- seattle_pet_licenses %>%  filter (species == "Cat") %>% group_by(year = year(license_issue_date)) %>% summarize(Cats = n())

species_year <- merge(dogs_year,cats_year, by="year")

draft <- species_year %>%
  pivot_longer(!year, names_to = "Species",
               values_to = "Licensed_pet"
  )
ggplot(data = draft, aes(x = year, y = Licensed_pet, color = Species)) + 
  # ggtitle("How the Data Progressed") +
  geom_line() +
  labs(title = "Trend of licensed cats and dogs between 2005 - 2016",
       subtitle = "Data drawn from Seattle Animal Shelter - Brought to Kaggle.com by AaronSchlegel",
       x = "Year",
       y = "Licensed pet",
       fill = "Species") +
    # scale_fill_manual(values = custom_colors) +
    theme(legend.position = "right",
          legend.title = element_text(face = "bold"))
#chart_script 
#This chart shows us how drastically the licenses of cats and dogs increased throughout 2005-2016 and the difference between cats and dogs. 
#As we use this graph in our project, we can see that there is a drastic difference between the cats and dogs, but we also notice that during 2015 and
#2016 there was a spike in the data. Meaning for some reason more people started to get their pets licensed. We can also tell by the chart 
#that as the years progresses, more and more pets are being licensed, which is a great thing. 
ggplot(data = seattle_pets_licenses) + 
  ggtitle("How the Data Progressed") +
  geom_col(mapping = aes(x = year, y = species, fill = species)) +
  facet_wrap(~species)
