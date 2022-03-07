# A function that takes in a dataset and returns a list of info about it:
library(dplyr)
library(lubridate)


seattle_pet_licenses <- read.csv('https://raw.githubusercontent.com/info-201b-wi22/exploratory-analysis-anhdang1/main/seattle_pet_licenses.csv?token=GHSAT0AAAAAABQJKIVXJSAQAY5BEM3XQUBOYQ3JKVA')


#The correlation between the number of pet licenses and zip code in Seattle: 
#Which area has the highest number of pets and the lowest?
high_zipcode <- seattle_pet_licenses %>%
  group_by(zip_code) %>%
  summarise(num_pet = n()) %>%
  filter(num_pet == max(num_pet)) %>%
  pull(zip_code)

low_zipcode <- seattle_pet_licenses %>%
  group_by(zip_code) %>%
  summarise(num_pet = n()) %>%
  filter(num_pet == min(num_pet)) %>%
  pull(zip_code)


#What are the  top 5 primary-breeds in dogs and top 5 primary-breeds in cats? Then determine preferred breeds

dog_breeds <- seattle_pet_licenses %>% group_by(primary_breed) %>% filter(species == "Dog") %>% 
  summarize(number_of_dogs = n()) %>% arrange(desc(number_of_dogs)) %>% slice(1:5) %>% pull(primary_breed)


cat_breeds <- seattle_pet_licenses %>% group_by(primary_breed) %>% filter(species == "Cat") %>% 
  summarize(number_of_cats = n()) %>% arrange(desc(number_of_cats)) %>% slice(1:5)%>% pull(primary_breed)

#The ratio of dogs and cats
dogs <- seattle_pet_licenses %>% group_by(species) %>% filter (species == "Dog") %>% summarize(Num_dogs = n())
cats <- seattle_pet_licenses %>% group_by(species) %>% filter (species == "Cat") %>% summarize(Num_cats = n())
table <- merge(x = dogs, y = cats, by = NULL)
table <- table %>% mutate(Ratio_of_Dogs_and_Cats = Num_dogs/Num_cats)
ratio_dog_cat <- table %>% pull(Ratio_of_Dogs_and_Cats)

#Big difference in 2014 and 2015
year_licensing_trend <- seattle_pet_licenses %>% group_by(year = year(license_issue_date)) %>% 
  summarize(number_of_licenses = n())

gap_year <- year_licensing_trend %>% filter(year == "2014" | year == "2015") %>%
  mutate(new_licenses = number_of_licenses - lag(number_of_licenses),na.rm=T) %>%
  filter(new_licenses>0) %>%
  pull(new_licenses)


