seattle_pet_licenses <- read.csv('https://raw.githubusercontent.com/info-201b-wi22/final-project-anhdang1/main/seattle_pet_licenses.csv?token=GHSAT0AAAAAABR5WKO56ZL6JPWWNHZGYMO6YRP2Q2A')
library("dplyr")
library("ggplot2")
dogs <- seattle_pet_licenses %>% group_by(species) %>% filter (species == "Dog") %>% summarize(Num_dogs = n())
cats <- seattle_pet_licenses %>% group_by(species) %>% filter (species == "Cat") %>% summarize(Num_cats = n())
table <- merge(x = dogs, y = cats, by = NULL)
table <- table %>% mutate(Ratio_of_Dogs_and_Cats = Num_dogs/Num_cats)

Chart_table <- seattle_pet_licenses %>% group_by(species) %>% summarize(Num_animal = n())
df_for_Chart <- Chart_table[-c(3), ]
ggplot(data = df_for_Chart) +
  geom_col(mapping = aes(x = species, y = Num_animal, fill = species)) + coord_flip()

#My chart aims to visually represent the ratio of Dogs and Cats. One bar, labeled at 'Cat' is the number of cats in the data set, and the 
#bar labeled as 'Dog' does the same, but for dogs. Looking at the chart, it can be seen that there is a much higher ratio of Dogs to Cats,
#and this is also seen in the table created above which contains the numerical value for the ratio, being ~1.879817. I included the chart 
#because although a number might be able to convey the difference between the species, actually being able to see it is highly effective
#and helpful for interpretation.