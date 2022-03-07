seattle_pet_licenses <- read.csv('https://raw.githubusercontent.com/info-201b-wi22/exploratory-analysis-anhdang1/main/seattle_pet_licenses.csv?token=GHSAT0AAAAAABQJKIVXJSAQAY5BEM3XQUBOYQ3JKVA')
library(ggplot2)
library(stringr)

seattle_pets_licenses <- subset(seattle_pet_licenses, species!="Livestock")
year <- str_sub(seattle_pets_licenses$license_issue_date, 1, 4)

ggplot(data = seattle_pets_licenses) + 
  ggtitle("How the Data Progressed") +
  geom_col(mapping = aes(x = year, y = species, fill = species)) +
  facet_wrap(~species)
#chart_script 
#This chart shows us how drastically the licenses of cats and dogs increased throughout 2005-2016 and the difference between cats and dogs. 
#As we use this graph in our project, we can see that there is a drastic difference between the cats and dogs, but we also notice that during 2015 and
#2016 there was a spike in the data. Meaning for some reason more people started to get their pets licensed. We can also tell by the chart 
#that as the years progresses, more and more pets are being licensed, which is a great thing. 
