seattle_pet_licenses <- read.csv('https://raw.githubusercontent.com/info-201b-wi22/final-project-anhdang1/main/seattle_pet_licenses.csv?token=GHSAT0AAAAAABR5WKO56ZL6JPWWNHZGYMO6YRP2Q2A')
library(ggplot2)
library(dplyr)
library(lubridate)

year_licensing_trend <- seattle_pet_licenses %>% group_by(year = year(license_issue_date)) %>% 
  summarize(number_of_licenses = n())


ggplot(data = year_licensing_trend) + ggtitle("Pet licensing trends over the year")+
  geom_line(mapping = aes(x = year, y = number_of_licenses, color = number_of_licenses))

#Chart_Script
#Why: This chart shows the grouped total license count by year from 2005-2016 to see the overall trend of pet licensing over the years.
#What: We can see that there is a sharp increase between 2014-2015 with 19,123 more pets being licensed. However, we can also clearly see that
#there is a limitation of the dataset on this chart. The number of licenses from 2005 till 2010 is extremely low (less than 10), which could mean
#that the numbers was not collected effectively and accurately. Only after 2014, we can see a large number of pet licenses and it keeps increasing
#over the years. 