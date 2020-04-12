library(jsonlite)
yelp <- stream_in(file("yelp_academic_dataset_business.json"))
head(yelp, 10)

yelp_flat <- flatten(yelp)
str(yelp_flat)



# Export a data frame
data_export <-iris
write_json(data_export, "iris.json")

install.packages("coronavirus")
devtools::install_github("covid19r/coronavirus")
library("coronavirus")
data("coronavirus")
head(coronavirus)

library(tidyr)

coronavirus %>% 
  filter(date == max(date)) %>%
  select(country = Country.Region, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)



library(dplyr)

summary_df <- coronavirus %>% group_by(Country.Region, type) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

summary_df %>% head(100) 

coronavirus::coronavirus
View(coronavirus)

x = filter(coronavirus, type == "recovered" , Country.Region == "India")
View(x)