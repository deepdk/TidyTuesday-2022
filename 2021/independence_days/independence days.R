#setup -

library(tidyverse)

library(lubridate)

library(plotly)

library(here)

#import data 
holidays <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv')

glimpse(holidays) 
View(holidays)

holidays_tidy <- holidays %>%
select(-event_commemorated_and_notes) %>%
mutate(across(where(is.character), factor))

#exploratory data analysts #which countries have more than one independence day?


holidays_tidy %>%
count(country, sort= TRUE)

 
holidays_tidy  %>%
  count(date_of_holiday, sort= TRUE)


  
holidays_tidy %>%
  count(weekday, sort = TRUE)


holidays_tidy %>%
  count(year, sort => TRUE)

#what countries established independence in 19687

holidays_tidy %>%
filter(year == 1960)
count(country)


holidays_tidy %>%
filter(year == 1960) %>%
count(independence_from, sort = TRUE)



holidays_tidy %>%
count(independence_from, sort = TRUE)

# create a bar graph

holidays_tidy %>%

ggplot(aes (x = independence_from, n=10)) +
geom_bar() + 
coord_flip()

df <- holidays[!is.na(holidays$independence_from), ]
glimpse(df)
data <- df %>%
  mutate(independence_from = recode(independence_from,
                                    "Russian Soviet Federative Socialist Republic" = "Soviet Union",
                                    "Russian Soviet Federative Socialist Republic and German Empire" = "Soviet Union",
                                    "Spanish Empire" = "Spain",
                                    "United Kingdom of Great Britain" = "United Kingdom",
                                    "Kingdom of Great Britain" = "United Kingdom",
                                    "United Kingdom of Great Britain and Ireland" = "United Kingdom",
                                    "United Kingdom of Portugal, Brazil and the Algarves" = "Portugal",
                                    "United Kingdom and the British Mandate for Palestine" = "United Kingdom",
                                    "SFR Yugoslavia" = "Yugoslavia",
                                    "Socialist Federal Republic of Yugoslavia" = "Yugoslavia",
                                    "Empire of Japan and France" = "Empire of Japan",
                                    "Spanish Empire[72]" = "Spain",
                                    ),
         holidays = case_when(str_detect(independence_from, "NA") ~ "NA", TRUE ~ independence_from)) %>%
  mutate(across(where(is.character), factor))

glimpse(data)

data %>%
  count(independence_from) %>%
  ggplot(aes(reorder(independence_from, n), n, fill = independence_from)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Country that has ruled the world ", subtitle = "Intrenational Independence Days ", x = " Names of Countries", y = "Count") +
  coord_flip() +
  theme_classic()  
