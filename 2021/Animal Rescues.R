library(tidyverse)  
library(here)

animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

glimpse(animal_rescues)

data <- animal_rescues %>%
  mutate(animal_group_parent = recode(animal_group_parent,
                                      cat = "Cat",
                                      Budgie = "Bird",
                                      Pigeon = "Bird"),
         animal_group_parent = case_when(str_detect(animal_group_parent, "Unknown") ~ "Unknown", TRUE ~ animal_group_parent)) %>%
  mutate(across(where(is.character), factor))

glimpse(data)


data %>%
  count(animal_group_parent) %>%
  ggplot(aes(reorder(animal_group_parent, n), n, fill = animal_group_parent)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Frequently rescued animals", subtitle = "Animal Rescue London 2020", x = " Names of Animals", y = "Count") +
  coord_flip() +
  theme_classic()
  
