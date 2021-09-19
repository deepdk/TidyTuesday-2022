library(tidyverse)
library(png)
library(patchwork)

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv', na ="NULL")

glimpse(scoobydoo)

tidyscooby <- scoobydoo %>%
  separate_rows(monster_type, sep = ",", convert = TRUE) %>%
  drop_na(monster_type, motive) %>%
  filter(monster_type != "") %>%
  mutate(monster_type = str_trim(monster_type),
         monster_type = recode(monster_type,
                               Possessed = "Possessed Object",
                               Disguise = "Disguised"),
         across(where(is.character), factor))

my_image <- readPNG("G:/scooby.png", native = TRUE)


tidyscooby %>%
  group_by(motive) %>%
  summarise(count = n()) %>%
  ggplot(aes(reorder(motive, count), count)) +
  geom_col() +
  labs(title = "Scooby Dooby Doooo ", subtitle = "Most common motives in scooby doo episodes ", x = " Types of motives", y = "Count") +
  coord_flip() +  
  theme_classic()+
  inset_element(p = my_image,
                left = 0.95,
                bottom = 0.35,
                right = 0.5,
                top = 0.85)
ggp_image  
  
  



