library(tidyverse)
library(ggplot2)
library(showtext)
library(ragg)
library(forcats)
library(ggtext)

circuits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/circuits.csv')
constructor_results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_results.csv')
constructor_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructor_standings.csv')
constructors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/constructors.csv')
driver_standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/driver_standings.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')
lap_times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/lap_times.csv')
pit_stops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/pit_stops.csv')
qualifying <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/qualifying.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')
results <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/results.csv')
seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/seasons.csv')
status <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/status.csv')

################## Important Files #############
#1. driver_standings
#2. Drivers
#3. results
#4. constructor
#5. constructor_standings

##--------- Objectives -------
#1. Try joining results,drivers and driver_standings on driverId
#2. Join constructor and constructor_standings on raceId

driver_results_df <- driver_standings %>% 
  left_join(races, by = "raceId") %>% 
  rename(driver_url = url) %>% 
  left_join(drivers, by = "driverId")

glimpse(driver_results_df)
  
drivers1 <- driver_results_df %>% 
  count(nationality) %>%
  mutate(name = fct_reorder(nationality, n)) 

ggplot(drivers1, aes(x=name, y= n)) +
  geom_col(fill = '#c75522') +
  coord_flip()+
  theme_classic()

########### number of winners by team #####

constructors_df <- constructor_standings %>% 
  left_join(constructors, by = "constructorId") 

########## bar plot for winners##########

winner <- constructors_df %>% 
  ## just use 2008 data
  dplyr::filter(position == '1') %>%
  ## turn into lumped factors with capitalized names
  dplyr::mutate(
    constructorRef = stringr::str_to_title(constructorRef),
    constructorRef = forcats::fct_lump(constructorRef, n = 10)
  ) %>% 
  ## add counts
  dplyr::count(constructorRef, sort = TRUE) %>% 
  ## order factor levels by number, put "Other" to end
  dplyr::mutate(
    constructorRef = forcats::fct_rev(forcats::fct_inorder(constructorRef)),
    constructorRef = forcats::fct_relevel(constructorRef, "Other", after = 0)
  )

winner

levels(winner$constructorRef) 

ggplot(winner, aes(x = n, y = constructorRef)) +
  ## draw bars
  geom_col(fill = "gray70") +
  ## change plot appearance
  theme_minimal()              

winner <- winner %>% 
  ## add percentage label with `sprintf()`
  dplyr::mutate(perc = paste0(sprintf("%4.1f", n / sum(n) * 100), "%"))

winner

ggplot(winner, aes(x = n, y = constructorRef)) +
  geom_col(fill = "gray70") +
  ## add percentage labels
  geom_text(aes(label = perc)) +
  theme_minimal()

ggplot(winner, aes(x = n, y = constructorRef)) +
  geom_col(fill = "gray70") +
  geom_text(
    aes(label = perc), 
    ## make labels left-aligned
    hjust = 1, nudge_x = -.5
  ) +
  theme_minimal()

## create color palette based on input data
pal <- c(
  "gray85",
  rep("gray70", length(winner$constructorRef) - 4), 
  "coral2", "mediumpurple1", "goldenrod1"
)

ggplot(winner, aes(x = n, y = constructorRef, 
                    fill = constructorRef)) +
  geom_col() +
  geom_text(
    aes(label = perc), 
    hjust = 1, nudge_x = -.5
  ) +
  ## add custom colors
  scale_fill_manual(values = pal, guide = "none") +
  theme_minimal()

winner <-
  winner %>% 
  mutate(
    color = case_when(
      row_number() == 1 ~ "goldenrod1",
      row_number() == 2 ~ "mediumpurple1",
      row_number() == 3 ~ "coral2",
      constructorRef == "Other" ~ "gray85",
      ## all others should be gray
      TRUE ~ "gray70"
    )
  )

ggplot(winner, aes(x = n, y = constructorRef, fill = color)) +
  geom_col() +
  geom_label(
    aes(label = perc), 
    hjust = 1, nudge_x = -.5,
    size = 4, fontface = "bold", family = "Fira Sans",
    ## turn into white box without outline
    fill = "white", label.size = 0
  ) +
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_identity(guide = "none") +
  theme_void() +
  theme(
    axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"),
    plot.margin = margin(rep(15, 4))
  )  +
  labs(title = "Percentage of formula one winners by Team ", x = " Names of Countries", y = "Count")
