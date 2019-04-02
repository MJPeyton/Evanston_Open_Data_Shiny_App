library(tidyverse)
library(ggplot2)
library(scales)
library(svglite)
library(distances)
library(ggrepel)

custom_font <- "Avenir Next"

theme_nu <- function () {
  custom_font <- "Avenir Next"
  theme_minimal() %+replace%
    theme(
      plot.background = element_rect(),
      title = element_text(color = "black", family = custom_font),
      plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0, margin = margin(10, 0, 0, 0)),
      plot.subtitle = element_text(size = rel(1.2), hjust = 0, margin = margin(10,0,10,0)),
      axis.title = element_text (color = "gray30"),
      axis.text = element_text(size = rel(0.8), color = "black", family = custom_font),
      axis.text.x = element_text(margin = margin(10,0,10,0)),
      legend.title = element_text(color = "black", family = custom_font),
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "left",
      legend.margin = margin(6, 6, 6, 6),
      legend.box.background = element_rect(),
      legend.box.margin = margin(6, 6, 6, 6),
      plot.caption = element_text(color = "gray40", size=rel(.8), hjust = 0, margin = margin(10,0,0,0)),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_line(color = "#E0E0E0", size = .2),
      panel.grid.minor = element_blank(),
      panel.background=element_blank()
    )
}

theme_set(theme_nu())

## Load Data

load("data.RData")

## Distance between Evanston and other rows (Distances package)

distance_weights <- c(100, 2, 2, 1, 1, 1, 1, 2, 2, 4, 10)

distances <- distances(data_select, id_variable = "GISJOIN", normalize = "studentize", weights = distance_weights)

## Note: These distances are pretty arbitrary, but after some playing around I got results that were close to what I wanted. I felt the need to introduce weights because of the exagerated effect that each percentage of race and education was having.

## Create Distance Table

distance_table <- data_select_noscale %>%
  mutate(distances = distances[1]) %>%
  mutate(State = row_data$State, Place = row_data$Place) %>%
  mutate(GISJOIN = row_data$GISJOIN)

distance_table <- as_tibble(distance_table)

top <- distance_table %>%
  top_n(6, wt=desc(distances)) %>%
  arrange(distances)

evanston <- data %>%
  filter(GISJOIN == "G17024582")

## Visualizations

## Visualize Distances

distance_table %>%
  ggplot(aes(distances)) +
  geom_density() +
  scale_x_continuous(limits = c(0, 15))  

## Charts for poster

top %>%
  ggplot(aes(Place, Total_population)) +
  geom_bar(stat = "identity")

#Poverty Rate
top %>%
  ggplot(aes(Place, Poverty_Rate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1)) +
  geom_text(aes(label = Poverty_Rate), hjust=-0.1, family=custom_font)

#Median Income
top %>%
  ggplot(aes(Place, Median_Income)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = Median_Income), hjust=-0.1, family=custom_font)

# Highest Education Level - HS
top %>%
  ggplot(aes(Place, Per_HS)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .25)) +
  geom_text(aes(label = Per_HS), hjust=-0.1, family=custom_font)

# Highest Education Level - BS

top %>%
  ggplot(aes(Place, Per_BS)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .25))  +
  geom_text(aes(label = Per_BS), hjust=-0.1, family=custom_font)

# Highest Education Level - Prof

top %>%
  ggplot(aes(Place, Per_Prof)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .25))  +
  geom_text(aes(label = Per_Prof), hjust=-0.1, family=custom_font)

## Explanation of Professional degrees: https://www.census.gov/topics/education/educational-attainment/about.html

# Highest Education Level - PhD

top %>%
  ggplot(aes(Place, Per_PhD)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .25))  +
  geom_text(aes(label = Per_PhD), hjust=-0.1, family=custom_font)

# Race - White

top %>%
  ggplot(aes(Place, Per_White)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .8))  +
geom_text(aes(label = Per_White), hjust=-0.1, family=custom_font)

# Race - Black

top %>%
  ggplot(aes(Place, Per_Black)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .8))  +
  geom_text(aes(label = Per_Black), hjust=-0.1, family=custom_font)

# Race - Asian

top %>%
  ggplot(aes(Place, Per_Asian)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .8))  +
  geom_text(aes(label = Per_Asian), hjust=-0.1, family=custom_font)

# Race - Hispanic

top %>%
  ggplot(aes(Place, Per_Hispanic)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(limits = c(0, .8))  +
  geom_text(aes(label = Per_Hispanic), hjust=-0.1, family=custom_font)
