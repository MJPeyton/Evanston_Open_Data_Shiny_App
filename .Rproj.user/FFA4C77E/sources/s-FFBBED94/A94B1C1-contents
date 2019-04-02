library(tidyverse)
## Data Import

# Data: Steven Manson, Jonathan Schroeder, David Van Riper, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 13.0 [Database]. Minneapolis: University of Minnesota. 2018. http://doi.org/10.18128/D050.V13.0

# Map: https://commons.wikimedia.org/wiki/Category:Blank_maps_of_the_United_States#/media/File:Blank_US_Map_(states_only).svg

nhgis <- read.csv("census data/nhgis0003_ds233_20175_2017_place.csv")

## Population and Race

data_race <- nhgis %>%
  select(GISJOIN,
         AHY1E001, 
         AHY2E002,
         AHY2E003,
         AHY2E004,
         AHY2E005,
         AHY2E006,
         AHZAE012)

names(data_race) <- c(
  "GISJOIN",
  "Total_population",
  "White",
  "Black",
  "AmericanIndian",
  "Asian",
  "PacificIslander",
  "Hispanic")

data_race$White <- as.numeric(data_race$White)

data_race <- data_race %>%
  mutate("Per_White" = White / Total_population) %>%
  mutate("Per_Black" = Black / Total_population) %>%
  mutate("Per_AmIndian" = AmericanIndian / Total_population) %>%
  mutate("Per_Asian" = Asian / Total_population) %>%
  mutate("Per_Pac" = PacificIslander / Total_population) %>%
  mutate("Per_Hispanic" = Hispanic / Total_population)

data_race <- data_race %>%
  select(-c(3:8))

## Education

data_edu <- nhgis %>%
  select(GISJOIN,
         AHY1E001,
         AH04E017, 
         AH04E022,
         AH04E023,
         AH04E024,
         AH04E025)

names(data_edu) <- c(
  "GISJOIN",
  "Total_population",
  "HS",
  "BS",
  "MS",
  "Prof",
  "PhD")

data_edu <- data_edu %>%
  mutate("Per_HS" = HS/Total_population) %>%
  mutate("Per_BS" = BS/Total_population) %>%
  mutate("Per_MS" = MS/Total_population) %>%
  mutate("Per_Prof" = Prof/Total_population) %>%
  mutate("Per_PhD" = PhD/Total_population)

data_edu <- data_edu %>%
  select(-c(2:7))

## Household Income / Poverty

data_houseincome <- nhgis %>%
  select(GISJOIN, 
         AH1PE001,
         AH1JE001,
         AH1JM002,
         AH1JM003
  )

names(data_houseincome) <- c(
  "GISJOIN",
  "Median_Income",
  "Poverty_count",
  "Under .5",
  ".5 to .99")

data_houseincome <- data_houseincome %>%
  mutate("Below_Poverty" = rowSums(.[4:5])) %>%
  mutate("Poverty_Rate" = Below_Poverty / `Poverty_count`) %>%
  select(c("GISJOIN",
           "Median_Income",
           "Poverty_Rate"))


## Location Details

data_loc <- nhgis %>%
  select(GISJOIN,
         STATE,
         PLACE)

names(data_loc) <- c(
  "GISJOIN",
  "State",
  "Place")

## Combine all data subsets

data <- right_join(data_loc, data_race, by="GISJOIN") %>%
  right_join(data_edu, by="GISJOIN") %>%
  right_join(data_houseincome, by="GISJOIN")

data <- as_tibble(data)

data <- drop_na(data)

ev_data <- data %>%
  filter(GISJOIN == "G17024582")

noev_data <- data %>%
  filter(GISJOIN != "G17024582")

ev_data_bind <- rbind(ev_data, noev_data)

row_data <- ev_data_bind

## Select Data

selected_columns <- c("Total_population",
                      "Per_White",
                      "Per_Black",
                      "Per_Asian",
                      "Per_Hispanic",
                      "Per_HS",
                      "Per_BS",
                      "Per_Prof",
                      "Per_PhD",
                      "Poverty_Rate",
                      "Median_Income")

data_select <- row_data %>%
  select(selected_columns) %>%
  scale() %>%
  as.data.frame()

data_select_noscale <- row_data %>%
  select(selected_columns) %>%
  as.data.frame()

data_select <- data_select %>%
  mutate(GISJOIN = row_data$GISJOIN)


#Save Data

save(data_select, data_select_noscale, row_data, file = "data.RData")

