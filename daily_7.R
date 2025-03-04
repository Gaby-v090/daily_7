#Gabriela Valdez 
#02/18/2025
#

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read_csv(url)


--------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggthemes)

covid |>
  filter(date == max(date)) |>
  group_by(state) |>
  summarize(cases = sum(cases, na.rm = TRUE)) |>
  ungroup() |>
  slice_max(cases, n = 6) |>
  pull(state) ->
  top_states

covid |>
  filter(state %in% top_states) |>
  group_by(state, date) |>
  summarise(daily_cases = sum(cases, na.rm = TRUE), .groups = 'drop') |>
  # arrange(state, date) |>
  # group_by(state) |>
  # mutate(cumulative_cases = sum(daily_cases)) |>
  ggplot(aes(x = date, y= daily_cases, color = state)) +
  geom_line(linewidth = 2) +
  facet_wrap(~state) +
  ggthemes::theme_gdocs() +
  theme(legend.position = 'none') +
  labs(title = 'Cummulative Case Counts',
       subtitle = 'Data Source: NY-Times',
       x = 'Date',
       y = 'Cases',
       caption = 'Daily Exercise 07')
----------------------------------------------------------------------------------
  
  national.data <- covid |>
  group_by(date) |>
  summarize(cases = sum(cases, na.rm = TRUE))


ggplot(national.data, aes(x = date, y = cases)) +
  geom_col(fill = "yellow") +
  labs(title = 'Covid in the USA',
       x = 'Date',
       y= 'Cases') +
  ggthemes::theme_gdocs() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))