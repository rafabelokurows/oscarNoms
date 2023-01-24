#https://www.r-bloggers.com/2020/04/scrape-html-table-using-rvest/
#https://www.boxofficemojo.com/year/world/2022/

library(tidyverse)
library(rvest)
content <- read_html("https://en.wikipedia.org/wiki/List_of_highest-grossing_films_in_the_United_States_and_Canada")
tables <- content %>% html_table(fill = TRUE)
library(ggplot2)
first_table <- tables[[1]]
first_table <- first_table[-1,]
library(janitor)
first_table <- first_table %>% clean_names()
first_table %>% 
  mutate(lifetime_gross = parse_number(lifetime_gross)) %>% 
  arrange(desc(lifetime_gross)) %>% 
  head(20) %>% 
  mutate(title = fct_reorder(title, lifetime_gross)) %>% 
  ggplot() + geom_bar(aes(y = title, x = lifetime_gross), stat = "identity", fill = "blue") +
  labs(title = "Top 20 Grossing movies in US and Canada",
       caption = "Data Source: Wikipedia ")


content <- read_html("https://www.boxofficemojo.com/year/world/2022/")
tables <- content %>% html_table(fill = TRUE)
tables

