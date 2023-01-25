library(tidyverse)
library(rvest)
library(ggplot2)
library(janitor)

# content <- read_html("https://en.wikipedia.org/wiki/List_of_highest-grossing_films_in_the_United_States_and_Canada")
# tables <- content %>% html_table(fill = TRUE)
# 
# first_table <- tables[[1]]
# first_table <- first_table[-1,]
# 
# first_table <- first_table %>% clean_names()
# first_table %>% 
#   mutate(lifetime_gross = parse_number(lifetime_gross)) %>% 
#   arrange(desc(lifetime_gross)) %>% 
#   head(20) %>% 
#   mutate(title = fct_reorder(title, lifetime_gross)) %>% 
#   ggplot() + geom_bar(aes(y = title, x = lifetime_gross), stat = "identity", fill = "blue") +
#   labs(title = "Top 20 Grossing movies in US and Canada",
#        caption = "Data Source: Wikipedia ")

movies=data.frame()
years=c(1990:2022)
for (year in years){
  print(year)
  url=paste0("https://www.boxofficemojo.com/year/",year,"/?grossesOption=totalGrosses")
  tableYear <- read_html(url) %>% html_table(fill = TRUE)
  toAdd = tableYear[[1]] %>% mutate(year=year)
  print(paste0("movies:",nrow(toAdd)))
  movies=bind_rows(movies,toAdd)
  
}

df2 = data.frame(wrong=c("The Godfather: Part III",
                         "Precious",
                         "Good Night, and Good Luck."),
                 right=c("The Godfather Part III",
                         "Precious: Based on the Novel 'Push' by Sapphire",
                         "Good Night, and Good Luck"))
movies = movies %>% 
  mutate(Release = ifelse(Release %in% df2$wrong,
                          df2$right[match(Release, df2$wrong)],
                          Release)) %>% 
  mutate(Release = case_when(Release == "The Postman" & year == 1995~"The Postman (Il Postino)",
                             TRUE~Release)) %>% 
  mutate(nominated = case_when(Release %in% c(nominees$film)~1,TRUE~0),
         nominated = case_when((Release =="Beauty and the Beast" & year == 2017)|
                               (Release =="Crash" & year == 1996)|
                               (Release =="Gladiator" & year == 1992)|
                                 (Release =="Les Misérables" & year %in% c(2020,1998))|
                                    (Release =="Little Women" & year %in% c(1994))~0,TRUE~nominated )
         ) 


#ver quantos filmes tinham homônimos
movies %>% filter(Release %in% c(movies %>% filter(nominated ==1 ) %>% count(Release) %>% filter(n>1) %>% pull(Release))) %>% arrange(Release)
nominees %>% filter(!film %in% movies$Release)

movies %>% filter(year == 2022)



movies2=data.frame()
years=c(1990:2022)
for (year in years){
  print(year)
  url=paste0("https://www.boxofficemojo.com/year/",year,"/?grossesOption=calendarGrosses")
  tableYear <- read_html(url) %>% html_table(fill = TRUE)
  toAdd = tableYear[[1]] %>% mutate(year=year)
  print(paste0("movies:",nrow(toAdd)))
  movies2=bind_rows(movies2,toAdd)
  
}
movies %>% filter(str_detect(Release,"Avatar"))




pageNominees <- read_html("https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture")
tables <- pageNominees %>% html_table(fill = TRUE)
nominees=data.frame()
for (i in 9:12){
  toAdd = tables[[i]] %>% janitor::clean_names() %>% 
    mutate(year=as.double(substring(year_of_film_release,1,4))) %>% 
    filter(!is.na(film)) %>% select(year,film)
  nominees = bind_rows(nominees,toAdd)
}

nominees %>% filter(!film %in% c(movies$Release))

movies %>% filter(str_dete)


movies %>% filter(Release %in% nominees$film) %>% count(year) %>% rename(movies=n) %>%  bind_cols(
nominees %>% count(year) %>% rename(nominees=n) %>% select(2) ) %>% janitor::adorn_totals("row")
nominees %>% filter(!film %in% c(movies$Release))


movies %>% filter(str_detect(tolower(Release),"good night"))



# 
# update_release_column <- function(df, df2) {
#   df %>% 
#     mutate(Release = ifelse(Release %in% df2$wrong,
#                             df2$right[match(Release, df2$wrong)],
#                             Release))
# }


#obter ID IMDB
#consultar Worldwide Gross:
#ex.: https://www.boxofficemojo.com/title/tt0110912/?ref_=bo_cso_table_2
#obter ratings, runtime, PG-rating, mês de lançamento
