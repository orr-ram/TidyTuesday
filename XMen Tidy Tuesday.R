#tidytuesdayR::tt_load(2020, week = 9)

characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')

xmen_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/xmen_bechdel.csv')

covers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/covers.csv')


# Which issues pass the test? ---------------------------------------------

library(tidyverse)

xmen_pass <- xmen_bechdel %>% 
  filter(pass_bechdel == 'yes')

# what happened to each character in these issues? and across all of these issues?

char_pass <- characters %>% 
  filter(issue %in% xmen_pass$issue)


# UpSet Plot --------------------------------------------------------------

# Created a new df containing binary numeric columns for each occurence of physical contact in each of the comics that passed the 
# Bechdel test

char_pass_upset <- char_pass %>% 
  select(c(character)) %>% 
  mutate(fighting = ifelse(is.na(char_pass$initiates_physical_conflict), 0, 1),
         killing = ifelse(char_pass$number_of_kills_humans >= 1 | char_pass$number_of_kills_non_humans >= 1, 1, 0),
         kissing = ifelse(is.na(char_pass$kiss_with_which_character), 0, 1),
         handholding = ifelse(is.na(char_pass$hand_holding_with_which_character), 0, 1),
         hugging = ifelse(is.na(char_pass$hugging_with_which_character), 0, 1))  %>% 
  as.data.frame()

library(UpSetR)

# Creating the UpSet plot:

upset(char_pass_upset, 
      group.by = 'character', 
      nsets = 6, 
      order.by = 'freq',
      decreasing = FALSE,
      mainbar.y.label = 'Joint Occurences',
      sets.x.label = 'Total Occurences',
      mb.ratio = c(0.7, 0.3),
      point.size = 5,
      line.size = 2,
      text.scale = c('intersection size title' = 2))

# Set the title using the 'grid.text()' function, because upset() has no argument for setting a title
      
grid::grid.text('Physical Contact in 
                   Bechdal-Passing X-Men Comics',
                x = 0.09, y = 0.93, gp=grid::gpar(fontsize=20, fontface = 'bold'))

# WordCloud! --------------------------------------------------------------

cloud_covers <- covers %>% 
  filter(!is.na(narrative_captions)) %>% 
  select(narrative_captions)

library(quanteda)

cover_corp <- corpus(cloud_covers$narrative_captions)

dtm <- dfm(cover_corp, 
           tolower = TRUE, remove_punct = TRUE,
           remove_numbers = TRUE, remove_symbols = TRUE,
           remove = c('<.*?->', 
                      '*-*', 'o', 'ms', '*th')) %>% 
             dfm_remove(stopwords('en'))

# word cloud:

library(wordcloud2)

set.seed(0.5)

frame <- colSums(dtm)

wordcloud2(data.frame(names(frame), frame),
           shape = 'circle',
           minRotation = -pi/6, 
           maxRotation = -pi/6, 
           rotateRatio=1)