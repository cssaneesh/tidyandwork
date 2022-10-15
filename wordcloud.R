library(googlesheets4)
library(dplyr)
library(wordcloud)
library(RColorBrewer)

gs4_auth()
path <- ('https://docs.google.com/spreadsheets/d/1ac8CuAQdRNXp9MjKsG7YWiHcT64tRgnCqlY9UhX-jEo/edit?usp=sharing')
test <- read_sheet (path)

head(test,3)

test1 <- data.frame(test %>% select(topic) %>% count(topic) %>% mutate(count= n*10))
head(test1,3)
max(test1$count)

set.seed(123)

wordcloud(words = test1$topic, 
          freq = test1$count,
          min.freq = 10,
          max.words = 50,
          colors = brewer.pal(7, 'BrBG'))

# export the file as .pdf
