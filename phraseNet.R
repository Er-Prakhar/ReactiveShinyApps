library(tidytext)
library(dplyr)
library(tidyr)
library(readr)
library(RColorBrewer)
library(ggplot2)


# text <- read_lines("romeo.txt")

phraseNet <- function(text, connectors, title = NULL){
  textFrame <- tibble(text = paste(text, collapse = " "))
  tidy_frame3 <- textFrame %>% 
    unnest_tokens(word, text, token = "ngrams", n = 3)
  # tidy_frame3
  tidy_frame_sep <- tidy_frame3 %>% 
    separate(word, c("word1", "word2", "word3"), sep = " ")
  
  #SELECT SEPARATION WORDS HERE: now "is"/"are"
  tidy_frame_filtered <- tidy_frame_sep %>%
    filter(word2 %in% connectors) %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word3 %in% stop_words$word)
  # tidy_frame_filtered
  
  edges <- tidy_frame_filtered %>% 
    count(word1, word3, sort = TRUE) %>%
    rename(from = word1, to = word3, width = n) %>%
    mutate(arrows = "to", id = row_number())
  
  right_words <- edges %>% count(word = to, wt = width)
  left_words <- edges %>% count(word = from, wt = width)
  
  #Computing node sizes and in/out degrees, colors.
  nodes <- left_words %>% full_join(right_words, by = "word") %>%
    replace_na(list(n.x = 0, n.y = 0)) %>%
    mutate(n.total = n.x + n.y,
           n.out = n.x - n.y,
           id = word,
           font.size = 40) %>%
    mutate(color = brewer.pal(9, "Blues")[cut_interval(n.out, 9)]) %>%
    rename(label = word, value = n.total)
  
  # # FILTERING edges with no further connections - can be commented
  # edges <- edges %>%
  #   left_join(nodes, c("from" = "id")) %>%
  #   left_join(nodes, c("to" = "id")) %>%
  #   filter(value.x > 1 | value.y > 1) %>%
  #   select(from, to, width, arrows, id)
  
  nodes <- nodes %>% filter(id %in% edges$from | id %in% edges$to )
  
  # visNetwork(nodes, edges, main = list(
  #   text = title,
  #   style = "font-size: 30px; font-family: verdana; color: green; text-align: center; font-weight: bold"
  # )
  # )
  list(nodes = nodes, edges = edges)
}


# phraseNet(text, c("is", "are"))


