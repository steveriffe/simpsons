## Credit to Adam Spannbauer for the initial analysis (available at
## https://raw.githubusercontent.com/AdamSpannbauer/tidytext_learning/master/trump_and_tidytext.R)
## that I've lightly modified below to suit a more recent twitter dataset.

##install.packages(c("hunspell", "tidyverse", "stringr", "fivethirtyeight", "lubridate", "gridExtra"))
setwd("C:/Users/riffesm/Desktop/Port/data-society-the-simpsons-by-the-data")


library(tidytext)
library(hunspell)
library(tidyverse)
library(stringr)
library(fivethirtyeight)
library(lubridate)
library(caTools)

#data(trump_twitter, package = "fivethirtyeight")

simpsons_script <- read.csv("simpsons_script_lines.csv", header = TRUE, stringsAsFactors = FALSE)

# check out structure and date range ------------------------------------------------
str(simpsons_script)
#minDate <- min(date(trump_twitter$created_at))
#maxDate <- max(date(trump_twitter$created_at))

#trump_twitter$created_at <- as.POSIXct(trump_twitter$created_at, format = "%Y-%m-%d %H:%M:%S")
#trump_twitter$is_retweet <- as.factor(trump_twitter$is_retweet)
#trump_twitter$source <- as.factor(trump_twitter$source)


# create vectorised stemming function using hunspell ----------------------------------
my_hunspell_stem <- function(token) {
  stem_token <- hunspell_stem(token)[[1]]
  if (length(stem_token) == 0) return(token) else return(stem_token[1])
}
vec_hunspell_stem <- Vectorize(my_hunspell_stem, "token")


simpsons_norm_text <- as.data.frame(simpsons_script$normalized_text)
names(simpsons_norm_text) <- list("normalized_text")
simpsons_norm_text$normalized_text <- as.character(simpsons_norm_text)
simpsons_norm_text$id <- simpsons_script$id


simpsons_norm_text_full <- simpsons_norm_text

spl <- sample.split(simpsons_norm_text$normalized_text, .2)
simpsons_norm_text <- subset(simpsons_norm_text, spl == TRUE)


# clean text by tokenizing & rm urls/stopwords ----------------------------------
simpsons_tokens <- simpsons_norm_text  %>% 
  mutate(text = str_replace_all(normalized_text, 
                                pattern=regex("(www|https?[^\\s]+)"), 
                                replacement = "")) %>% #rm urls
  mutate(text = str_replace_all(normalized_text,
                                pattern = "[[:digit:]]",
                                replacement = "")) %>% 
  tidytext::unnest_tokens(tokens, normalized_text) %>% #tokenize #do I need to replace text with normalized_text?
  mutate(tokens = vec_hunspell_stem(tokens)) %>% 
  filter(!(tokens %in% stop_words$word)) #rm stopwords


# get most used words in trumps tweets -----------------------------------------------
most_used_words <- simpsons_tokens %>% 
  group_by(tokens) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  arrange(desc(n))

# sentiment analysis -------------------------------------------------------------
sentimentDf <- get_sentiments("afinn") %>% 
  mutate(word = vec_hunspell_stem(word)) %>% 
  bind_rows(get_sentiments("afinn"))
simpsons_sentiment <- simpsons_tokens %>% 
  inner_join(sentimentDf, by=c("tokens"="word")) %>% 
  rename(Score=score)

simpsons_full_text_sent <- simpsons_sentiment %>% 
  group_by(id) %>% 
  summarise(Score = sum(Score)) %>% 
  ungroup() %>% 
  inner_join(simpsons_norm_text, by="id")

write.csv(simpsons_full_text_sent, "simpsons_full_text_sent.csv", row.names = FALSE)
summary(simpsons_full_text_sent)

