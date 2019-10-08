##Benjamin Hartmann

library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(tidyr)
library(topicmodels)
library(textdata)
library(tidyverse)
library(SnowballC)

##Small subsample of enron data for practice
library(translateR)
data(enron)

emails <- enron %>%
  select(email) %>%
  unnest_tokens(word, email)

emails <- emails %>%
  anti_join(stop_words)

emails %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in emails")

senti_email <- inner_join(emails, get_sentiments("nrc")) %>%
  count(sentiment)
senti_email$percent = (senti_email$n/sum(senti_email$n))*100

#Plotting the sentiment summary 
ggplot(senti_email, aes(sentiment, percent)) +   
  geom_bar(aes(fill = sentiment), position = 'dodge', stat = 'identity')+ 
  ggtitle("Sentiment analysis based on lexicon: 'NRC'")+
  coord_flip() +
  theme(legend.position = 'none', plot.title = element_text(size=18, face = 'bold'),
        axis.text=element_text(size=16),
        axis.title=element_text(size=14,face="bold"))