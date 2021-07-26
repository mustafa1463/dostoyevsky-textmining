#Importing the libraries

library(pdftools)
library(dplyr)
library(tidytext)
library(readtext)
library(tm)
library(ggplot2)
library(tidyr)
library(reshape2)
library(wordcloud2)
library(stringr)
data(stop_words)

#Import the books

book_1 <- pdftools::pdf_text("https://planetpdf.com/planetpdf/pdfs/free_ebooks/Crime_and_Punishment_NT.pdf")
book_2 <- pdftools::pdf_text("http://www.planetpublish.com/wp-content/uploads/2011/11/Notes_from_the_Underground_NT.pdf")
book_3 <- pdftools::pdf_text("https://planetpdf.com/planetpdf/pdfs/free_ebooks/The_Brothers_Karamazov_T.pdf")
book_4 <- pdftools::pdf_text("https://www.holybooks.com/wp-content/uploads/The-House-of-the-Dead.pdf")
book_5 <- pdftools::pdf_text("https://planetpdf.com/planetpdf/pdfs/free_ebooks/The_Idiot_NT.pdf")
book_6 <- pdftools::pdf_text("https://onemorelibrary.com/index.php/en/?option=com_djclassifieds&format=raw&view=download&task=download&fid=11294")
afinn <- get_sentiments("afinn")

#Functions

raw_tidyer <- function(x){
  y <- x %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>%
    count(word, sort = TRUE)
  return(y)
}

frequency_plot <- function(data, number_of_plots = 20){
  occurence <- as.numeric(data[number_of_plots,2])
  
  y <- data %>% 
    filter(n > occurence) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word, fill = word)) +
    geom_col(show.legend = FALSE) +
    labs(x = "Frequency", y = "Word", title = "Frequently Used Words") +
    theme(panel.background = element_rect(color = "black", fill = "black"),
          plot.background = element_rect(fill = 'black', colour = 'black'),
          plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0), colour = "white"), 
          axis.title.y = element_text(margin = margin(r = 15), size = 20, colour = "white"),
          axis.title.x = element_text(margin = margin(t = 15), size = 20, colour = "white"),
          plot.caption = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(colour = "white", size = 12, face="bold"),
          axis.text.y = element_text(colour = "white", size = 12, face="bold" ),
          axis.line = element_line(color="white", size = 1.5),
          axis.ticks = element_blank())

    return(y)
}


sentiment_analyzer <- function(data){
  dt <- data %>% 
    inner_join(afinn)
  
  dt$weight <- dt$value * dt$n
  dt <- dt[order(dt$weight), ]
  return(dt)
  
}

negative_plot <- function(data){
  graph <- ggplot(data[1:20,], aes(weight, reorder(word, -weight), fill = word)) + 
    geom_col(show.legend = FALSE) +
    labs(x = "Sentiment Value", y = "Word", title = "Sentiment Analysis") +
    theme(panel.background = element_rect(color = "black", fill = "black"),
          plot.background = element_rect(fill = 'black', colour = 'black'),
          plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0), colour = "white"), 
          axis.title.y = element_text(margin = margin(r = 15), size = 20, colour = "white"),
          axis.title.x = element_text(margin = margin(t = 15), size = 20, colour = "white"),
          plot.caption = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(colour = "white", size = 12, face="bold"),
          axis.text.y = element_text(colour = "white", size = 12, face="bold" ),
          axis.line = element_line(color="white", size = 1.5),
          axis.ticks = element_blank())
  return(graph)
  
}

positive_plot <- function(data){
  data <- tail(data,20)
  data <- data[order(data$weight), ]
  graph <- ggplot(data, aes(weight, reorder(word, weight), fill = word)) + 
    geom_col(show.legend = FALSE) +
    labs(x = "Sentiment Value", y = "Word", title = "Sentiment Analysis") +
    theme(panel.background = element_rect(color = "black", fill = "black"),
          plot.background = element_rect(fill = 'black', colour = 'black'),
          plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0), colour = "white"), 
          axis.title.y = element_text(margin = margin(r = 15), size = 20, colour = "white"),
          axis.title.x = element_text(margin = margin(t = 15), size = 20, colour = "white"),
          plot.caption = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(colour = "white", size = 12, face="bold"),
          axis.text.y = element_text(colour = "white", size = 12, face="bold" ),
          axis.line = element_line(color="white", size = 1.5),
          axis.ticks = element_blank())
  return(graph)
  
}

sentiment_summary <- function(sentimented_data){
  name_of_book <- sentimented_data[1,3]
  sentiment_graph <- sentimented_data %>%
    inner_join(get_sentiments("bing")) %>% 
    group_by(sentiment) %>%
    slice_max(n, n = 10) %>% 
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(x = "Frequency", y = "Word", title = paste("A Sentiment Analysis of", name_of_book)) + 
    theme(panel.background = element_rect(color = "black", fill = "black"),
          plot.background = element_rect(fill = 'black', colour = 'black'),
          plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0), colour = "white"), 
          axis.title.y = element_text(margin = margin(r = 15), size = 20, colour = "white"),
          axis.title.x = element_text(margin = margin(t = 15), size = 20, colour = "white"),
          plot.caption = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(colour = "white", size = 12, face="bold"),
          axis.text.y = element_text(colour = "white", size = 12, face="bold" ),
          axis.line = element_line(color="white", size = 1.5),
          strip.text.x = element_text(size = 12, color = "white", face = "bold"),
          strip.text.y = element_text(size = 12, color = "white", face = "bold"),
          strip.background = element_rect(color = "black", fill = "black"),
          axis.ticks = element_blank())
  
  return(sentiment_graph)
}
  
  

story_analyzer <- function(sentimented_data){
  sum <- sum(sentimented_data$weight)
  book_name <- sentimented_data[1,3]
  if(sum >= 0){
    paste(book_name, "is a", "positive book", ", with a score of", sum)
    
  }else{
    paste(book_name, "is a", "negative book", ", with a score of", sum)
  }
}

timeline_story <- function(tibble_data){
  x <- tibble_data %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("afinn"))
  
  y <- nrow(x)
  
  data <- x %>% 
    mutate(number = 1:y) %>% 
    group_by(line) %>% 
    summarize(summ = sum(value))
  
  graph <- ggplot(data, aes(x = line, y = summ, fill = summ)) + geom_col() + labs(x = "time", y = "sentiment")
  return(graph)
}




#Turn from raw files to tibbles

crime_and_punishment_raw <- tibble(line = 1:length(book_1), text = book_1)
notes_from_underground_raw <- tibble(line = 1:length(book_2), text = book_2)
brothers_karamazov_raw <- tibble(line = 1:length(book_3), text = book_3)
house_of_dead_raw <- tibble(line = 1:length(book_4), text = book_4)
idiot_raw <- tibble(line = 1:length(book_5), text = book_5)
white_nights_raw <- tibble(line = 1:length(book_6), text = book_6)

#Turn all the tibbles into unnested tokens

crime_and_punishment <- raw_tidyer(crime_and_punishment_raw)
notes_from_underground <- raw_tidyer(notes_from_underground_raw)
brothers_karamazov <- raw_tidyer(brothers_karamazov_raw)
house_of_dead <- raw_tidyer(house_of_dead_raw)
idiot <- raw_tidyer(idiot_raw)
white_nights <- raw_tidyer(white_nights_raw)

#Inspect the books' most used words & make a books list.

crime_and_punishment
notes_from_underground
brothers_karamazov
house_of_dead
idiot
white_nights

#Inspect them with visualisation

frequency_plot(crime_and_punishment)
frequency_plot(notes_from_underground)
frequency_plot(brothers_karamazov)
frequency_plot(house_of_dead)
frequency_plot(idiot)
frequency_plot(white_nights)

#Label the dataframes with their book names to merge later.

crime_and_punishment$book <- "Crime and Punishment"
notes_from_underground$book <- "Notes from the Underground" 
brothers_karamazov$book <- "The Brothers Karamazov"
house_of_dead$book <- "House of the Dead"
idiot$book <- "The Idiot"
white_nights$book <- "White Nights"

dostoyevsky_books <- rbind(crime_and_punishment, notes_from_underground, brothers_karamazov, 
                           house_of_dead, idiot, white_nights)

#A sentiment analysis with their weighted frequences (i.e by weight = negativity/positivity value * number of occurences)

sentiment_crime <- sentiment_analyzer(crime_and_punishment)
sentiment_underground <- sentiment_analyzer(notes_from_underground)
sentiment_karamazov <- sentiment_analyzer(brothers_karamazov)
sentiment_dead <- sentiment_analyzer(house_of_dead)
sentiment_idiot <- sentiment_analyzer(idiot)
sentiment_nights <- sentiment_analyzer(white_nights)

story_analyzer(sentiment_crime)
story_analyzer(sentiment_underground)
story_analyzer(sentiment_karamazov)
story_analyzer(sentiment_dead)
story_analyzer(sentiment_idiot)
story_analyzer(sentiment_nights)

negative_crime_plot <- negative_plot(sentiment_crime)
negative_underground_plot <- negative_plot(sentiment_underground)
negative_karamazov_plot <- negative_plot(sentiment_karamazov)
negative_dead_plot <- negative_plot(sentiment_dead)
negative_idiot_plot <- negative_plot(sentiment_idiot)
negative_nights_plot <- negative_plot(sentiment_nights)

negative_crime_plot
negative_underground_plot 
negative_karamazov_plot
negative_dead_plot
negative_idiot_plot
negative_nights_plot

positive_crime_plot <- positive_plot(sentiment_crime)
positive_underground_plot <- positive_plot(sentiment_underground)
positive_karamazov_plot <- positive_plot(sentiment_karamazov)
positive_dead_plot <- positive_plot(sentiment_dead)
positive_idiot_plot <- positive_plot(sentiment_idiot)
positive_nights_plot <- positive_plot(sentiment_nights)

positive_crime_plot
positive_underground_plot
positive_karamazov_plot
positive_dead_plot
positive_idiot_plot
positive_nights_plot

sentiment_summary(sentiment_crime)
sentiment_summary(sentiment_underground)
sentiment_summary(sentiment_karamazov)
sentiment_summary(sentiment_dead)
sentiment_summary(sentiment_idiot)
sentiment_summary(sentiment_nights)

timeline_story(crime_and_punishment_raw)
timeline_story(notes_from_underground_raw)
timeline_story(brothers_karamazov_raw)
timeline_story(house_of_dead_raw)
timeline_story(idiot_raw)
timeline_story(white_nights_raw)

#Wordclouds from books
crime_and_punishment
dostoyevsky_cloud <- function(book_unnested){
  graph <- wordcloud2(book_unnested[1:25,], color = "random-light", backgroundColor = "black", fontWeight = "bold", rotateRatio = 0.7,
             shape = "star", ellipticity = 1)
  
  return(graph)
}

dostoyevsky_cloud(crime_and_punishment)
dostoyevsky_cloud(notes_from_underground)
dostoyevsky_cloud(brothers_karamazov)
dostoyevsky_cloud(house_of_dead)
dostoyevsky_cloud(idiot)
dostoyevsky_cloud(white_nights)

##Creating bigrams

bigram_create <- function(raw_book){
  bigram_book <- raw_book %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
    count(bigram, sort = TRUE) %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    unite(bigram, word1, word2, sep = " ")
  return(bigram_book)
  
}


crime_and_punishment_bigram <- bigram_create(crime_and_punishment_raw)
notes_from_underground_bigram <- bigram_create(notes_from_underground_raw)
brothers_karamazov_bigram <- bigram_create(brothers_karamazov_raw)
house_of_dead_bigram <- bigram_create(house_of_dead_raw)
idiot_bigram <- bigram_create(idiot_raw)
white_nights_bigram <- bigram_create(white_nights_raw)

bigram_graph <- function(bigram_data){
  
  bigrams_graph <- bigram_data[1:20,] %>% 
    mutate(bigram = reorder(bigram, n)) %>% 
    ggplot(aes(n, bigram, group = 1)) +
    geom_col(show.legend = FALSE, fill = '#f0009c') +
    labs(x = "Frequency", y = "Bigrams", title = "Biagrams") + 
    theme(panel.background = element_rect(color = "black", fill = "black"),
          plot.background = element_rect(fill = 'black', colour = 'black'),
          plot.title = element_text(size = rel(2), hjust = 0.5, margin = margin(b = 15, t = 0, r = 0, l = 0), colour = "white"), 
          axis.title.y = element_text(margin = margin(r = 15), size = 20, colour = "white"),
          axis.title.x = element_text(margin = margin(t = 15), size = 20, colour = "white"),
          plot.caption = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(colour = "white", size = 12, face="bold"),
          axis.text.y = element_text(colour = "white", size = 12, face="bold" ),
          axis.line = element_line(color="white", size = 1.5),
          strip.text.x = element_text(size = 12, color = "white", face = "bold"),
          strip.text.y = element_text(size = 12, color = "white", face = "bold"),
          strip.background = element_rect(color = "black", fill = "black"),
          axis.ticks = element_blank())
  
  return(bigrams_graph)
}

crime_and_punishment_bigram_graph <- bigram_graph(crime_and_punishment_bigram)
notes_from_underground_bigram_graph <- bigram_graph(notes_from_underground_bigram)
brothers_karamazov_bigram_graph <- bigram_graph(brothers_karamazov_bigram)
house_of_dead_bigram_graph <- bigram_graph(house_of_dead_bigram)
idiot_bigram_graph <- bigram_graph(idiot_bigram)
white_nights_bigram_graph <- bigram_graph(white_nights_bigram)

crime_and_punishment_bigram_graph
notes_from_underground_bigram_graph
brothers_karamazov_bigram_graph 
house_of_dead_bigram_graph
idiot_bigram_graph
white_nights_bigram_graph