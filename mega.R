# Library Statements

library(tm)
library(tidytext)
library(ggplot2)
library(forcats)
library(textdata)
library(stringr)
library(tidyverse)
library(dplyr)

# Actual Analysis

week1Part1 <- read.csv("titleListWeek1Part1.csv") # read in the data 
class(week1Part1) # check on its class

# convert to data frame
# this code makes it a data frame, but it still has all the special characters. 
week1Part1.df <- data.frame(week1Part1, stringsAsFactors = FALSE)
class(week1Part1.df$title) # check the class of the titles


# remove the special characters 
week1Part1.df2 <- data.frame(week1Part1.df$title) # data frame that's just what we want

# Run the string replacement in a for-loop
rowNumbers <- nrow(week1Part1.df2) # row numbers for the loop
week1Part1.df3 <- week1Part1.df2 # create the data frame to replace the punctuation 

for (i in 1:rowNumbers) {
  week1Part1.df3[i, 1] <- str_replace_all(week1Part1.df3[i, 1], "[^[:alnum:]]", " ")
}

# change name of variable
names(week1Part1.df3)[names(week1Part1.df3) == "week1Part1.df.title"] <- "postTitles"
colnames(week1Part1.df3) # check that they renamed properly

# now, we have a data frame that's just the titles from the threads
# Unnest into words
w1p1 <- as_tibble(week1Part1.df3)
w1p1 <- mutate(w1p1, text = w1p1$postTitles)
w1p1 <- w1p1 %>% 
  unnest_tokens(word, text)

# Actual Analysis
week1Part2 <- read.csv("titleListWeek1Part2.csv") # read in the data 
class(week1Part2) # check on its class

# convert to data frame

# this code makes it a data frame, but it still has all the special characters. 
week1Part2.df <- data.frame(week1Part2, stringsAsFactors = FALSE)
class(week1Part2.df$title) # check the class of the titles


# remove the special characters 

week1Part2.df2 <- data.frame(week1Part2.df$title) # data frame that's just what we want

# Run the string replacement in a for-loop

rowNumbers <- nrow(week1Part2.df2) # row numbers for the loop

week1Part2.df3 <- week1Part2.df2 # create the data frame to replace the punctuation 

for (i in 1:rowNumbers) {
  week1Part2.df3[i, 1] <- str_replace_all(week1Part2.df3[i, 1], "[^[:alnum:]]", " ")
}


# change name of variable

names(week1Part2.df3)[names(week1Part2.df3) == "week1Part2.df.title"] <- "postTitles"

colnames(week1Part2.df3) # check that they renamed properly

# now, we have a data frame that's just the titles from the threads

# Unnest into words

w1p2 <- as_tibble(week1Part2.df3)
w1p2 <- mutate(w1p2, text = w1p2$postTitles)
w1p2 <- w1p2 %>% 
  unnest_tokens(word, text)

Week2Part1 <- read.csv("titleListWeek2Part1.csv") # read in the data 
class(Week2Part1) # check on its class

# convert to data frame

# this code makes it a data frame, but it still has all the special characters. 
Week2Part1.df <- data.frame(Week2Part1, stringsAsFactors = FALSE)
class(Week2Part1.df$title) # check the class of the titles


# remove the special characters 

Week2Part1.df2 <- data.frame(Week2Part1.df$title) # data frame that's just what we want

# Run the string replacement in a for-loop

rowNumbers <- nrow(Week2Part1.df2) # row numbers for the loop

Week2Part1.df3 <- Week2Part1.df2 # create the data frame to replace the punctuation 

for (i in 1:rowNumbers) {
  Week2Part1.df3[i, 1] <- str_replace_all(Week2Part1.df3[i, 1], "[^[:alnum:]]", " ")
}


# change name of variable

names(Week2Part1.df3)[names(Week2Part1.df3) == "Week2Part1.df.title"] <- "postTitles"

colnames(Week2Part1.df3) # check that they renamed properly

# now, we have a data frame that's just the titles from the threads

# Unnest into words

w2p1 <- as_tibble(Week2Part1.df3)
w2p1 <- mutate(w2p1, text = w2p1$postTitles)
w2p1 <- w2p1 %>% 
  unnest_tokens(word, text)

# Create a word chart

EndOfEnd <- read.csv("titleListEndOfEnd.csv") # read in the data 
class(EndOfEnd) # check on its class

# convert to data frame

# this code makes it a data frame, but it still has all the special characters. 
EndOfEnd.df <- data.frame(EndOfEnd, stringsAsFactors = FALSE)
class(EndOfEnd.df$title) # check the class of the titles


# remove the special characters 

EndOfEnd.df2 <- data.frame(EndOfEnd.df$title) # data frame that's just what we want

# Run the string replacement in a for-loop

rowNumbers <- nrow(EndOfEnd.df2) # row numbers for the loop

EndOfEnd.df3 <- EndOfEnd.df2 # create the data frame to replace the punctuation 

for (i in 1:rowNumbers) {
  EndOfEnd.df3[i, 1] <- str_replace_all(EndOfEnd.df3[i, 1], "[^[:alnum:]]", " ")
}


# change name of variable

names(EndOfEnd.df3)[names(EndOfEnd.df3) == "EndOfEnd.df.title"] <- "postTitles"

colnames(EndOfEnd.df3) # check that they renamed properly

# now, we have a data frame that's just the titles from the threads

# Unnest into words

data <- as_tibble(EndOfEnd.df3)
data <- mutate(data, text = data$postTitles)
data <- data %>% 
  unnest_tokens(word, text)

# Create a word chart

# analyzed by sentiment

sentiment <- get_sentiments("nrc")
w1p1_nrc <- inner_join(w1p1, sentiment, by = "word")
sentiment <- get_sentiments("nrc")
w1p2_nrc <- inner_join(w1p2, sentiment, by = "word")
sentiment <- get_sentiments("nrc")
w2p1_nrc <- inner_join(w2p1, sentiment, by = "word")
sentiment <- get_sentiments("nrc")
data_nrc <- inner_join(data, sentiment, by = "word")


sentiment <- get_sentiments("bing")
w1p1_bing <- inner_join(w1p1, sentiment, by = "word")
sentiment <- get_sentiments("bing")
w1p2_bing <- inner_join(w1p2, sentiment, by = "word")
sentiment <- get_sentiments("bing")
w2p1_bing <- inner_join(w2p1, sentiment, by = "word")
sentiment <- get_sentiments("bing")
data_bing <- inner_join(data, sentiment, by = "word")

sentiment <- get_sentiments("loughran")
w1p1_loughran <- inner_join(w1p1, sentiment, by = "word")
sentiment <- get_sentiments("loughran")
w1p2_loughran <- inner_join(w1p2, sentiment, by = "word")
sentiment <- get_sentiments("loughran")
w2p1_loughran <- inner_join(w2p1, sentiment, by = "word")
sentiment <- get_sentiments("loughran")
data_loughran <- inner_join(data, sentiment, by = "word")

  
w1p1_nrc_g <- ggplot(data = w1p1_nrc, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

w1p1_bing_g <- ggplot(data = w1p1_bing, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

w1p1_loughran_g <- ggplot(data = w1p1_loughran, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

w1p2_nrc_g <- ggplot(data = w1p2_nrc, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

w1p2_bing_g <- ggplot(data = w1p2_bing, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

w1p2_loughran_g <- ggplot(data = w1p2_loughran, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

w2p1_nrc_g <- ggplot(data = w2p1_nrc, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

w2p1_bing_g <- ggplot(data = w2p1_bing, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

w2p1_loughran_g <- ggplot(data = w2p1_loughran, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

data_nrc_g <- ggplot(data = data_nrc, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

data_bing_g <- ggplot(data = data_bing, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

data_loughran_g <- ggplot(data = data_loughran, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()

w1p1_nrc_g
w1p2_nrc_g
w2p1_nrc_g
data_nrc_g
w1p1_bing_g
w1p2_bing_g
w2p1_bing_g
data_bing_g
w1p1_loughran_g
w1p2_loughran_g
w2p1_loughran_g
data_loughran_g


