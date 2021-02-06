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
  
  data <- as_tibble(week1Part1.df3)
  data <- mutate(data, text = data$postTitles)
  data <- data %>% 
    unnest_tokens(word, text)
  
# Create a word chart
  
  # analyzed by sentiment
  
  sentiment <- get_sentiments("nrc")
  
  data <- inner_join(data, sentiment, by = "word")
  
  ggplot(data = data, aes(x = fct_rev(fct_infreq(sentiment)))) +
    geom_bar() +
    coord_flip()
  
  library(dplyr)
  nrc_sent <- data %>% group_by(sentiment) %>% tally()
  nrc_sent
