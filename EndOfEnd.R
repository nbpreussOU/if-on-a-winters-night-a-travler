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
  
  data <- inner_join(data, sentiment, by = "word")
  
  ggplot(data = data, aes(x = fct_rev(fct_infreq(sentiment)))) +
    geom_bar() +
    coord_flip()
  
  library(dplyr)
  nrc_sent <- data.frame(data %>% group_by(sentiment) %>% tally())
  nrc_sent

