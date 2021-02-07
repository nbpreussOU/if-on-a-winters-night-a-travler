# Author: Leah Pomerantz
# Hacklahoma
# Feb 6, 2021

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

data <- as_tibble(week1Part2.df3)
data <- mutate(data, text = data$postTitles)
data <- data %>% 
  unnest_tokens(word, text)

# Download Stock Data

getSymbols("GME", from = "2021-01-20", to = "2021-01-22")
GME
