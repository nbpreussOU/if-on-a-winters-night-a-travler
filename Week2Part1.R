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

week2Part1 <- read.csv("titleListWeek2Part1.csv") # read in the data 
class(week2Part1) # check on its class

# convert to data frame

# this code makes it a data frame, but it still has all the special characters. 
week2Part1.df <- data.frame(week2Part1, stringsAsFactors = FALSE)
class(week2Part1.df$title) # check the class of the titles


# remove the special characters 

week2Part1.df2 <- data.frame(week2Part1.df$title) # data frame that's just what we want

# Run the string replacement in a for-loop

rowNumbers <- nrow(week2Part1.df2) # row numbers for the loop

week2Part1.df3 <- week2Part1.df2 # create the data frame to replace the punctuation 

for (i in 1:rowNumbers) {
  week2Part1.df3[i, 1] <- str_replace_all(week2Part1.df3[i, 1], "[^[:alnum:]]", " ")
}


# change name of variable

names(week2Part1.df3)[names(week2Part1.df3) == "week2Part1.df.title"] <- "postTitles"

colnames(week2Part1.df3) # check that they renamed properly

# now, we have a data frame that's just the titles from the threads

# Unnest into words

data <- as_tibble(week2Part1.df3)
data <- mutate(data, text = data$postTitles)
data <- data %>% 
  unnest_tokens(word, text)

# Download Stock Data

getSymbols("GME", from = "2021-01-25", to = "2021-01-26")
GME

