#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tm)
library(tidytext)
library(ggplot2)
library(forcats)
library(textdata)
library(stringr)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(quantmod)
#source("mega.R") # this broke it

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Reddit Sentiment Analysis and GME Stock"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("timePeriod", # name of the input
                        "Time Period", # what the user sees
                        c("Jan 16-19, 2021", "Jan 20-22, 2021", # the different dates that can be chosen from
                          "Jan 25-26, 2021", "Feb 4-6, 2021")),
            selectInput("sentiment", # name of the input
                        "Sentiment", # what the user sees
                        c("Loughran", "bing", "nrc")),
            helpText("Lexicon Credit: Loughran is from Tim Loughran and Bill McDonald,
                        bing from Bing Liu and collaborators, and
                        nrc from Saif Mohammad and Peter Turney.") # Lexicon credit
        ),

        # Show a ggplot of the chosen inputs
        mainPanel(
            plotOutput("RedditJan1"), # plot of the Reddit data, Jan 16-19
            plotOutput("RedditJan2"), # plot of the Reddit data, Jan 20-22
            plotOutput("RedditJan3"), # plot of the Reddit data, Jan 25-26
            plotOutput("RedditFeb"), # plot of the Reddit data, Feb 4-6
            plotOutput("stonks") # GME stocks
            # possibly add text output of the GME values
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # Reddit Graphs
    
    # Necessary code - Reddit Sentiment Analysis
    
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
    
    # Necessary Code - GME Stock Graphs
    
    getSymbols("GME", from ="2021-1-19", to ="2021-2-4")
    
    df.stocks <- fortify(GME)
    
    p <- ggplot(data=df.stocks, aes(x=Index)) +
        geom_line(aes(y = GME.Adjusted), color = "darkred", size = 1) +
        xlab("Date") +
        ylab("Adjusted Price")
    
    ## Create date variables to highlight time range of interest
    jan19 <- as.Date("2021-1-19")
    
    jan20 <- as.Date("2021-1-20")
    
    jan22 <- as.Date("2021-1-22")
    
    jan25 <- as.Date("2021-1-25")
    
    jan26 <- as.Date("2021-1-26")
    
    feb2 <- as.Date("2021-2-2")
    
    feb3 <- as.Date("2021-2-3")
    
    
        # Render the output
    
        output$RedditJan1 <- renderPlot({
            
            # Jan 16-19 and Loughran - FIRST IF
            
            if(input$timePeriod == "Jan 16-19, 2021" & input$sentiment == "Loughran"){
                
                    w1p1_loughran_g <- ggplot(data = w1p1_loughran, aes(x = fct_rev(fct_infreq(sentiment)))) +
                        geom_bar(fill = rainbow(5)) +
                        coord_flip() + ggtitle("Jan 16-19, 2021 and Loughran Sentiment") +
                        xlab("Loughran Sentiment") + ylab("Count") # create plot
                    w1p1_loughran_g # release plot

            # output$stonks <- renderPlot({})
            }
            
            # Jan 16-19 and bing
            else{ 
                # SECOND IF
            if(input$timePeriod == "Jan 16-19, 2021" & input$sentiment == "bing"){

                w1p1_bing_g <- ggplot(data = w1p1_bing, aes(x = fct_rev(fct_infreq(sentiment)))) +
                    geom_bar(fill = rainbow(2)) + coord_flip() + 
                    ggtitle("Jan 16-19, 2021 and bing Sentiment") +
                    xlab("bing Sentiment") + ylab("Count")
                w1p1_bing_g
            }
                else{
                    # THIRD IF
                    # Jan 16-19 and nrc

                    if(input$timePeriod == "Jan 16-19, 2021" & input$sentiment == "nrc"){
                            
                            w1p1_nrc_g <- ggplot(data = w1p1_nrc, aes(x = fct_rev(fct_infreq(sentiment)))) +
                                geom_bar(fill = rainbow(10)) + coord_flip() + 
                                ggtitle("Jan 16-19, 2021 and nrc Sentiment") +
                                xlab("nrc Sentiment") + ylab("Count")
                            w1p1_nrc_g
                    }
                }
                }
        })
        
    
        output$RedditJan2 <- renderPlot({
            
            # Jan 20-22 and Loughran - FIRST IF
            
            if(input$timePeriod == "Jan 20-22, 2021" & input$sentiment == "Loughran"){
                
                w1p2_loughran_g <- ggplot(data = w1p2_loughran, aes(x = fct_rev(fct_infreq(sentiment)))) +
                    geom_bar(fill = rainbow(6)) +
                    coord_flip() + ggtitle("Jan 20-22, 2021 and Loughran Sentiment") +
                    xlab("Loughran Sentiment") + ylab("Count") # create plot
                w1p2_loughran_g # release plot
                
            }
            
            # Jan 20-22 and bing
            else{ 
                # SECOND IF
                if(input$timePeriod == "Jan 20-22, 2021" & input$sentiment == "bing"){
                    
                    w1p2_bing_g <- ggplot(data = w1p2_bing, aes(x = fct_rev(fct_infreq(sentiment)))) +
                        geom_bar(fill = rainbow(2)) + coord_flip() + 
                        ggtitle("Jan 20-22, 2021 and bing Sentiment") +
                        xlab("bing Sentiment") + ylab("Count")
                    w1p2_bing_g
                }
                else{
                    # THIRD IF
                    # Jan 20-22 and nrc
                    
                    if(input$timePeriod == "Jan 20-22, 2021" & input$sentiment == "nrc"){
                        
                        w1p2_nrc_g <- ggplot(data = w1p2_nrc, aes(x = fct_rev(fct_infreq(sentiment)))) +
                            geom_bar(fill = rainbow(10)) + coord_flip() + 
                            ggtitle("Jan 20-22, 2021 and nrc Sentiment") +
                            xlab("nrc Sentiment") + ylab("Count")
                        w1p2_nrc_g
                    }
                }
            }
        })
        
        output$RedditJan3 <- renderPlot({
            
            # Jan 25-26 and Loughran - FIRST IF
            
            if(input$timePeriod == "Jan 25-26, 2021" & input$sentiment == "Loughran"){
                
                w2p1_loughran_g <- ggplot(data = w2p1_loughran, aes(x = fct_rev(fct_infreq(sentiment)))) +
                    geom_bar(fill = rainbow(6)) +
                    coord_flip() + ggtitle("Jan 25-26, 2021 and Loughran Sentiment") +
                    xlab("Loughran Sentiment") + ylab("Count") # create plot
                w2p1_loughran_g # release plot
                
            }
            
            # Jan 25-26 and bing
            else{ 
                # SECOND IF
                if(input$timePeriod == "Jan 25-26, 2021" & input$sentiment == "bing"){
                    
                    w2p1_bing_g <- ggplot(data = w2p1_bing, aes(x = fct_rev(fct_infreq(sentiment)))) +
                        geom_bar(fill = rainbow(2)) + coord_flip() + 
                        ggtitle("Jan 25-26, 2021 and bing Sentiment") +
                        xlab("bing Sentiment") + ylab("Count")
                    w2p1_bing_g
                }
                else{
                    # THIRD IF
                    # Jan 25-26 and nrc
                    
                    if(input$timePeriod == "Jan 25-26, 2021" & input$sentiment == "nrc"){
                        
                        w2p1_nrc_g <- ggplot(data = w2p1_nrc, aes(x = fct_rev(fct_infreq(sentiment)))) +
                            geom_bar(fill = rainbow(10)) + coord_flip() + 
                            ggtitle("Jan 25-26, 2021 and nrc Sentiment") +
                            xlab("nrc Sentiment") + ylab("Count")
                        w2p1_nrc_g
                    }
                }
            }
        })
        
        output$RedditFeb <- renderPlot({
            
            # Feb 4-6 and Loughran - FIRST IF
            
            if(input$timePeriod == "Feb 4-6, 2021" & input$sentiment == "Loughran"){
                
                data_loughran_g <- ggplot(data = data_loughran, aes(x = fct_rev(fct_infreq(sentiment)))) +
                    geom_bar(fill = rainbow(5)) +
                    coord_flip() + ggtitle("Feb 4-6, 2021 and Loughran Sentiment") +
                    xlab("Loughran Sentiment") + ylab("Count") # create plot
                data_loughran_g # release plot
                
            }
            
            # Feb 4-6 and bing
            else{ 
                # SECOND IF
                if(input$timePeriod == "Feb 4-6, 2021" & input$sentiment == "bing"){
                    
                    data_bing_g <- ggplot(data = data_bing, aes(x = fct_rev(fct_infreq(sentiment)))) +
                        geom_bar(fill = rainbow(2)) + coord_flip() + 
                        ggtitle("Feb 4-6, 2021 and bing Sentiment") +
                        xlab("bing Sentiment") + ylab("Count")
                    data_bing_g
                }
                else{
                    # THIRD IF
                    # Feb 4-6 and nrc
                    
                    if(input$timePeriod == "Feb 4-6, 2021" & input$sentiment == "nrc"){
                        
                        data_nrc_g <- ggplot(data = data_nrc, aes(x = fct_rev(fct_infreq(sentiment)))) +
                            geom_bar(fill = rainbow(10)) + coord_flip() + 
                            ggtitle("Feb 4-6, 2021 and nrc Sentiment") +
                            xlab("nrc Sentiment") + ylab("Count")
                        data_nrc_g
                    }
                }
            }
        })
        
        # Stocks Graph - run all the dates options again in 4 if statements
    
    output$stonks <- renderPlot({
        if(input$timePeriod == "Jan 16-19, 2021"){
            
            ## Range: 1/19 - 1/20
            rect_jan19 <- data.frame(xmin=jan19, xmax=jan20, ymin=-Inf, ymax=Inf)
            str(rect_jan19)
            g <- p + geom_rect(data=rect_jan19, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                               color="grey20",
                               alpha=0.5,
                               inherit.aes = FALSE)
            g # print the graph
        }
        
        else{
            if(input$timePeriod == "Jan 20-22, 2021"){
                
                ## Range: 1/20 - 1/22
                rect_jan20 <- data.frame(xmin=jan20, xmax=jan22, ymin=-Inf, ymax=Inf)
                str(rect_jan20)
                m <- p + geom_rect(data=rect_jan20, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                                   color="grey20",
                                   alpha=0.5,
                                   inherit.aes = FALSE)
                m # print the graph
                
            }
            
            else{
                if(input$timePeriod == "Jan 25-26, 2021"){
                 
                    ## Range: 1/25 - 1/26
                    rect_jan25 <- data.frame(xmin=jan25, xmax=jan26, ymin=-Inf, ymax=Inf)
                    str(rect_jan25)
                    e <- p + geom_rect(data=rect_jan25, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                                       color="grey20",
                                       alpha=0.5,
                                       inherit.aes = FALSE)
                    e # print the graph
                    
                }
                
                else{
                    if(input$timePeriod == "Feb 4-6, 2021"){
                        
                        ## Range: 2/2 - 2/3
                        rect_feb2 <- data.frame(xmin=feb2, xmax=feb3, ymin=-Inf, ymax=Inf)
                        str(rect_feb2)
                        r <- p + geom_rect(data=rect_feb2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                                           color="grey20",
                                           alpha=0.5,
                                           inherit.aes = FALSE)
                        r # print the graph
                    }
                }
            }
        }
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
