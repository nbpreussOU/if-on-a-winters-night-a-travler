# Carson Schlittler
# 2/6 Hacklahoma

# Stock vs. Sentiment Plots
library(tidyverse)
library(quantmod)

getSymbols("GME", from ="2021-1-19", to ="2021-2-4")
#barChart(GME)

#candleChart(GME, multi.col=TRUE, theme="white")

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

## Range: 1/19 - 1/20
rect_jan19 <- data.frame(xmin=jan19, xmax=jan20, ymin=-Inf, ymax=Inf)
str(rect_jan19)
g <- p + geom_rect(data=rect_jan19, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color="grey20",
              alpha=0.5,
              inherit.aes = FALSE)

## Range: 1/20 - 1/22
rect_jan20 <- data.frame(xmin=jan20, xmax=jan22, ymin=-Inf, ymax=Inf)
str(rect_jan20)
m <- p + geom_rect(data=rect_jan20, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                   color="grey20",
                   alpha=0.5,
                   inherit.aes = FALSE)

## Range: 1/25 - 1/26
rect_jan25 <- data.frame(xmin=jan25, xmax=jan26, ymin=-Inf, ymax=Inf)
str(rect_jan25)
e <- p + geom_rect(data=rect_jan25, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                   color="grey20",
                   alpha=0.5,
                   inherit.aes = FALSE)


## Range: 2/2 - 2/3
rect_feb2 <- data.frame(xmin=feb2, xmax=feb3, ymin=-Inf, ymax=Inf)
str(rect_feb2)
r <- p + geom_rect(data=rect_feb2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                   color="grey20",
                   alpha=0.5,
                   inherit.aes = FALSE)

## Call the gmer plots
g
m
e
r

