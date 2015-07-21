library(XML)
library(httr)

URL <- c() ; section <- c() ; type <- c() ; title <- c() ; views <- c() 
date <- c() ; abstract <- c()
popular_key <- "xxxxx"

url <- "http://api.nytimes.com/svc/mostpopular/v2/mostviewed/all-sections/30.xml?"
mostViewed <- xmlParse(paste0(url,"&api-key=",popular_key))
count <- as.integer(as.numeric(unlist(xpathApply(mostViewed,"//num_results",xmlValue))[1])/20)

url <- "http://api.nytimes.com/svc/mostpopular/v2/mostviewed/all-sections/30.xml?offset="
for(i in 0:(count-1)){
offset <- i*20
mostViewed <- xmlParse(paste0(url,offset,"&api-key=",popular_key))

temp <- unlist(xpathApply(mostViewed,"//result/url",xmlValue))
URL <- c(URL,temp)

temp <- unlist(xpathApply(mostViewed,"//result/section",xmlValue))
section <- c(section,temp)

temp <- unlist(xpathApply(mostViewed,"//result/type",xmlValue))
type <- c(type,temp)

temp <- unlist(xpathApply(mostViewed,"//result/title",xmlValue))
title <- c(title,temp)

temp <- unlist(xpathApply(mostViewed,"//result/published_date",xmlValue))
date <- c(date,temp)

temp <- unlist(xpathApply(mostViewed,"//result/abstract",xmlValue))
abstract <- c(abstract,temp)
rm(mostViewed)
}

popular <- data.frame(URL,section,type,title,date,abstract,views)
popular$weekday <- as.factor(weekdays(as.Date(as.character(popular$date),"%Y-%m-%d")))
popular$title <- as.character(popular$title)
popular$abstract <- as.character(popular$abstract)
popular$weekday <- factor(popular$weekday,levels=c("Monday","Tuesday","Wednesday",
                                         "Thursday","Friday","Saturday",
                                         "Sunday"))
popular$rank <- c(1:dim(popular[1]))

p <- ggplot(popular, aes(weekday, rank,color=type))
p <- p + geom_boxplot() + geom_jitter() + facet_grid(. ~ type)
p
# for article, days of the week doesn't make much sense, but it seems there's some 
# difference posting blog and interactive in different weekdays
dev.copy(png,'plot1.1.png')
dev.off()

txt <- paste(popular$title,collapse=" ")
library("RWeka")
library("tm")
ar <- NGramTokenizer(txt,Weka_control(min = 2, max = 2))
br <- WordTokenizer(txt)
tbr <- data.frame(table(br))
tbr <- tbr[order(tbr$Freq,decreasing=TRUE),]
tbr <- subset(tbr,!br %in% c("a","A","in","In","of","For","for","the","The",
                             "is","Is","to","and","on","at","as","by","s",
                             "With","Are","After"))
top10 <- tbr[1:10,]
qplot(x=br,y=Freq,data=top10,color="words",xlab="word",ylab="Frequency",
      main="Most Frenquent words in popular title")
dev.copy(png,'plot2.1.png')
dev.off()
