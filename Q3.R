# load data 
myData <- read.csv("OnlineNewsPopularity/OnlineNewsPopularity.csv")

# process data
for(i in 1:nrow(myData)) {
    if(myData$data_channel_is_lifestyle[i]==1) {
        myData$topic[i] = "life"}
    else if (myData$data_channel_is_entertainment[i]==1) {
        myData$topic[i] = "entertainment"}
    else if (myData$data_channel_is_bus[i]==1) {
        myData$topic[i] = "business"}
    else if (myData$data_channel_is_socmed[i]==1) {
        myData$topic[i] = "socialMedia"}
    else if (myData$data_channel_is_tech[i]==1){
        myData$topic[i] = "tech"}
    else if (myData$data_channel_is_world[i]==1){
        myData$topic[i] = "world"}
    else {
        myData$topic[i] = "other"}
    }
myData$topic <- as.factor(myData$topic)

for(i in 1:nrow(myData)) {
    if (myData$weekday_is_monday[i]==1){
        myData$day[i] = "Monday"
    }
    else if (myData$weekday_is_tuesday[i]==1){
        myData$day[i] = "Tuesday"
    }
    else if (myData$weekday_is_wednesday[i]==1){
        myData$day[i] = "Wednesday"
    }
    else if (myData$weekday_is_thursday[i]==1){
        myData$day[i] = "Thursday"
    }
    else if (myData$weekday_is_friday[i]==1){
        myData$day[i] = "Friday"
    }
    else if (myData$weekday_is_saturday[i]==1){
        myData$day[i] = "Saturday"
    }
    else{
        myData$day[i] = "Sunday"
    }
}
myData$day <- as.factor(myData$day)
myData$day <- factor(myData$day,levels=c("Monday","Tuesday","Wednesday",
                                         "Thursday","Friday","Saturday",
                                         "Sunday"))   

# plot1
## shows how shares changes belong to different topics and published on 
## different days
library(ggplot2)    
qplot(x=day,y=shares,data=myData,color=topic,main="Shares of Different Topic
      on Day Scale")
dev.copy(png,'plot1.png')
dev.off()

# plot2

qplot(rate_negative_words,rate_positive_words,data=myData)
## accroding to this one, we know they didn't consider neutral word 
## the impact of negative words rate is the opposite of the positive words rate
## we only need to consider one
qplot(n_tokens_content,rate_positive_words,data=myData)
qplot(n_tokens_content,shares,data=myData)
qplot(rate_positive_words,shares,data=myData,main="How Positive Words Rate 
      Impact the Share of the Link")
dev.copy(png,'plot2.png')
dev.off()

