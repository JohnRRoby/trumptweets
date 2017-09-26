###
This code mines and plots the frequency of terms in recent trump tweets.

I borrow heavily from:

https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/

###

library(tweeteR)

library(tm)

library(dplyr)

library(ggplot2)

trump_recent<- userTimeline("realDonaldTrump", n = 78) #starts just after first Puerto Rico tweet

trump_recent_df <- tbl_df(map_df(trump_recent, as.data.frame))

corp <- Corpus(VectorSource(trump_recent_df$text))

#function to turn patterns into spaces

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

#use that function to remove dashes, hyphens, hashtags, etc

corp <- tm_map(corp, toSpace, "-")

corp <- tm_map(corp, toSpace, ":")

corp <- tm_map(corp, toSpace, "#")

corp <- tm_map(corp, toSpace, "<")

corp <- tm_map(corp, toSpace, ">")

corp <- tm_map(corp, toSpace, "amp")

corp <- tm_map(corp, toSpace, "https")

#make lowercase

corp <- tm_map(corp,content_transformer(tolower))

#strip digits

corp <- tm_map(corp, removeNumbers)

#remove stopwords

corp <- tm_map(corp, removeWords, stopwords("english"))

#strip whitespace

corp <- tm_map(corp, stripWhitespace)

#construct the document term matrix

dtm <- DocumentTermMatrix(corp)

#get frequency of words

freq <- colSums(as.matrix(dtm))

#create descending sort order

ord <- order(freq,decreasing=TRUE)

#pack into data.frame

df <- data.frame(term=names(freq),occurrences=freq)

#order factors

df$term <- factor(df$term, levels = df$term[order(-df$occurrences)])

#reverse factor levels

df$term <- factor(df$term, levels = rev(levels(df$term)))

#plot it

p <- ggplot(subset(df, occurrences>=3), aes(term, occurrences))+
geom_bar(stat="identity", width=1, size = .5, color = "black", fill = "tomato4")

p + coord_flip()+
theme(axis.text.x=element_text(angle=45, hjust=1))+
theme(panel.grid.major = element_line(color="white"),
panel.background=element_rect(fill="grey90"))+
theme(axis.text = element_text(size=10), 
axis.title = element_text(size=12, face="bold"))

#and bonus wordcloud

library(wordcloud)

wordcloud(corp, max.words=500, min.freq = 2, random.order=F, colors = c("steelblue4", "steelblue", "steelblue1", "steelblue2"), scale = c(3, .5))
