pkgs <-c('twitteR','ROAuth','httr','plyr','stringr','ggplot2','plotly')

for(p in pkgs) if(p %in% rownames(installed.packages()) == FALSE) {install.packages(p)}
for(p in pkgs) suppressPackageStartupMessages(library(p, quietly=TRUE, character.only=TRUE))

api_key <- "VbAVnCQUnSJmts4RBTOyNpmND"

api_secret <- "ftjcpR4kFxdtCpAtoCqr2qyEDA8BmOQGZgwRfqWSOvZ6e7xXiH"

access_token <- "788639088043814912-3ifNFQ5ajYftQAj9P7fvv1QEKT2OclS"

access_token_secret <- "4lFnt9BNyqrv6P6ZN7gXKVqmXQkwqzxsgUoTv3Q8vKDOi"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

tweets_samsung <- searchTwitter('#samsung', n=5000)

tweets_iphone <- searchTwitter('#iphone', n=5000)

tweets_motorola <- searchTwitter('#motorola', n=5000)

tweets_lenovo <- searchTwitter('#lenovo', n=5000)

tweet_micromax <-searchTwitter('#micromax', n=5000)

feed_samsung <- sapply(tweets_samsung, function(t) t$getText())

feed_iphone <- sapply(tweets_iphone, function(t) t$getText())

feed_motorola <- sapply(tweets_motorola, function(t) t$getText())

feed_lenovo <- sapply(tweets_lenovo, function(t) t$getText())

feed_micromax<-sapply(tweet_micromax,function(t) t$getText())

tweets_samsungDF <- twListToDF(tweets_samsung)
write.csv(tweets_samsungDF, "C:/users/HP/Desktop/New folder/tweets_samsung.csv")

tweets_iphoneDF<-twListToDF(tweets_iphone)
write.csv(tweets_iphoneDF,"C:/users/HP/Desktop/New folder/tweets_iphone.csv")

tweets_motorolaDF<-twListToDF(tweets_motorola)
write.csv(tweets_motorolaDF,"C:/users/HP/Desktop/New folder/tweets_motorola.csv")

tweets_lenovoDf<-twListToDF(tweets_lenovo)
write.csv(tweets_lenovoDf,"c:/users/HP/Desktop/New folder/tweets_lenovo.csv")

tweets_micromaxDf<-twListToDF(tweet_micromax)
write.csv(tweets_micromaxDf,"c:/users/HP/Desktop/New folder/tweets_micromax.csv")

yay <- scan('C:/users/HP/Desktop/New folder/positive-words.txt',what='character', comment.char=';')
boo <- scan('C:/users/HP/Desktop/New folder/negative-words.txt',what='character', comment.char=';')

bad_text <- c(boo, 'wtf', 'wait', 'waiting','epicfail', 'slow')
good_text <- c(yay, 'upgrade', ':)', '#iVoted', 'voted')

score.sentiment <- function(sentences, good_text, bad_text, .progress='none')
  
{
  require(plyr)
  
  require(stringr)
  scores = laply(sentences, function(sentence, good_text, bad_text) {
    sentence = gsub('[[:punct:]]', '', sentence)
    
    sentence = gsub('[[:cntrl:]]', '', sentence)
    
    sentence = gsub('\\d+', '', sentence)
    
    sentence <- iconv(sentence, 'UTF-8', 'ASCII')
    sentence = tolower(sentence)
    
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    
    pos.matches = match(words, good_text)
    neg.matches = match(words, bad_text)
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
    
  }, good_text, bad_text, .progress=.progress )
  
  
  
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
  
}

samsung <- score.sentiment(feed_samsung, good_text, bad_text, .progress='text')
samsung$name <- 'samsung'

iphone <- score.sentiment(feed_iphone, good_text, bad_text, .progress='text')
iphone$name <- 'iphone'

motorola <- score.sentiment(feed_motorola, good_text, bad_text, .progress='text')
motorola$name <- 'motorola'

lenovo <- score.sentiment(feed_lenovo, good_text, bad_text, .progress='text')
lenovo$name <- 'lenovo'

micromax<-score.sentiment(feed_micromax,good_text,bad_text, .progress='text')
micromax$name<-'micromax'

plotdat <- rbind(samsung, motorola, iphone, lenovo, micromax)

plotdat <- plotdat[c("name", "score")]

plotdat <- plotdat[!plotdat$score == 0, ]
plotdat <- plotdat[!plotdat$score > 3, ]
plotdat <- plotdat[!plotdat$score < (-3), ]

qplot(factor(score), data=plotdat, geom="bar",fill=factor(name),xlab = "Sentiment Score")

ep <- plotdat %>%
  ggplot(aes(x = score, fill = name)) +geom_histogram(binwidth = 1) +
  scale_fill_manual(values = rainbow(5)) +
  theme_classic(base_size = 12) +scale_x_continuous(name = "Sentiment Score") +
  scale_y_continuous(name = "Text count of tweets") +ggtitle("Super Mobiles Twitter Sentiment: 2016")+
theme(axis.title.y = element_text(face="bold", colour="#000000", size=10),axis.title.x = element_text(face="bold", colour="#000000", size=8),
      axis.text.x = element_text(angle=16, vjust=0, size=8))

ggplotly(ep)

















