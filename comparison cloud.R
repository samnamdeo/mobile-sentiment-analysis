library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

clean.text = function(x)
{
  
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # tolower
  x = tolower(x)
  return(x)
}

# clean texts
samsung_clean = clean.text(feed_samsung)
iphone_clean = clean.text(feed_iphone)
motorola_clean = clean.text(feed_motorola)
lenovo_clean = clean.text(feed_lenovo)
micromax_clean=clean.text(feed_micromax)

samsung = paste(samsung_clean, collapse=" ")
iphone = paste(iphone_clean, collapse=" ")
motorola = paste(motorola_clean, collapse=" ")
lenovo = paste(lenovo_clean, collapse=" ")
micromax=paste(micromax_clean, collapse =" ")

# put everything in a single vector
all = c(samsung, iphone, motorola, lenovo,micromax)



# remove stop-words
all = removeWords(all,removeWords,
                  c(stopwords("english"), "samsung", "iphone", "motorola", "micromax","lenovo"))



# create corpus
corpus = Corpus(VectorSource(all))


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "\\|")

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
# Remove english common stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
corpus <- tm_map(corpus, removeWords, c("blabla1", "blabla2","samsung","lenovo","iphone","motorola","micromax","moto")) 
# Remove punctuations
corpus <- tm_map(corpus, removePunctuation)
# Eliminate extra white spaces
corpus <- tm_map(corpus, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)

# add column names
colnames(tdm) = c("samsung", "iphone", "motorola", "micromax","lenovo")



# comparison cloud
comparison.cloud(tdm, random.order=TRUE,
                 colors = rainbow(14),
                 title.size=1, max.words=500)
