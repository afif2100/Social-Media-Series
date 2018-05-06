library(tm)
library(wordcloud2)
library(twitteR)
library(rtweet)

# Ganti Sesuai dengan Key Milik Kita
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw = searchTwitter('jokowi + presiden + joko widodo', 
                            n = 200e3, 
                            since = '2018-01-01', 
                            retryOnRateLimit = 10e3)


##save dulu datanya
#saveRDS(tw,file = 'tweet-mentah.rds')
##load datanya
tw <- readRDS('tweet-mentah10k.rds')
d = twListToDF(tw)
#remove(tw)

##visualisasi time series 
ts_plot(d, "1 hour") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Jokowi + presiden Twitter statuses from past 1 Week",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

##lanjut ke asosiasi
## hanya ambil data tweet saja
komen <- d$text
komenc <- Corpus(VectorSource(komen))

##Cleaning data
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
twitclean <- tm_map(komenc, removeURL)

removeNL <- function(y) gsub("\n", " ", y)
twitclean <- tm_map(twitclean, removeNL)

replacecomma <- function(y) gsub(",", "", y)
twitclean <- tm_map(twitclean, replacecomma)

removeRT <- function(y) gsub("RT ", "", y)
twitclean <- tm_map(twitclean, removeRT)

removetitik2 <- function(y) gsub(":", "", y)
twitclean <- tm_map(twitclean, removetitik2)

removetitikkoma <- function(y) gsub(";", " ", y)
twitclean <- tm_map(twitclean, removetitikkoma)

removetitik3 <- function(y) gsub("p…", "", y)
twitclean <- tm_map(twitclean, removetitik3)

removeamp <- function(y) gsub("&amp;", "", y)
twitclean <- tm_map(twitclean, removeamp)

removeUN <- function(z) gsub("@\\w+", "", z)
twitclean <- tm_map(twitclean, removeUN)

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)


#Menghapus  titik koma, menjadi non kapital
twitclean <- tm_map(twitclean, removePunctuation)
twitclean <- tm_map(twitclean, tolower)
twitclean <- tm_map(twitclean , removeWords, 
                    c('indonesia','presiden','jokowi','joko',
                      'widodo','pak','bpk','bilang'))


					  
					  
#Build a term-document matrix

{
dtm <- TermDocumentMatrix(twitclean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
}
head(d,n=10)


wordcloud2(d,shape = "cloud",
           backgroundColor = "black",
           color = 'random-light' ,size = 0.3)



## mencari asosiasi
v<-as.list(findAssocs(dtm,
                      terms= c('besan'),
                      corlimit= c(0.50,0.15,0.15,0.15,0.15,0.15,0.15)))
v


## save data
dataframe<-data.frame(text=unlist(sapply(twitclean, `[`)), stringsAsFactors=F)
View(dataframe)

write.csv(dataframe,file = 'twitclean-10kv8.csv')
dataframe[110,]
