options(java.parameters = "- Xmx2048m")
library(twitteR)
library(qdap)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(rJava)
library(NLP)
library(openNLP)
library(tm)
library(stringr)
library(gsubfn)
library(plyr)
library(dplyr)
library(openNLPmodels.en)
library(openNLPdata)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RMySQL)
library(textclean)
library(textreg)
consumerKey = "3I8JehozX8N4Bojg0qSdmDFLX"   
consumerSecret = "QBg2E2TcqtvlK0vrRIwIWZGSXjlPoYaLXwjrePRDLztepAU6cg"
accessToken = "2433072234-seryr8c6OCyNU5veonMKf5hvX8JKCUOiA20TcoC"
accessSecret = "rHOyUzdhL5GN6lqWh3LOxmuKD2hb0IvpeJm4hrs24O4nx"

my_oauth <- setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret,
                                access_token = accessToken, access_secret = accessSecret)
tweets<-twitteR:::searchTwitter("#trump",n=10,lang="en")
tweets
tweets.df<-twitteR:::twListToDF(tweets)

#con <- dbConnect(MySQL(), host="localhost", dbname="twitter", user="root", password="shubham95")

# chunk_into_sentences <- function(text) {
#   simpleText<-text
#   simpleText_str <- as.String(simpleText)
#   sent_token_annotator <- Maxent_Sent_Token_Annotator()
#   annotated_sentence <- annotate(simpleText_str, sent_token_annotator)
#   return(simpleText_str[annotated_sentence])
# }

mycorpus1 <- Corpus(VectorSource(tweets.df$text))
inspect(mycorpus1)
#mycorpus <- VCorpus(VectorSource(tweets.df$text))
#corpus_frame <- data.frame(text=unlist(sapply(mycorpus, `[`, "content")), stringsAsFactors=F)
#tweets<-chunk_into_sentences(corpus_frame)

#mycorpus1 <- Corpus(VectorSource(tweets))

removeInvalid <- function(x) gsub("[^\x20-\x7E]", " ", x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(removeInvalid))
inspect(mycorpus1)

mycorpus1 <- tm_map(mycorpus1, content_transformer(bracketX))
inspect(mycorpus1)



removeRetweet <- function(x) gsub("\\b+RT"," ", x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(removeRetweet))
inspect(mycorpus1)

removeMentions <- function(x) gsub('@\\S+', " ", x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(removeMentions))
inspect(mycorpus1)

mycorpus1 <- tm_map(mycorpus1,removeNumbers)
inspect(mycorpus1)

removeURL <- function(x) gsub("http[^[:space:]]*", " ", x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(removeURL))
inspect(mycorpus1)

removeTags <- function(x) gsub("[ |\t]{2,}", " ", x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(removeTags))
inspect(mycorpus1)


mycorpus1 <- tm_map(mycorpus1,content_transformer(replace_abbreviation))
inspect(mycorpus1)

mycorpus1 <- tm_map(mycorpus1,content_transformer(replace_contraction))
inspect(mycorpus1)

removeControls <- function(x) gsub('[[:cntrl:]]', " ", x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(removeControls))
inspect(mycorpus1)

removeSpecial <- function(x) gsub("\\d", " ", x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(removeSpecial))
inspect(mycorpus1)

mycorpus1= mycorpus1[mycorpus1 != " "]
inspect(mycorpus1)

removeDup <- function(x) unique(x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(removeDup))
inspect(mycorpus1)

mycorpus1 <- tm_map(mycorpus1, stripWhitespace)
inspect(mycorpus1)

addendmark<-function(x)add_missing_endmark(x,replacement = ".")
mycorpus1 <- tm_map(mycorpus1, content_transformer(addendmark))
inspect(mycorpus1)

addspacequestion <-function(x)gsub("\\?"," ?",x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(addspacequestion))
inspect(mycorpus1)

addspaceexclamation <-function(x)gsub("\\!"," !",x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(addspaceexclamation))
inspect(mycorpus1)

simpleText<-mycorpus1
simpleText_str <- as.String(simpleText)
sent_token_annotator <- Maxent_Sent_Token_Annotator()
annotated_sentence <- annotate(simpleText_str, sent_token_annotator)
sentences.df <- data.frame(matrix(unlist(simpleText_str[annotated_sentence]),byrow=T),stringsAsFactors=FALSE)
print(sentences.df)
mycorpus1 <- Corpus(VectorSource(sentences.df$matrix.unlist.simpleText_str.annotated_sentence....byrow...T.))
inspect(mycorpus1)
remove <-function(x)gsub("[c(]"," ",x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(remove))
inspect(mycorpus1)


mycorpus1 <- tm_map(mycorpus1, content_transformer(bracketX))
inspect(mycorpus1)


mycorpus1 <- tm_map(mycorpus1, removePunctuation)
inspect(mycorpus1)

mycorpus1 <- tm_map(mycorpus1, content_transformer(tolower))
inspect(mycorpus1)

tweets.df <- data.frame(text = sapply(mycorpus1, as.character), stringsAsFactors = FALSE)
tweets.df
dbWriteTable(con,name = 'tweets',tweets.df, overwrite = T, row.names =F)

Corpus  <- lapply(mycorpus1 , function(x){
  x <- as.String(x) } )
b<<-0
Corpus.taggedNP  <- lapply(Corpus , function(x) {
  b<<-b+1
  sent_token_annotator  <- Maxent_Sent_Token_Annotator ()
  print(sent_token_annotator)
  word_token_annotator  <- Maxent_Word_Token_Annotator ()
  pos_tag_annotator  <- Maxent_POS_Tag_Annotator ()
  gc()
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator , y1)
  y2w  <- subset(y2 , type == "word")
  tags  <- sapply(y2w$features , '[[', "POS")
  r1 <- sprintf("%s/%s", x[y2w], tags)
  tokenizedAndTagged <- data.frame(Tokens = x[y2w], Tags = tags)
  r2 <- paste(r1 , collapse = " ")
  # print(r2)
  chunkAnnotator <- Maxent_Chunk_Annotator(language = "en", probs = FALSE, model = NULL)
  p <- annotate(x,chunkAnnotator,y2)
  p2w  <- subset(p , type == "word")
  chunk_tags <- c()
  for(i in 1:length(p2w)){
    k <- unlist(p2w$features[[i]])
    chunk_tags <- append(chunk_tags, k[2])
  }
  tokenizedAndTagged1 <- data.frame(Tokens = x[y2w], Tags = chunk_tags)
  #print(tokenizedAndTagged1)
  list1<-c()
  noun_phrases<-c()
  list4<-c()
  verb_phrases<-c()
  #print("The noun phrases are")
  
  for(row in 1:nrow(tokenizedAndTagged1)) {
    w <- tokenizedAndTagged1[row, "Tokens"]
    t <- tokenizedAndTagged1[row, "Tags"]
    if( t== "B-NP") {
      list1<-c(as.character(w))
      list3<-c(list1)
      c<-row+1
      list2<-c()
      for(ro in c:nrow(tokenizedAndTagged1)) {
        w1 <- tokenizedAndTagged1[ro, "Tokens"]
        t1 <- tokenizedAndTagged1[ro, "Tags"]
        if(!is.na(t1) && t1=="I-NP") {
          list2 <- append(list2, as.character(w1))
        }
        else {
          break
        }
      }
      sink("nounfile.csv",append = TRUE)
      list3<-append(list3,list2)
      cat(b,",",list3,"\n")
      sink()
    }
  }
  #print("The verb phrases are")
  for(row in 1:nrow(tokenizedAndTagged1)) {
    w <- tokenizedAndTagged1[row, "Tokens"]
    t <- tokenizedAndTagged1[row, "Tags"]
    if( t== "B-VP") {
      list4<-c(as.character(w))
      list5<-c(list4)
      c<-row+1
      list6<-c()
      for(ro in c:nrow(tokenizedAndTagged1)) {
        w1 <- tokenizedAndTagged1[ro, "Tokens"]
        t1 <- tokenizedAndTagged1[ro, "Tags"]
        if(!is.na(t1) && t1=="I-VP") {
          list6 <- append(list6, as.character(w1))
        }
        else {
          break
        }
      }
      list5<-append(list5,list6)
      sink("verbfile.csv",append = TRUE)
      cat(b,",",list5,"\n")
      sink()
    }
  }
  gc()
} )

noundata.df <- read.csv("nounfile.csv", col.names = c("tweet_no","noun_phrase"))
noundata.df
dbWriteTable(con,name = 'noun',noundata.df, overwrite = T, row.names =F)

verbdata.df <- read.csv("verbfile.csv", col.names = c("tweet_no","verb_phrase"))
verbdata.df
dbWriteTable(con,name = 'verb',verbdata.df, overwrite = T, row.names =F)

verbcloud <- readLines("verbfile.txt")
#verbCloud_corpus <- Corpus(VectorSource(verbcloud))
wordcloud(verbcloud, max.words = 100, random.order = FALSE, min.freq = 1, colors = rainbow(5))

file.remove("nounfile.txt")
file.remove("verbfile.txt")
