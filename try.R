options(java.parameters = "- Xmx2048m")
options(digits = 12)
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

# tweets<-twitteR:::searchTwitter("#trump",n=10,lang="en")
# tweets
# tweets.df<-twitteR:::twListToDF(tweets)

tweets.df <- read.csv("jkSentences.csv")
con <- dbConnect(MySQL(), host="localhost", dbname="twitter", user="root", password="shubham95")


mycorpus1 <- Corpus(VectorSource(tweets.df$text))
inspect(mycorpus1)

removeUnicode <- function(x) gsub("\\\\u([a-zA-Z0-9]|[_])*", " ", x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(removeUnicode))
inspect(mycorpus1)


#mycorpus <- VCorpus(VectorSource(tweets.df$text))
#corpus_frame <- data.frame(text=unlist(sapply(mycorpus, `[`, "content")), stringsAsFactors=F)
#tweets<-chunk_into_sentences(corpus_frame)


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

#mycorpus1 <- tm_map(mycorpus1,content_transformer(replace_contraction))
#inspect(mycorpus1)

removeControls <- function(x) gsub('[[:cntrl:]]', " ", x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(removeControls))
inspect(mycorpus1)

removeSpecial <- function(x) gsub("\\d", " ", x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(removeSpecial))
inspect(mycorpus1)



mycorpus1 <- tm_map(mycorpus1, stripWhitespace)
inspect(mycorpus1)

# addendmark<-function(x)add_missing_endmark(x,replacement = ".")
# mycorpus1 <- tm_map(mycorpus1, content_transformer(addendmark))
# inspect(mycorpus1)
# 
# addspacequestion <-function(x)gsub("\\?"," ?",x)
# mycorpus1 <- tm_map(mycorpus1, content_transformer(addspacequestion))
# inspect(mycorpus1)
# 
# addspaceexclamation <-function(x)gsub("\\!"," !",x)
# mycorpus1 <- tm_map(mycorpus1, content_transformer(addspaceexclamation))
# inspect(mycorpus1)

#simpleText<-mycorpus1
#simpleText_str <- as.String(simpleText)
#annotated_sentence_prob <- annotate(simpleText_str,Maxent_Sent_Token_Annotator(probs = TRUE))
#simpleText_str[annotated_sentence]
#sentences.df <- data.frame(simpleText_str[annotated_sentence])
#print(sentences.df)
#mycorpus1 <- Corpus(VectorSource())
#inspect(mycorpus1)
#remove <-function(x)gsub("[c(]"," ",x)
#mycorpus1 <- tm_map(mycorpus1, content_transformer(remove))
#inspect(mycorpus1)
#mycorpus1 <- tm_map(mycorpus1, content_transformer(bracketX))
#inspect(mycorpus1)
  


# punc <- function (x) gsub("[^[:alnum:][:space:]#]", "", x)
# mycorpus1 <- tm_map(mycorpus1, content_transformer(punc))

mycorpus1 <- tm_map(mycorpus1, removePunctuation)
inspect(mycorpus1)

mycorpus1 <- tm_map(mycorpus1, content_transformer(tolower))
inspect(mycorpus1)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(trim))

removeDup <- function(x) unique(x)
mycorpus1 <- tm_map(mycorpus1, content_transformer(removeDup))
inspect(mycorpus1)

mycorpus1= mycorpus1[mycorpus1 != " "]
inspect(mycorpus1)



tweets.df <- data.frame(text = sapply(mycorpus1, as.character), stringsAsFactors = F)
View(tweets.df)
tweets.df <- tweets.df[!apply(is.na(tweets.df) | tweets.df == "", 1, all),]
tweets.df <- as.data.frame(tweets.df)
tweets.df<-tweets.df[sapply(gregexpr("\\W+",tweets.df$tweets.df),length)>1,]
tweets.df <-as.character(tweets.df)
tweets.df <- as.data.frame(tweets.df)
View(tweets.df)

# tweets.df  <- lapply(tweets.df , function(x){
#  x <- as.String(x) } )

mycorpus <- Corpus(VectorSource(tweets.df$tweets.df))

b<<-0
Corpus.taggedNP  <- lapply(mycorpus , function(x) {
  b<<-b+1
  x <- as.String(x)

    sent_token_annotator  <- Maxent_Sent_Token_Annotator ()
    # print(sent_token_annotator)
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
#View(tweets.df)
dbWriteTable(con,name = 'tweets',tweets.df, overwrite = T)

noundata.df <- read.csv("nounfile.csv", col.names = c("tweet_no","noun_phrase"))
#View(noundata.df)
dbWriteTable(con,name = 'noun',noundata.df, overwrite = T, row.names =F)

verbdata.df <- read.csv("verbfile.csv", col.names = c("tweet_no","verb_phrase"))
#verbdata.df
dbWriteTable(con,name = 'verb',verbdata.df, overwrite = T, row.names =F)

dbSendQuery(con,"INSERT INTO prior_values_verb (verb_phrase, prior) SELECT verb_phrase, COUNT(*)/(SELECT COUNT(*) FROM tweets) from verb GROUP BY verb_phrase")
dbSendQuery(con, "INSERT INTO prior_values_noun (noun_phrase, prior) SELECT noun_phrase, COUNT(*)/(SELECT COUNT(*) FROM tweets) from noun GROUP BY noun_phrase");

k=1
l=k+1
factor<-c()
theta<-c()
prior<-c()
from <- c()
i <- 1
j <- 2
list1 <- list()
#har list main hum triplet ya bigram bana rahe hai and finally hume list of lists banana hai
while(i<=(nrow(noundata.df)-1) && j<=nrow(noundata.df))
{
  noun1<-noundata.df[i,2]
  noun1<-as.character(noun1)
  noun2<-noundata.df[j,2]
  noun2<-as.character(noun2)
  class(noun1)
  noun2
  for(k in 1:nrow(verbdata.df))
  {
    verb1<-verbdata.df[k,2]
    verb1<-as.character(verb1)
    {
      max_value<-0
      # noun1 = " jallikattu "
      # noun2 = " supreme court tomorrow "
      # verb1 = " to be heard "
      x<-dbSendQuery(con,paste("SELECT Count(DISTINCT a.tweet_no) from noun as a INNER JOIN noun as b ON a.tweet_no=b.tweet_no inner join verb as c on b.tweet_no=c.tweet_no where (a.noun_phrase like '",noun1,"') AND (b.noun_phrase like '",noun2,"') AND (c.verb_phrase like '",verb1,"')",sep=""))
      c1.df <- dbFetch(x, n = -1)
      dbClearResult(dbListResults(con)[[1]])
      c1 <- c1.df$`Count(DISTINCT a.tweet_no)`[1]

      x<-dbSendQuery(con,paste("SELECT Count(DISTINCT a.tweet_no) from noun as a  INNER JOIN verb as c on a.tweet_no=c.tweet_no where (a.noun_phrase like '",noun1,"') AND (c.verb_phrase like '",verb1,"')",sep=""))
      c2.df <- dbFetch(x, n = -1)
      c2 <- c2.df$`Count(DISTINCT a.tweet_no)`[1]
      c2 <- c2-c1
      dbClearResult(dbListResults(con)[[1]])
      
      x<-dbSendQuery(con,paste("SELECT Count(DISTINCT a.tweet_no) from noun as a  INNER JOIN verb as c on a.tweet_no=c.tweet_no where (a.noun_phrase like '",noun2,"') AND (c.verb_phrase like '",verb1,"')",sep=""))
      c3.df <- dbFetch(x, n = -1)
      c3 <- c3.df$`Count(DISTINCT a.tweet_no)`[1]
      c3 <- c3-c1
      dbClearResult(dbListResults(con)[[1]])
      
      x<-dbSendQuery(con,"SELECT Count(*) from tweets")
      c4.df <- dbFetch(x, n = -1)
      class(c4.df)
      c4 <- c4.df$`Count(*)`[1]
      c4 <- c4-c1-c2-c3
      sum<-c1+c2+c3+c4
      dbClearResult(dbListResults(con)[[1]])
      if(c1==0 && c2==0 && c3==0) {
        #max_value<-c4
        from[k] <- 4
      }
      else
      {
      if(c1>c2 & c1>c3)
      {  
            # #append the value of noun1,noun2,verb1
            # list1[[k]]<-c(noun1,noun2,verb1)
            #max_value<-c1
            from[k] <- 1
      } else if(c2>c3)
      {
          # list1[[k]]<-c(noun1,verb1)
          #max_value<-c2
          from[k] <- 2
      } else 
      {
        # list1[[k]]<-c(noun2,verb1)
        #max_value<-c3
        from[k] <- 3
      }
      
      }
      # print(c1)
      # print(c2)
      # print(c3)
      # print(c4)
      # if(c1<0||c2<0||c3<0||c4<0) {
      #   print(i)
      #   print(k)
      #   quit()
      # }
      x1 <- c1/sum
      x2 <- c2/sum
      x3 <- c3/sum
      x4 <- c4/sum
      # factor[k]<-max_value/sum
      x <- dbSendQuery(con,paste("SELECT prior FROM prior_values_verb WHERE verb_phrase LIKE '",verb1,"'",sep=""))
      prior.df <- dbFetch(x)
      prior[k] <- prior.df$`prior`[1]
      dbClearResult(dbListResults(con)[[1]])
      if(x1>0) {
        x1<-log10(x1)+log10(prior[k])
      }
      else {
        x1<-0+log10(prior[k])
      }
      if(x2>0) {
        x2<-log10(x2)+log10(prior[k])
      }
      else {
        x2<-0+log10(prior[k])
      }
      if(x3>0) {
        x3<-log10(x3)+log10(prior[k])
      }
      else {
        x3<-0+log10(prior[k])
      }
      if(x4>0) {
        x4<-log10(x4)+log10(prior[k])
      }
      else {
        x4<-0+log10(prior[k])
      }
      factor[k] <- max(abs(x1),abs(x2),abs(x3),abs(x4))
    }
  }
  print(factor)
  theta<-max(factor)
  print(theta)
  max_verb <- verbdata.df[which.max(abs(factor)),2]
  max_verb <- as.character(max_verb)
  #store that verb corresponding to ith and jth noun
  if(from[which.max(abs(factor))] == 1) {
    list1[[i]] <- c(noun1,max_verb,noun2,theta) 
  } else if(from[which.max(abs(factor))] == 2)
  {
    list1[[i]] <- c(noun1,max_verb,theta)
  } else if(from[which.max(abs(factor))] == 3)
  {
    list1[[i]] <- c(noun2,max_verb,theta)
  }
  i<-i+1
  j<-j+1
}
list1
# verbcloud <- readLines("verbfile.txt")
# #verbCloud_corpus <- Corpus(VectorSource(verbcloud))
# wordcloud(verbcloud, max.words = 100, random.order = FALSE, min.freq = 1, colors = rainbow(5))
# file.remove("nounfile.txt")
# file.remove("verbfile.txt")

list1<- unique(list1)
list2 <- list()
x<-0
for(i in list1) {
  if(length(i) == 4) {
    x=x+1
    list2[[x]]<-i
    
    element = list2[[x]]
    
    y<-dbSendQuery(con,paste("SELECT Count(DISTINCT a.tweet_no) from noun as a INNER JOIN noun as b ON a.tweet_no=b.tweet_no inner join verb as c on b.tweet_no=c.tweet_no where (a.noun_phrase like '",element[1],"') AND (b.noun_phrase like '",element[3],"') AND (c.verb_phrase like '",element[2],"')",sep=""))
    c1.df <- dbFetch(y, n = -1)
    dbClearResult(dbListResults(con)[[1]])
    c1 <- c1.df$`Count(DISTINCT a.tweet_no)`[1]
    
    score = c1 * 10^(as.numeric(element[4]))
    
    list2[[x]][[4]] <- score
    
  }
}
list2

list3<-c()
x<-0
m<-1
# count = 0
for(i in list2) {
  if(i[[4]] == max(list2[[4]])) {
    x = x+1
    print(i[[1]])
    print(i[[2]])
    print(i[[3]])
    
    y<-dbSendQuery(con,paste("SELECT * from tweets WHERE row_names = (SELECT DISTINCT(a.tweet_no) from noun as a INNER JOIN noun as b ON a.tweet_no=b.tweet_no inner join verb as c on b.tweet_no=c.tweet_no where (a.noun_phrase like '", i[[1]],"') AND (b.noun_phrase like '", i[[3]],"') AND (c.verb_phrase like '", i[[2]],"'))",sep=""))
    c1.df <- dbFetch(y, n = -1)
    dbClearResult(dbListResults(con)[[1]])
    tweets<- as.list(c1.df[,2])
    print(tweets)
    
    list3[[x]] <- c(i[[1]],i[[2]],i[[3]],tweets,i[[4]])
    count=count+1
    list2[[m]]<-NULL
  }
  m=m+1
}
list3
actor.df <- data.frame(Noun=character(), score=integer(), stringsAsFactors=FALSE)

for(i in list3) {
  if(nrow(subset(actor.df,Noun==i[[1]])) == 0) {
    marks<-0
    x<-0
    for(j in list3) {
      x<-x+1
      if(j[[1]]==i[[1]] || j[[3]]==i[[1]]) {
        marks = marks + as.numeric(j[[length(list3[[x]])]])
      }
    }
    x <- dbSendQuery(con,paste("SELECT prior FROM prior_values_verb WHERE verb_phrase LIKE '",verb1,"'",sep=""))
    prior.df <- dbFetch(x)
    x <- prior.df$`prior`[1]
    dbClearResult(dbListResults(con)[[1]])
    marks = marks + x
    actor.df[nrow(actor.df) + 1,] = c(i[[1]],marks)
  }
  if(nrow(subset(actor.df,Noun==i[[3]])) == 0) {
    marks<-0
    x<-0
    for(j in list3) {
      x<-x+1
      if(j[[1]]==i[[3]] || j[[3]]==i[[3]]) {
        marks = marks + as.numeric(j[[length(list3[[x]])]])
      }
    }
    x <- dbSendQuery(con,paste("SELECT prior FROM prior_values_verb WHERE verb_phrase LIKE '",verb1,"'",sep=""))
    prior.df <- dbFetch(x)
    x <- prior.df$`prior`[1]
    dbClearResult(dbListResults(con)[[1]])
    marks = marks + x
    actor.df[nrow(actor.df) + 1,] = c(i[[3]],marks)
  }
}

topactors <- actor.df[rev(order(actor.df$score)),]