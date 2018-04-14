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
library(data.table)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RMySQL)
library(DBI)
library(textclean)
library(textreg)
consumerKey = "3I8JehozX8N4Bojg0qSdmDFLX"   
consumerSecret = "QBg2E2TcqtvlK0vrRIwIWZGSXjlPoYaLXwjrePRDLztepAU6cg"
accessToken = "2433072234-seryr8c6OCyNU5veonMKf5hvX8JKCUOiA20TcoC"
accessSecret = "rHOyUzdhL5GN6lqWh3LOxmuKD2hb0IvpeJm4hrs24O4nx"

my_oauth <- setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret, access_token = accessToken, access_secret = accessSecret)
shinyServer(function(input,output,session){
  tweetsdf <- function(){
    print("hi")
    tweets <- twitteR:::searchTwitter(input$term, n=input$cant, lang=input$lang)
    tdf <<- twitteR:::twListToDF(tweets)
    return(tdf$text)
  }
  cleantweets<-function(){
    print("hello")
    mycorpus1 <- Corpus(VectorSource(tdf$text))
    #inspect(mycorpus1)
    
    removeUnicode <- function(x) gsub("\\\\u([a-zA-Z0-9]|[_])*", " ", x)
    mycorpus1 <- tm_map(mycorpus1, content_transformer(removeUnicode))
    #inspect(mycorpus1)
    
    removeInvalid <- function(x) gsub("[^\x20-\x7E]", " ", x)
    mycorpus1 <- tm_map(mycorpus1, content_transformer(removeInvalid))
    #inspect(mycorpus1)
    
    mycorpus1 <- tm_map(mycorpus1, content_transformer(bracketX))
    #inspect(mycorpus1)
    
    removeRetweet <- function(x) gsub("\\b+RT"," ", x)
    mycorpus1 <- tm_map(mycorpus1, content_transformer(removeRetweet))
    #inspect(mycorpus1)
    
    removeMentions <- function(x) gsub('@\\S+', " ", x)
    mycorpus1 <- tm_map(mycorpus1, content_transformer(removeMentions))
    #inspect(mycorpus1)
    
    removeURL <- function(x) gsub("http[^[:space:]]*", " ", x)
    mycorpus1 <- tm_map(mycorpus1, content_transformer(removeURL))
    #inspect(mycorpus1)
    
    removeTags <- function(x) gsub("[ |\t]{2,}", " ", x)
    mycorpus1 <- tm_map(mycorpus1, content_transformer(removeTags))
    #inspect(mycorpus1)
    
    removeControls <- function(x) gsub('[[:cntrl:]]', " ", x)
    mycorpus1 <- tm_map(mycorpus1, content_transformer(removeControls))
    #inspect(mycorpus1)
    
    removeSpecial <- function(x) gsub("\\d", " ", x)
    mycorpus1 <- tm_map(mycorpus1, content_transformer(removeSpecial))
    #inspect(mycorpus1)
    
    mycorpus1 <- tm_map(mycorpus1, stripWhitespace)
    #inspect(mycorpus1)
    
    mycorpus1 <- tm_map(mycorpus1, removePunctuation)
    #inspect(mycorpus1)
    
    mycorpus1 <- tm_map(mycorpus1, content_transformer(tolower))
    #inspect(mycorpus1)
    
    trim <<- function (x) gsub("^\\s+|\\s+$", "", x)
    mycorpus1 <- tm_map(mycorpus1, content_transformer(trim))
    
    mycorpus1= mycorpus1[mycorpus1 != " "]
    #inspect(mycorpus1)
    
    remove1 <- function(x) gsub('\\b\\w{1}\\b','',x)
    mycorpus1<- tm_map(mycorpus1, content_transformer(remove1))
    
    mycorpus1<-tm_map(mycorpus1, removeWords, c("amp"))
    #inspect(mycorpus1)
    
    mycorpus1 <- tm_map(mycorpus1, stripWhitespace)
    #inspect(mycorpus1)
    
    tweets.df <- data.frame(text = sapply(mycorpus1, as.character), stringsAsFactors = F)
    tweets.df <- tweets.df[!apply(tweets.df == "", 1, all),]
    tweets.df <- as.data.frame(tweets.df)
    tweets.df<-tweets.df[sapply(gregexpr("\\W+",tweets.df$tweets.df),length)>1,]
    tweets.df <-as.character(tweets.df)
    tweets.df <<- as.data.frame(tweets.df)
    return(tweets.df)
  }
  nounverb<-function(){
    print("---------------------start---------------------")
    mycorpus <- Corpus(VectorSource(tweets.df$tweets.df))
    #inspect(mycorpus)
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
          cat(list3,"\n")
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
          cat(list5,"\n")
          sink()
        }
      }
      gc()
    })
    print("Done")
  }
  
  remaining<-function(){
    con <<- dbConnect(MySQL(), host="localhost", dbname="twitter", user="root", password="shubham95")
    colnames(tweets.df) <- c("data")
    dbWriteTable(con,name = 'tweets',value=tweets.df,overwrite = T)
    #View(tweets.df)
    print("Remaining started")
    noundata.df <- read.csv("nounfile.csv", col.names = c("noun_phrase"))
    noundata.df<<-unique(noundata.df)
    verbdata.df <- read.csv("verbfile.csv", col.names = c("verb_phrase"))
    verbdata.df<<-unique(verbdata.df)
    i <- 1
    total <- dbSendQuery(con,"SELECT count(*) from tweets")
    total <- dbFetch(total, n = -1)
    dbClearResult(dbListResults(con)[[1]])
    total <- total$`count(*)`[1]
    
    while(i<=nrow(verbdata.df)) {
      verb = verbdata.df[i,]
      verb <- as.character(verb)
      verb <-trim(verb)
      count = dbSendQuery(con,paste("SELECT count(*) from tweets where data like '%",verb,"%'",sep=""))
      count <- dbFetch(count, n = -1)
      dbClearResult(dbListResults(con)[[1]])
      count <- count$`count(*)`[1]
      prior <- count/total
      dbSendQuery(con,paste("INSERT INTO prior_values_verb (verb_phrase, prior) VALUES ('",verb,"',",prior,")",sep=""))
      i<-i+1
    }
    i <- 1
    
    while(i<=nrow(noundata.df)) {
      noun = noundata.df[i,]
      noun <- as.character(noun)
      noun <- trim(noun)
      count = dbSendQuery(con,paste("SELECT count(*) from tweets where data like '%",noun,"%'",sep=""))
      count <- dbFetch(count, n = -1)
      dbClearResult(dbListResults(con)[[1]])
      count <- count$`count(*)`[1]
      prior <- count/total
      count
      prior
      dbSendQuery(con,paste("INSERT INTO prior_values_noun (noun_phrase, prior) VALUES ('",noun,"',",prior,")",sep=""))
      i<-i+1
    }
    print("DONE")
  }
  remaining1<-function()
  {
    print("Into Remaining 1")
    k=1
    l=k+1
    factor<-c()
    theta<-c()
    prior<-c()
    from <- c()
    i <- 1
    j <- 2
    list1 <- list()
    p <- 1
    while(i<=(nrow(noundata.df)-1) && j<=nrow(noundata.df))
    {
      noun1<-noundata.df[i,]
      noun1<-as.character(noun1)
      noun2<-noundata.df[j,]
      noun2<-as.character(noun2)
      noun1 <- trim(noun1)
      noun2 <- trim(noun2)
      for(k in 1:nrow(verbdata.df))
      {
        verb1<-verbdata.df[k,1]
        verb1<-as.character(verb1)
        verb1<-trim(verb1)
        {
          max_value<-0
          x<-dbSendQuery(con,paste("SELECT COUNT(*) from tweets where data like '%",noun1,"%' and data like '%",noun2,"%' and data like '%",verb1,"%'",sep=""))
          c1.df <- dbFetch(x, n = -1)
          dbClearResult(dbListResults(con)[[1]])
          c1 <- c1.df$`COUNT(*)`[1]
          
          x<-dbSendQuery(con,paste("SELECT COUNT(*) from tweets where data like '%",noun1,"%' and data not like '%",noun2,"%' and data like '%",verb1,"%'",sep=""))
          c2.df <- dbFetch(x, n = -1)
          c2 <- c2.df$`COUNT(*)`[1]
          dbClearResult(dbListResults(con)[[1]])
          
          x<-dbSendQuery(con,paste("SELECT COUNT(*) from tweets where data not like '%",noun1,"%' and data like '%",noun2,"%' and data like '%",verb1,"%'",sep=""))
          c3.df <- dbFetch(x, n = -1)
          c3 <- c3.df$`COUNT(*)`[1]
          dbClearResult(dbListResults(con)[[1]])
          
          x<-dbSendQuery(con,paste("SELECT COUNT(*) from tweets where data not like '%",noun1,"%' and data not like '%",noun2,"%' and data like '%",verb1,"%'",sep=""))
          c4.df <- dbFetch(x, n = -1)
          c4 <- c4.df$`COUNT(*)`[1]
          sum<-c1+c2+c3+c4
          dbClearResult(dbListResults(con)[[1]])
          if(c1>c2 & c1>c3 & c1>c4) {
            from[k] <- 1
          }
          else if (c2>c3 & c2>c4){
            from[k] <- 2
          }
          else if(c3 > c4) {
            from[k] <- 3
          }
          else {
            from[k] <- 4
          }
          if(sum == 0) {
            x1 <- 0
            x2 <- 0
            x3 <- 0
            x4 <- 0
          }
          else {
            x1 <- c1/sum
            x2 <- c2/sum
            x3 <- c3/sum
            x4 <- c4/sum
          }
          x <- dbSendQuery(con,paste("SELECT prior FROM prior_values_verb WHERE verb_phrase LIKE '",verb1,"'",sep=""))
          prior.df <- dbFetch(x)
          prior[k] <- prior.df$`prior`[1]
          dbClearResult(dbListResults(con)[[1]])
          if(x1>0) {
            x1<-log10(x1)+log10(prior[k])
          }
          else {
            x1<-log10(prior[k])
          }
          if(x2>0) {
            x2<-log10(x2)+log10(prior[k])
          }
          else {
            x2<-log10(prior[k])
          }
          if(x3>0) {
            x3<-log10(x3)+log10(prior[k])
          }
          else {
            x3<-log10(prior[k])
          }
          if(x4>0) {
            x4<-log10(x4)+log10(prior[k])
          }
          else {
            x4<-log10(prior[k])
          }
          factor[k] <- max(abs(x1),abs(x2),abs(x3),abs(x4))
        }
      }
      theta<-max(factor)
      m<-0
      for(f in factor) {
        m<-m+1
        if(f==max(abs(factor))) {
          max_verb <- verbdata.df[m,]
          max_verb <- as.character(max_verb)
          if(from[m] == 1) {
            list1[[p]] <- c(noun1,max_verb,noun2,theta) 
            p<-p+1
          } else if(from[m] == 2)
          {
            list1[[p]] <- c(noun1,max_verb,theta)
            p<-p+1
          } else if(from[m] == 3)
          {
            list1[[p]] <- c(noun2,max_verb,theta)
            p<-p+1
          }
        }
      }
      i<-i+1
      j<-j+1
    }
    list1<<-unique(list1)
    print("Completed")
  }
  
  actors<-function()
  {
    print("Top Actors Started")
    list2 <- list()
    x<-0
    for(i in list1) {
      if(length(i) == 4) {
        x=x+1
        list2[[x]]<-i
        element = list2[[x]]
        y<-dbSendQuery(con,paste("SELECT COUNT(*) from tweets where data like '%",element[1],"%' and data like '%",element[2],"%' and data like '%",element[3],"%'",sep=""))
        c1.df <- dbFetch(y, n = -1)
        dbClearResult(dbListResults(con)[[1]])
        c1 <- c1.df$`COUNT(*)`[1]
        score = c1 * 10^(as.numeric(element[4]))
        list2[[x]][[4]] <- score
      }
    }
    list3<-c()
    x<-0
    m<-1
    for(i in list2) {
      x<-x+1
      
      i[[1]]<-trim(i[[1]])
      i[[2]]<-trim(i[[2]])
      i[[3]]<-trim(i[[3]])
      
      y<-dbSendQuery(con,paste("SELECT data from tweets where data like '%",i[[1]],"%' and data like '%",i[[2]],"%' and data like '%",i[[3]],"%'",sep=""))
      c1.df <- dbFetch(y, n = -1)
      dbClearResult(dbListResults(con)[[1]])
      tweets1<- c1.df[,1]
      tweets1 <- paste(tweets1, collapse = "  Next Tweet :  ")
      # tweets1<- as.list(c1.df[,1])
      list3[[x]] <- c(i[[1]],i[[2]],i[[3]],tweets1,i[[4]])
      # count=count+1
      m=m+1
    }
    actor.df <- data.frame(Noun=character(), score=integer(), stringsAsFactors=FALSE)
    for(i in list3) {
      i[[1]]<-trim(i[[1]])
      if(nchar(i[[1]])>2 && nrow(subset(actor.df,Noun==i[[1]])) == 0) {
        marks<-0
        x<-0
        for(j in list3) {
          x<-x+1
          if(j[[1]]==i[[1]] || j[[3]]==i[[1]]) {
            marks = marks + as.numeric(j[[length(list3[[x]])]])
          }
        }
        x <- dbSendQuery(con,paste("SELECT prior FROM prior_values_noun WHERE noun_phrase LIKE '",i[[1]],"'",sep=""))
        prior.df <- dbFetch(x)
        x <- prior.df$`prior`[1]
        dbClearResult(dbListResults(con)[[1]])
        marks = marks + x
        actor.df[nrow(actor.df) + 1,] = c(i[[1]],marks)
      }
      i[[3]]<-trim(i[[3]])
      if(nchar(i[[3]])>2 && nrow(subset(actor.df,Noun==i[[3]])) == 0) {
        marks<-0
        x<-0
        for(j in list3) {
          x<-x+1
          if(j[[1]]==i[[3]] || j[[3]]==i[[3]]) {
            marks = marks + as.numeric(j[[length(list3[[x]])]])
          }
        }
        x <- dbSendQuery(con,paste("SELECT prior FROM prior_values_noun WHERE noun_phrase LIKE '",i[[1]],"'",sep=""))
        prior.df <- dbFetch(x)
        x <- prior.df$`prior`[1]
        dbClearResult(dbListResults(con)[[1]])
        marks = marks + x
        actor.df[nrow(actor.df) + 1,] = c(i[[3]],marks)
      }
      clusters<<-list3
    }
    topactors <- actor.df[rev(order(as.numeric(actor.df$score))),]
    print("Top Actors Completed")
    return(topactors)
  }
  
  
  
  clusters1<-function(){
    print("Into clusters")
    clusters.df <- data.frame(matrix(unlist(clusters),nrow = length(clusters) ,byrow=T),stringsAsFactors = F)
    colnames(clusters.df)<-c("Noun1","Verb1","Noun2","Tweets","Score")
    clusters.df <- clusters.df[rev(order(as.numeric(clusters.df$Score))),]
    print("Clusters completed")
    return(clusters.df)
  }
  
  noun_phrase<-function() {
    rs <- dbSendQuery(con,"SELECT * from prior_values_noun");
    data <- fetch(rs, n=-1)
    dbClearResult(dbListResults(con)[[1]])
    data <- data[rev(order(as.numeric(data$prior))),]
    data <- unique(data)
    return(data)
  }
  verb_phrase<-function() {
    rs <- dbSendQuery(con,"SELECT * from prior_values_verb");
    data <- fetch(rs, n=-1)
    view(data)
    dbClearResult(dbListResults(con)[[1]])
    data <- data[rev(order(as.numeric(data$prior))),]
    data <- unique(data)
    return(data)
  }
  
  bigrams<-function(){
    bigram<-list()
    x<-0
    for(i in list1) {
      if(length(i)==3) {
        x=x+1
        bigram[[x]] <- i
        element = bigram[[x]]
        y<-dbSendQuery(con,paste("SELECT COUNT(*) from tweets where data like '%",element[1],"%' and data like '%",element[2],"%'",sep=""))
        c1.df <- dbFetch(y, n = -1)
        dbClearResult(dbListResults(con)[[1]])
        c1 <- c1.df$`COUNT(*)`[1]
        score <- c1 * 10^(as.numeric(element[3]))
        bigram[[x]][[3]] <- score
      }
    }
    
    bigram.df <- data.frame(Noun=character(), Verb=character(), score=integer(), stringsAsFactors=FALSE)
    for(i in bigram) {
      bigram.df[nrow(bigram.df) + 1,] = c(i[[1]],i[[2]],i[[3]])
    }
    bigram.df <- bigram.df[rev(order(as.numeric(bigram.df$score))),]
    return(bigram.df)
  }
  
  observeEvent(input$update, {
    output$table1 <- renderDataTable({ 
      Tweets <- tweetsdf()
      dt <- setDT(data.frame(Tweets))
      dt
    })
  })
  observeEvent(input$update, {
  output$table2<- renderDataTable({
    Tweets <- cleantweets()
    dt1 <- setDT(data.frame(Tweets))
    dt1
  })
  })
  observeEvent(input$update1,{
    nounverb()
    remaining()
    remaining1()
    output$table3<- renderDataTable({
      Actors <- actors()
      dt2 <- setDT(data.frame(Actors))
      dt2
    })
    output$table4<- renderDataTable({
      Clusters <- clusters1()
      dt3 <- setDT(data.frame(Clusters))
      dt3
    })
    output$table5<- renderDataTable({
      Bigrams <- bigrams()
      dt3 <- setDT(data.frame(Bigrams))
      dt3
    })
    output$table6<- renderDataTable({
      Nouns <- noun_phrase()
      dt3 <- setDT(data.frame(Nouns))
      dt3
    })
    output$table7<- renderDataTable({
      Verbs <- verb_phrase()
      dt3 <- setDT(data.frame(Verbs))
      dt3
    })
  })
})