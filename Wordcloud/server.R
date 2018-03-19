options(java.parameters = "- Xmx2048m")
library(shiny)
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

consumerKey = "3I8JehozX8N4Bojg0qSdmDFLX"   
consumerSecret = "QBg2E2TcqtvlK0vrRIwIWZGSXjlPoYaLXwjrePRDLztepAU6cg"
accessToken = "2433072234-seryr8c6OCyNU5veonMKf5hvX8JKCUOiA20TcoC"
accessSecret = "rHOyUzdhL5GN6lqWh3LOxmuKD2hb0IvpeJm4hrs24O4nx"
my_oauth <- twitteR:::setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret, access_token = accessToken, access_secret = accessSecret)

shinyServer(function(input,output,session){
  rawData <- reactive({
    input$update
    isolate({
    { tweets <- twitteR:::searchTwitter(input$term, n=input$cant, lang=input$lang)
    return(twitteR:::twListToDF(tweets))
    }
    })
    })
  wc_data <- reactive({
      input$update
      isolate({
        withProgress({
          setProgress(message = "Processing Corpus...")
          tweets <- twitteR:::searchTwitter(input$term, n=input$cant, lang=input$lang)
          tweets.df<<-twitteR:::twListToDF(tweets)
          mycorpus1 <- Corpus(VectorSource(tweets.df$text))
          mycorpus1 <- tm_map(mycorpus1, content_transformer(bracketX))
          removeRetweet <- function(x) gsub("\\b+RT"," ", x)
          mycorpus1 <- tm_map(mycorpus1, content_transformer(removeRetweet))
          removeNonASCII <- function(x) gsub("[^\x20-\x7E]", " ", x)
          mycorpus1 <- tm_map(mycorpus1, content_transformer(removeNonASCII))
          removeMentions <- function(x) gsub('@\\S+', ' ', x)
          mycorpus1 <- tm_map(mycorpus1, content_transformer(removeMentions))
          mycorpus1 <- tm_map(mycorpus1,removeNumbers)
          removeURL <- function(x) gsub("http[^[:space:]]*", " ", x)
          mycorpus1 <- tm_map(mycorpus1, content_transformer(removeURL))
          removeTags <- function(x) gsub("[ |\t]{2,}", " ", x)
          mycorpus1 <- tm_map(mycorpus1, content_transformer(removeTags))
          mycorpus1 <- tm_map(mycorpus1,content_transformer(replace_abbreviation))
          mycorpus1 <- tm_map(mycorpus1,content_transformer(replace_contraction))
          removeControls <- function(x) gsub('[[:cntrl:]]', ' ', x)
          mycorpus1 <- tm_map(mycorpus1, content_transformer(removeControls))
          removeSpecial <- function(x) gsub("\\d", ' ', x)
          mycorpus1 <- tm_map(mycorpus1, content_transformer(removeSpecial))
          mycorpus1 <- tm_map(mycorpus1, removePunctuation)
          mycorpus1 <- tm_map(mycorpus1, stripWhitespace)
          mycorpus1 <- tm_map(mycorpus1, content_transformer(tolower))
          mycorpus1= mycorpus1[mycorpus1 != ""]
          removeDup <- function(x) unique(x)
          mycorpus1 <- tm_map(mycorpus1, content_transformer(removeDup))
          Corpus  <- lapply(mycorpus1 , function(x){
            x <- as.String(x) } )
          Corpus.taggedNP  <- lapply(Corpus , function(x) {
            sent_token_annotator  <- Maxent_Sent_Token_Annotator ()
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
            chunkAnnotator <- Maxent_Chunk_Annotator(language = "en", probs = FALSE, model = NULL)
            p <- annotate(x,chunkAnnotator,y2)
            p2w  <- subset(p , type == "word")
            chunk_tags <- c()
            for(i in 1:length(p2w)){
              k <- unlist(p2w$features[[i]])
              chunk_tags <- append(chunk_tags, k[2])
            }
            tokenizedAndTagged1 <- data.frame(Tokens = x[y2w], Tags = chunk_tags)
            list1<-c()
            noun_phrases<-c()
            list4<-c()
            verb_phrases<-c()
            cat("",file="nounfile.txt",sep="\n",append=FALSE)
            cat("",file="verbfile.txt",sep="\n",append=FALSE)
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
                list3<-append(list3,list2)
                l1<-data.frame(list3)
                cat(list3,file="nounfile.txt",sep="\n",append=TRUE)
              }
            }
            
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
                cat(sapply(list5,toString),file="verbfile.txt",sep="\n",append=TRUE)
              }
            }
            gc()
          } )
        })
      })
    })
    
    wordcloud_rep <- repeatable(wordcloud)
    output$plotgraph1 <- renderPlot({
      withProgress({
        setProgress(message = "Creating Noun Wordcloud")
        mycorpus1 <- wc_data()
        nouncloud <- readLines("nounfile.txt")
        nounCloud_corpus <- Corpus(VectorSource(nouncloud))
        wordcloud(nounCloud_corpus, max.words = 100, random.order = FALSE, min.freq = 1, colors = rainbow(3),scale=c(5,1))
      })
    })
    output$plotgraph2 <- renderPlot({
      withProgress({
        setProgress(message = "Creating Verb Wordcloud")
        verbcloud <- readLines("verbfile.txt")
        verbCloud_corpus <- Corpus(VectorSource(verbcloud))
        wordcloud(verbCloud_corpus, max.words = 100, random.order = FALSE, min.freq = 1, colors = rainbow(3),scale=c(5,1))
      })
    })
    output$table1 <- renderTable({
      withProgress({
        setProgress(message = "Retrieving Tweets")
        head(rawData()[1],n=input$cant)
      })
      
    })
    
  })
