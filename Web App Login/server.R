shinyServer(function(input, output, session) {
#### UI code --------------------------------------------------------------
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 2, offset = 5,
            br(), br(), br(), br(),
            uiOutput("uiLogin"),
            uiOutput("pass")
          )
        )
      )
    } else {
    #### Our app's UI code goes here
    fluidPage(
      titlePanel("Twitter Data Summarization"),
      sidebarLayout(
      sidebarPanel(
        textInput("term", "Enter a term", ""),
        sliderInput("cant", "Select a number of tweets",min=5,max=1500,value=5),
        actionButton("update","Retrieve Tweets"),
        actionButton("update1","Summarize Data")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Tweets",dataTableOutput('table1')),
          tabPanel("Cleaned Tweets",dataTableOutput('table2')),
          tabPanel("Top Actors",dataTableOutput('table3')),
          tabPanel("Top Clusters",dataTableOutput('table4')),
          tabPanel("Bigrams",dataTableOutput('table5')),
          tabPanel("Noun",dataTableOutput('table6')),
          tabPanel("Verb",dataTableOutput('table7'))
          #tabPanel("WordCloud",plotOutput("plotgraph1"))
          #tabPanel("Verb Cloud",plotOutput("plotgraph2"))
        )
      )
    )
  )


    }
  })
  
#### OUR APP'S SERVER CODE GOES HERE ----------------------------------------
  consumerKey = "3I8JehozX8N4Bojg0qSdmDFLX"   
  consumerSecret = "QBg2E2TcqtvlK0vrRIwIWZGSXjlPoYaLXwjrePRDLztepAU6cg"
  accessToken = "2433072234-seryr8c6OCyNU5veonMKf5hvX8JKCUOiA20TcoC"
  accessSecret = "rHOyUzdhL5GN6lqWh3LOxmuKD2hb0IvpeJm4hrs24O4nx"
  
  my_oauth <- setup_twitter_oauth(consumer_key = consumerKey, consumer_secret = consumerSecret, access_token = accessToken, access_secret = accessSecret)
  
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
      
      removeDup <- function(x) unique(x)
      mycorpus1 <- tm_map(mycorpus1, content_transformer(removeDup))
      
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
      # noundata.df$total <- sapply(noundata.df$noun_phrase, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
      # stopw<-stopwords('english')
      # i<-1
      # while(i<=(nrow(noundata.df)))
      # {
      #   #print(i)
      #   if(as.numeric(noundata.df[i,2])==1)
      #   {
      #     for(j in stopw)
      #     {
      #       if(trim(as.character(noundata.df[i,1]))==j)
      #       {
      #         noundata.df<-noundata.df[-c(i),]
      #       }
      #     }
      #   }
      #   i<-i+1
      # }
      noundata.df<-unique(noundata.df)
      #noundata.df[2]=NULL
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
      print(list1)
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
      actor.df$total <- sapply(actor.df$Noun, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
      stopw<-stopwords('english')
      stopw<-as.data.frame(stopw)
      mycorpus4 <- Corpus(VectorSource(stopw$stopw))
      mycorpus4<-tm_map(mycorpus4,removePunctuation)
      stopw<-as.list(mycorpus4)
      i<-1
      while(i<=(nrow(actor.df)))
      {
        #print(i)
        if(as.numeric(actor.df[i,3])==1)
        {
          for(j in stopw)
          {
            if(trim(actor.df[i,1])==j)
            {
              actor.df<-actor.df[-c(i),]
            }
          }
        }
        i<-i+1
      }
      actor.df[3]=NULL
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
      print(class(data))
      print(data)
      data$total <- sapply(data$noun_phrase, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
      print(data)
      stopw<-stopwords('english')
      stopw<-as.data.frame(stopw)
      mycorpus5 <- Corpus(VectorSource(stopw$stopw))
      mycorpus5<-tm_map(mycorpus5,removePunctuation)
      stopw<-as.list(mycorpus5)
      i<-1
      while(i<=(nrow(data)))
      {
        #print(i)
        if(as.numeric(data[i,3])==1)
        {
          for(j in stopw)
          {
            if(trim(data[i,1])==j)
            {
              data<-data[-c(i),]
            }
          }
        }
        i<-i+1
      }
      data[3]=NULL
      return(data)
    }
    verb_phrase<-function() {
      rs <- dbSendQuery(con,"SELECT * from prior_values_verb");
      data <- fetch(rs, n=-1)
      dbClearResult(dbListResults(con)[[1]])
      data <- data[rev(order(as.numeric(data$prior))),]
      data <- unique(data)
      data$total <- sapply(data$verb_phrase, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
      print(data)
      stopw<-stopwords('english')
      stopw<-as.data.frame(stopw)
      mycorpus5 <- Corpus(VectorSource(stopw$stopw))
      mycorpus5<-tm_map(mycorpus5,removePunctuation)
      stopw<-as.list(mycorpus5)
      i<-1
      while(i<=(nrow(data)))
      {
        #print(i)
        if(as.numeric(data[i,3])==1)
        {
          for(j in stopw)
          {
            if(trim(data[i,1])==j)
            {
              data<-data[-c(i),]
            }
          }
        }
        i<-i+1
      }
      data[3]=NULL
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

    
#### PASSWORD server code ---------------------------------------------------- 
  # reactive value containing user's authentication status
  user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                               user_locked_out = FALSE, status = "")

  # authenticate user by:
  #   1. checking whether their user name and password are in the credentials 
  #       data frame and on the same row (credentials are valid)
  #   2. if credentials are valid, retrieve their lockout status from the data frame
  #   3. if user has failed login too many times and is not currently locked out, 
  #       change locked out status to TRUE in credentials DF and save DF to file
  #   4. if user is not authenticated, determine whether the user name or the password 
  #       is bad (username precedent over pw) or he is locked out. set status value for
  #       error message code below
  observeEvent(input$login_button, {
    credentials <- readRDS("credentials/credentials.rds")
    
    row_username <- which(credentials$user == input$user_name)
    row_password <- which(credentials$pw == digest(input$password)) # digest() makes md5 hash of password

    # if user name row and password name row are same, credentials are valid
    #   and retrieve locked out status
    if (length(row_username) == 1 && 
        length(row_password) >= 1 &&  # more than one user may have same pw
        (row_username %in% row_password)) {
      user_input$valid_credentials <- TRUE
      user_input$user_locked_out <- credentials$locked_out[row_username]
    }

    # if user is not currently locked out but has now failed login too many times:
    #   1. set current lockout status to TRUE
    #   2. if username is present in credentials DF, set locked out status in 
    #     credentials DF to TRUE and save DF
    if (input$login_button == num_fails_to_lockout & 
        user_input$user_locked_out == FALSE) {

      user_input$user_locked_out <- TRUE
            
      if (length(row_username) == 1) {
        credentials$locked_out[row_username] <- TRUE
        
        saveRDS(credentials, "credentials/credentials.rds")
      }
    }
      
    # if a user has valid credentials and is not locked out, he is authenticated      
    if (user_input$valid_credentials == TRUE & user_input$user_locked_out == FALSE) {
      user_input$authenticated <- TRUE
    } else {
      user_input$authenticated <- FALSE
    }

    # if user is not authenticated, set login status variable for error messages below
    if (user_input$authenticated == FALSE) {
      if (user_input$user_locked_out == TRUE) {
        user_input$status <- "locked_out"  
      } else if (length(row_username) > 1) {
        user_input$status <- "credentials_data_error"  
      } else if (input$user_name == "" || length(row_username) == 0) {
        user_input$status <- "bad_user"
      } else if (input$password == "" || length(row_password) == 0) {
        user_input$status <- "bad_password"
      }
    }
  })   

  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({
    wellPanel(
      textInput("user_name", "User Name:"),
      
      passwordInput("password", "Password:"),

      actionButton("login_button", "Log in")
    )
  })

  # red error message if bad credentials
  output$pass <- renderUI({
    if (user_input$status == "locked_out") {
      h5(strong(paste0("Your account is locked because of too many\n",
                       "failed login attempts. Contact administrator."), style = "color:red"), align = "center")
    } else if (user_input$status == "credentials_data_error") {    
      h5(strong("Credentials data error - contact administrator!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_user") {
      h5(strong("User name not found!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_password") {
      h5(strong("Incorrect password!", style = "color:red"), align = "center")
    } else {
      ""
    }
  })  
})