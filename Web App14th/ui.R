library(shiny)
library(twitteR)

shinyUI(fluidPage(
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
)

