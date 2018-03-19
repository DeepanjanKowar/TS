library(shiny)
library(twitteR)

shinyUI(fluidPage(
    titlePanel("Word Cloud"),
    sidebarLayout(
      sidebarPanel(
        textInput("term", "Enter a term", ""),
        sliderInput("cant", "Select a number of tweets",min=5,max=80, value = 5),
        actionButton("update","Create Cloud")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Tweets",tableOutput("table1")),
          tabPanel("Noun Cloud",plotOutput("plotgraph1")),
          tabPanel("Verb Cloud",plotOutput("plotgraph2"))
        )
      )
    )
  )
)

