library(shiny)
library(twitteR)

shinyUI(fluidPage(
    titlePanel("Word Cloud"),
    sidebarLayout(
      sidebarPanel(
        textInput("term", "Enter a term", ""),
        sliderInput("cant", "Select a number of tweets",min=5,max=100, value = 5),
        actionButton("update","Show Top Actors")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Tweets",tableOutput("table1")),
          tabPanel("Top Actors",tableOutput("table2"))
        )
      )
    )
  )
)

