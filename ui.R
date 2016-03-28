library(shiny)
library(ggplot2)

shinyUI(fluidPage(
    # Application title
    title = "Map for Napoleon March to Moscow",

    plotOutput('minardPlot'),
    hr(),

    fluidRow(

        column(3,
               h4("Troop Info by City"),
               selectInput("city", "City:", choice=unique(df$city),
                           selected=unique(df$city)[5] )
        ),
        column(5,
               verbatimTextOutput("text2")
        )
    )
 ))

