#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Author: Mira(Jing) Tang
# Date: 12/10/2018

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(lubridate)
library(tidyverse)
library(tidytext)
library(dplyr)
library(wordcloud)

# Import Data
Steam_sales <- read.csv("Steam_sales.csv")
Steam_playtime <- read.csv("Steam_playtime.csv")
Current_players <- read.csv("Current_players.csv")
StorePrices_PUBG <- read.csv("StorePrices_PUBG.csv")
raw.text <- read.csv("raw.text.csv")
Steam <- read.csv("Steam.csv")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = "Exploring Steam",
    titleWidth = 300,

    ## Dropdown menu - team message
    dropdownMenu(
      type = "messages",
      messageItem(
        from = "Mira Tang",
        message = "jingtang@bu.edu"
      )
    )
  ),
  ## Sidebar content
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("SALES", tabName = "1", icon = icon("fas fa-money-checkf-alt", lib = "font-awesome")),
      menuItem("REVIEWS", tabName = "2", icon = icon("fas fa-comments", lib = "font-awesome")),
      menuItem("TOP 100 GAMES", tabName = "3", icon = icon("fas fa-gamepad", lib = "font-awesome")),
      menuItem("PRICES - PUBG", tabName = "4", icon = icon("fas fa-motorcycle", lib = "font-awesome")),
      menuItem("DATA EXPLORER", tabName = "5", icon = icon("fas fa-info", lib = "font-awesome"))
    )
  ),

  ## Body content
  dashboardBody(
    tabItems(

      # First tab content
      tabItem(
        tabName = "1",
        plotOutput('plot'),
        fluidRow(column(4, offset = 1,
                        selectInput('x', 'X', names(dataset), "Price"),
                        selectInput('y', 'Y', names(dataset), "Owners After"),
                        selectInput('color', 'Color', names(dataset), "Sales"),
                        selectInput('size', 'Size', c('None', names(dataset)))
                        ))
      ),      

      # Second tab content
      tabItem(
        tabName = "2",
        fluidRow(column(width = 6,
                        box(width = NULL,height = 550,
                            sliderInput("freq","Minimum Frequency:",min = 1,  max = 10, value = 1),
                            sliderInput("max","Maximum Number of Words:",min = 1,  max = 300, value = 300))),
                 column(width = 6,
                        box(width = NULL,title="Word Cloud",solidHeader = TRUE,
                            plotOutput("word",height = 500)))
        )
      ),
      
      # Third tab content
      tabItem(
        tabName = "3",
        tableOutput('table1'),
        fluidRow(column(width = 12,
                        radioButtons("TOP_BY", "TOP BY:",
                                     c("SALES" = "SALES",
                                       "PLAYTIME" = "PLAYTIME",
                                       "CURRENT_PLAYERS" = "CURRENT PLAYERS"),"SALES"),
                        box(width = 12,title = "TOP 100 GAMES", collapsible = TRUE,
                            solidHeader = TRUE, DT::dataTableOutput("table1"), height = 450),
                        numericInput("maxrows1", "Rows to show:", 25)),
                 conditionalPanel(
                   condition = "input.TOP_BY == 'SALES'",
                   column(width = 6)
                 ),
                 conditionalPanel(
                   condition = "input.TOP_BY == 'PLAYTIME'",
                   column(width = 6)
                 ),
                 conditionalPanel(
                   condition = "input.TOP_BY == 'CURRENT_PLAYERS'",
                   column(width = 6)
                 )
               )
        ),

      
      # Fourth tab content
      tabItem(
        tabName = "4",
        numericInput("maxrows4", "Rows to show:", 25),
        box(title = "PRICES - PUBG", collapsible = TRUE,
            solidHeader = TRUE, DT::dataTableOutput("table4"), width = 12, height = 450),
        downloadButton("downloadCsv1", "Download as CSV")
      ),
      
      # Fifth tab content
      tabItem(
        tabName = "5",
        numericInput("maxrows5", "Rows to show:", 10),
        box(title = "DATA EXPLORER", collapsible = TRUE,
            solidHeader = TRUE, DT::dataTableOutput("table5"), width = 12, height = 450),
        downloadButton("downloadCsv2", "Download as CSV")
      )
    )
  )
)

# Define server 
server <- function(input, output, session) {

  # Tab 1 scatter plot
  output$plot <- renderPlot({
    dataset <- Steam_sales

    if (input$size == "None") {
      size <- 1
    } else {
      size <- dataset[, input$size]
    }


    ggplot(data = dataset) +
      aes(
        x = dataset[, input$x],
        y = dataset[, input$y],
        color = dataset[, input$color],
        size = size
      ) +
      geom_point() +
      scale_color_distiller(palette = "Spectral") +
      theme_minimal() +
      labs(
        x = input$x,
        y = input$y,
        color = input$color,
        size = input$size
      )
  })

  # Tab 2 Word Cloud
  output$word <- renderPlot({
    raw.text$X <- NULL
    raw.text$text <- as.character(raw.text$text)

    tidy_review <- raw.text %>%
      unnest_tokens(word, text) %>%
      mutate(word = str_extract(word, "[a-z'\\s]+")) %>%
      anti_join(stop_words, by = "word") %>%
      filter(
        !word %in% "[\\s]+",
        !word %in% "",
        !word %in% NA,
        !word %in% "Posted",
        !word %in% "posted",
        !word %in% "game"
      ) %>%
      count(word)

    wordcloud(
      words = tidy_review$word, freq = tidy_review$n, min.freq = input$freq,
      max.words = input$max, random.order = FALSE, rot.per = 0.35,
      colors = brewer.pal(8, "Dark2")
    )
  })

  # Tab 3 Top 100 Games
  output$table1 <- DT::renderDataTable({
    if (input.TOP_BY == "SALES") {
      tabledata <- Steam_sales[1:input$maxrows1, ]
      DT::datatable(tabledata, options = list(searching = TRUE, pageLength = 50, lengthMenu = c(50, 100, 500), scrollX = T, scrollY = "300px"), rownames = FALSE)
    }

    else if (input.TOP_BY == "PLAYTIME") {
      tabledata <- Steam_playtime[1:input$maxrows1, ]
      DT::datatable(tabledata, options = list(searching = TRUE, pageLength = 50, lengthMenu = c(50, 100, 500), scrollX = T, scrollY = "300px"), rownames = FALSE)
    }

    else if (input.TOP_BY == "CURRENT_PLAYERS") {
      tabledata <- Current_players[1:input$maxrows1, ]
      DT::datatable(tabledata, options = list(searching = TRUE, pageLength = 50, lengthMenu = c(50, 100, 500), scrollX = T, scrollY = "300px"), rownames = FALSE)
    }
  })

  # Tab 4 PUBG
  output$table4 <- DT::renderDataTable({
    tabledata <- StorePrices_PUBG[1:input$maxrows4, ]
    DT::datatable(tabledata, options = list(searching = TRUE, pageLength = 50, lengthMenu = c(50, 100, 500), scrollX = T, scrollY = "300px"), rownames = FALSE)
  })

  # Tab 4 PUBG csv file
  output$downloadCsv1 <- downloadHandler(
    filename = "PUBG.csv",
    content = function(file) {
      write.csv(StorePrices_PUBG, file)
    },
    contentType = "text/csv"
  )

  # Tab 5 Steam Explorer
  output$table5 <- DT::renderDataTable({
    tabledata <- Steam[1:input$maxrows5, ]
    DT::datatable(tabledata, options = list(searching = TRUE, pageLength = 50, lengthMenu = c(50, 100, 500), scrollX = T, scrollY = "300px"), rownames = FALSE)
  })

  # Tab 5 Steam csv file
  output$downloadCsv2 <- downloadHandler(
    filename = "STEAM.csv",
    content = function(file) {
      write.csv(Steam, file)
    },
    contentType = "text/csv"
  )
}
# Run the application
shinyApp(ui = ui, server = server)