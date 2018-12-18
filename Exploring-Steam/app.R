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
library(lubridate)
library(tidyverse)
library(tidytext)
library(dplyr)
library(wordcloud)
library(utils)

# Import Data
Steam_sales <- read.csv("Steam_sales.csv")
Steam_playtime <- read.csv("Steam_playtime.csv")
Current_players <- read.csv("Current_players.csv")
StorePrices_PUBG <- read.csv("StorePrices_PUBG.csv")
raw.text <- read.csv("raw.text.csv")
Steam <- read.csv("Steam.csv")
Steam_sales <- Steam_sales[,-1]
Steam_playtime <- Steam_playtime[,-1]
Current_players <- Current_players[,-1]
StorePrices_PUBG <- StorePrices_PUBG[,-1]
Steam <- Steam[,-1]
dataset <- Steam_sales


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
      menuItem("SALES", tabName = "1", icon = icon("fas fa-credit-card", lib = "font-awesome")),
      menuItem("REVIEWS", tabName = "2", icon = icon("fas fa-comments", lib = "font-awesome")),
      menuItem("TOP 100 GAMES", tabName = "3", icon = icon("fas fa-gamepad", lib = "font-awesome")),
      menuItem("BENFORD ANALYSIS", tabName = "4", icon = icon("fas fa-info", lib = "font-awesome")),
      menuItem("PRICES - PUBG", tabName = "5", icon = icon("fas fa-motorcycle", lib = "font-awesome")),
      menuItem("DATA EXPLORER", tabName = "6", icon = icon("fas fa-info", lib = "font-awesome"))
    )
  ),

  ## Body content
  dashboardBody(
    tabItems(

      # First tab content
      tabItem(
        tabName = "1",
        plotOutput('plot'),
        fluidRow(column(width = 4, offset = 1,
                        selectInput('x', 'X', names(dataset), "Price"),
                        selectInput('y', 'Y', names(dataset), names(dataset)[[4]])),
                 column(width = 4, offset = 1,
                        selectInput('color', 'Color', names(dataset), "Sales"),
                        selectInput('size', 'Size', c('None', names(dataset)))
                        ))
      ),      

      # Second tab content
      tabItem(
        tabName = "2",
        fluidRow(column(width = 3,
                        box(width = NULL,height = 550,
                            sliderInput("freq","Minimum Frequency:",min = 1,  max = 10, value = 1),
                            sliderInput("max","Maximum Number of Words:",min = 1,  max = 300, value = 300))),
                 column(width = 9,
                        box(width = NULL,title="Word Cloud",solidHeader = TRUE,
                            plotOutput("word",height = 500)))
        )
      ),
      
      # Third tab content
      tabItem(
        tabName = "3",
        tableOutput('table1'),
        fluidPage(titlePanel("TOP 100 GAMES"),
                  sidebarLayout(
                    sidebarPanel(width = 3,
                      selectInput("dataset", "TOP BY:",
                          choices = c("SALES", "PLAYTIME", "CURRENT PLAYERS")),
                      downloadButton("downloadData", "Download")
                      ),
                    mainPanel(
                      tableOutput("table")
                      )
                    )
                  )
        ),

      
      # Fourth tab content
      tabItem(
        tabName = "4",
        fluidRow(column(width = 12,
                        box(title = "Benford Analysis",width = NULL,solidHeader = TRUE,
                            plotOutput("benford",height = 400))),
                 column(width = 12,
                        infoBoxOutput("benfordresult",width = NULL))
        )
      ),
      
      # Fifth tab content
      tabItem(
        tabName = "5",
        numericInput("maxrows4", "Rows to show:", 25),
        box(title = "PRICES - PUBG", collapsible = TRUE,
            solidHeader = TRUE, DT::dataTableOutput("table4"), width = 12, height = 450),
        downloadButton("downloadCsv1", "Download as CSV")
      ),
      
      # Sixth tab content
      tabItem(
        tabName = "6",
        numericInput("maxrows5", "Rows to show:", 10),
        box(title = "DATA EXPLORER", collapsible = TRUE,
            solidHeader = TRUE, DT::dataTableOutput("table5"), width = 12, height = 450)
      )
    )
  )
)

# Define server 
server <- function(input, output) {

  # First tab content
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

  # Second tab content
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

  # Third tab content
  datasetInput <- reactive({
    switch(input$dataset,
           "SALES" = Steam_sales[1:100,-c(7,8)],
           "PLAYTIME" = Steam_playtime,
           "CURRENT PLAYERS" = Current_players)
  })
  
  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )

  # Fourth tab content
  output$benford <- renderPlot({
    library(benford.analysis)
    bfd <- benford(Steam_sales$Sales)
    plot(bfd)
  })
  
  myresult <- "According to the plot, we can get the conclusion that there are some discrepancies on this data. This data doesn't obey to the Benford's law consistently. From the result of duplicates data, the most probably duplicated number is 2000, over 20% of games sold 2000 times during the past half of year which is extremely weird. It might caused by recording in round or the sales data appears on Steam is not convincing."
  output$benfordresult <- renderInfoBox({
    infoBox(
      "Result", myresult, fill = TRUE
    )
  })

  # Fifth tab content
  output$table4 <- DT::renderDataTable({
    tabledata <- StorePrices_PUBG[1:input$maxrows4, ]
    DT::datatable(tabledata, options = list(searching = TRUE, pageLength = 50, lengthMenu = c(5000, 10000), scrollX = T, scrollY = "300px"), rownames = FALSE)
  })
  
  output$downloadCsv1 <- downloadHandler(
    filename = "PUBG.csv",
    content = function(file) {
      write.csv(StorePrices_PUBG, file)
    },
    contentType = "text/csv"
  )

  # Sixth tab content
  output$table5 <- DT::renderDataTable({
    tabledata <- Steam[1:input$maxrows5, ]
    DT::datatable(tabledata, options = list(searching = TRUE, pageLength = 50, lengthMenu = c(50, 100, 500), scrollX = T, scrollY = "300px"), rownames = FALSE)
  })

}
# Run the application
shinyApp(ui = ui, server = server)