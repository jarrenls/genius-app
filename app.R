
# Setup -------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(genius)
library(tidyverse)
library(DT)


# UI ----------------------------------------------------------------------

ui <- fluidPage(
    
    useShinyjs(),
    
    # Application title
    titlePanel("Geniuis Lyrics Tidy Data Extractor"),
    
    # Sidebar
    tabsetPanel(
        tabPanel("Instructions", 
                 br(),
                 fluidRow(
                     column(12,
                            includeMarkdown("instructions.md"),
                            textInput("url", h4("Enter URL here."), value = NA),
                            actionButton("submit", "Submit"),
                            actionButton("reset", "Clear")
                     )
                 ),
                 fluidPage(
                     column(12,
                            hr(),
                            mainPanel(
                                uiOutput("ui"),
                                width = 12
                            )
                     )
                 )
        ),
        tabPanel("Credits",
                 br(),
                 fluidRow(
                     column(12,
                            includeMarkdown("credits.md")
                     )
                 ))
    )
    
    
)



# Server ------------------------------------------------------------------

server <- function(input, output) {
    
    v <- reactiveValues(valid_url = NULL)
    
    observeEvent(
        input$submit, {
            v$valid_url <- length(grep("genius", input$url))
        }
    )
    
    observeEvent(
        input$reset, {
            reset("url")
            v$valid_url <- NULL
        }
    )
    
    output$ui <- 
        renderUI({
            if (is.null(v$valid_url)) {
                return()
            }
            if (length(grep("genius", input$url)) != 1) {
                htmlOutput("error")
            }
            else {
                fluidPage(
                    dataTableOutput("table"),
                    downloadButton("download", "Download")
                )
            }
        })
    
    output$error <- 
        renderUI({
            HTML(
                paste(
                    tags$span(style = "color:red", 
                              "You have entered an invalid Genius URL.")
                )
            )})
    
    output$table <- 
        renderDataTable(datatable({
            v$data <- genius_url(input$url, 
                                 info = "all")
            v$data
        }))
    
    output$download <- downloadHandler(
        filename = function () {
            paste0(unique(v$data$artist), 
                   "_",
                   unique(v$data$track_title),
                   ".csv")
        },
        content = function(file) {
            write.csv(v$data, file, row.names = FALSE)
        }
    )
}


# Run the App -------------------------------------------------------------

shinyApp(ui = ui, server = server)
