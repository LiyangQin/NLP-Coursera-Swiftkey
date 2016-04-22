#  -----------------------------------------------------------------------------
#  COURSERA JHU - DATA SCIENCE CAPSTONE
#  Next Word Prediction App
#  File: app.R
#  (c) 2016 - Enrique Pérez Herrero
#  GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007
#  21/Apr/2016
#  -----------------------------------------------------------------------------

library(ggplot2)
library(shiny)
library(shinythemes)
library(RColorBrewer)
library(wordcloud)

source("prediction.R")

# 1. SERVER --------------------------------------------------------------------

server <- function(input, output, session) {
  predicted_text <- reactive(predict_word(input$text_in_id_1))
  output$text_out_id_1 <- predicted_text
  # Clear
  observeEvent(input$button_1, {
    updateTextInput(session, "text_in_id_1",  value = "")
    })
  # Use predicted
  observeEvent(input$button_2, { 
    updateTextInput(session, "text_in_id_1",
                    value = paste(input$text_in_id_1, predicted_text()))
  })
  
  select_table <- reactive({
    eval(parse(text = input$ngrams))
    })
  output$ngrams_table <- renderDataTable(
    select_table(), options = list(pageLength = 10)
  )
  

  select_table2 <- reactive({
    eval(parse(text = input$ngrams2))
  })
  
  output$ggplot_ngram <- renderPlot({
    df <- head(select_table2() , 20)
    my_color <- brewer.pal(4, "Dark2")[c(1:4)[ngrams_names == input$ngrams2]]
    if(input$ngrams2 != "unigrams"){
      df$words <- paste(df$sentence, df$prediction, sep = "_")
    }
    ggplot(df, aes(reorder(words, frequency), frequency)) +
      geom_bar(stat = "identity", fill = my_color) +
      coord_flip() +
      xlab("") +
      ylab("") +
      theme_minimal()
  })
  
  
  select_table3 <- reactive({
    eval(parse(text = input$ngrams3))
  })
  
  output$display_wordcloud <- renderPlot({
    df <- head(select_table3() , 20)
    palette = brewer.pal(8, "Dark2")
    if(input$ngrams3 != "unigrams"){
      df$words <- paste(df$sentence, df$prediction, sep = "_")
    }
    my_scale <- c(1, .1) * (c(4:1)[ngrams_names == input$ngrams3]) * 1.7
    wordcloud(words = df$words,
              freq = df$frequency,
              colors = palette,
              scale = my_scale)
    })
  
}

# 2. UI ------------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  tags$hr(),
  titlePanel("NEXT WORD PREDICTION"),
  tags$hr(),
  
  mainPanel(tabsetPanel(
    #---------------------------------------------------------------------------
    tabPanel("Prediction",
             sidebarLayout(
               sidebarPanel(
                 tags$p(""),
                 tags$h3("Predicted next word:"),
                 wellPanel(h1(textOutput("text_out_id_1"), align = "center")),
                 fluidRow(
                   column(width = 4,  offset = 0,
                          actionButton("button_1",
                                       label = "Clear",
                                       icon = icon("refresh"))
                          ),
                   column(width = 4, offset = 3,
                          actionButton("button_2",
                                       label = "Add",
                                       icon = icon("plus-square"))
                   )
            )
      ),
      mainPanel(
      tags$p(""),
      tags$h3("Please, enter your text:"),
      h4(tags$textarea(id = "text_in_id_1", rows = 10, cols = 40, "")))
      )),
    
    
    #---------------------------------------------------------------------------
    tabPanel("N-grams tables",
             sidebarLayout(
               sidebarPanel(
                 selectInput("ngrams",
                             "N-gram table:",
                             ngrams_names)
                 ),
             mainPanel(dataTableOutput("ngrams_table"))
             )
    ), 
    
    #---------------------------------------------------------------------------
    tabPanel("Word Cloud",
             sidebarLayout(
               sidebarPanel(
                 selectInput("ngrams3",
                             "N-gram table:",
                             ngrams_names)
               ),
               mainPanel(plotOutput("display_wordcloud"))
               )
    ),
    
    #---------------------------------------------------------------------------
    tabPanel("N-grams Plot",
             sidebarLayout(
               sidebarPanel(
                 selectInput("ngrams2",
                             "N-gram table:",
                             ngrams_names)
               ),
               mainPanel(plotOutput("ggplot_ngram"))
               )
    ), 
    
    #---------------------------------------------------------------------------
    tabPanel('Help', includeMarkdown('help.Rmd'))
             
    )
)
)

# 3. APP -----------------------------------------------------------------------

shinyApp(ui = ui, server = server)
