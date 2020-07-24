library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)


# The list of valid books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")


# Function definition to read file
getFreq <- function(book, stopwords = TRUE) {

  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  text
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  print(text)
  return(text)
}



# task4: add in getFreq function for pre-processing

# task6: add in shinythemes function

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    # task2: add in the inputs in the sidebarPanel
    sidebarPanel(
      selectInput(inputId = "bookname","Choose a book:",books),
      checkboxInput(inputId = "stopwords","Stop words:",value=TRUE),
      actionButton(inputId = "Rerun","Run"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput(inputId = "maxwords","Max # of Words",
                  min = 10,  max = 200, value = 100, step = 10),
      sliderInput(inputId = "largestword","Size of largest words:",
                  min = 1, max = 8, value = 4),
      sliderInput(inputId = "smallestword","Size of smallest words:",
                  min = 0.1, max = 4, value = 0.5),
      hr(),
      h3("Word Count Settings"),
      sliderInput(inputId = "minwordcount","Minimum Words for Counts Chart:",
                  min = 10, max = 100, value = 25),
      sliderInput(inputId = "wordsize","Words size for Counts Chart:",
                  min = 8, max = 30, value = 14)
      
    ),
    # task1: within the mainPanel, create .two tabs (Word Cloud and Frequency)
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", plotOutput("cloud",height = "600px")),
        tabPanel("Word Counts", plotOutput("freq",height = "600px"))
      )
    )
  )
  
  
  
  
  # task3: add in the outputs in the sidebarPanel
  
  # task6: and modify your figure heights
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  
  

  freq <- eventReactive(input$Rerun,
                # Change when the "Rerun" button is pressed
                {
                  withProgress({
                    setProgress(message = "Processing corpus...")
                    getFreq(input$bookname,input$stopwords) # ... = replace with the two inputs from Task 2
                    
                  })
                  
                })
  
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$smallestword, input$largestword),
          random.order = FALSE, 
          max.words = input$maxwords, 
          colors=pal))
    
  }) 
  
  output$freq <- renderPlot({
    wcount <- getFreq(input$bookname,input$stopwords)
    wcount1 <- wcount %>% head(input$minwordcount)
    ggplot(data = wcount1, aes(reorder(word,n), y = n, fill=word)) + coord_flip() +
      geom_bar(stat = "identity") + 
      theme(axis.title = element_text(size = rel(input$wordsize)), axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = input$wordsize),legend.position = "none",
            panel.background = element_rect(fill = "white", colour = "grey50"))
  })
}

shinyApp(ui = ui, server = server)
