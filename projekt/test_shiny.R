library(shiny)
library(DT)
library(ggplot2)

foo <- function(komentar) {
  message(komentar)
}

# Define UI
ui <- shinyUI(fluidPage(
  titlePanel('Nelinearne transakcije'),
  
  sidebarPanel(
  fileInput('target_upload', 'Naloži datoteko:',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              '.csv'
            )),
  checkboxInput("header", "Moja tabela ima glavo", TRUE),
  radioButtons("separator","Podatki so ločeni z: ",choices = c(";",",",":"), selected=";",inline=TRUE),
  DT::dataTableOutput("sample_table")),
  
 mainPanel(plotOutput("plot", width = "800px", height = '600px'),
           actionButton("btn","Pokaži parametre"))
           #textOutput("text"))
)
)

# Define server logic
server <- shinyServer(function(input, output) {
  
  branje_datoteke <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    # if (ncol(inFile) != 2)
    #   return(NULL)
    df <- read.csv(inFile$datapath, header = input$header,sep = input$separator)
    colnames(df) <- c('Price','Profit')
    return(df)
  })
  
  output$sample_table<- DT::renderDataTable({
    df <- branje_datoteke()
    DT::datatable(df)
  })
  
  output$plot <- renderPlot({
    source('C:/Users/aanja/OneDrive/Dokumenti/fmf/magisterij/matematika z racunalnikom/Nelinearne-transakcije/projekt/dolgo porocilo/funkcija.R', local = TRUE)
    df <- branje_datoteke()
    price <- as.numeric(gsub(",", ".", df$Price))
    profit <- as.numeric(gsub(",", ".", df$Profit))
    
    df <- data.frame(price,profit)
    df <- df[order(df$price, decreasing = FALSE),]
    price <- df$price
    profit <- df$profit
    plot(x = price,
         y = profit,
         xlab = "Cena (v EUR/MWh)",
         ylab = "Profit (v EUR)",
         main = "Nakup call opcije",
         pch = 20, cex=1.5)
    abline(h = 0, lty='dashed')
    
    opt_fit(price,profit)


  })
  
  # observeEvent(input$btn, {
  #   withCallingHandlers(
  #     opt_fit(price,profit),
  #     foo(komentar),
  #     message = function(komentar) output$text <- renderPrint(m$komentar)
  #   )
  # })
  
}
)

# Run the application 
shinyApp(ui = ui, server = server)

