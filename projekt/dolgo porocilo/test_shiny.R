library(shiny)
library(DT)

# Define UI
ui <- shinyUI(fluidPage(
  titlePanel('Nelinearne transakcije'),
  
  sidebarPanel(
  fileInput('target_upload', 'Naloži csv datoteko:',
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
    df <- read.csv(inFile$datapath, header = input$header, sep = input$separator)
    colnames(df) <- c('Price','Profit')
    return(df)
  })
  
  output$sample_table<- DT::renderDataTable({
    df <- branje_datoteke()
    DT::datatable(df)
  })
  
  output$plot <- renderPlot({
    source('C:/Users/aanja/OneDrive/Dokumenti/fmf/magisterij/matematika z racunalnikom/Nelinearne-transakcije/Nelinearne-transakcije/projekt/dolgo porocilo/funkcija.R', local = TRUE)
    df <- branje_datoteke()
    
    price <- as.numeric(gsub(",", ".", df$Price))
    profit <- as.numeric(gsub(",", ".", df$Profit))
    
    df <- data.frame(price,profit)
    df <- df[order(df$price, decreasing = FALSE),]
    price <- as.numeric(df$price)
    profit <- as.numeric(df$profit)
    plot(x = price,
         y = profit,
         xlim = c(min(price)-20,max(price)+20),
         ylim = c(min(profit)-20, max(profit)+20),
         xlab = "Cena (v EUR/MWh)",
         ylab = "Profit (v EUR)",
         pch = 20, cex=1.5)
    abline(h = 0, lty='dashed')

    opt_fit(price,profit)


  })
  
}
)

# Run the application 
shinyApp(ui = ui, server = server)

