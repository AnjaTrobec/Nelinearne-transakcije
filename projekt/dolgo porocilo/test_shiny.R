library(shiny)
library(DT)
library(tidyverse)
library(bslib)
source('C:/Users/aanja/OneDrive/Dokumenti/fmf/magisterij/matematika z racunalnikom/Nelinearne-transakcije/Nelinearne-transakcije/projekt/dolgo porocilo/funkcija.R', local = TRUE)

# Define UI
ui <- shinyUI(fluidPage(
  
  theme = bs_theme(version = 4, bootswatch = "minty"),
                
  titlePanel(title = 'Nelinearne transakcije'),
  
  
  sidebarLayout(
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
           actionButton("btn","Pokaži parametre"),
           textOutput("text"))
))

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
    df <- branje_datoteke()
    
    price <- as.numeric(gsub(",", ".", df$Price))
    profit <- as.numeric(gsub(",", ".", df$Profit))
    df <- data.frame(price,profit)
    df <- df[order(df$price, decreasing = FALSE),]
    price <- as.numeric(df$price)
    profit <- as.numeric(df$profit)
    
    plot(x = price,
         y = profit,
         xlab = "Cena (v EUR/MWh)",
         ylab = "Profit (v EUR)",
         pch = 20, cex=1.5)
    abline(h = 0, lty='dashed')
    
    opt_fit(price,profit)
  })
  
  klik <- eventReactive(input$btn, {
    print(opt_fit(price,profit)[1]) 
    print(opt_fit(price,profit)[2])
  })
    
  output$text <- renderText({
    klik()
  })
  
  #output$text <- renderText({outputArgs = list(print(opt_fit(price,profit)[1]),print(opt_fit(price,profit)[2]))})
  
}
)


shinyApp(ui = ui, server = server)



#TEŽAVE:
# na začetku že takoj izpiše need finite xlim values, zakaj?
# zakaj moja minty tema ne dela :(
# a obstaja kakšen simpl trik, da zgleda aplikacija bolj proper
# kako usposbit gumb parametri
# zakaj mi rmd ne generira pdf datoteke?
# kako naredim, da aplikacija laufa v splošnem kjerkoli?
