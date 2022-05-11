library(shiny)
library(DT)
library(tidyverse)
library(bslib)
source('C:/Users/aanja/OneDrive/Dokumenti/fmf/magisterij/matematika z racunalnikom/Nelinearne-transakcije/Nelinearne-transakcije/projekt/dolgo porocilo/funkcija.R', local = TRUE)

# Define UI
ui <- shinyUI(fluidPage(
                
  titlePanel(title = 'PROJEKT'),
  
  navbarPage(title = "Nelinearne transakcije", 
             theme = bs_theme(version = 4, bootswatch = "minty")
             # <add tabPanels etc. from here on>)
             ),
  
  sidebarLayout(
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
            checkboxInput("opcija", "Nariši opcijo", value = FALSE),
            actionButton("parametri", "Pokaži parametre"),
            verbatimTextOutput("text"),
            plotOutput("plot2", width = "800px", height = '600px'))
  )
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
  
  output$sample_table <- DT::renderDataTable({
    df <- branje_datoteke()
    DT::datatable(df)
  })
  
  data <- reactive({
    df <- branje_datoteke()
    price <- as.numeric(gsub(",", ".", df$Price))
    profit <- as.numeric(gsub(",", ".", df$Profit))
    df <- data.frame(price,profit)
    df <- df[order(df$price, decreasing = FALSE),]
    price <- as.numeric(df$price)
    profit <- as.numeric(df$profit)
    list(price = price, profit = profit)
})
  
  output$plot <- renderPlot({
    podatki = branje_datoteke()
    if (!is.null(podatki)){
      if (!input$opcija){
      plot(x = data()$price,
           y = data()$profit,
           xlab = "Cena (v EUR/MWh)",
           ylab = "Profit (v EUR)",
           pch = 20, cex=1.5)
      abline(h = 0, lty='dashed')
      }
      else {
        plot(x = data()$price,
             y = data()$profit,
             xlab = "Cena (v EUR/MWh)",
             ylab = "Profit (v EUR)",
             pch = 20, cex=1.5)
        abline(h = 0, lty='dashed')
        opt_fit(data()$price,data()$profit)
      }
    }
  })
  
  #mogoče bi narisala opcijo kar na isti graf, ampak neznam naštimat da se nariše na isto sliko
  
  # narisi <- eventReactive(input$opcija, {
  #   plot(x = data()$price,
  #        y = data()$profit,
  #        xlab = "Cena (v EUR/MWh)",
  #        ylab = "Profit (v EUR)",
  #        pch = 20, cex=1.5)
  #   abline(h = 0, lty='dashed')
  #   opt_fit(data()$price,data()$profit)
  #   })
  # 
  # output$plot2 <- renderPlot({
  #   narisi()
  # })
  # 
  klik <- eventReactive(input$parametri, {
    print(opt_fit(data()$price,data()$profit))
  })
    
  output$text <- renderPrint({
    klik()
  })
}
)

shinyApp(ui = ui, server = server)



#TEŽAVE:
# na začetku že takoj izpiše need finite xlim values, zakaj?
# kako usposbit gumb parametri
# zakaj mi rmd ne generira pdf datoteke?
# kako naredim, da aplikacija laufa v splošnem kjerkoli?


