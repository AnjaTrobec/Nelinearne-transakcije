library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Izberi CSV datoteko", accept = ".csv")
    ),
    mainPanel(
      tableOutput("contents")
    )
  )
)

server <- function(input, output) {
  output$contents <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Izbrana datoteka ne ustreza csv formatu"))
    
    read.csv(file$datapath, sep=';', header = FALSE, stringsAsFactors=FALSE, col.names = c('Price','Profit'))
    
    price <- as.numeric(gsub(",", ".", df$Price))
    profit <- as.numeric(gsub(",", ".", df$Profit))
    plot(x = price,
         y = profit,
         xlab = "Cena (v €)",
         ylab = "Profit (v tisoè €)",
         main = "Cena vs Profit")
    abline(h = 0, col='blue')
    })
  
}

shinyApp(ui, server)

