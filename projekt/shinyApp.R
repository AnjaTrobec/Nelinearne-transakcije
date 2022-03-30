library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Izberi CSV datoteko", accept = ".csv"),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ";"),
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
    df <- read.csv("podatki.csv", sep=';', header = TRUE, stringsAsFactors=FALSE)
    
    
    # df <- read.csv(file$datapath,
    #                header = input$header,
    #                sep = input$sep)
    # 
    # if(input$disp == "head") {
    #   return(head(df))
    # }
    # else {
    #   return(df)
    }
    
    # price <- as.numeric(gsub(",", ".", df$Price))
    # profit <- as.numeric(gsub(",", ".", df$Profit))
    # plot(x = price,
    #      y = profit,
    #      xlab = "Cena (v €)",
    #      ylab = "Profit (v tisoè €)",
    #      main = "Cena vs Profit")
    # abline(h = 0, col='blue')
    # }
)
}

shinyApp(ui, server)

