ui <- fluidPage(
  titlePanel("Dataset Uploader"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("binsX","Enter no. of bins for X",
                  min = 1,max = 50,
                  value = 25),
      sliderInput("binsY","Enter no. of bins for Y",
                  min = 1,max = 50,
                  value = 25),
      fileInput(inputId = "file", "Choose file to upload:",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values",
                  "text/tab-separated-values",
                  "text/plain",
                  ".csv",
                  ".tsv")
      ),
      tableOutput("contents")      
    ),
    mainPanel(
      fluidRow(
        column(6,plotOutput("histx",width = "300px",height = "300px")),
        column(6,plotOutput("histy",width = "300px",height = "300px")),
        column(10,plotOutput("xyplot",width = "600px",height = "400px"))
      )   
    )
  )
)




server <- function(input, output) {
  filedata <- reactive({
    inFile <- input$file
    if (is.null(inFile))   return(NULL)
    read.csv(inFile$datapath,header = TRUE,sep = ",")
  })
  output$contents <- renderTable({
    inFile <- input$file
    if (is.null(inFile))   return(NULL)
    filedata()[,1:2]
  })
  output$xyplot <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))    return(NULL)
    plot(filedata()[,1],filedata()[,2],xlab = "X Values",ylab = "Y Values",
         pch=19,col="red",main = "The relationship between X and Y values")
  })
  output$histx <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))    return(NULL)
    hist(filedata()[,1],col = "wheat1",xlab = "X values",ylab = "Count",
         main = "Distribution of X values")
  })
  output$histy <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))    return(NULL)
    hist(filedata()[,2],col="lightblue",xlab = "Y values",ylab = "Count",
         main = "Distribution of Y values")
  })
}

shinyApp(ui = ui, server = server)

