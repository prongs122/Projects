ui <- fluidPage(
  titlePanel("CSV 2-Variable Dataset Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("axes","Transpose Scatterplot Variables",
                  choices = c("Variable 2 against Variable 1","Variable 1 against Variable 2")),
      fileInput("file", "Choose file to upload:",
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
        column(6,plotOutput("histVar1",width = "300px",height = "300px")),
        column(6,plotOutput("histVar2",width = "300px",height = "300px")),
        column(10,plotOutput("scatplot",width = "600px",height = "300px"))
      )   
    )
  )
)




server <- function(input,output) {
  filedata <- reactive({
    inFile <- input$file
    if (is.null(inFile))   return(NULL)
    read.csv(inFile$datapath,header = TRUE,sep = ",")
  })
  output$contents <- renderTable({
    inFile <- input$file
    if (is.null(inFile))   return(NULL)
    head(filedata()[,1:2])
  })
  output$scatplot <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))    return(NULL)
    x <- filedata()[,1]
    y <- filedata()[,2]
    df <- data.frame(x,y)
    
    if(input$axes == "Variable 2 against Variable 1"){
      ggplot(df,aes(x=x,y=y)) + 
        geom_point() +
        ggtitle("Scatterplot") +
        xlab(names(filedata()[1])) +
        ylab(names(filedata()[2])) +
        theme_economist() +
        theme(plot.title = element_text(size = 18,face = "bold")) +
        theme(axis.title = element_text(size = 12,face = "bold"))
      
    } else if(input$axes == "Variable 1 against Variable 2"){
      ggplot(df,aes(x=y,y=x)) + 
        geom_point() +
        ggtitle("Scatterplot") +
        xlab(names(filedata()[2])) +
        ylab(names(filedata()[1])) +
        theme_economist() +
        theme(plot.title = element_text(size = 18,face = "bold")) +
        theme(axis.title = element_text(size = 12,face = "bold"))
    }
    
    
  })
  output$histVar1 <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))    return(NULL)
    x <- filedata()[,1]
    y <- filedata()[,2]
    df <- data.frame(x,y)
    ggplot(df,aes(x=x)) + 
    geom_histogram(colour="black",fill="lightblue2") +
    xlab(names(filedata()[1])) +
    ggtitle(names(filedata()[1])) +
    theme_dark() +
    theme(axis.title = element_text(size = 12,face = "bold")) +
    theme(plot.title = element_text(size = 18,face = "bold")) 
      
    
  })
  output$histVar2 <- renderPlot({
    inFile <- input$file
    if (is.null(inFile))    return(NULL)
    x <- filedata()[,1]
    y <- filedata()[,2]
    df <- data.frame(x,y)
    ggplot(df,aes(x=y)) + 
    geom_histogram(colour="black",fill="lightblue2") +
    xlab(names(filedata()[2])) +
    ggtitle(names(filedata()[2])) +
    theme_dark() +
    theme(axis.title = element_text(size = 12,face = "bold")) +
    theme(plot.title = element_text(size = 18,face = "bold")) 
  })
}

shinyApp(ui = ui, server = server)