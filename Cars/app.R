library(shiny)
library(ggplot2)

mtcars$car <- rownames(mtcars)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css")
  ),
  titlePanel("Motor Trend Car Road Tests"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("transmission", "Transmission:",
                   choices = c("All", "Automatic", "Manual"),
                   selected = "All",
                   inline = TRUE),
      sliderInput("mpg", "Miles per Gallon:", 
                  min = min(mtcars$mpg), max = max(mtcars$mpg),
                  value = c(min(mtcars$mpg), max(mtcars$mpg)),
                  step = 0.1),
      sliderInput("hp", "Horsepower:", 
                  min = min(mtcars$hp), max = max(mtcars$hp),
                  value = c(min(mtcars$hp), max(mtcars$hp)),
                  step = 1),
      conditionalPanel(
        condition = "output.plotVisible",
        plotOutput("carCountPlot")
      )
    ),
    mainPanel(
      tableOutput("carTable")
    )
  )
)

server <- function(input, output, session) {
  #Reactive value to track user interaction
  interactionOccurred <- reactiveVal(FALSE)
  
  observeEvent({
    input$transmission
    input$mpg
    input$hp
  }, {
    interactionOccurred(TRUE)
  }, ignoreInit = TRUE)
  
  #Reactive expression to filter the data based on inputs
  filteredData <- reactive({
    data <- mtcars[mtcars$mpg >= input$mpg[1] &
                     mtcars$mpg <= input$mpg[2] &
                     mtcars$hp >= input$hp[1] &
                     mtcars$hp <= input$hp[2], ]
    
    if (input$transmission == "Automatic") {
      data <- data[data$am == 0, ]
    } else if (input$transmission == "Manual") {
      data <- data[data$am == 1, ]
    }
    
    data
  })
  
  #Render the table of filtered data
  output$carTable <- renderTable({
    data <- filteredData()
    
    #Replace numerical values 
    data$am <- ifelse(data$am == 0, "Automatic", "Manual")
    data$vs <- ifelse(data$vs == 0, "V-shaped", "Straight")
    
    #Select relevant columns 
    data <- data[, c("car", "mpg", "cyl", "hp", "wt", "vs", "am")]
    
    # Rename the columns
    colnames(data) <- c("Car Name", "Miles per Gallon", "Number of Cylinders", 
                        "Horsepower", "Weight", "Engine", "Transmission")
    
    data
  })
  
  #Render plot 
  output$carCountPlot <- renderPlot({
    req(interactionOccurred())  # Ensure plot is only rendered after interaction
    data <- filteredData()
    total_cars <- nrow(mtcars)
    selected_cars <- nrow(data)
    
    # Create a data frame for plot
    plot_data <- data.frame(
      category = c("Selected Cars", "Remaining Cars"),
      count = c(selected_cars, total_cars - selected_cars)
    )
    
    #Generate plot
    ggplot(plot_data, aes(x = 2, y = count, fill = category)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#E4E7EC", "#7F56D9")) +
      theme_minimal()+
      theme(legend.position="none", panel.background = element_blank(),
            panel.grid = element_blank(), axis.title = element_blank(),
            axis.text = element_blank(), 
            plot.background = element_rect(fill = "transparent", colour = NA),
            plot.title = element_text(hjust = 0.5, vjust = -5))+
      xlim(0.5, 2.5) +
      ggtitle("Selected Cars")
  }, bg = "transparent")
  
  #Reactive expression to control plot visibility
  output$plotVisible <- reactive({
    interactionOccurred()
  })
  
  outputOptions(output, "plotVisible", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
