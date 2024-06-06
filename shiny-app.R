install.packages(c("shiny", "ggplot2"))
library(shiny)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Adjustable Random Variable Generator and Model Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Number of Observations:", min = 1, max = 1000, value = 100),
      sliderInput("A_min", "Min A:", min = 0, max = 100, value = 18),
      sliderInput("A_max", "Max A:", min = 0, max = 100, value = 90),
      sliderInput("B_min", "Min B:", min = 0, max = 1, value = 0),
      sliderInput("B_max", "Max B:", min = 0, max = 1, value = 1),
      sliderInput("C_min", "Min C:", min = 0, max = 200, value = 50),
      sliderInput("C_max", "Max C:", min = 0, max = 200, value = 100),
      sliderInput("D_min", "Min D:", min = 100, max = 250, value = 150),
      sliderInput("D_max", "Max D:", min = 100, max = 250, value = 200),
      sliderInput("E_min", "Min E:", min = 0, max = 1, value = 0),
      sliderInput("E_max", "Max E:", min = 0, max = 1, value = 1),
      
      actionButton("generate", "Generate Data"),
      downloadButton("downloadData", "Download Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", tableOutput("table")),
        tabPanel("Histograms", plotOutput("histograms")),
        tabPanel("Model Summary", verbatimTextOutput("modelSummary")),
        tabPanel("P-values", plotOutput("pvalues"))
      )
    )
  )
)


server <- function(input, output) {
  generate_random_data <- function(n, limits, steps) {
    data <- data.frame(matrix(ncol = length(limits), nrow = n))
    colnames(data) <- names(limits)
    
    for (var in names(limits)) {
      if (steps[[var]] > 0) {
        seq_values <- seq(limits[[var]][1], limits[[var]][2], by = steps[[var]])
        seq_values <- sample(seq_values, n, replace = TRUE)
        data[[var]] <- seq_values
      } else {
        data[[var]] <- runif(n, limits[[var]][1], limits[[var]][2])
      }
    }
    return(data)
  }
  
  limits <- reactive({
    list(
      A = c(input$A_min, input$A_max),
      B = c(input$B_min, input$B_max),
      C = c(input$C_min, input$C_max),
      D = c(input$D_min, input$D_max),
      E = c(input$E_min, input$E_max)
    )
  })
  
  steps <- list(A = 1, B = 1, C = 0, D = 0, E = 1)
  
  data <- eventReactive(input$generate, {
    generate_random_data(input$n, limits(), steps)
  })
  
  output$table <- renderTable({
    data()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("random_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  output$histograms <- renderPlot({
    df <- data()
    p1 <- ggplot(df, aes(x = A)) + geom_histogram(binwidth = 1) + ggtitle("Histogram of A")
    p2 <- ggplot(df, aes(x = B)) + geom_histogram(binwidth = 0.1) + ggtitle("Histogram of B")
    p3 <- ggplot(df, aes(x = C)) + geom_histogram(binwidth = 5) + ggtitle("Histogram of C")
    p4 <- ggplot(df, aes(x = D)) + geom_histogram(binwidth = 5) + ggtitle("Histogram of D")
    p5 <- ggplot(df, aes(x = E)) + geom_histogram(binwidth = 0.1) + ggtitle("Histogram of E")
    gridExtra::grid.arrange(p1, p2, p3, p4, p5, ncol = 2)
  })
  
  output$modelSummary <- renderPrint({
    df <- data()
    model <- lm(A ~ B + C + D + E, data = df)
    summary(model)
  })
  
  output$pvalues <- renderPlot({
    df <- data()
    model <- lm(A ~ B + C + D + E, data = df)
    pvalues <- summary(model)$coefficients[, 4]
    pvalues_df <- data.frame(term = names(pvalues), pvalue = pvalues)
    ggplot(pvalues_df, aes(x = term, y = pvalue)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = 0.05, color = "red") +
      ggtitle("P-values of the Model Terms")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

