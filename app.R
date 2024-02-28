library(shiny)
library(tidyverse)
library(DT)


data <- read.csv("data/estate.csv")



ui <- fluidPage(
  titlePanel("EDA for Estate Data"),
  tabsetPanel(
    tabPanel("UA - Categorical Variables",
             sidebarLayout(
               sidebarPanel(
                 selectInput("cat_variable", "Select a Variable", choices = c("Bed", "Bath", "AC", "Garage", "Pool", "Year", "Quality", "Style", "Highway"), selected = "Bed"),
                 radioButtons("log_conversion_cat", "Log Conversion:", choices = c("No" = FALSE, "Yes" = TRUE), selected = FALSE)
               ),
               mainPanel(
                 plotOutput("barplot_cat")
               )
             )
    ),
    tabPanel("UA - Numeric Variables",
             sidebarLayout(
               sidebarPanel(
                 selectInput("num_variable", "Select a Variable", choices = c("Price", "Area", "Lot"), selected = "Price"),
                 radioButtons("log_conversion_num", "Log Conversion:", choices = c("No" = FALSE, "Yes" = TRUE), selected = FALSE),
                 sliderInput("num_bins", "Number of Bins", min = 1, max = 50, value = 10),
                 numericInput("null_value", "Null Value for t-test:", value = 0)
               ),
               mainPanel(
                 plotOutput("histogram_plot"),
                 verbatimTextOutput("t_test_results")
               )
             )
    ),
    tabPanel("Bivariate Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable1", "Select First Variable", choices = names(data)),
                 selectInput("variable2", "Select Second Variable", choices = names(data)),
                 checkboxInput("log_scale_var1", "Log Transform First Variable", value = FALSE),
                 checkboxInput("log_scale_var2", "Log Transform Second Variable", value = FALSE),
                 checkboxInput("add_ols_line", "Add OLS Line", value = FALSE)
               ),
               mainPanel(
                 plotOutput("bivariate_plot")
               )
             )
    ),
    tabPanel("Data Table",
             DTOutput("data_table")
    )
  )
)


server <- function(input, output) {
  # Univariate Analysis
  # Categorical Variables
  output$barplot_cat <- renderPlot({
    cat_variable_data <- switch(input$cat_variable,
                                "Bed" = data$Bed,
                                "Bath" = data$Bath,
                                "AC" = data$AC,
                                "Garage" = data$Garage,
                                "Pool" = data$Pool,
                                "Year" = data$Year,
                                "Quality" = data$Quality,
                                "Style" = data$Style,
                                "Highway" = data$Highway
    )
    
    if (input$log_conversion_cat) {
      cat_variable_data <- log(cat_variable_data)
    }
    
    ggplot(data.frame(Variable = cat_variable_data), aes(x = as.factor(Variable))) +
      geom_bar(fill = "lightblue", color = "black") +
      labs(x = input$cat_variable, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)
      )
  })
  
  # Numeric Variables
  output$histogram_plot <- renderPlot({
    num_variable_data <- switch(input$num_variable,
                                "Price" = data$Price,
                                "Area" = data$Area,
                                "Lot" = data$Lot
    )
    
    if (input$log_conversion_num) {
      num_variable_data <- log(num_variable_data)
    }
    
    ggplot(data.frame(Variable = num_variable_data), aes(x = Variable)) +
      geom_histogram(binwidth = (max(num_variable_data) - min(num_variable_data)) / input$num_bins, fill = "lightblue", color = "black") +
      labs(x = input$num_variable, y = "Frequency") +
      theme_minimal()
  })
  
  output$t_test_results <- renderPrint({
    num_variable_data <- switch(input$num_variable,
                                "Price" = data$Price,
                                "Area" = data$Area,
                                "Lot" = data$Lot
    )
    
    if (input$log_conversion_num) {
      num_variable_data <- log(num_variable_data)
    }
    
    t_test_result <- t.test(num_variable_data, mu = input$null_value)
    
    cat("One-sample t-test results for", input$num_variable, ":\n")
    cat("Mean:", round(mean(num_variable_data), 2), "\n")
    cat("Null Value:", input$null_value, "\n")
    cat("T-statistic:", round(t_test_result$statistic, 2), "\n")
    cat("P-value:", format(t_test_result$p.value, digits = 4), "\n")
  })
  
  # Bivariate Analysis
  output$bivariate_plot <- renderPlot({
    variable1 <- sym(input$variable1)
    variable2 <- sym(input$variable2)
    plot_data <- data
    if (input$log_scale_var1) {
      plot_data <- plot_data %>% mutate(!!variable1 := log(!!variable1))
    }
    if (input$log_scale_var2) {
      plot_data <- plot_data %>% mutate(!!variable2 := log(!!variable2))
    }
    
    is_numeric_variable1 <- as_label(variable1) %in% c("Area", "Lot", "Bed", "Bath", "Garage", "Year", "Price")
    is_numeric_variable2 <- as_label(variable2) %in% c("Area", "Lot", "Bed", "Bath", "Garage", "Year", "Price")
    
    if (is_numeric_variable1 && is_numeric_variable2) {
      # Scatter plot 
      ggplot(plot_data, aes(x = !!variable1, y = !!variable2)) +
        geom_point() +
        labs(x = as_label(variable1), y = as_label(variable2)) +
        theme_minimal() +
        geom_smooth(method = "lm", se = FALSE, color = "blue")
    } else {
      # Boxplots
      ggplot(plot_data, aes(x = as.factor(!!variable2), y = !!variable1)) +
        geom_boxplot() +
        labs(x = as_label(variable2), y = as_label(variable1)) +
        theme_minimal()
    }
  })
  
  # Data Table
  output$data_table <- renderDT({
    numeric_data <- data %>% select_if(is.numeric)
    datatable(numeric_data)
  })
}

shinyApp(ui, server)


#References: https://data-science-master.github.io/lectures/10_shiny/10_reactivity.html
#https://rstudio.github.io/DT/
#https://www.bioinformatics.babraham.ac.uk/shiny/Intro_to_Shiny_course/examples/06_tidyverse/
#https://genomicsclass.github.io/book/pages/bioc2_shiny.html
#https://shiny.rit.albany.edu/stat/corrsim/