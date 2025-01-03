library(shiny)

# Define UI for application
ui <- fluidPage(
  
  # Front Page
  uiOutput("front_page"),
  
  # Login Page
  uiOutput("login_page"),
  
  # Main Application
  uiOutput("app_ui")
  
)

# Define server logic
server <- function(input, output, session) {
  
  # Placeholder for navigation status
  front_page_displayed <- reactiveVal(TRUE)
  logged_in <- reactiveVal(FALSE)
  
  # Sample username and password
  valid_user <- "user"
  valid_password <- "173003"
  
  # Front Page UI
  output$front_page <- renderUI({
    if (front_page_displayed()) {
      fluidPage(
        tags$head(
          tags$style(HTML("\n            body { background-color: #f0f8ff; }\n            .front-page {\n              text-align: center;\n              margin-top: 50px;\n            }\n            .front-page img {\n              max-width: 50%;\n              height: 50%;\n              border-radius: 10px;\n              margin-bottom: 20px;\n            }\n            .front-page h2 {\n              color: #333333;\n              font-family: Arial, sans-serif;\n            }\n          "))
        ),
        div(class = "front-page",
            img(src = "https://www.iop.org/sites/default/files/styles/event_hero_1500x1000px/public/2024-04/Olympic.png?h=06ac0d8c&itok=LFNqRYqp", alt = "Front Page Image"),
            h2("Welcome to the Data Visualization App!"),
            actionButton("proceed_to_login", "Proceed to Login", class = "btn btn-primary")
        )
      )
    }
  })
  
  # Login Page UI
  output$login_page <- renderUI({
    if (!front_page_displayed() && !logged_in()) {
      fluidPage(
        tags$head(
          tags$style(HTML("\n            body { background-color: #f0f8ff; }\n            .login-panel {\n              max-width: 400px;\n              margin: 50px auto;\n              padding: 20px;\n              background-color: #ffffff;\n              border: 1px solid #dcdcdc;\n              border-radius: 10px;\n              box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);\n            }\n            .login-panel h4 {\n              text-align: center;\n              color: #333333;\n            }\n            .login-panel input {\n              margin-bottom: 10px;\n            }\n          "))
        ),
        titlePanel("Login"),
        div(class = "login-panel",
            h4("Please log in to access the application."),
            textInput("username", "Username:"),
            passwordInput("password", "Password:"),
            actionButton("login_btn", "Login", class = "btn btn-primary btn-block")
        )
      )
    }
  })
  
  # Main App UI
  output$app_ui <- renderUI({
    if (logged_in()) {
      fluidPage(
        titlePanel("Data Visualizations"),
        sidebarLayout(
          sidebarPanel(
            selectInput("plot_type", "Select Plot Type:",
                        choices = c("Bar Chart", "Line Chart", "Scatter Plot", "Heat Map")),
            actionButton("generate_plot", "Generate Plot"),
            br(),
            actionButton("predict_btn", "Generate Prediction")
          ),
          mainPanel(
            plotOutput("plot"),
            verbatimTextOutput("prediction")
          )
        )
      )
    }
  })
  
  # Handle Login
  observeEvent(input$login_btn, {
    if (input$username == valid_user && input$password == valid_password) {
      logged_in(TRUE)
    } else {
      showNotification("Invalid credentials, please try again.", type = "error")
    }
  })
  
  # Handle Front Page Navigation
  observeEvent(input$proceed_to_login, {
    front_page_displayed(FALSE)
  })
  
  # Handle Login
  observeEvent(input$login_btn, {
    if (input$username == valid_user && input$password == valid_password) {
      logged_in(TRUE)
    } else {
      showNotification("Invalid credentials, please try again.", type = "error")
    }
  })
  # Render Plots
  observeEvent(input$generate_plot, {
    output$plot <- renderPlot({
      req(logged_in())
      
      dataset <- read.csv("C:\\Users\\Mehak\\OneDrive\\Desktop\\olympics.csv") # Ensure the dataset is in the same directory as the app
      
      switch(input$plot_type,
             "Bar Chart" = {
               barplot(table(dataset$sport), main = "Sports Distribution", col = "blue", las = 2)
             },
             "Line Chart" = {
               plot(aggregate(medal ~ year, data = dataset, sum),
                    type = "l", col = "red", lwd = 2,
                    main = "Trend of Medals Over Years", xlab = "Year", ylab = "Medals")
             },
             "Scatter Plot" = {
               plot(dataset$height, dataset$weight, main = "Height vs Weight",
                    xlab = "Height", ylab = "Weight", pch = 19, col = "pink")
             },
             "Heat Map" = {
               num_data <- dataset[, sapply(dataset, is.numeric)]
               corr <- cor(num_data, use = "complete.obs")
               heatmap(corr, main = "Correlation Heatmap", col = heat.colors(256),
                       symm = TRUE)
             }
      )
    })
  })
}


# Generate Prediction

observeEvent(input$predict_btn, {
  output$prediction <- renderText({
    req(logged_in())  # Ensure the user is logged in
    
    # Load the dataset
    dataset <- read.csv("C:\\Users\\Mehak\\OneDrive\\Desktop\\olympics.csv")
    
    # Check if required columns exist
    if (!all(c("year", "medal") %in% names(dataset))) {
      return("Error: The dataset does not contain 'year' and 'medal' columns.")
    }
    
    # Ensure the columns are numeric
    if (!is.numeric(dataset$year) || !is.numeric(dataset$medal)) {
      return("Error: 'year' and 'medal' columns must be numeric.")
    }
    
    # Aggregate medals by year (if there are duplicates)
    medals_per_year <- aggregate(medal ~ year, data = dataset, sum)
    
    # Check if there's enough data for modeling
    if (nrow(medals_per_year) < 2) {
      return("Error: Not enough data to create a prediction model.")
    }
    
    # Build a linear regression model
    model <- lm(medal ~ year, data = medals_per_year)
    
    # Predict medals for the next year
    last_year <- max(medals_per_year$year, na.rm = TRUE)
    next_year <- last_year + 1
    predicted_medals <- predict(model, newdata = data.frame(year = next_year))
    
    # Output the prediction
    paste("Predicted total medals for the year", next_year, ":", round(predicted_medals, 2))
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

