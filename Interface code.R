# Load necessary libraries
library(shiny)
library(shinythemes)  # For adding a theme to the app
library(ggplot2)
library(caret)

# UI
ui <- fluidPage(
  theme = shinytheme("superhero"),  # Add a more colorful and modern theme
  titlePanel("Heart Disease Prediction Tool", windowTitle = "Heart Disease Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Input Your Health Details"),  # Section title with some styling
      numericInput("age", "Age:", value = 45, min = 1, max = 120),
      numericInput("restingBP", "Resting Blood Pressure (mm Hg):", value = 120),
      numericInput("cholesterol", "Cholesterol (mg/dl):", value = 200),
      numericInput("maxHR", "Maximum Heart Rate:", value = 150),
      numericInput("oldpeak", "Oldpeak (ST Depression):", value = 1.0),
      
      selectInput("sex", "Sex:", choices = c("Male" = 1, "Female" = 0)),
      selectInput("chestPain", "Chest Pain Type:", 
                  choices = c("Typical Angina" = 1, "Atypical Angina" = 2, 
                              "Non-Anginal Pain" = 3, "Asymptomatic" = 4)),
      selectInput("restingECG", "Resting ECG:", choices = c("Normal" = 1, 
                                                            "ST-T Abnormality" = 2, 
                                                            "LV Hypertrophy" = 3)),
      selectInput("exerciseAngina", "Exercise Induced Angina:", 
                  choices = c("Yes" = 1, "No" = 0)),
      selectInput("stSlope", "ST Slope:", choices = c("Up" = 1, "Flat" = 2, "Down" = 3)),
      
      actionButton("predict", "Predict", class = "btn-success")  # Green button for prediction
    ),
    
    mainPanel(
      h3("Prediction Results"),
      textOutput("result"),
      textOutput("probability"),
      br(),
      img(src = "https://www.verywellhealth.com/thmb/q-J3fzEBeNCugytzCghFIURvAa4=/500x350/filters:no_upscale():max_bytes(150000):strip_icc()/heart-health-checkup-72423872-59187e445f9b58647013de5b.jpg", 
          height = '200px', width = '350px', alt = "Heart Health Image")  # Adding an image for visuals
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(input$predict, {
    # Prepare user inputs as a data frame
    user_input <- data.frame(
      Age = input$age,
      RestingBP = input$restingBP,
      Cholesterol = input$cholesterol,
      MaxHR = input$maxHR,
      Oldpeak = input$oldpeak,
      Sex = as.numeric(input$sex),
      ChestPainType = as.numeric(input$chestPain),
      ExerciseAngina = as.numeric(input$exerciseAngina),
      ST_Slope = as.numeric(input$stSlope)
    )
    
    # Predict the class ("Yes" or "No") using type = "raw"
    class_prediction <- predict(logmod, newdata = user_input, type = "raw")
    
    # Predict the probability using type = "prob"
    prob_prediction <- predict(logmod, newdata = user_input, type = "prob")[, 2]  # Probability of "Yes"
    
    # Binary result (yes/no)
    result <- ifelse(class_prediction == 1, "Yes", "No")
    probability <- round(prob_prediction * 100, 2)
    
    # Display results
    output$result <- renderText({ paste("Heart Disease Prediction: ", result) })
    output$probability <- renderText({ paste("Probability of Yes: ", probability, "%") })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
