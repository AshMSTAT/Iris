
ui <- fluidPage(
  titlePanel("Predicting IRIS Species"),
  

  
  sidebarLayout(
    sidebarPanel(
      h4("Select Input for Model Predictions"),
      sliderInput("SepalLength", label = "Sepal Length:", value = 5.8
                  , min = 4.3, max = 7.9, step = .1)
      ,sliderInput("SepalWidth", "Sepal Width:", value = 3
                    , min = 2, max = 4.4, step = .1)
      ,sliderInput("PetalLength", "Petal Length:", value = 4.3
                    , min = 1, max = 6.9, step = .1)
      ,sliderInput("PetalWidth", "Petal Width:", value = 1.3
                    , min = 0.1, max = 2.5, step = .1)
      
    ),
    
    mainPanel(
      h4("Scatter and Density Plots"),
      tabsetPanel(
       tabPanel("SEPAL", plotOutput("sepalplot")), 
       tabPanel("PETAL", plotOutput("petalplot")) 
      ),
    hr(),
    h4("Species Prediction for Selected Values"),
    DT::dataTableOutput("pred_table")
    ))
  
  )
