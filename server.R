
server <- function(input, output){
  
  Preds <- reactive({
    generatePreds(
       SepalLength = input$SepalLength
      ,SepalWidth = input$SepalWidth
      ,PetalLength = input$PetalLength
      ,PetalWidth = input$PetalWidth
    )
  })
  

 
  
  output$sepalplot <- renderPlot({
    grid.arrange(sepalplot_top, 
                 empty, 
                 sepalscatter,
                 sepalplot_right, 
                 ncol=2, nrow=2, 
                 widths=c(4, 1), 
                 heights=c(1, 4))
    })
  
  
  
  output$petalplot <- renderPlot({
    grid.arrange(petalplot_top, 
                 empty, 
                 petalscatter, 
                 petalplot_right, 
                 ncol=2, nrow=2, 
                 widths=c(4, 1), 
                 heights=c(1, 4))
  })
  
  output$pred_table <- DT::renderDataTable({
    Preds() %>%
      datatable() %>%
      formatPercentage(columns = 'preds', digits = 2)
  })
  
  
  
}


