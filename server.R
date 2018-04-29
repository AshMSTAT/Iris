
server <- function(input, output, session){
  
  Preds <- reactive({
    generatePreds(
       SepalLength = input$SepalLength
      ,SepalWidth = input$SepalWidth
      ,PetalLength = input$PetalLength
      ,PetalWidth = input$PetalWidth
    )
  })
  
  output$pred_table <- DT::renderDataTable({
    Preds() %>%
      datatable() %>%
      formatPercentage(columns = 'preds', digits = 2)
  })
  
  # scatterplot of sepal length vs sepal width
  sepalscatter <- reactive({
    ggplot(data=iris,aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + 
      geom_point(aes(shape=Species), size=1.5) + 
      scale_color_manual(values = c("orange", "purple", "green")) + 
      theme(legend.position=c(.1,1),legend.justification=c(1,1)) +
      geom_smooth(method="loess") +
      annotate("point", x = input$SepalLength, y = input$SepalWidth, size=3, colour = "red")
    })
  
  # marginal density of sepal length - plot on top
  sepalplot_top <- reactive({
    ggplot(data=iris,aes(x=Sepal.Length, fill = Species)) + 
      geom_density(alpha=.5) + 
      scale_fill_manual(values = c("orange", "purple","green")) + 
      theme(legend.position = "none")+
      geom_vline(xintercept = input$SepalLength, col="red", lwd=1, lty=2)
  })
  
  # marginal density of sepal width- plot on the right
  sepalplot_right <- reactive({
    ggplot(data=iris,aes(x=Sepal.Width, fill = Species)) + 
      geom_density(alpha=.5) + coord_flip() + 
      scale_fill_manual(values = c("orange", "purple","green")) + 
      theme(legend.position = "none") +
      geom_vline(xintercept = mean(input$SepalWidth), col="red", lwd=1, lty=2)
  })
  
    output$sepalplot <-  renderPlot({grid.arrange(sepalplot_top(), empty, 
                                                  sepalscatter(),
                                                  sepalplot_right(), 
                                                  ncol=2, nrow=2, 
                                                  widths=c(4, 1), 
                                                  heights=c(1, 4))
                                    })
  
  
  
  
  
    
    # scatterplot of petal length vs petal width
    petalscatter <- reactive({
      ggplot(data=iris,aes(x=Petal.Length, y=Petal.Width, color=Species)) + 
      geom_point(aes(shape=Species), size=1.5) + 
      scale_color_manual(values = c("orange", "purple", "green")) + 
      theme(legend.position=c(.1,1),legend.justification=c(1,1)) +
      geom_smooth(method="loess") +
      annotate("point", x = mean(input$PetalLength), y = mean(input$PetalWidth), size=3, colour = "red")
    })
    
    
    # marginal density of petal length - plot on top
    petalplot_top <- reactive({
      ggplot(data=iris,aes(x=Petal.Length, fill = Species)) + 
      geom_density(alpha=.5) + 
      scale_fill_manual(values = c("orange", "purple","green")) + 
      theme(legend.position = "none") +
      geom_vline(xintercept = mean(input$PetalLength), col="red", lwd=1, lty=2)
    })
    
    
    # marginal density of petal width - plot on the right
    petalplot_right <- reactive({
      ggplot(data=iris,aes(x=Petal.Width, fill = Species)) + 
      geom_density(alpha=.5) + coord_flip() + 
      scale_fill_manual(values = c("orange", "purple","green")) + 
      theme(legend.position = "none") +
      geom_vline(xintercept = mean(input$PetalWidth), col="red", lwd=1, lty=2)
    })

    
    output$petalplot <- renderPlot({grid.arrange(petalplot_top(), empty, 
                                                 petalscatter(), 
                                                 petalplot_right(), 
                                                 ncol=2, nrow=2, 
                                                 widths=c(4, 1), 
                                                 heights=c(1, 4))
    
  })
  

  
  
}


