library(shiny)
library(ggplot2)
library(GGally)

data(mtcars)
dataset <- mtcars
dataset$cyl<-as.factor(dataset$cyl)
dataset$am<-as.factor(dataset$am)
dataset$gear<-as.factor(dataset$gear)
dataset$vs<-as.factor(dataset$vs)
dataset$carb<-as.factor(dataset$carb)

shinyServer(
  function(input, output) {

    #Draw boxplot based/reactive on user input
    output$boxplot <- renderPlot({  
        if(length(input$xAxis) == 0) return
        
        # Outcome is fixed
        yAxis <- 'mpg'

        
        # Generating initial frame for ggplot
        chartVar <- ggplot(data = dataset) 
        
        # Populate description for x and y
        x <- input$xAxis
        y <- yAxis
      
        # Populate box plot
          chartVar <- chartVar +
            geom_boxplot(aes_string(x = input$xAxis, 
                                    fill = input$xAxis,  
                                    y = yAxis)) +
            labs(x = input$xAxis,
                 y = yAxis,
                 title = paste(y,'versus',x))
        
        # Finally printing graph
        print(chartVar) 
      
    }, height = 700)
    
    
    #Draw ggplot based/reactive on user input
    output$plot <- renderPlot({
      
      p <- ggplot(dataset, aes_string(x=input$x, y=input$y)) + geom_point()
      
      if (input$size != 'None') {
        p <- p + aes_string(size=input$size)
      }
      
      if (input$color != 'None') 
        p <- p + aes_string(color=input$color)
      
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .')
        p <- p + facet_grid(facets)
      
      if (input$smooth)
        p <- p + geom_smooth()
      print(p)
      
    }, height=700)
    
    #Draw pairplot based/reactive on user input
    output$pairPlot <- renderPlot({
      
      bool_vec<-c(input$P_mpg,
                  input$P_cyl,
                  input$P_disp,
                  input$P_hp,
                  input$P_drat,
                  input$P_wt,
                  input$P_qsec,
                  input$P_vs,
                  input$P_am,
                  input$P_gear,
                  input$P_carb)
      
      if(sum(bool_vec)>1) {
        columns <- 1:ncol(dataset)
        columns <- columns[bool_vec]
        p <- ggpairs(dataset, columns = columns)
        print(p)
      }
    }, height = 700)
  }
)