
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)

max.col  <- 255
green    <- rgb(142,167,128, maxColorValue = max.col)
green.a  <- rgb(142,167,128,50, maxColorValue = max.col)
blue     <- rgb(10,73,119, maxColorValue = max.col)
charcoal <- rgb(57,65,77, maxColorValue = max.col)
slate    <- rgb(198,201,202, maxColorValue = max.col)
cols     <- c(green, blue, charcoal, slate)

shinyServer(function(input, output) {

  
  # summary table of data
  output$values <- renderTable({ df }, include.rownames=FALSE)
  
  # Generate model summaries
  output$BestModSum <- renderPrint({ summary(best.model) })
  output$BetterModSum <- renderPrint({ summary(better.model) })
  output$BaselineModSum <- renderPrint({ summary(base.model) })
  
  # explore data
  output$DataSum <- renderPrint({ summary(dat[,input$var]) })
  output$DataPlot<-renderPlot({
    
    x        <- dat[,input$var]
    x.class  <- class(x)
    x.df     <- data.frame(x)
    
    if (x.class == 'factor') {
      
      x.levels <- levels(x)
      x.cols   <- setNames(cols[1:length(x.levels)], x.levels)
      
      ggplot(x.df, aes(x, fill = x)) + geom_bar() + 
        labs(title='', x=input$var, y='') +
        scale_fill_manual(name=input$var, values=x.cols)
      
    } else {
      
      if(input$kde) {
        
        x.levels = c(input$var, 'Density')
        x.cols <- setNames(c(blue, green), x.levels)
        
        ggplot(x.df, aes(x, fill = input$var)) + geom_histogram(aes(y=..density..), color = slate) +
          geom_density(color = slate, aes(fill = 'Density'), alpha = 0) +
          geom_density(color = green, fill = rgb (0,0,0,0), size = 1.2) +
          labs(title='', x=input$var, y='Density') + 
          scale_fill_manual(name='', values=x.cols)
        
      } else {
        
        x.levels = input$var
        x.cols <- setNames(blue, x.levels)
        
        ggplot(x.df, aes(x, fill = input$var)) + geom_histogram(color = slate) +
          labs(title='', x=input$var, y='Count') + 
          scale_fill_manual(name='', values=x.cols)
      }
      
    }
    
  })
  output$ScatterPlot <- renderPlot({
    x <- dat[,input$var1]
    y <- dat[,input$var2]
    plot(x=x,y=y,xlab=input$var1,ylab=input$var2)
  })
  
})
