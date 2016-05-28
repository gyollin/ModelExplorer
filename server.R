
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  
  # summary table of data
  output$values <- renderTable({ df }, include.rownames=FALSE)
  
  # Generate model summaries
  output$BestModSum <- renderPrint({ summary(best.model) })
  output$BetterModSum <- renderPrint({ summary(better.model) })
  output$BaselineModSum <- renderPrint({ summary(base.model) })
  
  # explore data
  output$DataSum <- renderPrint({ summary(dat[,input$var]) })
  output$DataPlot <- renderPlot({
    x <- dat[,input$var]
    if( class(x)=="factor" ) {
      plot(x,ylab="count",main=input$var)
    } else {
      hist(x,ylab="count",xlab="",main=input$var)
    }
  })
  output$ScatterPlot <- renderPlot({
    x <- dat[,input$var1]
    y <- dat[,input$var2]
    plot(x=x,y=y,xlab=input$var1,ylab=input$var2)
  })
  
})
