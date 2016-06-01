
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(dplyr)

max.col  <- 255
green    <- rgb(142,167,128, maxColorValue = max.col)
green.a  <- rgb(142,167,128,50, maxColorValue = max.col)
blue     <- rgb(10,73,119, maxColorValue = max.col)
charcoal <- rgb(57,65,77, maxColorValue = max.col)
slate    <- rgb(198,201,202, maxColorValue = max.col)
cols     <- c(green, blue, charcoal, slate)

shinyServer(function(input, output, session) {
  
  # update ui
  updateUI<-function(var) {
    show.density <- var %in% c('Surr','SCPeriod','SCPhase','RiderCode')
    is.checked <- input$kde
    if(!show.density) {
      renderUI({
        checkboxInput('kde', 'Show Density', is.checked)
      })
    } else {
      renderUI({

      })
    }
  }
  
  # update ui
  updateRadio<-function(selected) {
    selected2 <- input$LiftModell
    options2 <- ModelList[!(selected == ModelList)]
    selected.index <- ModelList[which(ModelList == selected2)]
    if(selected.index > 1) {
        updateRadioButtons(session, 'LiftModel2', label = "2nd Model:", choices = options2, selected = options2[1])
    } else {
        updateRadioButtons(session, 'LiftModel2', label = "2nd Model:", choices = options2, selected = options2[2])
    }
  }
  
  # update ui
  observe({
    
    selected1 <- input$LiftModell
    
    updateRadio(selected1)
    
  })
  
  observe({
    output$show_density<-updateUI(input$var)
  })
  
  # summary table of data
  output$values <- renderTable({ df }, include.rownames=FALSE)
  
  # Generate model summaries
  output$BestModSum <- renderPrint({ summary(best.model) })
  output$BetterModSum <- renderPrint({ summary(better.model) })
  output$BaselineModSum <- renderPrint({ summary(base.model) })
  
  # explore data
  output$DataSum <- renderPrint({ summary(dat[,input$var]) })
  output$DataPlot <- renderPlot({
    
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
      
      custom.bin <- NULL
      
      if(input$var == 'q') {
        custom.bin <- 1
      }
      
      if(input$kde) {
        
        x.levels = c(input$var, 'Density')
        x.cols <- setNames(c(blue, green), x.levels)
        
        ggplot(x.df, aes(x, fill = input$var)) + geom_histogram(aes(y=..density..), color = slate, binwidth = custom.bin) +
          geom_density(color = slate, aes(fill = 'Density'), alpha = 0) +
          geom_density(color = green, fill = rgb (0,0,0,0), size = 1.2) +
          labs(title='', x=input$var, y='Density') + 
          scale_fill_manual(name='', values=x.cols)
        
      } else {
        
        x.levels = input$var
        x.cols <- setNames(blue, x.levels)
        
        ggplot(x.df, aes(x, fill = input$var)) + geom_histogram(color = slate, binwidth = custom.bin) +
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

  # AE Plot  
  output$AEPlot <- renderPlot({
    mod <- switch(which(input$AEmodel == ModelList),"best.model","better.model","base.model")
    AandE <- ifelse(input$AEcheck,"TRUE","FALSE")
     AEPlot(get(mod), 
      data.hld = testing,
      xvar = input$AEvar, 
      AandE = AandE)
  })
  
  # Two Way Lift Plot  
  output$LiftPlot <- renderPlot({
    mod1 <- switch(which(input$LiftModell == ModelList),"best.model","better.model","base.model")
    mod2 <- switch(which(input$LiftModel2 == ModelList),"best.model","better.model","base.model")
    TwoWay(get(mod1),get(mod2),data=testing,n=20) 
  })

  # predict
  data <- reactive({
    PredSCPeriod <- as.numeric(input$PredSCPeriod)
    mydf <- data.frame(
      SCPeriod = factor(input$PredSCPeriod,levels=c("4","7")),
      SCPhase = factor(ifelse(input$Predq < PredSCPeriod*4,"IN",
        ifelse(input$Predq > PredSCPeriod*4,"OUT","END")),
        levels = c("END","IN","OUT")),
      q = input$Predq,
      ITM = input$PredBB/input$PredAV,
      BB = log(input$PredBB),
      Age = input$PredAge,
      AV = log(input$PredAV),
      RiderCode = factor(input$PredRiderCode,levels=c("A","B","C"))
    )
    pred.best <- predict(best.model,newdata=mydf,type="response",se.fit=TRUE)
    pred.better <- predict(better.model,newdata=mydf,type="response",se.fit=TRUE)
    pred.base <- predict(base.model,newdata=mydf,type="response",se.fit=TRUE)
    pred <- data.frame(
      Model=ModelList,
      Probability=c(pred.best$fit,pred.better$fit,pred.base$fit),
      "Standard Error"=c(pred.best$se.fit,pred.better$se.fit,pred.base$se.fit)
    )
  })
  
  output$PredDf <- renderTable({
    data()
  },include.rownames=FALSE)
  output$PredBarplot <- renderPlot({
    barplot(height=data()[,"Probability"],names.arg=data()[,"Model"])
  })
  
})