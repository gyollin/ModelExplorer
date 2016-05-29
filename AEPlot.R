# A/E plot script

# for testing
#model = step30
#data.hld <- data.hold
#xvar = "TotFeesRiders"
#xvar = "AV"
#hold = FALSE
#color = "red"
#use.probs = FALSE
#probs = NULL
#filter = NULL
#sampladj = sampadj
#downscale = prop.all/prop.down

#tested
#q - integer: ok
#Comm - character: ok
#TotFeesRiders - numeric: if numeric then bucket
#GMDBInd - factor: if factor then sort
#EffState - factor: if factor then sort


AEPlot <- function(model = NULL, data.hld = NULL, xvar = NULL, hold = FALSE, color = "red",
                   use.probs = FALSE, probs = NULL, filter = NULL, sampladj = 0, downscale = 1,
                   AandE = "FALSE"){
  # Capture data types in dataset and of variable to be modeled along x-axis
  # Drives differences for numeric variables that need to be bucketed
  data.names <- names(data.hld)
  index.classes <- c()
  for (i in 1:length(data.names)) {
    index.classes[i] <- class(data.hld[, i])
  }
  xvar.class <- index.classes[which(data.names %in% xvar)]
  
  # Option to use other probabilties
  if(!use.probs) {
    # Option to filter by SCPeriod
    if(!is.null(filter)){
      data.hld <- filter(data.hld, SCPeriod == filter)
      if (class(model) == "list") {
        pred <- data.frame(Prob = model$fitted.values)
      } else {
        pred <- data.frame(Prob = predict(model, type = "link", newdata = data.hld))
      }
    } else {
      if (class(model)[1] == "list") {
        pred <- data.frame(Prob = model$fitted.values)
      } else {
        pred <- data.frame(Prob = predict(model, type = "link", newdata = data.hld))
      }
    }
    # Translate predicted log odds into probability
    #pred <- mutate(pred, Prob = (exp(Prob))/(1 + exp(Prob)))
    pred <- mutate(pred, Prob = (exp(Prob - sampladj))/(1 + exp(Prob - sampladj)))
    #pred <- mutate(pred, Exp2 = exp(data.hld$lnexp))
    
    # Join dataframe onto predictions
    pred <- cbind(pred, data.hld)
    
    # If numeric create buckets by generating dummy histogram (not plotted)
    if(xvar.class == "numeric"){
      xvar.breaks <- hist(data.hld[,which(data.names %in% xvar)], plot = FALSE)$breaks
      pred <- mutate(pred, Temp = data.hld[,which(data.names %in% xvar)])
      pred <- mutate(pred, TempGroup = data.hld[,which(data.names %in% xvar)])
      for (i in 1:length(xvar.breaks)){pred$TempGroup[which(pred$Temp > xvar.breaks[i])] <- xvar.breaks [i + 1]}
      pred.group <- group_by_(pred, "TempGroup")
    }else{
      # Otherwise use group by
      pred.group <- group_by_(pred, xvar)
    }
    
    # Summarize modeled and actual lapse rates by groups
    AE <- dplyr::summarize(pred.group, Actual = sum(Surr*Exp)/sum(Exp), Expected = sum(Prob*Exp)/sum(Exp), Count = n())
    #AE <- mutate(AE, Actual = log(Actual/(1-Actual)))
    #AE <- mutate(AE, Actual = exp(Actual - sampladj)/(1 + exp(Actual - sampadj)))
    AE <- mutate(AE, Ratio = Actual/Expected)
    #AE <- filter(AE, Dur <= 25)
  }
  if(use.probs){
    if(!is.null(filter)){
      probs <- filter(probs, SCPeriod == filter)
      probs.group <- group_by(probs, xvar)
      AE <- summarize(probs.group, Actual = downscale * sum(Surr*Exp)/sum(Exp), Expected = mean(Prob), Count = length(Surr))
      AE <- mutate(AE, Ratio = Actual/Expected)
      #AE <- filter(AE, Dur <= 25)
    } else {
      probs.group <- group_by(probs, xvar)
      AE <- dplyr::summarize(probs.group, Actual = downscale * sum(Surr*Exp)/sum(Exp), Expected = mean(Prob), Count = length(Surr))
      AE <- mutate(AE, Ratio = Actual/Expected)
      #AE <- filter(AE, Dur <= 25)
    }
  }
  if(AandE == FALSE) {
    if(xvar.class == "numeric"){
      xvar.plot <- unlist(as.vector(AE[, "TempGroup"]))
    } else {
      xvar.plot <- unlist(as.vector(AE[, xvar]))
    }
    if(!hold) {
      #plot(xvar.plot, AE$Ratio, type = 'n', ylab = "Actual vs. Expected", xlab = xvar, ylim = c(0.5,1.5))
      plot(xvar.plot, AE$Ratio, type = 'n', ylab = "Actual vs. Expected", xlab = xvar)
      abline(h = 1)
    }
    if(xvar.class == "numeric"){
      lines(xvar.plot, AE$Ratio, col = color, lwd = 2)
    }else{
      lines(AE$Ratio, col = color, lwd = 2)
    }
  } else {
    if(xvar.class == "numeric"){
      xvar.plot <- unlist(as.vector(AE[, "TempGroup"]))
    } else {
      xvar.plot <- unlist(as.vector(AE[, xvar]))
    }
    if(!hold) {
      plot(xvar.plot, AE$Actual, type = 'n', ylab = "Actual And Expected", xlab = xvar)
      abline(h = 1)
    }
    if(xvar.class == "numeric"){
      lines(xvar.plot, AE$Actual, col = "black", lwd = 2)
      lines(xvar.plot, AE$Expected, col = "blue", lwd = 2)
    }else{
      lines(AE$Actual, col = "black", lwd = 2)
      lines(AE$Expected, col = "blue", lwd = 2)
    }
  }
  #AE
}



