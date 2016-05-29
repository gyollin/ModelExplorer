#Plot 2-way lift chart
library(ggplot2)

TwoWay <- function(model1 = NULL, model2= NULL, data = NULL, n = 10){
#   if(class(model1) == "list" & class(model2) == "list") {
#     pred <- data.frame(Prob1 = model1$fitted.values,
#                        Prob2 = model2$fitted.values,
#                        Surr = data$Surr, Exp = data$Exp)
#     
#   } else {
  pred <- data.frame(Prob1 = predict(model1, type = "response", newdata = data, 
                                     n.trees = model1$n.trees),
                     Prob2 = predict(model2, type = "response", newdata = data,
                                     n.trees = model2$n.trees),
                     Surr = data$Surr, Exp = data$Exp)
  # }
  pred <- mutate(pred, Ratio = Prob1/Prob2)
  Tot.Surr <- sum(pred$Surr)
  pred.order <- arrange(pred, Ratio)
  pred.order.bucket <- try(mutate(pred.order, buckets = cut_number(Ratio, n = n)), silent = TRUE)
#   if(class(pred.order.bucket) == "try-error"){
#    pred.order.bucket <- mutate(pred.order, Ratio = Ratio + runif(n = nrow(pred.order))* 2^-10)
#    pred.order.bucket <- mutate(pred.order.bucket, buckets = cut_number(Ratio, n = 100))
#   }
  pred.order.bucket <- group_by(pred.order.bucket, buckets)
  AE <- dplyr::summarize(pred.order.bucket, Actual = sum(Surr*Exp)/sum(Exp), 
                  Expected.1 = mean(Prob1), Expected.2 = mean(Prob2), 
                  AE.1 = Actual/Expected.1, AE.2 = Actual/Expected.2)

  
  if(sd(AE$AE.1) > sd(AE$AE.2)) {
    plot(AE$AE.1, type = 'n', ylab = "Actual/Expected", xlab = "Rank of Model1/Model2", las = 1)
  } else {
  plot(AE$AE.2, type = 'n', ylab = "Actual/Expected", xlab = "Rank of Model1/Model2", las = 1)
  }
  abline(h=1)

  lines(AE$AE.1, col = "red", lwd = 1)
  lines(AE$AE.2, col = "blue", lwd = 1)

}



