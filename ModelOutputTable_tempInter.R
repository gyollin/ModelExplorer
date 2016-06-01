# model <- better.model
# base <- .01


ModelOutputTable_temp <- function(model, base = .01) {

  # Initialize dataframe  
  VIM.overall <- data.frame(matrix(nrow = length(coef(model)), ncol = 9))
  names(VIM.overall) <- c("Variable", "Coefficient", "pvalue", "Class", 
                          "Min", "Max", "VIM", "VarGroup", "Interaction")

  vars <- names(model$model)
  levels <- data.frame(sapply(vars, grepl, x = names(coef(model))))
  
  levels.count <- colSums(levels)
  levels$VarCount <- rowSums(levels)
  levels$VarName <- ""
  
  #levels.mult <- levels[which(levels.count > 1)]
  
  for (i in 1:length(coef(model))){
    if (levels$VarCount[i] == 1){
      levels$VarName[i] <- vars[which(levels[i,]==TRUE)[1]]
    }else {
      if (levels$VarCount[i] > 1){
      levels$VarName[i] <- paste0(vars[which(levels[i,]==TRUE)][1], ":", vars[which(levels[i,]==TRUE)][2])
      }
    }
  }  
  find.inter <- grep(":", names(coef(model)))
  VIM.overall$Interaction <- 0
  VIM.overall$Interaction[find.inter] <- 1
  VIM.overall$Class[find.inter] <- "interaction"

  # Filli n variable names, coefficients, p-values
  VIM.overall$Variable <- names(coef(model))
  VIM.overall$Coefficient <- coef(model)
  VIM.overall$pvalue <- coef(summary(model))[,4]
  
  VIM.overall$VarGroup <- levels$VarName
  
  # Find special cases
#  find.func <- grep("==", names(coef(model)))
    
  # Fill in row for Intercept (actually ignore as a VIM component)
#  VIM.overall[1, "VIM"] <- VIM.overall$Coefficient[1]
  
  for (i in 2:length(coef(model))) {
    # i <- 3
    if(VIM.overall$Interaction[i] == 0){
      VIM.overall[i, "Class"] <- attr(terms(model), "dataClasses")[VIM.overall$VarGroup[i]]
    }
    
    # if numeric, can find min, max, .05 and .95
    if(VIM.overall$Class[i] == "numeric"){
      VIM.overall[i, "Min"] <- min(model$model[,names(coef(model))[i]])
      VIM.overall[i, "Max"] <- max(model$model[,names(coef(model))[i]])
      VIM.overall[i, "VIM"] <- abs((quantile(model$model[,names(coef(model))[i]], 0.95) - 
                                      quantile(model$model[,names(coef(model))[i]], 
                                               0.05))*coef(model)[i])
      # if interaction, refine range
    } else if (i %in% find.inter) {
      inter.vars <- unlist(strsplit(VIM.overall$VarGroup[i], ":"))
      if((attr(terms(model), "dataClasses")[inter.vars[1]]) == "numeric"){
        inter.1 <- model$model[,inter.vars[1]]
      } else {
        inter.1 <- 1
      }
      if((attr(terms(model), "dataClasses")[inter.vars[2]]) == "numeric"){
        inter.2 <- model$model[,inter.vars[2]]
      } else {
        inter.2 <- 1
      }
      inter <- inter.1 * inter.2
        
      VIM.overall[i, "VIM"] <- round((quantile(inter, 0.95) - 
                                        quantile(inter, 0.05))*coef(model)[i], 4)
      VIM.overall[i, "Min"] <- min(inter)
      VIM.overall[i, "Max"] <- max(inter)

      # if factor, assign 0 and 1
      }else{
      VIM.overall[i, "Min"] <- 0
      VIM.overall[i, "Max"] <- 1
      VIM.overall[i, "VIM"] <- abs(VIM.overall$Coefficient[i])
      # To do: introduce grouping for VIM via group_by call
      }
  }
  VIM.overall <- mutate(VIM.overall,
                        Multiplicative = (exp(log(base/(1-base))+
                                                VIM)/(1+exp(log(base/(1-base))+VIM)))/base,
                        Direction = sign(Coefficient))
  
  VIM.overall <- arrange(VIM.overall,
                         desc(Multiplicative))
  
  VIM.overall <- mutate(VIM.overall, 
                        Rank = row_number())

  return(VIM.overall)
}

# rm(VIM.overall, levels, levels.count, find.inter, find.func, vars, levels.mult, n)