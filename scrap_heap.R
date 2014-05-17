conVsToGo <- function(data, distance.spec = 1:20){
      #this function returns a matrix with 3 columns listing the number of yards to go, the number of observations from
      #the data and the converstion rate with that number of yards to go.
      
      result <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("toGo", "nObs", "convRate")))
      
      for (i in 1:threshold){
            subs <- subset(data, togo == i)
            result <- rbind(result, c(i, nrow(subs), sum(subs[,"convert"])/nrow(subs)))
      }
      return(result)
}

runPassVsToGo <- function(data, distance.spec = 1:20){
      #this function returns a matrix with 3 columns listing the number of yards to go, the number of observations from
      #the data and the run% with that number of yards to go.
      
      result <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("toGo", "nObs", "runPassRate")))
      temp <- NULL
      
      for (i in 1:threshold){
            subs <- subset(data, togo == i)
            temp <- table(subs[, "Type"])
            result <- rbind(result, c(i, nrow(subs), temp[3]/(temp[3]+temp[2])))
      }
      return(result)
}

runConv <- function(data, distance.spec = 1:20){
      result <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("toGo", "nObs", "runConv")))
      
      for (i in distance.spec){
            subs <- subset(data, (togo == i)&(Type == 'Run'))
            result <- rbind(result, c(i, nrow(subs), sum(subs[, "convert"])/nrow(subs)))
      }
      return(result)
}

passConv <- function(data, distance.spec = 1:20){
      result <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("toGo", "nObs", "passConv")))
      
      for (i in distance.spec){
            subs <- subset(data, (togo == i)&(Type == 'Pass'))
            result <- rbind(result, c(i, nrow(subs), sum(subs[, "convert"])/nrow(subs)))
      }
      return(result)
}

thirdSummary <- function(years = 2012, distance.spec = 1:20, teams = NULL){
      
      result <- matrix(nrow = 0, ncol = 5, dimnames = list(NULL, c("toGo", "nObs", "runRate", "runConv", "passConv")))
      
      for (i in years){
            data <- getNFLData(i)
            # I need to add code here to subset the data based on which teams are selected
            data <- trimNFLData(data)
            data <- classifyNFLData(data)
            sumData <- cbind(runPassVsToGo(data, distance.spec), runConv(data, distance.spec)[,3], passConv(data, distance.spec)[,3])
            result <- rbind(result, sumData)
      }
      # I need to add code here to plot a summary plot with 2 axes and 3 lines
      return(result)
}
