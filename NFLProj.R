prepData <- function(read_dir = "H:/Docs/Sports/Project/raw_data", write_dir = "H:/Docs/Sports/Project/wd", years = c(2002:2009, 2011,2012)){ #9min
      
      for (i in years){                                                 
            setwd(read_dir)
            year_data <- read.csv(paste(as.character(i), "_nfl_pbp_data.csv", sep = "", quote = ""))
            year_data <- subset(year_data, select = -c(season,qtr))     #these are redundant columns
            year_data[is.na(year_data)] <- 0                            #replace NAs with 0s -- makes evaluating boolean expressions easy
            year_data <- addPType2(year_data)
            year_data <- addConvert(year_data)
            
            setwd(write_dir)
            write.csv(year_data, paste(as.character(i), "_nfl_pbp_dataP.csv", sep=""))
            cat(paste(as.character(i), " has been completed...\n"))
      }
}

loadData <- function(dir = "H:/Docs/Sports/Project/wd", years = 2012, teams = NULL){
      setwd(dir)
      result = NULL
      
      for (i in years){
            year_data <- read.csv(paste(as.character(i), "_nfl_pbp_dataP.csv", sep=""))
            if (!is.null(teams)){
                  year_data <- subset(year_data, (off %in% teams)|(def %in% teams))
            }
            result = rbind(result, year_data)
      }
      return(result)
}

runPassSummary <- function(data, ydline.spec = 20:80, down.spec = 1:4, distance.spec = 1:20, trim_last_five = TRUE, score_diff = -21:21){
      
      data <- subsetData(data, ydline.spec, down.spec, distance.spec, trim_last_five, score_diff)
      sumData <- cbind(runPassVsToGo(data, distance.spec), runConv(data, distance.spec)[,3], passConv(data, distance.spec)[,3])
      result <- matrix(nrow = 0, ncol = 5, dimnames = list(NULL, c("toGo", "nObs", "runRate", "runConv", "passConv")))
      result <- rbind(result, sumData)
      
      return(result)
}

addPType <- function(data){                                             #this function classifies each play as "Run", "Pass", or "Other"
      
      data[,"pType"] <- "Other"
      desc <- data[,"description"]                                      #seems to marginally decrease computation time
      
      for(i in 1:nrow(data)){
            if((grepl("tackle", desc[i])>0)|(grepl("end", desc[i])>0)|(grepl("up the middle", desc[i])>0)|(grepl("guard", desc[i])==1)&(grepl("scramble", desc[i])==0)){
                  data[i,"pType"] <- "Run"
            } else if((grepl("pass", desc[i])>0)|(grepl("sacked", desc[i])>0)|(grepl("scramble", desc[i])>0)){
                  data[i,"pType"] <- "Pass"
            } else if(grepl("field goal", desc[i])){
                  data[i, "pType"] <- "FG"
            } else if(grepl("punt", desc[i])){
                  data[i, "pType"] <- "Punt"
            }else if(grepl("PENALTY", desc[i])){
                  data[i, "pType"] <- "Pen"
            }
      }                                                                 
      return(data)
}

addPType2 <- function(data){                                             #this function classifies each play as "Run", "Pass", or "Other"
      
      data[,"pType"] <- "Run"
      desc <- data[,"description"]                                      #seems to marginally decrease computation time
      
      for(i in 1:nrow(data)){
            if((grepl("pass", desc[i])>0)|(grepl("sacked", desc[i])>0)|(grepl("scramble", desc[i])>0)){
                  data[i,"pType"] <- "Pass"
            } else if(grepl("field goal", desc[i])){
                  data[i, "pType"] <- "FG"
            } else if(grepl("punt", desc[i])){
                  data[i, "pType"] <- "Punt"
            } else if(grepl("No Play", desc[i])){   #this needs to be checked
                  data[i, "pType"] <- "Pen"
            } else if (grepl("kicks", desc[i])){
                  data[i, "pType"] <- "KO"
            } else if (grepl("extra point", desc[i])){
                  data[i, "pType"] <- "PAT"
            } else if (grepl("kneels", desc[i])){
                  data[i, "pType"] <- "Kneel"
            } else if (grepl("borted", desc[i])){
                  data[i, "pType"] <- "Aborted"
            }                                               #need to add 2pt attempts
      }                                                                 
      return(data)
}

addConvert <- function(data){                   #i would like to change this function s.t. it only has values for Run or Pass plays
      
      data[,"convert"] <- FALSE
      
      for(i in 1:(nrow(data) - 2)){
            
            if((((data[i + 1,"down"] == 1) | (data[i + 1, "offscore"] > data[i, "offscore"])) & (data[i, "off"] == data[i + 1, "off"]) & (data[i,"down"]) != 0)|((data[i+2, "offscore"]-data[i, "offscore"]==7)&(data[i, "off"]==data[i+2, "off"]))){
                  data[i,"convert"] <- TRUE
            }
      }
      
      if(((data[nrow(data), "down"] == 1)|grepl("TOUCHDOWN", data[nrow(data) - 1, "description"])|grepl("GOOD", data[nrow(data) - 1, "description"]))&(data[nrow(data), "off"] == data[nrow(data) - 1,  "off"])){
            data[nrow(data) - 1, "convert"] <- TRUE
      }
      
      if(grepl("TOUCHDOWN", data[nrow(data), "description"])|grepl("GOOD", data[nrow(data), "description"])){
            data[nrow(data), "convert"] <- TRUE
      }
      
      return(data)
}

subsetData <- function(data, ydline.spec = 20:80, down.spec = 1:4, distance.spec = 1:40, trim_last_five = FALSE, score_diff = -21:21){
      #this function trims the dataset according to user specifications
      #data is the dataset to be trimmed; field.spec is a string that equals "middle", "own20", "redzone", or "all"; off.spec and def.spec
      #are 2 to 3 letter strings corresponding to each team; down.spect is a vector of numbers describing downs to include; distance.spec
      #is a vector of numbers describing the acceptable values for "togo"; five_min is T/F specifying whether the data in the last 5 min
      #of each half should be excluded; score_diff is an integer threshold that excludes score differences above the threshold
      
      #i need to add code to specify off and def
      
      if (trim_last_five == TRUE){ 
            data <- subset(data, (min %in% c(5:30, 35:60)) )#| (qtr %in% c(1,3)))
      }
      
      data <- subset(data, (down %in% down.spec)&(ydline %in% ydline.spec)&(togo %in% distance.spec)&((offscore - defscore) %in% score_diff))
}

conVsToGo <- function(data, distance.spec = 1:25){
      #this function returns a matrix with 3 columns listing the number of yards to go, the number of observations from
      #the data and the converstion rate with that number of yards to go.
      
      result <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("toGo", "nObs", "convRate")))
      
      for (i in distance.spec){
            subs <- subset(data, (togo == i) & (pType %in% c("Run", "Pass")))
            result <- rbind(result, c(i, nrow(subs), sum(subs[,"convert"])/nrow(subs)))
      }
      return(result)
}

runPassVsToGo <- function(data, distance.spec = 1:25){
      #this function returns a matrix with 3 columns listing the number of yards to go, the number of observations from
      #the data and the run% with that number of yards to go.
      
      result <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("toGo", "nObs", "runPassRate")))
      temp <- NULL
      
      for (i in distance.spec){
            subs <- subset(data, (togo == i))
            temp <- table(subs[, "pType"])
            result <- rbind(result, c(i, nrow(subs), temp["Run"]/(temp["Run"]+temp["Pass"])))
      }
      return(result)
}

runConv <- function(data, distance.spec = 1:25){
      result <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("toGo", "nObs", "runConv")))
      
      for (i in distance.spec){
            subs <- subset(data, (togo == i)&(pType == 'Run'))
            result <- rbind(result, c(i, nrow(subs), sum(subs[, "convert"])/nrow(subs)))
      }
      return(result)
}

passConv <- function(data, distance.spec = 1:25){
      result <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, c("toGo", "nObs", "passConv")))
      
      for (i in distance.spec){
            subs <- subset(data, (togo == i)&(pType == 'Pass'))
            result <- rbind(result, c(i, nrow(subs), sum(subs[, "convert"])/nrow(subs)))
      }
      return(result)
}

getSummary <- function(data, OFF = NULL, DEF = NULL, DOWN = 1:4, threshold = 1:15){
      result <- matrix(nrow = 0, ncol = 6, dimnames = list(NULL, c("down", "toGo", "nObs", "runRate", "runConv", "passConv")))
      
      if (!is.null(OFF)){
            data <- subset(data, off %in% OFF)
      }
      
      if(!is.null(DEF)){
            data <- subset(data, def %in% DEF)
      }
      
      for (i in DOWN){
            down <- rep(i, max(threshold))
            data2 <- subset(data, down == i)
            
            sumData <- cbind(down, runPassVsToGo(data2, threshold), runConv(data2, threshold)[,3], passConv(data2, threshold)[,3])
            
            result <- rbind(result, sumData)
      }
      
      result <- data.frame(result)
      result[,1] <- as.factor(result[,1])
      return(result)
}

playSummary <- function(data, OFF = NULL, DEF = NULL, DOWN = 1:4, distance = 1:15){
      
      
      result <- getSummary(data, OFF, DEF, DOWN, distance)
      df.work <- NULL
      
      if(length(distance) > 1){
            if(length(DOWN) > 1){
                  p <- ggplot(result, aes(toGo, runRate))
                  p + geom_point(aes(color = down)) + geom_line(aes(color = down)) + ylim(0, 1) + theme_bw() + ggtitle(paste('Off:', OFF, 'Def:', DEF, 'Down:', min(DOWN), 'to', max(DOWN))) + scale_x_continuous(breaks = seq(1, max(distance), 1))
            }
            else if(length(DOWN == 1)){
                  p <- ggplot(result, aes(x = toGo))
                  p <- p + geom_line(aes(y = runRate, colour="Run Rate")) + ylim(0, 1) + theme_bw() + ggtitle(paste('Off:', OFF, 'Def:', DEF, 'Down:', DOWN)) + scale_x_continuous(breaks = seq(1, max(distance), 1))
                  p <- p + geom_line(aes(y = passConv, colour = "Pass Conversion"), linetype = "dashed")
                  p <- p + geom_line(aes(y = runConv, colour = "Run Conversion"), linetype = "dashed") + scale_colour_discrete(name = "Legend")
                  p + geom_point(aes(y = runConv, colour="Run Conversion")) + geom_point(aes(y = passConv, colour = "Pass Conversion")) + geom_point(aes(y = runRate, colour = "Run Rate"))
            }
      }
      else if(length(distance) == 1){
            if(length(DOWN) > 1){
                  print('Please specify DOWN')
            }
            else if(length(DOWN == 1)){
                  #comb <- data.frame("Summary" = c((1- result[1,4])*result[1,6], (1- result[1,4])*(1-result[1,6]), result[1,4]*result[1,5], result[1,4]*(1-result[1,5])), 
                  #                   Description = c("Converted Pass", "Failed Pass", "Converted Run", "Failed Run"),
                  #                   Test = c(1,1,1,1))#list(NULL, c("Converted Pass", "Failed Pass", "Converted Run", "Failed Run")))
                  #comb <- matrix(c("Converted Pass", "Failed Pass", "Converted Run", "Failed Run"), nrow = 4, ncol = 1)
                  #comb[,1] <- c("Converted Pass", "Failed Pass", "Converted Run", "Failed Run")                
                  #comb[,2] <- c((1- result[1,4])*result[1,6], (1- result[1,4])*(1-result[1,6]), result[1,4]*result[1,5], result[1,4]*(1-result[1,5]))
                  
                  #comb <- rbind(comb, c("Converted Pass", (1- result[1,4])*result[1,6]))
                  
                  #p <- ggplot(data = comb, aes(x=factor(1), y = Summary, fill = factor(Description)))
                  #p <- p + geom_bar(width = 1)
                  #p +facet(grid(facets=. ~ Test))
                  
                  #p <- ggplot(comb)
                  #p <- p + facet_grid(as.factor(comb[,1]) ~ comb[,2])
                  #p
                  df.work <- subset(data, (pType == 'Run')|(pType == 'Pass'))
                  df.work$pType <- droplevels(df.work$pType)
                  mosaicplot(df.work$pType ~ df.work$convert, color = 2:3)
            }
      }
     
      #return(result)
      #p2 <- ggplot(result, aes(toGo, runConv))
      #p2 + geom_point(aes(color = down),) + geom_line(aes(color = down)) + ylim(0, 1) + theme_bw() + ggtitle(paste('off =', OFF, 'def =', DEF, 'down =', min(DOWN), 'to', max(DOWN)))

      #p3 <- ggplot(result, aes(toGo, passConv))
      #p3 + geom_point(aes(color = down),) + geom_line(aes(color = down)) + ylim(0, 1) + theme_bw() + ggtitle(paste('off =', OFF, 'def =', DEF, 'down =', min(DOWN), 'to', max(DOWN)))
}

downSummary <- function(data, OFF = NULL, DEF = NULL, DOWN = 3, threshold = 10){
      
      result <- getSummary(data, OFF, DEF, DOWN, threshold)
      grp <- result[,3:5]
      
      p <- ggplot(result, aes(x = toGo))
      p <- p + geom_line(aes(y = runConv, colour="Run Conversion")) + ylim(0, 1) + theme_bw() + ggtitle(paste('Off:', OFF, 'Def:', DEF, 'Down:', DOWN)) + scale_x_continuous(breaks = seq(1, max(threshold), 1))
      p <- p + geom_line(aes(y = passConv, colour = "Pass Conversion"))
      p <- p + geom_line(aes(y = runRate, colour = "RunRate")) + scale_colour_discrete(name = "Legend")
      p
      #return(result)
      #p2 <- ggplot(result, aes(toGo, runConv))
      #p2 + geom_point(aes(color = down),) + geom_line(aes(color = down)) + ylim(0, 1) + theme_bw() + ggtitle(paste('off =', OFF, 'def =', DEF, 'down =', min(DOWN), 'to', max(DOWN)))
      
      #p3 <- ggplot(result, aes(toGo, passConv))
      #p3 + geom_point(aes(color = down),) + geom_line(aes(color = down)) + ylim(0, 1) + theme_bw() + ggtitle(paste('off =', OFF, 'def =', DEF, 'down =', min(DOWN), 'to', max(DOWN)))
}
