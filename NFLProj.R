prepData <- function(read_dir = "H:/Docs/Sports/Project/raw_data", write_dir = "H:/Docs/Sports/Project/wd", years = c(2002:2009, 2011,2012)){ #9min
      
      for (i in years){                                                 #i would like to add code to this loop that prints its progress
            setwd(read_dir)
            year_data <- read.csv(paste(as.character(i), "_nfl_pbp_data.csv", sep = ""))
            year_data <- subset(year_data, select = -c(season,qtr))     #these are redundant columns
            year_data[is.na(year_data)] <- 0                            #replace NAs with 0s -- makes evaluating boolean expressions easy
            year_data <- addPType2(year_data)
            year_data <- addConvert(year_data)
            
            setwd(write_dir)
            write.csv(year_data, paste(as.character(i), "_nfl_pbp_dataP.csv", sep=""))
            cat(paste(as.character(i), " has been completed..."))
      }
}

loadData <- function(dir = "H:/Docs/Sports/Project/wd", years = 2012, teams = NULL){
      setwd(dir)
      result = NULL
      
      for (i in years){
            year_data <- read.csv(paste(as.character(i), "_nfl_pbp_dataP.csv", sep=""))
            if teams != NULL{
                  year_data <- subset(year_data, (off %in% teams)|(def %in% teams))
            }
            result = rbind(result, year_data)
      }
      return(result)
}

runPassSummary <- function(data, ydline.spec = 20:80, down.spec = 1:4, distance.spec = 1:40, trim_last_five = FALSE, score_diff = -21:21){
      data <- subsetData(data, ydline.spec, down.spec, distance.spec, trim_last_five, score_diff)
      # this function will produce a play selection pie chart for a particular situation
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
            data <- subset(data, (min %in% c(5:30, 35:60)) | (qtr %in% c(1,3)))
      }
      
      data <- subset(data, (down %in% down.spec)&(ydline %in% ydline.spec)&(togo %in% distance.spec)&((offscore - defscore) %in% score_dif))
}
