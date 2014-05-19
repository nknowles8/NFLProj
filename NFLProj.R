prepData <- function(read_dir = "H:/Docs/Sports/Project/raw_data", write_dir = "H:/Docs/Sports/Project/wd", years = c(2002:2009, 2011,2012)){ #9min
      setwd(read_dir)
      
      for (i in years){
            year_data <- read.csv(paste(as.character(i), "_nfl_pbp_data.csv", sep = ""))
            year_data <- subset(year_data, select = -c(season,qtr))     #these are redundant columns
            year_data[is.na(year_data)] <- 0                            #replace NAs with 0s -- makes evaluating boolean expressions easy
            year_data <- addPType(year_data)
            year_data <- addConvert(year_data)
            
            setwd(write_dir)
            write.csv(year_data, paste(as.character(i), "_nfl_pbp_dataP.csv", sep=""))
      }
}

addPType <- function(data){                                             #this function classifies each play as "Run", "Pass", or "Other"
      
      data[,"pType"] <- "Other"
      desc <- data[,"description"]                                      #seems to marginally decrease computation time
      
      for(i in 1:nrow(data)){
            if((grepl("tackle", desc[i])>0)|(grepl("end", desc[i])>0)|(grepl("up the middle", desc[i])>0)|(grepl("guard", desc[i])==1)&(grepl("scramble", desc[i])==0)){
                  data[i,"pType"] <- "Run"
            } else if((grepl("pass", desc[i])>0)|(grepl("sacked", desc[i])>0)|(grepl("scramble", desc[i])>0)){
                  data[i,"pType"] <- "Pass"
            } 
      }
      return(data)
}

addConvert <- function(data){
      
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
