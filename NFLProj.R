getNFLData <- function(year){
      setwd("H:/Docs/Sports/Project")
      raw <- read.csv(paste(as.character(year), "_nfl_pbp_data.csv", sep = ""))
}

addConvert <- function(data){
      data[,"convert"] <- FALSE
      
      for(i in 1:(nrow(data) - 1)){
            
            if(is.na(data[i + 1, "down"])){
                  data[i + 1, "down"] <-5
            }
            
            if(((data[i + 1,"down"] == 1) | (data[i + 1, "offscore"] > data[i, "offscore"])) & (data[i, "off"] == data[i + 1, "off"]) & (is.na(data[i,"down"]) == FALSE)){
                  data[i,"convert"] <- TRUE
            }
      }
}

trimNFLData <- function(data, off.spec = "", def.spec = "", five_min = FALSE, score_diff = 21){
      #this function trims the dataset according to user specifications
      #data is the dataset to be trimmed; field.spec is a string that equals "middle", "own20", "redzone", or "all"; off.spec and def.spec
      #are 2 to 3 letter strings corresponding to each team; down.spect is a vector of numbers describing downs to include; distance.spec
      #is a vector of numbers describing the acceptable values for "togo"; five_min is T/F specifying whether the data in the last 5 min
      #of each half should be excluded; score_diff is an integer threshold that excludes score differences above the threshold
      
      data[,"convert"] <- FALSE
      yard.max <- 80
      yard.min <- 20
      
      data[1, "down"] <- 5
      
      for(i in 1:(nrow(data) - 1)){
            
            if(is.na(data[i + 1, "down"])){
                  data[i + 1, "down"] <-5
            }
            
            if(((data[i + 1,"down"] == 1) | (data[i + 1, "offscore"] > data[i, "offscore"])) & (data[i, "off"] == data[i + 1, "off"]) & (is.na(data[i,"down"]) == FALSE)){
                  data[i,"convert"] <- TRUE
            }
      }
      
      if(off.spec != ""){ #trim data based on offense
            data <- subset(data, off == off.spec)
      }
      
      if(def.spec != ""){ #trim data based on defense
            data <- subset(data, def == def.spec)
      }
      
      
      if (five_min == TRUE){ 
            data <- subset(data, (min %in% c(5:30, 35:60)) | (qtr %in% c(1,3)))
      }
      
      data <- subset(data,(abs(offscore-defscore)<=score_diff), select = -c(season,qtr))
}
 
classifyNFLData <-function(data){
      data[,"Type"] <- NA
      desc <- data[,"description"]
      
      for(i in 1:nrow(data)){
            if((grepl("tackle", desc[i])>0)|(grepl("end", desc[i])>0)|(grepl("up the middle", desc[i])>0)|(grepl("guard", desc[i])==1)&(grepl("scramble", desc[i])==0)){
                  data[i,"Type"] <- "Run"
            } else if((grepl("pass", desc[i])>0)|(grepl("sacked", desc[i])>0)|(grepl("scramble", desc[i])>0)){
                  data[i,"Type"] <- "Pass"
            } else{
                  data[i,"Type"] <- "Other"
            }
      }
      return(data)
}

prepSummary <- function(years = 2012, directory="H:/Docs/Sports/Project", off.spec = "", def.spec = ""){
      
      #this takes about 4 min to load 1 year of league data and 2 min for 1 yr of team data, 6 min for 2 years of team data
  
      setwd(directory)
      data <- NULL
      
      for (i in years){
            yeardata <- read.csv(paste(as.character(i), "_nfl_pbp_data.csv", sep = ""))
            yeardata <- trimNFLData(yeardata, off.spec, def.spec, five_min, score_diff)
            data <- rbind(data, yeardata)
      }
      
      data <- classifyNFLData(data)
}
