rankhospital <- function(state, outcome, num="best"){
    df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    outcomes <- c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
    states <- df[df[ ,7]==state, ]
    
    if(!(outcome %in% names(outcomes))){
            stop("invalid outcome")
    }else if(!(state %in% states)){
            stop("invalid state")
    }else{
                
            stateslist <- split(df, df$State)
            stateSTlist <- stateslist[state]
            stateSTdf <- as.data.frame(stateSTlist)
            StOut <- stateSTdf[,outcomes[outcome]]
            indiceStOut <- order(StOut, stateSTdf[,2], na.last = NA)
            rankStOut <- stateSTdf[indiceStOut,c(2,23)]
            rank <- 1:nrow(rankStOut)
            rankStOut <- cbind(rankStOut,rank)
            
            if(num == "best"){
                    return(rankStOut[which(rankStOut[, 3]==1), 1])
            }else if (num == "worst"){
                    return(rankStOut[which(rankStOut[, 3]==nrow(rankStOut)), 1])
            }else{
                    if(num > nrow(rankStOut)){
                            return(NA)
                    }else{
                            return(rankStOut[which(rankStOut[, 3]==num), 1])
                    }
            }
    }
}     

