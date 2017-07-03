best <- function(state, outcome){
    df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    outcomes <- c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
    
    if(!(outcome %in% names(outcomes))){
        stop("invalid outcome")
    }else if(!(state %in% states)){
        stop("invalid state")
    }else{
    
        states <- df[df[ ,7]==state, ]
        states_out <- states[ ,outcomes[outcome]]
        min_st_out <- min(states_out, na.rm = TRUE)
        hosp <- states[which(states_out==min_st_out), c(2)]
    
        return(hosp)
    }
}