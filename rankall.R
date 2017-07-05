## Read the data
## Subset to three columns
## Remove NA Values
## Order by state then outcome then hospital name
## Split by state
## Run lapply

## The following function receives a state data frame from the split data
## function_for_lapply(data) { do something with data }

# function_for_lapply(data) {
#         if(condition1) do something
#         if(condition2) do something else
#                 return a value
# }

rankall <- function(outcome, num="best"){
        df <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        
        outcomes <- c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
        
        if(!(outcome %in% names(outcomes))){
                stop("invalid outcome")
        }else{
        
                column_index <- outcomes[outcome]
                subsetdf <- df[ ,c(7,2,column_index)]
                compsubsetdf <- subsetdf[which(complete.cases(subsetdf)),]
                ordered <- compsubsetdf[order(compsubsetdf[,1],compsubsetdf[,3],compsubsetdf[,2]),]
                stateslist <- split(ordered, ordered[,1])
                
                hospitalname <- function(data){
                        if(num == "best"){
                                return(data$Hospital.Name[1])
                        }else if(num == "worst"){
                                return(data$Hospital.Name[nrow(data)])
                        }else{
                                return(data$Hospital.Name[num]) ##objects in stateslist are df (54 df for each state)
                                                                ##but in each df, the columns(State, Hospital.Name, Outcome) are vector
                        }
                }
                
                result <- lapply(stateslist, hospitalname)
                unlisted_values <- unlist(result)
                list_names <- names(result)
                
                final_df <- data.frame(hospital=unlisted_values,state=list_names,row.names = list_names)
                return(final_df)
        }
}

