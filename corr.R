# Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation 
# between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is 
# greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold 
# requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.

corr <- function(directory, threshold = 0){
        all_files <- list.files(directory, full.names = TRUE)
        
        v <- vector(mode = "numeric", length = 0)
        
        for(i in 1:length(all_files)){
                x <- read.csv(all_files[i])
                
                csum <- sum(complete.cases(x))
                
                xComp <- x[which(complete.cases(x)), ]
                
                if(csum > threshold){
                        
                        v <- c(v, cor(xComp$sulfate, xComp$nitrate))
                }
                
        }
        v
}