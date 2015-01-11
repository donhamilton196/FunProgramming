#
rankall <- function(outcome, num = "best") {
	## read the outcome data
	outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")

	## valid outcomes to examine
	valid_outcome <- data.frame(c("heart attack", "heart failure", "pneumonia"),c(11,17,23))
	if ( ! any(valid_outcome[,1] == outcome)) stop("invalid outcome")
	
	## the specified outcome
	hospital_outcome <- as.numeric(outcome_data[,valid_outcome[valid_outcome[,1]==outcome,2]])
	valid_data <- !is.na(hospital_outcome) 
	
	## return a data frame with the hospital names and the (abbreviated) state name	
	
	## get vector of states in the data
	states <- sort(unique(outcome_data$State))
	
	## the ugly loop
	ranked_output <- matrix(c("c1","c2"),nrow=1)
	for (state in states) {
		## return the hospital name in that state with the given rank
		hospitals <- outcome_data$State == state	

		state_hospital_name <- outcome_data$Hospital.Name[hospitals & valid_data]
		state_hospital_outcome <- hospital_outcome[hospitals & valid_data]
			
		rank <- 0
		if ( num == "best" ) rank <- 1
		if ( num == "worst" ) rank <- length(state_hospital_name)
		if ( rank == 0 & num > 0 & num <= length(state_hospital_name) ) rank <- num

		row <- list(NA,state)
		
		if ( rank != 0) {
			ranking <- state_hospital_name[order(state_hospital_outcome,state_hospital_name)]
			
			row <- list(ranking[rank],state)
		}
		ranked_output <- rbind(ranked_output,row)
	}
	ranked_output <- ranked_output[-1,]
	colnames(ranked_output) <- c("hospital","state")
	rownames(ranked_output) <- ranked_output[,2]
	data.frame(ranked_output)
}
