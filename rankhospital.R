#
rankhospital <- function(state, outcome, num = "best") {
	## read the outcome data
	outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
	
	## check that the state and outcome are valid
	if ( ! any(outcome_data$State == state)) stop("invalid state")

	## valid outcomes to examine
	valid_outcome <- data.frame(c("heart attack", "heart failure", "pneumonia"),c(11,17,23))
	if ( ! any(valid_outcome[,1] == outcome)) stop("invalid outcome")

	## return the hospital name in that state with the given rank
	hospital_outcome <- as.numeric(outcome_data[,valid_outcome[valid_outcome[,1]==outcome,2]])
	
	hospitals <- outcome_data$State == state	
	valid_data <- !is.na(hospital_outcome) 
	
	hospital_name <- outcome_data$Hospital.Name[hospitals & valid_data]
	hospital_outcome <- hospital_outcome[hospitals & valid_data]
	
	rank <- 0
	if ( num == "best" ) rank <- 1
	if ( num == "worst" ) rank <- length(hospital_name)
	if ( rank == 0 & num > 0 & num <= length(hospital_name) ) rank <- num
	if ( rank == 0) return(NA)
	
	ranking <- hospital_name[order(hospital_outcome,hospital_name)]
	ranking[rank]
}
