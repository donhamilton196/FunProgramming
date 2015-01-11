best <- function(state, outcome) {
	## read outcome data
	outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
	
	## check that state and outcome are valid
	if ( ! any(outcome_data$State == state)) stop("invalid state")
	
	## valid outcomes to examine
	valid_outcome <- data.frame(c("heart attack", "heart failure", "pneumonia"),c(11,17,23))
	if ( ! any(valid_outcome[,1] == outcome)) stop("invalid outcome")
	
	## return hospital name in that state with the lowest 30-day death rate
	hospital_outcome <- as.numeric(outcome_data[,valid_outcome[valid_outcome[,1]==outcome,2]]) 
	
	hospitals <- outcome_data$State == state	
	valid_data <- !is.na(hospital_outcome) 
	
	hospital_name <- outcome_data$Hospital.Name[hospitals & valid_data]
	hospital_outcome <- hospital_outcome[hospitals & valid_data]
	
	## handle ties
	best_hospitals <- sort(hospital_name[hospital_outcome == min(hospital_outcome)])

	best_hospitals[1]
}
