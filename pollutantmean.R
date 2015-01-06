#
# prototype
#
pollutantmean <- function(directory, pollutant, id = 1:332) {
	##
	## calculate the mean of the specified pollutant across a specified set of monitors
	##
	
	## 'directory' is location of the CSV files
	## 'pollutant' is the name of the pollutant, either "sulfate" or "nitrate"
	## 'id' is vector of monitor identifiers
	sum_of_columns  <- 0
	number_of_values <- 0
	for ( monitor in id ) {
		monitor_data <- read.csv(sprintf("%s/%03d.csv",directory,monitor))
		
		cleandata <- monitor_data[[pollutant]][!is.na(monitor_data[[pollutant]])]
		sum_of_columns <- sum_of_columns + sum(cleandata)
		number_of_values <- number_of_values + length(cleandata)
	}
	round((sum_of_columns / number_of_values),3)
}

#
# expected output
#
# source("pollutantmean.R")
# pollutantmean("specdata", "sulfate", 1:10)
# should return vector 4.064
# pollutantmean("specdata", "nitrate", 70:72)
# should return vector 1.706
# pollutantmean("specdata", "nitrate", 23)
# should return 1.281
