#
# prototype
#
complete <- function(directory, id = 1:332) {
	##
	## report the number of completely observed cases in each data file.
	##
	
	## 'directory' is location of the CSV files
	## 'id' is vector of monitor identifiers
	
	nobs <- vector("numeric", length = length(id))

	for ( i in seq_along(id)) {
		mdata <- read.csv(sprintf("%s/%03d.csv",directory,id[i]))		
		nobs[i] <- length(mdata$sulfate[complete.cases(mdata$sulfate,mdata$nitrate)])
	}
	data.frame(id,nobs)
}

#
# expected output
# id nobs
# 1  117
# 2  1041
# ...
# where 'id' is the monitor ID number and 'nobs' is the number of complete cases
#
# expected output
# source("complete.R")
# complete("specdata",1)
##    id nobs
## 1   1  117
#
# complete("specdata", c(2,4,8,10,12))
#
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96
