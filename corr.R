#
# prototype
#
corr <- function(directory, threshold = 0) {
	##
	## Calculate correlation between sulfate and nitrate for monitor locations
	## where the number of completely observed cases is greater than the threshold.
	##
	
	## 'directory' is location of the CSV files
	## 'threshold' is the number of completely observed observations required
	## to compute the correlation between nitrate and sulfate; the default is 0
	##
	## returns a numeric vector of correlations
	
	id <- 1:332	

	nobs <- vector("numeric", length = length(id))
	correlation <- vector("numeric", length = length(id))
	
	for ( i in seq_along(id)) {
		mdata <- read.csv(sprintf("%s/%03d.csv",directory,id[i]))
		select_obs <- complete.cases(mdata$sulfate,mdata$nitrate)
		
		s_obs <- mdata$sulfate[select_obs]
		n_obs <- mdata$nitrate[select_obs]
		
		correlation[i] <- if (length(s_obs) > threshold) {			
				cor(s_obs,n_obs)
			} else {
				NA
			}
	}
	correlation[!is.na(correlation)]
}

#
# expected output
#
# cr <- corr("specdata", 150)
# head(cr)
# [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
# summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630