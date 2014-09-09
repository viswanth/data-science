#The script has a function that takes a directory of data files and a threshold for complete cases and calculates 
#the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases 
#(on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors
#that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should 
#return a numeric vector of length 0.

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
	obs_frame <- complete(directory)
	comp_obs <- obs_frame$nobs
	#data_greater_than_threshold <- comp_obs[comp_obs > threshold)
	qualified_sets <- (1:332)[comp_obs > threshold]	
	index <- 1
	corelations <- vector()
	for(name in qualified_sets){
		if(name < 10){dirname <- paste("00",toString(name),sep="")}
		else if(name < 100){dirname <- paste("0",toString(name),sep="")}
		else{dirname <- toString(name)}
		
		dirname <- paste(directory,"/",dirname,".csv",sep="")
		file <- read.csv(dirname)
			
		sul <- file$sulfate
		nit <- file$nitrate

		log <- !is.na(sul) & !is.na(nit)

		log_sul <- sul[log]
		log_nit <- nit[log]
		
		corelations[index] <- cor(log_sul,log_nit)
		index <- index + 1

	}

	print(corelations)
	## Return a numeric vector of correlations
}

