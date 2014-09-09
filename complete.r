#This script contains a function that reads a directory full of files and reports the number of completely observed cases 
#in each data file. The function should return a data frame where the first column is the name of the file and the 
#second column is the number of complete cases.

complete <- function(directory, i = 1:332) {
  # 'directory' is a character vector of length 1 indicating the location of the CSV files
  # 'id' is an integer vector indicating the monitor ID numbers to be used
	obs <- vector()
      id <- vector()
	index <- 1
	for(name in i){
		if(name < 10){dirname <- paste("00",toString(name),sep="")}
		else if(name < 100){dirname <- paste("0",toString(name),sep="")}
		else{dirname <- toString(name)}
		
		dirname <- paste(directory,"/",dirname,".csv",sep="")
		file <- read.csv(dirname)
			
		sul <- file$sulfate
		nit <- file$nitrate

		log <- !is.na(sul) & !is.na(nit)
		
		obs[index] <- length(sul[log])
		id[index] <- name
		index <- index + 1
	}
	nobs <- obs
      data.frame(id = id, nobs = nobs)
      
        # Return a data frame of the form:
        # id nobs
        # 1  117
        # 2  1041
        # ...
        # where 'id' is the monitor ID number and 'nobs' is the
        # number of complete cases  
}
