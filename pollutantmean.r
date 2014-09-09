#This script contains a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across 
#a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'.
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory 
#specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any 
#missing values coded as NA. 

pollutantmean <- function(dir, pol, id=1:332){
	## 'dir' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
	total_sum <- 0
	total_length <- 0
	for(name in id){
		if(name < 10){dirname <- paste("00",toString(name),sep="")}
		else if(name < 100){dirname <- paste("0",toString(name),sep="")}
		else{dirname <- toString(name)}
		
		dirname <- paste(dir,"/",dirname,".csv",sep="")
		file <- read.csv(dirname)
		colData <- 	file[[pol]]
		log <- !is.na(colData)
		total_sum <- total_sum + sum(colData[log])
		total_length <- total_length + length(colData[log])	
	}
	print(total_sum/total_length)
	
	## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
}

