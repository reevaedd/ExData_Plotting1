plot2 <- function() {

	## Reading the whole data from the file in the current working directory
	powerData <- read.table("./household_power_consumption.txt", sep = ";", header = TRUE)

	## Convert each element of the data frame into character
	powerData <- data.frame(lapply(powerData, as.character), stringsAsFactors = FALSE)

	## Subsetting the data to only include the dates
	## 2007-02-01 and 2007-02-02
	subdata <- subset(powerData, powerData["Date"] == "1/2/2007" | powerData["Date"] == "2/2/2007")

	## Convert dates from character to Date format
	powerDate <- subdata["Date"]
	powerDate <- as.vector(powerDate[, 1])
	dt <- strptime(powerDate, "%d/%m/%Y")
	dt <- as.Date(dt)

	## Plot 2

	tm <- subdata["Time"]
	tm <- as.vector(tm[, 1])

	## Create datetime object
	datetime <- as.POSIXct(paste(dt, tm), format = "%Y-%m-%d %H:%M:%S")

	GAP <- subdata["Global_active_power"]
	GAP <- as.vector(GAP[, 1])
	GAP <- as.numeric(GAP)	
	par(mar = rep(2, 4))
	par(mar = c(4, 4, 2, 2))

	## Plotting
	plot(datetime, GAP, type = "l", ylab = "Global Active Power (kilowatts)")

	## Copy the plot to a PNG file
        ## The default size of the png is a width of 480 pixels and a height of 480 pixels
        dev.copy(png, file = "plot2.png")
	## Close the PNG device
	dev.off()
        
}   	
