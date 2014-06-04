plot1 <- function() {

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

	## Plot 1

	GAP <- subdata["Global_active_power"]
	GAP <- as.vector(GAP[, 1])
	GAP <- as.numeric(GAP)
	hist(GAP, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
        
        ## The default size of the png is a width of 480 pixels and a height of 480 pixels
        dev.copy(png, file = "plot1.png")
	dev.off()
        
}   	
