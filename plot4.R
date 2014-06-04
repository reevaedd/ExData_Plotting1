plot4 <- function() {

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

	## Plot 4

	tm <- subdata["Time"]
	tm <- as.vector(tm[, 1])

	## Create datetime object
	datetime <- as.POSIXct(paste(dt, tm), format = "%Y-%m-%d %H:%M:%S")

	GAP <- subdata["Global_active_power"]
	GAP <- as.vector(GAP[, 1])
	GAP <- as.numeric(GAP)

	Volt <- subdata["Voltage"]
	Volt <- as.vector(Volt[, 1])
	Volt <- as.numeric(Volt)		

	subMet1 <- subdata["Sub_metering_1"]
	subMet1 <- as.vector(subMet1[, 1])
	subMet1 <- as.numeric(subMet1)

	subMet2 <- subdata["Sub_metering_2"]
	subMet2 <- as.vector(subMet2[, 1])
	subMet2 <- as.numeric(subMet2)

	subMet3 <- subdata["Sub_metering_3"]
	subMet3 <- as.vector(subMet3[, 1])
	subMet3 <- as.numeric(subMet3)

	GRP <- subdata["Global_reactive_power"]
	GRP <- as.vector(GRP[, 1])
	GRP <- as.numeric(GRP)

	## Plotting
	par(mfrow = c(2, 2))
	plot(datetime, GAP, type = "l", ylab = "Global Active Power")
	plot(datetime, Volt, type = "l", ylab = "Voltage")
	plot(datetime, subMet1, type = "l", ylab = "Energy sub metering")
	lines(datetime, subMet2, type = "l", col = "red")
	lines(datetime, subMet3, type = "l", col = "blue")
	legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty = 1)
	plot(datetime, GRP, type = "l", ylab = "Global_reactive_power")

	## Copy the plot to a PNG file
	## The default size of the png is a width of 480 pixels and a height of 480 pixels
        dev.copy(png, file = "plot4.png")
	## Close the PNG device
	dev.off()
        
}   	
