# this function reads the file passed in fName from the current working directory
# it then reads the entire data set, extracts just the rows for the relevant dates
# as defined in the requirements, unloads the entire dateset, retaining only those
# rows that matter, and returning those rows for subsequent use
loadData <- function(fName){
  message("Reading Data")
  lDT <- fread(fName, na.strings = "?")

  message("Changing Date Format")
  lDT$Date <- as.Date(lDT$Date, format = '%d/%m/%Y')

  tmpDT <- lDT[lDT$Date >= as.Date("2007-02-01", format = '%Y-%m-%d') 
                & lDT$Date <= as.Date("2007-02-02", format = '%Y-%m-%d'),]

  remove(lDT)

  message("Changing active power format")
  tmpDT$Global_active_power <- as.numeric(tmpDT$Global_active_power)

  message("Changing reactive power format")
  tmpDT$Global_reactive_power <- as.numeric(tmpDT$Global_reactive_power)  

  message("Changing voltage power format")
  tmpDT$Voltage <- as.numeric(tmpDT$Voltage)

  message("Changing global intensity format")
  tmpDT$Global_intensity <- as.numeric(tmpDT$Global_intensity)
  return(tmpDT)
} 

# Plot1 assumes you've placed the file in the correct current working directory
plot1 <- function(){
  library(data.table)
  DT <- loadData("household_power_consumption.txt")
  png(width = 480, height = 480, units = "px","plot1.png")
  with(DT, hist(Global_active_power, col = "#FF2500", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", main = "Global Active Power"))
  dev.off()
}