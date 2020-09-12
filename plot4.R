library(dplyr)

#reading the data
data <- read.csv("household_power_consumption.txt", sep = ";")

#convert to Date and Time class
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data$Time <- strptime(data$Time, format = "%H:%M:%S")
data$Time <- format(data$Time, format = "%H:%M:%S")

#remove ? to avoid NA and convert to numeric
for (i in 3:ncol(data)){
  #print (i)
  data <- data[!(data[,i] %in% c("?")),]
  data[,i] <- as.numeric(data[,i])
}

#selecting the needed data at the specific dates
data_sel <- data[data$Date %in% as.Date(c("2007-02-01", "2007-02-02")),]

#creating datetime column with suitable format
data_sel <- mutate(data_sel, datetime = paste(Date, Time))
data_sel$datetime <- strptime(data_sel$datetime, "%Y-%m-%d %H:%M:%S")

#generating plot number 4
#first creating space for four plots
dev.new(width = 480, height = 480, unit = "px")
par(mfcol = c(2,2), mar = c(4,4,2,1), bg = "white")
#putting first plot
with(data_sel, plot(datetime, Global_active_power, ylab = "Global Active Power", xlab = "", type = "n"))
lines(data_sel$datetime, data_sel$Global_active_power)
#putting second plot
with(data_sel, plot(datetime, Sub_metering_1, ylab = "Energy sub metering", xlab = "", type = "n"))
lines(data_sel$datetime, data_sel$Sub_metering_1)
lines(data_sel$datetime, data_sel$Sub_metering_2, col = "red")
lines(data_sel$datetime, data_sel$Sub_metering_3, col = "blue")
legend("topright", cex = 0.8, legend =  c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd = c(2,2,2), col = c("black", "red", "blue"), bty ="n")
#putting third plot
with(data_sel, plot(datetime, Voltage, ylab = "Voltage", xlab = "datetime", type = "n"))
lines(data_sel$datetime, data_sel$Voltage)
#putting forth plot
with(data_sel, plot(datetime, Global_reactive_power, xlab = "datetime", type = "n"))
lines(data_sel$datetime, data_sel$Global_reactive_power)
dev.copy(png, file = "plot4.png")
dev.off()