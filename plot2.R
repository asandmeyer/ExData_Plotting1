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

#generating plot number 2
dev.new(width = 480, height = 480, unit = "px")
par(bg = "white")
with(data_sel, plot(datetime, Global_active_power, ylab = "Global Active Power (kilowatts)", xlab = "", type = "n"))
lines(data_sel$datetime, data_sel$Global_active_power)
dev.copy(png, file = "plot2.png")
dev.off()