####################################################################################
##################       read the data              ################################
####################################################################################

power_mode <- read.table("household_power_consumption.txt", sep = ";", 
                         header = TRUE)

power_min <- subset(power_mode, 
                    power_mode$Date=="1/2/2007" | power_mode$Date=="2/2/2007")
##################################################################################
################     first plot histogram           ##############################
##################################################################################

hist(as.numeric(power_min$Global_active_power), main = "Global active power",  
     col = "red" ,xlab = "Global Active Power(Kilowats)")

##################################################################################
##############   second plot time serie ##########################################
##################################################################################

power_min$Date <- as.Date(power_min$Date, format="%d/%m/%Y")
power_min$Time <- strptime(power_min$Time, format="%H:%M:%S")
power_min[1:1440,"Time"] <- format(power_min[1:1440,"Time"],"2007-02-01 %H:%M:%S")
power_min[1441:2880,"Time"] <- format(power_min[1441:2880,
                                                "Time"],"2007-02-02 %H:%M:%S")

plot(power_min$Time,as.numeric(as.character(power_min$Global_active_power)),type="l",
     xlab="",ylab="Global Active Power (kilowatts)") 
title(main = "Global active power vs Time")

##################################################################################
##############            tree plot Time Series                      #############
##################################################################################

plot(power_min$Time,power_min$Sub_metering_1,type="n",xlab="",
     ylab="Energy sub metering")
with(power_min,lines(Time,as.numeric(as.character(Sub_metering_1))))
with(power_min,lines(Time,as.numeric(as.character(Sub_metering_2)),col="red"))
with(power_min,lines(Time,as.numeric(as.character(Sub_metering_3)),col="blue"))
legend("topright", lty=1, col=c("black","red","blue"),
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

title(main = "Energy sub-metering")



###################################################################################
###########             third plot par_mfrow                   ####################
###################################################################################

par(mfrow=c(2,2))
with(power_min,{
  plot(power_min$Time,as.numeric(as.character(power_min$Global_active_power)),
       type="l",  xlab="",ylab="Global Active Power")  
  plot(power_min$Time,as.numeric(as.character(power_min$Voltage)), 
       type="l",xlab="datetime",ylab="Voltage")
  plot(power_min$Time,power_min$Sub_metering_1,type="n",xlab="",
       ylab="Energy sub metering")
  with(power_min,lines(Time,as.numeric(as.character(Sub_metering_1))))
  with(power_min,lines(Time,as.numeric(as.character(Sub_metering_2)),col="red"))
  with(power_min,lines(Time,as.numeric(as.character(Sub_metering_3)),col="blue"))
  legend("topright", lty=1, col=c("black","red","blue"),
         legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), cex = 0.098)
  plot(power_min$Time,as.numeric(as.character(power_min$Global_reactive_power)),
       type="l",xlab="datetime",ylab="Global_reactive_power")
})





