oldwd <- getwd()
setwd(file.path(getwd(),"/exdata-data-NEI_data"))

## Reading the data. It will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#calculating total emissions for each year
tEm <-  aggregate(Emissions ~ year, data = NEI, sum, na.rm = TRUE)

# Plotting the result
png(filename='plot1.png', width=480, height=480, units='px')
oldpar <- par(no.readonly = TRUE)
par(mar = c( 5.1, 4.1, 4.1, 2.1), lty = 1, pch = 17)
with(tEm, plot(year, Emissions/10^6, type = "b", main = "Total Emissions from PM2.5 in US, 1999 - 2008", xaxt = "n", cex.lab = 1.25, ylim = c(0,8), ylab = "PM2.5 Emissions, in tons * 10^6", xlab = "Year"))
with(tEm, axis(1, at=as.integer(year), las=1))
par(oldpar)
dev.off()

setwd(oldwd)