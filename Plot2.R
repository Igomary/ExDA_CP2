oldwd <- getwd()
setwd(file.path(getwd(),"/exdata-data-NEI_data"))

## Reading the data. It will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#calculating total emissions for PM2.5 in Baltiomire for each year
library(dplyr)
tEmBal <- NEI%>%
        filter(fips=="24510")%>%
        select(Emissions, year)%>%
        group_by(year)%>%
        summarize(Emissions = sum(Emissions, na.rm = TRUE))
       

# Plotting the result
png(filename='plot2.png', width=480, height=480, units='px')
oldpar <- par(no.readonly = TRUE)
par(mar = c( 5.1, 4.1, 4.1, 2.1), lty = 1, pch = 20)
with(tEmBal, plot(year, Emissions, type = "b", main = "Total Emissions from PM2.5 in Baltimore City, 1999 - 2008", xaxt = "n", cex.lab = 1.25, ylab = "PM2.5 Emissions, in tons", xlab = "Year"))
with(tEmBal, axis(1, at=as.integer(year), las=1))
model <- lm(Emissions ~ year, tEmBal) 
abline(model, lwd = 1, col = "lightgray" )
par(oldpar)
dev.off()

setwd(oldwd)