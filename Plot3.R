oldwd <- getwd()
setwd(file.path(getwd(),"/exdata-data-NEI_data"))

## Reading the data. It will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#calculating total emissions for PM2.5 in Baltiomire for each year by source
library(dplyr)
EmBal <- NEI%>%
        filter(fips=="24510")%>%
        select(Emissions, year,type)%>%
        group_by(year, type)%>%
        summarize(Emissions = sum(Emissions, na.rm = TRUE))

# Plotting the result
library(ggplot2)
png(filename='plot3.png', width=480, height=480, units='px')
pl <- ggplot(EmBal,aes(y=Emissions,x=year)) + 
        geom_point(aes(color = type)) + 
        labs(title = expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"), x="Year", y=expression("Total PM"[2.5]*" Emission, tons")) + 
        geom_smooth(size = 0.75, linetype = 3, method = "lm", se = FALSE) + 
        facet_grid (type~.) 
print(pl)
dev.off()

setwd(oldwd)