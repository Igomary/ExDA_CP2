oldwd <- getwd()
setwd(file.path(getwd(),"/exdata-data-NEI_data"))

## Reading the data. It will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


library(dplyr)

## searching for the motor vehicle sources 
MotorVehicles <- SCC %>%
        filter(grepl('vehicles',EI.Sector, ignore.case = T))%>%
        select(SCC)

## substracting info regarding PM2.5 emissions from the motor vehicle sources from NEI dataset
NEIMotorVehicles <- merge(NEI, MotorVehicles, by  = "SCC")


## calculating total emissions for PM2.5 from the motor vehicle sources  for each year in Baltimore City
TotMotorVehicles <- NEIMotorVehicles%>%
        filter(fips=="24510")%>%
        select(Emissions, year)%>%
        group_by(year)%>%
        summarize(Emissions = sum(Emissions, na.rm = TRUE))        

# Plotting the result
library(ggplot2)
png(filename='plot5.png', width=480, height=480, units='px')
pl <- ggplot(TotMotorVehicles,aes(factor(year),Emissions)) + 
        geom_bar(stat = "identity", fill = "grey") +
        labs(title = expression("PM"[2.5]*" Motor Vehicles Emissions in Baltimore City from 1999-2008"), x="Year", y=expression("Total PM"[2.5]*" Emission(tons)"))+
        theme_bw()
print(pl)
dev.off()

setwd(oldwd)