oldwd <- getwd()
setwd(file.path(getwd(),"/exdata-data-NEI_data"))

## Reading the data. It will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


library(dplyr)

## searching for the coal combustion-related sources 
CoalComb <- SCC %>%
        filter(grepl('coal',EI.Sector, ignore.case = T) & grepl('comb', EI.Sector, ignore.case = T))%>%
        select(SCC)

## substracting info regarding PM2.5 emissions from the coal combustion-related sources from NEI dataset
NEICoalComb <- merge(NEI, CoalComb, by  = "SCC")

## calculating total emissions for PM2.5 from the coal combustion-related sources  for each year
TotCoalComb <- aggregate(Emissions ~ year, data = NEICoalComb, sum, na.rm = TRUE)
        

# Plotting the result
library(ggplot2)
png(filename='plot4.png', width=480, height=480, units='px')
pl <- ggplot(TotCoalComb,aes(factor(year),Emissions/10^5)) + 
        geom_bar(stat = "identity", fill = "grey") +
        labs(title = expression("PM"[2.5]*" Coal Combustion Source Emissions Across US from 1999-2008"), x="Year", y=expression("Total PM"[2.5]*" Emission(10^5 tons)"))+
        theme_bw()
print(pl)

dev.off()

setwd(oldwd)