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
        summarize(Emissions = sum(Emissions, na.rm = TRUE)) %>%
        mutate(City = as.factor(rep("Baltimore")))

## creating a new column with values, representing changes of emissions comparing to the first value (1999, total)
TotMotorVehicles[,"EmissionsPer"] <- 0
for(i in 1:4){
        E <- TotMotorVehicles$Emissions[i]/TotMotorVehicles$Emissions[1]
        TotMotorVehicles[i,"EmissionsPer"]<- E
}

## calculating total emissions for PM2.5 from the motor vehicle sources  for each year in LA City
TotMotorVehiclesLA <- NEIMotorVehicles%>%
        filter(fips == "06037")%>%
        select(Emissions, year)%>%
        group_by(year)%>%
        summarize(Emissions = sum(Emissions, na.rm = TRUE))%>%
        mutate(City = as.factor(rep("LA")))

## creating a new column with values, representing changes of emissions comparing to the first value (1999, total)
TotMotorVehiclesLA[,"EmissionsPer"] <- 0
for(i in 1:4){
        E <- TotMotorVehiclesLA$Emissions[i]/TotMotorVehiclesLA$Emissions[1]
        TotMotorVehiclesLA[i,"EmissionsPer"]<- E
}


## binding dataframes for plotting
ComTotMV <- rbind(TotMotorVehicles,TotMotorVehiclesLA)

# Plotting the result (using gridExtra library for combining two plots in one file)
library(ggplot2)
library(grid)
library(gridExtra)

png(filename='plot6.png', width=960, height=480, units='px')
pl <- ggplot(ComTotMV,aes(y=EmissionsPer*100,x=year)) + 
        geom_line(aes(color = City)) + 
        labs(x="Year", y="Emission changes, in %") + 
        facet_grid (City~.) 

pr <- ggplot(ComTotMV,aes(y=Emissions,x=year)) + 
        geom_line(aes(color = City)) + 
        labs(x="Year", y="Emission changes, in tones") + 
        facet_grid (City~.) 
print(grid.arrange(pl, pr, ncol = 2))
dev.off()

setwd(oldwd)