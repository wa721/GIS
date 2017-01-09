#
# Values Script for the Transport Infrastructure Information Platform (LTIIP)
#
# This script clears and subsequently fills the global environment with the 
#  suffucient variables to power the LTIAP. This is designed to be run within 
#  R Studio and requires the working directory to be set to the LTIAP folder 
#  which also holds the ui.R and server.R scripts. 
#
# This script uses the curl and data.table packages to request CSV files from 
#  a GitHub repository. These files form the basis of the analysis and variable 
#  generation and as such, it is critical that these scripts are run on a machine
#  that it connected to the internet. 
#
#................................................................................
#

# Ensure all appropriate packages are installed and attached.
rm(list=ls())

'
library(akima)
library(curl)
library(data.table)
library(Matrix)
library(ggplot2)
library(ggmap)
library(SDMTools)
library(devtools)
install_github("rstudio/leaflet")
library(RColorBrewer)
library(shiny)
library(datasets)
library(deldir)
library(graphics)
library(grDevices)
library(grid)
library(fields)
library(mapproj)
library(maps)
library(maptools)
library(methods)
library(sp)
library(spam)
library(stats)
library(utils)
'


# Download the London Ward Profiles CSV. This is a copy of the data found at:
#  https://data.london.gov.uk/dataset/ward-profiles-and-atlas

WardData <- fread('https://raw.githubusercontent.com/wa721/TIIP/master/WardProfiles.csv')
"Contains National Statistics data © Crown copyright and database right 2012
Contains Ordnance Survey data © Crown copyright and database right 2012"

# Select the appopriate rows so as to ensure ward level data is only included. 

WardData <- WardData[0:625,]

# Download a CSV of Tube Stations and their weighted closeness centrality. This was
#  prepared in Python with the TfL Unified API and Python packages Requests and 
# Networkx.

TubeStations <- fread('https://raw.githubusercontent.com/wa721/TIIP/master/StationsDF.csv')

# Arrange the TubeStations CSV data.table by station ID in order to remove duplicates. 

TubeStations <- TubeStations[with(TubeStations,order(Ids)),]

# Each station in the TubeStations CSV was listed once for each line it is on. This 
#  is a small function to remove duplicate stations from the data.table while ensuring
#  line data for each station remains. This is required to avoid duplicate points
#  during the construction of Voroni tiles.
#

remlist <- numeric(length(TubeStations$Ids))
iterative <- 1

for (station in (1:length(TubeStations$Ids))) {
  if (TubeStations$Ids[station] == TubeStations$Ids[station+1]){
    TubeStations$Line[station + 1] <- paste(TubeStations$Line[station],TubeStations$Line[station + 1],sep=",")
    remlist[iterative] <- station
    iterative <- iterative +1
  }
}
rm(iterative)

remlist <- remlist[1:nnzero(remlist,na.counted = FALSE)]
TubeStations <- TubeStations[-c(remlist),]
rm(remlist)

# Data within the WardData data.table is spatially referenced using the British
# National Grid. UK2LatLon is a function that changes coordinates from the BNG 
# latitudes and longitudes. For the data in WardData data.table, a latidue error 
# of -0.12582 exists. As such, 0.12582 is added to all latitudes following the 
#  use of this function.
#
# This funciton is based on a tutorial provided by Alex Singleton and is available
# at: http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf
#

UK2LatLon <- function(df,EastColumnName,NorthColumnName){
  ukgrid <- "+init=epsg:27700"
  latlong <- "+init=epsg:4326"
  tempdf <- subset(df,select = c(EastColumnName,NorthColumnName))
  coords<-tempdf[,c(1,2)]
  temp_SP<-SpatialPointsDataFrame(coords, data=tempdf,proj4string = CRS(ukgrid))
  temp_SP_LL <- spTransform(temp_SP,CRS(latlong))
  finalDF <- data.frame(temp_SP_LL@coords)
  finalDF
}

# Convert the BNG coordinates in the WardData data.table into latitudes and longitudes
#  ensuring that 0.12582 is added to the latitudes. 

WardData$x <- UK2LatLon(WardData,"x","y")[,1]
WardData$y <- UK2LatLon(WardData,"x","y")[,2] + 0.12582

# This section interpolates the WardData data.table data across the area that London's
#  tube network spans. Given some tube stations lie outside of the Greater London
#  Authority (GLA) and thus outside of the range of the WardData data.table, extrapolation
#  of the interpolation was requried. Given the bounds of the network and the GLA, the 
#  maximum latitude and the maximum latitudes and longitudes were set according to the
#  bounds of the tube network, whereas the minimum bound for the latitude was set as the 
#  minumum latitude for the GLA, so as to promote reliable interpolation of the WardData
#  data.table data. The interpolation is a spline interpolation. 
#

#Define latitude and longitude as new variables for ease of programming

lat <- WardData$y
lon <- WardData$x

l <-  149#iterated number to retain integrity of data. 
interpNumber <- l

interpolatedinc <- interp(lon,
                          lat,
                          WardData$MedHHIncom,
                          xo=seq(min(TubeStations$Longitude),
                                 max(lon),
                                 length =interpNumber),
                          yo=seq(min(lat),
                                 max(TubeStations$Latitude),
                                 length =interpNumber),
                          linear=FALSE,
                          extrap=TRUE,
                          duplicate='error',
                          dupfun=NULL,
                          ncp=NULL,
                          nx=l,
                          ny=l)

interpolatedmage <- interp(lon,
                           lat,
                           WardData$MeanAge13,
                           xo=seq(min(TubeStations$Longitude),
                                  max(lon),
                                  length =interpNumber),
                           yo=seq(min(lat),
                                  max(TubeStations$Latitude),
                                  length =interpNumber),
                           linear=FALSE,
                           extrap=TRUE,
                           duplicate='error',
                           dupfun=NULL,
                           ncp=NULL,
                           nx=l,
                           ny=l)

interpolatedemplo <- interp(lon,
                            lat,
                            WardData$PctHHNoEmp,
                            xo=seq(min(TubeStations$Longitude),
                                   max(lon),
                                   length =interpNumber),
                            yo=seq(min(lat),
                                   max(TubeStations$Latitude),
                                   length =interpNumber),
                            linear=FALSE,
                            extrap=TRUE,
                            duplicate='error',
                            dupfun=NULL,
                            ncp=NULL,
                            nx=l,
                            ny=l)

interpolatedcars <- interp(lon,
                           lat,
                           WardData$CarPerHH11,
                           xo=seq(min(TubeStations$Longitude),
                                  max(lon),
                                  length =interpNumber),
                           yo=seq(min(lat),
                                  max(TubeStations$Latitude),
                                  length =interpNumber),
                           linear=FALSE,
                           extrap=TRUE,
                           duplicate='error',
                           dupfun=NULL,
                           ncp=NULL,
                           nx=l,
                           ny=l)

interpolatedpopu <- interp(lon,
                           lat,
                           (WardData$Pop2015),
                           xo=seq(min(TubeStations$Longitude),
                                  max(lon),
                                  length =interpNumber),
                           yo=seq(min(lat),
                                  max(TubeStations$Latitude),
                                  length =interpNumber),
                           linear=FALSE,
                           extrap=TRUE,
                           duplicate='error',
                           dupfun=NULL,
                           ncp=NULL,
                           nx=l,
                           ny=l)

interpolatedBikeUse <- interp(lon,
                              lat,
                              WardData$PctBike11,
                              xo=seq(min(TubeStations$Longitude),
                                     max(lon),
                                     length =interpNumber),
                              yo=seq(min(lat),
                                     max(TubeStations$Latitude),
                                     length =interpNumber),
                              linear=FALSE,
                              extrap=TRUE,
                              duplicate='error',
                              dupfun=NULL,
                              ncp=NULL,
                              nx=l,
                              ny=l)

interpolatedHousePrices <- interp(lon,
                                  lat,
                                  WardData$MedHouPr14,
                                  xo=seq(min(TubeStations$Longitude),
                                         max(lon),
                                         length =interpNumber),
                                  yo=seq(min(lat),
                                         max(TubeStations$Latitude),
                                         length =interpNumber),
                                  linear=FALSE,
                                  extrap=TRUE,
                                  duplicate='error',
                                  dupfun=NULL,
                                  ncp=NULL,
                                  nx=l,
                                  ny=l)

interpolatedEmployedPeople <- interp(lon,
                                     lat,
                                     WardData$InEmp2011,
                                     xo=seq(min(TubeStations$Longitude),
                                            max(lon),
                                            length =interpNumber),
                                     yo=seq(min(lat),
                                            max(TubeStations$Latitude),
                                            length =interpNumber),
                                     linear=FALSE,
                                     extrap=TRUE,
                                     duplicate='error',
                                     dupfun=NULL,
                                     ncp=NULL,
                                     nx=l,
                                     ny=l)

interpolatedBAME <- interp(lon,
                           lat,
                           WardData$PctBame11,
                           xo=seq(min(TubeStations$Longitude),
                                  max(lon),
                                  length =interpNumber),
                           yo=seq(min(lat),
                                  max(TubeStations$Latitude),
                                  length =interpNumber),
                           linear=FALSE,
                           extrap=TRUE,
                           duplicate='error',
                           dupfun=NULL,
                           ncp=NULL,
                           nx=l,
                           ny=l)

interpolatedSocialHousing <- interp(lon,
                                    lat,
                                    WardData$PctSocRen11,
                                    xo=seq(min(TubeStations$Longitude),
                                           max(lon),
                                           length =interpNumber),
                                    yo=seq(min(lat),
                                           max(TubeStations$Latitude),
                                           length =interpNumber),
                                    linear=FALSE,
                                    extrap=TRUE,
                                    duplicate='error',
                                    dupfun=NULL,
                                    ncp=NULL,
                                    nx=l,
                                    ny=l)

interpolatedJobSeekers <- interp(lon,
                                 lat,
                                 WardData$JSA2014,
                                 xo=seq(min(TubeStations$Longitude),
                                        max(lon),
                                        length =interpNumber),
                                 yo=seq(min(lat),
                                        max(TubeStations$Latitude),
                                        length =interpNumber),
                                 linear=FALSE,
                                 extrap=TRUE,
                                 duplicate='error',
                                 dupfun=NULL,
                                 ncp=NULL,
                                 nx=l,
                                 ny=l)

interpolatedVoterTurnout <- interp(lon,
                                   lat,
                                   WardData$Turnout12,
                                   xo=seq(min(TubeStations$Longitude),
                                          max(lon),
                                          length =interpNumber),
                                   yo=seq(min(lat),
                                          max(TubeStations$Latitude),
                                          length =interpNumber),
                                   linear=FALSE,
                                   extrap=TRUE,
                                   duplicate='error',
                                   dupfun=NULL,
                                   ncp=NULL,
                                   nx=l,
                                   ny=l)

interpolatedWorkingAge <- interp(lon,
                                 lat,
                                 WardData$Aged16_64,
                                 xo=seq(min(TubeStations$Longitude),
                                        max(lon),
                                        length =interpNumber),
                                 yo=seq(min(lat),
                                        max(TubeStations$Latitude),
                                        length =interpNumber),
                                 linear=FALSE,
                                 extrap=TRUE,
                                 duplicate='error',
                                 dupfun=NULL,
                                 ncp=NULL,
                                 nx=l,
                                 ny=l)

interpolatedDeprivationRank <- interp(lon,
                                      lat,
                                      WardData$IDRankLond,
                                      xo=seq(min(TubeStations$Longitude),
                                             max(lon),
                                             length =interpNumber),
                                      yo=seq(min(lat),
                                             max(TubeStations$Latitude),
                                             length =interpNumber),
                                      linear=FALSE,
                                      extrap=TRUE,
                                      duplicate='error',
                                      dupfun=NULL,
                                      ncp=NULL,
                                      nx=l,
                                      ny=l)
interpolatedCrime <- interp(lon,
                            lat,
                            WardData$CriRt1314,
                            xo=seq(min(TubeStations$Longitude),
                                   max(lon),
                                   length =interpNumber),
                            yo=seq(min(lat),
                                   max(TubeStations$Latitude),
                                   length =interpNumber),
                            linear=FALSE,
                            extrap=TRUE,
                            duplicate='error',
                            dupfun=NULL,
                            ncp=NULL,
                            nx=l,
                            ny=l)


  
rm(lat)
rm(lon)
rm(interpNumber)
rm(l)

# The akima interp function produces z values in a 2 dimensional matrix and x and y values 
#  as lists. To allow the data to be attached to tube stations at a later stage, the format
#  is restructured into a three column data.table. Data.table is used instead of data.frame
#  given it's superior performance with large datasets and in loops.
#

ReOrderInterp <- function(Interpolation){
  
  NumNA <- nnzero(Interpolation$z,na.counted = FALSE)

  z <- numeric(NumNA)
  x <- numeric(NumNA)
  y <- numeric(NumNA)

  inc <- 1
  for (columnInt in (1:length(Interpolation$x))){
    for (rowInt in (1:length(Interpolation$y))){
      if (!(is.na(Interpolation$z[rowInt,columnInt]))){
        z[inc] <- Interpolation$z[rowInt,columnInt]
        x[inc] <- Interpolation$x[columnInt]
        y[inc] <- Interpolation$y[rowInt]
        inc <- inc + 1  
      }
    }
  }
  interpolatedDF <- data.table(x=x,y=y,z=z)
  interpolatedDF
}

interpolatedcars <- ReOrderInterp(interpolatedcars)
interpolatedinc <- ReOrderInterp(interpolatedinc)
interpolatedmage <- ReOrderInterp(interpolatedmage)
interpolatedemplo <- ReOrderInterp(interpolatedemplo)
interpolatedpopu <- ReOrderInterp(interpolatedpopu)
interpolatedBikeUse <- ReOrderInterp(interpolatedBikeUse)
interpolatedHousePrices <- ReOrderInterp(interpolatedHousePrices)
interpolatedEmployedPeople <- ReOrderInterp(interpolatedEmployedPeople)
interpolatedBAME <- ReOrderInterp(interpolatedBAME)
interpolatedSocialHousing <- ReOrderInterp(interpolatedSocialHousing)
interpolatedJobSeekers <- ReOrderInterp(interpolatedJobSeekers)
interpolatedVoterTurnout <- ReOrderInterp(interpolatedVoterTurnout)
interpolatedWorkingAge <- ReOrderInterp(interpolatedWorkingAge)
interpolatedDeprivationRank <- ReOrderInterp(interpolatedDeprivationRank)
interpolatedCrime <- ReOrderInterp(interpolatedCrime)

# A Deloni tesselation and set of Voroni polygons were created for the tube stations
#  in order to allocate the interpolated spatial data to each relevant tube station.
#

tesselation <- deldir(TubeStations$Longitude,TubeStations$Latitude)
voroniTiles <- tile.list(tesselation)

# This function attaches a given interpolated variable to tube stations if the variable
#  is located within or on the bounds of its Voroni tile. This function is based on advice
#  from a tutorial which can be found at: 
#  http://flowingdata.com/2016/04/12/voronoi-diagram-and-delaunay-triangulation-in-r/
# 
# For all variables except for population, the value attatched to the appropriate tube
#  station is the mean of the values within the Voroni tiles. For population closest to
#  the tube station in question, the sum is found. This will allow prospective users to
#  consider whether or not the weighted centrality of tube stations within a network 
#  is appropriately proportional to the populations which they serve. 
# 

attachVariables <- function(variable){
  px<- variable$x
  py<- variable$y
  Z <- variable$z
  pointsDF <- data.frame(px,py)
  additionalVariable <- numeric(270)
  if (!(variable[1,3] == interpolatedpopu[1,3])){
    for (item in (1:270)) {
    x <- voroniTiles[[item]]$x
    y <- voroniTiles[[item]]$y
    polyDF <- data.frame(x,y)
    
    resultsDF <- data.table(x = pointsDF$px,Res = pnt.in.poly(pnts=pointsDF,poly.pnts = polyDF)[,3],Val = Z)
    rm(polyDF,x,y)
    
    additionalVariable[item] <- resultsDF[Res == 1, mean(Val)]
      }
    }
  if (variable[1,3] == interpolatedpopu[1,3]){
    for (item in (1:270)) {
    x <- voroniTiles[[item]]$x
    y <- voroniTiles[[item]]$y
    polyDF <- data.frame(x,y)
        
    resultsDF <- data.table(x = pointsDF$px,Res = pnt.in.poly(pnts=pointsDF,poly.pnts = polyDF)[,3],Val = Z)
    rm(x,y,polyDF)
    additionalVariable[item] <- resultsDF[Res == 1, sum(Val)]
      }
    }
  additionalVariable
  }

# Attach household car ownership, household income, employment, age, population and bike
#  use in the areas surrouding a given station to that station using the attachVariables
#  function.
#

TubeStations$CarOwnership <- attachVariables(interpolatedcars)
TubeStations$Income <- attachVariables(interpolatedinc)
TubeStations$Age <- attachVariables(interpolatedmage)
TubeStations$Employment <- attachVariables(interpolatedemplo)
TubeStations$Popu <- attachVariables(interpolatedpopu)
TubeStations$BikeUse <- attachVariables(interpolatedBikeUse)
TubeStations$HousePrices <- attachVariables(interpolatedHousePrices)
TubeStations$EmployedPeople <- attachVariables(interpolatedEmployedPeople)
TubeStations$BAME <- attachVariables(interpolatedBAME)
TubeStations$SocialHousing <- attachVariables(interpolatedSocialHousing)
TubeStations$JobSeekers <- attachVariables(interpolatedJobSeekers)
TubeStations$VoterTurnout <- attachVariables(interpolatedVoterTurnout)
TubeStations$WorkingAge <- attachVariables(interpolatedWorkingAge)
TubeStations$DeprivationRank <- attachVariables(interpolatedDeprivationRank)
TubeStations$Crime <- attachVariables(interpolatedCrime)

PopAdjustFactor <- function (variable){
  sum(TubeStations$variable)/sum(WardData$variable)
  #TubeStations$variable/adj
}
  
TubeStations$Popu <- TubeStations$Popu/(sum(TubeStations$Popu)/sum(WardData$Pop2015))
TubeStations$EmployedPeople <- TubeStations$EmployedPeople/(sum(TubeStations$EmployedPeople)/sum(WardData$InEmp2011))
TubeStations$WorkingAge <- TubeStations$WorkingAge/(sum(TubeStations$WorkingAge)/sum(WardData$Pct16_64))

# Develop plots of the aforementioned variables to the weighted closeness centrality of tube
#  stations and generate a list of plot summaries set as variableCodes.
#

degrees <- 2

MedianAbsoluteDeviation <- function (dataset) {
    difstor <- numeric(length(dataset))
    med <- median(dataset)
    for (element in 1:length(dataset)){
      difstor[element] <- abs(element - med)
    }
    median(difstor)
}

Outliers <- function (dataset) {
  rowstore <- numeric()
  MAD <- MedianAbsoluteDeviation(dataset)
  shifter <- 1
  for (rowindex in 1:length(dataset)){
    if (abs(dataset[rowindex]-median(dataset)) > 1.4826*degrees*MAD){
      rowstore[shifter] <- rowindex
      shifter <- shifter + 1
    }
  }
  rowstore[1:nnzero(rowstore,na.counted = FALSE)]
}



if (!(is.na(Outliers(TubeStations$Income)[1]))){
  IncomeStations <- TubeStations[-c(Outliers(TubeStations$Income)),]
  IncomeStations <- IncomeStations[-71,]
} else {
  IncomeStations <- TubeStations[-c(71,135,142),] #Epping's income is impossible.
}

if (!(is.na(Outliers(TubeStations$Age)[1]))){
  AgeStations <- TubeStations[-c(Outliers(TubeStations$Age)),]
} else {
  AgeStations <- TubeStations[-c(48,108),]
}

if (!(is.na(Outliers(TubeStations$CarOwnership)[1]))){
  CarStations <- TubeStations[-c(Outliers(TubeStations$CarOwnership)),]
} else {
  CarStations <- TubeStations
}

if (!(is.na(Outliers(TubeStations$Popu)[1]))){
  PopStations <- TubeStations[-c(Outliers(TubeStations$Popu)),]
  PopStations <- PopStations[-40,]
} else {
  PopStations <- TubeStations[-48,]
}

if (!(is.na(Outliers(TubeStations$Employment)[1]))){
  EmployStations <- TubeStations[-c(Outliers(TubeStations$Employment)),]
  EmployStations <- EmployStations[-c(71,48)]
} else {
  EmployStations <- TubeStations[-c(71,48)]
}

if (!(is.na(Outliers(TubeStations$BikeUse))[1])){
  BikeStations <- TubeStations[-c(Outliers(TubeStations$BikeUse)),]
} else {
  BikeStations <- TubeStations
}

if (!(is.na(Outliers(TubeStations$HousePrices))[1])){
  HousePricesStations <- TubeStations[-c(Outliers(TubeStations$HousePrices)),]
} else {
  HousePricesStations <- TubeStations
}

if (!(is.na(Outliers(TubeStations$EmployedPeople))[1])){
  EmployedPeopleStations <- TubeStations[-c(Outliers(TubeStations$EmployedPeople)),]
} else {
  EmployedPeopleStations <- TubeStations
}

if (!(is.na(Outliers(TubeStations$BAME))[1])){
  BAMEStations <- TubeStations[-c(Outliers(TubeStations$BAME)),]
} else {
  BAMEStations <- TubeStations
}

if (!(is.na(Outliers(TubeStations$SocialHousing))[1])){
  SocialHousingStations <- TubeStations[-c(Outliers(TubeStations$SocialHousing)),]
} else {
  SocialHousingStations <- TubeStations[-c(71,48)]
}

if (!(is.na(Outliers(TubeStations$JobSeekers))[1])){
  JobSeekersStations <- TubeStations[-c(Outliers(TubeStations$JobSeekers)),]
} else {
  JobSeekersStations <- TubeStations[-c(71,48)]
}

if (!(is.na(Outliers(TubeStations$VoterTurnout))[1])){
  VoterTurnoutStations <- TubeStations[-c(Outliers(TubeStations$VoterTurnout)),]
} else {
  VoterTurnoutStations <- TubeStations[-c(71,48)]
}

if (!(is.na(Outliers(TubeStations$WorkingAge))[1])){
  WorkingAgeStations <- TubeStations[-c(Outliers(TubeStations$WorkingAge)),]
} else {
  WorkingAgeStations <- TubeStations
}

if (!(is.na(Outliers(TubeStations$DeprivationRank))[1])){
  DeprivationRankStations <- TubeStations[-c(Outliers(TubeStations$DeprivationRank)),]
} else {
  DeprivationRankStations <- TubeStations[-c(71,48)]
}

if (!(is.na(Outliers(TubeStations$Crime))[1])){
  CrimeStations <- TubeStations[-c(Outliers(TubeStations$Crime)),]
} else {
  CrimeStations <- TubeStations
}




variableCodes <- list(summary(lm(as.formula(IncomeStations$Income ~ IncomeStations$ClosenessCentrality))),
                      summary(lm(as.formula(AgeStations$Age ~ AgeStations$ClosenessCentrality))),
                      summary(lm(as.formula(CarStations$CarOwnership ~ CarStations$ClosenessCentrality))),
                      summary(lm(as.formula(PopStations$Popu ~ PopStations$ClosenessCentrality))),
                      summary(lm(as.formula(EmployStations$Employment ~ EmployStations$ClosenessCentrality))),
                      summary(lm(as.formula(BikeStations$BikeUse ~ BikeStations$ClosenessCentrality))),
                      summary(lm(as.formula(HousePricesStations$HousePrices ~ HousePricesStations$ClosenessCentrality))),
                      summary(lm(as.formula(EmployedPeopleStations$EmployedPeople ~ EmployedPeopleStations$ClosenessCentrality))),
                      summary(lm(as.formula(BAMEStations$BAME ~ BAMEStations$ClosenessCentrality))),
                      summary(lm(as.formula(SocialHousingStations$SocialHousing ~ SocialHousingStations$ClosenessCentrality))),
                      summary(lm(as.formula(JobSeekersStations$JobSeekers ~ JobSeekersStations$ClosenessCentrality))),
                      summary(lm(as.formula(VoterTurnoutStations$VoterTurnout ~ VoterTurnoutStations$ClosenessCentrality))),
                      summary(lm(as.formula(WorkingAgeStations$WorkingAge ~ WorkingAgeStations$ClosenessCentrality))),
                      summary(lm(as.formula(DeprivationRankStations$DeprivationRank ~ DeprivationRankStations$ClosenessCentrality))),
                      summary(lm(as.formula(CrimeStations$Crime ~ CrimeStations$ClosenessCentrality)))
                      )


rIncome <- ggplot(WardData) + 
  geom_point(data = IncomeStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = IncomeStations$ClosenessCentrality, y= IncomeStations$Income), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[1]]$coefficients[1,1],
              slope = variableCodes[[1]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.") +
  ylab(label = "Average household income in GBP") +
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))

rAge <- ggplot(WardData) + 
  geom_point(data = AgeStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = AgeStations$ClosenessCentrality, y= AgeStations$Age), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[2]]$coefficients[1,1],
              slope = variableCodes[[2]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.") +
  ylab(label = "Average age of residents.") + 
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))


rCars <- ggplot(WardData) +  
  geom_point(data = CarStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = CarStations$ClosenessCentrality, y= CarStations$CarOwnership), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[3]]$coefficients[1,1],
              slope = variableCodes[[3]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.")  +
  ylab(label = "Average car ownership per household.") + 
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))


rPopulation <- ggplot(WardData) + 
  geom_point(data = PopStations,
             inherit.aes = FALSE, 
             mapping = aes(x = PopStations$ClosenessCentrality, y= PopStations$Popu),
             colour = "blue") +
  geom_abline(intercept = variableCodes[[4]]$coefficients[1,1],
              slope = variableCodes[[4]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.") +
  ylab(label = "Number of people closest to station.") + 
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))

rEmployment <- ggplot(WardData) + 
  geom_point(data = EmployStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = EmployStations$ClosenessCentrality, y= EmployStations$Employment), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[5]]$coefficients[1,1],
              slope = variableCodes[[5]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.") +
  ylab(label = "Unemployment rate.") +
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))

rBike <- ggplot(WardData) + 
  geom_point(data = BikeStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = BikeStations$ClosenessCentrality, y= BikeStations$BikeUse), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[6]]$coefficients[1,1],
              slope = variableCodes[[6]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.")  +
  ylab(label = "Percentage of residents who commute by bicyle.") + 
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))

rHousePrices <- ggplot(WardData) + 
  geom_point(data = HousePricesStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = HousePricesStations$ClosenessCentrality, y= HousePricesStations$HousePrices), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[7]]$coefficients[1,1],
              slope = variableCodes[[7]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.")  +
  ylab(label = "Average house price.") +
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))

rEmployedPeople <- ggplot(WardData) + 
  geom_point(data = EmployedPeopleStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = EmployedPeopleStations$ClosenessCentrality, y= EmployedPeopleStations$EmployedPeople), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[8]]$coefficients[1,1],
              slope = variableCodes[[8]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.")  +
  ylab(label = "Number of employed people cloest to station.") +
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))

rBAME <- ggplot(WardData) + 
  geom_point(data = BAMEStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = BAMEStations$ClosenessCentrality, y= BAMEStations$BAME), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[9]]$coefficients[1,1],
              slope = variableCodes[[9]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.")  +
  ylab(label = "Percentage of people of black, asian or minority ethnicity.") + 
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))


rSocialHousing <- ggplot(WardData) + 
  geom_point(data = SocialHousingStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = SocialHousingStations$ClosenessCentrality, y= SocialHousingStations$SocialHousing), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[10]]$coefficients[1,1],
              slope = variableCodes[[10]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.")  +
  ylab(label = "Percentage of dwelings that are socially rented.") + 
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))

rJobSeekers <- ggplot(WardData) + 
  geom_point(data = JobSeekersStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = JobSeekersStations$ClosenessCentrality, y= JobSeekersStations$JobSeekers), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[11]]$coefficients[1,1],
              slope = variableCodes[[11]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.")  +
  ylab(label = "Percentage of people on a job seekers allowance.") +
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))

rVoterTurnout <- ggplot(WardData) + 
  geom_point(data = VoterTurnoutStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = VoterTurnoutStations$ClosenessCentrality, y= VoterTurnoutStations$VoterTurnout), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[12]]$coefficients[1,1],
              slope = variableCodes[[12]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.")  +
  ylab(label = "Voter turnout in 2011 mayoral elections.") +
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))

rWorkingAge <- ggplot(WardData) + 
  geom_point(data = WorkingAgeStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = WorkingAgeStations$ClosenessCentrality, y= WorkingAgeStations$WorkingAge), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[13]]$coefficients[1,1],
              slope = variableCodes[[13]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.")  +
  ylab(label = "Number of working aged people (16-64 years).") + 
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))

rDeprivationRank <- ggplot(WardData) + 
  geom_point(data = DeprivationRankStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = DeprivationRankStations$ClosenessCentrality, y= DeprivationRankStations$DeprivationRank), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[14]]$coefficients[1,1],
              slope = variableCodes[[14]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.")  +
  ylab(label = "The average deprivation rank (lower value represents more deprived).")+
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))

rCrime <- ggplot(WardData) + 
  geom_point(data = CrimeStations, 
             inherit.aes = FALSE, 
             mapping = aes(x = CrimeStations$ClosenessCentrality, y= CrimeStations$Crime), 
             colour = "blue") +
  geom_abline(intercept = variableCodes[[15]]$coefficients[1,1],
              slope = variableCodes[[15]]$coefficients[2,1]) +
  xlab(label = "Average time to other stations, weighted by zone.")  +
  ylab(label = "Average crime rate in area.") +
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),axis.line= element_line(size=2,colour="black"))


RelationList <- list(rIncome,rAge,rCars,rPopulation,
                     rEmployment,rBike, rHousePrices, 
                     rEmployedPeople, rBAME,
                     rSocialHousing, rJobSeekers,
                     rVoterTurnout, rWorkingAge,
                     rDeprivationRank, rCrime)

# Develop palettes for each variable in order to allow for colourimetric distinction between the 
#  relevant variable in the leaflet plot. These plattes are suitable for people with colour blindness
#  and were also chosen to allow for simple distinction between values and visual congurency with 
#  the theme of the Shiny application. 
# 

palCar <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$CarOwnership)
palIncome <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$Income)
palAge <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$Age)
palPop <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$Popu)
palEmploy <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$Employment)
palBikeUse <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$BikeUse)
palHousePrices <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$HousePrices)
palEmployedPeople <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$EmployedPeple)
palBAME <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$BAME)
palSocialHousing <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$SocialHousing)
palJobSeekers <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$JobSeekers)
palVoterTurnout <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$VoterTurnout)
palWorkingAge <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$WorkingAge)
palDeprivationRank <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$DeprivationRank)
palCrime <- colorNumeric(
  palette = "PuBu",
  domain = TubeStations$Crime)

# These are the leaflet plots for each variable. The colour is a function of the selected WardData
#  variable whereas the radius of the circle markers are a function of the weighted closeensss centrality
#  of the tube stations. This allows for simple  mutlivariate interpretation of spatial data. 
# 
# Leaflet was chosen over ggplot2 given the subjectively aethetically pleasing nature of the pan and
#  zoom funcitons within leaflet plots. 
#

mCar <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palCar(TubeStations$CarOwnership),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Cars per Household: ", round(TubeStations$CarOwnership,digits=1)))%>%
  addLegend("bottomright", pal = palCar, values = TubeStations$CarOwnership,
            opacity = 1 )%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")

mIncome <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palIncome(TubeStations$Income),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,"," ," Lines: ",TubeStations$Line,","," Average Household Income in Pounds: ", round(TubeStations$Income,digits=0)))%>%
  addLegend("bottomright", pal = palIncome, values = TubeStations$Income,
            labFormat = labelFormat(prefix = "£"),
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")

mAge <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palAge(TubeStations$Age),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Average Age: ", round(TubeStations$Age,digits=1)))%>%
  addLegend("bottomright", pal = palAge, values = TubeStations$Age,
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")

mPop <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palPop(TubeStations$Popu),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Service Population: ",round(TubeStations$Popu,digits=0)))%>%
  addLegend("bottomright", pal = palPop, values = TubeStations$Popu,
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")

mEmploy <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palEmploy(TubeStations$Employment),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Unemployment Rate: ",round(TubeStations$Employment,digits=2)))%>%
  addLegend("bottomright", pal = palEmploy, values = TubeStations$Employment,
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")
  

mBikeUse <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palBikeUse(TubeStations$BikeUse),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Percentage of service population who ride bicycles: ",round(TubeStations$BikeUse,digits=2)))%>%
  addLegend("bottomright", pal = palBikeUse, values = TubeStations$BikeUse,
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")

mHousePrices <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palHousePrices(TubeStations$HousePrices),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Average house price: ",round(TubeStations$HousePrices,digits=0)))%>%
  addLegend("bottomright", pal = palHousePrices, values = TubeStations$HousePrices,
            opacity = 1, labFormat = labelFormat(prefix = "£"))%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")

mEmployedPeople <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palEmployedPeople(TubeStations$EmployedPeople),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Number of employed people: ",round(TubeStations$EmployedPeople,digits=2)))%>%
  addLegend("bottomright", pal = palEmployedPeople, values = TubeStations$EmployedPeople,
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")

mBAME <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palBAME(TubeStations$BAME),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Percentage of population who are of black, asian or minority ethnicity: ",round(TubeStations$BAME,digits=2)))%>%
  addLegend("bottomright", pal = palBAME, values = TubeStations$BAME,
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")

mJobSeekers <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palJobSeekers(TubeStations$JobSeekers ),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Percentage of population on job seekers allowance: ",round(TubeStations$JobSeekers,digits=2)))%>%
  addLegend("bottomright", pal = palJobSeekers , values = TubeStations$JobSeekers ,
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")

mSocialHousing <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palSocialHousing (TubeStations$SocialHousing  ),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Percentage of dwellings that are socally rented: ",round(TubeStations$SocialHousing ,digits=2)))%>%
  addLegend("bottomright", pal = palSocialHousing , values = TubeStations$SocialHousing  ,
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")

mVoterTurnout <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palVoterTurnout (TubeStations$VoterTurnout  ),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Voter turnout at 2011 Mayoral elections: ",round(TubeStations$VoterTurnout ,digits=2)))%>%
  addLegend("bottomright", pal = palVoterTurnout , values = TubeStations$VoterTurnout  ,
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")

mWorkingAge <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palWorkingAge (TubeStations$WorkingAge  ),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Number of working age people (16-64 years): ",round(TubeStations$WorkingAge ,digits=2)))%>%
  addLegend("bottomright", pal = palWorkingAge , values = TubeStations$WorkingAge  ,
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")

mDeprivationRank <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palDeprivationRank (TubeStations$DeprivationRank  ),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Average UK-wide deprivation rank: ",round(TubeStations$DeprivationRank ,digits=2)))%>%
  addLegend("bottomright", pal = palDeprivationRank , values = TubeStations$DeprivationRank  ,
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")


mCrime <- leaflet(TubeStations) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = TubeStations$Longitude, 
    lat = TubeStations$Latitude, 
    radius = TubeStations$ClosenessCentrality, 
    fillColor = palCrime (TubeStations$Crime  ),
    color = "black",
    fillOpacity = 1, 
    popup = paste("Name: ",TubeStations$CommonName,","," Lines: ",TubeStations$Line,","," Average crime rate: ",round(TubeStations$Crime ,digits=2)))%>%
  addLegend("bottomright", pal = palCrime , values = TubeStations$Crime  ,
            opacity = 1)%>%
  addScaleBar(position = "bottomleft")%>%
  addControl(img(src="northSign.png"),position = "topright")


# Leaflet plots are collated into a list for integration with Shiny input selections. 
# 



leafList <- list(mIncome,mAge, mCar,mPop,mEmploy, mBikeUse, 
                 mHousePrices, mEmployedPeople, mBAME,
                 mSocialHousing, mJobSeekers, mVoterTurnout,
                 mWorkingAge, mDeprivationRank, mCrime)

connectivityDist <- ggplot(TubeStations,aes(ClosenessCentrality)) + 
  geom_density() +
  xlab(label ="Weighted average time to other stations") +
  ylab(label = "Proportion of stations out of 1") + 
  ggtitle(label = "Distribution of weighted average time to other stations") +
  theme(panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"), axis.line= element_line(size=2,colour="black"))


#
# END
#
#..............................................................................................
#
#Edits
# Plot Labels
# t tests for SLG and adjusted alpha value for a t test
#   review QM coursework hints for this. 
# Improve the commentary. 
#
# Move the legend title to the bottom left as a text box. 
#
# Adjust the centrality labelling. 
#
# 
sum(TubeStations$Popu)
min(TubeStations$Popu)