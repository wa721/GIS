
censuscsv <- fread('https://raw.githubusercontent.com/wa721/LondonData/master/LondonData.csv')
censuscsv <- censuscsv[0:625,]

PTALcsv <- fread('https://raw.githubusercontent.com/wa721/LondonData/master/2014%20PTAI%20%2B%20PTAL%20Data.csv')

lat <- censuscsv$y
lon <- censuscsv$x
inc <- censuscsv$MedHHIncom
mage <- censuscsv$MeanAge13
employ <-censuscsv$EmpRt2011
cars <- censuscsv$CarPerHH11
ptal <- censuscsv$PTAL14
popu <- censuscsv$PopDenKm13

l <- 625

interpolatedinc <- interp(lon,lat,inc,xo=seq(min(lon),max(lon),length = l),yo=seq(min(lat),max(lat),length = l),linear=FALSE,extrap=FALSE,duplicate='error',dupfun=NULL,ncp=NULL,nx=l,ny=l)
interpolatedmage <- interp(lon,lat,mage,xo=seq(min(lon),max(lon),length = l),yo=seq(min(lat),max(lat),length = l),linear=FALSE,extrap=FALSE,duplicate='error',dupfun=NULL,ncp=NULL,nx=l,ny=l)
interpolatedemplo <- interp(lon,lat,employ,xo=seq(min(lon),max(lon),length = l),yo=seq(min(lat),max(lat),length = l),linear=FALSE,extrap=FALSE,duplicate='error',dupfun=NULL,ncp=NULL,nx=l,ny=l)
interpolatedcars <- interp(lon,lat,cars,xo=seq(min(lon),max(lon),length = l),yo=seq(min(lat),max(lat),length = l),linear=FALSE,extrap=FALSE,duplicate='error',dupfun=NULL,ncp=NULL,nx=l,ny=l)
#interpolatedptalData <- interp(lon,lat,ptal,xo=seq(min(lon),max(lon),length = l),yo=seq(min(lat),max(lat),length = l),linear=FALSE,extrap=FALSE,duplicate='error',dupfun=NULL,ncp=NULL,nx=l,ny=l)
interpolatedPTALData <- interp(PTALcsv$X,PTALcsv$Y,PTALcsv$PTAL2014Num,xo=seq(min(lon),max(lon),length = l),yo=seq(min(lat),max(lat),length = l),linear=FALSE,extrap=FALSE,duplicate='error',dupfun=NULL,ncp=NULL,nx=l,ny=l)
#interpptal <- interp(lon,lat,ptal,xo=seq(min(lon),max(lon),length = l),yo=seq(min(lat),max(lat),length = l),linear=FALSE,extrap=FALSE,duplicate='error',dupfun=NULL,ncp=NULL,nx=l,ny=l)
interpolatedpopu <- interp(lon,lat,popu,xo=seq(min(lon),max(lon),length = l),yo=seq(min(lat),max(lat),length = l),linear=FALSE,extrap=FALSE,duplicate='error',dupfun=NULL,ncp=NULL,nx=l,ny=l)

#image.plot(interpolatedIncomeData)
#image.plot(interpolatedAgeData)
#image.plot(interpolatedEmploymentData)
#image.plot(interpolatedCarsData)
#image.plot(interpolatedptalData)
#image.plot(interpolatedPTALData)
#image.plot(interpolatedpopuData)

variables <- list("Mean Household Income","Mean Age","Household Car Ownership","Population Density","Employment Rate")
variableCodes <- list("inc","mage","cars","popu","employ")
variableindex <- list(inc,mage,cars,popu,employ,ptal)

interplist <- list(interpolatedinc,interpolatedmage,interpolatedcars,interpolatedpopu,interpolatedemplo)
