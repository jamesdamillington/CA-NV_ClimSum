
rm(list = ls())
library(raster)
library(tidyverse)
library(ncdf4)

ncyear <- 2000

url <- "http://albers.cnr.berkeley.edu/data/noaa/livneh/CA_NV/"  
dir <- "C:/Users/k1076631/Google Drive/Teaching/2018-19/Undergrad/IGS/Patrick/CA-NV_ClimSum/"
fp <- "livneh_CA_NV_15Oct2014."

download_nc <- function(url, dir, fileprefix, year, month)
{
  link <- paste0(url,fileprefix,create_yrmon(year,month),".nc")
  dest <- paste0(dir,"CANV",create_yrmon(year,month),".nc")
  download.file(url=link,destfile=dest,mode="wb") 
}

delete_year <- function(dir, fileprefix, year, month)
{
  print(paste0(dir,"CANV",create_yrmon(year,month),".nc"))
  unlink(paste0(dir,"CANV",create_yrmon(year,month),".nc"),force=TRUE)
}

create_yrmon <- function(year,mon)
{
  ifelse(mon > 10,
    return(paste0(year-1,mon)), 
    return(paste0(year,sprintf("%02d",mon)))
  )
}


nc_raster <- function(filename)
{
  nc1 <- nc_open(filename)
  
  pre_array <- ncvar_get(nc1,"Prec")
  lon <- ncvar_get(nc1,"lon")
  lat <- ncvar_get(nc1,"lat")
  
  M <- length(lon)
  N <- length(lat)
  dx <- diff(lon[1:2])
  dy <- diff(lat[1:2])
  
  r <- raster(t(pre_array[,,1][,N:1]), xmn=lon[1]-dx/2, xmx=lon[M]+dx/2, ymn=lat[1]-dy/2, ymx=lat[N]+dy/2, crs=CRS("+init=epsg:4326"))
  
  return(r)
}


mons <- seq(1,12,1)
year <- 2002
  
for(i in mons)
{
  download_nc(url, dir, fp, year, i)
}

#create winter brick
winter <- stack()
win <- c(11, 12, seq(1,4,1))
  
for(j in win)
{
 print(create_yrmon(year, j))  
 r <- nc_raster(paste0(dir,"CANV", create_yrmon(year, j),".nc"))
 winter <- stack(winter,r)
}
names(winter) <- c("Nov","Dec","Jan","Feb","Mar","Apr")
plot(winter)

wintersum <- stackApply(winter,indices=rep.int(1,6),fun=sum,na.rm=FALSE)
plot(wintersum)

#create summer brick
summer <- stack()
summ <- c(seq(5,10,1))
  
for(j in summ)
{
 print(create_yrmon(year, j))  
 r <- nc_raster(paste0(dir,"CANV", create_yrmon(year, j),".nc"))
 summer <- stack(summer,r)
}
names(summer) <- c("May","Jun","Jul","Aug","Sep","Oct")
plot(summer)

summersum <- stackApply(summer,indices=rep.int(1,6),fun=sum,na.rm=FALSE)
plot(summersum)  

#calc sasonality
seasonality <- wintersum - summersum
plot(seasonality)

writeRaster(seasonality, paste0(dir,"pptnSeasonality_",year,".asc"),format="ascii")
writeRaster(wintersum, paste0(dir,"pptnWinter_Sum_",year,".asc"),format="ascii")
writeRaster(summersum, paste0(dir,"pptnSummer_Sum_",year,".asc"),format="ascii")
  

for(i in mons)
{
  delete_year(dir, fp, year, i)
}


