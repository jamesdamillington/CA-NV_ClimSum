#Create rasters of precipitation seasonality 
#From CA, NV climate data from http://albers.cnr.berkeley.edu
#For Patrick Baldwin IGS

rm(list = ls())
library(raster)
library(tidyverse)
library(ncdf4)

url <- "http://albers.cnr.berkeley.edu/data/noaa/livneh/CA_NV/"  
dir <- "C:/Users/k1076631/Google Drive/Teaching/2018-19/Undergrad/IGS/Patrick/CA-NV_ClimSum/"
fp <- "livneh_CA_NV_15Oct2014."

#function to download nc file and create on HD
download_nc <- function(url, dir, fileprefix, year, month)
{
  link <- paste0(url,fileprefix,create_yrmon(year,month),".nc")
  dest <- paste0(dir,"Data/CANV",create_yrmon(year,month),".nc")
  download.file(url=link,destfile=dest,mode="wb") 
}

#function to create year month text string for files
#assumed Nov and Dec from previous year are part of THIS year
create_yrmon <- function(year,mon)
{
  ifelse(mon > 10,
    return(paste0(year-1,mon)),  #if Nov, Dec, use previous year
    return(paste0(year,sprintf("%02d",mon))) #else use this year
  )
}

#function to extract @prec' layer from nc file and output as raster
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

#loop to work through mutiple years
years <- seq(1971,2010,1)

for(year in years)
{
  #download data for this year
  for(i in seq(1,12,1))
  {
    download_nc(url, dir, fp, year, i)
  }
  
  #create winter brick (six months)
  winter <- stack()
  win <- c(11, 12, seq(1,4,1))
  for(j in win)
  {
   r <- nc_raster(paste0(dir,"Data/CANV", create_yrmon(year, j),".nc"))
   winter <- stack(winter,r)
  }
  names(winter) <- c("Nov","Dec","Jan","Feb","Mar","Apr")
  
  #create summer brick (six months)
  summer <- stack()
  summ <- c(seq(5,10,1))
  for(j in summ)
  {
   r <- nc_raster(paste0(dir,"Data/CANV", create_yrmon(year, j),".nc"))
   summer <- stack(summer,r)
  }
  names(summer) <- c("May","Jun","Jul","Aug","Sep","Oct")
  
  #sum values (to get total pptn) in thebricks
  wintersum <- stackApply(winter,indices=rep.int(1,6),fun=sum,na.rm=FALSE)
  summersum <- stackApply(summer,indices=rep.int(1,6),fun=sum,na.rm=FALSE)
  
  #calc sasonality (total winter pptn minus total summer pptn) 
  seasonality <- wintersum - summersum
  
  #write files to disk
  writeRaster(seasonality, paste0(dir,"pptnSeasonality_",year,".asc"),format="ascii",overwrite=TRUE)
  writeRaster(wintersum, paste0(dir,"pptnWinter_Sum_",year,".asc"),format="ascii",overwrite=TRUE)
  writeRaster(summersum, paste0(dir,"pptnSummer_Sum_",year,".asc"),format="ascii",overwrite=TRUE)
  
  print(paste0(year," complete."))
}

