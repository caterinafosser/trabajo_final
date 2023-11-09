setwd(paste(getwd(),"/Escritorio/Labo_Cate/trabajo_final",sep=""))
rm(list=ls())

library(ncdf4)
library(lubridate)
library(metR)

archivo<-paste(getwd(),"/skt.sfc.mon.mean.nc",sep="")
nc<-nc_open(archivo)
GlanceNetCDF(nc)

datos<-ReadNetCDF(nc)

#a
datos_81_10<-ReadNetCDF(nc,vars="skt",
                        subset=list(time=c("1981-01-01","2010-12-01")))

media_clima_mens<-aggregate(datos_81_10$skt,list(month(datos_81_10$time),datos_81_10$lon,datos_81_10$lat),mean)
colnames(media_clima_mens)<-list("mes","lon","lat","skt")
#faltan mapas enero y julio

#b
ninio_60_20<-ReadNetCDF(nc,vars="skt",
                        subset=list(lat=list(5:-5),
                                    lon=list(190:240),
                                    time=c("1960-01-01","2020-12-01")))

prom_ninio_60_20<-aggregate(ninio_60_20$skt,list(month(ninio_60_20$time),year(ninio_60_20$time)),mean)  ##tengo un valor de skt para la region Ninio 3.4, para cada mes de cada anio entre 1960-2020


ninio_81_10<-subset(media_clima_mens,lat %in% ninio_60_20$lat)
ninio_81_10<-subset(ninio_81_10,lon %in% ninio_60_20$lon)
ninio_81_10<-subset(ninio_81_10,time %in% c("1981-01-01","2010-12-01"))  ##revisar q hice aca xq creo cagada :p
