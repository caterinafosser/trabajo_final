setwd(paste(getwd(),"/Escritorio/Labo_Cate/trabajo_final",sep=""))
rm(list=ls())

library(ncdf4)
library(lubridate)
library(metR)

archivo<-paste(getwd(),"/skt.sfc.mon.mean.nc",sep="")
nc<-nc_open(archivo)
GlanceNetCDF(nc)

datos<-ReadNetCDF(nc)

#a-----
datos_81_10<-ReadNetCDF(nc,vars="skt",
                        subset=list(time=c("1981-01-01","2010-12-01")))

media_clima_mens<-aggregate(datos_81_10$skt,list(month(datos_81_10$time),datos_81_10$lon,datos_81_10$lat),mean)
colnames(media_clima_mens)<-list("mes","lon","lat","skt")
#faltan mapas enero y julio

#b-----
{
ninio_60_20<-ReadNetCDF(nc,vars="skt",
                        subset=list(lat=list(5:-5),
                                    lon=list(190:240),
                                    time=c("1960-01-01","2020-12-01")))

prom_ninio_60_20<-aggregate(ninio_60_20$skt,list(month(ninio_60_20$time),year(ninio_60_20$time)),mean)
colnames(prom_ninio_60_20)<-list("mes","anio","skt_ninio_3.4")
}
##tengo un valor de skt para la region Ninio 3.4, para cada mes de cada anio entre 1960-2020. 
#Graficar esta serie!!

{
ninio_81_10<-subset(media_clima_mens,lat %in% ninio_60_20$lat)
ninio_81_10<-subset(ninio_81_10,lon %in% ninio_60_20$lon)
prom_ninio_81_10<-aggregate(ninio_81_10$skt,list(month(ninio_81_10$mes)),mean)
colnames(prom_ninio_81_10)<-list("mes","skt_clim_3.4")
}
#estos son los datos de media climatologia mensual para skt en ninio 3.4. 
#los datos de anomalia los obtengo restando el valor de cada mes entre 1960-2020 con el
#valor del mes correspondiente de esta serie:

{
anom_ninio_60_20<-c()
for (i in 1:nrow(prom_ninio_60_20)){
  for (j in 1:12){
    if (prom_ninio_60_20$mes[i]==j){
      anom_ninio_60_20[i]<-prom_ninio_60_20$skt_ninio_3.4[i]-prom_ninio_81_10$skt_clim_3.4[which(prom_ninio_81_10$mes==j)]
    }
  }
}
}
##Graficar la serie y marcar umbrales +-0.5

##c-----

#serie de promedios moviles trimestrales:
{
media_movil_anom_ninio<-c()
for (i in 2:length(anom_ninio_60_20)-1){
  media_movil_anom_ninio<-c(media_movil_anom_ninio,(anom_ninio_60_20[i-1]+anom_ninio_60_20[i]+anom_ninio_60_20[i+1])/3)
}
}

{

}