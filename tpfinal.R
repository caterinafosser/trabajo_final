setwd(paste(getwd(),"/Escritorio/Labo_Cate/trabajo_final",sep=""))
rm(list=ls())

library(ncdf4)
library(lubridate)
library(metR)
library(ggplot2)

archivo<-paste(getwd(),"/skt.sfc.mon.mean.nc",sep="")
nc<-nc_open(archivo)
GlanceNetCDF(nc)

datos<-ReadNetCDF(nc)

#a-----
datos_81_10<-ReadNetCDF(nc,vars="skt",
                        subset=list(time=c("1981-01-01","2010-12-01")))

media_clima_mens<-aggregate(datos_81_10$skt,list(month(datos_81_10$time),datos_81_10$lon,datos_81_10$lat),mean)
colnames(media_clima_mens)<-list("mes","lon","lat","skt")

enero<-subset(media_clima_mens,mes==1)
julio<-subset(media_clima_mens,mes==7)

mapa <- map_data("world")
mi_mapa <- geom_path(data = mapa, aes(long, lat, group = group),
                     linewidth = 0.1)
plot_enero<- ggplot(data=enero, mapping = aes(x= ConvertLongitude(lon), y=lat))+
  geom_contour_fill(aes(z=skt)) +
  geom_contour(aes(z = skt),
               color = "black",
               size = 0.2) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1) +
  mi_mapa+
  coord_sf(expand=FALSE)+  ##consultar xq del lado derecho ""faltan""
  labs(x = "Longitud",
      y = "Latitud",
      fill = "skt [°C]",
      title = "Temperatura en superficie")

library(RColorBrewer)
display.brewer.all()


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
#estos son los datos de media climatologica mensual para skt en ninio 3.4. 
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

#serie de promedios moviles trimestrales adelantados:
{
media_movil_anom_ninio<-c()
for (i in 2:length(anom_ninio_60_20)-1){
  media_movil_anom_ninio<-c(media_movil_anom_ninio,(anom_ninio_60_20[i-1]+anom_ninio_60_20[i]+anom_ninio_60_20[i+1])/3)
}
}

#busco ninios
{
n<-0
fecha_i<-c()
fecha_f<-c()
duracion<-c()
for (i in 1:length(media_movil_anom_ninio)){
  if (media_movil_anom_ninio[i]>=0.5){
    n<-n+1
    if (media_movil_anom_ninio[i+1]<0.5){
      if (n>5){
        duracion<-c(duracion,n)
        fecha_i<-c(fecha_i,paste(prom_ninio_60_20[i-(n-1),1],prom_ninio_60_20[i-(n-1),2],sep="-"))
        fecha_f<-c(fecha_f,paste(prom_ninio_60_20[i,1],prom_ninio_60_20[i,2],sep="-"))
      }
      n<-0
    }
  }
}
eventos_ninio<-data.frame("Fecha inicio"=fecha_i,"Fecha fin"=fecha_f,"Duracion (meses)"=duracion)
}

#busco ninias
n<-0
fecha_i<-c()
fecha_f<-c()
duracion<-c()
for (i in 1:(length(media_movil_anom_ninio)-1)){
  if (media_movil_anom_ninio[i]<=-0.5){
    n<-n+1
    if (media_movil_anom_ninio[i+1]> -0.5){   ##XQ NO ANDA???!! LLORO
      if (n>5){
        duracion<-c(duracion,n)
        fecha_i<-c(fecha_i,paste(prom_ninio_60_20[i-(n-1),1],prom_ninio_60_20[i-(n-1),2],sep="-"))
        fecha_f<-c(fecha_f,paste(prom_ninio_60_20[i,1],prom_ninio_60_20[i,2],sep="-"))
      }
      n<-0
    }
  }
}
eventos_ninia<-data.frame("Fecha inicio"=fecha_i,"Fecha fin"=fecha_f,"Duracion (meses)"=duracion)

