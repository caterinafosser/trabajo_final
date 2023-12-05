setwd(paste(getwd(),"/Escritorio/Labo_Cate/trabajo_final",sep=""))
rm(list=ls())

#cargo librerias que voy a usar
{library(ncdf4)
library(lubridate)
library(metR)
library(ggplot2)
library(gridExtra)
library(cowplot)}

archivo<-paste(getwd(),"/skt.sfc.mon.mean.nc",sep="")
nc<-nc_open(archivo);rm(archivo)
GlanceNetCDF(nc) #miro las caracteristicas de los datos

#a-----
datos_81_10<-ReadNetCDF(nc,vars="skt", #datos que me interesan p climatologia
                        subset=list(time=c("1981-01-01","2010-12-01")))
#media climatologica para cada punto de grilla:
media_clima_mens<-aggregate(datos_81_10$skt,list(month(datos_81_10$time),datos_81_10$lon,datos_81_10$lat),mean);rm(datos_81_10)
colnames(media_clima_mens)<-list("mes","lon","lat","skt")

#me quedo con enero y julio para graficarlos:
enero<-subset(media_clima_mens,mes==1)
julio<-subset(media_clima_mens,mes==7)

##mapa centrado en la long. de cambio de fecha:
mapa <- map_data("world2")
mi_mapa <- geom_path(data = mapa, aes(long, lat, group = group),
                     linewidth = 0.1);rm(mapa)
#mapa de enero:
plot_enero<- ggplot(data=enero, mapping = aes(x= lon, y=lat))+
  geom_contour_fill(aes(z=skt)) +
  geom_contour(aes(z = skt),
               color = "black",
               size = 0.2) +
  scale_fill_stepsn(n.breaks=8,
                    colours = c("#277da1","#4d908e","#43aa8b","#90be6d","#f9c74f","#f8961e","#f3722c","#f94144"),
                    guide = guide_colorsteps(ticks = T),
                    limits=c(-60,40))+
  mi_mapa+
  coord_sf(xlim=range(enero$lon),ylim=range(enero$lat),expand=F)+
  labs(x = "Longitud",
      y = "Latitud",
      fill = "skt [°C]",
      title = "Temperatura en superficie en Enero - Climatologia 1981-2010")+
  theme(
    plot.title = element_text(hjust = 0.5));rm(enero)

#mapa de julio:
plot_julio<- ggplot(data=julio, mapping = aes(x= lon, y=lat))+
  geom_contour_fill(aes(z=skt)) +
  geom_contour(aes(z = skt),
               color = "black",
               size = 0.2) +
  scale_fill_stepsn(n.breaks=8,
                    colours = c("#277da1","#4d908e","#43aa8b","#90be6d","#f9c74f","#f8961e","#f3722c","#f94144"),
                    guide = guide_colorsteps(ticks = T),
                    limits=c(-60,40))+
  mi_mapa+
  coord_sf(xlim=range(julio$lon),ylim=range(julio$lat),expand=F)+
  labs(x = "Longitud",
       y = "Latitud",
       fill = "skt [°C]",
       title = "Temperatura en superficie en Julio - Climatologia 1981-2010")+
  theme(
    plot.title = element_text(hjust = 0.5));rm(julio)

#mapa de julio con recuadro:
plot_julio_mas_cajita<-plot_julio+geom_rect(xmin = 190, xmax = 240, ymin = -5, ymax = 5,fill=NA,colour="black")

#junto las figuras para que esten en un mismo panel:
plot_both<-plot_grid(plot_enero + theme(legend.position = "none"),
                     plot_julio + theme(legend.position = "right"),
                     ncol=2,rel_widths = c(1, 1.14))

#b-----
###
###marco zona de El Ninio 3.4 en el mapa (tomo los datos y hago medias de nuevo):
cajita_81_10<-ReadNetCDF(nc,vars="skt",
                        subset=list(lat=list(15:-15), #limites mayores a los de la region
                                    lon=list(180:250),
                                    time=c("1981-01-01","2010-12-01")))
media_clima_cajita<-aggregate(cajita_81_10$skt,list(month(cajita_81_10$time),cajita_81_10$lon,cajita_81_10$lat),mean);rm(cajita_81_10)
colnames(media_clima_cajita)<-list("mes","lon","lat","skt")

#me quedo con julio para graficar:
cajita_julio<-subset(media_clima_cajita,mes==7); rm(media_clima_cajita)

plot_cajita_julio<-ggplot(data=cajita_julio, mapping = aes(x= lon, y=lat))+
  geom_contour_fill(aes(z=skt)) +
  geom_contour(aes(z = skt),
               color = "black",
               size = 0.2) +
  scale_fill_stepsn(n.breaks=8,
                    colours = c("#277da1","#4d908e","#43aa8b","#90be6d","#f9c74f","#f8961e","#f3722c","#f94144"),
                    guide = guide_colorsteps(ticks = T),
                    limits=c(22,30))+
  mi_mapa+
  coord_sf(xlim=range(cajita_julio$lon),ylim=range(cajita_julio$lat),expand=F)+
  geom_rect(xmin = 190, xmax = 240, ymin = -5, ymax = 5,fill=NA,colour="black")+
  labs(x="Longitud",
       y="Latitud",
       title="Temperatura superficial en la región Niño 3.4 - Climatologia 1981-2010",
       fill = "skt [°C]")+
  theme(
    plot.title = element_text(hjust = 0.5));rm(cajita_julio,mi_mapa)


#ahora si trabajo con los datos de el ninio 3.4
{
ninio_60_20<-ReadNetCDF(nc,vars="skt",
                        subset=list(lat=list(5:-5),
                                    lon=list(190:240),
                                    time=c("1960-01-01","2020-12-01"))); rm(nc)

prom_ninio_60_20<-aggregate(ninio_60_20$skt,list(month(ninio_60_20$time),year(ninio_60_20$time)),mean)
colnames(prom_ninio_60_20)<-list("mes","anio","skt_ninio_3.4")
}
##tengo un valor de skt para la region Ninio 3.4, para cada mes de cada anio entre 1960-2020. 

{
ninio_81_10<-subset(media_clima_mens,lat %in% ninio_60_20$lat); rm(media_clima_mens)
ninio_81_10<-subset(ninio_81_10,lon %in% ninio_60_20$lon); rm(ninio_60_20)
prom_ninio_81_10<-aggregate(ninio_81_10$skt,list(month(ninio_81_10$mes)),mean); rm(ninio_81_10)
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
rm(prom_ninio_81_10)
}

fechas<-seq(as.Date("1960-01-01"),by="month",length.out=732)
datos_ninio_60_20<-data.frame(fechas,anom_ninio_60_20); rm(fechas) #para poder graficar con ggplot2

serie<-ggplot()+
      geom_line(data=datos_ninio_60_20,mapping=aes(y = anom_ninio_60_20,x=fechas))+
      geom_hline(yintercept = 0.5,color="#F94144",linetype="longdash")+
      geom_hline(yintercept = -0.5,color="#277DA1",linetype="longdash")+
      geom_text(aes(x = min(datos_ninio_60_20$fechas), y = 0.5, label = 0.5,hjust=1.5,vjust=-1))+
      geom_text(aes(x = min(datos_ninio_60_20$fechas), y = -0.5, label = -0.5,hjust=1.5,vjust=1.5))+
      labs(x="Año",
           y="Anomalía",
           title="Serie de anomalías de skt en la región Niño 3.4",
           subtitle="con respecto al período 1981-2010")+
      theme_minimal()
##c-----

#serie de promedios moviles trimestrales adelantados:
{
media_movil_anom_ninio<-c()
for (i in 2:length(anom_ninio_60_20)-1){
  media_movil_anom_ninio<-c(media_movil_anom_ninio,(anom_ninio_60_20[i-1]+anom_ninio_60_20[i]+anom_ninio_60_20[i+1])/3)
}
rm(anom_ninio_60_20)
}

#busco ninios
{
n<-0
fecha_i<-c()
fecha_f<-c()
duracion<-c()
barras_ninios<-matrix(NA,nrow=732,ncol=1)
for (i in 1:length(media_movil_anom_ninio)){
  if (media_movil_anom_ninio[i]>=0.5){
    n<-n+1
    if (media_movil_anom_ninio[i+1]<0.5){
      if (n>5){
        duracion<-c(duracion,n)
        fecha_i<-c(fecha_i,paste(prom_ninio_60_20[i-(n-1),1],prom_ninio_60_20[i-(n-1),2],sep="-"))
        fecha_f<-c(fecha_f,paste(prom_ninio_60_20[i,1],prom_ninio_60_20[i,2],sep="-"))
        barras_ninios[(i-(n-1)):i]<-3
      }
      n<-0
    }
  }
}
datos_ninio_60_20$barras<-(as.vector(barras_ninios)); rm(barras_ninios)
#archivo en formato ascii con info:
eventos_ninio<-data.frame("Fecha_inicio"=fecha_i,"Fecha_fin"=fecha_f,"Duracion_en_meses"=duracion)
info_eventos_ninio<-paste(getwd(),"/eventos_ninio.txt",sep="")
write.table(eventos_ninio,info_eventos_ninio,quote=F);rm(eventos_ninio,info_eventos_ninio)
}

#busco ninias
{
n<-0
fecha_i<-c()
fecha_f<-c()
duracion<-c()
barras_ninias<-matrix(NA,nrow=732,ncol=1)
for (i in 1:(length(media_movil_anom_ninio)-1)){  ##hago esto pq mi ultimo dato es <-0.5 (no entra en ninia tho)
  if (media_movil_anom_ninio[i]<=-0.5){
    n<-n+1
    if (media_movil_anom_ninio[i+1]> -0.5){ 
      if (n>5){
        duracion<-c(duracion,n)
        fecha_i<-c(fecha_i,paste(prom_ninio_60_20[i-(n-1),1],prom_ninio_60_20[i-(n-1),2],sep="-"))
        fecha_f<-c(fecha_f,paste(prom_ninio_60_20[i,1],prom_ninio_60_20[i,2],sep="-"))
        barras_ninias[(i-(n-1)):i]<--3
      }
      n<-0
    }
  }
}
datos_ninio_60_20$barras2<-(as.vector(barras_ninias)); rm(barras_ninias)
#archivo en formato ascii con info:
eventos_ninia<-data.frame("Fecha_inicio"=fecha_i,"Fecha_fin"=fecha_f,"Duracion_en_meses"=duracion)
info_eventos_ninia<-paste(getwd(),"/eventos_ninia.txt",sep="")  ##modificar ruta de salida
write.table(eventos_ninia,info_eventos_ninia,quote=F);rm(eventos_ninia,info_eventos_ninia)
rm(duracion,fecha_f,fecha_i,i,j,n,media_movil_anom_ninio,prom_ninio_60_20)
}

#serie de anomalias con ninios y ninias superpuestos como barras
serie2<-ggplot()+
  geom_bar(data=datos_ninio_60_20,mapping=aes(y = barras,x=fechas,color="El Niño"),stat="identity",alpha=0.01)+
  geom_bar(data=datos_ninio_60_20,mapping=aes(y = barras2,x=fechas,color="La Niña"),stat="identity",alpha=0.01)+
  geom_line(data=datos_ninio_60_20,mapping=aes(y = anom_ninio_60_20,x=fechas))+
  geom_hline(yintercept = 0.5,color="#F94144",linetype="longdash")+
  geom_hline(yintercept = -0.5,color="#277DA1",linetype="longdash")+
  scale_color_manual(values=c("El Niño"="#F47E3E","La Niña"="#50B99A"))+
  labs(x="Año",
       y="Anomalía",
       title="Serie de anomalías de skt en la región Niño 3.4",
       subtitle="con respecto al período 1981-2010",
       color="Eventos ENSO")+
  theme_minimal()
#e----

{soi<-read.table(file=paste(getwd(),"/soi.txt",sep=""),skip = 3,header=T)
soi_60_20<-subset(soi,soi$YEAR>=1960&soi$YEAR<=2020);rm(soi)
soi_60_20$YEAR<-NULL
soi_60_20<-as.matrix(soi_60_20)
soi_60_20<-matrix(t(soi_60_20),ncol=1)}

datos_ninio_60_20$soi<-as.vector(soi_60_20); rm(soi_60_20)
 
serie3<-ggplot()+
  geom_line(data=datos_ninio_60_20,mapping=aes(y = anom_ninio_60_20,x=fechas,color="Anom. skt"))+
  geom_line(data=datos_ninio_60_20,mapping=aes(y=soi,x=fechas,color="SOI"))+
  geom_hline(yintercept = 0.5,color="#F94144",linetype="dashed")+
  geom_hline(yintercept = -0.5,color="#277DA1",linetype="dashed")+
  scale_color_manual(values=c("Anom. skt"="#E75A0D","SOI"="#3A9278"))+
  labs(x="Año",
       y="Anomalía/Índice",
       title="Serie de anomalías de skt e índice SOI en la región Niño 3.4",
       color="Índices ENSO")+
  theme_minimal()

#FiN! :)