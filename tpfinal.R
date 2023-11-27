setwd(paste(getwd(),"/Escritorio/Labo_Cate/trabajo_final",sep=""))
rm(list=ls())

library(ncdf4)
library(lubridate)
library(metR)
library(ggplot2)
library(RColorBrewer)
require(gridExtra)
#display.brewer.all()

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

##mapa centrado en la long. de cambio de fecha:
mapa <- map_data("world2")
mi_mapa <- geom_path(data = mapa, aes(long, lat, group = group),
                     linewidth = 0.1)
#mapa de enero:
plot_enero<- ggplot(data=enero, mapping = aes(x= lon, y=lat))+
  geom_contour_fill(aes(z=skt)) +
  geom_contour(aes(z = skt),
               color = "black",
               size = 0.2) +
  #scale_fill_gradient2(low = "#19126A",
  #                     mid = "#E6E2C3",
  #                    high = "#B11F2C",)+
  #scale_fill_gradientn(colors = hcl.colors(6,palette = "RdYlBu",rev = T),guide = guide_colourbar(reverse = T))+
  #scale_fill_distiller(palette(value=c("#E6E2C3","#B11F2C")),
  #                     direction = -1,
  #                     guide = guide_colorsteps(barheight = 10,
  #                                              barwidth =1)) +
  scale_fill_stepsn(n.breaks=8,
                    colours = c("#277da1","#4d908e","#43aa8b","#90be6d","#f9c74f","#f8961e","#f3722c","#f94144"),
                    guide = guide_colorsteps(ticks = T),
                    limits=c(-60,40))+
  mi_mapa+
  coord_sf(xlim=range(enero$lon),ylim=range(enero$lat),expand=F)+
  labs(x = "Longitud",
      y = "Latitud",
      fill = "skt [°C]",
      title = "Temperatura en superficie en Enero - Climatologia 1981-2010")

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
       title = "Temperatura en superficie en Julio - Climatologia 1981-2010")
#mapa de julio con recuadro:
plot_julio_mas_cajita<-plot_julio+geom_rect(xmin = 190, xmax = 240, ymin = -5, ymax = 5,fill=NA,colour="black")

#junto las figuras:
figura<-grid.arrange(plot_enero,plot_julio,nrow=1)



#b-----
###marco zona de El Ninio 3.4 en el mapa (tomo los datos y hago medias de nuevo):
cajita_81_10<-ReadNetCDF(nc,vars="skt",
                        subset=list(lat=list(15:-15),
                                    lon=list(180:250),
                                    time=c("1981-01-01","2010-12-01")))
media_clima_cajita<-aggregate(cajita_81_10$skt,list(month(cajita_81_10$time),cajita_81_10$lon,cajita_81_10$lat),mean)
colnames(media_clima_cajita)<-list("mes","lon","lat","skt")

cajita_julio<-subset(media_clima_cajita,mes==7)

plot_cajita_julio<-ggplot(data=cajita_julio, mapping = aes(x= lon, y=lat))+
  geom_contour_fill(aes(z=skt)) +
  geom_contour(aes(z = skt),
               color = "black",
               size = 0.2) +
  scale_fill_stepsn(n.breaks=8,
                    colours = c("#277da1","#4d908e","#43aa8b","#90be6d","#f9c74f","#f8961e","#f3722c","#f94144"),
                    guide = guide_colorsteps(ticks = T),
                    limits=c(20,30))+
  mi_mapa+
  coord_sf(xlim=range(cajita_julio$lon),ylim=range(cajita_julio$lat),expand=F)+
  geom_rect(xmin = 190, xmax = 240, ymin = -5, ymax = 5,fill=NA,colour="black")


{
ninio_60_20<-ReadNetCDF(nc,vars="skt",
                        subset=list(lat=list(5:-5),
                                    lon=list(190:240),
                                    time=c("1960-01-01","2020-12-01")))

prom_ninio_60_20<-aggregate(ninio_60_20$skt,list(month(ninio_60_20$time),year(ninio_60_20$time)),mean)
colnames(prom_ninio_60_20)<-list("mes","anio","skt_ninio_3.4")
}
##tengo un valor de skt para la region Ninio 3.4, para cada mes de cada anio entre 1960-2020. 

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

fechas<-seq(as.Date("1960-01-01"),by="month",length.out=732)
datos_ninio_60_20<-data.frame(fechas,anom_ninio_60_20) #para poder graficar con ggplot2

serie<-ggplot()+
      #geom_bar(data=datos_ninio_60_20,mapping=aes(y = barras,x=fechas),stat="identity",alpha=0.01,color="pink")+
      #geom_bar(data=datos_ninio_60_20,mapping=aes(y = barras2,x=fechas),stat="identity",alpha=0.01,color="lightblue")+
      geom_line(data=datos_ninio_60_20,mapping=aes(y = anom_ninio_60_20,x=fechas))+
      geom_hline(yintercept = 0.5,color="#F94144",linetype="dashed")+
      geom_hline(yintercept = -0.5,color="#277DA1",linetype="dashed")+
      geom_text(aes(x = min(datos_ninio_60_20$fechas), y = 0.5, label = 0.5,hjust=1.5,vjust=-1))+
      geom_text(aes(x = min(datos_ninio_60_20$fechas), y = -0.5, label = -0.5,hjust=1.5,vjust=1.5))
     
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
datos_ninio_60_20$barras<-("barras"=as.vector(barras_ninios))
#archivo en formato ascii con info:
eventos_ninio<-data.frame("Fecha_inicio"=fecha_i,"Fecha_fin"=fecha_f,"Duracion_en_meses"=duracion)
info_eventos_ninio<-"/home/clinux01/Escritorio/Labo_Cate/trabajo_final/eventos_ninio.txt"  ##modificar ruta de salida
write.table(eventos_ninio,info_eventos_ninio,quote=F)
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
datos_ninio_60_20$barras2<-("barras2"=as.vector(barras_ninias))
#archivo en formato ascii con info:
eventos_ninia<-data.frame("Fecha_inicio"=fecha_i,"Fecha_fin"=fecha_f,"Duracion_en_meses"=duracion)
info_eventos_ninia<-"/home/clinux01/Escritorio/Labo_Cate/trabajo_final/eventos_ninia.txt"  ##modificar ruta de salida
write.table(eventos_ninia,info_eventos_ninia,quote=F)
}

#serie de anomalias con ninios y ninias superpuestos como barras
serie2<-ggplot()+
  geom_bar(data=datos_ninio_60_20,mapping=aes(y = barras,x=fechas),stat="identity",alpha=0.01,color="#F47E3E")+
  geom_bar(data=datos_ninio_60_20,mapping=aes(y = barras2,x=fechas),stat="identity",alpha=0.01,color="#50B99A")+
  geom_line(data=datos_ninio_60_20,mapping=aes(y = anom_ninio_60_20,x=fechas))+
  geom_hline(yintercept = 0.5,color="#F94144",linetype="longdash")+
  geom_hline(yintercept = -0.5,color="#277DA1",linetype="longdash")

#e----

{soi<-read.table(file=paste(getwd(),"/soi.txt",sep=""),skip = 3,header=T)
soi_60_20<-subset(soi,soi$YEAR>=1960&soi$YEAR<=2020)
soi_60_20$YEAR<-NULL
soi_60_20<-as.matrix(soi_60_20)
soi_60_20<-matrix(t(soi_60_20),ncol=1)}

datos_ninio_60_20$soi<-as.vector(soi_60_20)
 
serie3<-ggplot()+
  geom_line(data=datos_ninio_60_20,mapping=aes(y = anom_ninio_60_20,x=fechas))+
  geom_hline(yintercept = 0.5,color="red",linetype="dashed")+
  geom_hline(yintercept = -0.5,color="blue",linetype="dashed")+
  geom_line(data=datos_ninio_60_20,mapping=aes(y=soi,x=fechas))

##probar haciendo medias trimestrales moviles del SOI (grafico aparte)
