setwd("D:/ghDataAndCode/WuYuRong_ocean")

library(ggplot2)
library(data.table)
library(mapproj)
library(rgdal)

data <- fread("surf_disc_2008.csv")

global_shp <- readOGR("./spatial_data/global_country.shp")
global_shp <-  fortify(global_shp)

data_p1 <- subset(data,select = c(V1,V2,V3,V4,V6,V7,year,month))
names(data_p1) <- c("V1","V2","V3","time","lon","lat","year","month")

data_p2 <- subset(data,select = c(V1,V2,V3,V5,V8,V9,year,month))
names(data_p2) <- c("V1","V2","V3","time","lon","lat","year","month")

data <- rbind(data_p1,data_p2)

data <- data[order(data$V1,data$time),]


table(data$month)

mon <- unique(data$month)

mon_group <- function(mon,k,n){
  s <- 1:k
  re <- s+n-1
  
  if (n <= (length(mon)-k)){
    m <- mon[re]
    return(m)
  } else {
    re <- s + length(mon)-k
    m <- mon[re]
    return(m)
  }
}

ggplot(data,aes(x=lon,y=lat))+
  geom_path(aes(group=V1,col=as.factor(V1)))+
  guides(col=FALSE)

ggplot(data,aes(x=lon,y=lat)) + xlim(-180,180) + ylim(-90,90)+ theme_bw()+
   geom_polygon(data=global_shp,aes(x=long,y=lat,group=group),fill="gray80",col="black",lwd=0.6)+
   geom_path(aes(group=V1,col=as.factor(V1)),lwd=0.6)+
   guides(col=FALSE)+
   labs(x="",y="")+ 
   coord_map('ortho', orientation = c(10, -10, 0))

ggplot(data[data$month %in% mon_group(mon,7,2),],aes(x=lon,y=lat)) + 
  xlim(-180,180) + ylim(-90,90)+ theme_bw()+
  geom_polygon(data=global_shp,aes(x=long,y=lat,group=group),fill="gray80",col="black",lwd=0.6)+
  geom_path(aes(group=V1,col=as.factor(V1)),lwd=0.6)+
  guides(col=FALSE)+
  labs(x="",y="")+ 
  coord_map('ortho', orientation = c(10, 170, 0))


ggplot(data[data$month %in% mon_group(mon,7,2),],aes(x=lon,y=lat)) + 
  xlim(-180,180) + ylim(-90,90)+ theme_bw()+
  geom_polygon(data=global_shp,aes(x=long,y=lat,group=group),fill="gray80",col="black",lwd=0.6)+
  geom_path(aes(group=V1,col=as.factor(V1)),lwd=0.6)+
  guides(col=FALSE)+
  labs(x="",y="")+ 
  coord_map('ortho', orientation = c(10, -10, 0))
