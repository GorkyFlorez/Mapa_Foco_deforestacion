library(RPostgres)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
library(ggnewscale)
dvr         <- RPostgres::Postgres()
db          <- 'postgres'  ##Nombre de la BBDD
host_db     <- 'localhost'
db_port     <- '5432' 
db_user     <- 'postgres'  ##Tu usuario
db_password <- 'gflorezc' ##Tu contraseña 

# 3.0 Conexión
con <- dbConnect(dvr, dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
dbListTables(con)



Dis_MDD<- st_read(con, layer = "Dis_MDD")
Dis_MDD <- st_transform(Dis_MDD, crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Provi_Peru<- st_read(con, layer = "Provi_Peru")
Provi_Peru <- st_transform(Provi_Peru, crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

SurAmerica <- st_read(con, layer = "SurAmerica")
SurAmerica <- st_transform(SurAmerica, crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Dis_MDD_box = st_as_sfc(st_bbox(Dis_MDD))

Defores = stack("Raster/Deforesta_MDD.tif")

Deforesta_MDD    <- crop(Defores , Dis_MDD)                           #
Deforesta_MDD  <- Deforesta_MDD  <- mask(Deforesta_MDD , Dis_MDD)
plot(Deforesta_MDD)

library(tidyverse)
tbl = rasterToPoints(Deforesta_MDD, spatial = F)
tbl = as_tibble(tbl)
tbl = setNames(tbl, c("x", "y", "year"))
tbl = filter(tbl, year > 0)

tbl_2019 = tbl%>% subset(year > 18)  %>% mutate(Años = 2000 +year)

tbl_2019_SF <- tbl_2019 %>% st_as_sf(coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +no_defs")

summ = tbl_2019 %>%
 summarise(count =n())

tbl_2019_ha = mutate(summ, meters =count *900, has = meters /10000)
tbl_2019_ha 
summary(tbl_2019_ha$has)

SurA= ggplot()+
  
  geom_sf(data = SurAmerica , fill="white", color="black", size=0.05)+
  geom_sf(data = Provi_Peru, fill="gray", color="black", size=0.01)+
  geom_sf(data =Dis_MDD, fill="black", color="black", size=0.01)+
  geom_sf(data = Dis_MDD_box, fill=NA, color="red", size=0.1)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        
        panel.background = element_rect(fill = "#BFD1FF"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1,
           label = "Sur America",size = 3, family="serif", color =
             "black",  fontface="italic", face = "bold")+
             annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1,
                      label = "Pacific ocean",size = 3, family="serif", color =
                        "black",  fontface="italic", angle=90)+
                        annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1,
                                 label = "Atlantic ocean",size = 3, family="serif", color =
                                   "black",  fontface="italic")+
                                   annotate(geom = "text", x = -75, y = -13, hjust = 0, vjust = 1,
                                            label = "Peru",size = 3, family="serif", color =
                                              "black",  fontface="italic")
                                            
SurA


st_kde <- function(points,cellsize, bandwith, extent = NULL){
  require(MASS)
  require(raster)
  require(sf)
  if(is.null(extent)){
    extent_vec <- st_bbox(points)[c(1,3,2,4)]
  } else{
    extent_vec <- st_bbox(extent)[c(1,3,2,4)]
  }
  
  n_y <- ceiling((extent_vec[4]-extent_vec[3])/cellsize)
  n_x <- ceiling((extent_vec[2]-extent_vec[1])/cellsize)
  
  extent_vec[2] <- extent_vec[1]+(n_x*cellsize)-cellsize
  extent_vec[4] <- extent_vec[3]+(n_y*cellsize)-cellsize
  
  coords <- st_coordinates(points)
  matrix <- kde2d(coords[,1],coords[,2],h = bandwith,n = c(n_x,n_y),lims = extent_vec)
  raster(matrix)
}


points_kd <- st_kde(tbl_2019_SF,0.01,0.3)
plot(points_kd)


points_kde<- crop(points_kd  ,Dis_MDD ) 

Dens_ras   <-  rasterToPoints(points_kde)
Dens_raster   <-  data.frame(Dens_ras)
colnames(Dens_raster ) = c("X", "Y", "den")

summary(Dens_raster$den)
Dens_raster %>%
  subset(den <= 2.1449908 & den> 0.01) ->Dens_raster_focos


colores<- c("#FFFFBF" ,"#FFFF95","#FFFF6A" ,"#FFFF40",
                    "#FFFF15", "#FFFF00", "#FFF200", "#FFE400", "#FFD700", 
                    "#FFC900", "#FFBC00", "#FFAE00","#FFA100", "#FF9400" ,
                    "#FF8600", "#FF7900", "#FF6B00", "#FF5E00", "#FF5100",
                    "#FF4300", "#FF3600", "#FF2800", "#FF1B00", "#FF0D00", 
                    "#FF0000")





ggplot()+
  geom_sf(data = Dis_MDD, fill=NA, color="gray", size=2)+
  #geom_sf(data = tbl_2019_SF, aes(fill= Años),color="black", size=0.2, show.legend = F, alpha=0.6)+
  new_scale_fill()+
  geom_raster(data = Dens_raster_focos, aes(x=X, y=Y, fill=den), alpha=0.6)+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(0.5, 1, 1.5, 2),
                       labels = c("Baja","Media", "Alta", "Muy Alta"),
                       na.value = 'white',
                       name= "Concentración de \ndeforestación")+
  guides(fill = guide_legend(nrow = 5, ncol=1))+
  theme(legend.position = c(0.1,0.8),
        legend.text=element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif", face='bold',hjust=0.5))+
  annotation_custom(Logo_png2)
  

library(grid)
library(png)
library(ggimage)


Logo2 <- readPNG("PNG/Foto_cir.png", FALSE)
Logo_png2 <- rasterGrob(Logo2, x = unit(0.88, "npc"),y = unit(0.8, "npc"), width = unit(0.2, "npc"))

ANP_Peru <- st_read(con, layer = "ANP_Peru")
ANP_Peru <- st_transform(ANP_Peru,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Rio_poli = st_read(con,"Rio_poli")  %>% st_as_sf()
Rio_poli <- st_transform(Rio_poli  , crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Via_Mal <- st_read (con,"Red_vial_MDD") # Caragmos un shp de puerto maldonado
Via_Maldonado  <- st_transform(Via_Mal ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))# transformamos la proyeccion

Derechos_mineros_2021_MDD = st_read(con,"Derechos_mineros_2021_MDD")  %>% st_as_sf()
Derechos_mineros_2021_MDD<- st_transform(Derechos_mineros_2021_MDD  , crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

CP_MINERIA = st_read("SHP/CP_MINERIA.shp")  %>% st_as_sf()
CP_MINERIA<- st_transform(CP_MINERIA  , crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
CP_MINERIA_xy <- cbind(CP_MINERIA, st_coordinates(st_centroid(CP_MINERIA$geometry)))
CCNN = st_read(con,"CCNN")  %>% st_as_sf()
CCNNA  <- st_transform(CCNN  , crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
  
library(ggrepel)
MDD_GG= ggplot()+
  geom_sf(data = SurAmerica, fill="#ffdab9", color="black", size=2)+
  
  geom_sf(data = Provi_Peru, fill="white", color="gray", size=2)+
  geom_sf(data = Dis_MDD, fill="#adc178", color="gray", size=2)+
  
  geom_sf(data = Rio_poli, fill="#a2d2ff", color="#a2d2ff")+
  
  geom_sf(data = ANP_Peru, fill="#84a98c", alpha=0.4,  color= "#84a98c")+
  geom_sf(data = CCNNA, fill="#c2c5aa", alpha=0.6, color= "#c2c5aa")+
  geom_sf_text(data =CCNNA, aes(label =comunidad), size=1, color="#495057")+
  geom_sf(data = Derechos_mineros_2021_MDD, fill="#f4f1de", alpha=0.4,  color= "#ddb892")+
  geom_sf(data = Dis_MDD, fill=NA, alpha=0.4,  color= "#2e1867", alpha=0.4)+
  geom_sf(data = Via_Maldonado, color="#1d3557", size=0.01, alpha=0.6)+

  #geom_sf(data = tbl_2019_SF, aes(fill= Años),color="black", size=0.2, show.legend = F, alpha=0.6)+
  new_scale_fill()+
  geom_raster(data = Dens_raster_focos, aes(x=X, y=Y, fill=den), alpha=0.6)+
  scale_fill_gradientn(colours = colores, 
                       breaks = c(0.5, 1, 1.5, 2),
                       labels = c("Baja","Media", "Alta", "Muy Alta"),
                       na.value = 'white',
                       name= "Concentración de \ndeforestación")+
  guides(fill = guide_legend(nrow = 5, ncol=1))+
  
  coord_sf(xlim = c(-73, -68.3), ylim = c(-14 ,-9),expand = FALSE)+
  theme_bw()+
  theme(axis.text.x  = element_text(face="bold", color="black", size=8,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=8),
        legend.position = c(0.1,0.8),
        legend.text=element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif", face='bold',hjust=0.5))+
  scale_x_continuous(breaks = c(-72, -71, -70, -69))+
  scale_y_continuous(breaks = c(-13, -12, -11, -10))+
  
  ggplot2::annotate(geom = "text", x = -69.5, y = -13.4, hjust = 0, vjust = 1, 
           label = "Parque Nacional \n   Bahuaja-Sonene",size = 3, family="serif", color = 
             "#40704a",  fontface="italic", face = "bold")+
             ggplot2::annotate(geom = "text", x = -69.35, y = -12.9, hjust = 0, vjust = 1, 
                      label = "Reserva Nacional \n   Tambopata",size = 2, family="serif", color = 
                        "#40704a",  fontface="italic", face = "bold")+
                        ggplot2::annotate(geom = "text", x = -70.8, y = -10.9, hjust = 0, vjust = 1, 
                                 label = "Reserva Nacional \n   alto Purus",size = 2, family="serif", color = 
                                   "#40704a",  fontface="italic", face = "bold")+
                                   geom_label_repel(data = CP_MINERIA_xy , aes(x = X.1, y = Y.1, label = NOMBRE), 
                                                    family="serif", box.padding = unit(2, "lines"), size =2, face = "bold",color = 'black',
                                                    point.padding = unit(0.5, "lines"))+
  geom_point(data= CP_MINERIA_xy , aes(X.1,Y.1), show.legend = FALSE)+

  ggplot2::annotate(geom = "text", x = -69, y = -11.5, hjust = 0, vjust = 1, label = "BOLIVIA",size = 5, angle=300,
                    family="serif", color = "black", face = "bold")+
  ggplot2::annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, label = "BRASIL",size = 5, 
                    family="serif", color = "black", face = "bold")+
  ggplot2::annotate(geom = "text", x = -72.3, y = -10.5, hjust = 0, vjust = 1, label = "UCAYALI",size = 5, 
                    family="serif", color = "black", face = "bold")+
  ggplot2::annotate(geom = "text", x = -72.5, y = -12.4, hjust = 0, vjust = 1, label = "CUSCO",size = 5, 
                    family="serif", color = "black", face = "bold")+
  ggplot2::annotate(geom = "text", x = -70 , y = -13.5, hjust = 0, vjust = 1, label = "PUNO",size = 5, 
                    family="serif", color = "black", face = "bold")+
  ggplot2::annotate(geom = "text", x = -72.2 , y = -9.3, hjust = 0, vjust = 1, label = "Deforestación en Madre de Dios en 2019",size = 9, 
                    family="serif", color = "black", face = "bold")+
  ggplot2::annotate("label", x = -71.5 , y = -11.2, hjust = 0, vjust = 1, label = "MADRE DE DIOS",size = 5,
                    family="serif", color = "black", face = "bold")+
  ggplot2::annotate(geom = "text", x = -72.5 , y = -13.5, hjust = 0, vjust = 1, label = "Hectáreas: 30521 ",size = 9,
                    family="serif", color = "red", face = "bold")+
  
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs( x = 'Longitud', y = 'Latitud')+
  annotation_custom(Logo_png2)



library(cowplot)
Mapa= ggdraw() +
  coord_equal(xlim = c(0, 25), ylim = c(0, 25), expand = FALSE) +
  draw_plot(MDD_GG , width = 25, height = 25,x = 0, y = 0)+
  draw_plot(SurA , width = 6, height = 6,x = 19, y = 1)+
  
  geom_segment(aes(x=15, xend=22, y=6.3, yend=18), 
               linetype = "dashed", color = "black" , size = 0.4)+

  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "black", fill = NA, size = 1))

ggsave(plot=Mapa,"Mapa/Mapa deforestacion_MDD.png",units = "cm",width = 25, #alto
       height = 25, #ancho
       dpi=1200)













