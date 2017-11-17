library(tidyr)
library(dplyr)
library(reshape2)
library (ggplot2)
#Con este script realizo el resumen de las areas de cobertura de ecuador, las recalculo en base a su orden, y las delimito
  
  temp = list.files(pattern="*.csv")
  myfiles = lapply(temp, read.delim)
  temp = list.files(pattern="*.csv")
  for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))
  
  temp = list.files(pattern="*.csv")
  list2env(
    lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
           read.csv), envir = .GlobalEnv)
  colnames (disolve_coberturaQ1)[25] <- "area"
  cobertura_ecuador_disolvedef<-rbind(disolve_coberturaBQ1, disolve_coberturaBQ2, disolve_coberturaCumbi1_provisional, disolve_coberturaCumbi2_provisional, disolve_coberturaCumbi3_provisional, disolve_coberturacumbi4_clip, disolve_coberturaCumbi5, disolve_coberturaCumbi6, disolve_coberturaCUSU1, disolve_coberturaCUSU2, disolve_coberturaQ1, disolve_coberturaQ2, disolve_coberturaQ3, disolve_coberturaQ4, disolve_coberturaQ5)
  cobertura_ecuador_disolvedef
  
  cobertura_ecuador_disolvedef<- cobertura_ecuador_disolvedef[,-(4:25)]
  cobertura_ecuador_disolvedef

  #Que me ordene las  cada localidad
  
  landuse_selection_ecuador<-cobertura_ecuador_disolvedef%>%
    arrange (Location, desc(area_recal) ) 
  landuse_selection_ecuador
  
  # recategorizar clases de variables en la columna cobertura
  # Aquí sé el número de categorias que tengo dentro de la columna cobertura
   table(landuse_selection_ecuador$cobertura)
   
   #creacion de vector nulo
   Area_optima<- NULL
   # Creo un loop para que me vaya metiendo las variables. Para el loop 1:nrow(dataframe) lo que me dice es que el loop pase por todas las filas, si quisiera columnas sería length y entonces ahí es
    # donde aplicaría la función.
   for (n in 1:nrow(landuse_selection_ecuador)) {
       if (landuse_selection_ecuador$cobertura[n]== "Cuerpo Agua"| landuse_selection_ecuador$cobertura[n]== "Erial" | landuse_selection_ecuador$cobertura[n]== "Cultivo"|
           landuse_selection_ecuador$cobertura[n]== "Area Poblada"|landuse_selection_ecuador$cobertura[n]== "Infraestructura Antropica"){ 
           Area_optima[n]<-"Antropizado" } else {Area_optima[n]<- "Natural"}
     
   }
   
   # sumo el vector nuevo creado a través del bucle a mi dataframe
   landuse_selection_ecuador_recategories<-cbind (landuse_selection_ecuador, Area_optima)
   
   # intento de realizarlo a través de dplyr pero hay algo en el argumento mutate que no funciona
   #newdataframe_cobertura<-landuse_selection_ecuador%>%
   #mutate(antropization= ifelse(grepl("Cuerpo Agua"||"Erial"||"Cultivo"||"Area Poblada"||"Infraestructura Antropica", cobertura), "Antropizado","natural"))
   #newdataframe_cobertura

# Distribución de frecuencias de las diferentes localidades   
Antropization_degree_location<-landuse_selection_ecuador_recategories%>%
     group_by(Location, Area_optima)%>%
     summarise(area_antropizado= sum (area_recal))%>%
     group_by(Location)%>%
     mutate (freq =area_antropizado /sum (area_antropizado))%>%
     #group_by(Area_optima)%>%
     #arrange(desc(freq))
  Antropization_degree_location
  # Subset para que me lo organice en un gráfico de barras
  Natural_area_arrange<- subset (Antropization_degree_location[,], Area_optima == "Natural")
  Natural_area_arrange$Altitude<- c(2818,3041,2759,2943,3064,3152,3189,3628,3086,3500,3777,3780,3747,3838,3779)
  Natural_area_arrange<- Natural_area_arrange[-c(3,4,5),]
  Natural_area_arrange<-arrange(Natural_area_arrange, desc(freq))
  Natural_area_arrange<-as.data.frame(Natural_area_arrange)
  #con una columna de alturas
 
# representación de la distribución de frecuencias
  
  #(no)con esta funcion mutate ordeno los paneles antes de que me salgan basados en las frecuencias 
 
 ##Antropization_degree_location<- mutate(Antropization_degree_location,
                       #Location = reorder(Location, freq))
  
 # Antropization_degree_location$Location <- factor(Antropization_degree_location$Location, levels = Antropization_degree_location$Location[order(-Antropization_degree_location$freq)])
  

# plot de Natural area ordenado
  p <- ggplot(Natural_area_arrange, aes(x=Altitude, y=freq, fill=Location, size=2)) +
    geom_bar(stat="identity", size=2) +  ggtitle("Grado de paisaje natural")
  print(p) 
  
h<-qplot(Location,freq, data=Natural_area_arrange, geom="bar")
print(h) 
  ## Plot de natural area y antropization
  p <- ggplot(Antropization_degree_location, aes(x=1, y=freq, fill=Area_optima)) +
    geom_bar(stat="identity") + facet_wrap (~ Location, ncol=3)
    ggtitle("Grado de antropizacion")
  print(p) 
  p <- p + coord_polar(theta='y')
  print(p)
  
   
   
###################
   #library(dplyr)
   #antropization <- NULL
   #nofunciona
     #newdataframe_cobertura<-landuse_selection_ecuador$cobertura %>%
       #mutate(antropization= ifelse(grepl("Cuerpo Agua"||"Erial"||"Cultivo"||"Area Poblada"||"Infraestructura Antropica", cobertura), "Antropizado","natural"))
     #newdataframe_cobertura
  library(maps)  
  library(ggmap)
  library(mapdata)
  library (ggplot2)
  library(devtools)
  library(dplyr)
  library(stringr)
  install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
  devtools::install_github("dkahle/ggmap")
  


  