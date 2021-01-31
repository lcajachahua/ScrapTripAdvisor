# autor: Luis Cajachahua
# fecha de actualización: Diciembre, 2020

# Antes de nada, limpiamos el workspace, por si hubiera algún dataset o información cargada
rm(list = ls())

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Limpiamos la consola
cat("\014")

# Cargamos las librerías que vamos a necesitar
# install.packages("rvest")
library(rvest)

## CODIGO PARA DESCARGA DE COMENTARIOS EN INGLÉS

## Atractivos en Ingles

lista_eng<-read.csv("listeng.csv",sep=",", header = T,stringsAsFactors = F)


## Funcion colec_hotel sirve para Descargar Comentarios de Hoteles
## La funcion tiene cinco argumentos:
## n es el numero de paginas de comentarios
## region es la Región del país donde se ubica el atractivo 
## atrac es el atractivo del cual se está descargando la información
## web1 es la primera parte de la dirección 
## web2 es la segunda parte de la dirección 
## como resultado r debe generar un dataframe con todas las columnas necesarias 

colec_hotel<-function(n,region,atrac,web1,web2) {
  looping<-c("")
  for(i in 1:n) {tmp<-paste("or",i*5,"-",sep="")
  looping<-c(looping,tmp)}
  tableout <- data.frame()
  for(i in looping){url <- paste (web1,i,web2,sep="")
  reviews <- url %>% read_html()
  id <- reviews %>% html_nodes(".oETBfkHU") %>% html_attr("data-reviewid")
  usuario <- reviews %>% html_nodes("._1r_My98y") %>% html_text()
  quote <- reviews %>% html_nodes(".glasR4aX") %>% html_text()
  rating <- reviews %>% html_nodes(".nf9vGX55 .ui_bubble_rating") %>% html_attrs() %>% gsub("ui_bubble_rating bubble_", "", .) %>% as.integer()
  review <- reviews %>% html_nodes(".IRsGHoPm") %>% html_text()
  quote <- gsub("[\n\r\t\"]", " ", quote)
  quote <- iconv(quote,to="UTF-8")
  review <- gsub("[\n\r\t\"]", " ", review)
  review <- iconv(review,to="UTF-8")
  usuario <- gsub("[\n\r\t\"]", "", usuario)
  usuario <- iconv(usuario,to="UTF-8")
  try(temp.tableout <- data.frame("Region"=region,"Atraccion"=atrac,id, usuario, quote, rating, review))
  write.table(temp.tableout,file="hotel_eng.txt",row.names=F,col.names=F,append=T,sep=",")}}

for (i in 1:4) {temp.atrac<-colec_hotel(lista_eng[i,8],lista_eng[i,3],lista_eng[i,4],lista_eng[i,6],lista_eng[i,7])}



## Funcion colec_rest para Descargar Comentarios de Restaurantes y Atractivos
## La funcion tiene cinco argumentos:
## n es el numero de paginas de comentarios
## region es la Región del país donde se ubica el atractivo 
## atrac es el atractivo del cual se está descargando la información
## web1 es la primera parte de la dirección 
## web2 es la segunda parte de la dirección 
## como resultado r debe generar un dataframe con todas las columnas necesarias 

colect_rest<-function(n,region,atrac,web1,web2) {
  looping<-c("")
  for(i in 1:n) {tmp<-paste("or",i,"0-",sep="")
  looping<-c(looping,tmp)}
  tableout <- data.frame()
  for(i in looping){url <- paste (web1,i,web2,sep="")
  reviews <- url %>% read_html()
  id <- reviews %>% html_nodes(".oETBfkHU") %>% html_attr("data-reviewid")
  usuario <- reviews %>% html_nodes("._1r_My98y") %>% html_text()
  quote <- reviews %>% html_nodes(".glasR4aX") %>% html_text()
  rating <- reviews %>% html_nodes(".nf9vGX55 .ui_bubble_rating") %>% html_attrs() %>% gsub("ui_bubble_rating bubble_", "", .) %>% as.integer()
  review <- reviews %>% html_nodes(".IRsGHoPm") %>% html_text()
  quote <- gsub("[\n\r\t\"]", " ", quote)
  quote <- iconv(quote,to="UTF-8")
  review <- gsub("[\n\r\t\"]", " ", review)
  review <- iconv(review,to="UTF-8")
  usuario <- gsub("[\n\r\t\"]", "", usuario)
  usuario <- iconv(usuario,to="UTF-8")
  try(temp.tableout <- data.frame("Region"=region,"Atraccion"=atrac,id, usuario, quote, rating, review))
  write.table(temp.tableout,file="rest_eng.txt",row.names=F,col.names=F,append=T,sep=",")}}

for (i in 5:12) {temp.atrac<-colect_rest(lista_eng[i,8],lista_eng[i,3],lista_eng[i,4],lista_eng[i,6],lista_eng[i,7])}

