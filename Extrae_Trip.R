
library(rvest)

## CODIGO PARA DESCARGA DE COMENTARIOS EN INGLÉS

## Atractivos en Ingles

lista_eng<-read.csv("C:/Temp/listeng.csv",sep=",", header = T,stringsAsFactors = F)


## Obtener cantidad total de páginas (de comentarios) por cada sitio

for (i in 1:419) {
  web <-lista_eng[i,5]
  reviews2 <- web %>% read_html() %>% html_nodes("#REVIEWS .listContainer")
  num <- reviews2 %>% html_nodes(".pagination-details") %>% html_text()
  write.table(data.frame(i,num),file="C:/Temp/lista_eng.txt",row.names=F,col.names=F,append=T,sep=",")
}



## Funcion colec_hotel para Descargar Comentarios de Hoteles
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
  reviews <- url %>% read_html() %>% html_nodes("#REVIEWS .listContainer")
  lugar <- reviews %>% html_nodes(".info_text") %>% html_text()
  tex1 <- reviews %>% html_nodes(".info_text div") %>% html_text()
  tex2 <- reviews %>% html_nodes(".userLoc") %>% html_text()
  usuario <- setdiff(tex1,tex2)
  tmpx1<-sapply(usuario,nchar)
  lugar<-substr(lugar,tmpx1+1,tmpx1+30)
  id <- reviews %>% html_nodes(".quote a") %>% html_attr("id")
  quote <- reviews %>% html_nodes(".quote span") %>% html_text()
  rating <- reviews %>% html_nodes(".ui_bubble_rating") %>% html_attrs() %>% gsub("ui_bubble_rating bubble_", "", .) %>% as.integer()
  date <- reviews %>% html_nodes(".ratingDate") %>% html_attr("title")
  review <- reviews %>% html_nodes(".entry .partial_entry") %>% html_text()
  rev2 <- reviews %>% html_nodes(".mgrRspnInline .entry .partial_entry") %>% html_text()
  review  <- setdiff(review,rev2)
  quote <- gsub("[\n\r\t\"]", " ", quote)
  quote <- iconv(quote,to="UTF-8")
  review <- gsub("[\n\r\t\"]", " ", review)
  review <- iconv(review,to="UTF-8")
  lugar <- gsub("[\n\r\t\"]", "", lugar)
  lugar <- iconv(lugar,to="UTF-8")
  usuario <- gsub("[\n\r\t\"]", "", usuario)
  usuario <- iconv(usuario,to="UTF-8")
  try(temp.tableout <- data.frame("Region"=region,"Atraccion"=atrac,id, lugar, usuario, quote, rating, date, review))
  write.table(temp.tableout,file="C:/Temp/comm_eng.txt",row.names=F,col.names=F,append=T,sep=",")}}

for (i in 1:161) {temp.atrac<-colec_hotel(lista_eng[i,8],lista_eng[i,3],lista_eng[i,4],lista_eng[i,6],lista_eng[i,7])}



## Funcion colec_rest para Descargar Comentarios de Restaurantes y Atractivos
## La funcion tiene cinco argumentos:
## n es el numero de paginas de comentarios
## region es la Región del país donde se ubica el atractivo 
## atrac es el atractivo del cual se está descargando la información
## web1 es la primera parte de la dirección 
## web2 es la segunda parte de la dirección 
## como resultado r debe generar un dataframe con todas las columnas necesarias 
## CODIGO PARA DESCARGA DE COMENTARIOS EN INGLÉS

colect_rest<-function(n,region,atrac,web1,web2) {
  looping<-c("")
  for(i in 1:n) {tmp<-paste("or",i,"0-",sep="")
  looping<-c(looping,tmp)}
  tableout <- data.frame()
  for(i in looping){url <- paste (web1,i,web2,sep="")
  reviews2 <- url %>% read_html() %>% html_nodes("#REVIEWS .listContainer")
  usuario <- reviews2 %>% html_nodes(".username") %>% html_text()
  lugar <- reviews2 %>% html_nodes(".member_info") %>% html_text()
  tmpx1<-sapply(usuario,nchar)
  lugar<-substr(lugar,tmpx1+1,tmpx1+30)
  lugar = gsub("[[:digit:]]", "", lugar)
  reviews <- url %>% read_html() %>% html_nodes("#REVIEWS .innerBubble")
  id <- reviews %>% html_node(".quote a") %>% html_attr("id")
  quote <- reviews %>% html_node(".quote span") %>% html_text()
  rating <- reviews %>% html_node(".rating .ui_bubble_rating") %>% html_attrs() %>% gsub("ui_bubble_rating bubble_", "", .) %>% as.integer()
  date <- reviews %>% html_node(".rating .ratingDate") %>% html_attr("title")
  review <- reviews %>% html_node(".entry .partial_entry") %>% html_text()
  quote <- gsub("[\n\r\t\"]", " ", quote)
  quote <- iconv(quote,to="UTF-8")
  review <- gsub("[\n\r\t\"]", " ", review)
  review <- iconv(review,to="UTF-8")
  lugar <- gsub("[\n\r\t\"]", "", lugar)
  lugar <- iconv(lugar,to="UTF-8")
  usuario <- gsub("[\n\r\t\"]", "", usuario)
  usuario <- iconv(usuario,to="UTF-8")
  temp.tableout <- data.frame("Region"=region,"Atraccion"=atrac,id, lugar, usuario, quote, rating, date, review)
  write.table(temp.tableout,file="C:/Temp/comm_eng.txt",row.names=F,col.names=F,append=T,sep=",")}}

for (i in 162:419) {temp.atrac<-colect_rest(lista_eng[i,8],lista_eng[i,3],lista_eng[i,4],lista_eng[i,6],lista_eng[i,7])}






## CODIGO PARA DESCARGA DE COMENTARIOS EN ESPAÑOL

## Atractivos en Espanol

lista_esp<-read.csv("C:/Temp/listesp.csv",sep=",", header = T,stringsAsFactors = F)


## Obtener cantidad total de páginas (de comentarios) por cada sitio

for (i in 1:465) {
  web <-lista_esp[i,5]
  reviews2 <- web %>% read_html() %>% html_nodes("#REVIEWS .listContainer")
  num <- reviews2 %>% html_nodes(".pagination-details") %>% html_text()
  write.table(data.frame(i,num),file="C:/Temp/lista_esp.txt",row.names=F,col.names=F,append=T,sep=",")
}


## Funcion colec_hotel para Descargar Comentarios de Hoteles

colec_hotel<-function(n,region,atrac,web1,web2) {
  looping<-c("")
  for(i in 1:n) {tmp<-paste("or",i*5,"-",sep="")
  looping<-c(looping,tmp)}
  tableout <- data.frame()
  for(i in looping){url <- paste (web1,i,web2,sep="")
  reviews <- url %>% read_html() %>% html_nodes("#REVIEWS .listContainer")
  lugar <- reviews %>% html_nodes(".info_text") %>% html_text()
  tex1 <- reviews %>% html_nodes(".info_text div") %>% html_text()
  tex2 <- reviews %>% html_nodes(".userLoc") %>% html_text()
  usuario <- setdiff(tex1,tex2)
  tmpx1<-sapply(usuario,nchar)
  lugar<-substr(lugar,tmpx1+1,tmpx1+30)
  id <- reviews %>% html_nodes(".quote a") %>% html_attr("id")
  quote <- reviews %>% html_nodes(".quote span") %>% html_text()
  rating <- reviews %>% html_nodes(".ui_bubble_rating") %>% html_attrs() %>% gsub("ui_bubble_rating bubble_", "", .) %>% as.integer()
  date <- reviews %>% html_nodes(".ratingDate") %>% html_attr("title")
  review <- reviews %>% html_nodes(".entry .partial_entry") %>% html_text()
  rev2 <- reviews %>% html_nodes(".mgrRspnInline .entry .partial_entry") %>% html_text()
  review  <- setdiff(review,rev2)
  quote <- gsub("[\n\r\t\"]", " ", quote)
  quote <- iconv(quote,to="UTF-8")
  review <- gsub("[\n\r\t\"]", " ", review)
  review <- iconv(review,to="UTF-8")
  lugar <- gsub("[\n\r\t\"]", "", lugar)
  lugar <- iconv(lugar,to="UTF-8")
  usuario <- gsub("[\n\r\t\"]", "", usuario)
  usuario <- iconv(usuario,to="UTF-8")
  try(temp.tableout <- data.frame("Region"=region,"Atraccion"=atrac,id, lugar, usuario, quote, rating, date, review))
  write.table(temp.tableout,file="C:/Temp/comm_esp.txt",row.names=F,col.names=F,append=T,sep=",")}}


for (i in 1:161) {temp.atrac<-colec_hotel(lista_esp[i,8],lista_esp[i,3],lista_esp[i,4],lista_esp[i,6],lista_esp[i,7])}


## Funcion colec_rest para Descargar Comentarios de Restaurantes y Atractivos

colect_rest<-function(n,region,atrac,web1,web2) {
  looping<-c("")
  for(i in 1:n) {tmp<-paste("or",i,"0-",sep="")
  looping<-c(looping,tmp)}
  tableout <- data.frame()
  for(i in looping){url <- paste (web1,i,web2,sep="")
  reviews2 <- url %>% read_html() %>% html_nodes("#REVIEWS .listContainer")
  usuario <- reviews2 %>% html_nodes(".username") %>% html_text()
  lugar <- reviews2 %>% html_nodes(".member_info") %>% html_text()
  tmpx1<-sapply(usuario,nchar)
  lugar<-substr(lugar,tmpx1+1,tmpx1+30)
  lugar = gsub("[[:digit:]]", "", lugar)
  reviews <- url %>% read_html() %>% html_nodes("#REVIEWS .innerBubble")
  id <- reviews %>% html_node(".quote a") %>% html_attr("id")
  quote <- reviews %>% html_node(".quote span") %>% html_text()
  rating <- reviews %>% html_node(".rating .ui_bubble_rating") %>% html_attrs() %>% gsub("ui_bubble_rating bubble_", "", .) %>% as.integer()
  date <- reviews %>% html_node(".rating .ratingDate") %>% html_attr("title")
  review <- reviews %>% html_node(".entry .partial_entry") %>% html_text()
  quote <- gsub("[\n\r\t\"]", " ", quote)
  quote <- iconv(quote,to="UTF-8")
  review <- gsub("[\n\r\t\"]", " ", review)
  review <- iconv(review,to="UTF-8")
  lugar <- gsub("[\n\r\t\"]", "", lugar)
  lugar <- iconv(lugar,to="UTF-8")
  usuario <- gsub("[\n\r\t\"]", "", usuario)
  usuario <- iconv(usuario,to="UTF-8")
  temp.tableout <- data.frame("Region"=region,"Atraccion"=atrac,id, lugar, usuario, quote, rating, date, review)
  write.table(temp.tableout,file="C:/Temp/comm_esp.txt",row.names=F,col.names=F,append=T,sep=",")}}


for (i in 162:465) {temp.atrac<-colect_rest(lista_esp[i,8],lista_esp[i,3],lista_esp[i,4],lista_esp[i,6],lista_esp[i,7])}



