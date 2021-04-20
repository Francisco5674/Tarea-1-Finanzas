
setwd("~/Dropbox/UCatolica/courses/Finanzas_I/3. Tareas/2021/Tarea1/Tarea1_R")

load(file="precios.RData")
load(file="Nacciones.RData")
load(file="sectores.RData")



str(precios)
install.packages('zoo')
library(zoo)



precios2=as.data.frame(precios)  # esto lo hago porque la función aggregate necesita coomo input un objeto data.frame


precios2$ipsa=NULL


names(precios2)  #todas menos el ipsa

precios2$fecha=as.yearmon(precios2$fecha)
str(precios2)

#aqui le decimos a R que agregue la columna 2 de precios, utilizando las fechas  yearmon
#miren las fechas antes:
print(precios2$fecha)

#luego lo que hace es a cada grupo de fechas iguales, escoge el tail, 1 que significa al ultima observación

#eso sería el último precio de cada mes:

aa=aggregate(precios2[,2], by=list(precios2$fecha), tail,1)
names(aa)=c("fecha","precios")
print(aa)


#por ejemplo
print(aa[1:3,])   #los tres primeros precios mensuales que nos entrega

print(precios2[1:24,1:2]) #Enero 20217 coincide 

#tail,2 las 2 últimas observaciones para reemplazar los NA
bb=aggregate(precios2[,2], by=list(precios2$fecha), tail,2)
names(bb)=c("fecha","precios")
print(bb)


#por ejemplo Marzo 2018, deberíamos ocupar la observación anterior


#crear una funcion para que me de la penúltima observación:
myfun=function(a){
  return(head(tail(a, n=2), n=1))  #aqui me toma la primera observacion de tail, 2.  Esa sería la penúltima
}

cc=aggregate(precios2[,2], by=list(precios2$fecha), myfun)  #aquí le entrego myfun
names(cc)=c("fecha","precios")
print(cc)

#en resumen, estas funciones entregan aa y cc con la ultima y penultima observación del mes, para que reemplacen los correspondientes NA


