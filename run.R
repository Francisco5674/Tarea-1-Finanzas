
setwd("~/Dropbox/UCatolica/courses/Finanzas_I/3. Tareas/2021/Tarea1/Tarea1_R")

load(file="precios.RData")
load(file="Nacciones.RData")
load(file="sectores.RData")



str(precios)

colnames(precios)
colnames(Nacciones)

View(Nacciones)
View(precios)


#sacar el precio máximo del ipsa?

max(precios[,"ipsa"])

a=max(precios[,"ipsa"], na.rm=TRUE)

str(a)

a=max(as.numeric(precios[,"ipsa"]), na.rm=TRUE)

str(a)

#qué posicion tiene ipsa

q=which(colnames(precios)=="bci")
q=which(colnames(precios)=="ipsa")

max(precios[,q],na.rm=TRUE)
max(as.numeric(precios[,q]),na.rm=TRUE)


#desviaciónn estándar de los precios

sd(as.numeric(precios[,"ipsa"]),na.rm=TRUE)


#quiero realizar una operación pàra todas las columnas:
length(colnames(precios))


nombres=colnames(precios)[2:32]


maxi=c()
i=1
for (n in nombres){
  maxi[i]=max(as.numeric(precios[,n]),na.rm=TRUE)
  i=i+1
  
}


#diferencia de precios diarios

n="ipsa"
d=diff(as.numeric(precios[,n]))
d[1:3]

diffP=c()
for (n in nombres){
  d=diff(as.numeric(precios[,n]))
  diffP=cbind(diffP,d)
}


colnames(diffP)=nombres

colnames(diffP)


p1=diffP[,"aguas-a"]
p2=diffP[,"aesgener"]

plot(p1, type="l")
lines(p2, col="red")


#szmulewiczf@gmail.com   Daniel


