# Tratamiento de datos
# Estableciendo el modelo de regresion

# Primero nos encargamos de aislar los nombres importantes
nombres = colnames(precios)
nombres = nombres[3:32]

#Calculamos los retornos
rendimientos = retorno_de_instrumentos(precios)

# Solo nos quedamos desde el tercer elemento en adelante porque el primero es 'fecha'
# y el segundo es 'IPSA' que corresponde a la variables depdendiente

# Aislamos los valores de IPSA
IPSA = as.numeric(rendimientos[,2])

# Ahora calculamos los residuos para cada accion, de esta forma ajustamos 
# los precios y vemos cuales son los coeficientes beta y alpha 
residuosdataframe = c()

for (name in nombres){
  
  print(paste('######## ',name,' ########'))
  
  pos = which(colnames(rendimientos) == name)
  
  y = as.numeric(rendimientos[,pos])
  x = IPSA
  reg = lm(y~IPSA)
  
  print(summary(reg))
  
  predicciones = predict(reg, newdata = data.frame(x))
  residuos = unname(y - predicciones)
  
  
  residuosdataframe = cbind(residuosdataframe, residuos)
}

#Agregamos la fecha y cambiamos los nombres

Dates <- precios[-1,1]

print(length(Dates))

residuosdataframe = cbind(Dates, residuosdataframe)
colnames(residuosdataframe)[1] = 'fecha'
colnames(residuosdataframe)[2:31] = nombres

# Veamos que pasa con cencosud
residuosdataframe[residuosdataframe[,1] == '2021-03-19'][9]
residuosdataframe[residuosdataframe[,1] == '2021-03-20'][9]
residuosdataframe[residuosdataframe[,1] == '2021-03-21'][9]
residuosdataframe[residuosdataframe[,1] == '2021-03-22'][9]
residuosdataframe[residuosdataframe[,1] == '2021-03-23'][9]
residuosdataframe[residuosdataframe[,1] == '2021-03-24'][9]
residuosdataframe[residuosdataframe[,1] == '2021-03-25'][9]
residuosdataframe[residuosdataframe[,1] == '2021-03-26'][9]


####################################################################################
# Ahora veremos las 5 acciones con menor retorno y las 5 acciones con mayor 
# retorno anormal

# primero veamos todos los retornos anormales para el 25 de marzo del 2021

retornos = as.numeric(residuosdataframe[residuosdataframe[,1] == '2021-03-25'][2:31])
names(retornos) = nombres
print(sort(retornos))

# ahora veamos todos los retornos anormales para el 10 de junio del 2020

retornos = as.numeric(residuosdataframe[residuosdataframe[,1] == '2020-06-10'][2:31])
names(retornos) = nombres
print(sort(retornos))

# por ultimo veamos todos los retornos anormales para el 18 de marzo del 2020

retornos = as.numeric(residuosdataframe[residuosdataframe[,1] == '2020-03-18'][2:31])
names(retornos) = nombres
print(sort(retornos))

#grafico retornos anormales en el tiempo

# sujeto de prueba ripley

ripley_r = residuosdataframe[,colnames(residuosdataframe) == 'ripley']

plot(ripley_r, col='red')

####################################################################################
#  RETORNO SIN AJUSTAR

# primero veamos todos los retornos para el 25 de marzo del 2021

retornos = as.numeric(rendimientos[rendimientos[,1] == '2021-03-25'][2:31])
names(retornos) = nombres
print(sort(retornos))

# ahora veamos todos los retornos para el 10 de junio del 2020

retornos = as.numeric(rendimientos[rendimientos[,1] == '2020-06-10'][2:31])
names(retornos) = nombres
print(sort(retornos))

# por ultimo veamos todos los retornos para el 18 de marzo del 2020

retornos = as.numeric(rendimientos[rendimientos[,1] == '2020-03-18'][2:31])
names(retornos) = nombres
print(sort(retornos))

ripley_r = rendimientos[,colnames(rendimientos) == 'ripley']
print(length(ripley_r))

lines(ripley_r)


#################################################################################

# Ahora veremos que acciones pertenecen al sector financiero, de comercio y 
# energia electrica

# primero armemos un portfolio para cada sector

Comercio <- portfolio('Comercio')

Finanzas <- portfolio('Finanzas y Seguros')

Energia <- portfolio('Energía Eléctrica')

# Ahora los unmimos en una tabla

portafolios = cbind(Comercio, Finanzas[,2], Energia[,2])
colnames(portafolios) = c('fecha','Comercio', 'Finanzas y Seguros', 'Energía Eléctrica')

# calculamos los retornos

retorno_de_portafolio = portafolios

residuos_portafolio = c()

nombres <- colnames(retorno_de_portafolio)
nombres <- nombres[-1]

for (name in nombres){
  
  print(paste('######## ',name,' ########'))
  
  pos = which(colnames(retorno_de_portafolio) == name)
  
  y = as.numeric(retorno_de_portafolio[,pos])
  x = IPSA
  reg = lm(y~x)
  
  print(summary(reg))
  
  predicciones = predict(reg, newdata = data.frame(x))
  residuos = unname(y - predicciones)
  
  residuos_portafolio= cbind(residuos_portafolio, residuos)
}

Dates <- precios[-1,1]

residuos_portafolio = cbind(Dates, residuos_portafolio)
colnames(residuos_portafolio)[1] = 'fecha'
colnames(residuos_portafolio)[2:4] = nombres

####################################################################################
#   RETORNOS AJUSTADOS
# primero veamos todos los retornos anormales para el 25 de marzo del 2021

retornos_A_25 = as.numeric(residuos_portafolio[residuos_portafolio[,1] == '2021-03-25'][2:4])
names(retornos_A_25) = nombres


# ahora veamos todos los retornos anormales para el 10 de junio del 2020

retornos_A_10 = as.numeric(residuos_portafolio[residuos_portafolio[,1] == '2020-06-10'][2:4])
names(retornos_A_10) = nombres


# por ultimo veamos todos los retornos anormales para el 18 de marzo del 2020

retornos_A_18 = as.numeric(residuos_portafolio[residuos_portafolio[,1] == '2020-03-18'][2:4])
names(retornos_A_18) = nombres


retornos_A_25
retornos_A_10
retornos_A_18
(retornos_A_18 + retornos_A_25 + retornos_A_10)/3

####################################################################################
# RETORNOS NO AJUSTADOS
# primero veamos todos los retornos para el 25 de marzo del 2021

retornos_25 = as.numeric(retorno_de_portafolio[retorno_de_portafolio[,1] == '2021-03-25'][2:4])
names(retornos_25) = nombres

# ahora veamos todos los retornos para el 10 de junio del 2020

retornos_10 = as.numeric(retorno_de_portafolio[retorno_de_portafolio[,1] == '2020-06-10'][2:4])
names(retornos_10) = nombres

# por ultimo veamos todos los retornos para el 18 de marzo del 2020

retornos_18 = as.numeric(retorno_de_portafolio[retorno_de_portafolio[,1] == '2020-03-18'][2:4])
names(retornos_18) = nombres

(retornos_18 + retornos_25 + retornos_10)/3
####################################################################
#Retornos normeles dia 0 promedio

data_fechas = cbind(as.numeric(retorno_de_portafolio[retorno_de_portafolio[,1] == '2021-03-25'][-1]),
      as.numeric(retorno_de_portafolio[retorno_de_portafolio[,1] == '2020-06-10'][-1]),
      as.numeric(retorno_de_portafolio[retorno_de_portafolio[,1] == '2020-03-18'][-1]))
colnames(data_fechas) <- c('2021-03-25', '2020-06-10', '2020-03-18')
rownames(data_fechas) <- c('comercio', 'finanzas', 'energia')

productoria = data_fechas[,1] + data_fechas[,2] + data_fechas[,3]
resultado_0 = productoria*(1/3)
#retornos dia 0
resultado_0

####################################################################
#Retonos normales ventana
#retornos dia -1
data_fechas = cbind(as.numeric(retorno_de_portafolio[retorno_de_portafolio[,1] == '2021-03-24'][-1]),
                    as.numeric(retorno_de_portafolio[retorno_de_portafolio[,1] == '2020-06-09'][-1]),
                    as.numeric(retorno_de_portafolio[retorno_de_portafolio[,1] == '2020-03-17'][-1]))
colnames(data_fechas) <- c('2021-03-24', '2020-06-09', '2020-03-17')
rownames(data_fechas) <- c('comercio', 'finanzas', 'energia')

productoria = data_fechas[,1] + data_fechas[,2] + data_fechas[,3]
resultado_menos1 = productoria*(1/3)
resultado_menos1 

#retornos promedio del dia 1
data_fechas = cbind(as.numeric(retorno_de_portafolio[retorno_de_portafolio[,1] == '2021-03-26'][-1]),
                    as.numeric(retorno_de_portafolio[retorno_de_portafolio[,1] == '2020-06-11'][-1]),
                    as.numeric(retorno_de_portafolio[retorno_de_portafolio[,1] == '2020-03-19'][-1]))
colnames(data_fechas) <- c('2021-03-26', '2020-06-11', '2020-03-19')
rownames(data_fechas) <- c('comercio', 'finanzas', 'energia')

productoria = data_fechas[,1] + data_fechas[,2] + data_fechas[,3]
resultado_mas1 = productoria*(1/3)
resultado_mas1 

((resultado_0 + 1)* (resultado_mas1 + 1)* (resultado_menos1 + 1)) - 1

##################################################################
#retornos anormales del dia 0
data_fechas = cbind(as.numeric(residuos_portafolio[residuos_portafolio[,1] == '2021-03-25'][-1]),
                    as.numeric(residuos_portafolio[residuos_portafolio[,1] == '2020-06-10'][-1]),
                    as.numeric(residuos_portafolio[residuos_portafolio[,1] == '2020-03-18'][-1]))
colnames(data_fechas) <- c('2021-03-25', '2020-06-10', '2020-03-18')
rownames(data_fechas) <- c('comercio', 'finanzas', 'energia')

productoria = data_fechas[,1] + data_fechas[,2] + data_fechas[,3]
resultado_A_0 = productoria*(1/3)
#retornos dia 0
resultado_A_0

#################################################################
#Retonos anormales ventana
#retornos dia -1
data_fechas = cbind(as.numeric(residuos_portafolio[residuos_portafolio[,1] == '2021-03-24'][-1]),
                    as.numeric(residuos_portafolio[residuos_portafolio[,1] == '2020-06-09'][-1]),
                    as.numeric(residuos_portafolio[residuos_portafolio[,1] == '2020-03-17'][-1]))
colnames(data_fechas) <- c('2021-03-24', '2020-06-09', '2020-03-17')
rownames(data_fechas) <- c('comercio', 'finanzas', 'energia')

productoria = data_fechas[,1] + data_fechas[,2] + data_fechas[,3]
resultado_A_menos1 = productoria*(1/3)
resultado_A_menos1 

#retornos promedio del dia 1
data_fechas = cbind(as.numeric(residuos_portafolio[residuos_portafolio[,1] == '2021-03-26'][-1]),
                    as.numeric(residuos_portafolio[residuos_portafolio[,1] == '2020-06-11'][-1]),
                    as.numeric(residuos_portafolio[residuos_portafolio[,1] == '2020-03-19'][-1]))
colnames(data_fechas) <- c('2021-03-26', '2020-06-11', '2020-03-19')
rownames(data_fechas) <- c('comercio', 'finanzas', 'energia')

productoria = data_fechas[,1] + data_fechas[,2] + data_fechas[,3]
resultado_A_mas1 = productoria*(1/3)
resultado_A_mas1 

((resultado_A_0 + 1)* (resultado_A_mas1 + 1)* (resultado_A_menos1 + 1)) - 1




