# primero nos encargamos de sacar el grupo de interest

# filtramos las fechas importantes
data = precios[precios[,1] >= '2018-01-01',]
data = data[,colnames(data) %in% c('fecha','colbun', 'enelam', 'cmpc', 'sonda', 'ipsa')]

# ahora sacamos los rendimientos con la funcion creada con anterioridad

data_r = retorno_de_instrumentos(data)

#ahora vamos a graficar

# como lo vamos hacer para varias acciones, es mejor crear una funcion

graficar_ipsa <- function(data, accion){
  graf_1 <- as.numeric(data[,colnames(data) == accion])
  graf_2 <- as.numeric(data[,colnames(data) == 'ipsa'])
  
  # el rojo sera la accion de interes
  plot(graf_1, type = 'l', lwd = 2 ,  col = 'red', 
       main = paste('ipsa (azul, claro) vs', accion, '(rojo, oscuro)'),
       ylab = 'retornos', xlab = 'tiempo')
  # el negro es el ipsa
  lines(graf_2, col = 'skyblue', type = 'l', lwd = 2)
}

graficar_ipsa(data_r, 'sonda')

graficar_ipsa(data_r, 'enelam')

graficar_ipsa(data_r, 'colbun')

graficar_ipsa(data_r, 'cmpc')

# ahora veamos las correlaciones

corr <- function(y, x){
  res <- cor(na.omit(as.numeric(data_r[,colnames(data_r) == y])), 
      na.omit(as.numeric(data_r[,colnames(data_r) == x])))
  return(res)
}

corr_table = rbind(c(corr('ipsa', 'ipsa'), corr('ipsa', 'sonda'), 
                     corr('ipsa', 'enelam'), corr('ipsa', 'colbun'), 
                     corr('ipsa', 'cmpc')),
                   c(corr('sonda', 'ipsa'), corr('sonda', 'sonda'), 
                     corr('sonda', 'enelam'), corr('sonda', 'colbun'), 
                     corr('sonda', 'cmpc')),
                   c(corr('enelam', 'ipsa'), corr('enelam', 'sonda'), 
                     corr('enelam', 'enelam'), corr('enelam', 'colbun'), 
                     corr('enelam', 'cmpc')),
                   c(corr('colbun', 'ipsa'), corr('colbun', 'sonda'), 
                     corr('colbun', 'enelam'), corr('colbun', 'colbun'), 
                     corr('colbun', 'cmpc')),
                   c(corr('cmpc', 'ipsa'), corr('cmpc', 'sonda'), 
                     corr('cmpc', 'enelam'), corr('cmpc', 'colbun'), 
                     corr('cmpc', 'cmpc')))
colnames(corr_table) <- c('ipsa', 'sonda', 'enelam', 'colbun', 'cmpc')                   
rownames(corr_table) <- c('ipsa', 'sonda', 'enelam', 'colbun', 'cmpc') 

corr_table

# portafolios

# construiremos los portafolios respectivos

port_A <- portfolio_ew_especifico(precios, c('colbun', 'cmpc'), n_port = 'A')
port_B <- portfolio_ew_especifico(precios, c('colbun', 'cmpc', 'enelam'), n_port = 'B')
port_C <- portfolio_ew_especifico(precios, c('colbun', 'cmpc', 'enelam', 'sonda'), n_port = 'C')

port_A <- port_A[port_A[,1] >= '2018-01-01',]
port_B <- port_B[port_B[,1] >= '2018-01-01',]
port_C <- port_C[port_C[,1] >= '2018-01-01',]

#desviacion estandar y retorno promedio de cada portafolio

# media geometrica, media aritmetica y desviacion estandar A
retorno_promedio(na.omit(as.numeric(port_A[,2])))
mean(na.omit(as.numeric(port_A[,2])))
sd(na.omit(as.numeric(port_A[,2])))

# media geometrica, media aritmetica y desviacion estandar B
retorno_promedio(na.omit(as.numeric(port_B[,2])))
mean(na.omit(as.numeric(port_B[,2])))
sd(na.omit(as.numeric(port_B[,2])))

# media geometrica, media aritmetica y desviacion estandar C
retorno_promedio(na.omit(as.numeric(port_C[,2]))) * 100
mean(na.omit(as.numeric(port_C[,2]))) * 100
sd(na.omit(as.numeric(port_C[,2]))) 
