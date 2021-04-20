install.packages("stargazer")
library(stargazer)

residuosdataframe = c()

for (name in nombres){
  
  #print(paste('######## ',name,' ########'))
  
  pos = which(colnames(rendimientos) == name)
  
  retorno = as.numeric(rendimientos[,pos])
  x = IPSA
  reg = lm(retorno~IPSA)
  
  stargazer(reg, title = name, report = "vct*", omit.table.layout = "sn")
  
  #print(summary(reg))
  
  predicciones = predict(reg, newdata = data.frame(x))
  residuos = unname(retorno - predicciones)
  
  
  residuosdataframe = cbind(residuosdataframe, residuos)
}

#Agregamos la fecha y cambiamos los nombres

Dates <- precios[-1,1]

residuosdataframe = cbind(Dates, residuosdataframe)
colnames(residuosdataframe)[1] = 'fecha'
colnames(residuosdataframe)[2:31] = nombres

