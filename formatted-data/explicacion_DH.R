#el primer objeto de strata.cumtroof es un vector que contiene
#las variables para estratificar. En mi caso son los valores del p2. 
#Entonces pongo resultados[,12], que señala la columna que tiene los mismos
#el segundo es n, el tamaño de la muestra. Esta es igual a la cantidad de observaciones
#el ultimo es Ls, el numero de estratos que deseo, 5.
strata.DH_2020 <- strata.cumrootf(resultados[,12],
                                  n = length(resultados$p2distance),
                                  Ls = 5)


#aqui pego los estratos (q se encuentran en strata.DH_2020[[stratumID]])
#a el data frame de resultados, como una nueva columna
#observa que los estratos son numericos
assign(paste0("resultados"), data.frame(resultados, strata.DH_2020[["stratumID"]])) 


#aqui le pido en orden; que guarde en niveles al df "resultados"
#Que asigne niveles a la columna 13, del recien creado df niveles
#Estos niveles (levels) asignados van de 1 a 5, de muy bajo a muy alto,
#finalmente, junto la columna 13 de niveles con el df resultados y borro niveles

for(i in 1){
  niveles = get(paste0("resultados")) 
  levels(niveles[,13]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("resultados"), niveles)
  rm(niveles)
}
