#Categorical PCA
#Install.packages("Gifi")
library("Gifi")
library("psych")
library("dplyr")


#--------
satisfaction_data <- read.csv("satisfaction_names.csv")


satisfaction <- satisfaction_data %>%
  select("Satisfaccion_con_pisos", "Satisfaccion_con_muros_y_techos", "Satisfaccion_con_pintura_y_recubrimientos", 
  "Satisfaccion_con_iluminacion_natural", "Satisfaccion_con_ventilacion_natural", "Satisfaccion_con_proteccion_contra_exterior",
  "Satisfaccion_con_tamano_de_dormitorios", "Satisfaccion_con_tamano_de_banos", "Satisfaccion_con_tamano_de_cocina",
  "Satisfaccion_con_tamano_de_sala_comedor", "Satisfaccion_con_tamano_de_patio") 

#-----
#PRUEBAS

#Matriz de correlación

corPlot(satisfaction,cex = .6,stars = TRUE, show.legend=FALSE)


# Determinante de la matriz de correlaciones


#Estimar la matriz de correlaciones
SAT<-cor(satisfaction)
#Calcular el determinante de la matriz
det(SAT)# Es cercano a cero.


#El análisis de suficiencia general o índice Kaiser-Meyer-Olkin
KMO(SAT) 
#Overall MSA =  0.9 --> BUENO

# Criterios para determinar el número de factores

scree(SAT)         #Gráfico de sedimentación NOS INDICA 2 DIMENSIONES
plot(modeloSAT, plot.type = "screeplot")

#---

modeloSAT <-princals(satisfaction, ndim = 2, ordinal = TRUE, ties = "s", knots = knotsGifi(satisfaction, "E"), 
                   degrees = 2, copies = 1, missing = "m", normobj.z = TRUE, active = TRUE,
                   itmax = 1000, eps = 1e-06, verbose = FALSE)

modeloSAT

#Transformation plots for different types of quantification. 
plot(modeloSAT, plot.type = "transplot", var.subset = 1:4, lwd = 2, col.lines = "coral")

#Observed category scores are on
#the x-axis, and the numeric values (standard scores) obtained after optimal
#quantification (category quantifications) are on the y-axis. The line connecting
#category quantifications indicates the variable's transformation


modeloSAT$objectscores
modeloSAT$quantifications
modeloSAT$weights #Identificar en que componente van?

#Obtener dimensiones

satisfaction$D1<-modeloSAT$objectscores[,1]
satisfaction$D2<-modeloSAT$objectscores[,2]

satisfaction$Indice1SAT<-with(satisfaction,100*(D1-min(D1))/(max(D1)-min(D1))) #Componente 1
satisfaction$Indice2SAT<-with(satisfaction,100*(D2-min(D2))/(max(D2)-min(D2))) #Componente 2

satisfaction$IndiceSATISFACCION<-with(satisfaction,(Indice1SAT+Indice2SAT)/2)


write.csv(satisfaction, "IndiceSatisfaccion_CATPCA.csv")



#ESTRATIFICACION
library("stratification")

strata.CATPCASer <- strata.cumrootf(satisfaction[,16],
                                    n = length(satisfaction),
                                    Ls = 5)

assign(paste0("satisfaction"), data.frame(satisfaction, strata.CATPCASer[["stratumID"]])) 


for(i in 1){
  niveles = get(paste0("satisfaction")) 
  levels(niveles[,17]) = c("Muy baja", "Baja","Media","Alta","Muy Alta")
  assign(paste0("satisfaction"), niveles)
  rm(niveles)
}

write.csv(satisfaction_data, "Satisfaction_CATPCA_Estratos.csv")
