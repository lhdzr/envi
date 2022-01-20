#INDICES OBJETIVOS DE CALIDAD DE LA VIVIENDA
library(dplyr)
library(psych)



datos_onu <- read.csv("datos_onu2.csv")

#TOMANDO EN CUENTA LOS FACTORES DE EXPANSION --> NO ME DEJO 
#no applicable method for 'select' applied to an object of class "c('survey.design2', 'survey.design')"
library(survey)
svy_datos_onu <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS,
                           weights = ~FACTOR, data = datos_onu)


# Variables a utilizar por indice caracteristica de la ONU


servicios <- datos_onu %>%
  select("P4_12", "P4_14", "P4_15", "P4_16", "P4_17")
    #“P4_12”, “P4_14”, “P4_15”, “P4_16”, “P4_17”)
  #select("Sanitario_conectado_a_drenaje","Como_consigue_agua", "A_donde_conecta_el_drenaje","Luz_electrica", "Combustible_para_cocinar", "Llaves_y_mangueras_en_casa") 


habitabilidad <- datos_onu %>%
  select("P4_4", "P4_5", "P4_6", "P4_7_1", "P4_7_2", "P4_7_3", "P4_8_1", "P4_8_2", "P4_8_3", "P4_8_4", "problemas_vivienda", "ran_hacinamiento")
  #select("Material_de_paredes", "Material_de_techo", "Material_de_piso", "Aislamiento_termico_en_techos", "Aislamiento_termico_en_paredes", "Aislamiento_termico_en_ventanas", "Aislamiento_de_ruido_en_techo", "Aislamiento_de_ruido_en_paredes", "Aislamiento_de_ruido_en_ventanas", "Aislamiento_de_ruido_en_puertas", "problemas_vivienda", "hacinamiento") 


accesibilidad <- datos_onu %>%
  select("P6_7_1", "P6_7_2", "P6_7_3", "P6_7_4", "P6_9_1")
  #select("D_Rampas", "D_Puertas", "D_Banos", "D_Pasamanos")


ubicacion <- datos_onu %>%
  select("P6_5_1", "P6_5_2", "P6_5_3", "P6_5_4", "P6_5_5", "P6_5_6", "P6_9_2", "P6_9_3", "P6_9_4", "P6_9_5", "P6_9_6", "P6_9_7")
  #select("Satisfaccion_con_distancia_tiempo_entre_vivienda_y_trabajo", "Satisfaccion_con_distancia_tiempo_entre_vivienda_y_centros_escolares", "Satisfaccion_con_distancia_tiempo_entre_vivienda_y_centros_de_salud", "Satisfaccion_con_distancia_tiempo_entre_vivienda_y_mercados", "Satisfaccion_con_distancia_tiempo_entre_vivienda_y_parques", "Satisfaccion_con_distancia_tiempo_entre_vivienda_y_centros_de_recreacion_culturales")

#NO NECESITAN INDICE --> SERAN KPIs

#adecuacionCult <- svy_datos_onu %>%  
#select("Adaptacion_cultural")

#segTenencia <- svy_datos_onu %>%
#select("")

#asequibilidad <- svy_datos_onu %>%
#select("")

#---------------------------------------------------
  #SERVICIOS
#---------------------------------------------------

#No hay NA
colSums(is.na(servicios)) #Ya arregle los NA

#Análisis de las variables en SERVICIOS
describe(servicios)

#Comprobar la realización de Análisis de factores

#1. Correlación
corPlot(servicios,cex = .6,stars = TRUE, show.legend=FALSE)



#2. Determinante de la matriz de correlaciones
#Estimar la matriz de correlaciones
S<-cor(servicios)
#Calcular el determinante de la matriz
det(S) #No es muy cercano a cero

#3. Prueba de contraste de esfericidad de Bartlett
cortest.bartlett(S,n= 55147) #p-value < 0.05

#Analisis de suficiencia general o índice Kaiser-Meyer-Olkin
  #|Criterio        |Evaluación |
 # |----------------|-----------|
  #|MSA ≥ 0.9       |Excelente  |
 # |0.8 ≤ MSA < 0.9 |Bueno      |
  #|0.7 ≤ MSA < 0.8 |Aceptable  |
  #|0.6 ≤ MSA < 0.7 |Regular    | <-----
  #|0.5 ≤ MSA < 0.6 |Bajo       |
  #|MSA < 0.5       |Inaceptable|

KMO(S) #Overall MSA= 0.74 (REGULAR). Todos > 0.5 no hay que borrar variables

#Estandarización de las variables
DzS <- data.frame(scale(servicios)) #Estandarizar las variables del indicador SERVICIOS

#MODELOS
modeloS1 <-principal(DzS,nfactors = 5, rotate= "none")
modeloS1

#MODELO FINAL
modeloS2<-principal(DzS,nfactors = 2, rotate='none')
modeloS2

modeloS2<-principal(DzS,nfactors = 2, rotate="varimax")
modeloS2

#cARGAS POR COMPONENTE
print(modeloS2$loadings,cut=0.5, sort=TRUE) #SOLO P4_16 QUEDA EN COMPONENTE 2 Y TODOS EN MISMO SENTIDO

fa.diagram(modeloS2)


#oBTENCIÓN DE PUNTAJES
modeloS2<-principal(DzS,nfactors = 2, rotate="varimax",
                    scores=TRUE,method="regression") #con el método que queramos calcular los puntajes

DzS$PC1<-modeloS2$scores[,1]
DzS$PC2<-modeloS2$scores[,2]

modeloS2$weights 

#cHECAR MISMO SENTIDO DE LAS VARIABLES
alpha(DzS[,c(1,2,3,4,5)],check.keys=TRUE)


#Poner el índice de 0 a 100
DzS$Indice1<-with(DzS,100*(PC1-min(PC1))/(max(PC1)-min(PC1))) #Componente 1
DzS$Indice2<-with(DzS,100*(PC2-min(PC2))/(max(PC2)-min(PC2))) #Componente 2

print(modeloS2)

#                       RC1  RC2
#SS loadings           2.26 1.01
#Proportion Var        0.45 0.20
#Cumulative Var        0.45 0.66
#Proportion Explained  0.69 0.31
#Cumulative Proportion 0.69 1.00

#Ponderadores obtenidos del modelo estimado, renglon "Proportion Var".
w1S=0.45/(0.45+0.20)
w2S=0.20/(0.20+0.45)


DzS$IndiceSERVICIOS<-with(DzS,w1S*Indice1+w2S*Indice2) #ponderando los dos índices, nuestro final se llama columna ÍndiceSERVICIOS

write.csv(DzS, file="IndiceSERVICIOS.csv")

#---------------------------------------------------------------
#HABITABILIDAD
#--------------------------------------------------------------


#No hay NA
colSums(is.na(habitabilidad))

#Análisis de las variables en SERVICIOS
describe(habitabilidad)

#Comprobar la realización de Análisis de factores

#1. Correlación
corPlot(habitabilidad,cex = .6,stars = TRUE, show.legend=FALSE)


#2. Determinante de la matriz de correlaciones
#Estimar la matriz de correlaciones
H<-cor(habitabilidad)
#Calcular el determinante de la matriz
det(H) #No es muy cercano a cero

#3. Prueba de contraste de esfericidad de Bartlett
cortest.bartlett(H,n= 55147) #p-value < 0.05

#Analisis de suficiencia general o índice Kaiser-Meyer-Olkin

KMO(H) #Overall MSA= 0.78 (REGULAR). Todos > 0.5 no hay que borrar variables

#Estandarización de las variables
DzH <- data.frame(scale(habitabilidad)) #Estandarizar las variables del indicador HABITABILIDAD

#MODELOS
modeloH1 <-principal(DzH,nfactors = 12, rotate= "none") 
modeloH1 #Cumulative var sugiere 4 (60% varianza), mean item complexity=3

#MODELO FINAL
modeloH2<-principal(DzH,nfactors = 3, rotate='none') #3 explican el 52% de la varianza
modeloH2

modeloH2<-principal(DzH,nfactors = 3, rotate="varimax")
modeloH2

#cARGAS POR COMPONENTE
print(modeloH2$loadings,cut=0.3, sort=TRUE) #Con 0.5 se quedan fuera problemas de vivenda y hacinamiento

fa.diagram(modeloH2) #Problemas de vivienda está negativo? 

#Reescalar variable de Problemas de la vivienda (negativa)
DzH$problemas_vivienda<-with(DzH,max(problemas_vivienda)-problemas_vivienda)
modeloH2<-principal(DzH,nfactors = 3, rotate="varimax")
modeloH2


#OBTENCIÓN DE PUNTAJES
modeloH2<-principal(DzH,nfactors = 3, rotate="varimax",
                    scores=TRUE,method="regression") #con el método que queramos calcular los puntajes

DzH$PC1<-modeloH2$scores[,1]
DzH$PC2<-modeloH2$scores[,2]
DzH$PC3<-modeloH2$scores[,3]

modeloH2$weights 


#cHECAR MISMO SENTIDO DE LAS VARIABLES
alpha(DzH[,c(1:12)],check.keys=TRUE)
fa.diagram(modeloH2)


#Poner el índice de 0 a 100
DzH$Indice1<-with(DzH,100*(PC1-min(PC1))/(max(PC1)-min(PC1))) #Componente 1
DzH$Indice2<-with(DzH,100*(PC2-min(PC2))/(max(PC2)-min(PC2))) #Componente 2
DzH$Indice3<-with(DzH,100*(PC3-min(PC3))/(max(PC3)-min(PC3))) #Componente 2

print(modeloH2)

#                       RC1  RC2  RC3
#SS loadings           3.63 2.93 2.69
#Proportion Var        0.24 0.20 0.18
#Cumulative Var        0.24 0.44 0.62
#Proportion Explained  0.39 0.32 0.29
#Cumulative Proportion 0.39 0.71 1.00

#Ponderadores obtenidos del modelo estimado, renglon "Proportion Var".
w1H=0.24/(0.24+0.20+0.18)
w2H=0.20/(0.20+0.24+0.18)
w3H=0.18/(0.18+0.20+0.24)


DzH$IndiceHABITABILIDAD<-with(DzH,w1H*Indice1+w2H*Indice2+w3H*Indice3) #ponderando los dos índices, nuestro final se llama columna ÍndiceHABITABILIDAD

write.csv(DzH, file="IndiceHABITABILIDAD.csv")

#---------------------------------------------------------------
#ACCESIBILIDAD
#--------------------------------------------------------------

#No hay NA
colSums(is.na(accesibilidad))

#Análisis de las variables en SERVICIOS
describe(accesibilidad)

#Comprobar la realización de Análisis de factores

#1. Correlación
corPlot(accesibilidad,cex = .6,stars = TRUE, show.legend=FALSE)


#2. Determinante de la matriz de correlaciones
#Estimar la matriz de correlaciones
A<-cor(accesibilidad)
#Calcular el determinante de la matriz
det(A) #Cercano a cero

#3. Prueba de contraste de esfericidad de Bartlett
cortest.bartlett(A,n= 55147) #p-value < 0.05

#Analisis de suficiencia general o índice Kaiser-Meyer-Olkin

KMO(A) #Overall MSA= 0.86 (BUENO). Todos > 0.5 no hay que borrar variables

#Estandarización de las variables
DzA <- data.frame(scale(accesibilidad)) #Estandarizar las variables del indicador ACCESIBILIDAD

#MODELOS
modeloA1 <-principal(DzA,nfactors = 5, rotate= "none") 
modeloA1 #Cumulative var sugiere 1 (67% varianza), mean item complexity=1.4

#MODELO FINAL
modeloA2<-principal(DzA,nfactors = 2, rotate='none')
modeloA2

modeloA2<-principal(DzA,nfactors = 2, rotate="varimax")
modeloA2

#cARGAS POR COMPONENTE
print(modeloA2$loadings,cut=0.5, sort=TRUE) #Solo P6_9_1 Se queda fuera (no está correlacionado Y ESTÁ NEGATIVA)
fa.diagram(modeloA2) 

#Reescalar variable de Problemas de la vivienda (negativa)
DzA$P6_9_1<-with(DzA,max(P6_9_1)-P6_9_1)
modeloA2<-principal(DzA,nfactors = 2, rotate="varimax")
modeloA2


#OBTENCIÓN DE PUNTAJES
modeloA2<-principal(DzA,nfactors = 2, rotate="varimax",
                    scores=TRUE,method="regression") #con el método que queramos calcular los puntajes

DzA$PC1<-modeloA2$scores[,1]
DzA$PC2<-modeloA2$scores[,2]


modeloA2$weights 


#cHECAR MISMO SENTIDO DE LAS VARIABLES
alpha(DzA[,c(1:5)],check.keys=TRUE)
fa.diagram(modeloA2)


#Poner el índice de 0 a 100
DzA$Indice1<-with(DzA,100*(PC1-min(PC1))/(max(PC1)-min(PC1))) #Componente 1
DzA$Indice2<-with(DzA,100*(PC2-min(PC2))/(max(PC2)-min(PC2))) #Componente 2


print(modeloA2)

#                       RC1  RC2
#SS loadings           4.34 1.99
#Proportion Var        0.62 0.28
#Cumulative Var        0.62 0.90
#Proportion Explained  0.69 0.31
#Cumulative Proportion 0.69 1.00

#Ponderadores obtenidos del modelo estimado, renglon "Proportion Var".
w1A=0.62/(0.62+0.28)
w2A=0.28/(0.28+0.62)


DzA$IndiceACCESIBILIDAD<-with(DzA,w1A*Indice1+w2A*Indice2) #ponderando los dos índices, nuestro final se llama columna ÍndiceACCESIBILIDAD

write.csv(DzA, file="IndiceACCESIBILIDAD.csv")


#---------------------------------------------------------------
#UBICACACION
#--------------------------------------------------------------

#No hay NA
colSums(is.na(ubicacion))

#Análisis de las variables en SERVICIOS
describe(ubicacion)

#Comprobar la realización de Análisis de factores

#1. Correlación
corPlot(ubicacion,cex = .6,stars = TRUE, show.legend=FALSE)


#2. Determinante de la matriz de correlaciones
#Estimar la matriz de correlaciones
U<-cor(ubicacion)
#Calcular el determinante de la matriz
det(U) #Cercano a cero

#3. Prueba de contraste de esfericidad de Bartlett
cortest.bartlett(U,n= 55147) #p-value < 0.05

#Analisis de suficiencia general o índice Kaiser-Meyer-Olkin

KMO(U) #Overall MSA= 0.82 (BUENO). Todos > 0.5 no hay que borrar variables

#Estandarización de las variables
DzU <- data.frame(scale(ubicacion)) #Estandarizar las variables del indicador UBICACION

#MODELOS
modeloU1 <-principal(DzU,nfactors = 12, rotate= "none") 
modeloU1 #Cumulative var sugiere 4 (64% varianza), mean item complexity=3.5

#MODELO FINAL
modeloU2<-principal(DzU,nfactors = 4, rotate='none')
modeloU2

modeloU2<-principal(DzU,nfactors = 4, rotate="varimax")
modeloU2

#cARGAS POR COMPONENTE
print(modeloU2$loadings,cut=0.5, sort=TRUE) #Ninguna fuera, todas positivas
fa.diagram(modeloU2) 



#OBTENCIÓN DE PUNTAJES
modeloU2<-principal(DzU,nfactors = 4, rotate="varimax",
                    scores=TRUE,method="regression") #con el método que queramos calcular los puntajes

DzU$PC1<-modeloU2$scores[,1]
DzU$PC2<-modeloU2$scores[,2]
DzU$PC3<-modeloU2$scores[,3]
DzU$PC4<-modeloU2$scores[,4]


modeloU2$weights 


#cHECAR MISMO SENTIDO DE LAS VARIABLES
alpha(DzU[,c(1:12)],check.keys=TRUE)


#Poner el índice de 0 a 100
DzU$Indice1<-with(DzU,100*(PC1-min(PC1))/(max(PC1)-min(PC1))) #Componente 1
DzU$Indice2<-with(DzU,100*(PC2-min(PC2))/(max(PC2)-min(PC2))) #Componente 2
DzU$Indice3<-with(DzU,100*(PC2-min(PC3))/(max(PC3)-min(PC3))) #Componente 3
DzU$Indice4<-with(DzU,100*(PC2-min(PC4))/(max(PC4)-min(PC4))) #Componente 4

print(modeloU2)

#                       RC1  RC2  RC4  RC3
#SS loadings           2.47 1.96 1.67 1.61
#Proportion Var        0.21 0.16 0.14 0.13
#Cumulative Var        0.21 0.37 0.51 0.64
#Proportion Explained  0.32 0.25 0.22 0.21
#Cumulative Proportion 0.32 0.57 0.79 1.00

#Ponderadores obtenidos del modelo estimado, renglon "Proportion Var".
w1U=0.21/(0.21+0.16+0.14+0.13)
w2U=0.16/(0.21+0.16+0.14+0.13)
w3U=0.14/(0.21+0.16+0.14+0.13)
w4U=0.13/(0.21+0.16+0.14+0.13)


DzU$IndiceUBICACION<-with(DzU,w1U*Indice1+w2U*Indice2+w3U*Indice3+w4U*Indice4) #ponderando los dos índices, nuestro final se llama columna ÍndiceACCESIBILIDAD

write.csv(DzU, file="IndiceUBICACION.csv")
