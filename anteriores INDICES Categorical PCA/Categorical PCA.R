#Categorical PCA
#Install.packages("Gifi")
library("Gifi")
library("psych")
library("dplyr")


#--------
datos_onu <- read.csv("datos_onu_names.csv")


servicios <- datos_onu %>%
  select("Sanitario_conectado_a_drenaje","Como_consigue_agua", "A_donde_conecta_el_drenaje","Luz_electrica", "Combustible_para_cocinar", "Llaves_y_mangueras_en_casa") 
#select("P4_12", "P4_14", "P4_15", "P4_16", "P4_17")



modeloS<-princals(servicios, ndim = 2, ordinal = TRUE, ties = "s", knots = knotsGifi(servicios, "D"), 
                  degrees = 2, copies = 1, missing = "s", normobj.z = TRUE, active = TRUE,
                  itmax = 1000, eps = 1e-06, verbose = FALSE)

## scree plot
plot(modeloS, "screeplot") 

modeloS
modeloS$weights #Identificar en qué componente van

#Componente 1:
#Sanitario_conectado_a_drenaje  
#Como_consigue_agua            
#A_donde_conecta_el_drenaje     
#Combustible_para_cocinar       
#Llaves_y_mangueras_en_casa 

#Componente 2:
#Luz_electrica

modeloS$evals
modeloS$quantifications #por categoria
modeloS$dmeasures
modeloS$lambda
modeloS$rhat
modeloS$loadings
modeloS$scoremat
modeloS$transform

modeloS$objectscores

servicios$D1<-modeloS$objectscores[,1]
servicios$D2<-modeloS$objectscores[,2]

## loadings plot
plot(modeloS, "loadplot", main = "Loadings Plot")


servicios$Indice1<-with(servicios,100*(D1-min(D1))/(max(D1)-min(D1))) #Componente 1
servicios$Indice2<-with(servicios,100*(D2-min(D2))/(max(D2)-min(D2))) #Componente 2

servicios$IndiceSERVICIOS<-with(servicios,(Indice1+Indice2)/2)


#-----

habitabilidad <- datos_onu %>%
  select("Material_de_paredes", "Material_de_techo", "Material_de_piso", "Aislamiento_termico_en_techos", "Aislamiento_termico_en_paredes", "Aislamiento_termico_en_ventanas", "Aislamiento_de_ruido_en_techo", "Aislamiento_de_ruido_en_paredes", "Aislamiento_de_ruido_en_ventanas", "Aislamiento_de_ruido_en_puertas", "problemas_vivienda", "ran_hacinamiento") 
#select("P4_4", "P4_5", "P4_6", "P4_7_1", "P4_7_2", "P4_7_3", "P4_8_1", "P4_8_2", "P4_8_3", "P4_8_4", "problemas_vivienda", "ran_hacinamiento")

modeloH <-princals(habitabilidad, ndim = 3, ordinal = TRUE, ties = "s", knots = knotsGifi(habitabilidad, "D"), 
                  degrees = 2, copies = 1, missing = "s", normobj.z = TRUE, active = TRUE,
                  itmax = 1000, eps = 1e-06, verbose = FALSE)


modeloH
## scree plot
plot(modeloH, "screeplot") 

## loadings plot
plot(modeloH, "loadplot", main = "Loadings Plot")

#Obtener dimensiones

habitabilidad$D1<-modeloH$objectscores[,1]
habitabilidad$D2<-modeloH$objectscores[,2]
habitabilidad$D3<-modeloH$objectscores[,2]


modeloH$weights #Identificar en qué componente van
print(modeloH$weights, cut=0.5)

#Componente 1:
#Material_de_techo
#Aislamiento_termico_en_paredes
#Aislamiento_termico_en_ventanas
#Aislamiento_de_ruido_en_techo  
#Aislamiento_de_ruido_en_paredes 
#Aislamiento_de_ruido_en_ventanas 
#Aislamiento_de_ruido_en_puertas 

#Componente 2:
#Material_de_paredes
#Material_de_piso 
#ran_hacinamiento  

#Componente 3:
# Aislamiento_termico_en_techos 
# problemas_vivienda  



habitabilidad$Indice1<-with(habitabilidad,100*(D1-min(D1))/(max(D1)-min(D1))) #Componente 1
habitabilidad$Indice2<-with(habitabilidad,100*(D2-min(D2))/(max(D2)-min(D2))) #Componente 2
habitabilidad$Indice3<-with(habitabilidad,100*(D3-min(D3))/(max(D3)-min(D3))) #Componente 3

habitabilidad$IndiceHABITABILIDAD<-with(habitabilidad,(Indice1+Indice2+Indice3)/3)


#----------------
accesibilidad <- datos_onu %>%
  select("D_Rampas", "D_Puertas", "D_Banos", "D_Pasamanos", "Problema_en_su_barrio_con._equipo_para_personas_con_discapacidad")
# select("P6_7_1", "P6_7_2", "P6_7_3", "P6_7_4", "P6_9_1")


modeloA <-princals(accesibilidad, ndim = 2, ordinal = TRUE, ties = "s", knots = knotsGifi(accesibilidad, "D"), 
                   degrees = 2, copies = 1, missing = "s", normobj.z = TRUE, active = TRUE,
                   itmax = 1000, eps = 1e-06, verbose = FALSE)


modeloA

accesibilidad$D1<-modeloA$objectscores[,1]
accesibilidad$D2<-modeloA$objectscores[,2]

## loadings plot
plot(modeloA, "loadplot", main = "Loadings Plot")

## scree plot
plot(modeloA, "screeplot") 

accesibilidad$Indice1<-with(accesibilidad,100*(D1-min(D1))/(max(D1)-min(D1))) #Componente 1
accesibilidad$Indice2<-with(accesibilidad,100*(D2-min(D2))/(max(D2)-min(D2))) #Componente 2

accesibilidad$IndiceACCESIBILIDAD<-with(accesibilidad,(Indice1+Indice2)/2)

#----------------

ubicacion <- datos_onu %>%
  select("Satisfaccion_con_distancia_tiempo_entre_vivienda_y_trabajo", 
         "Satisfaccion_con_distancia_tiempo_entre_vivienda_y_centros_escolares", 
         "Satisfaccion_con_distancia_tiempo_entre_vivienda_y_centros_de_salud", 
         "Satisfaccion_con_distancia_tiempo_entre_vivienda_y_mercados", 
         "Satisfaccion_con_distancia_tiempo_entre_vivienda_y_parques",
         "Satisfaccion_con_distancia_tiempo_entre_vivienda_y_centros_de_recreaci.n.culturales",
         "Problema_en_su_barrio_con_exceso_de_ruido", "Problema_en_su_barrio_con_basura_en_la_calle",
         "Problema_en_su_barrio_con_fabricas_y_construcciones", "Problema_en_su_barrio_con_deterioro_o_abandono",
         "Problema_en_su_barrio_con_vandalismo", "Problema_en_su_barrio_con_robo_y_asalto")

modeloU <-princals(ubicacion, ndim = 2, ordinal = TRUE, ties = "s", knots = knotsGifi(ubicacion, "D"), 
                   degrees = 2, copies = 1, missing = "s", normobj.z = TRUE, active = TRUE,
                   itmax = 1000, eps = 1e-06, verbose = FALSE)


modeloU
modeloU$loadings
modeloU$quantifications

ubicacion$D1<-modeloU$objectscores[,1]
ubicacion$D2<-modeloU$objectscores[,2]


## loadings plot
plot(modeloU, "loadplot", main = "Loadings Plot")

## scree plot
plot(modeloU, "screeplot") 

ubicacion$Indice1<-with(ubicacion,100*(D1-min(D1))/(max(D1)-min(D1))) #Componente 1
ubicacion$Indice2<-with(ubicacion,100*(D2-min(D2))/(max(D2)-min(D2)))  #Componente 2

ubicacion$IndiceUBICACION<-with(ubicacion,(Indice1+Indice2)/2)

#----------------------
# Guardar bases de datos con indices creados
write.csv(servicios, file="Indiceservicios.csv")
write.csv(habitabilidad, file="Indicehabitabilidad.csv")
write.csv(accesibilidad, file="Indicesaccesibilidad.csv")
write.csv(ubicacion, file="Indiceubicacion.csv")
