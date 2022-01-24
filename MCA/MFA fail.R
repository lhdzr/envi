library("FactoMineR")
library("factoextra")
library(dplyr)

datos_onu <- read.csv("datos_onu_names.csv")

servicios.m<- datos_onu %>%
  select("FACTOR","Sanitario_conectado_a_drenaje","Como_consigue_agua", "A_donde_conecta_el_drenaje","Luz_electrica", "Combustible_para_cocinar", "Llaves_y_mangueras_en_casa") 


datos=read.table("datos_onu_names.csv",header=TRUE,sep=",",row.names=1)

              mfa <- MFA(datos,
                     group = c(6,12,5,12),
                     type = c("n","n","n","n"),
                     ncp=5,
                     name.group = c("servicios","habitabilidad","accesibilidad", "ubicacion"),
                     num.group.sup = c(4), graph = TRUE, weight.col.mfa = NULL, 
                     row.w = "FACTOR", axes = c(1,2), tab.comp=NULL)
                    
                    




habitabilidad <- datos_onu %>%
  select("Material_de_paredes", "Material_de_techo", "Material_de_piso",
         "Aislamiento_termico_en_techos", "Aislamiento_termico_en_paredes", 
         "Aislamiento_termico_en_ventanas", "Aislamiento_de_ruido_en_techo", 
         "Aislamiento_de_ruido_en_paredes", "Aislamiento_de_ruido_en_ventanas", 
         "Aislamiento_de_ruido_en_puertas", "problemas_vivienda", "ran_hacinamiento") 
#select("P4_4", "P4_5", "P4_6", "P4_7_1", "P4_7_2", "P4_7_3", "P4_8_1", "P4_8_2", "P4_8_3", "P4_8_4", "problemas_vivienda", "ran_hacinamiento")



accesibilidad <- datos_onu %>%
  select("D_Rampas", "D_Puertas", "D_Banos", "D_Pasamanos", 
         "Problema_en_su_barrio_con._equipo_para_personas_con_discapacidad")
# select("P6_7_1", "P6_7_2", "P6_7_3", "P6_7_4", "P6_9_1")



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
