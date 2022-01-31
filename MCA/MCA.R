library("FactoMineR")
library("factoextra")
library("dplyr")
library("ExPosition")
library("prettyGraphs")

datos_onu <- read.csv("datos_onu_names.csv")

servicios<- datos_onu %>%
  select("FACTOR", "Sanitario_conectado_a_drenaje","Como_consigue_agua", "A_donde_conecta_el_drenaje","Luz_electrica", "Combustible_para_cocinar", "Llaves_y_mangueras_en_casa") 

#Variables as factors
i=0
while(i < ncol(servicios)){
  i=i+1  
  servicios[,i] = as.factor(servicios[,i])
}


#servicios[,c(1:6)] <- lapply(servicios[,c(1:6)] , factor)

ModeloS<- MCA(servicios,row.w = length(servicios$FACTOR),  ncp = 5)

summary(ModeloS)


modelo <- epMCA(serviciosN, make_data_nominal = TRUE, DESIGN = NULL, make_design_nominal = FALSE, 
      masses = NULL, weights = TRUE, hellinger = FALSE, 
      symmetric = TRUE, correction = c("b"), graphs = FALSE, k = 2)


print(modelo$loadings,cut=0.5, sort=TRUE)
fa.diagram(modeloS2)

#OBTENCION DE PUNTAJES
modeloS2<-MCA(servicios,ncp = 2, 
                  method="indicator") #con el método que queramos calcular los puntajes



servicios$PC1<-modeloS2$scores[,1]
servicios$PC2<-modeloS2$scores[,2]

modeloS2$weights 

#get_eigenvalue(res.mca): Extract the eigenvalues/variances retained by each dimension (axis)
#fviz_eig(res.mca): Visualize the eigenvalues/variances
#get_mca_ind(res.mca), get_mca_var(res.mca): Extract the results for individuals and variables, respectively.
#fviz_mca_ind(res.mca), fviz_mca_var(res.mca): Visualize the results for individuals and variables, respectively.
#fviz_mca_biplot(res.mca): Make a biplot of rows and columns.

#The proportion of variances retained by the different dimensions (axes)
eig.val <- get_eigenvalue(S)
eig.val

#visualize the percentages of inertia explained by each MCA dimensions
fviz_screeplot(S, addlabels = TRUE, ylim = c(0, 45))


#draw the biplot of individuals and variable categories:
fviz_mca_biplot(S, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())
#Explanaition:
#The plot above shows a global pattern within the data. Rows (individuals) are represented 
#by blue points and columns (variable categories) by red triangles.

#The distance between any row points or column points gives a measure of their similarity 
#(or dissimilarity). Row points with similar profile are closed on the factor map. 
#The same holds true for column points. 


#MODELO FINAL
modeloP<-principal(S,nfactors = 2, rotate='none')
modeloP

modeloS2<-principal(DzS,nfactors = 2, rotate="varimax")
modeloS2

#cARGAS POR COMPONENTE
print(modeloS2$loadings,cut=0.4, sort=TRUE) #SOLO P4_16 QUEDA EN COMPONENTE 2 Y negativo

fa.diagram(modeloS2)





