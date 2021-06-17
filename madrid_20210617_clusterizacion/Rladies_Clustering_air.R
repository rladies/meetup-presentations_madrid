


### ###  INSTALAR PAQUETES

install.packages("factoextra") 

library(factoextra) # clustering algorithms & visualization
library(cluster)    # clustering algorithms

library(tidyverse)  
library(gridExtra)
library(dplyr)
library(reshape2)
library(ggmap)


### ### CARGAR LOS DATOS

## LOCACLIZACIÓN DE ESTACIONES METEOROLÓGICAS:
estaciones <- read.csv(file = 'C:/Users/Elena/Documents/_ACADEMIC/RLADIES/datos_calidad_airemadrid/Anio202104/informacion_estaciones_red_calidad_aire_red.csv',
                     stringsAsFactors = FALSE,
                     header = TRUE,
                     sep = ";")

## DESCRIPCIÓN MAGNITUD DE LAS MEDICIONES DE CALIDAD DEL AIRE:
magnitud <- read.csv(file = 'C:/Users/Elena/Documents/_ACADEMIC/RLADIES/datos_calidad_airemadrid/Anio202104/magnitud.csv',
                stringsAsFactors = FALSE,
                header = TRUE,
                sep = ";")

## MEDICIONES TOMADAS EN CADA ESTACIÓN, PARA LAS DIFERENTES MEDIACIONES, PARA LAS 24 H
df0 <- read.csv(file = 'C:/Users/Elena/Documents/_ACADEMIC/RLADIES/datos_calidad_airemadrid/Anio202104/abr_mo21.csv',
                stringsAsFactors = FALSE,
                header = TRUE,
                sep = ";")

## UNIÓN DE LOS DATASETS
df1 <- merge(df0, magnitud, by.x = "MAGNITUD", by.y = "MAGNITUD", all.x = TRUE, all.y = FALSE)
df1 <- merge(df1, estaciones, by.x = "ESTACION", by.y = "ESTACION", all.x = TRUE, all.y = FALSE)

head(df1)

### ### EXPLORAR LOS DATOS

# CONTEO POR ESTACIONES
df1_res = df1 %>% 
          group_by(NOMBRE_ESTACION ) %>% 
          summarise(n = n())

# CONTEO POR ESTACION Y MAGNITUD
df1_res1 = df1 %>% 
  group_by(NOMBRE_ESTACION, NOMBRE_MAGNITUD ) %>% 
  summarise(n = n())


# BUSCAR MAGNITUDES QUE ESTÉN COMPLETAS EN TODAS LAS ESTACIONES
df1_cast <- dcast(df1_res1, NOMBRE_ESTACION~NOMBRE_MAGNITUD, fun.aggregate = mean)
df1_cast_red = df1_cast[ , colSums(is.na(df1_cast)) == 0]


# SELECCIÓN DE LOS DATOS QUE VAMOS A USAR EN EL CLUSTERING
# Nos quedamos con 1 dia del MES y una MAGNITUD
df <- subset(df1, DIA == 7 & NOMBRE_MAGNITUD == "DióxidodeNitrógeno")

# SELECCIÓN DE LAS COLUMNAS DE DATOS - NUMERICOS
val_hora <- df[ , seq(9, 55, by=2)]
rownames(val_hora) <- df$NOMBRE_ESTACION

# SELECCIÓN DE LAS COLUMNAS DE DATOS - CATEGORICOS
val_principales <- df[ , c("ANO","MES","DIA","NOMBRE_ESTACION","NOMBRE_MAGNITUD","LATITUD","LONGITUD")]

df_final <-cbind(val_principales,val_hora)

    
### ### ESCALAR LOS DATOS
df_scaled <- scale(val_hora) 

#summary(val_hora)
#summary(df_scaled)

### ### CALCULAR LA MATRIZ DE DISTANCIAS ENTRE TODOs LOS ELEMENTOS

distancias <- get_dist(df_scaled, method = "euclidean") # por defecto distancia euclídea

#Acepta los siguientes métodos:
# "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", 
# "pearson", "spearman" , "kendall"


### ### VISUALIZAR LAS DISTANCIAS (rojo = grandes diferencias) 

fviz_dist(distancias, gradient = list(low = "green", mid = "white", high = "red"))

################################ K-MEANS CLUSTERING ################################ 

# Número de clusters
centers = 2

# Número de configuraciones iniciales
nstarts = 25

k2 <- kmeans(df_scaled, centers = centers, nstart = nstarts)
str(k2)

# OUTPUT:
k2$cluster         # vector de enteros que indica el cluster de cada línea del dataset (from 1:k) 
k2$centers         # matriz de centroides
k2$totss           # la suma total de los cuadrados
k2$withinss        #vector de suma de cuadrados dentro del cluster, uno por cluster
k2$tot.withinss    #Suma de cuadrados total dentro del cluster
k2$betweenss       #La suma de cuadrados entre clusters
k2$size            #el número de puntos en cada cluster


# VISUALIZACIÓN EN 2 DIMENSIONES: 
# SI hay más de 2 dimensiones calculará PCA (Análisis de componentes principales) para reducirlo a las 2 componentes
# principales que explican la mayoría de la varianza

fviz_cluster(k2, data = df_scaled)

### COMPARAR VARIAS CONFIGURACIONES

k3 <- kmeans(df_scaled, centers = 3, nstart = nstarts)
k4 <- kmeans(df_scaled, centers = 4, nstart = nstarts)
k5 <- kmeans(df_scaled, centers = 5, nstart = nstarts)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df_scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df_scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df_scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df_scaled) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)


### BUSCAR EL NÚMERO DE CLUSTERING ÓPTIMO 

set.seed(123)
fviz_nbclust(df_scaled, kmeans, method = "silhouette")
fviz_nbclust(df_scaled, kmeans, method = "wss")

# acepta los siguientes métodos: "silhouette", "wss", "gap_stat"

###### SELECCIÓN SOLUCIÓN ÓPTIMA

clu_sel <- 2

k <- kmeans(df_scaled, centers = clu_sel, nstart = nstarts)
fviz_cluster(k, data = df_scaled)

### RESULTADOS

final <-  df_final
final$cluster <- as.factor(k$cluster)  

k$size 

#####  VISUALIZAR ESTACIONES Y CLUSTERS GEOLOCALIZADOS

qmplot(LONGITUD, LATITUD, data = final, maptype = "toner-lite", color = cluster,
       size = I(6), alpha = I(.6), legend = "topleft", shape = I(15) )
 

#####  ANALIZAR LOS RESULTADOS
# calcular la media diaria por estación para identificar qué es cluster 1(alta emisión), 2 (baja emisión)
final$Mean <- rowMeans(final[,8:31])



################################ CLUSTERING JERÁRQUICO ################################ 

# Matriz de distancias
d <- dist(df_scaled, method = "euclidean")

# USING COMPLETE LNKAGE
hc1 <- hclust(d, method = "complete" )

# Visualización del arbol del cluster
plot(hc1, hang = -1,cex=0.7,labels = rownames(df_scaled), main= "Cluster Estaciones Calidad Aire")

# Visualización de los grupos formados por el cluster

k = 2
fviz_dend(x = hc1, k = k, cex = 0.6) +
  geom_hline(yintercept = 12, linetype = "dashed") +
  labs(title = "Herarchical clustering",
  subtitle = "Distancia euclídea, Lincage complete, K=2")

k = 3
fviz_dend(x = hc1, k = k, cex = 0.6) +
  geom_hline(yintercept = 9, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=3")

k = 4
fviz_dend(x = hc1, k = k, cex = 0.6) +
  geom_hline(yintercept = 7, linetype = "dashed") +
  labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Lincage complete, K=4")



### RESULTADOS

# Creación de k grupos
clusters <- cutree(hc1, k=2)

final$cluster_JERQ <- as.factor(clusters)  


#####  VISUALIZAR ESTACIONES Y CLUSTERS GEOLOCALIZADOS

qmplot(LONGITUD, LATITUD, data = final, maptype = "toner-lite", color = cluster_JERQ,
       size = I(6), alpha = I(.6), legend = "topleft", shape = I(15) )

