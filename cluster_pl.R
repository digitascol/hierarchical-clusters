#Paquetes
library(tidyverse)
library(RColorBrewer)
library(scales)
library(readxl)
library(viridis)
library(tseries)
library(urca)
library(forecast)
library(reshape2)
library(cluster)
library(Matrix)
library(stringdist)
library(tempdisagg)
library(tsbox)
#Objetivo: Cluster y pronóstico
#Importar base
base <- read_excel("x", col_names = T)
#Valores unicos 
uniqueThema <- unique(as.character(base$Keyword))
#Calculo de distancias entre palabras
distancemodels <- stringdistmatrix(uniqueThema,uniqueThema,method = "jw")
#Nombrar filas con los keywords (matrix m*m)
rownames(distancemodels) <- uniqueThema
#Cluster jerarquico 
hc <- hclust(as.dist(distancemodels), method = "complete")
#Grafico jerarquico 
plot(hc)
#Numero de clusters
k=3
#cuantos keywords hay por cluster
summary(as.factor(cutree(hc, k = k)))
####CLUSTER
x<- as.data.frame(cutree(hc, k = k))
fix(x)
# Unir las bases y nombrar las columnas
dfClust <- data.frame(uniqueThema, cutree(hc, k=k))
names(dfClust) <- c('Thema','cluster')
dfClust2 <- cbind(base,dfClust$cluster)
fix(dfClust2)
#Una vez se tiene la base se organiza para realizar proyecciones
base_proyeccion <- dfClust2
#Nombrar las columnas por la fecha
colnames(base_proyeccion)[1:length(base)] <- c("Palabras",
                                               "1/06/2017","1/07/2017","1/08/2017", "1/09/2017","1/10/2017", "1/11/2017", "1/12/2017",
                                               "1/01/2018","1/02/2018","1/03/2018", "1/04/2018","1/05/2018", "1/06/2018", "1/07/2018","1/08/2018","1/09/2018","1/10/2018","1/11/2018","1/12/2018",
                                               "1/01/2019","1/02/2019","1/03/2019", "1/04/2019","1/05/2019", "1/06/2019", "1/07/2019","1/08/2019","1/09/2019","1/10/2019","1/11/2019","1/12/2019",
                                               "1/01/2020","1/02/2020","1/03/2020", "1/04/2020","1/05/2020", "1/06/2020", "1/07/2020","1/08/2020","1/09/2020","1/10/2020","1/11/2020","1/12/2020", 
                                               "1/01/2021","1/02/2021","1/03/2021","1/04/2021","1/05/2021", "cluster")
colnames(base_proyeccion)[50] <- "Cluster"
#Formato wide a long
base_long <- melt(data = base_proyeccion[,-1],id.vars = c("Cluster"),variable_name = c("Fecha"))
#Modificar formato de las variables
base_long$variable <- as.Date(base_long$variable, format = "%d/%m/%Y") 
base_long$value <- as.numeric(base_long$value)
base_long$Cluster <- as.factor(base_long$Cluster)
#Renombrar
colnames(base_long) <- c("Cluster", "Fecha", "Valor")
#Group by para resumir
base_agrupada <- base_long %>% group_by(Cluster, Fecha) %>% 
  summarise(Valor = sum(Valor))
#Darle nombre a los clusters
base_agrupada$Cluster <- gsub(x = base_agrupada$Cluster,"1","Cultura")
base_agrupada$Cluster <- gsub(x = base_agrupada$Cluster,"2","Fiestas y Carnavales")
base_agrupada$Cluster <- gsub(x = base_agrupada$Cluster,"3","Iconos")
#### Grafico por cluster en el tiempo ####
base_agrupada %>% ggplot(mapping = aes(x = Fecha, y = Valor, color = Cluster)) + 
  geom_line(size=1)+ theme_classic() + 
  labs(title =  "Comportamiento de la Chispa Panameña", x = "", y = "", subtitle = "", color = "") + 
  scale_color_discrete(type = viridis(3)) + 
  scale_y_continuous(labels = comma_format(big.mark = ".",decimal.mark = ",")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position="top")
###Exportar a excel
rio::export(x=base_proyeccion, "panama_cluster.xlsx")

#### Modelo de series de tiempo pronóstico ####
#Desagregación temporal con el método Denton: de mensual a semanal
denton <- td(base_agrupada[49:96,]$Valor ~ 1, to = 4, method = "denton-cholette")
#Tamaño
length(denton$values) #192 valores equivalentes a las semanas
#Pronostico modelo AR4
plot(forecast(auto.arima(denton$values), h = 20))
#Errores
residuals(auto.arima(denton$values)) %>% plot() #Denton
#Estabilidad
plot(auto.arima(denton$values)) #El modelo es estable según el circulo unitario
