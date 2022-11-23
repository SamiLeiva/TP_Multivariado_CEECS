library(psych)
library(tidyverse)
library(corrplot)
library(factoextra)

########################################
### Apertura de la base y selección ####
########################################

## Uso de una base de datos abierta de 
 # Spreng, R. N. (2022, September 14). Goal-Directed Cognition in Older and Younger Adults. https://doi.org/10.17605/OSF.IO/YHZXE
 # Spreng, R.N., Setton, R., Alter, U. et al. Neurocognitive aging data release with behavioral, structural and multi-echo functional MRI measures. Sci Data 9, 119 (2022). https://doi.org/10.1038/s41597-022-01231-7
datos <- read.csv2("https://osf.io/download/huj8p/",
                    sep = ",", header = TRUE, dec = ".")
# El data set contiene 301 observaciones y 71 variables

# Selección de variables para el análisis: datos de participantes y variables de interés
colnames(datos)
datos_selec <- datos[,-c(1,3:6,9, 12:31, 37:50, 55,57,59)]
# Cambio de nombre de las variables a castellano
names(datos_selec) <- c("Genero", "Edad", "Educacion","BDI","GDS","P_apertura", "P_responsabilidad","P_extraversion","P_amabilidad","P_neuroticismo",
                      "IRI_toma_perspectiva","IRI_fantasia","IRI_preocupacion_empatica","IRI_malestar_personal",
                      "E_hostilidad_desconfianza","E_apoyo_emocional","E_miedo_activacion","E_amistad",
                      "E_satisfaccion_de_vida","E_apoyo_instrumental","E_soledad","E_proposito","E_hostilidad_percibida",
                      "E_rechazo_percibido","E_estres_percibido","E_afecto_positivo","E_tristeza","E_autoeficacia")

#######################
### Estandarización ###
#######################

# Todas las variables afectivas, excepto algunas, están expresadas en puntaje burto. Se estandarizan para hacerlas comparables para el análisis de componentes principales.
# Las variables 15 a 28 están expresadas en puntaje T. Los paso a Z.
estandarizacionT <- function(x){
  (x-50)/10
}
datos_selec[15:28] <- apply(datos_selec[15:28], 2, estandarizacionT)
# El resto están expresadas en puntaje bruto. Estandarizo en función de su media y DE
estandarizacion <- function(x){
  (x-mean(x, na.rm = TRUE))/sd(x, na.rm=TRUE)
}
datos_selec[4:14] <- apply(datos_selec[4:14], 2, estandarizacion)

#################################################
### Puesta a punto final antes de aplicar PCA ###
#################################################

# Unificación de variable depresión: la sintomatología depresiva se evaluó con dos escalas diferentes (una aplicada a jovenes y otra a personas de mayor edad).
# Dado que cada individuo tiene un solo valor para depresión (BDI o GDS), se unifica en una sola variable llamada "depre".
datos_selec$Depresion <- ifelse(is.na(datos_selec$GDS),
                            datos_selec$BDI,
                            datos_selec$GDS)
datos_selec <- datos_selec[,-c(4,5)] # Elimino las columnas de bdi y gds

# Se eliminan a los participantes con datos faltanes ya que PCA puede ser aplicado con datos completos. En este caso y para el TP de la metaria, se decide no hacer imputación de los casos.
datos_selec <- na.omit(datos_selec)

# Descripción de la muestra final
datos_selec$Genero <- factor(datos_selec$Genero)
summary(datos_selec[,c("Genero", "Edad", "Educacion")])
sd(datos_selec$Edad)
sd(datos_selec$Educacion)

## Base para PCA
# Base con variables numéricas de interés para aplicar PCA (24 variables)
datos.pca <- datos_selec[-c(1:3)]

# Gráfico de observación de la distribución de las variables
datos.pca%>% pivot_longer(cols = c(names(datos.pca))) %>% 
  ggplot(aes(x=value))+ 
  geom_histogram(bins=25,color="white", fill = "#6C4DB6")+
  facet_wrap(~name, scales='free')+theme_minimal()+
  labs(x="Valor",y="Frecuencia",title="Distribuciones de las variables analizadas")

#################################################
### Exploración de asociación entre variables ###
#################################################

# Gráficos de correlaciones entre las variables
  #(Codigo del gráfico basado en el trabajo de Bartolomé (2021, July 28). Karina Bartolome: Análisis multivariado de características de las viviendas, educación y empleo en Argentina. Retrieved from https://karbartolome-blog.netlify.app/posts/metodos-multivariados/)
    # Matriz de correlaciones de Pearson
    mat.cor <- datos.pca %>% cor()
    # Cálculo de los p-valores de las correlaciones:
    pvalores <- cor.mtest(datos.pca, conf.level = .95)
    # Grafico    
    mat.cor %>% corrplot(method = 'circle', type = "lower", tl.cex = 0.7,
                   order = 'FPC', p.mat =pvalores$p, insig = "pch", tl.col='black',
                   col = c('#AC0110','#CF3D49','#D4767E','#F2B5BA',
                           '#d9f0d3','#a6dba0','#5aae61','#1b7837'))
    
# Testeo formal de factorización de la matriz mediante test de Bartlett
cortest.bartlett(cor(datos.pca), n = nrow(datos.pca), diag = TRUE)

##################################################
### Reducción de dimensiones mediante PCA #######
#################################################
#Aplicación de componentes principales
cp <- princomp(datos.pca)
summary(cp)
#Screeplot
fviz_screeplot(cp, addlabels = TRUE)
#Numero optimo de componentes
paran::paran(datos.pca, graph=FALSE, status=FALSE, all=FALSE)

#Cargas factoriales
cp$loadings
# Las guardo en un dataframe
cargas <-  loadings(cp)
cargas <- as.data.frame.matrix(cargas)
#Gráfico de cargas factoriales
cargas %>% 
  tibble::rownames_to_column(var="variable") %>% 
  pivot_longer(cols = -variable) %>% 
  filter(name %in% c("Comp.1","Comp.2","Comp.3")) %>% 
  group_by(name) %>% 
  slice_max(abs(value), n=13) %>% 
  ungroup() %>%
  ggplot(aes(x=variable,
             y=value))+
  geom_col(alpha=0.8, fill = "#4DB693")+ 
  coord_flip()+
  facet_wrap(~name, scales='free')+ theme_minimal()+
  labs(x='Variable',y='Carga factorial', 
       title='Carga factorial: componentes principales', 
       subtitle='Primeros 3 componentes (13 cargas más relevantes)')

#Gráfico biplot
fviz_pca_var(cp, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

#Obtención de scores
scores <- cp$scores
head(scores)

fviz_pca_ind(cp, col.ind = "cos",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Mostrando la falta de asociación entre las componentes**
corrplot(cor(scores), type = "lower", tl.cex = 0.7, tl.col='black')

