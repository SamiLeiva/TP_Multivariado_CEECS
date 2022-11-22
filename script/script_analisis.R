library(corrplot)
library(psych)

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
datos_selec <- datos[,-c(1:9, 12:31, 37:50, 55,57,59)]

#######################
### Estandarización ###
#######################

# Todas las variables, excepto algunas, están expresadas en puntaje burto. Se estandarizan para hacerlas comparables para el análisis de componentes principales.
# Las variables 12 a 25 están expresadas en puntaje T. Los paso a Z.
estandarizacionT <- function(x){
  (x-50)/10
}
datos_selec[12:25] <- apply(datos_selec[12:25], 2, estandarizacionT)
# El resto están expresadas en puntaje bruto. Estandarizo en función de su media y DE
estandarizacion <- function(x){
  (x-mean(x, na.rm = TRUE))/sd(x, na.rm=TRUE)
}
datos_selec[1:11] <- apply(datos_selec[1:11], 2, estandarizacion)

#################################################
### Puesta a punto final antes de aplicar PCA ###
#################################################

# Unificación de variable depresión: la sintomatología depresiva se evaluó con dos escalas diferentes (una aplicada a jovenes y otra a personas de mayor edad).
# Dado que cada individuo tiene un solo valor para depresión (BDI o GDS), se unifica en una sola variable llamada "depre".
datos_selec$depre <- ifelse(is.na(datos_selec$gds_sum),
                            datos_selec$bdi_sum,
                            datos_selec$gds_sum)
datos.pca <- datos_selec[,-c(1,2)] # Elimino las columnas de bdi y gds

# Se eliminan a los participantes con datos faltanes ya que PCA puede ser aplicado con datos completos. En este caso y para el TP de la metaria, se decide no hacer imputación de los casos.
datos.pca <- na.omit(datos.pca)

#Cambio los nombres de columnas al castellano
names(datos.pca) <- c("P_apertura", "P_responsabilidad","P_extraversion","P_amabilidad","P_neuroticismo",
                         "IRI_toma_perspectiva","IRI_fantasia","IRI_preocupacion_empatica","IRI_malestar_personal",
                         "E_ostilidad_desconfianza","E_apoyo_emocional","E_miedo_activacion","E_amistad",
                         "E_satisfaccion_de_vida","E_apoyo_instrumental","E_soledad","E_proposito","E_hostilidad_percibida",
                         "E_rechazo_percibido","E_estres_percibido","E_afecto_positivo","E_tristeza","E_autoeficacia","Depresion")


#################################################
### Exploración de asociación entre variables ###
#################################################

# Gráficos de correlaciones entre las variables



corrplot(cor(datos.pca), method = "ellipse", type = "upper")


cortest.bartlett(cor(datos.pca), n = nrow(datos.pca))

cp <- princomp(datos.pca)
summary(cp)
screeplot(cp)
cp$loadings
