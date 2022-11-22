library(corrplot)
library(psych)

### Conociendo la base para el TP ####

## Uso de una base de datos abierta de 
 # Spreng, R. N. (2022, September 14). Goal-Directed Cognition in Older and Younger Adults. https://doi.org/10.17605/OSF.IO/YHZXE
 # Spreng, R.N., Setton, R., Alter, U. et al. Neurocognitive aging data release with behavioral, structural and multi-echo functional MRI measures. Sci Data 9, 119 (2022). https://doi.org/10.1038/s41597-022-01231-7
datos <- read.csv2("https://osf.io/download/huj8p/",
                    sep = ",", header = TRUE, dec = ".")
# El data set contiene 301 observaciones y 71 variables

# Selección de variables para el análisis: datos de participantes y variables de interés
colnames(datos)
datos_selec <- datos[,-c(1:9, 12:31, 37:50, 55,57,59)]

# Estandarización de los datos: todas las variables, excepto algunas cognitivas, están expresadas en puntaje burto. Se estandarizan para haceras comparables.
# Las variables 37 a 53 están en puntaje estandarizado T. Los paso a Z
estandarizacionT <- function(x){
  (x-50)/10
}
datos_selec[12:25] <- apply(datos_selec[12:25], 2, estandarizacionT)
# El resto están en puntaje bruto. Estandarizo en función de su media y DE
estandarizacion <- function(x){
  (x-mean(x, na.rm = TRUE))/sd(x, na.rm=TRUE)
}
datos_selec[1:11] <- apply(datos_selec[1:11], 2, estandarizacion)

#Unificación de variable depresión
datos_selec$depre <- ifelse(is.na(datos_selec$gds_sum),
                          datos_selec$bdi_sum,
                          datos_selec$gds_sum)
datos.pca <- datos_selec[,-c(1,2)] # Elimino bdi y gds

#Elimino los datos faltantes
datos.pca <- na.omit(datos.pca)

#Cambio los nombres de columnas a castellano
names(datos.pca) <- c("P_apertura", "P_responsabilidad","P_extraversion","P_amabilidad","P_neuroticismo",
                         "IRI_toma_perspectiva","IRI_fantasia","IRI_preocupacion_empatica","IRI_malestar_personal",
                         "E_ostilidad_desconfianza","E_apoyo_emocional","E_miedo_activacion","E_amistad",
                         "E_satisfaccion_de_vida","E_apoyo_instrumental","E_soledad","E_proposito","E_hostilidad_percibida",
                         "E_rechazo_percibido","E_estres_percibido","E_afecto_positivo","E_tristeza","E_autoeficacia","Depresion")

#Correlaciones
cortest.bartlett(cor(datos.pca), n = nrow(datos.pca))


corrplot(cor(datos.pca), method = "ellipse", type = "upper")

cp <- princomp(datos.pca)
summary(cp)
screeplot(cp)
cp$loadings
