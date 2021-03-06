#En este script se presentan la manera en que se obtuvieron las estadisticas descriptivas
#y gr�ficas para analizar las variables

#Carga de librerias
require(pacman)
library(recepies)
## p_load llama/instala-llama las librer�as que se enlistan:
p_load(tidyverse, # contiene las librer�as ggplot, dplyr...
       rvest)# web-scraping

#rm(list = ls())

# usar la funci�n p_load de pacman para instalar/llamar las librer�as de la clase
p_load(rio) # Librer�a para importar datos 
p_load(tidyverse) # Librer�a para limpiar datos
p_load(e1071) # Tiene la funci�n para calcular skewness
p_load(EnvStats) # Transformaci�n Box-Cox
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librer�a para visualizar datos
p_load(scales) # Formato de los ejes en las gr�ficas
p_load(ggpubr) # Combinar gr�ficas
p_load(knitr) # Tablas dentro de Rmarkdown
p_load(kableExtra) # Tablas dentro de Rmarkdown
p_load(recepies)
p_load(caret)

#Se obtenienen las estadisiticas descriptivas y gr�ficas 
#para cada una de las variables de inter�s
#ingtot
summary(db_imputado$ingtot)
descriptivos_ingtotal <- summary(db_imputado$ingtot)
histogram(db_imputado$ingtot)
descr_ingtotal <- as.data.frame(rbind(names(descriptivos_ingtotal), as.numeric(descriptivos_ingtotal)))
View(descr_ingtotal)
write.csv2(descr_ingtotal, file = "descr_ingtotal.csv")
#edad
summary(db2$age)
descriptivos_age <- summary(db_imputado$age)
descr_age <- as.data.frame(rbind(names(descriptivos_age), as.numeric(descriptivos_age)))
View(descr_age)
write.csv2(descr_age, file = "descr_age.csv")
#sex
summary(db2$sex)
histogram(db2$sex)
#oficio
summary(db2$oficio)
#totalHoursWorked
summary(db_imputado$totalHoursWorked)
descriptivos_totalHoursWorked <- summary(db_imputado$totalHoursWorked)
descr_age <- as.data.frame(rbind(names(descriptivos_totalHoursWorked ), as.numeric(descriptivos_totalHoursWorked )))
View(totalHoursWorked)
write.csv2(descr_age, file = "descr_totalHoursWorked.csv")



#Graficas para las variables categoricas 

#Gr�fica de dispersi�n para totalHoursWorked vs ingtot

ggplot(db_imputado, aes(x = totalHoursWorked, y = ingtot)) +
  geom_point(color = "darkblue", alpha = 0.5) +
  theme_classic() +
  scale_y_continuous(labels = scales::dollar) +
  # scale_y_continuous(labels = scales::dollar, trans = 'log10') +
  labs(x = "Horas trabajadas", y = "Ingresos totales")

ggplot(db2, aes(x = totalHoursWorked, y = ingtot)) +
  geom_point(color = "darkblue", alpha = 0.5) +
  theme_classic() +
  labs(x = "Horas trabajadas", y = "Ingresos totales")

#M�ximo Nivel Educativo

ggplot() +
  geom_bar(data = db_imputado, aes(x = maxEducLevel), 
           fill = "darkred", alpha = 0.5) +
  labs(x = "M�ximo nivel educativo", y = "Frequencia") + 
  theme_classic()


#Estrato

ggplot() +
  geom_bar(data = db_imputado, aes(x = estrato1), 
           fill = "darkred", alpha = 0.5) +
  labs(x = "Estrato", y = "Frequencia") + 
  theme_classic()

#Oficio
ggplot() +
  geom_bar(data = db_imputado, aes(x = oficio), 
           fill = "darkred", alpha = 0.5) +
  labs(x = "Oficio", y = "Frequencia") + 
  theme_classic()

#Estrato
ggplot() +
  geom_bar(data = db_imputado, aes(x = oficio), 
           fill = "darkred", alpha = 0.5) +
  labs(x = "Oficio", y = "Frequencia") + 
  theme_classic()


