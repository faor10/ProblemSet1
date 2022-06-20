require(pacman)
library(recepies)
## p_load llama/instala-llama las librerías que se enlistan:
p_load(tidyverse, # contiene las librerías ggplot, dplyr...
       rvest)# web-scraping

#rm(list = ls())

# usar la función p_load de pacman para instalar/llamar las librerías de la clase
p_load(rio) # Librería para importar datos 
p_load(tidyverse) # Librería para limpiar datos
p_load(e1071) # Tiene la función para calcular skewness
p_load(EnvStats) # Transformación Box-Cox
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librería para visualizar datos
p_load(scales) # Formato de los ejes en las gráficas
p_load(ggpubr) # Combinar gráficas
p_load(knitr) # Tablas dentro de Rmarkdown
p_load(kableExtra) # Tablas dentro de Rmarkdown
p_load(recepies)

##Scraping 
i<-0
p1<-0
tablep1<-0

for (i in 1:10) {
  print(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i ,".html"))
  p1[i]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i ,".html")
  tablep1[i]<-read_html(p1[i]) %>% html_table
  }

tablep1[1]

geih<-data.frame()
geih<-rbind(geih,tablep1[1])
geih2<-data.frame()
geih2<-rbind(geih2,tablep1[2])
geih3<-data.frame()
geih3<-rbind(geih3,tablep1[3])
geih4<-data.frame()
geih4<-rbind(geih4,tablep1[4])
geih5<-data.frame()
geih5<-rbind(geih5,tablep1[5])
geih6<-data.frame()
geih6<-rbind(geih6,tablep1[6])
geih7<-data.frame()
geih7<-rbind(geih7,tablep1[7])
geih8<-data.frame()
geih8<-rbind(geih8,tablep1[8])
geih9<-data.frame()
geih9<-rbind(geih9,tablep1[9])
geih10<-data.frame()
geih10<-rbind(geih10,tablep1[10])
geih$X1.3217 <- NULL
geih2$X1.3218 <- NULL
geih3$X1.3218 <- NULL
geih4$X1.3218 <- NULL
geih5$X1.3218 <- NULL
geih6$X1.3218 <- NULL
geih7$X1.3218 <- NULL
geih8$X1.3218 <- NULL
geih9$X1.3217 <- NULL
geih10$X1.3217 <- NULL

rm(db)
db<-rbind(geih,geih2,geih3,geih4,geih5,geih6,geih7,geih8,geih9,geih10)
db



# Utilizando el diccionario, identificamos variables categóricas
# para volverlas a tipo factor
db <- db %>%
  mutate_at(.vars = c(
    "cclasnr11", "cclasnr2", "cclasnr3", "cclasnr4", "cclasnr5",
    "cclasnr6", "cclasnr7", "cclasnr8", "clase", "college",
    "cotPension", "cuentaPropia", "depto", "directorio", "dominio",
    "dsi", "estrato1", "formal", "ina", "inac", "informal",
    "maxEducLevel", "p6050", "microEmpresa", "ocu", "oficio", 
    "orden", "p6090", "p6100", "p6210", "p6210s1", "p6240", "p6510",
    "p6510s2", "p6545", "p6545s2", "p6580", "p6580s2", "p6585s1",
    "p6585s1a2", "p6585s2", "p6585s2a2", "p6585s4", "p6585s4a2",
    "p6590", "p6610", "p6620", "p6630s1", "p6630s2", "p6630s3",
    "p6630s4", "p6630s6", "p6920", "p7040", "p7050", "p7090",
    "p7110", "p7120", "p7140s1", "p7140s2", "p7150", "p7160",
    "p7310", "p7350", "p7422", "p7472", "p7495", "p7500s1",
    "p7500s2", "p7500s3", "p7505", "p7510s1", "p7510s2",
    "p7510s3", "p7510s5", "p7510s6", "p7510s7", "pea", "pet", 
    "regSalud", "relab", "secuencia_p", "sex", "sizeFirm", "wap"),
    .funs = factor)

# Estadísticas descriptivas
summary(db)

cantidad_na <- sapply(db, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(db)

# Porcentaje de observaciones faltantes. 
p <- mean(porcentaje_na[,1])
print(paste0("En promedio el ", round(p*100, 2), "% de las entradas están vacías"))

# Ordenamos de mayor a menor
porcentaje_na <- arrange(porcentaje_na, desc(cantidad_na))
# Convertimos el nombre de la fila en columna
porcentaje_na <- rownames_to_column(porcentaje_na, "variable")

# Quitamos las variables que no tienen NAs
filtro <- porcentaje_na$cantidad_na == 0
variables_sin_na <- porcentaje_na[filtro, "variable"]
variables_sin_na <- paste(variables_sin_na, collapse = ", ")
print(paste("Las variables sin NAs son:", variables_sin_na))

porcentaje_na <- porcentaje_na[!filtro,]

orden <- porcentaje_na$variable[length(porcentaje_na$variable):1]
porcentaje_na$variable <- factor(porcentaje_na$variable,
                                 levels = orden)

# Como son tantas variables vamos a hacer 3 gráficas
ggplot(porcentaje_na[1:50,], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))


ggplot(porcentaje_na[51:100,], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

ggplot(porcentaje_na[101:nrow(porcentaje_na),], 
       aes(y = variable, x = cantidad_na)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  geom_text(aes(label = paste0(round(100*cantidad_na, 1), "%")),
            colour = "white", position = "dodge", hjust = 1.3,
            size = 2, fontface = "bold") +
  theme_classic() +
  labs(x = "Porcentaje de NAs", y = "Variables") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1))

filtro <- porcentaje_na$cantidad_na > 0.05
variables_eliminar <- porcentaje_na$variable[filtro]
k0 <- ncol(db)
db2 <- db %>%
  select(-variables_eliminar)
k1 <- ncol(db)
print(paste("Se eliminarion", k0-k1, "variables. Ahora la base tiene", k1, "columnas."))



##Solo personas mayores a 18 años
# based on variable values
db2 <- db[ which(db$age>=18 & db$ocu==1), ]


# Cargamos los datos para crear un pipeline
ingredientes <- recipe(x = db2)

# Variables a imputar
filtro <- sapply(db2, function(x) sum(is.na(x)) > 0)
variables_imputar <- names(db2)[filtro]

# Imputar datos p6210.
# Observemos su distribución. Note que es una variable categorica
# Convertimos la variable p6210 en categorica
db2$p6210 <- factor(db2$p6210)

ggplot(db, aes(x = p6210)) +
  geom_bar(fill = "darkblue") +
  labs(x = "Nivel educativo", y = "Frequencia") + 
  theme_classic()

# Imputamos con la moda
pipeline <- ingredientes %>%
  # Imputamos con la moda
  step_impute_mode(p6210)

# Estimar parametros
parametros <- prep(pipeline, training = db2)

# Transformar datos
db_imputado <- bake(parametros, db2)

# Observemos como se ve la distribucion antes y después de imputar
ggplot() +
  geom_bar(data = db2, aes(x = p6210), 
           fill = "darkblue", alpha = 0.5) +
  geom_bar(data = db_imputado, aes(x = p6210), 
           fill = "darkred", alpha = 0.5) +
  labs(x = "Nivel educativo", y = "Frequencia") + 
  theme_classic()



# Imputar datos maxEducLevel

# Cargamos los datos para crear un pipeline
ingredientes <- recipe(x = db2)

# Variables a imputar
filtro <- sapply(db2, function(x) sum(is.na(x)) > 0)
variables_imputar <- names(db2)[filtro]

# Observemos su distribución. Note que es una variable categorica
# Convertimos la variable p6210 en categorica
db2$maxEducLevel <- factor(db2$maxEducLevel)

ggplot(db2, aes(x = maxEducLevel)) +
  geom_bar(fill = "darkblue") +
  labs(x = "Nivel educativo", y = "Frequencia") + 
  theme_classic()

# Imputamos con la moda
pipeline <- ingredientes %>%
  # Imputamos con la moda
  step_impute_mode(maxEducLevel)

# Estimar parametros
parametros <- prep(pipeline, training = db2)

# Transformar datos
db_imputado <- bake(parametros, db2)

# Observemos como se ve la distribucion antes y después de imputar
ggplot() +
  geom_bar(data = db2, aes(x = maxEducLevel), 
           fill = "darkblue", alpha = 0.5) +
  geom_bar(data = db_imputado, aes(x = maxEducLevel), 
           fill = "darkred", alpha = 0.5) +
  labs(x = "Total horas trabajadas", y = "Frequencia") + 
  theme_classic()


#Datos atipicos
ggplot(db2, aes(x = totalHoursWorked, y = ingtot)) +
  geom_point(color = "darkblue", alpha = 0.5) +
  theme_classic() +
  scale_y_continuous(labels = scales::dollar) +
  # scale_y_continuous(labels = scales::dollar, trans = 'log10') +
  labs(x = "Horas trabajadas", y = "Ingresos totales (Escala log)")

ggplot(db2, aes(y = totalHoursWorked)) +
  geom_boxplot(fill = "darkblue", alpha = 0.5) +
  theme_classic() +
  labs(y = "Horas trabajadas") +
  scale_x_discrete( )

ggplot(db2, aes(y = ingtot)) +
  geom_boxplot(fill = "darkblue", alpha = 0.5) +
  theme_classic() +
  labs(y = "Ingresos totales") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_discrete( ) 


# Cargamos los datos para crear un pipeline
ingredientes <- recipe(x = db2)

pipeline <- ingredientes %>%
  # Volvemos media 0
  step_center(totalHoursWorked, ingtot) %>% 
  # Volvemos desv. est. 1
  step_scale(totalHoursWorked, ingtot) %>% 
  # Hacemos transformación spatial sign
  step_spatialsign(totalHoursWorked, ingtot) 

# Estimar parametros
parametros <- prep(pipeline, training = db2)

# Transformar datos
db2 <- bake(parametros, db2)

ggplot(db2, aes(x = totalHoursWorked, y = ingtot)) +
  geom_point(color = "darkblue", alpha = 0.5) +
  theme_classic() +
  labs(x = "Horas trabajadas", y = "Ingresos totales")
