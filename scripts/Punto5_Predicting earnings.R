#Solución del Punto 5
#Carga de librerias

require(pacman)
require(stargazer)
library(recepies)
library(MASS)

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
p_load(caret)
p_load (huxtable)
p_load(officer)
p_load(flextable)
p_load(mass)

#Transformaciones para los modelos
db_imputado<- db_imputado %>% mutate(mujer = ifelse(db_imputado$sex == 0, 1, 0))
db_imputado<-db_imputado %>% mutate(log_ingreso=log(db_imputado$ingtot))
db_imputado<- db_imputado %>% mutate(age2 = age^2)

#PUNTO A****************************
#Split the sample into two samples: a training (70%) and a test (30%) sample.
#Don't forget to set a seed (in R, set.seed(10101), where 10101 is the seed.)

set.seed(10101) 
db_imputado <- db_imputado %>%
  mutate(ingtot,
          holdout= as.logical(1:nrow(db_imputado) %in%
         sample(nrow(db_imputado), nrow(db_imputado)*.3))
  )

test<-db_imputado[db_imputado$holdout==T,]
train<-db_imputado[db_imputado$holdout==F,]


#Modelo 1 constante
model1<-lm(ingtot~1,data=train)
summary(model1)
coef(model1)     
mean(train$ingtot)
tabla1<-huxreg("Constante"=model1)
quick_docx(tabla1, file = "D:/OneDrive - Universidad de los Andes/Intersemestral 2/Big Data/Taller1/scripts/tabla_regs.docx")
test$model1<-predict(model1,newdata = test)
with(test,mean((ingtot-model1)^2))

#Modelo 2 - Age

model2<-lm(ingtot~age+age2,data=train)
test$model2<-predict(model2,newdata = test)
with(test,mean((ingtot-model2)^2))


#Modelo 3 - Log Female

model3<-lm(log_ingreso~mujer,data=train)
test$model3<-predict(model3,newdata = test)
with(test,mean((log_ingreso-model3)^2))

tabla2<-huxreg("Age Earning"=model2,"Female"=model3)
quick_docx(tabla2, file = "D:/OneDrive - Universidad de los Andes/Intersemestral 2/Big Data/Taller1/scripts/tabla_regs.docx")

#Modelo 4 - totalHoursWorked

model4<-lm(ingtot~age+age2+totalHoursWorked,data=train)
test$model4<-predict(model4,newdata = test)
with(test,mean((ingtot-model4)^2))

#Modelo 5 - totalHoursWorked + maxEducLevel + mujer  

model5<-lm(ingtot~age+age2+totalHoursWorked+maxEducLevel+mujer,data=train)
test$model5<-predict(model5,newdata = test)
with(test,mean((ingtot-model5)^2))

#Modelo 6 - totalHoursWorked^2 + maxEducLevel + mujer 

model6<-lm(ingtot~age+age2+poly(totalHoursWorked,2)+maxEducLevel+mujer,data=train)
test$model6<-predict(model6,newdata = test)
with(test,mean((ingtot-model6)^2))

#Modelo 7 - totalHoursWorked + interaccion mujer maxEducLeve
model7<-lm(ingtot~age+age2+poly(totalHoursWorked,2)+mujer+maxEducLevel+mujer*maxEducLevel,data=train)
test$model7<-predict(model7,newdata = test)
with(test,mean((ingtot-model7)^2))

#Modelo 8 - age + totalHoursWorked^2 + sex
model8<-lm(ingtot~age+age2+poly(totalHoursWorked,3)+maxEducLevel+mujer+mujer*age,data=train)
test$model8<-predict(model8,newdata = test)
with(test,mean((ingtot-model8)^2))


#Modelo 9 
model9<-lm(ingtot~age+age2+poly(totalHoursWorked,4)+maxEducLevel+sex+sex*age+maxEducLevel*sex+estrato1,data=train)
test$model9<-predict(model9,newdata = test)
with(test,mean((ingtot-model9)^2))


#For the model with the lowest average prediction error, compute the 
#leverage statistic for each observation in the test sample.

model9<-lm(ingtot~age+age2+poly(totalHoursWorked,4)+maxEducLevel+sex+sex*age+maxEducLevel*sex+estrato1,data=test)
hats <- as.data.frame(hatvalues(model9))
hats[order(-hats['hatvalues(model9)']), ]
plot(hatvalues(model9), type = 'h',xlab="Observación", ylab="Leverage static")
stud_resids <- studres(model9)
stud_resids2<-as.data.frame(stud_resids)

alphas <- c()
for (j in 1:nrow(test)) {
  uj[j] <- model9$residual[j]
  hj[j] <- lm.influence(model9)$hat[j]
  print(j)
  alpha <- uj/(1-hj)
  alphas <- c(alphas, alpha)
} 

plot(hatvalues(model9),model9$residual)
plot(hj,uj)
plot(hj,stud_resids, xlab="Leverage(h_j)", ylab= "Studentized_Residuals(u_j)")

#Se revisan outliers
test$ingtot[2156]
predict(model9,newdata=test[2156,])

#Punto B*******************************
#Repeat the previous point but use K-fold cross-validation.

model1 <- train(ingtot~.,
                # model to fit
                data = db_imputado,
                trControl = trainControl(method = "cv", number = 5), method = "null")

model2 <- train(ingtot~age+age2,
                # model to fit
                data = db_imputado,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

model3 <- train(log_ingreso~mujer,
                # model to fit
                data = db_imputado,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

model4 <- train(ingtot~age+age2+totalHoursWorked+maxEducLevel+mujer,
                # model to fit
                data = db_imputado,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

model5 <- train(ingtot~age+age2+poly(totalHoursWorked,2)+maxEducLevel+mujer,
                # model to fit
                data = db_imputado,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

model6 <- train(ingtot~age+age2+poly(totalHoursWorked,2)+mujer+maxEducLevel+mujer*maxEducLevel,
                # model to fit
                data = db_imputado,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

model7 <- train(ingtot~age+age2+poly(totalHoursWorked,3)+mujer+maxEducLevel+mujer*maxEducLevel,
                # model to fit
                data = db_imputado,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

model8 <- train(ingtot~age+age2+poly(totalHoursWorked,4)+maxEducLevel+sex+sex*age+maxEducLevel*sex+estrato1,
                # model to fit
                data = db_imputado,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

#LOOCV. With your preferred predicted model (the one with the lowest average
#prediction error) perform the following exercise:

u<-0

for(i in 1:dim(db_imputado)[1]){
  #Estimacion de la regresion menos la i-th observacion
  reg_1<-lm(ingtot~age+age2+poly(totalHoursWorked,4)+maxEducLevel+sex+sex*age+maxEducLevel*sex+estrato1,db_imputado[-i,])
  #Calculo del error de prediccion para la i - th observation, i.e. (yi - y^i)
  y_hat<-predict(reg_1,newdata=db_imputado[i,])
  print(i)
  #u[1]<-i
  #print(u)
  u[i]<-(db_imputado[i,]$ingtot-y_hat)^2

}

u<-as.data.frame(u)
LOOCV<-mean(u[,1])

#Compare the results to those obtained in the computation of the leverage
#statistic
u2<-u
u2$new <- c(1:dim(u)[1])

u[order(-u['u']), ]
plot(u2[,2],u2[,1], type = 'h', xlab="Observacion", ylab="Error de predicción")

#Atajo Leverage Static!

(reg_1$residuals/(1-lm.influence(reg_1)$hat))^2
sum((reg_1$residuals/(1-lm.influence(reg_1)$hat))^2)
cv<-1/dim(db_imputado)[1]*(sum((reg_1$residuals/(1-lm.influence(reg_1)$hat))^2))
