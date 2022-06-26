##PUNTO 3##

##INSTALAMOS PAQUETES NECESARIOS
install.packages("stargazer")
install.packages("pacman")
install.packages("tidyverse")
require(tidyverse)
require(stargazer)
require(pacman)

##3.1, 3.2 y 3.3 How good is this model in sample fit? 

summary(db_imputado$ingtot) ##ingtot fue la variable escogida 
db_imputado<- db_imputado %>% mutate(age2 = age^2)
mod_1<-lm(ingtot~age+age2,db_imputado)
summary(mod_1) 
stargazer(mod_1,type="text",title="Regresión ingresos-edad", out="mod1_ingresos-edad.txt")
res <- resid(mod_1)

#produce residual vs. fitted plot
plot(fitted(mod_1), res)

#add a horizontal line at 0 
abline(0,0)

##3.4 Plot the predicted age-earnings profile implied by the above equation
plot(x=db_imputado$age, y=predict(mod_1),
     xlab='edad',
     ylab='ingresos predichos',
     main='Ingreso predicho vs Edad')
lines(sort(db_imputado$age),                 # Draw polynomial regression curve
      fitted(mod_1)[order(db_imputado$age)],
      col = "blue",
      type = "l")

##3.5. What is the “peak age” suggested by the above equation? Use bootstrap to calculate the standard errors and construct the confidence intervals.

coefs<-mod_1$coef
coefs
b1<-coefs[2]
b2<-coefs[3]

##Para sacar Peak age toca derivar e igualar a 0 (derivada=B1+2B2AGE)
Peakage=(-b1)/(2*b2)
Peakage ##Edad pico: 57 años

##Usa bootstap para calcular Error estándar y construir IDC
mean_age<-mean(db_imputado$age) ##Para encontrar la elasticidad ingreso-edad y el error estándar vamos a utilizar la edad media de la muestra
elast_ingreso_edad<-b1+2*b2*mean_age ##para comprobar que el bootstrap esta bien
elast_ingreso_edad ##28103.87, esto significa que manteniendo todas las demás variables constantes, en promedio un año más de edad implica que el ingreso aumente en 28.103,7 pesos


eta_mod1.fn<-function(data,index,
                      mean_age=mean(db_imputado$age), 
                      mean_age2=mean(db_imputado$age2)){
  mod_2<-lm(ingtot~age+age2, db_imputado, subset = index)
  coefs<-mod_2$coefficients
  B1<-coefs[2]
  B2<-coefs[3]
  elastpt<-B1+2*B2*mean_age
  return(elastpt)
}

##Hacemos el bootstrap
require(boot)
results<-boot(data=db_imputado, eta_mod1.fn,R=1000)
results
elast_ingreso_edad

##Intervalos de confianza a partir del bootstrap
SE_1<-1449.747*1.96
IDC1_lowerbound<-elast_ingreso_edad-SE_1
IDC1_upperbound<-elast_ingreso_edad+SE_1
print(c(IDC1_lowerbound,IDC1_upperbound)) ##De acuerdo al bootstrap realizado, la elasticidad ingreso-edad puede varias en el intervalo de (25.262,37, 30.945,37)


########  PUNTO 4 #########################

##4.1 Estimate the unconditional earnings gap
db_imputado<- db_imputado %>% mutate(mujer = ifelse(db_imputado$sex == 0, 1, 0))
db_imputado<-db_imputado %>% mutate(log_ingreso=log(db_imputado$ingtot))
mod_3<-lm(log_ingreso~mujer,db_imputado)
mod_3
stargazer(mod_3,type="text",title="Regresión log ingresos-mujer", out="mod3_ingresosvsmujer.txt")
coefs_mod3<-mod_3$coefficients
Coef_mod3<-coefs_mod3[2]
Coef_mod3 ##El coeficiente B2 significa que manteniendo todas las demas variables constantes, en promedio las mujeres ganan 37,8% menos en ingresos que a comparación de los hombres.

##Sample fit
#get list of residuals 
residuales_mod3 <- resid(mod_3)
#producir residual vs. fitted plot
plot(fitted(mod_3), residuales_mod3, main="Residuales vs valores predichos" )
#add a horizontal line at 0 
abline(0,0)

#Q-Q plot for residuals
qqnorm(residuales_mod3)

#add a straight diagonal line to the plot
qqline(residuales_mod3) 

## 4.2 Estimate and plot the predicted age-earnings profile by gender. Do men and women in Bogota have the same intercept and slopes?
mod_4<-lm(log_ingreso~age+age2+mujer,db_imputado)
mod_4
stargazer(mod_4,type="text",title="Regresión log ingresos-edad-genero", out="mod4_logingresos-edad-genero.txt")

db_imputado<- db_imputado %>% mutate(mujer_age = mujer*age)
mod_6<-lm(log_ingreso~age+age2+mujer+mujer_age,db_imputado)
mod_6
stargazer(mod_6,type="text",title="Regresión log ingresos-edad-genero-edad*genero", out="mod6_logingresos-edad-genero-edad*genero.txt")

ggplot(data = db_imputado, aes(x = age, y = predict(mod_6))) +
  geom_point(aes(shape = factor(mujer))) +
  geom_point(aes(color = factor(mujer))) +
  geom_smooth(method = lm(log_ingreso~age+age2+mujer+mujer_age,db_imputado), 
              se = FALSE, 
              aes(color = factor(mujer))) +
  labs(title = "Regresión Log ingreso vs Edad (por genero)",
       x = "Edad",
       y = "Ingreso (log ingreso)")

##What is the implied “peak age” by gender?. Use bootstrap to calculate the standard errors and construct the confidence intervals. Do these confidence intervals overlap?
coefs1<-mod_6$coef
coefs1
b_1<-coefs1[2]
b_2<-coefs1[3]
b_3<-coefs1[4]
b_4<-coefs1[5]

##Para sacar Peak age por genero nos toca derivar e igualar a 0 (derivada=B1+2B2AGE+B4Mujer)
b_1
b_2
b_3
b_4
Peakage_mujer=(-b_1-b_4)/(2*b_2)
Peakage_mujer ##38.57 años
Peakage_hombre=(-b_1)/(2*b_2)
Peakage_hombre ##46.26 años

##4.3 Usa bootstap para calcular Error estándar y construir IDC

##En el caso de las mujeres:
meanage_women<-mean(db_imputado[db_imputado$mujer = 1, "age"]) ##usamos la edad promedio de las mujeres para el bootstrap
by(data = db_imputado$age, INDICES = db_imputado$mujer, FUN = mean)
mean_edad_mujer<-mean(db_imputado$age [db_imputado$mujer==1] , na.rm = TRUE)
mean_edad_hombre<-mean(db_imputado$age [db_imputado$mujer==0] , na.rm = TRUE)
elast_ingreso_edad_mujer<-b_1+2*b_2*mean_edad_mujer+b_4 ##para comprobar que el bootstrap esta bien
elast_ingreso_edad_mujer

eta_mod2.fn<-function(data,index,
                      mean_edad_mujer=mean(db_imputado$age [db_imputado$mujer==1] , na.rm = TRUE), 
                      mean_edad2_mujer=mean(db_imputado$age2 [db_imputado$mujer==1] , na.rm = TRUE)){
  mod_2<-lm(log_ingreso~age+age2+mujer+mujer_age, db_imputado, subset = index)
  coefs<-mod_2$coefficients
  B1<-coefs[2]
  B2<-coefs[3]
  B4<-coefs[5]
  elastpt_edad_mujer<-B1+2*B2*mean_edad_mujer+B4
  return(elastpt_edad_mujer)
}

require(boot)
results_1<-boot(data=db_imputado, eta_mod2.fn,R=1000)
results_1
elast_ingreso_edad_mujer

##En el caso de los hombres:

mean_edad_hombre<-mean(db_imputado$age [db_imputado$mujer==0] , na.rm = TRUE) ##Para el bootstrap usamos la edad promedio de los hombres
mean_edad_hombre
elast_ingreso_edad_hombre<-b_1+2*b_2*mean_edad_hombre ##para comprobar que el bootstrap esta bien
elast_ingreso_edad_hombre

eta_mod3.fn<-function(data,index,
                      mean_edad_hombre=mean(db_imputado$age [db_imputado$mujer==0] , na.rm = TRUE), 
                      mean_edad2_hombre=mean(db_imputado$age2 [db_imputado$mujer==0] , na.rm = TRUE)){
  mod_2<-lm(log_ingreso~age+age2+mujer+mujer_age, db_imputado, subset = index)
  coefs<-mod_2$coefficients
  B1<-coefs[2]
  B2<-coefs[3]
  B4<-coefs[5]
  elastpt_edad_hombre<-B1+2*B2*mean_edad_hombre
  return(elastpt_edad_hombre)
}

require(boot)
results_2<-boot(data=db_imputado, eta_mod3.fn,R=1000)
results_2
elast_ingreso_edad_hombre

##Intervalos de confianza
##Mujeres:
SE_2<-0.002177814*1.96
IDC2_lowerbound<-elast_ingreso_edad_mujer-SE_2
IDC2_upperbound<-elast_ingreso_edad_mujer+SE_2
print(c(IDC2_lowerbound,IDC2_upperbound)) ##Manteniendo las demás variables constantes, en promedio al aumentar un año de edad el ingreso de las mujeres varia en el intervalo de (-0.5706096%,  0.2830935%)

##Hombres:
SE_3<-0.001520186*1.96
IDC3_lowerbound<-elast_ingreso_edad_hombre-SE_3
IDC3_upperbound<-elast_ingreso_edad_hombre+SE_3
print(c(IDC3_lowerbound,IDC3_upperbound)) ##Manteniendo las demás variables constantes, en promedio al aumentar un año de edad el ingreso de los hombres varia en el intervalo de (1.117025%, 1.712938%)

##4.4 Equal Pay for Equal Work? A common slogan is “equal pay for equal work”.One way to interpret this is that for employees with similar worker and job
##characteristics, no gender earnings gap should exist. Estimate a conditional earnings gap that incorporates control variables such as similar worker and job characteristics (X).

##(a) Estimate the conditional earnings gap log(Income) = β1 + β2 Female + θX + u

summary(db_imputado$oficio) ##Utilizamos la variable de control oficio al representar los respectivos trabajos de cada individuo de la muestra

mod_7<-lm(log_ingreso~ mujer+oficio, db_imputado)
mod_7
stargazer(mod_7,type="text",title="Regresión log ingresos-mujer-oficio", out="mod7_logingresos-mujer-oficio.txt")

## (b) Use FWL to repeat the above estimation, where the interest lies on β2. Do you obtain the same estimates?

##USAMOS RESIDUALES PARA ENCONTRAR COEFICIENTE CON FWL
r1 = residuals(lm(log_ingreso~ oficio, db_imputado))
r2 = residuals (lm(mujer~oficio, db_imputado))
# ols
coef(lm(log_ingreso~ mujer+oficio, db_imputado))
# FWL ols
coef(lm(r1 ~ -1 + r2))

## (c) How should we interpret the β2 coefficient? How good is this model in sample fit? Is the gap reduced? Is this evidence that the gap is a selection problem and not a ”discrimination problem”?

mod_7
coefs_mod7<-mod_7$coefficients
mod7_B1<-coefs_mod7[2]
mod7_B1 ##El coeficiente se interpreta como que manteniendo las demás variables constantes, el log ingreso de las mujeres es en promedio 35,8% menor al de los hombres.

##Sample fit
#get list of residuals 
residuales_mod7 <- resid(mod_7)
#producir residual vs. fitted plot
plot(fitted(mod_7), residuales_mod7, main="Residuales vs valores predichos" )
#add a horizontal line at 0 
abline(0,0)

#Q-Q plot for residuals
qqnorm(residuales_mod7)

#add a straight diagonal line to the plot
qqline(residuales_mod7) 
## A comparación del mod_3, en el mod_7 se disminuye ligeramente la brecha entre los ingresos por genero. 
## Esto podría considerarse como que la brecha de ingresos entre genero si puede relacionarse con un problema de selección de las variables explicativas del modelo, pero así mismo también es un problema de discriminación porque en todas las regresiones realizadas las mujeres tienen un menor salario.

