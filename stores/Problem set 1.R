##PUNTO 3##
require(tidyverse)
install.packages("stargazer")
require(stargazer)
require(pacman)
##How good is this model in sample fit?
##Para esto debemos hacer la regresión y ver el R-cuadrado
summary(db_imputado$ingtot)
db_imputado<- db_imputado %>% mutate(age2 = age^2)
mod_1<-lm(ingtot~age+age2,db_imputado)
summary(mod_1) 
stargazer(mod_1,type="text",title="Regresión ingresos-edad", out="mod1.txt")

##Plot the predicted age-earnings profile implied by the above equation
plot(x=db_imputado$age, y=predict(mod_1),
     xlab='edad',
     ylab='valores predichos',
     main='Ingreso predicho vs Edad')
lines(sort(db_imputado$age),                 # Draw polynomial regression curve
      fitted(mod_1)[order(db_imputado$age)],
      col = "blue",
      type = "l")

##What is the “peak age” suggested by the above equation? Use bootstrap to
##calculate the standard errors and construct the confidence intervals.
coefs<-mod_1$coef
coefs
b1<-coefs[2]
b2<-coefs[3]

##Para sacar Peak age toca derivar e igualar a 0 (derivada=B1+2B2AGE)
b1
b2
b1+b2
Peakage=(-b1)/(2*b2)
Peakage 

##Usa bootstap para calcular Error estándar y construir IDC
mean_age<-mean(db_imputado$age)
elast_ingreso_edad<-b1+2*b2*mean_age ##para comprobar que el bootstrap esta bien
elast_ingreso_edad

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

require(boot)
results<-boot(data=db_imputado, eta_mod1.fn,R=1000)
results
elast_ingreso_edad

##Intervalos de confianza

boot.ci(results,conf=0.95,
        type = c("norm", "basic",
                 "perc", "bca"))
quantile(results, c(0.025, 0.975))

##PUNTO 4##

##Estimate the unconditional earnings gap
db_imputado<- db_imputado %>% mutate(mujer = ifelse(db_imputado$sex == 0, 1, 0))
db_imputado<-db_imputado %>% mutate(log_ingreso=log(db_imputado$ingtot))
mod_3<-lm(log_ingreso~mujer,db_imputado)
mean(db_imputado$mujer)
str(db_imputado$mujer)

mod_3