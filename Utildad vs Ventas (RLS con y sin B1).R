#----------------Modelos Lineales----------------------------#
#--------------------Deber 2-------------------------#
#---------------Patricia Guerrero---------#

data <- read.table("data_rls_uti.txt", header = TRUE, dec=",", sep="\t")
#View(data)
str(data)
data
#Realizamos una regresión lineal de los datos
reg<-lm(Utilidad~Ventas,data)
summary(reg)
#las ventas aumentan en una unidad  entonces la utilidad aumenta 0.43
#B1=137.08 Y B2=0.43
#Y=B1+B2X
#RAZON T t1=B1^/ee(B1^)=0.485
# |t1|>tn-2(alfa/2)=t(40-2)(0.025_)=2.024394
qt(0.975,df=38) # valor percentil t student

#en este caso 0.48> 2.024 no se cumple, no rechazamos Ho,asi que puede centrar los datos  ya que beta1 no es significativo
#valor p<0.05 rechazo la hipotesis nula valor p=0.63 solo rechazamos
#Son t2=23.663  aqui si rechazo ya que 23.663>2.024 , rechazo Ho a un 95% de confianza  la variable ventas influye en la utilidad
#R^2  el 93% de los dtos son explicados por la regresion

str(reg)
anova<-aov(reg)
summary(anova)

#df grados de libertad 
#Ho:B2=0 Y H1:B2!=0
#Ho:B2=B3=...
#RECHAZO H0 si F>F(1,n-2_)(alfa) a favor de H1:B2!=0
#F=559
qf(0.95,df1=1,df2=38) #valor percentil F fisher
#4.098172
#RECHAZO Ho Y CONCLUIMOS QUE LA REGRESION ES SIGNIFICATIVA

#intervalos de confianza 
confint(reg,level=0.95)
#solo analizando el intervalo de confianza rechazamos la hipotesis nula de beta 2,  a un 95% de confianza
names(reg)
str(reg[["residuals"]])
res<-reg[["residuals"]]
prediccion<-reg[["fitted.values"]]
data2<-data.frame(data,prediccion,res)
data2
#View(data2)

#fitted values son las predicciones y^

#análisis de los residuos
hist(res,15)
mean(res)

#Prueba de normalidad
qqnorm(res)
qqline(res,col="red")

#Gráficas
plot(data[,"Ventas"],data[,"Utilidad"])
plot(res,prediccion)

---------------Regresion sin B1-----------------------------------------------------------------
# como vimos gracias a las pruebas de hipotesis NO RECHAZAMOS Ho, beta1 no es significativo por lo tanto centro los datos
#Centramos los datos

media_u<-mean(data[,"Utilidad"])
media_v<-mean(data[,"Ventas"])

utilidad_1<-data[,"Utilidad"]-media_u
ventas_1<-data[,"Ventas"]-media_v
data1<-data.frame (Utilidad=utilidad_1,Ventas=ventas_1) 
#View(data1)
str(data)

#Regresión
reg1<-lm(Utilidad~Ventas,data1)
summary(reg1)
#B1=0 Y B2=0.4399
#Y=B2X
str(reg1)
anova1<-aov(reg1)
summary(anova1)

#Ho:B2=0 Y H1:B2!=0
#Rechazo Ho si F>F(1,n-2_)(alfa) a favor de H1:B2!=0
#F=559

#Valor percentil
qt(0.975 , df =38) 
qf(0.95 , df1=1,df2=38)
#F(1,n-2)(alfa)=4.0987
#Por lo tanto se rechaza Ho, así que se tiene que B2 es significativo, es decir la utilidad depende linealmmente de las Ventas

#Intervalos de confianza
confint(reg1,level=0.95)

names(reg1)
res1<-reg1[["residuals"]]
prediccion1<-reg[["fitted.values"]]
data1_2<-data.frame(data1,Predicciones=prediccion1,Residuos=res1)
View(data1_2)

#análisis de los residuos
hist(res1,15) #se asemeja a la gráfica de una distribución normal
mean(res1)

#Prueba de normalidad
qqnorm(res1)
qqline(res1,col="red")

#Gráficas
plot(data1[,"Ventas"],data1[,"Utilidad"])#se ve que tiene tendencia lineal
plot(res1,prediccion1)#no existe evidencia de violación de hipótesis
