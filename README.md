# Bioestadistica-8
# getwd()
# setwd("C:/Users/Laboratorio/Desktop/BE")
# Ejemplo 1
sapos<-read.csv("sapos_tamaño.csv",header=TRUE,sep=";")
sapos
Tatio<-sapos$localidad=="tatio"
Katarpe<-sapos$localidad=="katarpe"
Farellones<-sapos$localidad=="farellones"
boxplot(svl~localidad,data=sapos,ylab="SVL (mm)",main="Tamaño a la metamorfosis en tres poblaciones")
# H0: Los datos se distribuyen normalmente.
# H1: Los datos NO se distribuyen normalmente.
shapiro.test(sapos$svl[Tatio])
shapiro.test(sapos$svl[Katarpe])
shapiro.test(sapos$svl[Farellones])
# H0: La varianza entre las muestras es similar (varianza homogénea u homocedástica)
# H1: La varianza entre las muestras NO es similar (varianza heterogénea o heterocedástica)
bartlett.test(sapos$svl~sapos$localidad)
# se acepta hay homogeneidad de varianza
# H0: El promedio del tamaño a la metamorfosis es igual en las tres poblaciones
# H1: El promedio del tamaño a la metamorfosis NO es igual en las tres poblaciones
# H1: Al menos una de las poblaciones presenta un promedio de tamaño significativamente diferente
ANOVA_01<-aov(sapos$svl~sapos$localidad)
summary(ANOVA_01)
# se rechaza porque al menos uno de los tamaños de la metamorfosis es distinta
Tukey01<-TukeyHSD(ANOVA_01)
Tukey01
# las únicas en las que hay diferencias es en el tatio con las otras
plot(Tukey01,col="red")
par(mfrow=c(2,2))
plot(ANOVA_01)
# Ejemplo 2
vitamina<-read.csv("vitamina.csv", header=TRUE, sep=";")
head(vitamina)
str(vitamina)
is.factor(vitamina$Largo)
is.factor(vitamina$Formato)
is.factor(vitamina$Dosis)
vitamina$Dosis<-factor(vitamina$Dosis,levels= c(0.5,1,2),labels=c("D0.5","D1.0","D2.0"))
head(vitamina)
is.factor(vitamina$Dosis)
table(vitamina$Formato,vitamina$Dosis)
boxplot(vitamina$Largo~vitamina$Formato*vitamina$Dosis,names=c("Dosis 0.5\nAscórbico","Dosis 0.5\nJugo","Dosis 1.0\nAscórbico","Dosis 1.0\nJugo","Dosis 2.0\nAscórbico","Dosis 2.0\nJugo"),ylab="Largo del diente",xlab="Dosis",main="Ingesta Vitamina C vs Largo Incisivos",col=c("lightgreen","bisque"))
# H0: El promedio de crecimiento dental entre las tres dosis es igual.
# H0: El promedio de crecimiento dental entre los dos formatos es igual.
ANOVA_01<-aov(Largo~Formato+Dosis,data=vitamina)
summary(ANOVA_01)
# se rechaza la nula, debido a que los distintos factores alteran el crecimiento del diente
ANOVA_02<-aov(Largo~Formato*Dosis,data=vitamina)
summary(ANOVA_02)
# se rechaza
# se rechaza
# se rechaza
Tukey_02<-TukeyHSD(ANOVA_02)
Tukey_02
par(mar=c(5.1,6,4.1,2.1),mgp=c(3,1,0),las=1)
plot(Tukey_02,col="red",las=1)
par(mfrow=c(2,2))
plot(ANOVA_02)
aov.residuos_01<-residuals(object=ANOVA_02)
shapiro.test(x=aov.residuos_01)
# distribución normal
install.packages("car",dep=TRUE)
library(car)
# H0: La varianza entre las muestras es similar (varianza homogénea u homocedástica)
# H1: La varianza entre las muestras NO es similar (varianza heterogénea o heterocedástica)
leveneTest(Largo~Formato*Dosis,data=vitamina)
# se acepta, hay homogeneidad
