url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo'
datos <- read.table(file=url, header=TRUE)
hombres <- datos[datos$sexo=="Hombre", ]
hombres

par(mfrow=c(1, 2))
require(car)  # Debe instalar antes el paquete car
qqPlot(hombres$altura, pch=19,
       main='QQplot para la altura de hombres',
       xlab='Cuantiles teóricos',
       ylab='Cuantiles muestrales')

hist(hombres$altura, freq=TRUE,
     main='Histograma para la altura de hombres',
     xlab='Altura (cm)',
     ylab='Frecuencia')

res <- t.test(x=hombres$altura, conf.level=0.90)
res$conf.int


url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo'
datos <- read.table(file=url, header=TRUE)
hombres <- datos[datos$sexo=="Hombre", ]
mujeres <- datos[datos$sexo=="Mujer", ]

par(mfrow=c(2,2))
require(car)  # Debe instalar antes el paquete car
qqPlot(hombres$altura, pch=19, las=1, main='QQplot altura hombres',
       xlab='Cuantiles teóricos', ylab='Cuantiles muestrales')

hist(hombres$altura, las=1, xlab='Altura', ylab='Frecuencia',
     main='Histograma altura hombres')

qqPlot(mujeres$altura, pch=19, las=1, main='QQplot altura mujeres',
       xlab='Cuantiles teóricos', ylab='Cuantiles muestrales')


hist(mujeres$altura, las=1, xlab='Altura', ylab='Frecuencia',
     main='Histograma altura mujeres')

res <- t.test(x=hombres$altura, y=mujeres$altura,
              paired=FALSE, var.equal=TRUE, conf.level = 0.95)
res$conf.int






Antes   <- c(81, 87, 86, 82, 90, 86, 96, 73,
             74, 75, 72, 80, 66, 72, 56, 82)
Despues <- c(78, 91, 78, 78, 84, 67, 92, 70,
             58, 62, 70, 58, 66, 60, 65, 73)
Diferencia <- Antes - Despues

par(mfrow=c(1,2))
require(car)
qqPlot(Diferencia, pch=19, main='QQplot para Diferencias', las=1, 
       xlab='Cuantiles teóricos', ylab='Cuantiles muestrales')

plot(density(Diferencia), main='Densidad para Diferencias', las=1,
     xlab='Diferencia de tiempo', ylab='Densidad')

res <- t.test(x=Antes, y=Despues, paired=TRUE, conf.level=0.95)
res$conf.int