#Simule una muestra aleatoria de una  
#N(18,3)y calcule un intervalo de confianza unilateral superior del  
#90% para la media.
x <- rnorm(50, mean = 18, sd =3)
res <- t.test(x, alternative = "greater", conf.level = 0.90)
res$conf.int

#FUNCION VAR.TEST
if (!require('devtools')) install.packages('devtools')
devtools::install_github('fhernanb/stests', force=TRUE)
library(stests)

stats::var.test()   # Para usar la función var.test del paquete stats
stests::var.test()  # Para usar la función var.test del paquete stests


#Intervalo de confianza bilateral para la varianza
require(stests)  # Para cargar el paquete
res <- stests::var.test(x=hombres$altura, conf.level=0.98)
res$conf.int

#Intervalo de confianza bilateral para la razón de varianzas
res1 <- stests::var.test(x=hombres$altura, y=mujeres$altura, conf.level=0.95)
res1$conf.int

res2 <- stats::var.test(x=hombres$altura, y=mujeres$altura, conf.level=0.95)
res2$conf.int

#Función prop.test
prop.test(x, n, p=NULL,
          alternative=c("two.sided", "less", "greater"),
          conf.level=0.95, correct=TRUE)


intervalo de confianza bilateral para la proporción  p
res <- prop.test(x=275, n=500, conf.level=0.90)
res$conf.int

Intervalo de confianza bilateral para la diferencia de proporciones
res <- prop.test(x=c(75, 80), n=c(1500, 2000), conf.level=0.90)
res$conf.int