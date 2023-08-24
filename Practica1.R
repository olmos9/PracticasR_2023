library(ggplot2)
library(readxl)
library(tidyr) 
library(dplyr)

#Alumno: Nahun Antonio Mart??nez Olmos

# Primero cargaremos nuestra base de datos
restaurante <- read_excel('Restaurante, Power BI, Datos.xlsx')
restaurante <- restaurante[!is.na(restaurante$`Tipo de Cliente`),]

# Ahora veremos cuantas filas y cuantas columnas contiene nuestra base de datos
dim(restaurante)
# Como podemos observar, son 30132 filas y 12 columnas las que nuestra base contiene

# Ahora veremos el nombre de las 12 variables que contiene nuestra base de datos
colnames(restaurante)

# A continuaci??n, visualizaremos los 6 primeros registros de nuestra base de datos
head(restaurante)

# Ahora veremos todos los registros
View(restaurante)

# Ahora veremos las caracter??sticas de las 12 variables
str(restaurante)
# C??mo podemos observar se cuenta con 5 variables n??mericas, 5 de tipo texto y 2 de otro tipo

# Acontinuaci??n se mostrar?? un gr??fico de puntos donde se muestra la dependencia 
#de la propina con respecto al precio de un producto
ggplot(data = restaurante)+
  geom_point(mapping = aes(x=Costo, y=Propina))
# Como podemos observar para este restaurante se aprecia que a menor precio de producto
#mayor propina

# En el siguiente gr??fico podemos observar de que los clientes nuevos son los que 
#brindan mejor propina en este restaurante
ggplot(data = restaurante)+
  geom_point(mapping = aes(x=Costo, y=Propina, color=`Tipo de Cliente`))

# El siguiente gr??fico es el mismo que el anterior solo que tambien clasificado
#mediante el tipo de de orden
ggplot(data=restaurante)+
  geom_point(mapping = aes(x=Costo,y=Propina,color=`Tipo de Cliente`))+
  facet_wrap(~Tipo,nrow=1,ncol=3)
# Como podemos observar, las mayores propinas se dan con mayor frecuencia para los
#clientes nuevos que ordenan comida

# Acontibuaci??n se visualizar?? en un gr??fico de barras el numero de ventas en cada categor??a
ggplot(data=restaurante)+
  stat_count(mapping=aes(x=Categoria,fill=Categoria))
# Como podemos observar, las bebidas sin alcohol son las que mas se venden, y esto
#garantiza un ambiente sano en este restaurante.

# Ahora veremos un gr??fico de barras apiladas con respecto al tipo de cliente
ggplot(data=restaurante)+
  geom_bar(mapping = aes(x=Categoria,fill=`Tipo de Cliente`))
# Como podemos observar, claramente los clientes nuevos son los que mas compras realizan
#en este restaurante


#Conclusi??n; se deben mejorar los servicios del restaurante, debido a que los resultados 
#han arrojado que no son muchos los clientes que optan por regresar al restaurante,
#esto se puede confirmar en el siguiente gr??fico

ggplot(data=restaurante,mapping = aes(x=factor(1),fill=`Tipo de Cliente`))+
  geom_bar(position="fill")+
  coord_polar(theta="y")+
  labs(x="",y="")


