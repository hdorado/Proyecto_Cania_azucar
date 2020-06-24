
# Lectura de la base de datos.

base_datos <- read.table('Datos/wpbc.data',sep=',',row.names = 1)

base_datos$V2

mod <- glm(V2~.,data = base_datos,family = binomial() )

table(base_datos$V2,mod$fitted.values)
