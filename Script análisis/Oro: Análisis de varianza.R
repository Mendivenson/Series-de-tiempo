# Cambio del directorio de trabajo al directorio donde todos los scripts están alojados
library(here)
setwd(here())

# Lectura de los datos desde la ruta relativa
oro = readRDS('Datos/Oro.rds')

# Cálculo de lambda y gráfica
lambda_oro = MASS::boxcox(object = lm(oro ~ 1), 
                           seq(-0.1, 0.1, length = 100))


# Aplicación de la transformación
oro_tran = log(oro)

lambda_oro = MASS::boxcox(object = lm(oro_tran ~ 1), 
                          seq(0, 1.5, length = 100))

# Gráfica comparativa de las dos series:

par(mfrow = c(1,2))
plot.ts(oro, xlab = 'Tiempo', ylab = NULL, 
        main = 'Precio de venta de gramo de oro en Colombia', 
        col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Datos originales(En miles de pesos)')),
      side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
plot.ts(oro_tran, xlab = 'Tiempo', ylab = NULL, 
        main = 'Precio de venta de gramo de oro en Colombia',
        col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Datos tranformados por Box-Cox')), 
      side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
par(mfrow = c(1,1))

oro = oro_tran

# == Como la transformación pareció surtir efecto en la serie, trabajamos con 
# == esta serie por lo que si necesitamos la serie transformada será necesario
# == llamar con source() este script.