# Cambio del directorio de trabajo al directorio donde todos los scripts están alojados
library(here)
setwd(here())

# Lectura de los datos desde la ruta relativa
leche = readRDS('Datos/Leche.rds')

# Cálculo de lambda y gráfica
lambda_leche = MASS::boxcox(object = lm(leche ~ 1), 
                           seq(0, 8, length = 50))


# Busquéda de lambda y aplicación de la transformación
lambda_leche = lambda_leche$x[which.max(lambda_leche$y)]
leche_tran = 1/(lambda_leche) * leche^(lambda_leche) - 1

MASS::boxcox(lm(leche_tran~1), seq(0,2, length = 50))

# Gráfica comparativa de las dos series:

par(mfrow = c(1,2))
plot.ts(leche, xlab = 'Tiempo', ylab = NULL, 
        main = 'Volumen de leche producido en Colombia', 
        col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Datos originales(En millones de litros)')),
      side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
plot.ts(leche_tran, xlab = 'Tiempo', ylab = NULL, 
        main = 'Volumen de leche producido en Colombia',
        col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Datos tranformados por Box-Cox')), 
      side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
par(mfrow = c(1,1))


# == Como la transformación no parece hacer mayor cosa, no se cambia la serie
# == guardada con extensión RDS