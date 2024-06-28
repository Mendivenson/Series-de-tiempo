# Cambio a la ruta en que se encuentra el archivo
library(here)
setwd(here())

# Lectura de los datos de tipo ts
source('Script análisis/Oro: Análisis de varianza.R')

# Aplicacion del suavizamiento kernel
diffOro = diff(oro, lag = 1)

# Gráfica de la serie original con la línea de regresión
plot.ts(diffOro, xlab = 'Tiempo', ylab = '', 
        main = 'Precio de venta de gramo de oro',
        col = 'black', cex = 0.5)
mtext(bquote(bold('Serie diferenciada (Un rezago)')), side = 3, line = 0.5, 
      adj = 0.5, cex = 0.8, col = 'darkgray')



