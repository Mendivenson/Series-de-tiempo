# Cambio a la ruta en que se encuentra el archivo
library(here)
setwd(here())

# Lectura de los datos de tipo ts
leche = readRDS('Datos/Leche.rds')

# Aplicacion del suavizamiento kernel
par(mfrow = c(1,2))
ModeloKernelLeche = ksmooth(y = as.numeric(leche), x = time(leche), kernel = 'normal',
                bandwidth = 2.5)

# Gráfica de la serie original con la línea de regresión
plot.ts(leche, xlab = 'Tiempo', ylab = NULL, main = 'Volumen de leche producido en Colombia',
        col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Suavizado Kernel')), side = 3, line = 0.5, 
      adj = 0.5, cex = 0.8, col = 'darkgray')
lines(ModeloKernelLeche, col = 'red')

# Gráfica de la serie ajustada (Sin tendencia)
leche = leche - ModeloKernelLeche$y
plot(leche,xlab = 'Tiempo', ylab = NULL, 
     main = 'Volumen de leche producido en Colombia',
     col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Datos sin tendencia')), side = 3, line = 0.5, adj = 0.5, 
      cex = 0.8, col = 'darkgray')

par(mfrow = c(1,1))
