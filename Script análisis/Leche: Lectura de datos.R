# Cambio del directorio de trabajo al directorio donde todos los scripts están alojados
library(here)
setwd(here())

# Lectura de los datos desde la ruta relativa
leche = read.csv('Datos/Volumen de Acopio Total  (2008 - 2023).csv')

# Conversión a objeto de tipo ts
leche = ts(leche$NACIONAL, start = 2008, frequency = 12)/1000000

# Gráfica de la serie:
plot.ts(leche, col = 'darkblue',
        main = 'Volumen de leche producido en Colombia',
        ylab = 'Volumen',
        xlab = 'Tiempo')
mtext(bquote(bold('En millones de litros')), side = 2, line = 2, adj = 0.5,
      cex = 0.8, col = 'darkgray')

# Guardado de la serie de tiempo como serie de tiempo 
saveRDS(leche, 'Datos/Leche.rds')


