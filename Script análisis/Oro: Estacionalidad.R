# Cambio a la ruta en que se encuentra el archivo
library(here)
setwd(here())

# Lectura de los datos sin tendencia kernel
source('Script análisis/Oro: Eliminación de la tendencia.R')
# rm(list = ls()[ls() != 'leche'])
# == Tenga en cuenta que, para cualquier análisis de la estacionalidad es 
# == necesario eliminar la tendencia de la serie primero por eso cargamos los datos
# == con la tendencia eliminada

# Análisis descriptivo para determinar la frecuencia

# = ACF
pacf(diffOro, lag.max = 60, main = 'Serie precio gramo de oro (Diaria)')
mtext(bquote(bold('Datos diferenciados a un rezago')), side = 3, 
      line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')

# = Periodograma
library(latex2exp)
SpectrumOro = spectrum(diffOro, log = 'no', main = 'Periodogram')
mtext(TeX('Frequency $\\times$ 365', bold = T),
      side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')

frecuencias = SpectrumOro$freq
Periodograma = SpectrumOro$spec

for (i in 1:3){
  ind = which.max(Periodograma)
  cat('El valor máximo no.', i, ' en el que se máximiza el periodograma es', Periodograma[ind], '\nen la frecuencia ', frecuencias[ind],'\n')
  abline(v = frecuencias[ind], col = 'darkred', lty = 2)
  Periodograma = Periodograma[-ind]
  frecuencias = frecuencias[-ind]
}


# == Note que las frecuencias en que se maximiza el periodograma son las
# == correspondiente a un año, seis meses y 5 años que son armónicos todos

# Eliminación de la componente estacional mediante variables dummy
# rm(list = ls()[ls() != 'leche'])
Dummy = lm(as.numeric(leche) ~ as.factor(rep(1:12, 16)))

# Gráfica de la serie
par(mfrow = c(2,1))

plot.ts(leche, ylab = 'Volumen', xlab = 'Tiempo',
        main = 'Volumen de leche producido en Colombia')
mtext(bquote(bold('En millones de litros')), side = 2, line = 2, adj = 0.5,
      cex = 0.8, col = 'darkgray')
lines(x = time(leche)[1:192], y = Dummy$fitted.values, col = 'darkred')
abline(v = 2008:2024, col = 'gray', lty = 'dashed', lwd = 1.5)

plot(x = time(leche)[1:192],y = Dummy$residuals, ylab = 'Volumen', xlab = 'Tiempo',
        main = 'Volumen de leche producido en Colombia', type = 'l')
mtext(bquote(bold('Sin tendencia y desestacionalizada')), side = 1, line = -17, 
      adj = 0.5, cex = 0.8, col = 'darkgray')
abline(v = 2008:2024, col = 'gray', lty = 'dashed', lwd = 1.5)


par(mfrow = c(1,1))
