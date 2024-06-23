# Cambio del directorio de trabajo al directorio donde todos los scripts están alojados
library(here)
setwd(here())

# Lectura de los datos desde la ruta relativa
library(dplyr)
oro = read.csv('Datos/Serie histórica diaria por año (1994 - 2024).csv')
oro = oro %>% rename(Fecha = Fecha..DD.MM.AAAA.)
oro = oro %>% 
  filter(Metal == "Oro", Métrica == "Precio de Venta") %>%
  arrange(Fecha) %>%
  select(Fecha, Valor)
oro$Fecha = as.Date(oro$Fecha)
oro = oro[oro$Fecha >= as.Date('2004-01-01'),]

# Conversión a objeto de tipo ts
oro = ts(oro$Valor, start = 2004, frequency = 365)/1000

# Gráfica de la serie:
plot.ts(oro, col = 'darkblue',
        main = 'Histórico precio de venta gramo oro',
        ylab = 'Precio en miles de pesos',
        xlab = 'Tiempo')


# Guardado de la serie de tiempo como serie de tiempo 
saveRDS(leche, 'Datos/Oro.rds')


