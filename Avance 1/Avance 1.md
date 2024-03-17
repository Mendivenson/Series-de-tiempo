# Avance 1. Series de Tiempo Univariadas
## Departamento de Estadística. Facultad de Ciencias
### Universidad Nacional de Colombia
2024-03-15

---
### Autores:

- [Michel Mendivenson Barragán Zabala](mbarraganz@unal.edu.co)
- [Sharon Juliana Zambrano Ortiz](shzambranoo@unal.edu.co)
- [Nicolás Alejandro Quenguan Parra](naquenguanp@unal.edu.co)
- [Aryin Jair Torres Quiñones](artorresq@unal.edu.co)

---
**Nota:** Algunos de los widgets generados en el documento final sólo renderizan en la versión html de forma apropiada, por lo que es mejor revisar la versión de RPubs de este documento.

---

``` r
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)); install.packages('readxl')
leche = readxl::read_xlsx('../Datos/Volumen de Acopio Total  (2008 - 2023).xlsx', 
                          sheet = 'VOLUMEN REGIONAL Y DEPTAL. ', 
                          range = 'B29:AF221')

datos <- read.csv(file = '../Datos/Serie histórica diaria por año (1994 - 2024).csv')
datos <- datos %>% rename(Fecha = Fecha..DD.MM.AAAA.)
datos <- datos %>%
  dplyr::filter(Metal == "Oro", Métrica == "Precio de Venta") %>%
  dplyr::arrange(Fecha) %>%
  dplyr::select(Fecha, Valor)
datos$Fecha <- as.Date(datos$Fecha)
datos$Valor <- as.numeric(gsub(',','.',datos$Valor))
# Tomamos los datos desde el año 2004 hasta la última fecha
datos2 <- datos[datos$Fecha >= as.Date('2004-01-01'),]
```

------------------------------------------------------------------------

## **Los datos**

Las dos series escogidas corresponden a:

- [La producción de leche mensual en Colombia (En el período 2008 a
  2023)](http://uspleche.minagricultura.gov.co/documentos.html)
- [El precio de compra o venta diario del oro por gramo (En el período
  2010 a Febrero de
  2024)](https://www.banrep.gov.co/es/estadisticas/precios-del-dia-para-el-gramo-oro-plata-y-platino)

``` r
# Con frecuencia mensual
ts_leche = ts(leche$NACIONAL, start = c(2008,1), frequency = 12)/1000000

# COn frecuencia diaria
tsventa <- ts(data = datos2$Valor, start = c(2004,1),frequency = 365)
```

------------------------------------------------------------------------

<!-- ========================================== Sección 1 ==========================================  -->

## **Visualización de los datos**

### Volumen mensual de leche producido

``` r
plot.ts(ts_leche, col = 'darkblue',
        main = 'Volumen de leche producido en Colombia',
        # sub = 'En millones de litros',
        ylab = 'Volumen',
        xlab = 'Tiempo')

mtext(bquote(bold('En millones de litros')), side = 2, line = 2, adj = 0.5, cex = 0.8, col = 'darkgray')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/GraficaTSLeche-1.png" style="display: block; margin: auto;" />

No se observan cambios significativos en la varianza, pero sí se observa
cierto grado de tendencia aunque no del todo lineal. Aunque la varianza
no parece necesitar de una estabilización de todas formas se intenta
estabilizar para ver si la serie cambia en algún aspecto importante
luego de la transformación.

### Precio diario del oro en Colombia

``` r
plot(tsventa/1000, main='Histórico precio de venta gramo de oro',ylab='Precio en miles de pesos', xlab='Año')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/GraficaTSOro-1.png" style="display: block; margin: auto;" />

En la gráfica es posible observar que la serie cuenta con componentes de
tendencia y también es posible concluir que la serie cuenta con
heterocedasticidad marginal debido a que el rango de la variable va
aumentando con el tiempo. Sin embargo, a simple vista (Y tal vez debido
a la frecuencia) no es posible observar alguna señal de estacionalidad.

### 

<!-- ========================================== Sección 2 ==========================================  -->

## **Estabilización de la varianza**

### Volumen mensual de leche producido en Colombia

Para estabilizar la varianza, usamos la transformación Box-Cox con el
$\lambda$ calculado en el siguiente código:

``` r
lambda_leche = MASS::boxcox(object = lm(ts_leche ~ 1), seq(0,8, length = 50))
```

<img src="Avance-1--Versión-2-_files/figure-gfm/LambdaLeche-1.png" style="display: block; margin: auto;" />

    ## El valor de lambda calculado es:  3.151515

Note que no hay evidencia que nos diga que $\lambda$ pudiese ser 1 por
lo que intentamos aplicar la transformación a la serie obteniendo ahora
sí un $\lambda$ con posible valor de 1:

``` r
# lambda_leche = 2.25
EstTS_leche = 1/(lambda_leche) * ts_leche^(lambda_leche) - 1

MASS::boxcox(lm(EstTS_leche~1), seq(0,2, length = 50))
```

<img src="Avance-1--Versión-2-_files/figure-gfm/EstabilizarLeche-1.png" style="display: block; margin: auto;" />

Sin embargo, si se revisan las gráficas de la serie y su transformación:

``` r
default_par = par()
par(mfrow = c(1,2))
plot.ts(ts_leche, xlab = 'Tiempo', ylab = NULL, main = 'Volumen de leche producido en Colombia', col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Datos originales(En millones de litros)')), side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
plot.ts(EstTS_leche, xlab = 'Tiempo', ylab = NULL, main = 'Volumen de leche producido en Colombia',
        col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Datos tranformados por Box-Cox')), side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/GraficaLecheEstabilizada-1.png" style="display: block; margin: auto;" />

``` r
par(default_par)
```

Es fácil notar de que la serie con la transformación, en esencia, la
misma serie pero con un aumento importante en la escala por lo que se
decide seguir utilizando los datos sin transformaciones para los
procesos subsiguientes a este.

### Precio diario de venta del oro en Colombia

Según el gráfico anterior, no parece existir una mayor fluctación en el
rango del precio a lo largo del tiempo. Sin embargo se realizará la
prueba de Box-Cox para verificar. Vamos a ver si presenta variación
marginal.

``` r
# Método de Guerrero
 # g <- forecast::BoxCox.lambda(tsventa, method = 'guerrero', lower = -3, upper = );g
# 
# g_tsventa <- g^(-1) * (tsventa^g -1)
# 
# plot(g_tsventa)

# Método de LMV
ll<- forecast::BoxCox.lambda(tsventa, method = 'loglik', lower = -2, upper = 1.5);ll
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## [1] 0

``` r
MASS::boxcox(lm(tsventa ~ 1),seq(-0.2, 0.3, length = 50))
```

<img src="Avance-1--Versión-2-_files/figure-gfm/LambdaOro-1.png" style="display: block; margin: auto;" />

``` r
# ll_tsventa <- ll^(-1) * (tsventa^ll -1)
# ll_tsventa <- BoxCox(ll_tsventa,ll)
ll_tsventa <- log(tsventa)
```

Dado que $\lambda\neq 1$, , entonces si se necesita estabilizar la
varianza. Como el valor de $\lambda = 0$ se realizara la transformación
logaritmo natural.

``` r
# ll_tsventa <- log(tsventa)
plot(ll_tsventa)
```

<img src="Avance-1--Versión-2-_files/figure-gfm/LambdaOro2-1.png" style="display: block; margin: auto;" />

``` r
MASS::boxcox(lm(ll_tsventa ~ 1),seq(-2, 4, length = 50))
```

<img src="Avance-1--Versión-2-_files/figure-gfm/LambdaOro2-2.png" style="display: block; margin: auto;" />

Al realizar una segunda prueba a la serie transformada obtenemos la
confirmación que 1 pertenece al intervalo de confianza para $\lambda$ .
Aunque parece no haber cambios evidentes en la gráfica, procederemos a
trabajar con la serie transformada.

``` r
EstTS_leche = ts_leche
```

### 

------------------------------------------------------------------------

<!-- ========================================== Sección 3 ==========================================  -->

## **Eliminación de la tendencia**

### Volumen mensual de leche producida en Colombia

Usando el filtro de promedios móviles:

``` r
plot(decompose(EstTS_leche))
```

<img src="Avance-1--Versión-2-_files/figure-gfm/MediasMovilesLeche-1.png" style="display: block; margin: auto;" />

Es posible notar que los datos de esta serie presentan una tendencia no
del todo lineal y que además la estacionalidad estimada por medio de
este filtro parece ser la idonéa puesto que la componente residual no
parece mostrar comportamiento estacional. Ahora bien, busquemos otra
forma de ajustar la tendencia.

Aún si la tendencia del filtro de promedios móviles no es totalmente
lineal, podemos intentar estimar la tendencia de la serie por medio de
una regresión lineal:

``` r
par(mfrow = c(1,2))
ModeloLinealLeche = lm(EstTS_leche ~ time(EstTS_leche))

plot.ts(EstTS_leche, xlab = 'Tiempo', ylab = NULL, main = 'Volumen de leche producido en Colombia',
        col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Ajuste con regresión lineal')), side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
abline(ModeloLinealLeche, col = 'red')

# Los residuales
NoTendAjusLinealLeche = EstTS_leche - predict(ModeloLinealLeche)
plot.ts(NoTendAjusLinealLeche,xlab = 'Tiempo', ylab = NULL, main = 'Volumen de leche producido en Colombia',
        col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Datos sin tendencia')), side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/AjusteLinealLeche-1.png" style="display: block; margin: auto;" />

``` r
par(default_par)
```

Si bien por medio de la regresión lineal la serie parece sufrir un
cambio en su “tendencia” general también podría argumentarse que aún
presenta algo de tendencia. Probemos ahora con el método *lowess*
(Regresión del vecino más cercano):

``` r
par(mfrow = c(1,2))
ModeloLowessLeche = lowess(EstTS_leche, f = 0.25)
plot.ts(EstTS_leche, xlab = 'Tiempo', ylab = NULL, main = 'Volumen de leche producido en Colombia',
        col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Ajuste con regresión local o lowess')), side = 3, line = 0.5, 
      adj = 0.5, cex = 0.8, col = 'darkgray')
lines(ModeloLowessLeche, col = 'red')

# Datos sin tendencia
NoTendAjusLowessLeche = EstTS_leche - ModeloLowessLeche$y
plot.ts(NoTendAjusLowessLeche,xlab = 'Tiempo', ylab = NULL, main = 'Volumen de leche producido en Colombia',
        col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Datos sin tendencia')), side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/Regresión lowess-1.png" style="display: block; margin: auto;" />

``` r
par(default_par)
```

Claramente este método estima mucho mejor al tendencia de nuestros
datos. Sin embargo, probemos con el método de *suavizamiento Kernel*:

``` r
par(mfrow = c(1,2))
ModeloKernelLeche = ksmooth(y = as.numeric(EstTS_leche), x = time(EstTS_leche),kernel = 'normal', bandwidth = 2.5)
plot.ts(EstTS_leche, xlab = 'Tiempo', ylab = NULL, main = 'Volumen de leche producido en Colombia',
        col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Suavizado Kernel')), side = 3, line = 0.5, 
      adj = 0.5, cex = 0.8, col = 'darkgray')
lines(ModeloKernelLeche, col = 'red')

# Datos sin tendencia
NoTendAjusKernelLeche = EstTS_leche - ModeloKernelLeche$y
plot(NoTendAjusKernelLeche,xlab = 'Tiempo', ylab = NULL, main = 'Volumen de leche producido en Colombia',
     col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Datos sin tendencia')), side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/SuavizamientoKernelLeche-1.png" style="display: block; margin: auto;" />

``` r
par(default_par)
```

Donde obtenemos un comportamiento muy similar al presentado con el
método *lowess*. Finalmente, podemos aplicar el método *splines*
solamente para estar seguros de que la tendencia demostrada antes sea la
adecuada:

``` r
par(mfrow = c(1,2))

ModeloSplinesLeche = smooth.spline(time(EstTS_leche), EstTS_leche, spar = 0.8)
plot.ts(EstTS_leche, xlab = 'Tiempo', ylab = NULL, main = 'Volumen de leche producido en Colombia',
        col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Suavizado Splines')), side = 3, line = 0.5, 
      adj = 0.5, cex = 0.8, col = 'darkgray')
lines(ModeloSplinesLeche, col = 'red')

# plot(ModeloSplinesLeche, type = 'l')


# Datos sin tendencia
NoTendAjusSplinesLeche = EstTS_leche - ModeloSplinesLeche$y
plot(NoTendAjusSplinesLeche,xlab = 'Tiempo', ylab = NULL, main = 'Volumen de leche producido en Colombia',
     col = 'darkblue', cex = 0.5)
mtext(bquote(bold('Datos sin tendencia')), side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/AjusteSplinesLeche-1.png" style="display: block; margin: auto;" />

``` r
# for (i in 2008:2023){
#   abline(v = i, col = '#8B8970', lty = 'dashed', pch = 2)
# }


par(default_par)
```

Como todos los tres métodos (*Lowess, Kernel y splines*) nos dan
resultados similares, podemos usar cualquiera de los tres resultados
para continuar con la estimación de la estacionalidad. En el caso
particular de este documento, se estará utilizando la serie residual
asociada al método de suavizamiento *Kernel*.

### Precio diario de venta de oro en Colombia

Exploramos los diferentes métodos para estimar la tendencia, entre
ellos, regresión lineal simple, splines, kernel y LOESS, ninguno resulto
ser efectivo, pues las series resultantes sin tendencias no parecían ser
estacionarias en el sentido débil. Por esta razón se utilizó la serie
diferenciada a 1 rezago.

``` r
fit_lltsventa<- lm(ll_tsventa~time(ll_tsventa), na.action=NULL)
plot.ts(ll_tsventa,
        main = "Estimación de la tendencia modelo lineal normal",
        ylab = 'Log(Precio)',
        xlab = 'Años')
abline(fit_lltsventa, col = "red")
```

<img src="Avance-1--Versión-2-_files/figure-gfm/TendenciaLinealOro-1.png" style="display: block; margin: auto;" />

``` r
notenden_lltsventa <- ll_tsventa - predict(fit_lltsventa)
plot(notenden_lltsventa, main='Serie sin tendencia modelo lineal normal',xlab='Año',ylab='Log(precio)')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/TendenciaLinealOro-2.png" style="display: block; margin: auto;" />
La estimación usando el modelo lineal no fue efectiva para explicar la
tendencia en algunos años,2005 al 2006, 2010 al 2012, 2015 al 2017 .

Se usara regresión LOESS para estimar la tendencia.

``` r
library(tsibble)
```

    ## 
    ## Attaching package: 'tsibble'

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, union

``` r
library(timetk)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.4.4     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter()       masks stats::filter()
    ## ✖ lubridate::interval() masks tsibble::interval()
    ## ✖ dplyr::lag()          masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(feasts)
```

    ## Loading required package: fabletools

``` r
library(fable)

df_ll_tsventa<-data.frame(Fecha=datos2$Fecha,ll_tsventa=as.matrix(ll_tsventa))
df_tsventa=data.frame(Fecha=datos2$Fecha,Precioro=as.matrix(datos2$Valor))

str(df_tsventa)
```

    ## 'data.frame':    7352 obs. of  2 variables:
    ##  $ Fecha   : Date, format: "2004-01-01" "2004-01-02" ...
    ##  $ Precioro: num  37157 37086 37086 37086 37311 ...

``` r
tibble_lltsventa <- tibble(df_ll_tsventa)

tsibble_tsventa <- as_tsibble(df_tsventa)
```

    ## Using `Fecha` as index variable.

``` r
tibble_lltsventa%>%timetk::plot_time_series(Fecha, ll_tsventa, 
                   .interactive = TRUE,
                   .plotly_slider = TRUE)
```


``` r
# Ajuste Loess
tibble_lltsventa%>%
  mutate( lltsventa_ajus = smooth_vec(ll_tsventa,span = 0.60, degree = 2) )
```

    ## # A tibble: 7,352 × 3
    ##    Fecha      ll_tsventa lltsventa_ajus
    ##    <date>          <dbl>          <dbl>
    ##  1 2004-01-01       10.5           10.3
    ##  2 2004-01-02       10.5           10.3
    ##  3 2004-01-03       10.5           10.3
    ##  4 2004-01-04       10.5           10.3
    ##  5 2004-01-05       10.5           10.3
    ##  6 2004-01-06       10.6           10.3
    ##  7 2004-01-07       10.5           10.3
    ##  8 2004-01-08       10.5           10.3
    ##  9 2004-01-09       10.5           10.3
    ## 10 2004-01-10       10.5           10.3
    ## # ℹ 7,342 more rows

``` r
# Gráfico estimación LOESS
g1 <- tibble_lltsventa%>%
  mutate(lltsventa_ajus = smooth_vec(ll_tsventa,span = 0.17 , degree = 2))%>%
  ggplot(aes(Fecha, ll_tsventa)) +
    geom_line() +
    geom_line(aes(y = lltsventa_ajus), color = "red") +
    labs(title = 'Estimación de la tendencia por LOESS',
         x = 'Años',
         y = 'Log(precio)')


# Primera llamada a smooth_vec() y almacenamiento de las estimaciones
tibble_lltsventa <- tibble_lltsventa %>%
  mutate(lltsventa_ajus_1 = smooth_vec(ll_tsventa, span = 0.17, degree = 2))

# Segunda llamada a smooth_vec() y almacenamiento de las estimaciones
# tibble_lltsventa <- tibble_lltsventa %>%
#   mutate(lltsventa_ajus_2 = smooth_vec(ll_tsventa, span = 0.12, degree = 2))

 

NoTendLOESS <- ll_tsventa - as.numeric(tibble_lltsventa$lltsventa_ajus_1)

# NoTendLOESS2 <- ll_tsventa - as.numeric(tibble_lltsventa$lltsventa_ajus_2)

plot.ts(NoTendLOESS, main='Serie sin tendencia LOESS',xlab='Año',ylab='Log(precio)')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/TendenciaLOESSOro-2.png" style="display: block; margin: auto;" />

``` r
# plot.ts(NoTendLOESS2, main='Serie sin tendencia LOESS',xlab='Año',ylab='Log(precio)')

acf(NoTendLOESS,lag.max = 60)
```

<img src="Avance-1--Versión-2-_files/figure-gfm/TendenciaLOESSOro-3.png" style="display: block; margin: auto;" />

``` r
# STL

tsibble_tsventa %>%
  model(
    STL(log(Precioro) ~ trend() +
                   season(window = "periodic"),
    robust = TRUE)) %>%
  components() %>%
  autoplot()
```

<img src="Avance-1--Versión-2-_files/figure-gfm/TendenciaLOESSOro-4.png" style="display: block; margin: auto;" />

``` r
modelo_stl <- tsibble_tsventa %>%
  model(STL(log(Precioro) ~ trend() + season(window = "periodic"), robust = TRUE))

# Estimación de la tendencia por STL
componentes <- components(modelo_stl)

NoTendSTL <- ll_tsventa - componentes$trend

plot(NoTendSTL, main='Serie sin tendencia STL',xlab='Año',ylab='Log(precio)')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/TendenciaLOESSOro-5.png" style="display: block; margin: auto;" />
Usando regresión por splines

``` r
# 0.40
Spline_lltsventa <- smooth.spline(x = time(ll_tsventa),y = ll_tsventa,spar = 0.80)

NoTendSPLINE <- ll_tsventa - Spline_lltsventa$y

plot(ll_tsventa, main='Estimación de la tendencia por splines', ylab='log(precio)',xlab='Año')
lines(x = Spline_lltsventa$x, y=Spline_lltsventa$y, col = 'red')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/TendenciaSplinesOro-1.png" style="display: block; margin: auto;" />

``` r
plot(NoTendSPLINE, main='Serie sin tendencia SPLINES',xlab='Año',ylab='Log(precio)')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/TendenciaSplinesOro-2.png" style="display: block; margin: auto;" />

``` r
# acf(NoTendSPLINE,lag.max = 60)
```

Usando regresión Kernel

``` r
ker_lltsventa <- ksmooth(x = time(ll_tsventa),y =ll_tsventa,kernel = 'normal',bandwidth = 0.49 )
plot(ll_tsventa)
lines(x = ker_lltsventa$x,y = ker_lltsventa$y,col='red')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/TendKernelOro-1.png" style="display: block; margin: auto;" />

``` r
NoTendKer <- ll_tsventa - ker_lltsventa$y

# acf(NoTendKer,lag.max = 60)

plot(NoTendKer ,main='Serie sin tendencia Kernel',xlab='Año',ylab='Log(precio)')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/TendKernelOro-2.png" style="display: block; margin: auto;" />

Se realiza el test de Dickey - Fuller para verificar la presencia de
raíces unitarias, lo cual implica que la serie no es estacionaria;
características como la media y varianza pueden cambiar con el tiempo.
La presencia de una raíz unitaria sugiere que la tendencia de la serie
es estocástica y no determinística.

Utilizamos la prueda de Dickey-Fuller aumentada para detectar la
presencia de raíces unitarias

``` r
# Serie diferenciada
dif_ll_tsventa <- diff(ll_tsventa,lag = 1)
# Modelo autoregresivo a 1 rezago
ar(ll_tsventa)
```

    ## 
    ## Call:
    ## ar(x = ll_tsventa)
    ## 
    ## Coefficients:
    ##      1  
    ## 0.9995  
    ## 
    ## Order selected 1  sigma^2 estimated as  0.0003607

``` r
# Prueba de Dickey-Fuller
tseries::adf.test(ll_tsventa,k=3)
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  ll_tsventa
    ## Dickey-Fuller = -3.1866, Lag order = 3, p-value = 0.09012
    ## alternative hypothesis: stationary

``` r
# Modelo autoregresivo y prueba a la serie diferenciada
ar(dif_ll_tsventa)
```

    ## 
    ## Call:
    ## ar(x = dif_ll_tsventa)
    ## 
    ## 
    ## Order selected 0  sigma^2 estimated as  0.0001114

``` r
tseries::adf.test(dif_ll_tsventa)
```

    ## Warning in tseries::adf.test(dif_ll_tsventa): p-value smaller than printed
    ## p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  dif_ll_tsventa
    ## Dickey-Fuller = -18.839, Lag order = 19, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
plot(dif_ll_tsventa)
```

<img src="Avance-1--Versión-2-_files/figure-gfm/SerieDiffOro-1.png" style="display: block; margin: auto;" />

Se ajustó un modelo auto-regresivo a la serie con varianza estabilizada,
como el coeficiente $\phi=0.9995$ para el primer rezago, es
característica de que cada valor de la serie está muy correlacionado con
su valor inmediatamente anterior. Se realiza el test de Dickey - Fuller,
y con una significancia del 5% no se rechaza la hipótesis nula; la serie
no es estacionaria. Por lo tanto se procede a diferenciar la serie a 1
rezago, y se vuelve a aplicar el modelo y la prueba.

### 

------------------------------------------------------------------------

<!-- ========================================== Sección 4 ==========================================  -->

## **Detección de la estacionalidad**

### Volumen mensual de leche producida en Colombia

Note que la serie demuestra cierto comportamiento cíclico con periodo
anual, es decir, todos los mismos meses tiene el mismo comportamiento,
siendo un comportamiento más obvio en el mes de Febrero:

``` r
# par(mfrow = c(2,1))
# Datos sin tendencia

NoTendLeche = NoTendAjusKernelLeche
par(cex.axis = 1, lwd = 1.5, mar = c(4,4,0.5,0.5))
plot.ts(NoTendLeche,xlab = 'Tiempo', ylab = NULL, main = NULL,
        col = 'darkblue', las = 2)#, xlim = c(2008,2016))
#mtext(bquote(bold('Datos sin tendencia')), side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
for (i in 2008:2024){
  abline(v = i, col = '#8B8970', lty = 'dashed', pch = 2)
}

axis(1, at = 2008:2024, labels = 2008:2024,las = 2)
# plot(NoTendAjusSplinesLeche,xlab = 'Tiempo', ylab = NULL, main = NULL,
#      col = 'darkblue', cex = 0.5, xlim = c(2016,2023))
# for (i in 2008:2023){
#   abline(v = i, col = '#8B8970', lty = 'dashed', pch = 2)
par(lwd = 1)
for (i in time(NoTendLeche)){
  abline(v = i, col = '#CDC9A5', lty = 4)
}
```

<img src="Avance-1--Versión-2-_files/figure-gfm/SerieMesesDivididos-1.png" style="display: block; margin: auto;" />

``` r
par(default_par)
```

Si bien no es del todo perceptible a simple vista, podemos notar que,
por ejemplo en los meses de Febrero siempre se registran los picos más
bajos de la serie en ese año mientras que en los meses de Junio o Julio
se presenta los valores más altos de la serie. Es decir, podemos más o
menos intuir que habrá un período anual. Ahora bien, revisemos el acf de
la serie:

``` r
acf(NoTendLeche, lag.max = 56, main = 'Serie volumen de leche mensual')
mtext(bquote(bold('Datos sin tendencia ajustados con kernel')), side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/acfMuestralLeche-1.png" style="display: block; margin: auto;" />
Nuevamente, el acf nos sugiere un componente estacional. Indaguemos más
sobre la estacionalidad anual de la serie:

``` r
df = as.data.frame(NoTendLeche)
colnames(df) = c('Volume')
month = as.numeric(rownames(df)) %% 12
month[month == 0] = 12
df$'Month' = month.name[month]

SerieDataframe = data.frame(row.names = month.name[1:12])
for (i in month.name[1:12]){
  SerieDataframe[i,1:16] = df$Volume[df$Month == i]
}

rm(df)
SerieDataframe = t(SerieDataframe)
rownames(SerieDataframe) = 2008:2023
boxplot(SerieDataframe,las = 2, cex.axis = 0.8, main = 'Boxplot mensual (Volumen de leche)')
mtext(bquote(bold('Diágnostico de estacionalidad')),
      side = 3, line = 0.3, adj = 0.5, cex = 0.8, col = 'darkgray')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/MonthlyGraphsLeche-1.png" style="display: block; margin: auto;" />

El gráfico *boxplot* mensual nos muestra precisamente que uno de los
meses para los que pudieramos argumentar un cambio importante en la
media es febrero siendo el mes con la media más baja de todos. Además,
vemos que como se mencionó anteriormente el ciclo que se parece cumplir
año a año es una disminución importante en el mes de Febrero y una
recuperación en los meses sucesivos a este hasta llegar a Noviembre que
representa una caída de menor impacto que la del mes de febrero. Si
revisamos el gráfico de coordenadas paralelas para los meses nos
encontraremos con algo muy similar:

``` r
library(forecast)
ggseasonplot(NoTendLeche, main = 'Coordenadas paralelas: Volumen de leche mensual')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/SeasonalPlotLeche-1.png" style="display: block; margin: auto;" />

``` r
monthplot(NoTendLeche, ylab = '', main = 'Volumen de leche captada por la industria')
mtext(bquote(bold('Sub series mensuales')),
      side = 3, line = 0.3, adj = 0.5, cex = 0.8, col = 'darkgray')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/SeasonalPlotLeche-2.png" style="display: block; margin: auto;" />

``` r
par(default_par)
```

Sin embargo, no sobra revisar el periodograma para determinar si la
serie cuenta con ciclos de otros períodos:

``` r
library(latex2exp)
SpectrumLeche = spectrum(NoTendLeche, log = 'no', main = 'Periodogram')
mtext(TeX('Frequency $\\times$ 12', bold = T),
      side = 3, line = 0.5, adj = 0.5, cex = 0.8, col = 'darkgray')

frecuencias = SpectrumLeche$freq
Periodograma = SpectrumLeche$spec

for (i in 1:3){
  ind = which.max(Periodograma)
  cat('El valor máximo no.', i, ' en el que se máximiza el periodograma es', Periodograma[ind], '\nen la frecuencia ', frecuencias[ind],'\n')
  abline(v = frecuencias[ind], col = 'darkred', lty = 2)
  Periodograma = Periodograma[-ind]
  frecuencias = frecuencias[-ind]
}
```

<img src="Avance-1--Versión-2-_files/figure-gfm/SpectrumLeche-1.png" style="display: block; margin: auto;" />

    ## El valor máximo no. 1  en el que se máximiza el periodograma es 374.0577 
    ## en la frecuencia  1 
    ## El valor máximo no. 2  en el que se máximiza el periodograma es 149.8748 
    ## en la frecuencia  0.5 
    ## El valor máximo no. 3  en el que se máximiza el periodograma es 118.3689 
    ## en la frecuencia  5

Note que la frecuencia en que se maximiza el espectograma es aquella que
ya habíamos señalado como probable: la equivalente a un período de un
año ($\omega = 1 \times \frac{1}{12}$). Además de esta, el segundo valor
mayor representa un ciclo de período 24 meses
($\omega = 0.5 \times \frac{1}{12} = \frac{1}{24}$) que es un armónico
del primero por lo que el ciclo principal de la serie se da en períodos
de una año.

``` r
library(TSstudio)
ts_heatmap(NoTendLeche,title = 'Heatmap Volumen de leche captado serie mensual')
```


### Precio diario de venta de oro en Colombia

``` r
# ACF, PACF

acf(dif_ll_tsventa,lag.max = 60, main='Serie diferenciada')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/EstacionalidadOro-1.png" style="display: block; margin: auto;" />

``` r
acf(NoTendKer,lag.max = 60, main='Serie sin tendecia Kernel')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/EstacionalidadOro-2.png" style="display: block; margin: auto;" />

``` r
pacf(dif_ll_tsventa,lag.max = 60 , main='Serie diferenciada')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/EstacionalidadOro-3.png" style="display: block; margin: auto;" />

``` r
PeriodogrNoTendDif <- spectrum(dif_ll_tsventa, main = "Periodograma serie diferenciada", log = "no")
```

<img src="Avance-1--Versión-2-_files/figure-gfm/EstacionalidadOro-4.png" style="display: block; margin: auto;" />

``` r
# Funcion para calcular el periodo
find.freq <- function(x)
    {
        n <- length(x)
        spec <- spec.ar(c(x),plot=FALSE)
        if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
        {
            period <- round(1/spec$freq[which.max(spec$spec)])
            if(period==Inf) # Find next local maximum
            {
                j <- which(diff(spec$spec)>0)
                if(length(j)>0)
                {
                    nextmax <- j[1] + which.max(spec$spec[j[1]:500])
                    period <- round(1/spec$freq[nextmax])
                }
                else
                    period <- 1
            }
        }
        else
            period <- 1
        return(period)
    }

find.freq(dif_ll_tsventa)
```

    ## [1] 1

``` r
# Para la serie sin tendencia diferenciada
PeriodogrNoTendDif <- spectrum(dif_ll_tsventa, main = "Periodograma S.Diferenciada", log = "no")
abline(v = PeriodogrNoTendDif$freq[match(max(PeriodogrNoTendDif$spec),PeriodogrNoTendDif$spec)], col='red')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/EstacionalidadOro-5.png" style="display: block; margin: auto;" />

``` r
# periodograma <- PeriodogrNoTendDif
# max(PeriodogrNoTendDif$spec)
# periodograma$freq[match(max(periodograma$spec),periodograma$spec)]
# periodo=1/periodograma$freq[match(max(periodograma$spec),periodograma$spec)]
# periodo
```

Con los motivos mencionados anteriormente y con base en el ACF, los
valores decrecen rápidamente y caen dentro de las bandas de confianza;
sugiriendo que la correlación es significativamente 0, con lo cual la
primera diferenciación fue exitosa para remover la tendencia.

``` r
start_date <- as.Date("2004-01-01")  
dates <- seq.Date(from = start_date, by = "day", length.out = length(NoTendSTL))
years <- as.numeric(format(dates, "%Y"))

# Crear un dataframe para usar en ggplot
data <- data.frame(Year = years, Value = as.numeric(NoTendSTL))

# Crear una variable que indica cada 4 años
data$FourYearPeriod <- (data$Year - min(data$Year)) %/% 4

# Crear el diagrama de cajas
ggplot(data, aes(x = factor(FourYearPeriod), y = Value)) +
  geom_boxplot() +
  xlab("Período de 4 Años") +
  ylab("Valor") +
  ggtitle("Diagrama de Cajas por Período de 4 Años") +
  theme_minimal()
```

<img src="Avance-1--Versión-2-_files/figure-gfm/BoxPLot cada 4 años-1.png" style="display: block; margin: auto;" />

Los primero 4 bloques (0 al 3) muestra una variabilidad relativamente
consistente en términos de rango intercuartil.

El bloque 4 parece tener una mayor variabilidad con la caja y los
bigotes más extendidos, lo que sugiere una dispersión más amplia de los
datos en ese período.

El bloque 5 se muestra bastante diferente, con una caja muy pequeña y
pocos datos, lo que podría indicar un bloque incompleto o un período con
menos datos disponibles.

``` r
library(lubridate)
ll_tb<-as_tsibble(dif_ll_tsventa,index=tibble(fecha))
colnames(ll_tb)<-c("Fecha","Dato")

# 
ll_tb$dia <-wday(ll_tb$Fecha, label = TRUE, abbr = TRUE,week_start = 1)
ll_tb$mes <- factor(month.abb[month(ll_tb$Fecha)], levels =   month.abb)

ll_tb$mes <- factor(month.abb[month(ll_tb$Fecha)], levels =   month.abb)

# Boxplot diario
ll_tb%>%mutate(diff_ND=Dato-lag(Dato))%>%plot_seasonal_diagnostics(.date_var = Fecha,.value = diff_ND,.feature_set = c("wday.lbl"),.geom="boxplot")
```


``` r
# Boxplot mensual
ll_tb%>%mutate(diff_ND=Dato-lag(Dato))%>%plot_seasonal_diagnostics(.date_var = Fecha,.value = diff_ND,.feature_set = c("month.lbl"),.geom="boxplot")
```


``` r
# Boxplot Anual
ll_tb%>%mutate(diff_ND=Dato-lag(Dato))%>%plot_seasonal_diagnostics(.date_var = Fecha,.value = diff_ND,.feature_set = c("year"),.geom="boxplot")
```



``` r
# Boxplot trimestral
ll_tb%>%mutate(diff_ND=Dato-lag(Dato))%>%plot_seasonal_diagnostics(.date_var = Fecha,.value = diff_ND,.feature_set = c("quarter"),.geom="boxplot")
```

``` r
TSstudio::ts_heatmap(ll_tsventa,title = "Mapa de Calor - Venta de oro en Colombia")
```

Para los diagramas de cajas como característica común, se puede ver que
hay presencia de datos atípicos por semana, mes, trimestre y año.

Para los días lunes, martes y miércoles se presenta menor variabilidad
en el rango intercuartil, en contraste con los días viernes y sábado.
Sin embargo no hay diferencias en términos de la media.

En los meses y trimestres no se presentan cambios respecto a la media ni
hay variación entre sus respectivas categorías.

Para el los años de 2008, 2009 , 2015, 2020 hay mayor variación en el
rango intercuartil. Sin embargo no hay diferencias en la media.

Respecto al mapa de calor pareciera que hay una intervención que
comienza en el 2004 y va hasta el 2012, y del 2013 hasta la fecha.

### 

<!-- ========================================== Sección 4 ==========================================  -->

## **Eliminación de la componente estacional**

### Volumen mensual de leche producida en Colombia

Ahora sólo nos resta eliminar la componente estacional de la serie. Para
esto utilizaremos dos modelos de componentes de Fourier y un modelo de
variables dummy:

``` r
library(tsibble)
library(tibble)
library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(TSA)
library(dplyr)

TsibbleLeche = as_tsibble(NoTendLeche)


ModelosEstacionalidadLeche<-TsibbleLeche%>%model(
  'Fourier (2 Componentes)'=ARIMA(value ~ fourier(K=2)+pdq(0, 0, 0) + PDQ(0, 0, 0)),
  'Fourier (3 Componentes)'=ARIMA(value ~ fourier(K=3)+pdq(0, 0, 0) + PDQ(0, 0, 0)),
  'Fourier (4 Componentes)'=ARIMA(value ~ fourier(K=4)+pdq(0, 0, 0) + PDQ(0, 0, 0)),
  'Dummy'=ARIMA(value ~ season()+pdq(0, 0, 0) + PDQ(0, 0, 0))
)


ModelosAjustadosEstacionalidadLeche<-TsibbleLeche%>%
  left_join(fitted(ModelosEstacionalidadLeche)|>group_by(.model)%>%
                                                     pivot_wider(names_from = .model, values_from = .fitted))

par(mfrow = c(4,1), mar = c(2,2.5,3,0.5))
ModelosAjustadosEstacionalidadLeche = as.data.frame(ModelosAjustadosEstacionalidadLeche)

for (i in  colnames(ModelosAjustadosEstacionalidadLeche[,3:6])){
  # cat("###", paste('Modelo', i), "\n")
  plot(x = as.numeric(time(NoTendLeche)), 
        y = ModelosAjustadosEstacionalidadLeche[,i] , col = 'darkred',
       type = 'l', main = paste('Datos originales vs. Valores ajustados: ', i), ylim = c(-45,45), xlab = '', ylab = '', lwd = 2)
  lines(x = as.numeric(time(NoTendLeche)), y = as.numeric(NoTendLeche), col = 'black', lwd = 0.7)
}
```

<img src="Avance-1--Versión-2-_files/figure-gfm/Componente Estacional Leche-1.png" style="display: block; margin: auto;" />

A simple vista, parece ser que el modelo dummy ajusta mejor la
estacionalidad anual de nuestros datos. Sin embargo, la mejor opción es
siempre valerse de criterios de información para decidir cuál de
nuestros modelos es mejor:

``` r
# install.packages('DT')
knitr::kable(glance(ModelosEstacionalidadLeche), col.names = c('Modelo', '$\\sigma ^2$', 'Log-verosimilitud', 'AIC', 'AICc', 'BIC', '',''))
```

| Modelo                  | $\sigma ^2$ | Log-verosimilitud |      AIC |     AICc |      BIC |     |     |
|:------------------------|------------:|------------------:|---------:|---------:|---------:|:----|:----|
| Fourier (2 Componentes) |    168.2627 |         -762.4656 | 1534.931 | 1535.254 | 1551.219 |     |     |
| Fourier (3 Componentes) |    163.3990 |         -758.6231 | 1531.246 | 1531.855 | 1554.049 |     |     |
| Fourier (4 Componentes) |    156.4657 |         -753.4228 | 1524.846 | 1525.835 | 1554.163 |     |     |
| Dummy                   |    140.2703 |         -741.3552 | 1506.710 | 1508.453 | 1545.800 |     |     |

Observamos que, de todos los aspectos medidos, la regresión dummy parece
ser aquella que lo hace mejor por lo cual revisaremos qué coeficientes
esta regresión ha estimado:

``` r
knitr::kable(ModelosEstacionalidadLeche %>% dplyr::select(Dummy) %>%  coef())
```

| .model | term           |   estimate | std.error |  statistic |   p.value |
|:-------|:---------------|-----------:|----------:|-----------:|----------:|
| Dummy  | season()year2  | -23.761487 |  2.874826 | -8.2653640 | 0.0000000 |
| Dummy  | season()year3  |  -5.964460 |  2.874827 | -2.0747201 | 0.0393459 |
| Dummy  | season()year4  |  -3.833140 |  2.874827 | -1.3333467 | 0.1839978 |
| Dummy  | season()year5  |   8.213288 |  2.874827 |  2.8569682 | 0.0047485 |
| Dummy  | season()year6  |   5.701942 |  2.874827 |  1.9834038 | 0.0487477 |
| Dummy  | season()year7  |  15.065082 |  2.874827 |  5.2403449 | 0.0000004 |
| Dummy  | season()year8  |  11.162330 |  2.874826 |  3.8827840 | 0.0001420 |
| Dummy  | season()year9  |  -1.214940 |  2.874827 | -0.4226133 | 0.6730503 |
| Dummy  | season()year10 |   4.498269 |  2.874827 |  1.5647097 | 0.1192977 |
| Dummy  | season()year11 |  -7.850423 |  2.874827 | -2.7307469 | 0.0069076 |
| Dummy  | season()year12 |  -0.115805 |  2.874827 | -0.0402824 | 0.9679098 |

Note que la variable de base o de comparación es la variable de Enero.

<div style="height: 15px;">

</div>

<details>
<summary>
Ajuste con variables dummy usando la función `lm`
</summary>
<hr>

```` r
  df_dummy = as.data.frame(cbind(value = as.numeric(NoTendLeche), month = rep(1:12, 16)))
  RegresionDummy = lm(value ~ factor(month) - 1, data = df_dummy)
  summary(RegresionDummy)
  ```
  
  ```
  ## 
  ## Call:
  ## lm(formula = value ~ factor(month) - 1, data = df_dummy)
  ## 
  ## Residuals:
  ##     Min      1Q  Median      3Q     Max 
  ## -38.821  -7.046   0.344   7.574  24.055 
  ## 
  ## Coefficients:
  ##                  Estimate Std. Error t value Pr(>|t|)    
  ## factor(month)1    0.07042    2.96910   0.024  0.98110    
  ## factor(month)2  -23.76149    2.96910  -8.003 1.44e-13 ***
  ## factor(month)3   -5.96446    2.96910  -2.009  0.04605 *  
  ## factor(month)4   -3.83314    2.96910  -1.291  0.19836    
  ## factor(month)5    8.21329    2.96910   2.766  0.00626 ** 
  ## factor(month)6    5.70194    2.96910   1.920  0.05638 .  
  ## factor(month)7   15.06508    2.96910   5.074 9.65e-07 ***
  ## factor(month)8   11.16233    2.96910   3.759  0.00023 ***
  ## factor(month)9   -1.21494    2.96910  -0.409  0.68288    
  ## factor(month)10   4.49827    2.96910   1.515  0.13152    
  ## factor(month)11  -7.85042    2.96910  -2.644  0.00892 ** 
  ## factor(month)12  -0.11581    2.96910  -0.039  0.96893    
  ## ---
  ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  ## 
  ## Residual standard error: 11.88 on 180 degrees of freedom
  ## Multiple R-squared:  0.4201, Adjusted R-squared:  0.3815 
  ## F-statistic: 10.87 on 12 and 180 DF,  p-value: 4.095e-16
  ```
  

```r
plot.ts(NoTendLeche, col = 'darkblue', xlab = 'Tiempo', ylab = '', main = 'Serie de tiempo volumen de leche captado por la industria colombiana')
lines(x = as.numeric(time(NoTendLeche)), y = fitted(RegresionDummy), col = 'darkred', lwd = 2)
legend('topright', legend = c('Valores reales', 'Valores ajustados'),col = c('darkblue', 'darkred'), lty = 1,lwd = c(1,2), bty = 'n',cex = 0.8)
````

<img src="Avance-1--Versión-2-_files/figure-gfm/Plot dummy leche lm-1.png" style="display: block; margin: auto;" />
Note que los resultados son exactamente los mismos obtenidos en
comparación con los obtenido del método usado anteriormentem con la
diferencia de que en este caso podríamos eliminar variables para
verificar si la inclusión de estas en el modelo lo afecta de forma
significativa respecto a sus medidas de calidad (AIC, AICc,BIC, etc.).
Aunque por el momento no revisaremos esto.

<hr>
</details>

<div style="height: 15px;">

</div>

Veamos más de cerca el ajuste por variables dummy alcanzado respecto a
la estacionalidad de nuestra serie:

``` r
par(mar = c(5,4,4,8))
plot(time(NoTendLeche)[1:97],NoTendLeche[1:97], type = 'l', lwd = 1, main = 'Datos originales vs. Valores ajustados con variables dummy',
     ylab = '', xlab = 'Tiempo')
mtext(bquote(bold('Enero 2008 - Enero 2016')),
      side = 3, line = 0.3, adj = 0.5, cex = 0.8, col = 'darkgray')
lines(time(NoTendLeche)[1:97], ModelosAjustadosEstacionalidadLeche$Dummy[1:97], col = 'darkred', lwd = 2)
legend('right',legend = c('Datos\noriginales\n', 'Valores\najustados\n'), col = c('black', 'darkred'), lty = 1,
       xpd = T, inset = c(-0.1, 0), bty = 'n', lwd= c(1,2))
abline(v = 2008:2023, col = 'gray', lty = 'dashed')
abline(h = 0, col = 'black', lwd = 0.3)
```

<img src="Avance-1--Versión-2-_files/figure-gfm/DummyVsOriginales1-1.png" style="display: block; margin: auto;" />

``` r
par(mar = c(5,4,4,8))
plot(time(NoTendLeche)[97:192],NoTendLeche[97:192], type = 'l', lwd = 1, main = 'Datos originales vs. Valores ajustados con variables dummy',
     ylab = '', xlab = 'Tiempo')
mtext(bquote(bold('Enero 2016 - Diciembre 2023')),
      side = 3, line = 0.3, adj = 0.5, cex = 0.8, col = 'darkgray')
lines(time(NoTendLeche)[97:192], ModelosAjustadosEstacionalidadLeche$Dummy[97:192], col = 'darkred', lwd = 2)
abline(v = 2008:2023, col = 'gray', lty = 'dashed')
abline(h = 0, col = 'black', lwd = 0.3)
legend('right',legend = c('Datos\noriginales\n', 'Valores\najustados\n'), col = c('black', 'darkred'), lty = 1,
       xpd = T, inset = c(-0.1, 0), bty = 'n', lwd= c(1,2))
```

<img src="Avance-1--Versión-2-_files/figure-gfm/DummyVsOriginales2-1.png" style="display: block; margin: auto;" />

Habiendo hecho todo lo anterior, la serie con varianza estabilizada, sin
tendencia y desestacionalizada resulta de la siguiente forma:

``` r
plot.ts(NoTendLeche - ModelosAjustadosEstacionalidadLeche$Dummy, col = 'black',
        main = 'Serie volumen de leche captado por la industria colombiana',
        # sub = 'En millones de litros',
        ylab = '', xaxt = 'n',
        xlab = 'Tiempo')
axis(labels = 2008:2024, at = 2008:2024, side = 1, las = 2)
abline(v = 2008:2024, col = 'gray', lty = 'dashed', lwd = 1.5)

mtext(bquote(bold('Sin tendencia y desestacionalizada')), side = 1, line = -12.3, adj = 0.5, cex = 0.8, col = 'darkgray')
```

<img src="Avance-1--Versión-2-_files/figure-gfm/FinalLeche-1.png" style="display: block; margin: auto;" />

### Precio diario de venta de oro en Colombia
