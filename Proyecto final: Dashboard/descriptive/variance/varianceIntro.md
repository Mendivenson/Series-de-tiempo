# Análisis de la varianza:

Una de las suposiciones importantes realizadas sobre las series de tiempo (Al menos en lo estudiado en el curso) es la homocedasticidad o varianza constante. Sin embargo, este supuesto habitualmente se ve incumplido por lo que se tienen herramientas como en este caso la **transformación de Box-Cox** para lograr esa varianza constante. Si bien existen más métodos, por el momento sólo utilizaremos este.

**¡CUIDADO!:** Este método sólo es aplicable a series sin valores negativos. En caso de haber valores negativos se recomienda hacer una transformación para en general usar esta página.

En este caso, se utilizará la librería ```MASS```con su función ```boxcox```que encuentra el valor $\lambda$ para el cuál la log verosimilitud se maximiza por lo que se pide inicialmente que se ingrese el intervalo en el que se buscarán esos valores y la cantidad de valores que se quieren buscar.
