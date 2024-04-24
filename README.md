# Series de tiempo univariadas (2024-I)

## Departamento de Estadística. Facultad de Ciencias

### Universidad Nacional de Colombia

En este repositorio se encuentra todo lo relacionado al proyecto llevado a cabo en la materia de series de tiempo univariadas de la Universidad Nacional de Colombia 
en el semestre 2024-I a cargo del profesor [Sergio Calderón](sacalderonv@unal.edu.co). Estos avances también se encuentran publicados en su versión ```html``` [Rpubs](https://rpubs.com/Mendivenson).

La entrega final del proyecto será la implementación de una aplicación en R (Shiny). Todos los detalles de la implementación a lo largo de los avances se encuentra
en este repositorio:

- [**Avance 1:**](https://rpubs.com/Mendivenson/Avance1_STdU) Análisis descriptivo de las series de tiempo.
- [**Avance 2:**]() Modelo de suavizamiento exponencial y medidas de precisión.
- [**Proyecto final:**](https://y688a9-michel0mendivenson-barragan0zabala.shinyapps.io/TimeSeriesDashboard/) Dashboard para análisis y predicción básica de series de tiempo con los métodos vistos en clase.

### Datos:

Para el proyecto se usarán dos series (Una de frecuencia diaria y la otra mensual) correspondientes a:

- [La producción de leche mensual en Colombia (En el período 2008 a 2023)](http://uspleche.minagricultura.gov.co/documentos.html)
- [El precio de compra o venta diario del oro por gramo (En el período 2010 a Febrero de 2024)](https://www.banrep.gov.co/es/estadisticas/precios-del-dia-para-el-gramo-oro-plata-y-platino)

---

### To do:

- [ ] **Avance 1:**
  
  - [ ] Gráfico correlación en 12 períodos de tiempo (Serie mensual)

- [ ] **Avance 2:**
  
  - [ ] Suavizamiento exponencial (Serie mensual)
  
  - [ ] Suavizamiento exponencial (Serie diaria)
  
  - [ ] Medidas de precisión (Serie mensual)
  
  - [ ] Medidas de precisión (Serie diaria)

---

**Nota 1:** Hacer todo dentro de ```.Rmd``` se ve bonito. Sin embargo, es una mucho mejor idea hacer los scripts por aparte de cada cosa y después llamar los resultados dentro del ```html```. De esa forma mucho del código innecesario (Como el de las gráficas boxplot) ni se ve en el documento final ni ocupa espacio en el archivo ```.Rmd```

**Nota 2:** Creo que, de hecho, hay una opción dentro de los chunks de código de ``R`` que permite a los chunks imprimir código de otros scripts. [Puede revisarse acá](https://stackoverflow.com/questions/52397430/include-code-from-an-external-r-script-run-in-display-both-code-and-output)
