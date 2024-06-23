# DASHBOARD PARA ANÁLISIS Y PREDICCIÓN DE SERIES DE TIEMPO:
Esta app desarrollada en shiny se presenta como proyecto final para la materia. Tiene una organización similar a la vista con respecto a los avances y también respecto a la vista en clase:

1. Lectura de datos como objetos ```ts```.
1. Análisis descriptivo de series de tiempo.
    - Análisis de varianza.
    - Detección y eliminación de la tendencia.
    - Detección y eliminación de la estacionariedad.
1. Filtro exponencial. $\star$
1. Árboles de decisión. $\star$

## Sobre agregar elementos al menú de la barra de opciones de la página:
El procedimiento general es:

1. Generar un script ```.R``` con dos funciones principales con los sufijos UI y server.
1. Incluir en el script [```app.R```] el menú correspondiente y llamar el módulo dentro de la función teniendo en cuenta si este devuelve valores o no.
1. Para que el menú se presente correctamente también se debe agregar el cuerpo del menú dos veces: La primera para la definición y la segunda para agregarlo dentro del objeto bodyItems en el mismo script.

La forma común de llamar módulos dentro de shiny es cargar el script con la función ```source()```, cargar el módulo con ```callModule('Nombre del server dentro del módulo', 'Id que se le dará al módulo dentro de la app principal', 'Otras variables que se deban pasar' )``` dentro de la función server del script principal y finalmente cargar la interfaz del módulo usando la función UI del módulo con el argumento del id dado en la función ```callModule```.

**Nota:** Si la función server del módulo devuelve un objeto se hace ```objeto = callModule``` para guardarlo en la aplicación principal y dejarlo disponible para otros módulos.
