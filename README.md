# Haskinator

Implementación de un sistema de predicciones para el primer proyecto de CI3661 -
Laboratorio de Lenguajes de Programación para el trimestre Enero-Marzo 2021. 
Está basado en el conocido juego __Akinator__, e implementado en __Haskell__.

## Dependencias

* The Glorious Glasgow Haskell Compilation System, version 8.4.4

## Cómo ejecutar el programa

Para ejecutar __Haskinator__ primero debe ser compilado. Para ello, ejecute
el siguiente comando:

```bash
$ make 
```

Este comando creará el ejecutable para iniciar el juego llamado "Haskinator".

### Obtener archivos objeto

Por defecto, el __makefile__ del proyecto elimina todos los archivos objeto. Si
usted desea obtener estos archivos, puede ejecutar el siguiente comando:

```bash
$ make haskinator
```

Básicamente ejecuta lo mismo que el caso por defecto, pero sin realizar la 
limpieza.

Si después de crear estos archivos desea eliminarlos, ejecute:

```bash
$ make clean
```

## Implementación

### Instancias de Read y Show 

Incialmente intentamos implementar nuestros propios métodos para leer y escribir
oraculos, que si bien facilitaban el parseo de los mismos, generaba una cantidad
de código sustancial que fue simplificada haciendo un "__cast__" de la operación
`read` como una variable de tipo `Oraculo`. 

### Utilizar `Oraculo.hs` como módulo

Para utilizar `Oraculo.hs` como un módulo de __Haskell__ solo es necesario 
importar el archivo del módulo. Serán exportadas únicamente las funciones y 
tipos solicitados en el enunciado del proyecto, y no las funciones auxilares de
otros módulos que fueron utilizadas en él.

### Funciones de Correctitud del Oraculo

Se define un `Oraculo` como correcto si no tiene Preguntas o Predicciones 
repetidas. Si bien tiene sentido que existan, estas fueron consideradas como
restricciones de la implementación a modo de simplificación.

Para asegurar la correctitud de un `Oraculo`, se deben obtener todas sus 
predicciones y preguntas como listas de `String`s. Al agregar una nueva 
prediccion o pregunta se verifica que esta no se encuentra en la lista.

### Funciones de Impresión

Estas funciones son utilizadas para darle un formato agradable a la salida
de texto que se le muestra al usuario. Se resaltan los mensajes de error con el
color rojo y los mensajes de éxito con el color verde.

### Funciones de Creación

Constituida únicamente por la función encargada de crear un nuevo oraculo 
con una única predicción. Ella es usada en cualquier otro mecanismo que necesite
crear una predicción como consecuencia de su ejecución.

### Funciones de Predicción

Son las encargadas de presentar al usuario el flujo de predicción a través de 
una serie de preguntas, siguiendo el oraculo que se encuentre en memoria en ese
momento. Solo se puede ejecutar si existe un oraculo cargado en memoria.

Si la predicción no es acertada, el usuario puede contribuir con la creación de
un nuevo nodo de pregunta en el arbol que constituye el oraculo, o una nueva 
opción si ninguna de las opciones era acertada para el elemento que el usuario
tenía en mente. Para ello se hace uso de las funciones de correctitud del 
oraculo descritas anteriormente, de manera que no se introduzcan inconsistencias.

### Funciones de Persistencia y Cargado

Son las encargadas de manejar la carga de un oraculo desde un archivo de texto
y el guardado hacia un archivo. Este es el mecanismo que el programa de 
predicción utiliza para la persistencia de datos.

### Funciones de Pregunta Crucial

Para verificar el momento de bifurcación en el recorrido del arbol, fue 
implementado un algoritmo para calcular el __ancestro mínimo común__. Para ello,
aplicamos el algoritmo __DFS__ para obtener el camino desde la raíz del arbol
del oraculo hasta cada uno de los nodos a comparar. Finalmente, se comparan los
caminos retornando así el punto a partir del cual difieren.

Es importante destacar que antes de la ejecución de cualquiera de estos 
algoritmos, se realizan las verificaciones necesarias con el uso de las funciones
descritas en secciones anteriores, de manera que se compruebe que los nodos a 
ser buscados en el arbol son efectivamente parte de él.

### Funciones de "__Main__"

Son las encargas de mostrar información al usuario de las acciones que tiene
a su disposición, iniciando el ciclo de interacción con el mismo.

Aquí es importante destacar que para la correcta compilación y funcionamiento
del programa, fue necesario indicar que no se use el Buffering en la entrada y
salida del programa. De esta manera, se evita la lectura de caracteres que el 
usuario no introduce intencionalmente.

## Integrantes
* Manuel Faria (15-10463)
* Amin Arriaga (16-10072) 
