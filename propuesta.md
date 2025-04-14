---
title: "Lenguaje para Funciones Recursivas de Listas (FRL)"
author: Tomás Maiza (M-7116/1)
geometry: margin=2cm
output: pdf_document
header-includes:
  - \usepackage{graphicx}
  - \usepackage{amsmath}
  - \usepackage{amssymb}
---

La idea del TP Final es desarrollar un lenguaje para manipular Funciones Recursivas de Listas (FRL).

Se define una **lista** como una secuencia ordenada de cero o más elementos pertenecientes a los $N_0$. Llamando _L_ al conjunto de todas las listas, las **funciones de lista** son funciones que van de _L_ en _L_.

Las listas admiten diferentes formas y operaciones que se desea representar en el lenguaje, como:

- Listas vacías
- Operación concatenación
- Funciones base: 
  - **Cero a derecha** (inserta un 0 como último elemento de la lista)
  - **Cero a izquierda** (inserta un 0 como primer elemento de la lista)
  - **Borrar a derecha** (borra el último elemento de la lista)
  - **Borrar a izquierda** (borra el primer elemento de la lista)
  - **Sucesor a izquierda** (incrementa en una unidad el primer elemento de la lista)
  - **Sucesor a derecha** (incrementa en una unidad el último elemento de la lista)
- **Operador composición** (permite la aplicación de una sucesión de funciones)
- **Operador repetición** (permite iterar una función sobre una lista)

Además, puede incluir algunas funciones básicas formadas a partir de las funciones base:

- **Pasar a izquierda** (mueve el primer elemento al último lugar de la lista)
- **Pasar a derecha** (mueve el último elemento alprimer lugar de la lista)
- **Duplicar a izquierda** (duplica el primer elemento de la lista)
- **Duplicar a derecha** (duplica el último elemento de la lista)
- **Intercambiar extremos** (intercambia el primer y el último elemento)

El lenguaje debe proveer la forma de escribir y representar listas (y mostrarlas definiendo un **Pretty Printer**), permitir utilizar las funciones base y operadores para definir funciones e interpretar la aplicación de las mismas. 

Además, debe permitir visualizar la **traza** de la ejecución de una secuencia de funciones compuestas sobre una lista. Para ello va a contar con la posibilidad de trabajar en un entorno interactivo que permite ejecutar funciones y mostrar su paso a paso, también con la posibilidad de cargar archivos para poder utilizar funciones predefinidas.

Para generar el parser se va a utilizar la librería **Parsec** vista durante el cursado.

