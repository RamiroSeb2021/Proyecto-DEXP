# 📊 Aplicación Shiny - Pruebas de Hipótesis y Tamaños de Muestra

Este proyecto tiene como propósito desarrollar una aplicación interactiva usando **Shiny en R**, con el objetivo de facilitar el aprendizaje y la aplicación de conceptos estadísticos abordados durante el curso. En particular, la app se centrará en:

- Aplicación de **pruebas de hipótesis** paramétricas y no paramétricas.
- Cálculo de **tamaños de muestra** bajo diferentes supuestos.
- Simulación y visualización de resultados estadísticos.
- Herramientas de análisis complementarias que se irán integrando conforme se avancen en los contenidos del curso.

---

## 📁 Estructura del Proyecto

La carpeta del proyecto contiene los siguientes archivos y directorios:

| Nombre              | Tipo       | Descripción |
|---------------------|------------|-------------|
| `T1/`               | Carpeta    | Contiene los scripts y materiales relacionados con el **primer tercio del curso**, enfocados en fundamentos y pruebas básicas. |
| `T2/`               | Carpeta    | Contiene los scripts correspondientes al **tercer tercio del curso**, donde se abordan temas más avanzados y complementarios. |
| `DataSets.zip`      | Archivo    | Archivo comprimido que contiene los **conjuntos de datos** utilizados en los códigos de las carpetas `T1` y `T2`. Se recomienda descomprimirlo para el correcto funcionamiento del proyecto. |
| `ui.R`              | Script R   | Define la **interfaz gráfica** de la aplicación Shiny. Aquí se establece la organización visual, menús, inputs y outputs de la app. |
| `server.R`          | Script R   | Contiene la **lógica del servidor** de la app. Aquí se programan los cálculos estadísticos, generación de gráficos y procesamiento de entradas del usuario. |

---

## 🚀 ¿Cómo iniciar la app?

Una vez descargado el proyecto:

1. Asegúrate de tener R y RStudio instalados.
2. Descomprime el archivo `DataSets.zip` dentro del directorio del proyecto.
3. Abre `ui.R` o `server.R` en RStudio.
4. Ejecuta la app con el siguiente comando en consola:

```r
shiny::runApp()
```

---

## 🧠 Notas adicionales

- La app se encuentra en etapa de desarrollo. Se irán incorporando más funcionalidades a medida que se cubran nuevos temas en clase.
- Asegúrate de tener instalados los paquetes necesarios (`shiny`, `ggplot2`, `dplyr`, etc.). Puedes instalarlos ejecutando:

```r
install.packages(c("shiny", "ggplot2", "dplyr"))
```
