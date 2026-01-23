# Modelado de Distribución de Especies – Hongos (Prueba de Concepto)

Este repositorio contiene una prueba de concepto (POC) desarrollada como parte de una etapa exploratoria inicial de un proyecto de tesis de Maestría en Ciencia de Datos, orientado al modelado de distribución espacial de especies fúngicas en Argentina.

El objetivo principal de esta POC es evaluar la disponibilidad de datos, la factibilidad técnica del procesamiento y la construcción de un pipeline básico de integración y modelado utilizando datos abiertos de biodiversidad y variables climáticas.

---

## Alcance y objetivos

En este repositorio se implementa un pipeline exploratorio para:

- Descargar y curar registros de ocurrencias de una especie de hongo desde GBIF.
- Filtrar y preprocesar ocurrencias georreferenciadas dentro de Argentina y subregiones seleccionadas.
- Integrar variables climáticas de WorldClim como predictores ambientales.
- Construir un dataset espacial combinando ocurrencias y variables ambientales.
- Entrenar modelos preliminares de distribución de especies.
- Generar mapas exploratorios de idoneidad ambiental.
