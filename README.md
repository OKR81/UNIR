# UNIR

Caso Practiico de R

Este repositorio va a ser usado por el grupo compuesto por:

- Óscar Núñez López
- María García Ribas
- Consuelo Azaña García
Tendrá fines didáticos y de aprendizaje; para ello iremos subiendo y modificando un Ejercicio Práctico basado en programación R de forma colaborativa.

# Proyecto de Análisis Bioinformático

## Propósito del Proyecto
El propósito de este proyecto es analizar cómo ciertas variables sociodemográficas se relacionan con el consumo de alimentos y nutrientes, y su impacto en la prevalencia de enfermedades como diabetes, obesidad o enfermedades cardiovasculares. Este análisis se realiza mediante técnicas de reducción de dimensionalidad, visualización gráfica, análisis descriptivo y modelado predictivo.

## Objetivos

### Objetivo Principal
**Relacionar variables sociodemográficas (como el peso, el riesgo de padecer diabetes y el IMC) con el consumo de ciertos alimentos y nutrientes, y analizar su impacto en la prevalencia de enfermedades.**

### Objetivos Específicos
1. **Calidad de Datos**: Garantizar la integridad de los datos mediante la identificación y eliminación de valores faltantes.
2. **Reducción de Dimensionalidad**: Aplicar PCA para resumir la información nutricional en componentes principales.
3. **Análisis Descriptivo**: Realizar análisis descriptivos de las variables y los componentes principales.
4. **Modelado Predictivo**: Implementar un modelo de regresión logística para predecir la prevalencia de enfermedades utilizando los componentes principales y otras variables relevantes.
5. **Evaluación del Modelo**: Evaluar la precisión y significancia del modelo predictivo.
6. **Conclusiones y Recomendaciones**: Extraer conclusiones basadas en los resultados obtenidos y ofrecer recomendaciones para futuras investigaciones.
7. **Presentación de Resultados**: Presentar los hallazgos y el análisis realizado de manera clara y concisa.

## Instrucciones de Uso
### Requisitos Previos
- **Software**: R con las bibliotecas necesarias (`ggplot2`, `plotly`, `readr`, `dplyr`, `stats`, `factoextra`, `nortest`, `gtsummary`).
- **Datos**: Conjunto de datos sociodemográficos, consumo de alimentos y prevalencia de enfermedades.

### Pasos para el Análisis
1. **Carga y Preprocesamiento de Datos**:
    - Importar los datos en R.
    - Limpiar y preparar los datos eliminando valores NA y explorando la estructura de los datos.

2. **Comprobación de la Normalidad y Estandarización**:
    - Verificar la normalidad de los datos utilizando pruebas estadísticas como Anderson Darling.
    - Estandarizar los datos para normalizar las escalas de las variables.

3. **Análisis de Componentes Principales (PCA)**:
    - Aplicar PCA a los datos de alimentos y nutrientes para reducir la dimensionalidad y resumir la información en componentes principales.
    - Interpretar los componentes principales y su contribución a la variabilidad de los datos.

4. **Análisis Descriptivo**:
    - Crear gráficos descriptivos que muestren la distribución de los componentes principales y su relación con las variables sociodemográficas y de consumo.
    - Crear una tabla descriptiva que resuma las variables importantes, incluyendo los valores p.

5. **Modelado Predictivo**:
    - Implementar un modelo de regresión logística utilizando como variable objetivo la prevalencia de la enfermedad seleccionada.
    - Utilizar como variables predictoras los componentes principales obtenidos en el PCA y otras variables relevantes.

6. **Evaluación del Modelo**:
    - Evaluar el modelo de regresión logística para identificar las variables más significativas y su impacto en la enfermedad prevalente.
    - Analizar los resultados y validar el modelo utilizando técnicas como la matriz de confusión, ROC y AUC.

7. **Conclusiones y Recomendaciones**:
    - Realizar conclusiones basadas en los resultados obtenidos.
    - Ofrecer recomendaciones y posibles áreas de investigación futura relacionadas con la relación entre el consumo de alimentos, nutrientes y la enfermedad estudiada.

8. **Presentación de Resultados**:
    - Presentar los hallazgos y el análisis realizado de manera clara y concisa en un informe grupal.
    - Destacar los puntos clave y las interpretaciones relevantes para facilitar la comprensión y discusión.
=======


>>>>>>> 6cce5cf8774876ddf5118ea532eae843b5cb86a7
