#Borrar el entorno de trabajo
rm(list=ls()) 

# Cargar librerías necesarias
library(ggplot2)     # Para gráficos 2D
library(plotly)      # Para gráficos 3D
library(readr)       # Para leer archivos CSV
library(stats)       # Para realizar PCA
library(factoextra)  # Para eigenvalue y visualización de PCA
library(dplyr)       # Para manipulación de datos
library(nortest)     # Para pruebas de normalidad
library(gtsummary)   # Para tablas resumen

# Cargar la base de datos
datos <- read.csv("mubio02_act3_alimentos_nutrientes_4900.csv")

# Ver los datos cargados
View(datos)

# Verificar si hay valores NA
any(is.na(datos)) # Retorna TRUE si hay valores NA

# Contar valores NA en cada columna
colSums(is.na(datos))

# Eliminar filas con valores NA
datos_limpios <- datos[complete.cases(datos),]

# Verificar nuevamente si hay valores NA
any(is.na(datos_limpios)) # Debería retornar FALSE

# Ver los datos limpios
View(datos_limpios)

# Cromprobar la normalidad de los datos
pvalor <- matrix(NA, nrow=ncol(datos_limpios), ncol=1) # Crear una matriz para registrar los p values

# Utilizar un bucle para calcular la normalidad en cada columna
for (i in 2:ncol(datos_limpios)) {
  resultado_anderson <- ad.test(datos_limpios[[i]]) #Aplicar el test de Anderson-Darling a cada columna
  pvalor[i, ] <- resultado_anderson$p.value #Guardar el valor de p-value en la matriz
}

rownames(pvalor) <- colnames(datos_limpios) #Asignar el nombres a las filas de la matriz

pvalor #Mostrar la matriz con los p-value


# Normalizar todos los datos numéricos
datos_limpios_norm <- datos_limpios %>% 
  mutate(across(where(is.numeric), scale))

# Ver las primeras filas de los datos normalizados
head(datos_limpios_norm)

# Ver los datos normalizados
View(datos_limpios_norm)

# Realizar PCA de los alimentos y nutrientes
pca <- prcomp(datos_limpios_norm[, 21:ncol(datos_limpios_norm)], center = TRUE, scale. = TRUE)

# Imprimir el resumen del PCA
print(summary(pca))

# Obtener los valores propios (eigenvalues)
eigenvalues <- get_eigenvalue(pca)

# Imprimir los valores propios
print(eigenvalues) # Verificar que con dos dimensiones se explica más del 60-70% de la varianza acumulada

# Comprobación visual de la varianza explicada por cada componente
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 20))

# Visualizar las variables más destacadas según sus contribuciones en los ejes PC1 y PC2
fviz_contrib(pca, choice = "var", axes = 1, top = 10) 
fviz_contrib(pca, choice = "var", axes = 2, top = 10)


## Crear gráficos de los componentes principales

# Gráfico para visualizar las dos primeras componentes principales
fviz_pca_ind(pca, geom.ind = "point", 
             col.ind = "blue", 
             axes = c(1, 2), 
             pointsize = 1.5) 

# Visualizar el gráfico de correlación de las variable 
fviz_pca_var(pca, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)

# Crear dataframe de scores del PCA
scores_PCA_df <- as.data.frame(pca$x)

# Ver las primeras filas de los scores del PCA
head(scores_PCA_df)

# Crear terciles para cada componente principal
componentes_terciles <- scores_PCA_df %>%
  mutate(across(everything(), ~ ntile(., 3), .names = "tercil_{col}"))

# Convertir terciles a factores
componentes_terciles_factor <- componentes_terciles %>%
  mutate(across(starts_with("tercil_"), as.factor))

# Comprobar los factores
View(componentes_terciles_factor)

# Extraer factores de PC1 y PC2
terciles_factor_PC1 <- componentes_terciles_factor[178]
terciles_factor_PC2 <- componentes_terciles_factor[179]

# Ver la estructura del factor PC1
str(terciles_factor_PC1)

# Ver los datos originales
View(datos)

# Seleccionar variables sociodemográficas continuas
socio_continuas <- select(datos_limpios_norm, altura, peso, IMC, edad, METs_h_semana)

# Ver las variables sociodemográficas continuas
View(socio_continuas)

# Combinar variables sociodemográficas con los terciles de PC1
socio_continuas_PC1 <-cbind(socio_continuas, terciles_factor_PC1) 

# Ver los datos combinados para PC1
View(socio_continuas_PC1)

# Ver la clase del dataframe combinado
class(socio_continuas_PC1)

# Combinar variables sociodemográficas con los terciles de PC2
socio_continuas_PC2 <-cbind(socio_continuas, terciles_factor_PC2) 

# Ver los datos combinados para PC2
View(socio_continuas_PC2)

# Crear tabla resumen para PC1
tabla_resumen_PC1 <- socio_continuas_PC1 %>%
  select(tercil_PC1, altura, peso, IMC, edad, METs_h_semana) %>%
  tbl_summary(
    by = tercil_PC1, 
    type = list(altura ~ "continuous",
                peso ~ "continuous",
                IMC ~ "continuous",
                edad ~ "continuous",
                METs_h_semana ~ "continuous"),
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  ) %>%
  add_p(
    test = all_continuous() ~ "kruskal.test", # Buscar el valor p usando la prueba de Kruskal-Wallis
    pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>%
  modify_header(label = "**Variables**") %>%
  modify_header(all_stat_cols() ~ "**T**")

# Ver la tabla resumen para PC1
tabla_resumen_PC1

# Crear tabla resumen para PC2
tabla_resumen_PC2 <- socio_continuas_PC2 %>%
  select(tercil_PC2, altura, peso, IMC, edad, METs_h_semana) %>%
  tbl_summary(
    by = tercil_PC2, 
    type = list(
      altura ~ "continuous",
      peso ~ "continuous",
      IMC ~ "continuous",
      edad ~ "continuous",
      METs_h_semana ~ "continuous"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  ) %>%
  add_p(
    test = all_continuous() ~ "kruskal.test", 
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  modify_header(label = "**Variables**") %>%
  modify_header(all_stat_cols() ~ "**T**")

# Ver la tabla resumen para PC2
tabla_resumen_PC2

# Unir las dos tablas resumen en una sola tabla final
tabla_final <- tbl_merge(
  tbls = list(tabla_resumen_PC1, tabla_resumen_PC2),
  tab_spanner = c("**PC1**", "**PC2**"))

# Ver la tabla final
tabla_final

# Preparar el dataframe (datos) para regresión logística

# Seleccionar la columna de diabetes y convertirla en factor binario
col_diabetes <- select(datos_limpios, diab_prev)
col_diabetes_factor <- as.factor(col_diabetes$diab_prev)

# Ver la estructura del factor de diabetes
str(col_diabetes_factor)

# Ver las primeras filas del factor de diabetes
head(col_diabetes_factor)

# Ajustar modelo de regresión logística
modelo_logistico <- glm(diab_prev ~ tercil_PC21 + tercil_PC22 + edad + peso + IMC,
                        family = binomial,
                        data = cbind(datos_limpios, terciles_factor_PC1, terciles_factor_PC2))

# Ver los resultados del modelo logístico
summary(modelo_logistico)

# Calcular los intervalos de confianza para los coeficientes del modelo
confint(modelo_logistico) # Intervalo de confianza

# Calcular las Odds Ratios (OR) a partir de los coeficientes del modelo
exp(modelo_logistico$coefficients) # Las OR

# Calcular los intervalos de confianza para las Odds Ratios
exp(confint(modelo_logistico)) # Intervalo de confianza de las OR

