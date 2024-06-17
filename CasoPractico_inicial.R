# Caso Práctico de R - Versión 1

# Borrar el entorno de trabajo
rm(list=ls())

# Cargar librerías necesarias
library(readr)       # Para leer archivos CSV
library(dplyr)       # Para manipulación de datos
library(nortest)     # Para pruebas de normalidad
library(factoextra)  # Para PCA
library(ggplot2)     # Para gráficos
library(gtsummary)   # Para tablas resumen
library(gt)          # Para guardar tablas en formato Word

# Cargar la base de datos
data <- read.csv("mubio02_act3_alimentos_nutrientes_4900.csv")

# Mostrar las primeras filas del data frame data
head(data)

# Convertir la columna id a factor
data$id <- as.factor(data$id)

# Verificar si hay valores NA y eliminarlos
colSums(is.na(data))
data <- data[complete.cases(data),]
any(is.na(data))

# Estandarizar todas las columnas numéricas 
data_scale <- data %>%
  mutate(across(where(is.numeric), scale))

# Mostrar las primeras filas de los datos estandarizados
head(data_scale)

# Comprobar la normalidad de los datos
pvalor <- matrix(NA, nrow=ncol(data_scale), ncol=1) # Crear una matriz para registrar los p values

# Utilizar un bucle para calcular la normalidad en cada columna
for (i in 2:ncol(data_scale)) {
  resultado_anderson <- ad.test(data_scale[[i]]) # Aplicar el test de Anderson-Darling a cada columna
  pvalor[i, ] <- resultado_anderson$p.value # Guardar el valor de p-value en la matriz
}

rownames(pvalor) <- colnames(data_scale) # Asignar el nombres a las filas de la matriz

# Mostrar la matriz con los p-value
pvalor

# Realizar el PCA de alimentos y nutrientes
pca <- prcomp(data_scale[, 21:ncol(data_scale)], center = TRUE, scale. = TRUE)

# Resumen del PCA
summary(pca)

# Extraer los valores propios/varianzas de los componentes principales
eigenvalues <- get_eigenvalue(pca)
head(eigenvalues)

# Generar un scree plot para la elección del número de componentes principales
fviz_eig(pca, addlabels = TRUE)

# Extraer los resultados de las variables
var <- get_pca_var(pca)

# Calcular el valor de R-cuadrado para cada componente
pca_sdev <- as.data.frame(pca$sdev)
pca_sdev

# Mostrar las cargas para cada variable
pca_rotation <- as.data.frame(pca$rotation)
pca_rotation

# Crear gráficos de los componentes principales
# Gráfico para visualizar las dos primeras componentes principales
fviz_pca_ind(pca, geom.ind = "point", 
             col.ind = "blue", 
             axes = c(1, 2), 
             pointsize = 1.5) 

# Visualizar el gráfico de correlación de las variables
fviz_pca_var(pca, col.var = "cos2", 
             geom.var = "arrow", 
             labelsize = 2, 
             repel = FALSE)

# Realizar PCA solo de las variables sociodemográficas
pca2 <- prcomp(data_scale[,2:21], center = TRUE, scale. = TRUE)

# Extraer los componentes principales en un data frame
pca_data <- as.data.frame(pca2$x)

# Calcular los terciles del componente1 y componente2
terciles <- quantile(pca_data$PC1, probs = c(0.33, 0.66))
terciles2 <- quantile(pca_data$PC2, probs = c(0.33, 0.66))

# Crear las nuevas variables categóricas basadas en los terciles calculados
datos <- data %>%
  mutate(Tercil_PC1 = case_when(
    pca_data$PC1 <= terciles[1] ~ 1,
    pca_data$PC1 <= terciles[2] ~ 2,
    TRUE ~ 3
  ))

datos2 <- datos %>%
  mutate(Tercil_PC2 = case_when(
    pca_data$PC2 <= terciles[1] ~ 1,
    pca_data$PC2 <= terciles[2] ~ 2,
    TRUE ~ 3
  ))

# Convertir Tercil_PC1 y Tercil_PC2 a factores
datos$Tercil_PC1 <- as.factor(datos$Tercil_PC1)
datos2$Tercil_PC2 <- as.factor(datos2$Tercil_PC2)

# Crear un vector con las columnas que se quieren añadir al nuevo data frame
columnas <- c("edad", "altura", "peso", "IMC", "sexo", "Tercil_PC1")

# Crear dos data frames con las columnas anteriores
datos_PC1 <- datos %>% select(all_of(columnas))
datos_PC2 <- datos2 %>% select(all_of(columnas))

# Crear una tabla descriptiva del componente1
tabla1 <- datos_PC1 %>%
  tbl_summary(
    by = Tercil_PC1, # Crear tabla según terciles
    type = all_continuous() ~ "continuous",
    statistic = all_continuous() ~ "{median} ({p25} - {p75})", # Calcular mediana y rangos intercuartílicos para todas las variables continuas
    missing_text = "(missing)"
  ) %>%
  add_p(
    test = all_continuous() ~ "kruskal.test", # Buscar el valor p usando la prueba de Kruskal-Wallis
    pvalue_fun = ~ style_pvalue(.x, digits = 3) # Valor p con 3 decimales
  ) %>%
  as_gt() # Convertir la tabla de gtsummary a un objeto gt

# Guardar la tabla en un archivo .docx
gt::gtsave(tabla1, "tabla_PC1.docx")

# Crear una tabla descriptiva del componente2
tabla2 <- datos_PC2 %>%
  tbl_summary(
    by = Tercil_PC1, # Crear tabla según terciles
    type = all_continuous() ~ "continuous",
    statistic = all_continuous() ~ "{median} ({p25} - {p75})", # Calcular mediana y rangos intercuartílicos para todas las variables continuas
    missing_text = "(missing)"
  ) %>%
  add_p(
    test = all_continuous() ~ "kruskal.test", # Buscar el valor p usando la prueba de Kruskal-Wallis
    pvalue_fun = ~ style_pvalue(.x, digits = 3) # Valor p con 3 decimales
  ) %>%
  as_gt() # Convertir la tabla de gtsummary a un objeto gt

# Guardar la tabla en un archivo .docx
gt::gtsave(tabla2, "tabla_PC2.docx")

# Implementar un modelo de regresión logística
# Verificar la clase de la variable Tercil_PC1
class(datos2$Tercil_PC1)

# Convertir Tercil_PC1 y Tercil_PC2 a factores
datos2$Tercil_PC1 <- as.factor(datos2$Tercil_PC1)
datos2$Tercil_PC2 <- as.factor(datos2$Tercil_PC2)

# Renombrar las columnas para evitar conflictos con nombres
datos2 <- datos2 %>%
  rename(
    Tercil_PC1_ = Tercil_PC1,
    Tercil_PC2_ = Tercil_PC2
  )

# Aplicar el modelo de regresión logística
modelo_logistica <- glm(diab_prev ~ Tercil_PC1_ + Tercil_PC2_, data = datos2, family = "binomial")

# Mostrar un resumen del modelo de regresión logística
summary(modelo_logistica)

# Calcular los intervalos de confianza para los coeficientes del modelo
confint(modelo_logistica) # Intervalo de confianza

# Calcular las Odds Ratios (OR) a partir de los coeficientes del modelo
exp(modelo_logistica$coefficients) # Las OR
exp(coef(modelo_logistica)) # Las OR

# Calcular los intervalos de confianza para las Odds Ratios
exp(confint(modelo_logistica)) # Intervalo de confianza de las OR


# Visualización de variables PCA
fviz_pca_var(pca, col.var = "black")

# Gráfico de contribuciones de las variables a los componentes principales
fviz_pca_var(pca, col.var = "cos2", gradient.cols = c("blue", "yellow", "red"))

# Visualización de cos2 para las variables en los componentes 1 y 2
fviz_cos2(pca, choice = "var", axes = 1:2)

# Visualización de individuos en el espacio de PCA
fviz_pca_ind(pca, col.var = "cos2", gradient.cols = c("blue", "yellow", "red"))

# Realizar K-means clustering y visualizar en el espacio PCA
kmeans <- kmeans(data_scale, centers = 3)
grupo <- as.factor(kmeans$cluster)

fviz_pca_var(pca, col.var = "cluster", gradient.cols = c("grey", "yellow", "blue"))
