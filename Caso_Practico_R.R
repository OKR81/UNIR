
library(ggplot2)     # Para graficos 2D
library(plotly)      # Para graficos 3D
library(readr)       # Para leer archivos CSV
library(stats)       # Para realizar PCA
library(factoextra)  # Para eigenvalue
library(dplyr)
library(nortest)
library(gtsummary)

datos <- read.csv("mubio02_act3_alimentos_nutrientes_4900.csv") #Cargar la base de datos

View(datos)

# Verificar si hay valores NA y eliminarlos

any(is.na(datos))

colSums(is.na(datos))

datos_limpios <- datos[complete.cases(datos),]

any(is.na(datos_limpios))

View(datos_limpios)

# Normalizar todos los datos

datos_limpios_norm <- datos_limpios %>% 
  mutate(across(where(is.numeric), scale))

head(datos_limpios_norm)

View(datos_limpios_norm)

# Extraccion nutrientes y alimentos

datos_alimentos <- datos[ ,28:length(names(datos_limpios_norm))]

# PCA de nutrientes y alimentos

pca <- prcomp(datos_limpios_norm)

print(summary(pca))


eigenvalues <- get_eigenvalue(pca)

print(eigenvalues) # con dos dimensiones explica los datos de Nutrientes y alimentos (PC1 y PC2) por encima de 60-70% (varianza acumulada)

# Comprobacion visual

fviz_eig(pca, addlabels = T, ylim = c(0, 20))

# Vriables mas destacadas segun scores

fviz_contrib(pca, choice = "var", axes = 1, top = 10) 

fviz_contrib(pca, choice = "var", axes = 2, top = 10)

# Terciles PCA1 y PCA2

scores_PCA_df <- as.data.frame(pca$x)

head(scores_PCA_df)

componentes_terciles <- scores_PCA_df %>%
  mutate(across(everything(), ~ ntile(., 3), .names = "tercil_{col}"))

head(componentes_terciles)

componentes_terciles_factor <- componentes_terciles %>%
  mutate(across(starts_with("tercil_"), as.factor))

# Comprobar factor

View(componentes_terciles_factor)

terciles_factor_PC1 <- componentes_terciles_factor[178]

terciles_factor_PC2 <- componentes_terciles_factor[179]

str(terciles_factor_PC1)

View(datos)

# Union variables sociodemograficas y PC1

socio_continuas <- select(datos_limpios_norm, altura, peso, IMC, edad, METs_h_semana)

View(socio_continuas)

socio_continuas_PC1 <-cbind(socio_continuas, terciles_factor_PC1) 

View(socio_continuas_PC1)

class(socio_continuas_PC1)

# Union variables sociodemograficas y PC2

socio_continuas_PC2 <-cbind(socio_continuas, terciles_factor_PC2) 

View(socio_continuas_PC2)

# Tabla resumen PC1

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

tabla_resumen

# Tabla resumen PC2

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

# Union de las dos tablas

tabla_final <- tbl_merge(
  tbls = list(tabla_resumen_PC1, tabla_resumen_PC2),
  tab_spanner = c("**PC1**", "**PC2**"))

tabla_final

# Dataframe (datos) para regresion logistica

## columna diabetes como factor binomial

col_diabetes <- select(datos_limpios, diab_prev)

col_diabetes_factor <- as.factor(col_diabetes$diab_prev)

str(col_diabetes_factor)

head(col_diabetes_factor)

# Dividir en terciles











modelo_logistico <- glm(diabetes ~ PC1_tercil + PC2_tercil + edad + peso + IMC +,
                        family = binomial,
                        data = datos)




# Obtencion p-valores de variables continuas


var_continuas <- c()






# Extraccion de factores

efa <- factanal()















