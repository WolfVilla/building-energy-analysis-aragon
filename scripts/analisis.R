# ============================================================
# PROYECTO FINAL
# Analisis energetico de edificios de Aragon
# Curso: Exploracion y Analisis de Datos Estadisticos
# ============================================================

# ============================================================
# LIBRERIAS
# ============================================================

library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(corrplot)

# ============================================================
# IMPORTAR FUNCIONES
# ============================================================

source("scripts/funciones.R")

# ============================================================
# 1. CARGA DE DATOS
# ============================================================

datos <- read.csv(
  "data//Energia_Aragon.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

cat("Base cargada correctamente\n")

# ============================================================
# 2. REVISION INICIAL DEL CONJUNTO DE DATOS
# ============================================================
#
# Esta seccion permite identificar:
# - cantidad de observaciones y variables,
# - tipos de datos disponibles,
# - estructura general de la base,
# - posibles inconsistencias iniciales.
#
# Las funciones utilizadas permiten explorar
# rapidamente la calidad y composicion del dataset.

head(datos)

dim(datos)

names(datos)

str(datos)

summary(datos)

# ------------------------------------------------------------
# Limpieza y conversion de variables de fecha
# ------------------------------------------------------------
#
# Se eliminan formatos horarios innecesarios y se convierten
# las fechas al formato Date de R para facilitar
# comparaciones temporales y validacion de registros.

datos$Fecha_emision <- as.character(datos$Fecha_emision)
datos$Fecha_expiracion <- as.character(datos$Fecha_expiracion)

datos$Fecha_emision <- gsub(" 0:00| 00:00", "", datos$Fecha_emision)
datos$Fecha_expiracion <- gsub(" 0:00| 00:00", "", datos$Fecha_expiracion)

convertir_fecha <- function(x) {
  
  as.Date(
    x,
    tryFormats = c(
      "%Y-%m-%d",
      "%d/%m/%Y",
      "%m/%d/%Y"
    )
  )
}

datos$Fecha_emision <- convertir_fecha(datos$Fecha_emision)

datos$Fecha_expiracion <- convertir_fecha(datos$Fecha_expiracion)


# ------------------------------------------------------------
# Conversion de variables cuantitativas
# ------------------------------------------------------------
#
# Las variables numericas son convertidas explicitamente
# para asegurar que las operaciones estadisticas
# y graficas sean calculadas correctamente.

datos$Emision_CO2 <- as.numeric(datos$Emision_CO2)

datos$`ConsumoKWh/m2/Anio` <- as.numeric(
  datos$`ConsumoKWh/m2/Anio`
)

datos$Superficie_m2 <- as.numeric(
  datos$Superficie_m2
)

datos$Anio_construccion <- as.integer(
  datos$Anio_construccion
)

datos$Dias_hasta_expiracion <- as.integer(
  datos$Dias_hasta_expiracion
)


# ------------------------------------------------------------
# Eliminar duplicados
# ------------------------------------------------------------

duplicados <- sum(duplicated(datos))

cat("Duplicados encontrados:", duplicados, "\n")

datos <- datos[!duplicated(datos), ]
cat("Filas despues de eliminar duplicados:",
    nrow(datos),
    "\n")


# ------------------------------------------------------------
# Eliminacion de categorias invalidas
# ------------------------------------------------------------
#
# Se eliminan registros con clasificaciones representadas
# mediante el simbolo "-" debido a que no aportan
# informacion util para el analisis energetico.

datos <- datos[
  datos$Clasificacion_consumo != "-",
]


# ------------------------------------------------------------
# Filtrado de años de construccion inconsistentes
# ------------------------------------------------------------
#
# Se eliminan registros con años imposibles o fuera
# de rangos razonables para evitar distorsiones
# en el analisis temporal de edificaciones.

datos <- datos[
  !is.na(datos$Anio_construccion) &
    datos$Anio_construccion >= 1000 &
    datos$Anio_construccion <= 2026,
]


# ------------------------------------------------------------
# Valores faltantes
# ------------------------------------------------------------

faltantes <- colSums(is.na(datos))

tabla_faltantes <- data.frame(
  Variable = names(faltantes),
  Cantidad_NA = faltantes,
  Porcentaje_NA = round(
    faltantes / nrow(datos) * 100,
    2
  )
)

print(tabla_faltantes)

cat(
  "\nTotal de valores faltantes:",
  sum(faltantes),
  "\n"
)

# ============================================================
# 4. ANALISIS DESCRIPTIVO NUMERICO
# ============================================================

variables_numericas <- c(
  "Emision_CO2",
  "ConsumoKWh/m2/Anio",
  "Superficie_m2",
  "Anio_construccion",
  "Dias_hasta_expiracion"
)


# ------------------------------------------------------------
# Construccion de tabla resumen estadistica
# ------------------------------------------------------------
#
# Se calculan medidas de tendencia central
# y dispersion para describir el comportamiento
# general de las variables numericas.
#
# Se incluye:
# - media,
# - mediana,
# - desviacion estandar,
# - extremos,
# - IQR.

tabla_resumen <- data.frame(
  
  Variable = variables_numericas,
  
  Media = sapply(
    datos[variables_numericas],
    mean,
    na.rm = TRUE
  ),
  
  Mediana = sapply(
    datos[variables_numericas],
    median,
    na.rm = TRUE
  ),
  
  Desviacion_Estandar = sapply(
    datos[variables_numericas],
    sd,
    na.rm = TRUE
  ),
  
  Minimo = sapply(
    datos[variables_numericas],
    min,
    na.rm = TRUE
  ),
  
  Maximo = sapply(
    datos[variables_numericas],
    max,
    na.rm = TRUE
  ),
  
  IQR = sapply(
    datos[variables_numericas],
    IQR,
    na.rm = TRUE
  )
)

print(tabla_resumen)

# ============================================================
# MATRIZ DE CORRELACION
# ============================================================
#
# Se calcula una matriz de correlacion de Spearman
# debido a:
# - asimetria,
# - presencia de valores extremos,
# - distribuciones no normales.
#
# Esto permite identificar asociaciones monotónicas
# entre las variables numericas principales.

correlaciones <- cor(
  
  datos[variables_numericas],
  
  method = "spearman",
  
  use = "complete.obs"
)

print(correlaciones)


# ------------------------------------------------------------
# Exportar matriz de correlacion
# ------------------------------------------------------------

write.csv(
  
  correlaciones,
  
  "outputs/tabla_correlaciones.csv"
)

# ------------------------------------------------------------
# Heatmap de correlaciones
# ------------------------------------------------------------

png(
  "graficos/heatmap_correlaciones.png",
  width = 900,
  height = 700
)

corrplot(
  
  correlaciones,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)

dev.off()

# ============================================================
# IDENTIFICACION DE VALORES ATIPICOS
# ============================================================
#
# Se identifican observaciones extremadamente elevadas
# utilizando el criterio del percentil 99.
#
# Esto permite detectar registros que pueden:
# - distorsionar analisis,
# - afectar visualizaciones,
# - influir en medidas estadisticas.

limite_consumo <- quantile(
  datos$`ConsumoKWh/m2/Anio`,
  0.99,
  na.rm = TRUE
)
outliers_consumo <- datos[
  datos$`ConsumoKWh/m2/Anio` > limite_consumo,
]
cat(
  "\nCantidad de outliers en consumo:",
  nrow(outliers_consumo),
  "\n"
)


limite_emisiones <- quantile(
  datos$Emision_CO2,
  0.99,
  na.rm = TRUE
)
outliers_emisiones <- datos[
  datos$Emision_CO2 > limite_emisiones,
]
cat(
  "\nCantidad de outliers en emision:",
  nrow(outliers_emisiones),
  "\n"
)

# ------------------------------------------------------------
# Exportar observaciones atipicas
# ------------------------------------------------------------

write.csv(
  outliers_consumo,
  "outputs/outliers_consumo.csv",
  row.names = FALSE
)

write.csv(
  outliers_emisiones,
  "outputs/outliers_emisiones.csv",
  row.names = FALSE
)


# ============================================================
# 5. GRAFICOS NUMERICOS
# ============================================================

dir.create("graficos", showWarnings = FALSE)


# ------------------------------------------------------------
# Histograma de emisiones de CO2
# ------------------------------------------------------------
#
# Permite visualizar:
# - distribucion de frecuencias,
# - asimetria,
# - concentracion de observaciones,
# - posible presencia de valores extremos.

grafico_emisiones <- crear_histograma(
  data = datos,
  variable = "Emision_CO2",
  titulo = "Distribucion de emisiones CO2",
  etiqueta_x = "Emisiones CO2",
  color = "lightblue"
)

guardar_grafico(
  grafico_emisiones,
  "histograma_emisiones"
)

# ------------------------------------------------------------
# Histograma consumo
# ------------------------------------------------------------

grafico_consumo <- crear_histograma(
  data = datos,
  variable = "ConsumoKWh/m2/Anio",
  titulo = "Distribucion del consumo energetico",
  etiqueta_x = "Consumo kWh/m2/año",
  color = "lightgreen"
)

guardar_grafico(
  grafico_consumo,
  "histograma_consumo"
)


# ------------------------------------------------------------
# Boxplot de emisiones de CO2
# ------------------------------------------------------------
#
# Utilizado para identificar:
# - dispersion,
# - mediana,
# - rango intercuartilico,
# - valores atipicos.

boxplot_emisiones <- crear_boxplot(
  data = datos,
  variable_y = "Emision_CO2",
  titulo = "Boxplot emisiones CO2",
  etiqueta_y = "Emisiones CO2",
  color = "lightblue"
)

guardar_grafico(
  boxplot_emisiones,
  "boxplot_emisiones"
)

# ------------------------------------------------------------
# Boxplot consumo
# ------------------------------------------------------------

boxplot_consumo <- crear_boxplot(
  data = datos,
  variable_y = "ConsumoKWh/m2/Anio",
  titulo = "Boxplot consumo energetico",
  etiqueta_y = "Consumo kWh/m2/año",
  color = "lightgreen"
)

guardar_grafico(
  boxplot_consumo,
  "boxplot_consumo"
)

# ============================================================
# 6. ANALISIS CATEGORICO
# ============================================================

# ------------------------------------------------------------
# Funcion para analisis de variables categoricas
# ------------------------------------------------------------
#
# La funcion calcula:
# - frecuencias absolutas,
# - porcentajes,
# - categoria dominante.
#
# Esto permite resumir rapidamente el comportamiento
# de variables cualitativas.

analisis_variable <- function(variable, nombre) {
  
  cat("\n=========================\n")
  cat("Variable:", nombre, "\n")
  cat("=========================\n")
  
  tabla <- table(variable)
  
  tabla <- tabla[names(tabla) != "-"]
  
  porcentaje <- round(
    prop.table(tabla) * 100,
    2
  )
  
  print(tabla)
  
  cat("\nPorcentajes (%):\n")
  
  print(porcentaje)
  
  dominante <- names(tabla)[which.max(tabla)]
  
  cat("\nCategoria dominante:",
      dominante,
      "\n")
}


analisis_variable(
  datos$Clasificacion_consumo,
  "Clasificacion consumo"
)

analisis_variable(
  datos$Clasificacion_Emisiones,
  "Clasificacion emisiones"
)

analisis_variable(
  datos$Tipo_edificio,
  "Tipo edificio"
)

analisis_variable(
  datos$Estado_edificio,
  "Estado edificio"
)

analisis_variable(
  datos$Provincia,
  "Provincia"
)


# ============================================================
# 7. GRAFICOS CATEGORICOS
# ============================================================

grafico_bar_consumo <- crear_barplot(
  data = datos,
  variable = "Clasificacion_consumo",
  titulo = "Clasificacion de consumo",
  etiqueta_x = "Clasificacion energetica",
  color = "lightgreen"
)

guardar_grafico(
  grafico_bar_consumo,
  "barplot_consumo"
)


grafico_bar_emisiones <- crear_barplot(
  data = datos,
  variable = "Clasificacion_Emisiones",
  titulo = "Clasificacion de emisiones",
  etiqueta_x = "Clasificacion energetica",
  color = "lightblue"
)

guardar_grafico(
  grafico_bar_emisiones,
  "barplot_emisiones"
)


grafico_bar_provincia <- crear_barplot(
  data = datos,
  variable = "Provincia",
  titulo = "Distribucion por provincia",
  etiqueta_x = "Provincia",
  color = "lightcoral"
)

guardar_grafico(
  grafico_bar_provincia,
  "barplot_provincia"
)

# ============================================================
# 8. ANALISIS BIVARIADO
# ============================================================

# ------------------------------------------------------------
# Relacion entre consumo energetico y emisiones
# ------------------------------------------------------------
#
# El grafico de dispersion permite explorar
# asociaciones entre ambas variables e identificar:
# - tendencias generales,
# - dispersion,
# - concentracion de observaciones,
# - posibles valores extremos.

grafico_dispersion <- crear_dispersion(
  data = datos,
  variable_x = "ConsumoKWh/m2/Anio",
  variable_y = "Emision_CO2",
  titulo = "Relacion entre consumo y emisiones",
  etiqueta_x = "Consumo kWh/m2/año",
  etiqueta_y = "Emisiones CO2",
  color = "blue",
  logaritmico = FALSE
)

guardar_grafico(
  grafico_dispersion,
  "dispersion_consumo_emisiones"
)


# ============================================================
# Analisis logaritmico consumo vs emisiones
# ============================================================
#
# Debido a la presencia de valores extremos muy elevados,
# se aplica una transformacion logaritmica para reducir
# problemas de escala y visualizar con mayor claridad
# la relacion general entre las variables.
#
# Se utiliza log10(x + 1) para evitar problemas
# con observaciones iguales a cero.

grafico_dispersion_log <- crear_dispersion(
  data = datos,
  variable_x = "ConsumoKWh/m2/Anio",
  variable_y = "Emision_CO2",
  titulo = "Relacion logaritmica entre consumo y emisiones",
  etiqueta_x = "log10(Consumo kWh/m2/año + 1)",
  etiqueta_y = "log10(Emisiones CO2 + 1)",
  color = "blue",
  logaritmico = TRUE
)

guardar_grafico(
  grafico_dispersion_log,
  "dispersion_logaritmica"
)


# ------------------------------------------------------------
# Correlacion logaritmica
# ------------------------------------------------------------

correlacion_log <- cor(
  log10(datos$`ConsumoKWh/m2/Anio` + 1),
  log10(datos$Emision_CO2 + 1),
  method = "spearman",
  use = "complete.obs"
)

cat(
  "\nCorrelacion logaritmica Spearman:",
  correlacion_log,
  "\n"
)

# ------------------------------------------------------------
# Correlacion no parametrica de Spearman
# ------------------------------------------------------------
#
# Se utiliza Spearman debido a:
# - asimetria en las distribuciones,
# - presencia de valores extremos,
# - posible ausencia de linealidad.
#
# Esta correlacion evalua asociaciones monotónicas
# entre las variables.

correlacion <- cor(
  datos$`ConsumoKWh/m2/Anio`,
  datos$Emision_CO2,
  
  method = "spearman",
  use = "complete.obs"
)

cat(
  "\nCorrelacion Spearman:",
  correlacion,
  "\n"
)

# ============================================================
# MODELO DE REGRESION LINEAL
# ============================================================
#
# Se construye un modelo de regresion lineal
# para explorar como distintas variables
# pueden asociarse con las emisiones de CO2.
#
# Variables utilizadas:
# - consumo energetico,
# - superficie,
# - año de construccion.
#
# El modelo tiene caracter exploratorio
# y no debe interpretarse como causalidad.

modelo_regresion <- lm(
  
  Emision_CO2 ~
    
    `ConsumoKWh/m2/Anio` +
    
    Superficie_m2 +
    
    Anio_construccion,
  
  data = datos
)

summary(modelo_regresion)

cat(
  "\nR-cuadrado:",
  round(summary(modelo_regresion)$r.squared, 4),
  "\n"
)

cat(
  "\nCoeficientes del modelo:\n"
)

print(
  coef(summary(modelo_regresion))
)

# ------------------------------------------------------------
# Exportar resumen del modelo
# ------------------------------------------------------------

capture.output(
  
  summary(modelo_regresion),
  
  file = "outputs/modelo_regresion.txt"
)

# ------------------------------------------------------------
# Grafico de regresion
# ------------------------------------------------------------

png(
  "graficos/diagnostico_regresion.png",
  width = 1200,
  height = 900
)

par(mfrow = c(2, 2))

plot(modelo_regresion)

dev.off()

par(mfrow = c(1, 1))

# ------------------------------------------------------------
# Boxplot consumo por clasificacion
# ------------------------------------------------------------

grafico_consumo_clasificacion <- crear_boxplot(
  data = datos,
  variable_x = "Clasificacion_consumo",
  variable_y = "ConsumoKWh/m2/Anio",
  titulo = "Consumo por clasificacion energetica",
  etiqueta_x = "Clasificacion",
  etiqueta_y = "Consumo kWh/m2/año",
  color = "lightgreen"
)

guardar_grafico(
  grafico_consumo_clasificacion,
  "consumo_clasificacion"
)

# ------------------------------------------------------------
# Boxplot emisiones por clasificacion
# ------------------------------------------------------------

grafico_emisiones_clasificacion <- crear_boxplot(
  data = datos,
  variable_x = "Clasificacion_Emisiones",
  variable_y = "Emision_CO2",
  titulo = "Emisiones por clasificacion energetica",
  etiqueta_x = "Clasificacion",
  etiqueta_y = "Emisiones CO2",
  color = "lightblue"
)

guardar_grafico(
  grafico_emisiones_clasificacion,
  "emisiones_clasificacion"
)


# ------------------------------------------------------------
# Agrupacion de edificios por periodo de construccion
# ------------------------------------------------------------
#
# Los años son agrupados en intervalos historicos
# para facilitar comparaciones energeticas
# entre edificaciones de distintas epocas.

datos$Periodo_construccion <- cut(
  
  datos$Anio_construccion,
  
  breaks = c(
    0,
    1900,
    1950,
    1980,
    2000,
    2010,
    2026
  ),
  
  labels = c(
    "<=1900",
    "1901-1950",
    "1951-1980",
    "1981-2000",
    "2001-2010",
    "2011-2026"
  )
)


# ------------------------------------------------------------
# Boxplot consumo por periodo
# ------------------------------------------------------------

grafico_consumo_periodo <- crear_boxplot(
  data = datos,
  variable_x = "Periodo_construccion",
  variable_y = "ConsumoKWh/m2/Anio",
  titulo = "Consumo por periodo de construccion",
  etiqueta_x = "Periodo",
  etiqueta_y = "Consumo kWh/m2/año",
  color = "lightyellow"
)

guardar_grafico(
  grafico_consumo_periodo,
  "consumo_periodo"
)


# ============================================================
# 9. EXPORTACION DE RESULTADOS
# ============================================================
#
# Se exportan tablas resumen para:
# - documentacion del analisis,
# - inclusion en el informe,
# - respaldo de resultados estadisticos.

write.csv(
  tabla_resumen,
  "outputs/tabla_resumen_numerico.csv",
  row.names = FALSE
)

write.csv(
  tabla_faltantes,
  "outputs/tabla_valores_faltantes.csv",
  row.names = FALSE
)

cat("\nProyecto ejecutado correctamente.\n")