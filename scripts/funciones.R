
# ============================================================
# LIBRERIAS
# ============================================================

library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(corrplot)


# ============================================================
# FUNCIONES
# ============================================================

tema_proyecto <- function() {
  
  theme_minimal(base_size = 14) +
    
    theme(
      
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      ),
      
      axis.title = element_text(
        face = "bold"
      )
    )
}

# ============================================================
# FUNCION PARA EXPORTAR GRAFICOS
# ============================================================

guardar_grafico <- function(
    grafico,
    nombre_archivo,
    ancho = 10,
    alto = 6
) {
  
  ggsave(
    
    filename = paste0(
      "graficos/",
      nombre_archivo,
      ".png"
    ),
    
    plot = grafico,
    width = ancho,
    height = alto,
    dpi = 300
    
  )
}

# ============================================================
# FUNCION PARA CREAR HISTOGRAMAS
# ============================================================

crear_histograma <- function(
    data,
    variable,
    titulo,
    etiqueta_x,
    color = "lightblue",
    bins = 30
) {
  if (!variable %in% names(data)) {
    
    stop("La variable no existe en el dataset.")
  }
  ggplot(
    data,
    aes(x = .data[[variable]])
  ) +
    
    geom_histogram(
      bins = bins,
      fill = color,
      color = "white"
    ) +
    
    labs(
      title = titulo,
      x = etiqueta_x,
      y = "Frecuencia"
    ) +
    
    tema_proyecto()
  
}

# ============================================================
# FUNCION PARA CREAR BOXPLOTS
# ============================================================

crear_boxplot <- function(
    data,
    variable_x = NULL,
    variable_y,
    titulo,
    etiqueta_x = NULL,
    etiqueta_y,
    color = "lightblue"
) {
  if (!variable_y %in% names(data)) {
    
    stop("La variable_y no existe en el dataset.")
  }
  
  if (!is.null(variable_x) &&
      !variable_x %in% names(data)) {
    
    stop("La variable_x no existe en el dataset.")
  }
  # ----------------------------------------------------------
  # Boxplot simple
  # ----------------------------------------------------------
  
  if (is.null(variable_x)) {
    
    grafico <- ggplot(
      data,
      aes(y = .data[[variable_y]])
    ) +
      
      geom_boxplot(
        fill = color,
        outlier.color = "red"
      )
    
  } else {
    
    # --------------------------------------------------------
    # Boxplot por categorias
    # --------------------------------------------------------
    
    grafico <- ggplot(
      data,
      aes(
        x = .data[[variable_x]],
        y = .data[[variable_y]]
      )
    ) +
      
      geom_boxplot(
        fill = color,
        outlier.color = "red"
      )
  }
  
  # ----------------------------------------------------------
  # Configuracion general
  # ----------------------------------------------------------
  
  grafico +
    
    labs(
      title = titulo,
      x = etiqueta_x,
      y = etiqueta_y
    ) +
    
    tema_proyecto()
  
}

# ============================================================
# FUNCION PARA CREAR GRAFICOS DE BARRAS
# ============================================================

crear_barplot <- function(
    data,
    variable,
    titulo,
    etiqueta_x,
    etiqueta_y = "Frecuencia",
    color = "lightblue"
) {
  if (!variable %in% names(data)) {
    
    stop("La variable no existe en el dataset.")
  }
  ggplot(
    data,
    aes(x = .data[[variable]])
  ) +
    
    geom_bar(
      fill = color
    ) +
    
    labs(
      title = titulo,
      x = etiqueta_x,
      y = etiqueta_y
    ) +
    
    tema_proyecto()
}
# ============================================================
# FUNCION PARA CREAR GRAFICOS DE DISPERSION
# ============================================================

crear_dispersion <- function(
    data,
    variable_x,
    variable_y,
    titulo,
    etiqueta_x,
    etiqueta_y,
    color = "blue",
    logaritmico = FALSE
) {
  if (!variable_y %in% names(data)) {
    
    stop("La variable_y no existe en el dataset.")
  }
  
  if (!is.null(variable_x) &&
      !variable_x %in% names(data)) {
    
    stop("La variable_x no existe en el dataset.")
  }
  # ----------------------------------------------------------
  # Transformacion logaritmica opcional
  # ----------------------------------------------------------
  
  if (logaritmico) {
    
    x_values <- log10(
      data[[variable_x]] + 1
    )
    
    y_values <- log10(
      data[[variable_y]] + 1
    )
    
    data_plot <- data.frame(
      x_values,
      y_values
    )
    
    x_plot <- "x_values"
    y_plot <- "y_values"
    
  } else {
    
    data_plot <- data
    
    x_plot <- variable_x
    y_plot <- variable_y
  }
  
  # ----------------------------------------------------------
  # Crear grafico
  # ----------------------------------------------------------
  
  ggplot(
    data_plot,
    aes(
      x = .data[[x_plot]],
      y = .data[[y_plot]]
    )
  ) +
    
    geom_point(
      color = color,
      alpha = 0.5
    ) +
    
    labs(
      title = titulo,
      x = etiqueta_x,
      y = etiqueta_y
    ) +
    
    tema_proyecto()
}