# ============================================================
# Proyecto 2 - ¿Están bien calibradas las casas de apuestas?
# Script Maestro de Ejecución Reproducible (run_all.R)
# ============================================================
# Orquesta el flujo completo de análisis desde los datos limpios
# hasta la generación automática del reporte de calibración.
# Cumple con el Entregable 2 y 3 de la rúbrica.
# ============================================================

cat("\n############################################################\n")
cat("# INICIANDO FLUJO COMPLETO DEL PROYECTO\n")
cat("############################################################\n\n")

tiempo_inicio <- Sys.time()

# 1. Ajuste automático de directorio de trabajo si se ejecuta desde subcarpetas
if (!file.exists("config.R")) {
  if (file.exists("../config.R")) {
    setwd("..")
    cat("Ajustando directorio de trabajo a la raíz del proyecto:", getwd(), "\n")
  } else if (file.exists("../../config.R")) {
    setwd("../..")
    cat("Ajustando directorio de trabajo a la raíz del proyecto:", getwd(), "\n")
  }
}

# 2. Configuración central (paquetes, rutas y librería de usuario viven en config.R)
source("config.R", encoding = "UTF-8")

# 3. Función resiliente para localizar y ejecutar scripts (independiente de tildes o codificación)
ejecutar_script <- function(patron, nombre_fase) {
  carpetas_busqueda <- c("scripts", ".")
  candidatos <- unlist(lapply(carpetas_busqueda, function(d) {
    if (dir.exists(d)) list.files(d, full.names = TRUE) else character(0)
  }))
  # Búsqueda por patrón insensible a mayúsculas
  archivo <- grep(patron, candidatos, value = TRUE, ignore.case = TRUE)
  if (length(archivo) == 0) {
    stop(sprintf("\n[ERROR] No se pudo encontrar el script para '%s'.\nPatrón buscado: '%s'\nDirectorio actual: '%s'\n",
                 nombre_fase, patron, getwd()))
  }
  archivo_seleccionado <- archivo[1]
  cat(sprintf("\n>>> %s\n-> Archivo: %s\n\n", toupper(nombre_fase), basename(archivo_seleccionado)))
  source(archivo_seleccionado, encoding = "UTF-8")
}

# ---- FASE 0 Y 1: Ingesta y Limpieza --------------------------------
# Por defecto el flujo EXIGE los CSV crudos en data/raw/. Solo se permite
# arrancar desde una base_consolidada.csv ya generada si se activa de forma
# explícita (USAR_BASE_CONSOLIDADA <- TRUE, por ejemplo en config.R).
# Así nunca se corre "en silencio" con resultados de una corrida anterior.
if (!exists("USAR_BASE_CONSOLIDADA")) USAR_BASE_CONSOLIDADA <- FALSE

archivos_raw <- list.files(DIR_RAW, pattern = "\\.csv$", full.names = TRUE)
base_consolidada <- file.path(DIR_OUT, "base_consolidada.csv")

if (length(archivos_raw) > 0) {
  ejecutar_script("0.*renombre", "Fase 0: Renombre de las bases de datos")
  ejecutar_script("1.*limpieza", "Fase 1: Verificación de calidad y limpieza de datos")
} else if (USAR_BASE_CONSOLIDADA && file.exists(base_consolidada)) {
  warning("No hay CSV crudos en ", DIR_RAW, ". Se omiten las Fases 0 y 1 y se usa ",
          base_consolidada, " (USAR_BASE_CONSOLIDADA = TRUE). ",
          "Los resultados NO se regeneraron desde los datos originales.",
          call. = FALSE)
} else {
  stop(sprintf(paste0(
    "\n[ERROR] No hay archivos .csv en '%s'.\n",
    "Coloca ahí los CSV descargados de football-data.co.uk (ver README) y vuelve a correr.\n",
    "Si solo quieres reutilizar una base ya consolidada, define USAR_BASE_CONSOLIDADA <- TRUE ",
    "(requiere '%s')."), DIR_RAW, base_consolidada), call. = FALSE)
}

# ---- FASE 2: Tratamiento del Margen y Probabilidades ---------------
ejecutar_script("2.*margen", "Fase 2: Conversión a probabilidades y remoción del margen (Multiplicativo, Aditivo, Shin)")

# ---- FASE 3: Evaluación de la Calibración ---------------------------
ejecutar_script("3.*calibraci", "Fase 3: Evaluación de calibración, Murphy, RPS y contrastes apareados")

# ---- FASE 4: Desviaciones Sistemáticas -----------------------------
ejecutar_script("4.*desviaci", "Fase 4: Sesgo favorito-longshot y Apertura vs. Cierre")

# ---- FASE 5: Comparación entre Ligas -------------------------------
ejecutar_script("5.*liga", sprintf("Fase 5: Comparación entre ligas (%s)",
                                   paste(NOMBRES_LIGA[LIGAS], collapse = " vs. ")))

# ---- FASE 6: Autogeneración del Reporte (Entregable 3) --------------
cat("\n\n>>> FASE 6: Compilando Reporte de Calibración Autogenerado (reporte_calibracion.Rmd)...\n\n")
if (file.exists("reporte_calibracion.Rmd")) {
  # Asegurar detección de Pandoc si está en RStudio/Quarto
  ruta_pandoc_rstudio <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools"
  if (dir.exists(ruta_pandoc_rstudio) && Sys.getenv("RSTUDIO_PANDOC") == "") {
    Sys.setenv(RSTUDIO_PANDOC = ruta_pandoc_rstudio)
  }
  
  # El .Rmd normalmente corre este script en su primer chunk. Como aquí ya se
  # ejecutó todo el flujo, se avisa al .Rmd para que no lo repita (evita un ciclo).
  options(proyecto.en_pipeline = TRUE)
  
  rmarkdown::render(
    input = "reporte_calibracion.Rmd",
    output_file = normalizePath(file.path(DIR_OUT, "reporte_calibracion.html"), mustWork = FALSE),
    quiet = TRUE
  )
  options(proyecto.en_pipeline = NULL)
  cat(sprintf("¡Reporte HTML autogenerado con éxito en %s/reporte_calibracion.html!\n", DIR_OUT))
} else {
  warning("No se encontró reporte_calibracion.Rmd en la raíz del proyecto.")
}

tiempo_fin <- Sys.time()

cat("\n\n############################################################\n")
cat("# FLUJO COMPLETO TERMINADO EXITOSAMENTE\n")
cat(sprintf("# Tiempo total: %.2f minutos\n", as.numeric(difftime(tiempo_fin, tiempo_inicio, units = "mins"))))
cat("############################################################\n")
cat(sprintf("\nArchivos generados en %s/:\n", DIR_OUT))
for (f in list.files(DIR_OUT, pattern = "\\.(csv|html)$")) {
  cat(sprintf("  - %s\n", f))
}