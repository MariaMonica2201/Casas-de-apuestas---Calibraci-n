#Cambio de nombre para cada archivo 

source("config.R")

# La carpeta de origen y destino es la misma: renombramos in situ.
CARPETA_ORIGEN <- DIR_RAW
LIGAS_VALIDAS <- LIGAS

# Determina la temporada (formato "1213") a partir de una fecha
# dd/mm/yyyy o dd/mm/yy. Usa agosto como corte (la temporada
# europea arranca en agosto).
inferir_temporada <- function(fechas_txt) {
  fechas <- lubridate::dmy(fechas_txt, quiet = TRUE)
  fechas <- fechas[!is.na(fechas)]
  if (length(fechas) == 0) return(NA_character_)
  fecha_ref <- fechas[1]  # primera fecha del archivo, suele ser representativa
  anio <- lubridate::year(fecha_ref)
  mes  <- lubridate::month(fecha_ref)
  inicio <- if (mes >= 8) anio else anio - 1
  sprintf("%02d%02d", inicio %% 100, (inicio + 1) %% 100)
}

# Busca todos los .csv en la carpeta de origen
candidatos <- list.files(CARPETA_ORIGEN, pattern = "\\.csv$",
                         full.names = TRUE, ignore.case = TRUE)

if (length(candidatos) == 0) {
  stop(sprintf("No se encontraron archivos .csv en %s. Revisa la ruta CARPETA_ORIGEN.",
               CARPETA_ORIGEN))
}

message(sprintf("Se encontraron %d archivos .csv en %s. Analizando...",
                length(candidatos), CARPETA_ORIGEN))

resumen <- data.frame(archivo_original = character(),
                      liga_detectada = character(),
                      temporada_detectada = character(),
                      estado = character(),
                      stringsAsFactors = FALSE)

for (archivo in candidatos) {
  fila <- data.frame(archivo_original = basename(archivo),
                     liga_detectada = NA_character_,
                     temporada_detectada = NA_character_,
                     estado = NA_character_,
                     stringsAsFactors = FALSE)
  
  df <- tryCatch(
    fread(archivo, encoding = "Latin-1", showProgress = FALSE, nrows = 400),
    error = function(e) NULL
  )
  
  if (is.null(df) || !all(c("Div", "Date") %in% names(df))) {
    fila$estado <- "No parece un CSV de football-data.co.uk (faltan columnas Div/Date)"
    resumen <- rbind(resumen, fila)
    next
  }
  
  liga <- unique(df$Div)
  liga <- liga[liga %in% LIGAS_VALIDAS]
  
  if (length(liga) != 1) {
    fila$estado <- sprintf("Liga no reconocida o ambigua (Div = %s)",
                           paste(unique(df$Div), collapse = ", "))
    resumen <- rbind(resumen, fila)
    next
  }
  
  temporada <- inferir_temporada(df$Date)
  if (is.na(temporada)) {
    fila$liga_detectada <- liga
    fila$estado <- "No se pudo interpretar la fecha para inferir temporada"
    resumen <- rbind(resumen, fila)
    next
  }
  
  destino <- file.path(DIR_RAW, sprintf("%s_%s.csv", liga, temporada))
  
  if (normalizePath(archivo) == normalizePath(destino, mustWork = FALSE)) {
    fila$liga_detectada <- liga
    fila$temporada_detectada <- temporada
    fila$estado <- "OK -> ya tenía el nombre correcto"
  } else if (file.exists(destino)) {
    fila$liga_detectada <- liga
    fila$temporada_detectada <- temporada
    fila$estado <- sprintf("Ya existe %s; se dejó %s sin tocar (posible duplicado, revisar a mano)",
                           basename(destino), basename(archivo))
  } else {
    file.rename(archivo, destino)
    fila$liga_detectada <- liga
    fila$temporada_detectada <- temporada
    fila$estado <- sprintf("OK -> renombrado a %s", basename(destino))
  }
  
  resumen <- rbind(resumen, fila)
}

message("\n---- Resumen ----")
print(resumen, row.names = FALSE)

# Verificación final: ¿tenemos todas las combinaciones liga-temporada esperadas?
esperados <- expand.grid(liga = LIGAS_VALIDAS, temporada = TEMPORADAS,
                         stringsAsFactors = FALSE)
esperados$ruta <- file.path(DIR_RAW, sprintf("%s_%s.csv", esperados$liga, esperados$temporada))
esperados$existe <- file.exists(esperados$ruta)

faltantes <- esperados[!esperados$existe, c("liga", "temporada")]
if (nrow(faltantes) > 0) {
  message(sprintf("\nAÚN FALTAN estas combinaciones en %s/:", DIR_RAW))
  print(faltantes, row.names = FALSE)
} else {
  message(sprintf("\n¡Los %d archivos esperados están completos en %s/!", nrow(esperados), DIR_RAW))
}