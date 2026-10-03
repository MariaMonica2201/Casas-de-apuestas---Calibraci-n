# ============================================================
# Proyecto 2 - ¿Están bien calibradas las casas de apuestas?
# Configuración central del proyecto
# ============================================================
# Todos los parámetros que definen el ALCANCE y las decisiones
# metodológicas del análisis viven aquí. Para analizar otra liga,
# otro rango de temporadas u otros operadores, este es el ÚNICO
# archivo que hay que tocar.
# ============================================================

# ---- Alcance: ligas y temporadas --------------------------------
LIGAS <- c("E0", "SP1")                 # E0 = Premier League, SP1 = La Liga
NOMBRES_LIGA <- c(E0 = "Premier League", SP1 = "La Liga")

# Partidos por liga y temporada (para validar la base consolidada).
# Premier League y La Liga: 20 equipos -> 380. Bundesliga: 306.
PARTIDOS_POR_LIGA_TEMPORADA <- c(E0 = 380, SP1 = 380)

# football-data.co.uk codifica la temporada como "AABB" (ej. "1213"
# = temporada 2012/13). anios_inicio define el rango: 2012 -> temporada
# 2012/13, hasta 2019 -> temporada 2019/20.
anios_inicio <- 2012:2019
TEMPORADAS <- sprintf("%02d%02d", anios_inicio %% 100, (anios_inicio + 1) %% 100)
ETIQUETA_PERIODO <- sprintf("%d/%02d a %d/%02d",
                            min(anios_inicio), (min(anios_inicio) + 1) %% 100,
                            max(anios_inicio), (max(anios_inicio) + 1) %% 100)

# ---- Operadores ------------------------------------------------
# Operadores principales para las Fases 3 y 4. Elegidos por tener
# cobertura casi completa (>99%) en todas las combinaciones
# liga-temporada (ver inventario_cobertura.csv).
OPERADORES_PRINCIPALES <- c("B365", "PS", "PSC", "WH")

# Par apertura/cierre de un MISMO operador (Fase 4, parte B).
# En football-data, "PS" son las cuotas previas al cierre (no
# necesariamente de apertura) y "PSC" las de cierre.
PAR_APERTURA_CIERRE <- c(apertura = "PS", cierre = "PSC")

# Casas que se comparan ENTRE SÍ (Friedman/Wilcoxon, Fase 3).
# Se excluyen las cuotas de cierre: PS y PSC son la misma casa en dos
# momentos, y compararlas con las demás mezclaría calidad del operador
# con momento de la cuota (esa comparación se hace aparte, Fase 4-B).
OPERADORES_CASAS <- setdiff(OPERADORES_PRINCIPALES, PAR_APERTURA_CIERRE[["cierre"]])

# ---- Advertencia de calidad de la fuente (Pinnacle) -------------
# Football-Data advierte que desde el 23 de julio de 2025 las cuotas
# de Pinnacle son poco confiables (API de entrega desactualizada). Las
# cuotas de estos prefijos se anulan (NA) para partidos desde esa
# fecha. Con el alcance 2012/13-2019/20 no afecta a ninguna fila, pero
# protege el flujo si se añaden temporadas nuevas.
PINNACLE_FECHA_CORTE <- as.Date("2025-07-23")
PINNACLE_PREFIJOS    <- c("PS", "PSC")

# ---- Sensibilidad por COVID-19 ----------------------------------
# Temporada interrumpida por la pandemia y fecha (aprox.) de
# reanudación por liga. Los partidos desde esa fecha se jugaron a
# puerta cerrada. VERIFICAR las fechas antes de citarlas en el informe.
COVID_TEMPORADA    <- "1920"
COVID_REANUDACION  <- c(E0 = as.Date("2020-06-17"), SP1 = as.Date("2020-06-11"))

# ---- Parámetros metodológicos -------------------------------------
N_BINS              <- 10             # bins de la curva de calibración (esquema principal: igual ancho)
N_BINS_SENSIBILIDAD <- c(5, 10, 20)   # bins para la sensibilidad al agrupamiento
N_BINS_FAVLONG      <- 20             # bins para el análisis favorito-longshot
N_MIN_BIN           <- 30             # bins con menos observaciones se marcan como poco confiables
N_BOOT              <- 500            # réplicas bootstrap (IC por partido y por bloques)
SEMILLA             <- 20262          # semilla para reproducibilidad del bootstrap

# ---- Rutas de carpetas ---------------------------------------------
DIR_RAW <- "data/raw"
DIR_OUT <- "outputs"

dir.create(DIR_RAW, recursive = TRUE, showWarnings = FALSE)
dir.create(DIR_OUT, recursive = TRUE, showWarnings = FALSE)

# ---- Paquetes usados en todo el flujo (única lista del proyecto) ----
# Librería de usuario en Windows (si existe)
lib_usuario <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library",
                         paste0(R.version$major, ".", substr(R.version$minor, 1, 1)))
if (dir.exists(lib_usuario) && !lib_usuario %in% .libPaths()) {
  .libPaths(c(lib_usuario, .libPaths()))
}

paquetes_proyecto <- c("data.table", "dplyr", "purrr", "readr", "stringr",
                       "lubridate", "binom", "sandwich",
                       "ggplot2", "scales", "knitr", "rmarkdown")
faltantes <- setdiff(paquetes_proyecto, rownames(installed.packages()))
if (length(faltantes) > 0) {
  install.packages(faltantes, repos = "https://cloud.r-project.org")
}
# Los paquetes usados solo en el reporte (ggplot2, scales, knitr, rmarkdown)
# se cargan desde el Rmd; aquí solo se garantiza que estén instalados.
paquetes_cargar <- c("data.table", "dplyr", "purrr", "readr", "stringr", "lubridate", "binom")
invisible(lapply(paquetes_cargar, library, character.only = TRUE))