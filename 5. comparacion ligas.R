# ============================================================
# Proyecto 2 - ¿Están bien calibradas las casas de apuestas?
# Extensión: Comparación entre ligas (definidas en LIGAS, config.R)
# ============================================================
# Contrasta margen y calibración entre las ligas que ya están dentro del
# alcance del proyecto. NO amplía el alcance ni descarga datos nuevos:
# solo desagrega por Liga lo que las Fases 2-3 calcularon de forma agrupada.
#
# Requiere haber corrido antes las Fases 1-2 (necesita
# base_con_probabilidades.csv y distribucion_margen.csv en DIR_OUT).
#
# Preguntas que responde:
#   A) ¿El margen (overround) es distinto entre ligas?
#   B) ¿La calibración es distinta entre ligas? Se usa la MUESTRA COMÚN de la
#      Fase 3 (los mismos partidos para todas las casas). Como el Brier depende
#      también de la dificultad intrínseca de cada liga, se reportan además la
#      fiabilidad de Murphy y la desviación ponderada por liga, que miden
#      descalibración y no dificultad.
#      Los partidos de una liga y otra son eventos distintos: muestras
#      independientes (Mann-Whitney), a diferencia de apertura/cierre (apareada).
# ============================================================

source("config.R")
set.seed(SEMILLA)

base <- fread(file.path(DIR_OUT, "base_con_probabilidades.csv"), encoding = "UTF-8")
distribucion_margen <- fread(file.path(DIR_OUT, "distribucion_margen.csv"), encoding = "UTF-8")

# ---- Muestra común apareada (igual que en las Fases 3 y 4) -------------
cond_comun <- rep(TRUE, nrow(base))
for (op in OPERADORES_PRINCIPALES) {
  colH <- paste0("pnorm_mult_", op, "_H")
  colD <- paste0("pnorm_mult_", op, "_D")
  colA <- paste0("pnorm_mult_", op, "_A")
  if (all(c(colH, colD, colA) %in% names(base))) {
    cond_comun <- cond_comun & (!is.na(base[[colH]]) & !is.na(base[[colD]]) & !is.na(base[[colA]]))
  }
}
base_comun <- base[cond_comun]
mes_txt <- if ("Date_parsed" %in% names(base_comun)) substr(as.character(base_comun$Date_parsed), 1, 7) else ""
base_comun[, bloque_boot := paste(Liga, Temporada, mes_txt)]
message(sprintf("Muestra común apareada: %d partidos (%s)", nrow(base_comun),
                paste(sprintf("%s: %d", LIGAS, sapply(LIGAS, function(l) sum(base_comun$Liga == l))),
                      collapse = ", ")))

# ---- A. Margen por liga -----------------------------------------------
margen_por_liga <- distribucion_margen[
  Operador %in% OPERADORES_PRINCIPALES,
  .(Margen_medio_pct = round(weighted.mean(Margen_medio_pct, N_partidos, na.rm = TRUE), 2),
    N_partidos = sum(N_partidos)),
  by = .(Liga, Operador)
]
setorder(margen_por_liga, Liga, Operador)

message("---- Margen medio por liga y operador ----")
print(margen_por_liga)

# Promedio entre CASAS distintas (sin las cuotas de cierre, para no contar dos
# veces a Pinnacle).
margen_resumen_liga <- margen_por_liga[Operador %in% OPERADORES_CASAS, .(
  Margen_medio_pct = round(mean(Margen_medio_pct), 2)
), by = Liga]
margen_resumen_liga[, Liga_nombre := NOMBRES_LIGA[Liga]]

message(sprintf("\n---- Margen medio general por liga (promedio de las casas: %s) ----",
                paste(OPERADORES_CASAS, collapse = ", ")))
print(margen_resumen_liga[, .(Liga_nombre, Margen_medio_pct)])

# ---- B. Brier score por liga y operador (muestra común) ---------------
calcular_brier_por_liga <- function(dt, operador, liga) {
  colH <- paste0("pnorm_mult_", operador, "_H")
  colD <- paste0("pnorm_mult_", operador, "_D")
  colA <- paste0("pnorm_mult_", operador, "_A")
  
  sub <- dt[Liga == liga]
  yH <- as.integer(sub$FTR == "H"); yD <- as.integer(sub$FTR == "D"); yA <- as.integer(sub$FTR == "A")
  brier <- (sub[[colH]] - yH)^2 + (sub[[colD]] - yD)^2 + (sub[[colA]] - yA)^2
  
  fH <- mean(yH); fD <- mean(yD); fA <- mean(yA)
  brier_base_frec <- (fH - yH)^2 + (fD - yD)^2 + (fA - yA)^2
  
  data.table(
    Liga = liga, Operador = operador, N = nrow(sub),
    Brier_operador = round(mean(brier), 4),
    Brier_base_frecuencia = round(mean(brier_base_frec), 4),
    Mejora_vs_frecuencia_pct = round(100 * (mean(brier_base_frec) - mean(brier)) / mean(brier_base_frec), 2)
  )
}

combinaciones <- CJ(Operador = OPERADORES_PRINCIPALES, Liga = LIGAS)
brier_por_liga <- rbindlist(mapply(
  calcular_brier_por_liga,
  operador = combinaciones$Operador, liga = combinaciones$Liga,
  MoreArgs = list(dt = base_comun), SIMPLIFY = FALSE
))
setorder(brier_por_liga, Operador, Liga)

message("\n---- Brier score por liga y operador (muestra común) ----")
print(brier_por_liga)

# ---- C. Descalibración por liga (fiabilidad de Murphy y desviación ponderada) ----
# El Brier mezcla calibración con dificultad intrínseca del campeonato; la
# fiabilidad y la desviación ponderada miden solo la descalibración.
fiabilidad_resultado <- function(prob, obs, n_bins = N_BINS) {
  ok <- !is.na(prob) & !is.na(obs)
  prob <- prob[ok]; obs <- obs[ok]
  bin <- cut(prob, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE, labels = FALSE)
  d <- data.table(p = prob, o = obs, bin = bin)[, .(n = .N, bp = mean(p), bo = mean(o)), by = bin]
  sum((d$n / length(prob)) * (d$bp - d$bo)^2)
}

calibracion_por_liga <- rbindlist(lapply(OPERADORES_PRINCIPALES, function(op) {
  colH <- paste0("pnorm_mult_", op, "_H")
  colD <- paste0("pnorm_mult_", op, "_D")
  colA <- paste0("pnorm_mult_", op, "_A")
  rbindlist(lapply(LIGAS, function(lg) {
    sub <- base_comun[Liga == lg]
    yH <- as.integer(sub$FTR == "H"); yD <- as.integer(sub$FTR == "D"); yA <- as.integer(sub$FTR == "A")
    rel <- fiabilidad_resultado(sub[[colH]], yH) +
      fiabilidad_resultado(sub[[colD]], yD) +
      fiabilidad_resultado(sub[[colA]], yA)
    largo <- rbindlist(list(
      data.table(p = sub[[colH]], o = yH),
      data.table(p = sub[[colD]], o = yD),
      data.table(p = sub[[colA]], o = yA)
    ))
    largo[, bin := cut(p, breaks = seq(0, 1, length.out = N_BINS + 1), include.lowest = TRUE, labels = FALSE)]
    cur <- largo[, .(N = .N, pred = mean(p), obs = mean(o)), by = bin]
    desv_pond <- sum(cur$N * abs(100 * (cur$obs - cur$pred))) / sum(cur$N)
    data.table(Liga = lg, Operador = op, N_partidos = nrow(sub),
               Fiabilidad_Murphy = round(rel, 5),
               Desviacion_pond_pp = round(desv_pond, 2))
  }))
}))
setorder(calibracion_por_liga, Operador, Liga)

message("\n---- Descalibración por liga: fiabilidad de Murphy y desviación ponderada ----")
print(calibracion_por_liga)

# ---- D. Contraste formal entre ligas ----------------------------------
# Dos ligas: Wilcoxon/Mann-Whitney para MUESTRAS INDEPENDIENTES + IC bootstrap
# por bloques (dentro de cada liga) de la diferencia media de Brier.
# Más de dos ligas: Kruskal-Wallis (sin IC).
boot_dif_ligas <- function(brier, liga, bloque, l1, l2, B = N_BOOT) {
  agrupar <- function(l) {
    i <- liga == l
    bl <- as.integer(factor(bloque[i]))
    list(nb = max(bl),
         sumas = as.numeric(rowsum(brier[i], bl)),
         cuentas = as.numeric(rowsum(rep(1, sum(i)), bl)))
  }
  a1 <- agrupar(l1); a2 <- agrupar(l2)
  media_boot <- function(a) {
    w <- tabulate(sample.int(a$nb, a$nb, replace = TRUE), nbins = a$nb)
    sum(w * a$sumas) / sum(w * a$cuentas)
  }
  dif <- numeric(B)
  for (b in seq_len(B)) dif[b] <- media_boot(a2) - media_boot(a1)
  quantile(dif, c(0.025, 0.975), names = FALSE)
}

comparar_ligas <- function(dt, operador) {
  colH <- paste0("pnorm_mult_", operador, "_H")
  colD <- paste0("pnorm_mult_", operador, "_D")
  colA <- paste0("pnorm_mult_", operador, "_A")
  yH <- as.integer(dt$FTR == "H"); yD <- as.integer(dt$FTR == "D"); yA <- as.integer(dt$FTR == "A")
  brier_partido <- (dt[[colH]] - yH)^2 + (dt[[colD]] - yD)^2 + (dt[[colA]] - yA)^2
  
  if (length(LIGAS) == 2) {
    l1 <- LIGAS[1]; l2 <- LIGAS[2]
    test <- wilcox.test(brier_partido[dt$Liga == l1], brier_partido[dt$Liga == l2])
    ic <- boot_dif_ligas(brier_partido, dt$Liga, dt$bloque_boot, l1, l2)
    data.table(
      Operador = operador,
      Liga_1 = l1, Liga_2 = l2,
      Brier_medio_Liga_1 = round(mean(brier_partido[dt$Liga == l1]), 4),
      Brier_medio_Liga_2 = round(mean(brier_partido[dt$Liga == l2]), 4),
      Dif_Brier_L2_menos_L1 = round(mean(brier_partido[dt$Liga == l2]) - mean(brier_partido[dt$Liga == l1]), 5),
      IC_boot_Dif_inferior = round(ic[1], 5),
      IC_boot_Dif_superior = round(ic[2], 5),
      Prueba = "Mann-Whitney",
      Valor_p = signif(test$p.value, 4)
    )
  } else {
    test <- kruskal.test(brier_partido ~ factor(dt$Liga))
    data.table(Operador = operador, Liga_1 = NA_character_, Liga_2 = NA_character_,
               Brier_medio_Liga_1 = NA_real_, Brier_medio_Liga_2 = NA_real_,
               Dif_Brier_L2_menos_L1 = NA_real_, IC_boot_Dif_inferior = NA_real_,
               IC_boot_Dif_superior = NA_real_,
               Prueba = "Kruskal-Wallis", Valor_p = signif(test$p.value, 4))
  }
}

contraste_ligas <- rbindlist(lapply(OPERADORES_PRINCIPALES, comparar_ligas, dt = base_comun))
contraste_ligas[, Valor_p_Holm := signif(p.adjust(Valor_p, method = "holm"), 4)]

message("\n---- Contraste entre ligas (muestra común) ----")
message("Prueba: Wilcoxon/Mann-Whitney para MUESTRAS INDEPENDIENTES")
message("(correcta aquí porque los partidos de una liga y otra son eventos distintos,")
message("a diferencia de la comparación apertura/cierre de la Fase 4, que sí es apareada).")
print(contraste_ligas)

# ---- Guardar salidas ----------------------------------------------------
fwrite(margen_por_liga, file.path(DIR_OUT, "margen_por_liga.csv"))
fwrite(margen_resumen_liga, file.path(DIR_OUT, "margen_resumen_liga.csv"))
fwrite(brier_por_liga, file.path(DIR_OUT, "brier_por_liga.csv"))
fwrite(calibracion_por_liga, file.path(DIR_OUT, "calibracion_por_liga.csv"))
fwrite(contraste_ligas, file.path(DIR_OUT, "contraste_ligas.csv"))

message(sprintf("\nListo. Archivos guardados en %s/:", DIR_OUT))
message(" - margen_por_liga.csv")
message(" - margen_resumen_liga.csv")
message(" - brier_por_liga.csv")
message(" - calibracion_por_liga.csv")
message(" - contraste_ligas.csv")

# ---- Resumen final ------------------------------------------------------
cat("\n================ RESUMEN: COMPARACIÓN ENTRE LIGAS ================\n")
cat(sprintf("A) Margen medio por liga (promedio de las casas: %s):\n",
            paste(OPERADORES_CASAS, collapse = ", ")))
print(margen_resumen_liga[, .(Liga_nombre, Margen_medio_pct)])
if (length(LIGAS) == 2) {
  m <- margen_resumen_liga[match(LIGAS, Liga), Margen_medio_pct]
  cat(sprintf("-> Diferencia de margen entre ligas: %.2f p.p.\n", abs(m[2] - m[1])))
}
cat("--------------------------------------------------------------------\n")
cat("B) Calibración por liga (muestra común):\n")
print(brier_por_liga[, .(Liga, Operador, N, Brier_operador)])
print(calibracion_por_liga)
cat("--------------------------------------------------------------------\n")
cat("C) Contraste formal entre ligas:\n")
print(contraste_ligas[, .(Operador, Brier_medio_Liga_1, Brier_medio_Liga_2,
                          Dif_Brier_L2_menos_L1, IC_boot_Dif_inferior, IC_boot_Dif_superior,
                          Valor_p, Valor_p_Holm)])
if (any(contraste_ligas$Valor_p_Holm < 0.05, na.rm = TRUE)) {
  cat("-> Hay diferencia estadísticamente significativa (Holm) en el Brier para: ",
      paste(contraste_ligas[Valor_p_Holm < 0.05, Operador], collapse = ", "), "\n")
} else {
  cat("-> No se encontró diferencia significativa (Holm) en el Brier entre ligas.\n")
  cat("   Ausencia de evidencia no demuestra igualdad: ver el IC de la diferencia.\n")
}
cat("====================================================================\n")