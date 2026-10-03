# ============================================================
# Proyecto 2 - ¿Están bien calibradas las casas de apuestas?
# Fase 3: Evaluación de la calibración e inferencia estadística
# ============================================================
# Lee base_con_probabilidades.csv (Fase 2).
# Incluye:
# 1. Muestra común apareada (partidos con cuotas en todos los operadores)
# 2. Curvas de calibración con intervalos de Wilson e intervalos bootstrap
#    por partido (los 3 resultados de un mismo partido no son independientes)
# 3. Curvas desagregadas por resultado (H, D, A) y por liga
# 4. Reglas de puntuación: Brier Score vs. líneas base
# 5. BONIFICACIÓN: Descomposición de Murphy (Reliability, Resolution, Uncertainty)
# 6. BONIFICACIÓN: Ranked Probability Score (RPS) para el mercado ordinal 1X2
# 7. Sensibilidad de conclusiones (Multiplicativo vs. Aditivo vs. Shin)
# 8. Inferencia apareada ENTRE CASAS (Friedman + Wilcoxon con Holm + IC bootstrap
#    por bloques de la diferencia media). Las cuotas de cierre se comparan aparte (Fase 4).
# 9. Hosmer-Lemeshow SEPARADO POR RESULTADO (H/D/A), con desviación simple y ponderada
# 10. Sensibilidad al esquema de agrupamiento y a los partidos post-reanudación COVID-19
# ============================================================

source("config.R")
set.seed(SEMILLA)

base <- fread(file.path(DIR_OUT, "base_con_probabilidades.csv"), encoding = "UTF-8")

# ---- 0. Muestra común apareada --------------------------------------
# Para que la comparación entre operadores sea formalmente válida y apareada,
# filtramos los partidos donde TODOS los operadores principales tienen datos
# completos de cuotas (evitando sesgos de selección muestral).
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
message(sprintf("Muestra común apareada construida: %d partidos (%.1f%% del total de %d)",
                nrow(base_comun), 100 * nrow(base_comun) / nrow(base), nrow(base)))

# Identificador de partido (para bootstrap por partido) y bloque temporal
# (liga-temporada-mes) para el bootstrap por bloques de la inferencia apareada.
base_comun[, id_partido := .I]
mes_txt <- if ("Date_parsed" %in% names(base_comun)) substr(as.character(base_comun$Date_parsed), 1, 7) else ""
base_comun[, bloque_boot := paste(Liga, Temporada, mes_txt)]

# Marca de partidos posteriores a la reanudación por COVID-19 (creada en la Fase 1)
if (!"Post_reanudacion_COVID" %in% names(base_comun)) {
  warning("La base no trae Post_reanudacion_COVID (vuelve a correr la Fase 1); se asume FALSE.")
  base_comun[, Post_reanudacion_COVID := FALSE]
}

# Indicadores observados binarios (muestra común completa)
oH <- as.integer(base_comun$FTR == "H")
oD <- as.integer(base_comun$FTR == "D")
oA <- as.integer(base_comun$FTR == "A")

# ---- 1. Formato largo por operador y método -------------------------
construir_formato_largo <- function(dt, operador, metodo = "mult") {
  colH <- paste0("pnorm_", metodo, "_", operador, "_H")
  colD <- paste0("pnorm_", metodo, "_", operador, "_D")
  colA <- paste0("pnorm_", metodo, "_", operador, "_A")
  
  if (!all(c(colH, colD, colA) %in% names(dt))) return(NULL)
  
  largo <- rbindlist(list(
    dt[!is.na(get(colH)), .(id_partido, Liga, Temporada, Resultado_evaluado = "H",
                            Prob_predicha = get(colH), Ocurrio = as.integer(FTR == "H"))],
    dt[!is.na(get(colD)), .(id_partido, Liga, Temporada, Resultado_evaluado = "D",
                            Prob_predicha = get(colD), Ocurrio = as.integer(FTR == "D"))],
    dt[!is.na(get(colA)), .(id_partido, Liga, Temporada, Resultado_evaluado = "A",
                            Prob_predicha = get(colA), Ocurrio = as.integer(FTR == "A"))]
  ))
  largo[, `:=`(Operador = operador, Metodo = metodo)]
  largo
}

# Formato largo principal (Multiplicativo)
datos_largos_mult <- rbindlist(lapply(OPERADORES_PRINCIPALES, construir_formato_largo,
                                      dt = base_comun, metodo = "mult"))

# Formatos largos alternativos para sensibilidad
datos_largos_adit <- rbindlist(lapply(OPERADORES_PRINCIPALES, construir_formato_largo,
                                      dt = base_comun, metodo = "adit"))
datos_largos_shin <- rbindlist(lapply(OPERADORES_PRINCIPALES, construir_formato_largo,
                                      dt = base_comun, metodo = "shin"))

# Diagnóstico explícito de filas excluidas por no convergencia de Shin.
# normalizar_shin() (Fase 2) devuelve NA cuando uniroot no converge o la cuota
# bruta es <=0; construir_formato_largo() descarta esas filas, y aquí se
# documenta cuántos partidos se pierden por operador al usar Shin.
resumen_na_shin <- rbindlist(lapply(OPERADORES_PRINCIPALES, function(op) {
  colH <- paste0("pnorm_shin_", op, "_H")
  if (!colH %in% names(base_comun)) return(NULL)
  data.table(
    Operador = op,
    N_total = nrow(base_comun),
    N_excluidos_shin_NA = sum(is.na(base_comun[[colH]])),
    Pct_excluidos = round(100 * sum(is.na(base_comun[[colH]])) / nrow(base_comun), 2)
  )
}))
message("\n---- Filas excluidas por no convergencia de Shin (por operador) ----")
print(resumen_na_shin)

# ---- 2. Curvas de calibración: Wilson + bootstrap por partido --------
# Los intervalos de Wilson tratan cada fila del formato largo como independiente,
# pero cada partido aporta 3 filas (H, D, A) dependientes entre sí (si ocurrió H,
# no ocurrieron D ni A). Por eso se agregan intervalos bootstrap percentil que
# remuestrean PARTIDOS completos (cluster bootstrap), que sí respetan esa dependencia.
boot_curva_cluster <- function(d, B = N_BOOT) {
  ids <- unique(d$id_partido)
  n <- length(ids)
  idx <- match(d$id_partido, ids)
  bins <- d$bin
  y <- d$Ocurrio
  nb <- max(bins)
  frec <- matrix(NA_real_, nrow = B, ncol = nb)
  for (b in seq_len(B)) {
    w  <- tabulate(sample.int(n, n, replace = TRUE), nbins = n)
    wi <- w[idx]
    r_den <- rowsum(wi, bins)
    r_num <- rowsum(wi * y, bins)
    den <- numeric(nb); num <- numeric(nb)
    den[as.integer(rownames(r_den))] <- r_den[, 1]
    num[as.integer(rownames(r_num))] <- r_num[, 1]
    frec[b, ] <- ifelse(den > 0, num / den, NA_real_)
  }
  cuantil <- function(x, p) {
    if (all(is.na(x))) NA_real_ else quantile(x, p, na.rm = TRUE, names = FALSE)
  }
  data.table(
    bin = seq_len(nb),
    IC_boot_inferior = apply(frec, 2, cuantil, p = 0.025),
    IC_boot_superior = apply(frec, 2, cuantil, p = 0.975)
  )
}

construir_curva_calibracion <- function(dt, n_bins = N_BINS, esquema = "igual_ancho",
                                        grupo_vars = c("Operador"), bootstrap = FALSE) {
  d <- copy(dt)
  if (esquema == "igual_ancho") {
    d[, bin := cut(Prob_predicha, breaks = seq(0, 1, length.out = n_bins + 1),
                   include.lowest = TRUE, labels = FALSE)]
  } else if (esquema == "igual_frecuencia") {
    d[, bin := cut(rank(Prob_predicha, ties.method = "first"),
                   breaks = n_bins, include.lowest = TRUE, labels = FALSE)]
  } else {
    stop("esquema debe ser 'igual_ancho' o 'igual_frecuencia'")
  }
  
  vars_agrup <- c(grupo_vars, "bin")
  curva <- d[, {
    n <- .N
    aciertos <- sum(Ocurrio)
    ic <- binom::binom.confint(aciertos, n, methods = "wilson")
    .(
      Prob_predicha_media = mean(Prob_predicha),
      Frecuencia_observada = aciertos / n,
      IC_inferior = ic$lower,
      IC_superior = ic$upper,
      N = n
    )
  }, by = vars_agrup]
  
  curva[, Desviacion_pp := 100 * (Frecuencia_observada - Prob_predicha_media)]
  curva[, Bin_poco_poblado := N < N_MIN_BIN]
  
  if (bootstrap) {
    grupos <- unique(d[, ..grupo_vars])
    boots <- rbindlist(lapply(seq_len(nrow(grupos)), function(i) {
      g <- grupos[i]
      dd <- d[g, on = grupo_vars]
      res <- boot_curva_cluster(dd)
      cbind(g[rep(1, nrow(res))], res)
    }))
    curva <- merge(curva, boots, by = vars_agrup, all.x = TRUE)
  }
  setorderv(curva, vars_agrup)
  curva
}

# Desviación media absoluta PONDERADA por N (equivalente a un ECE): evita que bins
# con muy pocas observaciones (p. ej. N = 7) pesen igual que bins con miles.
desv_abs_pond <- function(curva) sum(curva$N * abs(curva$Desviacion_pp)) / sum(curva$N)

curva_igual_ancho <- construir_curva_calibracion(datos_largos_mult, N_BINS, "igual_ancho",
                                                 "Operador", bootstrap = TRUE)
curva_igual_frec  <- construir_curva_calibracion(datos_largos_mult, N_BINS, "igual_frecuencia", "Operador")

# Curvas desagregadas por resultado (H, D, A)
curva_por_resultado <- construir_curva_calibracion(datos_largos_mult, N_BINS, "igual_ancho",
                                                   c("Operador", "Resultado_evaluado"))

# Curvas desagregadas por liga
curva_por_liga <- construir_curva_calibracion(datos_largos_mult, N_BINS, "igual_ancho",
                                              c("Operador", "Liga"))

message(sprintf("\n---- Curva de calibración general (igual ancho, %d bins; IC Wilson e IC bootstrap por partido) ----", N_BINS))
print(curva_igual_ancho)

# ---- 3. Reglas de puntuación: Brier Score vs. Líneas Base -----------
# Todas las funciones calculan los indicadores observados desde dt$FTR, de modo
# que sirven para la muestra completa y para submuestras (sensibilidad COVID).
# Brier multiclase (suma sobre los 3 resultados): el máximo de la línea base
# uniforme es 2/3; no es comparable con el Brier binario por resultado.
calcular_brier_completo <- function(dt, operador, metodo = "mult") {
  colH <- paste0("pnorm_", metodo, "_", operador, "_H")
  colD <- paste0("pnorm_", metodo, "_", operador, "_D")
  colA <- paste0("pnorm_", metodo, "_", operador, "_A")
  yH <- as.integer(dt$FTR == "H"); yD <- as.integer(dt$FTR == "D"); yA <- as.integer(dt$FTR == "A")
  
  pH <- dt[[colH]]; pD <- dt[[colD]]; pA <- dt[[colA]]
  
  brier_partido <- (pH - yH)^2 + (pD - yD)^2 + (pA - yA)^2
  n_validos <- sum(!is.na(brier_partido))
  brier_op <- mean(brier_partido, na.rm = TRUE)
  
  # Líneas base (frecuencias de la propia muestra; la base uniforme no depende de los datos)
  fH <- mean(yH); fD <- mean(yD); fA <- mean(yA)
  brier_base_frec <- mean((fH - yH)^2 + (fD - yD)^2 + (fA - yA)^2)
  brier_base_unif <- mean((1/3 - yH)^2 + (1/3 - yD)^2 + (1/3 - yA)^2)
  
  data.table(
    Operador = operador,
    Metodo = metodo,
    N_partidos = n_validos,
    Brier_operador = round(brier_op, 4),
    Brier_linea_base_frecuencia = round(brier_base_frec, 4),
    Brier_linea_base_uniforme = round(brier_base_unif, 4),
    Mejora_vs_frecuencia_pct = round(100 * (brier_base_frec - brier_op) / brier_base_frec, 2),
    Mejora_vs_uniforme_pct = round(100 * (brier_base_unif - brier_op) / brier_base_unif, 2)
  )
}

tabla_brier <- rbindlist(lapply(OPERADORES_PRINCIPALES, calcular_brier_completo,
                                dt = base_comun, metodo = "mult"))
message("\n---- Brier Score por operador (Muestra común, Multiplicativo) ----")
print(tabla_brier)

# ---- 4. BONIFICACIÓN: Descomposición de Murphy del Brier ------------
# Brier multiclase = Reliability - Resolution + Uncertainty
# Para cada resultado binario j in {H, D, A}:
#   UNC_j = bar_o_j * (1 - bar_o_j)
#   REL_j = sum_k (n_k / N) * (bar_p_jk - bar_o_jk)^2
#   RES_j = sum_k (n_k / N) * (bar_o_jk - bar_o_j)^2
descomposicion_murphy <- function(prob, observacion, n_bins = 10) {
  validos <- !is.na(prob) & !is.na(observacion)
  prob <- prob[validos]
  observacion <- observacion[validos]
  
  N <- length(prob)
  bar_o <- mean(observacion)
  unc <- bar_o * (1 - bar_o)
  
  bins <- cut(prob, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE, labels = FALSE)
  df_bin <- data.table(p = prob, o = observacion, bin = bins)[, .(
    n = .N,
    bar_p = mean(p),
    bar_o_k = mean(o)
  ), by = bin]
  
  rel <- sum((df_bin$n / N) * (df_bin$bar_p - df_bin$bar_o_k)^2)
  res <- sum((df_bin$n / N) * (df_bin$bar_o_k - bar_o)^2)
  brier <- rel - res + unc
  
  # Brier real (sin agrupar). La identidad REL - RES + UNC es exacta solo si todas
  # las predicciones dentro de un bin son iguales; al agrupar en bins de ancho
  # fijo queda un término intra-bin que se reporta aparte en vez de ocultarlo.
  brier_real <- mean((prob - observacion)^2)
  
  list(Reliability = rel, Resolution = res, Uncertainty = unc, Brier = brier,
       Brier_real = brier_real)
}

calcular_murphy_operador <- function(dt, operador, metodo = "mult") {
  colH <- paste0("pnorm_", metodo, "_", operador, "_H")
  colD <- paste0("pnorm_", metodo, "_", operador, "_D")
  colA <- paste0("pnorm_", metodo, "_", operador, "_A")
  yH <- as.integer(dt$FTR == "H"); yD <- as.integer(dt$FTR == "D"); yA <- as.integer(dt$FTR == "A")
  
  mH <- descomposicion_murphy(dt[[colH]], yH, N_BINS)
  mD <- descomposicion_murphy(dt[[colD]], yD, N_BINS)
  mA <- descomposicion_murphy(dt[[colA]], yA, N_BINS)
  
  # Suma multiclase
  rel_total <- mH$Reliability + mD$Reliability + mA$Reliability
  res_total <- mH$Resolution + mD$Resolution + mA$Resolution
  unc_total <- mH$Uncertainty + mD$Uncertainty + mA$Uncertainty
  brier_total <- rel_total - res_total + unc_total
  brier_real_total <- mH$Brier_real + mD$Brier_real + mA$Brier_real
  
  data.table(
    Operador = operador,
    Metodo = metodo,
    Fiabilidad_Reliability = round(rel_total, 5),
    Resolucion_Resolution = round(res_total, 5),
    Incertidumbre_Uncertainty = round(unc_total, 5),
    Brier_calculado = round(brier_total, 4),
    Brier_real = round(brier_real_total, 4),
    Termino_intrabin = round(brier_real_total - brier_total, 4)
  )
}

tabla_murphy <- rbindlist(lapply(OPERADORES_PRINCIPALES, calcular_murphy_operador,
                                 dt = base_comun, metodo = "mult"))
message("\n---- BONIFICACIÓN: Descomposición de Murphy del Brier ----")
print(tabla_murphy)
message("Nota metodológica: Fiabilidad mide descalibramiento (cercano a 0 es ideal);")
message("Resolución mide capacidad discriminativa (mayor es mejor); Incertidumbre es constante.")

# ---- 5. BONIFICACIÓN: Ranked Probability Score (RPS) ----------------
# El RPS penaliza más fuerte los pronósticos alejados del resultado en la escala
# ordenada natural: Victoria Local (H) <-> Empate (D) <-> Victoria Visitante (A).
# RPS = 0.5 * [ (p_H - o_H)^2 + ((p_H + p_D) - (o_H + o_D))^2 ]
calcular_rps_operador <- function(dt, operador, metodo = "mult") {
  colH <- paste0("pnorm_", metodo, "_", operador, "_H")
  colD <- paste0("pnorm_", metodo, "_", operador, "_D")
  yH <- as.integer(dt$FTR == "H"); yD <- as.integer(dt$FTR == "D")
  
  pH <- dt[[colH]]; pD <- dt[[colD]]
  
  # Función acumulada
  P1 <- pH; O1 <- yH
  P2 <- pH + pD; O2 <- yH + yD
  
  rps_partido <- 0.5 * ((P1 - O1)^2 + (P2 - O2)^2)
  rps_op <- mean(rps_partido, na.rm = TRUE)
  
  # Líneas base RPS
  fH <- mean(yH); fD <- mean(yD)
  rps_base_frec <- mean(0.5 * ((fH - yH)^2 + ((fH + fD) - (yH + yD))^2))
  rps_base_unif <- mean(0.5 * ((1/3 - yH)^2 + ((2/3) - (yH + yD))^2))
  
  data.table(
    Operador = operador,
    Metodo = metodo,
    RPS_operador = round(rps_op, 4),
    RPS_linea_base_frecuencia = round(rps_base_frec, 4),
    RPS_linea_base_uniforme = round(rps_base_unif, 4),
    Mejora_vs_frecuencia_pct = round(100 * (rps_base_frec - rps_op) / rps_base_frec, 2),
    Mejora_vs_uniforme_pct = round(100 * (rps_base_unif - rps_op) / rps_base_unif, 2)
  )
}

tabla_rps <- rbindlist(lapply(OPERADORES_PRINCIPALES, calcular_rps_operador,
                              dt = base_comun, metodo = "mult"))
message("\n---- BONIFICACIÓN: Ranked Probability Score (RPS) ----")
print(tabla_rps)

# ---- 6. Sensibilidad real de conclusiones entre métodos --------------
# Compara cómo cambian Brier, RPS, Fiabilidad de Murphy y la desviación de
# calibración (simple y ponderada por N) al pasar de Multiplicativo -> Aditivo -> Shin.
sensibilidad_conclusiones <- rbindlist(lapply(c("mult", "adit", "shin"), function(met) {
  rbindlist(lapply(OPERADORES_PRINCIPALES, function(op) {
    colH <- paste0("pnorm_", met, "_", op, "_H")
    if (!colH %in% names(base_comun)) return(NULL)
    
    br <- calcular_brier_completo(base_comun, op, met)
    rp <- calcular_rps_operador(base_comun, op, met)
    mu <- calcular_murphy_operador(base_comun, op, met)
    
    dt_largo_tmp <- construir_formato_largo(base_comun, op, met)
    curva_tmp <- construir_curva_calibracion(dt_largo_tmp, N_BINS, "igual_ancho", "Operador")
    
    data.table(
      Operador = op,
      Metodo = met,
      Brier = br$Brier_operador,
      RPS = rp$RPS_operador,
      Fiabilidad_Murphy = mu$Fiabilidad_Reliability,
      Desviacion_media_abs_pp = round(mean(abs(curva_tmp$Desviacion_pp)), 2),
      Desviacion_pond_pp = round(desv_abs_pond(curva_tmp), 2)
    )
  }))
}))

message("\n---- Sensibilidad de conclusiones entre métodos (Mult vs Adit vs Shin) ----")
print(sensibilidad_conclusiones)

# ---- 7. Pruebas apareadas ENTRE CASAS (muestra común) ---------------
# Todas las casas cotizan sobre los mismos partidos, así que se aplica:
# a) Prueba global de Friedman (medidas repetidas no paramétricas)
# b) Wilcoxon apareado con corrección de Holm
# c) IC bootstrap por bloques (liga-temporada-mes) de la diferencia MEDIA de Brier:
#    Wilcoxon contrasta pseudomedianas, no la media; el bootstrap por bloques
#    respeta la dependencia entre partidos de una misma liga, temporada y mes.
# Solo se comparan OPERADORES_CASAS (sin cuotas de cierre): PS y PSC son la misma
# casa en dos momentos; esa comparación se hace en la Fase 4 (apertura vs cierre).
matriz_brier <- sapply(OPERADORES_CASAS, function(op) {
  pH <- base_comun[[paste0("pnorm_mult_", op, "_H")]]
  pD <- base_comun[[paste0("pnorm_mult_", op, "_D")]]
  pA <- base_comun[[paste0("pnorm_mult_", op, "_A")]]
  (pH - oH)^2 + (pD - oD)^2 + (pA - oA)^2
})

boot_dif_media <- function(dif, bloque, B = N_BOOT) {
  bl <- as.integer(factor(bloque))
  nb <- max(bl)
  sumas <- as.numeric(rowsum(dif, bl))
  cuentas <- as.numeric(rowsum(rep(1, length(dif)), bl))
  medias <- numeric(B)
  for (b in seq_len(B)) {
    w <- tabulate(sample.int(nb, nb, replace = TRUE), nbins = nb)
    medias[b] <- sum(w * sumas) / sum(w * cuentas)
  }
  quantile(medias, c(0.025, 0.975), names = FALSE)
}

# Prueba global de Friedman
test_friedman <- friedman.test(matriz_brier)
message("\n---- Prueba global de Friedman entre casas (datos apareados) ----")
cat(sprintf("Casas comparadas: %s\n", paste(OPERADORES_CASAS, collapse = ", ")))
cat(sprintf("Chi-cuadrado Friedman = %.2f, gl = %d, Valor-p = %g\n",
            test_friedman$statistic, test_friedman$parameter, test_friedman$p.value))

# Pruebas apareadas por pares con corrección de Holm
pares <- combn(OPERADORES_CASAS, 2, simplify = FALSE)
comparaciones_apareadas <- rbindlist(lapply(pares, function(par) {
  op1 <- par[1]; op2 <- par[2]
  brier1 <- matriz_brier[, op1]
  brier2 <- matriz_brier[, op2]
  
  test_w <- wilcox.test(brier1, brier2, paired = TRUE)
  diff_media <- mean(brier1) - mean(brier2)
  ic_boot <- boot_dif_media(brier1 - brier2, base_comun$bloque_boot)
  
  data.table(
    Comparacion = sprintf("%s vs %s", op1, op2),
    Brier_Op1 = round(mean(brier1), 4),
    Brier_Op2 = round(mean(brier2), 4),
    Dif_Brier = round(diff_media, 5),
    IC_boot_Dif_inferior = round(ic_boot[1], 5),
    IC_boot_Dif_superior = round(ic_boot[2], 5),
    Mejora_pct = round(100 * -diff_media / mean(brier2), 2),
    Wilcoxon_V = unname(test_w$statistic),
    Valor_p_crudo = test_w$p.value
  )
}))

comparaciones_apareadas[, Valor_p_ajustado_Holm := p.adjust(Valor_p_crudo, method = "holm")]
comparaciones_apareadas[, Significativo_Holm_0.05 := Valor_p_ajustado_Holm < 0.05]

message("\n---- Comparaciones apareadas por pares (Wilcoxon + Holm; IC bootstrap por bloques de la dif. media) ----")
print(comparaciones_apareadas[, .(Comparacion, Dif_Brier, IC_boot_Dif_inferior, IC_boot_Dif_superior,
                                  Mejora_pct,
                                  Valor_p_crudo = signif(Valor_p_crudo, 4),
                                  Valor_p_ajustado_Holm = signif(Valor_p_ajustado_Holm, 4),
                                  Significativo_Holm_0.05)])

# ---- 8. Prueba de Hosmer-Lemeshow SEPARADA POR RESULTADO ------------
# Cada partido aporta 3 filas al formato largo (H, D, A) que NO son
# independientes entre sí (si ocurrió H, no ocurrieron D ni A). Por eso la
# prueba se corre por separado para cada resultado: dentro de una categoría
# cada partido aporta una sola fila. (La dependencia más amplia entre partidos
# de la misma temporada/jornada queda como limitación declarada.)
#
# Grados de libertad: las probabilidades son EXÓGENAS (las emite el mercado,
# no un modelo ajustado a estos datos), así que no se estima ningún parámetro
# y los grados de libertad son gl = G, el número de bins ocupados. La regla
# G - 2 es para un modelo logístico ajustado sobre la misma muestra. Como no
# hay un único estándar para este caso, también se guardan los p-valores con
# gl = G - 1 y gl = G - 2 para mostrar si la conclusión depende de esa elección.
#
# Se evalúa con los TRES métodos de remoción del margen. Dentro de cada método,
# las pruebas (operadores x 3 resultados) forman una familia y se ajustan con
# Holm. Bins_esperado_menor_5 cuenta los bins donde alguna frecuencia esperada
# (de ocurrir o no ocurrir) es menor que 5, pues ahí la aproximación
# chi-cuadrado es menos confiable.
prueba_hosmer_lemeshow <- function(dt_largo, operador, resultado, metodo = "mult",
                                   n_bins = N_BINS) {
  d <- dt_largo[Operador == operador & Resultado_evaluado == resultado & Metodo == metodo]
  d[, bin := cut(Prob_predicha, breaks = seq(0, 1, length.out = n_bins + 1),
                 include.lowest = TRUE, labels = FALSE)]
  
  resumen <- d[, .(
    N = .N,
    Observado = sum(Ocurrio),
    Predicho = sum(Prob_predicha)
  ), by = bin]
  
  resumen[, chi2_termino := (Observado - Predicho)^2 / (Predicho * (1 - Predicho / N))]
  resumen[, Desviacion_pp := 100 * (Observado / N - Predicho / N)]
  
  estadistico <- sum(resumen$chi2_termino, na.rm = TRUE)
  gl <- sum(!is.na(resumen$chi2_termino))   # bins ocupados = G
  p_valor <- pchisq(estadistico, df = gl, lower.tail = FALSE)
  
  data.table(
    Operador = operador,
    Metodo = metodo,
    Resultado = resultado,
    Estadistico_HL = round(estadistico, 2),
    Grados_libertad = gl,
    Valor_p = signif(p_valor, 4),
    Valor_p_gl_menos1 = signif(pchisq(estadistico, df = gl - 1, lower.tail = FALSE), 4),
    Valor_p_gl_menos2 = signif(pchisq(estadistico, df = gl - 2, lower.tail = FALSE), 4),
    Bins_esperado_menor_5 = sum(resumen$Predicho < 5 | (resumen$N - resumen$Predicho) < 5),
    Rechaza_calibracion_perfecta_0.05 = p_valor < 0.05,
    Desviacion_media_abs_pp = round(mean(abs(resumen$Desviacion_pp)), 2),
    Desviacion_pond_abs_pp = round(sum(resumen$N * abs(resumen$Desviacion_pp)) / sum(resumen$N), 2)
  )
}

correr_hl_completo <- function(dt_largo, metodos = c("mult", "adit", "shin")) {
  comb <- CJ(Metodo = metodos, Operador = OPERADORES_PRINCIPALES,
             Resultado = c("H", "D", "A"), sorted = FALSE)
  tab <- rbindlist(mapply(
    prueba_hosmer_lemeshow,
    metodo = comb$Metodo,
    operador = comb$Operador,
    resultado = comb$Resultado,
    MoreArgs = list(dt_largo = dt_largo),
    SIMPLIFY = FALSE
  ))
  # Corrección de Holm dentro de cada método (familia de pruebas)
  tab[, Valor_p_Holm := signif(p.adjust(Valor_p, method = "holm"), 4), by = Metodo]
  tab[, Rechaza_Holm_0.05 := Valor_p_Holm < 0.05]
  tab
}

resumir_hl <- function(tab) {
  tab[, .(
    Pruebas = .N,
    Rechazos_sin_ajuste = sum(Rechaza_calibracion_perfecta_0.05),
    Rechazos_con_Holm = sum(Rechaza_Holm_0.05)
  ), by = Metodo]
}

datos_largos_hl <- rbindlist(list(datos_largos_mult, datos_largos_adit, datos_largos_shin))
tabla_hl <- correr_hl_completo(datos_largos_hl)
resumen_hl <- resumir_hl(tabla_hl)

message("\n---- Hosmer-Lemeshow separado por resultado (H/D/A), gl = G (bins ocupados) ----")
print(tabla_hl)
message("\n---- Resumen de rechazos por método ----")
print(resumen_hl)
message("\nRECORDATORIO: un valor_p significativo NO implica que la desviación")
message("importe en la práctica. Revisa siempre las desviaciones (simple y ponderada) junto al p-valor.")
message("Compara Valor_p, Valor_p_gl_menos1 y Valor_p_gl_menos2: si un rechazo cambia")
message("con los grados de libertad, la conclusión depende de esa elección.")

# ---- 9. Sensibilidad al esquema de agrupamiento ---------------------
comparar_esquemas <- function() {
  resultados <- list()
  for (esquema in c("igual_ancho", "igual_frecuencia")) {
    for (nb in N_BINS_SENSIBILIDAD) {
      curva_tmp <- construir_curva_calibracion(datos_largos_mult, nb, esquema, "Operador")
      resumen <- curva_tmp[, .(
        Esquema = esquema, N_bins = nb,
        Desviacion_media_abs_pp = round(mean(abs(Desviacion_pp)), 2),
        Desviacion_pond_pp = round(sum(N * abs(Desviacion_pp)) / sum(N), 2),
        Bins_poco_poblados = sum(Bin_poco_poblado)
      ), by = Operador]
      resultados[[length(resultados) + 1]] <- resumen
    }
  }
  rbindlist(resultados)
}

sensibilidad_agrupamiento <- comparar_esquemas()
message("\n---- Sensibilidad al esquema de agrupamiento (simple vs. ponderada por N) ----")
print(sensibilidad_agrupamiento)

# ---- 10. Sensibilidad: sin partidos posteriores a la reanudación COVID-19 --
# Los partidos de COVID_TEMPORADA desde la reanudación se jugaron a puerta
# cerrada (ventaja de local distinta). Se repiten Brier, RPS, Murphy, desviación
# ponderada y Hosmer-Lemeshow (los tres métodos) sin ellos.
n_post_covid <- sum(base_comun$Post_reanudacion_COVID)
base_sin_covid <- base_comun[Post_reanudacion_COVID == FALSE]
escenarios <- list("Muestra común completa" = base_comun,
                   "Sin partidos post-reanudación COVID-19" = base_sin_covid)

sensibilidad_covid <- rbindlist(lapply(names(escenarios), function(nom) {
  dt_e <- escenarios[[nom]]
  rbindlist(lapply(OPERADORES_PRINCIPALES, function(op) {
    br <- calcular_brier_completo(dt_e, op, "mult")
    rp <- calcular_rps_operador(dt_e, op, "mult")
    mu <- calcular_murphy_operador(dt_e, op, "mult")
    cur <- construir_curva_calibracion(construir_formato_largo(dt_e, op, "mult"),
                                       N_BINS, "igual_ancho", "Operador")
    data.table(Escenario = nom, Operador = op, N_partidos = nrow(dt_e),
               Brier = br$Brier_operador, RPS = rp$RPS_operador,
               Fiabilidad_Murphy = mu$Fiabilidad_Reliability,
               Desviacion_pond_pp = round(desv_abs_pond(cur), 2))
  }))
}))

# El Hosmer-Lemeshow se repite con los tres métodos de remoción del margen: un
# rechazo que sobrevive a Holm con un método puede no sobrevivir al excluir los
# partidos a puerta cerrada, y solo se sabe si se evalúa cada método.
METODOS_COVID_HL <- c("mult", "adit", "shin")
hl_covid_det <- list()
hl_covid <- rbindlist(lapply(names(escenarios), function(nom) {
  dt_e <- escenarios[[nom]]
  largo_e <- rbindlist(lapply(METODOS_COVID_HL, function(m) {
    rbindlist(lapply(OPERADORES_PRINCIPALES, construir_formato_largo,
                     dt = dt_e, metodo = m))
  }))
  tab_e <- correr_hl_completo(largo_e, metodos = METODOS_COVID_HL)
  hl_covid_det[[nom]] <<- data.table(Escenario = nom,
                                     tab_e[, .(Metodo, Operador, Resultado, Valor_p, Valor_p_Holm, Rechaza_Holm_0.05)])
  res_e <- resumir_hl(tab_e)
  res_e[, Escenario := nom]
  setcolorder(res_e, "Escenario")
  res_e
}))

message(sprintf("\n---- Sensibilidad COVID-19: %d partidos post-reanudación excluidos ----", n_post_covid))
print(sensibilidad_covid)
print(hl_covid)

# ---- 10b. ¿Cambió la ventaja de local? y estabilidad del rechazo -----
# (1) Para la victoria local (H), compara la tasa observada con la probabilidad
#     implícita media en los partidos posteriores a la reanudación y en el resto.
#     Es una comprobación descriptiva (z sobre la suma de probabilidades), no
#     entra en ninguna familia de Holm.
# (2) Control por remoción aleatoria: si quitar los partidos de COVID elimina un
#     rechazo de Hosmer-Lemeshow, ¿ocurre lo mismo al quitar el mismo número de
#     partidos elegidos al azar? Se repite el Hosmer-Lemeshow completo (los 12
#     contrastes del método, con Holm) en cada réplica.
if (!exists("N_REMUESTRAS_ALEATORIAS")) N_REMUESTRAS_ALEATORIAS <- 200
GRUPO_POST  <- "Post-reanudación COVID-19"
GRUPO_RESTO <- "Resto de la muestra común"

desv_local <- function(dt_e, op, metodo) {
  p <- dt_e[[paste0("pnorm_", metodo, "_", op, "_H")]]
  o <- as.numeric(dt_e$FTR == "H")
  ok <- !is.na(p)
  p <- p[ok]
  o <- o[ok]
  z <- (sum(o) - sum(p)) / sqrt(sum(p * (1 - p)))
  data.table(N = length(p),
             Tasa_observada = mean(o),
             Prob_implicita_media = mean(p),
             Desviacion_pp = 100 * (mean(o) - mean(p)),
             z = z,
             Valor_p = 2 * pnorm(-abs(z)))
}

grupos_local <- list(base_comun[Post_reanudacion_COVID == TRUE], base_sin_covid)
names(grupos_local) <- c(GRUPO_POST, GRUPO_RESTO)
grupos_local <- Filter(function(g) nrow(g) > 0, grupos_local)

ventaja_local_covid <- rbindlist(lapply(names(grupos_local), function(g) {
  rbindlist(lapply(c("mult", "shin"), function(m) {
    rbindlist(lapply(OPERADORES_PRINCIPALES, function(op) {
      cbind(data.table(Grupo = g, Metodo = m, Operador = op),
            desv_local(grupos_local[[g]], op, m))
    }))
  }))
}))

rech_completa <- tabla_hl[Rechaza_Holm_0.05 == TRUE, .(Metodo, Operador, Resultado)]
# hl_covid_det es una lista (un data.table por escenario): se unen antes de filtrar
hl_covid_det_dt <- rbindlist(hl_covid_det)
p_sin_covid <- hl_covid_det_dt[Escenario == names(escenarios)[2],
                               .(Metodo, Operador, Resultado, Valor_p_sin_covid = Valor_p)]
p_completa <- tabla_hl[, .(Metodo, Operador, Resultado, Valor_p_completa = Valor_p)]

remocion_aleatoria <- data.table()
if (n_post_covid > 0 && nrow(rech_completa) > 0) {
  set.seed(SEMILLA + 1)
  message(sprintf("\nControl por remoción aleatoria de %d partidos (%d réplicas por método; puede tardar)...",
                  n_post_covid, N_REMUESTRAS_ALEATORIAS))
  res_rem <- list()
  for (m in unique(rech_completa$Metodo)) {
    objetivo <- rech_completa[Metodo == m]
    reps <- vector("list", N_REMUESTRAS_ALEATORIAS)
    for (b in seq_len(N_REMUESTRAS_ALEATORIAS)) {
      quitar <- sample.int(nrow(base_comun), n_post_covid)
      largo_b <- rbindlist(lapply(OPERADORES_PRINCIPALES, construir_formato_largo,
                                  dt = base_comun[-quitar], metodo = m))
      tab_b <- correr_hl_completo(largo_b, metodos = m)
      reps[[b]] <- merge(objetivo,
                         tab_b[, .(Metodo, Operador, Resultado,
                                   Valor_p_b = Valor_p, Rechaza_b = Rechaza_Holm_0.05)],
                         by = c("Metodo", "Operador", "Resultado"))
    }
    res_rem[[m]] <- rbindlist(reps)
  }
  todas_rem <- rbindlist(res_rem)
  todas_rem <- merge(todas_rem, p_sin_covid, by = c("Metodo", "Operador", "Resultado"))
  remocion_aleatoria <- todas_rem[, .(
    N_remuestras = .N,
    Valor_p_sin_covid = Valor_p_sin_covid[1],
    Prop_mantiene_Holm = mean(Rechaza_b),
    P_mediana = median(Valor_p_b),
    P_p5 = unname(quantile(Valor_p_b, 0.05)),
    P_p95 = unname(quantile(Valor_p_b, 0.95)),
    Prop_aleat_p_mayor_o_igual = mean(Valor_p_b >= Valor_p_sin_covid[1])
  ), by = .(Metodo, Operador, Resultado)]
  remocion_aleatoria <- merge(p_completa, remocion_aleatoria,
                              by = c("Metodo", "Operador", "Resultado"))
  setcolorder(remocion_aleatoria, c("Metodo", "Operador", "Resultado", "Valor_p_completa",
                                    "Valor_p_sin_covid"))
}

message("\n---- Victoria local (H): post-reanudación vs. resto, método multiplicativo ----")
if (nrow(ventaja_local_covid) > 0) {
  print(ventaja_local_covid[Metodo == "mult",
                            .(Grupo, Operador, N, Tasa_observada = round(Tasa_observada, 3),
                              Prob_implicita_media = round(Prob_implicita_media, 3),
                              Desviacion_pp = round(Desviacion_pp, 2), Valor_p = round(Valor_p, 3))])
}
message("\n---- Control: remoción aleatoria de partidos (rechazos con Holm en la muestra completa) ----")
if (nrow(remocion_aleatoria) > 0) {
  print(remocion_aleatoria[, .(Metodo, Operador, Resultado, Valor_p_completa, Valor_p_sin_covid,
                               Prop_mantiene_Holm = round(Prop_mantiene_Holm, 2),
                               P_p5 = signif(P_p5, 2), P_mediana = signif(P_mediana, 2),
                               P_p95 = signif(P_p95, 2),
                               Prop_aleat_p_mayor_o_igual = round(Prop_aleat_p_mayor_o_igual, 2))])
}

# ---- 11. Guardar salidas --------------------------------------------
fwrite(curva_igual_ancho, file.path(DIR_OUT, "curva_calibracion_igual_ancho.csv"))
fwrite(curva_igual_frec, file.path(DIR_OUT, "curva_calibracion_igual_frecuencia.csv"))
fwrite(curva_por_resultado, file.path(DIR_OUT, "curva_calibracion_por_resultado.csv"))
fwrite(curva_por_liga, file.path(DIR_OUT, "curva_calibracion_por_liga.csv"))
fwrite(tabla_brier, file.path(DIR_OUT, "brier_score.csv"))
fwrite(tabla_murphy, file.path(DIR_OUT, "descomposicion_murphy.csv"))
fwrite(tabla_rps, file.path(DIR_OUT, "rps_score.csv"))
fwrite(sensibilidad_conclusiones, file.path(DIR_OUT, "sensibilidad_conclusiones_metodos.csv"))
fwrite(comparaciones_apareadas, file.path(DIR_OUT, "comparacion_apareada_operadores.csv"))
fwrite(data.table(Operadores = paste(OPERADORES_CASAS, collapse = "/"),
                  N_partidos = nrow(base_comun),
                  Chi2_Friedman = unname(test_friedman$statistic),
                  gl = unname(test_friedman$parameter),
                  Valor_p = test_friedman$p.value),
       file.path(DIR_OUT, "friedman_operadores.csv"))
fwrite(tabla_hl, file.path(DIR_OUT, "hosmer_lemeshow.csv"))
# Mismo nombre y columnas base que ya lee el reporte (solo método multiplicativo)
fwrite(tabla_hl[Metodo == "mult"], file.path(DIR_OUT, "hosmer_lemeshow_por_resultado.csv"))
fwrite(sensibilidad_agrupamiento, file.path(DIR_OUT, "sensibilidad_agrupamiento.csv"))
fwrite(resumen_na_shin, file.path(DIR_OUT, "shin_no_convergencia.csv"))
fwrite(sensibilidad_covid, file.path(DIR_OUT, "sensibilidad_covid.csv"))
fwrite(hl_covid, file.path(DIR_OUT, "sensibilidad_covid_hl.csv"))
fwrite(rbindlist(hl_covid_det), file.path(DIR_OUT, "sensibilidad_covid_hl_detalle.csv"))
fwrite(ventaja_local_covid, file.path(DIR_OUT, "ventaja_local_covid.csv"))
fwrite(remocion_aleatoria, file.path(DIR_OUT, "remocion_aleatoria_hl.csv"))

message(sprintf("\nListo. Todos los archivos de la Fase 3 guardados en %s/:", DIR_OUT))
message(" - curva_calibracion_igual_ancho.csv (con IC Wilson e IC bootstrap por partido)")
message(" - curva_calibracion_igual_frecuencia.csv")
message(" - curva_calibracion_por_resultado.csv")
message(" - curva_calibracion_por_liga.csv")
message(" - brier_score.csv")
message(" - descomposicion_murphy.csv (BONIFICACIÓN)")
message(" - rps_score.csv (BONIFICACIÓN)")
message(" - sensibilidad_conclusiones_metodos.csv")
message(" - comparacion_apareada_operadores.csv (entre casas, con IC bootstrap por bloques)")
message(" - friedman_operadores.csv")
message(" - hosmer_lemeshow.csv (separado por H/D/A)")
message(" - sensibilidad_agrupamiento.csv")
message(" - sensibilidad_covid.csv y sensibilidad_covid_hl.csv")
message(" - ventaja_local_covid.csv y remocion_aleatoria_hl.csv")
message(" - shin_no_convergencia.csv")

# ---- 12. Resumen final consolidado ----------------------------------
mostrar_resumen_fase3 <- function() {
  cat("\n")
  cat("================ RESUMEN FASE 3 (CALIBRACIÓN E INFERENCIA) ================\n")
  cat(sprintf("Muestra común apareada:           %d partidos evaluados simultáneamente\n", nrow(base_comun)))
  cat(sprintf("Operadores principales:           %s\n", paste(OPERADORES_PRINCIPALES, collapse = ", ")))
  cat(sprintf("Casas comparadas entre sí:        %s (cierre se compara aparte, Fase 4)\n",
              paste(OPERADORES_CASAS, collapse = ", ")))
  cat("--------------------------------------------------------------------------\n")
  cat("Brier Score y RPS vs. Líneas Base (Muestra Común):\n")
  comp_metr <- merge(tabla_brier[, .(Operador, Brier = Brier_operador, Mejora_Brier = Mejora_vs_frecuencia_pct)],
                     tabla_rps[, .(Operador, RPS = RPS_operador, Mejora_RPS = Mejora_vs_frecuencia_pct)],
                     by = "Operador")
  print(comp_metr)
  cat("--------------------------------------------------------------------------\n")
  cat("Bonificación Murphy (Brier = Fiabilidad - Resolución + Incertidumbre):\n")
  print(tabla_murphy[, .(Operador, Fiabilidad_Reliability, Resolucion_Resolution, Incertidumbre_Uncertainty, Brier_calculado, Brier_real, Termino_intrabin)])
  cat("--------------------------------------------------------------------------\n")
  cat("Sensibilidad metodológica (Brier y desviación por método de margen):\n")
  print(sensibilidad_conclusiones[, .(Operador, Metodo, Brier, RPS, Desviacion_media_abs_pp, Desviacion_pond_pp)])
  cat("--------------------------------------------------------------------------\n")
  cat("Inferencia Apareada entre casas (Friedman, Wilcoxon + Holm, IC bootstrap por bloques):\n")
  cat(sprintf("  Friedman p-valor = %g\n", test_friedman$p.value))
  print(comparaciones_apareadas[, .(Comparacion, Dif_Brier, IC_boot_Dif_inferior, IC_boot_Dif_superior,
                                    Mejora_pct, Valor_p_ajustado_Holm, Significativo_Holm_0.05)])
  cat("--------------------------------------------------------------------------\n")
  cat("Hosmer-Lemeshow por resultado (multiplicativo):\n")
  print(tabla_hl[Metodo == "mult", .(Operador, Resultado, Valor_p, Valor_p_Holm, Rechaza_calibracion_perfecta_0.05,
                                     Rechaza_Holm_0.05, Desviacion_media_abs_pp, Desviacion_pond_abs_pp)])
  cat("Rechazos de Hosmer-Lemeshow por método (sin ajuste y con Holm):\n")
  print(resumen_hl)
  cat("--------------------------------------------------------------------------\n")
  cat(sprintf("Sensibilidad COVID-19 (%d partidos post-reanudación excluidos):\n", n_post_covid))
  print(hl_covid)
  if (nrow(ventaja_local_covid) > 0) {
    cat("Victoria local (H), post-reanudación vs. resto (multiplicativo):\n")
    print(ventaja_local_covid[Metodo == "mult",
                              .(Grupo, Operador, N, Desviacion_pp = round(Desviacion_pp, 2), Valor_p = round(Valor_p, 3))])
  }
  if (nrow(remocion_aleatoria) > 0) {
    cat("Control por remoción aleatoria (proporción de réplicas que mantiene el rechazo con Holm):\n")
    print(remocion_aleatoria[, .(Metodo, Operador, Resultado, Prop_mantiene_Holm = round(Prop_mantiene_Holm, 2))])
  }
  cat("==========================================================================\n")
}

mostrar_resumen_fase3()