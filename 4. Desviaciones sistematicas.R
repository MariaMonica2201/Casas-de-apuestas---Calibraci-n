# ============================================================
# Proyecto 2 - ¿Están bien calibradas las casas de apuestas?
# Fase 4: Análisis de desviaciones sistemáticas
# ============================================================
# Parte de base_con_probabilidades.csv (Fase 2) sobre la muestra común.
# Investiga:
#   A) Sesgo favorito-longshot:
#      - Análisis por bins (desviación observada vs. predicha), con medias
#        simples y PONDERADAS por N en los extremos
#      - Spearman sobre bins con N >= N_MIN_BIN (los bins casi vacíos no cuentan)
#      - Pendiente de calibración logística con error estándar ROBUSTO por
#        clúster de partido (cada partido aporta 3 filas dependientes: H, D, A)
#      - Pendiente por resultado (H, D, A) separado
#      - SENSIBILIDAD METODOLÓGICA: ¿atenúa o elimina el método de Shin el sesgo?
#   B) Apertura vs. Cierre (par definido en config.R) - BONIFICABLE:
#      - Comparación apareada (Wilcoxon) + IC bootstrap por bloques de la
#        diferencia media de Brier y de RPS
# ============================================================

source("config.R")
set.seed(SEMILLA)

base <- fread(file.path(DIR_OUT, "base_con_probabilidades.csv"), encoding = "UTF-8")

# Filtrar a muestra común apareada
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
base_comun[, id_partido := .I]
mes_txt <- if ("Date_parsed" %in% names(base_comun)) substr(as.character(base_comun$Date_parsed), 1, 7) else ""
base_comun[, bloque_boot := paste(Liga, Temporada, mes_txt)]

# ---- 0. Función de formato largo ------------------------------------
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

datos_largos_mult <- rbindlist(lapply(OPERADORES_PRINCIPALES, construir_formato_largo,
                                      dt = base_comun, metodo = "mult"))
datos_largos_shin <- rbindlist(lapply(OPERADORES_PRINCIPALES, construir_formato_largo,
                                      dt = base_comun, metodo = "shin"))

# ============================================================
# PARTE A: Sesgo favorito-longshot
# ============================================================
# 1. Agrupamiento en N_BINS_FAVLONG bins para caracterizar los extremos
analizar_favorito_longshot <- function(dt_largo, operador, metodo = "mult", n_bins = N_BINS_FAVLONG) {
  d <- dt_largo[Operador == operador & Metodo == metodo]
  d[, bin := cut(Prob_predicha, breaks = seq(0, 1, length.out = n_bins + 1),
                 include.lowest = TRUE, labels = FALSE)]
  
  resumen <- d[, {
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
  }, by = bin]
  
  resumen[, `:=`(
    Desviacion_pp = 100 * (Frecuencia_observada - Prob_predicha_media),
    Bin_poco_poblado = N < N_MIN_BIN,
    Operador = operador,
    Metodo = metodo
  )]
  setorder(resumen, bin)
  resumen
}

favlong_mult <- rbindlist(lapply(OPERADORES_PRINCIPALES, analizar_favorito_longshot,
                                 dt_largo = datos_largos_mult, metodo = "mult"))
favlong_shin <- rbindlist(lapply(OPERADORES_PRINCIPALES, analizar_favorito_longshot,
                                 dt_largo = datos_largos_shin, metodo = "shin"))
favlong_detalle <- rbind(favlong_mult, favlong_shin)

# 2. Contraste formal: Spearman + Calibration Slope Logística
# logit(P(Y=1)) = alpha + beta * logit(p)
# beta > 1: Sesgo favorito-longshot (los longshots ocurren menos de lo que indica su
#           probabilidad y los favoritos más: el mercado exagera la incertidumbre)
# beta = 1: Calibración ideal a lo largo de todo el espectro
# beta < 1: Lo contrario: probabilidades demasiado extremas (exceso de confianza)
#
# Error estándar: el modelo se ajusta sobre el formato largo (3 filas por partido:
# H, D, A), que no son independientes entre sí. Se usa un estimador robusto por
# CLÚSTER de partido (sandwich::vcovCL). El EE del modelo se guarda aparte para
# comparar. La pendiente se estima además por resultado (H, D, A) más abajo,
# porque el empate no se comporta como favorito/longshot.
extremos_ponderados <- function(sub_bins, n_bins = N_BINS_FAVLONG) {
  idx_long <- sub_bins$bin <= 0.2 * n_bins   # prob. < 20% aprox. (longshots)
  idx_fav  <- sub_bins$bin >  0.8 * n_bins   # prob. > 80% aprox. (favoritos)
  list(
    long_simple = mean(sub_bins$Desviacion_pp[idx_long], na.rm = TRUE),
    fav_simple  = mean(sub_bins$Desviacion_pp[idx_fav], na.rm = TRUE),
    long_pond   = weighted.mean(sub_bins$Desviacion_pp[idx_long], sub_bins$N[idx_long], na.rm = TRUE),
    fav_pond    = weighted.mean(sub_bins$Desviacion_pp[idx_fav],  sub_bins$N[idx_fav],  na.rm = TRUE),
    n_long      = sum(sub_bins$N[idx_long]),
    n_fav       = sum(sub_bins$N[idx_fav])
  )
}

ajustar_modelo_calibracion <- function(dt_largo, operador, metodo = "mult") {
  d <- dt_largo[Operador == operador & Metodo == metodo]
  
  # Evitar logit(0) o logit(1) con recorte mínimo
  p_clip <- pmax(pmin(d$Prob_predicha, 0.9999), 0.0001)
  logit_p <- qlogis(p_clip)
  
  mod <- glm(d$Ocurrio ~ logit_p, family = binomial(link = "logit"))
  coefs <- summary(mod)$coefficients
  
  intercepto <- coefs[1, 1]
  pendiente <- coefs[2, 1]
  se_modelo <- coefs[2, 2]
  p_modelo_1 <- 2 * pnorm(-abs((pendiente - 1) / se_modelo))
  
  # EE robusto por clúster de partido; H0: beta = 1
  V_cl <- sandwich::vcovCL(mod, cluster = d$id_partido)
  se_cluster <- sqrt(V_cl[2, 2])
  p_cluster_1 <- 2 * pnorm(-abs((pendiente - 1) / se_cluster))
  
  # Spearman sobre bins con al menos N_MIN_BIN observaciones
  sub_bins <- favlong_detalle[Operador == operador & Metodo == metodo]
  sub_ok <- sub_bins[N >= N_MIN_BIN]
  test_sp <- suppressWarnings(
    cor.test(sub_ok$Prob_predicha_media, sub_ok$Desviacion_pp, method = "spearman")
  )
  ext <- extremos_ponderados(sub_bins)
  
  data.table(
    Operador = operador,
    Metodo = metodo,
    Bins_usados_spearman = nrow(sub_ok),
    Correlacion_spearman = round(unname(test_sp$estimate), 3),
    Valor_p_spearman = signif(test_sp$p.value, 4),
    Desv_longshots_pp = round(ext$long_pond, 2),          # ponderada por N
    Desv_favoritos_pp = round(ext$fav_pond, 2),           # ponderada por N
    Desv_longshots_simple_pp = round(ext$long_simple, 2),
    Desv_favoritos_simple_pp = round(ext$fav_simple, 2),
    N_longshots = ext$n_long,
    N_favoritos = ext$n_fav,
    Pendiente_calibracion_beta = round(pendiente, 3),
    SE_pendiente_modelo = round(se_modelo, 3),
    SE_pendiente = round(se_cluster, 3),                  # robusto por clúster (principal)
    Valor_p_beta_eq_1_modelo = signif(p_modelo_1, 4),
    Valor_p_beta_eq_1 = signif(p_cluster_1, 4)            # robusto por clúster (principal)
  )
}

contraste_mult <- rbindlist(lapply(OPERADORES_PRINCIPALES, ajustar_modelo_calibracion,
                                   dt_largo = datos_largos_mult, metodo = "mult"))
contraste_shin <- rbindlist(lapply(OPERADORES_PRINCIPALES, ajustar_modelo_calibracion,
                                   dt_largo = datos_largos_shin, metodo = "shin"))

contraste_favlong <- rbind(contraste_mult, contraste_shin)

# Comparaciones múltiples: (operadores x 2 métodos) contrastes para cada tipo de
# prueba. Se ajusta con Holm; "Sesgo_robusto_Holm" exige además beta > 1 (dirección
# del sesgo favorito-longshot) y p ajustado < 0.05. La columna "Sesgo_sin_ajuste"
# usa el p-valor SIN corregir y no debe presentarse como hallazgo.
contraste_favlong[, Valor_p_beta_Holm := signif(p.adjust(Valor_p_beta_eq_1, method = "holm"), 4)]
contraste_favlong[, Valor_p_spearman_Holm := signif(p.adjust(Valor_p_spearman, method = "holm"), 4)]
contraste_favlong[, Sesgo_sin_ajuste := Pendiente_calibracion_beta > 1 & Valor_p_beta_eq_1 < 0.05]
contraste_favlong[, Sesgo_robusto_Holm := Pendiente_calibracion_beta > 1 & Valor_p_beta_Holm < 0.05]

message("---- Sesgo favorito-longshot: Spearman, extremos y Pendiente de Calibración (EE por clúster) ----")
print(contraste_favlong)

# 3. Pendiente de calibración por resultado (H, D, A) por separado
# Dentro de un resultado cada partido aporta una sola fila; se usa igualmente EE
# por clúster de partido (trivial aquí) para mantener el mismo estimador.
ajustar_pendiente_por_resultado <- function(dt_largo, operador, metodo, resultado) {
  d <- dt_largo[Operador == operador & Metodo == metodo & Resultado_evaluado == resultado]
  p_clip <- pmax(pmin(d$Prob_predicha, 0.9999), 0.0001)
  logit_p <- qlogis(p_clip)
  mod <- glm(d$Ocurrio ~ logit_p, family = binomial(link = "logit"))
  b <- unname(coef(mod)[2])
  se <- sqrt(sandwich::vcovCL(mod, cluster = d$id_partido)[2, 2])
  data.table(Operador = operador, Metodo = metodo, Resultado = resultado,
             Pendiente_calibracion_beta = round(b, 3),
             SE_pendiente = round(se, 3),
             Valor_p_beta_eq_1 = signif(2 * pnorm(-abs((b - 1) / se)), 4))
}

comb_res <- CJ(Metodo = c("mult", "shin"), Operador = OPERADORES_PRINCIPALES,
               Resultado = c("H", "D", "A"), sorted = FALSE)
pendientes_resultado <- rbindlist(mapply(function(met, op, res) {
  dl <- if (met == "mult") datos_largos_mult else datos_largos_shin
  ajustar_pendiente_por_resultado(dl, op, met, res)
}, met = comb_res$Metodo, op = comb_res$Operador, res = comb_res$Resultado, SIMPLIFY = FALSE))
pendientes_resultado[, Valor_p_beta_Holm := signif(p.adjust(Valor_p_beta_eq_1, method = "holm"), 4)]

message("\n---- Pendiente de calibración por resultado (H/D/A), Holm sobre todas las pruebas ----")
print(pendientes_resultado)

# ============================================================
# PARTE B: Apertura vs. Cierre (par definido en config.R)
# ============================================================
# Comparación apareada: mismo operador, mismos partidos de la muestra común.
op_ap <- PAR_APERTURA_CIERRE[["apertura"]]
op_ci <- PAR_APERTURA_CIERRE[["cierre"]]

oH <- as.integer(base_comun$FTR == "H")
oD <- as.integer(base_comun$FTR == "D")
oA <- as.integer(base_comun$FTR == "A")

prob_op <- function(op, res) base_comun[[paste0("pnorm_mult_", op, "_", res)]]

brier_partido <- function(op) {
  (prob_op(op, "H") - oH)^2 + (prob_op(op, "D") - oD)^2 + (prob_op(op, "A") - oA)^2
}
rps_partido <- function(op) {
  pH <- prob_op(op, "H"); pD <- prob_op(op, "D")
  0.5 * ((pH - oH)^2 + ((pH + pD) - (oH + oD))^2)
}

brier_ap <- brier_partido(op_ap); brier_ci <- brier_partido(op_ci)
rps_ap   <- rps_partido(op_ap);   rps_ci   <- rps_partido(op_ci)

# IC bootstrap por bloques (liga-temporada-mes) de la diferencia media cierre - apertura
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

# Wilcoxon apareado para Brier y RPS
test_brier <- wilcox.test(brier_ci, brier_ap, paired = TRUE)
test_rps   <- wilcox.test(rps_ci, rps_ap, paired = TRUE)
ic_brier <- boot_dif_media(brier_ci - brier_ap, base_comun$bloque_boot)
ic_rps   <- boot_dif_media(rps_ci - rps_ap, base_comun$bloque_boot)

resumen_apertura_cierre <- data.table(
  Metrica = c("Brier Score", "Ranked Probability Score (RPS)"),
  Operador_apertura = op_ap,
  Operador_cierre = op_ci,
  N_partidos = rep(nrow(base_comun), 2),
  Media_Apertura = c(round(mean(brier_ap), 4), round(mean(rps_ap), 4)),
  Media_Cierre = c(round(mean(brier_ci), 4), round(mean(rps_ci), 4)),
  Diferencia = c(round(mean(brier_ci) - mean(brier_ap), 5), round(mean(rps_ci) - mean(rps_ap), 5)),
  IC_boot_Dif_inferior = c(round(ic_brier[1], 5), round(ic_rps[1], 5)),
  IC_boot_Dif_superior = c(round(ic_brier[2], 5), round(ic_rps[2], 5)),
  Mejora_cierre_pct = c(round(100 * (mean(brier_ap) - mean(brier_ci)) / mean(brier_ap), 2),
                        round(100 * (mean(rps_ap) - mean(rps_ci)) / mean(rps_ap), 2)),
  Wilcoxon_V = c(unname(test_brier$statistic), unname(test_rps$statistic)),
  Valor_p = c(signif(test_brier$p.value, 4), signif(test_rps$p.value, 4))
)

message(sprintf("\n---- Apertura (%s) vs. Cierre (%s): contraste apareado (Wilcoxon + IC bootstrap por bloques) ----",
                op_ap, op_ci))
print(resumen_apertura_cierre)

# ---- Guardar salidas ------------------------------------------------
fwrite(favlong_detalle, file.path(DIR_OUT, "sesgo_favorito_longshot_detalle.csv"))
fwrite(contraste_favlong, file.path(DIR_OUT, "sesgo_favorito_longshot_contraste.csv"))
fwrite(pendientes_resultado, file.path(DIR_OUT, "sesgo_favorito_longshot_por_resultado.csv"))
fwrite(resumen_apertura_cierre, file.path(DIR_OUT, "apertura_vs_cierre.csv"))

message(sprintf("\nListo. Archivos de Fase 4 guardados en %s/:", DIR_OUT))
message(" - sesgo_favorito_longshot_detalle.csv")
message(" - sesgo_favorito_longshot_contraste.csv")
message(" - sesgo_favorito_longshot_por_resultado.csv")
message(" - apertura_vs_cierre.csv")

# ---- Resumen final --------------------------------------------------
cat("\n")
cat("================ RESUMEN FASE 4: DESVIACIONES SISTEMÁTICAS ================\n")
cat("A) Sesgo Favorito-Longshot y Sensibilidad (Multiplicativo vs. Shin):\n")
cat("   (extremos ponderados por N; EE de beta robusto por clúster de partido)\n")
print(contraste_favlong[, .(Operador, Metodo, Correlacion_spearman, Desv_longshots_pp,
                            Desv_favoritos_pp, Pendiente_calibracion_beta, SE_pendiente,
                            Valor_p_beta_Holm, Sesgo_robusto_Holm)])
cat("--------------------------------------------------------------------------\n")
cat(sprintf("B) Apertura (%s) vs. Cierre (%s) - Bonificación:\n", op_ap, op_ci))
print(resumen_apertura_cierre[, .(Metrica, Media_Apertura, Media_Cierre, Dif = Diferencia,
                                  IC_boot_Dif_inferior, IC_boot_Dif_superior,
                                  Mejora_pct = Mejora_cierre_pct, Valor_p)])
cat("==========================================================================\n")