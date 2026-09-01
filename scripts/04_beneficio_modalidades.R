# =============================================================================
# 04_beneficio_modalidades.R
# -----------------------------------------------------------------------------
# ¿Es realmente MAS LUCRATIVO el alquiler no habitual que el habitual una vez
# descontados gastos e impuestos? Este script lleva la comparacion desde los
# ingresos hasta el beneficio despues de impuestos, en tres escalones:
#
#   1. INGRESOS INTEGROS (casilla 102)                        -> lo que ya publica el informe
#   2. BENEFICIO BRUTO  = rendimiento neto fiscal (c.149)     -> ingresos menos TODOS los
#      gastos deducibles (reparacion, comunidad, intereses, suministros, seguros,
#      tributos, amortizacion...). Es el "beneficio antes de impuestos" fiscal.
#   3. BENEFICIO NETO   = beneficio bruto - IRPF estimado, donde el IRPF tiene en
#      cuenta las DOS asimetrias fiscales entre modalidades:
#        (a) la REDUCCION del art. 23.2 LIRPF (c.150): en el ejercicio 2023 es el
#            60% del rendimiento neto positivo y SOLO la tiene la vivienda
#            habitual (a los alquileres turisticos/temporales no les es aplicable).
#            Desde 2024: 60% para contratos anteriores al 26/5/2023; para los
#            nuevos, 50% general, 60% rehabilitada, 70% y 90% en zonas tensionadas.
#        (b) la IMPUTACION DE RENTAS por los dias en que la vivienda NO esta
#            arrendada, tomada tal y como la DECLARO cada contribuyente (c.89,
#            con el porcentaje aplicado en la c.87): no se simula; si el panel
#            no trae esas casillas, la imputacion se deja a cero y se avisa.
#      El impuesto se estima aplicando el tipo MARGINAL del declarante segun su
#      renta bruta (escala general 2023, estatal + autonomica media).
#      Requiere haber pasado 01c (version 3) para disponer de las casillas
#      104, 116, 117, 132, 146-148, 151, 152, 154, 87 y 89.
#
# Todo se expresa por VIVIENDA ENTERA al 100% de titularidad, en euros al ano y
# en euros por DIA de alquiler (dias censales por referencia catastral).
#
# USO: source("02_informe_arrendadores.R"); source("04_beneficio_modalidades.R")
# Necesita en memoria: base, pers, ANIO_REF, carpeta_anio(), fmt().
# =============================================================================

if (!exists("base") || !exists("pers"))
  stop("Ejecuta antes 02_informe_arrendadores.R en esta misma sesion de R.")
suppressPackageStartupMessages(library(data.table))

# Escala general del IRPF por ejercicio (estatal + autonomica de tipo medio).
# Cada ano se grava con SU PROPIA escala: la de 2016 no tiene los tramos
# superiores creados despues (47% desde 300.000, vigente desde 2021).
ESCALAS <- list(
  "2016" = data.table(desde = c(0, 12450, 20200, 35200, 60000),
                      tipo  = c(0.19, 0.24, 0.30, 0.37, 0.45)),
  "2023" = data.table(desde = c(0, 12450, 20200, 35200, 60000, 300000),
                      tipo  = c(0.19, 0.24, 0.30, 0.37, 0.45, 0.47)))
escala_de <- function(anio) {
  a <- as.character(anio)
  if (!is.null(ESCALAS[[a]])) return(ESCALAS[[a]])
  ESCALAS[[if (as.integer(anio) <= 2020) "2016" else "2023"]]
}
tipo_marginal <- function(renta, anio = ANIO_REF) {
  E <- escala_de(anio)
  r <- suppressWarnings(as.numeric(renta)); r[is.na(r)] <- 0
  E$tipo[findInterval(pmax(r, 0), E$desde)]
}

# --- TIPO MARGINAL DEDUCIDO DEL PROPIO PANEL ---------------------------------
# Si el 01c aporto la base liquidable general sometida a gravamen y las cuotas
# integras estatal y autonomica (casillas 505/545/546 en 2023; 450/499/500 en
# 2016), el tipo marginal NO se impone: se estima empiricamente por COMUNIDAD
# AUTONOMA, que es donde difieren las escalas. Para cada CCAA se ordenan los
# declarantes por base liquidable, se agrupan en tramos finos y el tipo marginal
# de cada tramo es el incremento de cuota integra dividido por el incremento de
# base (la derivada de la funcion de gravamen observada en los datos). Asi se
# recoge la escala autonomica real de cada comunidad y ejercicio, sin
# simplificar y sin depender de tablas externas.
marginal_empirico <- function(d, n_bins = 60, min_n = 200) {
  d <- d[is.finite(blg) & blg > 0 & is.finite(cuota)]
  if (nrow(d) < min_n) return(NULL)
  setorder(d, blg)
  d[, bin := cut(blg, breaks = unique(quantile(blg, seq(0, 1, length.out = n_bins + 1))),
                 include.lowest = TRUE, labels = FALSE)]
  r <- d[, .(blg = mean(blg), cuota = mean(cuota), n = .N), by = bin][order(blg)]
  r <- r[n >= 10]
  if (nrow(r) < 5) return(NULL)
  r[, `:=`(dblg = blg - shift(blg), dcuota = cuota - shift(cuota))]
  r[, tmg := fifelse(!is.na(dblg) & dblg > 0, dcuota / dblg, NA_real_)]
  r[, tmg := pmin(pmax(tmg, 0), 0.60)]          # acota valores imposibles
  r <- r[!is.na(tmg)]
  if (!nrow(r)) return(NULL)
  r[, .(blg_desde = blg, tipo_marginal = tmg)]
}
aplicar_marginal <- function(base_liq, tabla) {
  if (is.null(tabla)) return(NA_real_)
  i <- findInterval(pmax(suppressWarnings(as.numeric(base_liq)), 0), tabla$blg_desde)
  tabla$tipo_marginal[pmax(i, 1)]
}
num <- function(x) { v <- suppressWarnings(as.numeric(x)); v[is.na(v)] <- 0; v }
wm <- function(x, w) { ok <- !is.na(x) & !is.na(w); if (!any(ok)) NA_real_ else sum(x[ok] * w[ok]) / sum(w[ok]) }
col_or <- function(d, nombres) { h <- intersect(nombres, names(d)); if (length(h)) h[1] else NA }

message("\n", strrep("=", 74), "\n  BENEFICIO DEL ALQUILER POR MODALIDAD: ingresos -> bruto -> neto de IRPF\n",
        strrep("=", 74))

b <- copy(base)
b[, w := share * FACTORCAL]                       # peso de vivienda entera
sh <- pmax(num(b$share), 1e-9)
b[, ingresos := num(INGRESOS_102) / sh]
c149 <- col_or(b, c("REND_NETO_149", "RNETO_149"))
c150 <- col_or(b, c("REDUCCION_150", "REDUC_150"))
if (is.na(c149)) stop("El panel no trae la casilla 149 (rendimiento neto): reejecuta 01b.")
b[, bruto := num(get(c149)) / sh]                 # rendimiento neto fiscal = ingresos - gastos
b[, gastos := ingresos - bruto]
b[, reduccion := if (is.na(c150)) 0 else num(get(c150)) / sh]

# desglose de gastos (si el panel trae las casillas de detalle)
# Formula oficial (Modelo 100, 2023): [149] = [102] - [104] - [107] - [109] -
# [110] - [111] - [112] - [113] - [114] - [115] - [116] - [117] - [131] - [132]
# - [146] - [147] - [148]
DET <- c(intereses_financiacion_104 = "INTERESES_APL_104",
         reparacion_conservacion_107 = "G_REP_APL_107", reparacion_alt = "G_REPARA_106",
         comunidad_109 = "G_COMUN_109", formalizacion_110 = "G_FORMAL_110",
         defensa_juridica_111 = "G_JURID_111", servicios_personales_112 = "G_SERVPER_112",
         suministros_113 = "G_SUMIN_113", seguros_114 = "SEGUROS_114",
         tributos_ibi_115 = "TRIBUTOS_115", saldos_dudoso_cobro_116 = "DUDOSOS_116",
         otros_gastos_117 = "OTROS_GASTOS_117", amortizacion_inmueble_131 = "AMORT_INM_131",
         amortizacion_muebles_132 = "AMORT_MUEBLES_132", amortizacion_accesorio_146 = "AMORT_ACCES_146",
         amortizacion_especial_147 = "AMORT_ESPEC_147", otros_deducibles_148 = "OTROS_DEDUC_148")
for (k in names(DET)) if (DET[[k]] %in% names(b)) b[, (paste0("g_", k)) := num(get(DET[[k]])) / sh]
if ("g_reparacion_conservacion_107" %in% names(b) && "g_reparacion_alt" %in% names(b)) {
  b[g_reparacion_conservacion_107 == 0 & g_reparacion_alt > 0,
    g_reparacion_conservacion_107 := g_reparacion_alt]
}
if ("g_reparacion_alt" %in% names(b)) b[, g_reparacion_alt := NULL]
gcols <- grep("^g_", names(b), value = TRUE)
if (length(gcols)) b[, g_no_desglosado := pmax(gastos - rowSums(.SD), 0), .SDcols = gcols]
faltan <- setdiff(unname(DET[names(DET) != "reparacion_alt"]), names(b))
if (length(faltan))
  message("  AVISO: el panel no trae ", paste(faltan, collapse = ", "),
          " -> su importe queda dentro de 'no_desglosado' (el TOTAL de gastos es exacto via c.149). ",
          "Ejecuta 01c v3 para desglosarlo.")

# dias (censales si existen) y valor catastral (modulo si existe)
b[, dias := if ("DIAS_RC" %in% names(b)) { x <- num(DIAS_RC); fifelse(x > 0, pmin(x, 365), num(dias_ef)) } else num(dias_ef)]
b[dias <= 0, dias := NA_real_]

# --- IRPF estimado -----------------------------------------------------------
# renta bruta del declarante -> tipo marginal
rb <- pers[, .(IDENPER = get(names(pers)[1]), renta_total)]
b <- merge(b, rb, by = "IDENPER", all.x = TRUE)
# tipo marginal: empirico por CCAA si el panel trae cuotas; si no, escala legal
cq <- col_or(b, "BLG_GRAVAMEN"); ce <- col_or(b, "CUOTA_ESTATAL"); ca <- col_or(b, "CUOTA_AUTONOMICA")
TIPO_ORIGEN <- "escala legal general (el panel no trae cuotas del fichero 4)"
b[, t_marg := NA_real_]
if (!is.na(cq) && !is.na(ce)) {
  b[, blg := num(get(cq))]
  b[, cuota := num(get(ce)) + (if (is.na(ca)) 0 else num(get(ca)))]
  ccaa_col <- col_or(b, c("ccaa", "CCAA", "cod_ccaa"))
  if (is.na(ccaa_col)) {
    CCAA_DE_PROV <- c("04"="AND","11"="AND","14"="AND","18"="AND","21"="AND","23"="AND","29"="AND","41"="AND",
      "22"="ARA","44"="ARA","50"="ARA","33"="AST","07"="BAL","35"="CAN","38"="CAN","39"="CNT",
      "05"="CYL","09"="CYL","24"="CYL","34"="CYL","37"="CYL","40"="CYL","42"="CYL","47"="CYL","49"="CYL",
      "02"="CLM","13"="CLM","16"="CLM","19"="CLM","45"="CLM","08"="CAT","17"="CAT","25"="CAT","43"="CAT",
      "03"="CVA","12"="CVA","46"="CVA","06"="EXT","10"="EXT","15"="GAL","27"="GAL","32"="GAL","36"="GAL",
      "28"="MAD","30"="MUR","26"="RIO","51"="CEU","52"="MEL")
    b[, ccaa_k := unname(CCAA_DE_PROV[formatC(as.character(cpro2), width = 2, flag = "0")])]
  } else b[, ccaa_k := as.character(get(ccaa_col))]
  b[is.na(ccaa_k), ccaa_k := "ND"]
  decl <- unique(b[, .(IDENPER, ccaa_k, blg, cuota)])
  tablas <- split(decl, by = "ccaa_k")
  tablas <- lapply(tablas, function(x) marginal_empirico(copy(x)))
  nac_tab <- marginal_empirico(copy(decl))
  b[, t_marg := {
    tb <- tablas[[.BY$ccaa_k]]
    if (is.null(tb)) tb <- nac_tab
    aplicar_marginal(blg, tb)
  }, by = ccaa_k]
  n_ok <- sum(!vapply(tablas, is.null, logical(1)))
  if (any(is.finite(b$t_marg))) {
    TIPO_ORIGEN <- sprintf("deducido del panel: cuota integra / base liquidable, por CCAA (%d comunidades con escala propia estimada)", n_ok)
    message(sprintf("  Tipo marginal DEDUCIDO del panel (cuotas del fichero 4): %d CCAA con escala propia; medio %.1f%%",
                    n_ok, 100 * wm(b$t_marg, b$w)))
    # Control comparable: la escala legal aplicada a la MISMA magnitud (la base
    # liquidable general). Aplicarla a la renta bruta sobreestima, porque la
    # base es menor que la renta tras reducciones y minimo personal.
    b[, t_legal_base := tipo_marginal(blg, ANIO_REF)]
    b[, t_legal_renta := tipo_marginal(renta_total, ANIO_REF)]
    message(sprintf(paste0("  Control: escala legal sobre la MISMA base %.1f%% ",
            "(deducido del panel %.1f%%); sobre la renta bruta seria %.1f%%, pero ",
            "no es comparable. Base liquidable general mediana: %s EUR."),
            100 * wm(b$t_legal_base, b$w), 100 * wm(b$t_marg, b$w),
            100 * wm(b$t_legal_renta, b$w),
            fmt(median(b$blg[b$blg > 0], na.rm = TRUE))))
  }
}
if (!any(is.finite(b$t_marg))) {
  b[, t_marg := tipo_marginal(renta_total, ANIO_REF)]
  message("  Tipo marginal por ESCALA LEGAL general (ejecuta 01c para incorporar las cuotas reales del fichero 4 y deducirlo por CCAA).")
}
# base gravada: rendimiento neto reducido oficial (c.154) si esta; si no, 149 - 150
# Base gravada por inmueble. Se preferiria la c.154 (rendimiento neto
# reducido), pero en muchos paneles esa casilla llega vacia o a cero: si se
# usara a ciegas, la base seria nula y el IRPF desapareceria. Por eso se
# calcula SIEMPRE la alternativa 149 - 150 - 151 (con el suelo del rendimiento
# minimo 152) y solo se acepta la 154 en los registros donde viene informada y
# es coherente con esa alternativa.
c154 <- col_or(b, "REND_NETO_RED_154")
c151 <- col_or(b, "REDUCCION_151")
c152 <- col_or(b, "REND_MIN_152")
b[, red151 := if (is.na(c151)) 0 else num(get(c151)) / sh]
b[, min152 := if (is.na(c152)) 0 else num(get(c152)) / sh]
b[, base_calc := pmax(pmax(bruto, 0) -
                      fifelse(bruto > 0, pmin(reduccion + red151, pmax(bruto, 0)), 0),
                      min152)]
# La base gravada se calcula SIEMPRE como 149 - 150 - 151 (suelo c.152): es la
# formula del Modelo 100 y esta disponible en todos los registros. La c.154 se
# usa solo como CONTROL: se compara con el calculo y se informa de la
# concordancia, sin sustituirlo (llega vacia o parcial en muchos paneles).
b[, base_gravada := base_calc]
if (!is.na(c154)) {
  b[, base_154 := num(get(c154)) / sh]
  con <- b[base_154 > 0]
  if (nrow(con)) {
    coincide <- con[, mean(abs(base_154 - base_calc) <= pmax(1, 0.02 * base_calc))]
    message(sprintf(paste0("  Base gravada = 149 - 150 - 151 (suelo 152). Control ",
            "con la c.154: informada en el %.1f%% de los inmuebles y coincide con ",
            "el calculo en el %.1f%% de ellos."),
            100 * nrow(con) / nrow(b), 100 * coincide))
  }
} else message("  Base gravada = 149 - 150 - 151 (suelo c.152); la c.154 no esta en el panel.")
if (b[, sum(base_gravada * w)] <= 0)
  message("  AVISO: la base gravada agregada es nula: el IRPF estimado saldra 0. ",
          "Revisa las casillas 149/150/154 del panel.")
# imputacion de rentas: la DECLARADA (c.89), nunca simulada
c89 <- col_or(b, "RENTA_IMPUTADA_89"); c87 <- col_or(b, "PCT_IMPUTACION_87")
if (!is.na(c89)) {
  b[, imputacion := num(get(c89)) / sh]
  b[, pct_imput := if (is.na(c87)) NA_real_ else num(get(c87))]
  ci <- b[uso == "tur", .(pct_con_imputacion = round(100 * mean(imputacion > 0), 1),
                          imputacion_media_si_aplica = round(wm_ <- sum(imputacion[imputacion > 0] * w[imputacion > 0]) /
                                                              max(sum(w[imputacion > 0]), 1)),
                          pct_aplicado_medio = if (is.na(c87)) NA_real_ else
                            round(mean(pct_imput[imputacion > 0], na.rm = TRUE), 2))]
  message(sprintf("  Imputacion de rentas (c.89) en vivienda no habitual: declarada en el %.1f%% de las viviendas; media %s EUR cuando se aplica; %% aplicado medio %s",
                  ci$pct_con_imputacion, fmt(ci$imputacion_media_si_aplica),
                  ifelse(is.na(ci$pct_aplicado_medio), "n/d", ci$pct_aplicado_medio)))
} else {
  b[, imputacion := 0]
  message("  AVISO: el panel no trae la casilla 89 (renta imputada): la imputacion se computa como 0. Ejecuta 01c v3.")
}
b[, irpf := (base_gravada + imputacion) * t_marg]
b[, neto := bruto - irpf]

# --- agregacion por modalidad ------------------------------------------------
resumen_mod <- function(d, etiqueta) {
  d[, .(grupo = etiqueta,
        viviendas = round(sum(w)),
        ingresos_anuales = round(wm(ingresos, w)),
        gastos_deducibles = round(wm(gastos, w)),
        pct_gastos_sobre_ingresos = round(100 * wm(gastos, w) / wm(ingresos, w), 1),
        beneficio_bruto_anual = round(wm(bruto, w)),
        margen_bruto_pct = round(100 * wm(bruto, w) / wm(ingresos, w), 1),
        reduccion_media = round(wm(reduccion, w)),
        pct_reduccion_sobre_bruto_positivo = round(100 * sum(reduccion * w) /
                                                   pmax(sum(pmax(bruto, 0) * w), 1), 1),
        imputacion_declarada_media = round(wm(imputacion, w)),
        pct_viviendas_con_imputacion = round(100 * sum(w[imputacion > 0]) / sum(w), 1),
        tipo_marginal_medio_pct = round(100 * wm(t_marg, w), 1),
        irpf_estimado = round(wm(irpf, w)),
        beneficio_neto_anual = round(wm(neto, w)),
        margen_neto_pct = round(100 * wm(neto, w) / wm(ingresos, w), 1),
        dias_alquiler = round(wm(dias, w)),
        ingresos_por_dia = round(sum(ingresos * w) / sum(dias * w, na.rm = TRUE), 1),
        beneficio_bruto_por_dia = round(sum(bruto * w) / sum(dias * w, na.rm = TRUE), 1),
        beneficio_neto_por_dia = round(sum(neto * w) / sum(dias * w, na.rm = TRUE), 1))]
}
R <- rbind(resumen_mod(b[uso == "hab"], "Vivienda habitual"),
           resumen_mod(b[uso == "tur"], "Vivienda no habitual"))

# --- POR DECLARANTE (unidad del informe: modulo 2) ---------------------------
# Para cada declarante se suma lo que declara en TODAS sus viviendas de la
# modalidad (importes tal y como los declara, es decir, en su porcentaje de
# titularidad, igual que la casilla 102 del modulo 2), y se promedia con
# FACTORCAL sobre los declarantes de esa modalidad. Asi el beneficio es
# directamente comparable con los ingresos por declarante del informe.
b[, `:=`(ing_decl = num(INGRESOS_102),
         bruto_decl = num(get(c149)),
         red_decl = if (is.na(c150)) 0 else num(get(c150)),
         imp_decl = imputacion * pmax(num(share), 1e-9),
         irpf_decl = irpf * pmax(num(share), 1e-9))]
por_decl <- function(mask, etiqueta) {
  d <- b[mask, .(ing = sum(ing_decl), bru = sum(bruto_decl), red = sum(red_decl),
                 imp = sum(imp_decl), irp = sum(irpf_decl), viv = .N,
                 w = FACTORCAL[1], tmg = t_marg[1]), by = IDENPER]
  if (!nrow(d)) return(NULL)
  d[, net := bru - irp]
  data.table(
    grupo = etiqueta,
    declarantes = round(sum(d$w)),
    viviendas_por_declarante = round(wm(d$viv, d$w), 2),
    ingresos_anuales = round(wm(d$ing, d$w)),
    gastos_deducibles = round(wm(d$ing - d$bru, d$w)),
    pct_gastos_sobre_ingresos = round(100 * wm(d$ing - d$bru, d$w) / wm(d$ing, d$w), 1),
    beneficio_bruto_anual = round(wm(d$bru, d$w)),
    margen_bruto_pct = round(100 * wm(d$bru, d$w) / wm(d$ing, d$w), 1),
    reduccion_media = round(wm(d$red, d$w)),
    imputacion_declarada_media = round(wm(d$imp, d$w)),
    tipo_marginal_medio_pct = round(100 * wm(d$tmg, d$w), 1),
    irpf_estimado = round(wm(d$irp, d$w)),
    beneficio_neto_anual = round(wm(d$net, d$w)),
    margen_neto_pct = round(100 * wm(d$net, d$w) / wm(d$ing, d$w), 1))
}
D <- rbind(por_decl(b$uso == "hab", "Vivienda habitual"),
           por_decl(b$uso == "tur", "Vivienda no habitual"))
escalera <- data.table(
  unidad = c(rep("por declarante", 3), rep("por vivienda entera (100% titularidad)", 3),
             rep("por dia de alquiler (por vivienda)", 3)),
  escalon = rep(c("Ingresos integros anuales", "Beneficio bruto (tras gastos)",
                  "Beneficio neto (tras IRPF)"), 3),
  vivienda_habitual = c(D$ingresos_anuales[1], D$beneficio_bruto_anual[1], D$beneficio_neto_anual[1],
                        R$ingresos_anuales[1], R$beneficio_bruto_anual[1], R$beneficio_neto_anual[1],
                        R$ingresos_por_dia[1], R$beneficio_bruto_por_dia[1], R$beneficio_neto_por_dia[1]),
  vivienda_no_habitual = c(D$ingresos_anuales[2], D$beneficio_bruto_anual[2], D$beneficio_neto_anual[2],
                           R$ingresos_anuales[2], R$beneficio_bruto_anual[2], R$beneficio_neto_anual[2],
                           R$ingresos_por_dia[2], R$beneficio_bruto_por_dia[2], R$beneficio_neto_por_dia[2]))
escalera[, diferencial_no_hab_vs_hab_pct := round(100 * (vivienda_no_habitual / vivienda_habitual - 1), 1)]

message("\nESCALERA ingresos -> beneficio (", ANIO_REF, "):")
for (i in seq_len(nrow(escalera)))
  message(sprintf("   [%-38s] %-30s habitual %9s | no habitual %9s | dif %+6.1f%%",
                  escalera$unidad[i], escalera$escalon[i], fmt(escalera$vivienda_habitual[i], 1),
                  fmt(escalera$vivienda_no_habitual[i], 1), escalera$diferencial_no_hab_vs_hab_pct[i]))
message(sprintf("   Gastos sobre ingresos: habitual %.1f%% | no habitual %.1f%% (por declarante)",
                D$pct_gastos_sobre_ingresos[1], D$pct_gastos_sobre_ingresos[2]))
message(sprintf("   Reduccion 23.2 LIRPF sobre el bruto positivo: habitual %.1f%% | no habitual %.1f%% (debe ser ~0)",
                R$pct_reduccion_sobre_bruto_positivo[1], R$pct_reduccion_sobre_bruto_positivo[2]))

# --- desglose de gastos ------------------------------------------------------
desglose <- NULL
if (length(gcols)) {
  gc2 <- c(gcols, "g_resto_financiacion_otros")
  desglose <- rbindlist(lapply(c(hab = "Vivienda habitual", tur = "Vivienda no habitual"), function(et) {
    d <- b[uso == names(which(c(hab = "Vivienda habitual", tur = "Vivienda no habitual") == et))]
    data.table(concepto = sub("^g_", "", gc2),
               euros_anuales_por_vivienda = round(sapply(gc2, function(cc) wm(d[[cc]], d$w))),
               pct_de_los_ingresos = round(100 * sapply(gc2, function(cc) wm(d[[cc]], d$w)) / wm(d$ingresos, d$w), 1),
               modalidad = et)
  }))
}

# --- uni / multi (cartera total) dentro del no habitual ------------------------
cart <- b[, .(n_total = .N), by = IDENPER]
b <- merge(b, cart, by = "IDENPER", all.x = TRUE)
um <- rbind(resumen_mod(b[uso == "tur" & n_total == 1], "No habitual - Uniarrendador (1 vivienda en alquiler)"),
            resumen_mod(b[uso == "tur" & n_total >= 2], "No habitual - Multiarrendador (2 o mas)"))

# --- equivalentes NETOS de las variables del informe (M2) --------------------
# Las metricas del informe principal son de INGRESOS INTEGROS (c.102), igual que
# la estadistica de la AEAT. Aqui se recalculan las mismas con el rendimiento
# neto fiscal (c.149) para poder mostrarlas en paralelo.
neto_decl <- b[, .(ing = sum(num(INGRESOS_102)), net = sum(num(get(c149))), w = FACTORCAL[1]),
               by = .(IDENPER, uso)][, .(
  ingresos_medios_por_declarante = round(sum(ing * w) / sum(w)),
  rendimiento_neto_medio_por_declarante = round(sum(net * w) / sum(w))), by = uso]
neto_viv <- b[, .(alquiler_medio_mensual_ingresos = round(wm(ingresos, w) / 12),
                  rendimiento_neto_medio_mensual = round(wm(bruto, w) / 12),
                  ingresos_por_dia = round(sum(ingresos * w) / sum(dias * w, na.rm = TRUE), 1),
                  rendimiento_neto_por_dia = round(sum(bruto * w) / sum(dias * w, na.rm = TRUE), 1)), by = uso]
equiv <- merge(neto_decl, neto_viv, by = "uso")
equiv[, modalidad := fifelse(uso == "hab", "Vivienda habitual", "Vivienda no habitual")][, uso := NULL]
setcolorder(equiv, "modalidad")
d2 <- function(col) round(100 * (equiv[[col]][2] / equiv[[col]][1] - 1), 1)
equiv_dif <- data.table(variable = names(equiv)[-1],
                        diferencial_no_hab_vs_hab_pct = sapply(names(equiv)[-1], d2))
message("\nEQUIVALENTES NETOS de M2 (diferencial no habitual vs habitual): ",
        paste(sprintf("%s %+.1f%%", equiv_dif$variable, equiv_dif$diferencial_no_hab_vs_hab_pct), collapse = " | "))

# --- comparativa temporal 2016 vs 2023 ---------------------------------------
# Si el 02 dejo en memoria las bases de otros ejercicios (fin_full / datos), se
# repite la escalera para cada ano. En 2016 el fichero 8 tiene otra numeracion
# (P60 ingresos, P70 rendimiento neto, P71 reduccion, P59 renta imputada), pero
# el 01c las exporta con los MISMOS nombres, asi que el calculo es identico.
serie_ben <- NULL
otros <- if (exists("fin_full")) fin_full else if (exists("datos")) datos else NULL
if (!is.null(otros)) {
  anios <- names(otros)
  esc_ano <- function(a) {
    bb <- tryCatch(copy(otros[[a]]$base), error = function(e) NULL)
    if (is.null(bb) || !nrow(bb)) return(NULL)
    c149a <- col_or(bb, c("REND_NETO_149", "RNETO_149"))
    if (is.na(c149a)) { message("  (", a, ": sin rendimiento neto en el panel; ejecuta 01c sobre ese ejercicio)"); return(NULL) }
    bb[, w := share * FACTORCAL]
    sha <- pmax(num(bb$share), 1e-9)
    bb[, ingresos := num(INGRESOS_102) / sha]
    bb[, bruto := num(get(c149a)) / sha]
    c150a <- col_or(bb, c("REDUCCION_150", "REDUC_150"))
    bb[, reduccion := if (is.na(c150a)) 0 else num(get(c150a)) / sha]
    # base gravada: rendimiento neto reducido oficial (c.154 / P74) si esta
    c154a <- col_or(bb, "REND_NETO_RED_154")
    c151a <- col_or(bb, "REDUCCION_151"); c152a <- col_or(bb, "REND_MIN_152")
    bb[, red151 := if (is.na(c151a)) 0 else num(get(c151a)) / sha]
    bb[, min152 := if (is.na(c152a)) 0 else num(get(c152a)) / sha]
    bb[, base_calc := pmax(pmax(bruto, 0) -
                           fifelse(bruto > 0, pmin(reduccion + red151, pmax(bruto, 0)), 0),
                           min152)]
    bb[, base_gravada := base_calc]
    # imputacion declarada (c.89 / P59)
    c89a <- col_or(bb, "RENTA_IMPUTADA_89")
    bb[, imputacion := if (is.na(c89a)) 0 else num(get(c89a)) / sha]
    # IRPF con la escala PROPIA de ese ejercicio y la renta del declarante de
    # ese mismo ano (pers del ejercicio): marcos fiscales no mezclados.
    pa <- tryCatch(otros[[a]]$pers, error = function(e) NULL)
    hay_renta <- !is.null(pa) && "renta_total" %in% names(pa)
    if (hay_renta) {
      ra <- data.table(IDENPER = pa[[1]], renta_total = pa$renta_total)
      bb <- merge(bb, ra, by = "IDENPER", all.x = TRUE)
      bb[, t_marg := tipo_marginal(renta_total, a)]
      bb[, irpf := (base_gravada + imputacion) * t_marg]
      bb[, neto := bruto - irpf]
    } else {
      message("  (", a, ": sin renta del declarante; no se calcula el beneficio neto de ese ano)")
      bb[, `:=`(irpf = NA_real_, neto = NA_real_)]
    }
    # por declarante: suma de lo declarado en todas sus viviendas de la
    # modalidad (misma unidad que el modulo 2 del informe)
    dcl <- bb[, .(ing = sum(num(INGRESOS_102)), bru = sum(num(get(c149a))),
                  irp = sum(irpf * pmax(num(share), 1e-9)), w = FACTORCAL[1]),
              by = .(IDENPER, uso)]
    dcl[, net := bru - irp]
    pd <- dcl[, .(ingresos_por_declarante = round(wm(ing, w)),
                  beneficio_bruto_por_declarante = round(wm(bru, w)),
                  beneficio_neto_por_declarante = round(wm(net, w))), by = uso]
    bb <- merge(bb, pd, by = "uso", all.x = TRUE)
    bb[, .(anio = as.integer(a),
           modalidad = fifelse(uso == "hab", "Vivienda habitual", "Vivienda no habitual"),
           ingresos_por_declarante = ingresos_por_declarante[1],
           beneficio_bruto_por_declarante = beneficio_bruto_por_declarante[1],
           beneficio_neto_por_declarante = beneficio_neto_por_declarante[1],
           ingresos_anuales = round(wm(ingresos, w)),
           beneficio_bruto_anual = round(wm(bruto, w)),
           margen_bruto_pct = round(100 * wm(bruto, w) / wm(ingresos, w), 1),
           reduccion_media = round(wm(reduccion, w)),
           imputacion_declarada_media = round(wm(imputacion, w)),
           tipo_marginal_medio_pct = round(100 * wm(t_marg, w), 1),
           irpf_estimado = round(wm(irpf, w)),
           beneficio_neto_anual = round(wm(neto, w)),
           margen_neto_pct = round(100 * wm(neto, w) / wm(ingresos, w), 1)),
       by = uso][, uso := NULL][]
  }
  serie_ben <- rbindlist(lapply(anios, esc_ano), fill = TRUE)
  if (!is.null(serie_ben) && nrow(serie_ben) >= 4) {
    setorder(serie_ben, anio, modalidad)
    dif_ano <- serie_ben[, .(diferencial_ingresos_pct = round(100 * (ingresos_anuales[modalidad == "Vivienda no habitual"] /
                                 ingresos_anuales[modalidad == "Vivienda habitual"] - 1), 1),
                             diferencial_beneficio_bruto_pct = round(100 * (beneficio_bruto_anual[modalidad == "Vivienda no habitual"] /
                                 beneficio_bruto_anual[modalidad == "Vivienda habitual"] - 1), 1),
                             diferencial_beneficio_neto_pct = round(100 * (beneficio_neto_anual[modalidad == "Vivienda no habitual"] /
                                 beneficio_neto_anual[modalidad == "Vivienda habitual"] - 1), 1)), by = anio]
    serie_ben <- merge(serie_ben, dif_ano, by = "anio")
    message("\nCOMPARATIVA TEMPORAL (ingresos / bruto / neto): ",
            paste(sprintf("%d: ingresos %+.1f%% / bruto %+.1f%% / neto %+.1f%%", dif_ano$anio,
                          dif_ano$diferencial_ingresos_pct, dif_ano$diferencial_beneficio_bruto_pct,
                          dif_ano$diferencial_beneficio_neto_pct),
                  collapse = " | "))
  } else serie_ben <- NULL
}

# --- graficos ----------------------------------------------------------------
# Replican, en BENEFICIO (bruto y neto), los mismos graficos que el informe
# principal presenta en INGRESOS, para poder mostrarlos en paralelo.
COL_HAB <- "#1F5C99"; COL_NH <- "#D1495B"; COL_UNI <- "#3D8361"; COL_MULTI <- "#E09F3E"
GRID <- "#e4e4e4"
graficos_b <- character(0)
g6 <- function(nombre, alto = 1500, ancho = 2400, mar = c(5.5, 7, 6.2, 2.5), expr) {
  f <- file.path(carpeta_anio(ANIO_REF), paste0(nombre, ".png"))
  ok <- tryCatch({
    png(f, width = ancho, height = alto, res = 220)
    on.exit(dev.off(), add = TRUE)
    par(mar = mar, mgp = c(4.1, 0.8, 0), tcl = -0.25, cex.axis = 0.85, cex.lab = 0.95,
        cex.main = 1.12, font.main = 1, xpd = NA, bg = "white", col.axis = "grey25")
    expr; TRUE
  }, error = function(e) { message("  (sin grafico ", nombre, ": ", e$message, ")"); FALSE })
  if (ok) { graficos_b <<- c(graficos_b, f); f } else NA_character_
}
ejeY <- function(tope, dec = 0) {
  at <- pretty(c(0, tope), n = 5); abline(h = at, col = GRID, lwd = 1)
  axis(2, at = at, labels = fmt(at, dec), las = 1, lwd = 0, lwd.ticks = 0, line = -0.4)
}
bar2 <- function(m, nombres, leyenda, main, ylab, sub = NULL,
                 cols = c(COL_HAB, COL_NH), dec = 0) {
  tope <- max(m, na.rm = TRUE) * 1.30
  bp <- barplot(m, beside = TRUE, names.arg = rep("", ncol(m)), col = cols, border = NA,
                ylim = c(0, tope), axes = FALSE, ylab = ylab)
  ejeY(tope, dec)
  barplot(m, beside = TRUE, names.arg = rep("", ncol(m)), col = cols, border = NA,
          ylim = c(0, tope), axes = FALSE, add = TRUE)
  text(bp, m, labels = fmt(m, dec), pos = 3, cex = 0.8, font = 2, col = "grey15", offset = 0.3)
  mtext(nombres, side = 1, at = colMeans(bp), line = 0.9, cex = 0.88)
  title(main = main, line = 4.0, adj = 0)
  if (!is.null(sub)) mtext(sub, side = 3, line = 2.5, adj = 0, cex = 0.78, col = "grey40")
  legend("top", legend = leyenda, fill = cols, border = NA, bty = "n", horiz = TRUE,
         cex = 0.88, inset = c(0, -0.10))
  invisible(bp)
}
SUB_UNI <- "Por declarante (suma de todas sus viviendas de la modalidad). Bruto = rendimiento neto fiscal (c.149); neto = tras IRPF al tipo marginal"

# B1. escalera ingresos -> bruto -> neto
g6("M6_g1_escalera_beneficio", expr = {
  m <- rbind(escalera$vivienda_habitual[1:3], escalera$vivienda_no_habitual[1:3])
  bp <- bar2(m, c("Ingresos integros", "Beneficio bruto", "Beneficio neto"),
             c("Vivienda habitual", "Vivienda no habitual"),
             "De los ingresos al beneficio: cuanta ventaja del no habitual sobrevive",
             "Euros al ano por vivienda", SUB_UNI)
  mtext(sprintf("dif. %+.0f%%", escalera$diferencial_no_hab_vs_hab_pct[1:3]),
        side = 1, at = colMeans(bp), line = 2.1, cex = 0.85, col = "grey30")
})

# B2. beneficio anual por vivienda (equivalente del grafico de alquiler medio)
g6("M6_g2_beneficio_anual_modalidad", expr = {
  m <- rbind(c(D$beneficio_bruto_anual[1], D$beneficio_neto_anual[1]),
             c(D$beneficio_bruto_anual[2], D$beneficio_neto_anual[2]))
  bar2(m, c("Beneficio bruto", "Beneficio neto (tras IRPF)"),
       c("Vivienda habitual", "Vivienda no habitual"),
       "Beneficio anual por declarante segun modalidad", "Euros al ano", SUB_UNI)
})

# B3. beneficio por DIA (equivalente del grafico de dias/precio por dia)
g6("M6_g3_beneficio_por_dia", expr = {
  m <- rbind(c(R$ingresos_por_dia[1], R$beneficio_bruto_por_dia[1], R$beneficio_neto_por_dia[1]),
             c(R$ingresos_por_dia[2], R$beneficio_bruto_por_dia[2], R$beneficio_neto_por_dia[2]))
  bar2(m, c("Ingresos/dia", "Beneficio bruto/dia", "Beneficio neto/dia"),
       c("Vivienda habitual", "Vivienda no habitual"),
       "Rendimiento por dia de alquiler: ingresos frente a beneficio",
       "Euros por dia alquilado",
       "Dias censales por referencia catastral. El no habitual se ocupa menos dias, lo que eleva su rendimiento diario",
       dec = 1)
})

# B4. margen: que parte de cada euro ingresado queda como beneficio
g6("M6_g4_margen_sobre_ingresos", expr = {
  m <- rbind(c(D$margen_bruto_pct[1], D$margen_neto_pct[1], D$pct_gastos_sobre_ingresos[1]),
             c(D$margen_bruto_pct[2], D$margen_neto_pct[2], D$pct_gastos_sobre_ingresos[2]))
  bar2(m, c("Margen bruto", "Margen neto", "Gastos deducibles"),
       c("Vivienda habitual", "Vivienda no habitual"),
       "Que parte de cada euro ingresado queda como beneficio",
       "% de los ingresos integros",
       "Margen bruto = beneficio bruto / ingresos; margen neto = beneficio neto / ingresos", dec = 1)
})

# B5. cuanto se lleva el IRPF y cuanto vale la reduccion 23.2
g6("M6_g5_carga_fiscal", expr = {
  m <- rbind(c(D$reduccion_media[1], D$irpf_estimado[1], D$imputacion_declarada_media[1]),
             c(D$reduccion_media[2], D$irpf_estimado[2], D$imputacion_declarada_media[2]))
  bar2(m, c("Reduccion 23.2 aplicada", "IRPF estimado", "Imputacion declarada"),
       c("Vivienda habitual", "Vivienda no habitual"),
       "La asimetria fiscal entre modalidades, en euros por declarante",
       "Euros al ano",
       "La reduccion del art. 23.2 LIRPF solo la aplica la vivienda habitual; la imputacion de rentas es la declarada (c.89)")
})

# B6. uni vs multi dentro del no habitual
if (nrow(um) == 2) g6("M6_g6_beneficio_uni_multi", expr = {
  m <- rbind(c(um$ingresos_anuales[1], um$beneficio_bruto_anual[1], um$beneficio_neto_anual[1]),
             c(um$ingresos_anuales[2], um$beneficio_bruto_anual[2], um$beneficio_neto_anual[2]))
  bar2(m, c("Ingresos integros", "Beneficio bruto", "Beneficio neto"),
       c("Uniarrendador", "Multiarrendador"),
       "Beneficio del alquiler no habitual segun tipo de arrendador",
       "Euros al ano por vivienda",
       "Clasificacion por cartera TOTAL de viviendas en alquiler del declarante",
       cols = c(COL_UNI, COL_MULTI))
})

# B7. desglose de gastos por concepto y modalidad
if (!is.null(desglose)) g6("M6_g7_desglose_gastos", mar = c(11, 7, 6.2, 2.5), expr = {
  d <- as.data.table(desglose)[euros_anuales_por_vivienda > 0]
  w <- dcast(d, concepto ~ modalidad, value.var = "euros_anuales_por_vivienda", fill = 0)
  w[, tot := rowSums(.SD), .SDcols = setdiff(names(w), "concepto")]
  setorder(w, -tot); w <- head(w, 10)
  m <- t(as.matrix(w[, setdiff(names(w), c("concepto", "tot")), with = FALSE]))
  tope <- max(m) * 1.30
  bp <- barplot(m, beside = TRUE, names.arg = rep("", ncol(m)), col = c(COL_HAB, COL_NH),
                border = NA, ylim = c(0, tope), axes = FALSE, ylab = "Euros al ano por vivienda")
  ejeY(tope)
  barplot(m, beside = TRUE, names.arg = rep("", ncol(m)), col = c(COL_HAB, COL_NH),
          border = NA, ylim = c(0, tope), axes = FALSE, add = TRUE)
  mtext(gsub("_", " ", w$concepto), side = 1, at = colMeans(bp), line = 0.7, cex = 0.7, las = 2)
  title(main = "En que se van los gastos deducibles, por modalidad", line = 4.0, adj = 0)
  mtext("Casillas del Modelo 100 que componen el rendimiento neto (c.149)",
        side = 3, line = 2.5, adj = 0, cex = 0.78, col = "grey40")
  legend("top", legend = rownames(m), fill = c(COL_HAB, COL_NH), border = NA, bty = "n",
         horiz = TRUE, cex = 0.88, inset = c(0, -0.10))
})

# B8. evolucion del diferencial: ingresos vs beneficio bruto
if (!is.null(serie_ben)) g6("M6_g8_evolucion_diferencial", expr = {
  d <- unique(serie_ben[, .(anio, diferencial_ingresos_pct, diferencial_beneficio_bruto_pct)])
  setorder(d, anio)
  hay_neto <- all(is.finite(d$diferencial_beneficio_neto_pct))
  m <- if (hay_neto) rbind(d$diferencial_ingresos_pct, d$diferencial_beneficio_bruto_pct,
                           d$diferencial_beneficio_neto_pct)
       else rbind(d$diferencial_ingresos_pct, d$diferencial_beneficio_bruto_pct)
  leg <- if (hay_neto) c("En ingresos", "En beneficio bruto", "En beneficio neto")
         else c("En ingresos", "En beneficio bruto")
  bar2(m, as.character(d$anio), leg,
       "Evolucion de la ventaja del alquiler no habitual",
       "% sobre la vivienda habitual",
       "Bruto: tras gastos deducibles (c.149 / P70). Neto: tras IRPF al tipo marginal con la escala de cada ejercicio",
       cols = c(COL_HAB, COL_NH, COL_MULTI)[seq_len(nrow(m))], dec = 1)
})

message("\nGraficos de beneficio generados: ", length(graficos_b))

# --- volcado -----------------------------------------------------------------
hojas_b <- Filter(Negate(is.null), list(
  Escalera = escalera, Por_declarante = D, Por_vivienda = R, Uni_multi_no_habitual = um,
  Desglose_gastos = desglose, Serie_beneficio_anios = serie_ben, Equivalentes_netos_M2 = equiv, Equivalentes_netos_dif = equiv_dif,
  Notas = data.frame(nota = c(
    "Unidad: vivienda entera al 100% de titularidad, ponderada por participacion x FACTORCAL; dias censales por referencia catastral.",
    "Beneficio bruto = rendimiento neto fiscal (casilla 149) = ingresos integros (c.102) menos gastos deducibles. Los gastos fiscales pueden diferir de los economicos: intereses y reparaciones tienen limite anual (exceso a 4 anios), y costes no deducibles (tiempo propio, comisiones no declaradas) no aparecen.",
    "Reduccion art. 23.2 LIRPF (c.150): en 2023, 60% del rendimiento neto positivo, solo vivienda habitual. Desde 2024: 60% contratos anteriores al 26/5/2023; nuevos: 50% general, 60% rehabilitada, 70%/90% zonas tensionadas. Turistico/temporal: sin reduccion.",
    "Imputacion de rentas: la DECLARADA en la c.89 por los dias no arrendados (porcentaje aplicado en c.87), sin simulacion; la hoja Por_modalidad indica en que % de viviendas se aplica.",
    "Base gravada = rendimiento neto reducido oficial (c.154 = max(149-150-151, 152)); IRPF estimado = (base gravada + imputacion) x tipo MARGINAL del declarante segun su renta bruta (escala general 2023 estatal + autonomica media). No incluye minimos personales ni deducciones autonomicas.",
    "Unidad principal: POR DECLARANTE (suma de sus viviendas de cada modalidad, importes en su porcentaje de titularidad), coherente con los ingresos por declarante del modulo 2 del informe. La hoja Por_vivienda ofrece la misma escalera por vivienda entera al 100% de titularidad (unidad del contraste con la AEAT).",
    sprintf("Tipo de gravamen: %s.", TIPO_ORIGEN),
    "Cuando el panel trae las cuotas integras (fichero 4), el tipo marginal se ESTIMA de los datos por comunidad autonoma y ejercicio (derivada de la cuota respecto de la base liquidable en tramos finos), de modo que recoge la escala autonomica real de cada territorio; no se impone ninguna escala teorica.",
    "La serie temporal publica las dos unidades: '..._por_declarante' (comparable con el modulo 2 y con M5 del informe) y las columnas sin sufijo, que son por vivienda entera al 100% de titularidad.",
    "Serie temporal: cada ejercicio se grava con SU escala del IRPF (2016: 19/24/30/37/45%; 2023: idem mas 47% desde 300.000) y con la renta del declarante de ese mismo ano; los marcos fiscales no se mezclan.",
    "Las metricas del informe principal (alquiler medio, ingresos por declarante, ingresos por dia) son de INGRESOS INTEGROS, como la estadistica AEAT; la hoja Equivalentes_netos_M2 las recalcula con el rendimiento neto (c.149).",
    "Fuera del alcance: IVA (el alquiler de vivienda esta exento; el turistico con servicios hoteleros no), arrendamiento como actividad economica y personas juridicas."))))
ruta_b <- file.path(carpeta_anio(ANIO_REF), "beneficio_modalidades.xlsx")
ok <- FALSE
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  for (h in names(hojas_b)) { openxlsx::addWorksheet(wb, h); openxlsx::writeData(wb, h, as.data.frame(hojas_b[[h]])) }
  if (length(graficos_b)) {
    openxlsx::addWorksheet(wb, "Graficos")
    fila <- 1
    for (f in graficos_b) {
      openxlsx::insertImage(wb, "Graficos", f, width = 10, height = 6.25, startRow = fila, startCol = 2)
      fila <- fila + 34
    }
  }
  openxlsx::saveWorkbook(wb, ruta_b, overwrite = TRUE); ok <- TRUE
}
if (!ok) for (h in names(hojas_b))
  write.csv2(as.data.frame(hojas_b[[h]]), sub("\\.xlsx$", paste0("_", h, ".csv"), ruta_b), row.names = FALSE)
message("\nBeneficio por modalidad guardado en: ", if (ok) ruta_b else sub("\\.xlsx$", "_*.csv", ruta_b))
