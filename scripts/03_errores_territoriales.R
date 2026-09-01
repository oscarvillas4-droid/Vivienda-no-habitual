# =============================================================================
# 03_errores_territoriales.R
# -----------------------------------------------------------------------------
# ¿Cuánto error asumo al estimar la VIVIENDA NO HABITUAL por territorio?
#
# Este script cuantifica, nivel a nivel (nacional -> CCAA -> provincia ->
# capitales de provincia), los TRES errores que componen la estimación:
#
#   E1. ERROR DE CLASIFICACION (filtros): la desviacion sistematica entre lo
#       que el criterio identifica como "vivienda no habitual" y el dato
#       oficial de la AEAT, alli donde la AEAT publica ancla (nacional, CCAA
#       y provincia). A nivel nacional el muestreo es despreciable y la
#       localizacion no interviene, asi que la desviacion nacional es una
#       medida casi pura del error de los filtros.
#
#   E2. ERROR DE LOCALIZACION (cobertura del modulo): la parte de viviendas
#       no habituales cuya referencia catastral no cruza con el modulo
#       inmobiliario y que, por tanto, no puede asignarse a una provincia o
#       municipio. Se mide su tamano, si difieren sistematicamente de las
#       localizadas (sesgo potencial) y la COTA de cuanto podria mover cada
#       provincia si se repartieran de otra manera.
#
#   E3. ERROR MUESTRAL (panel ~5%): la varianza de estimar con una muestra,
#       cuantificada por bootstrap remuestreando DECLARANTES completos
#       (respeta la correlacion intra-declarante). Crece al descender de
#       nivel territorial y es el unico error medible a nivel municipal,
#       donde no existe ancla oficial.
#
# USO:  source("02_informe_arrendadores.R", encoding = "UTF-8")  # primero
#       source("03_errores_territoriales.R", encoding = "UTF-8") # despues
# Necesita los objetos del 02 en memoria: base, AEAT_PROV, ANIO_REF,
# carpeta_anio(), fmt(). Parametros abajo.
# =============================================================================

if (!exists("base") || !exists("AEAT_PROV"))
  stop("Ejecuta antes 02_informe_arrendadores.R en esta misma sesion de R.")

B_BOOT  <- as.integer(Sys.getenv("B_BOOT", "300"))  # replicas bootstrap
MIN_N   <- 30      # muestra minima para publicar una celda
AEAT_NACIONAL_NH <- 309479L  # viviendas no habituales, dato oficial 2023

suppressPackageStartupMessages(library(data.table))
set.seed(20240807)

message("\n", strrep("=", 74), "\n  ERRORES TERRITORIALES DE LA VIVIENDA NO HABITUAL (E1 filtros, ",
        "E2 localizacion, E3 muestreo)\n", strrep("=", 74))

# --- 0. preparacion ----------------------------------------------------------
CCAA_DE_PROV <- c(
  "04"="Andalucia","11"="Andalucia","14"="Andalucia","18"="Andalucia",
  "21"="Andalucia","23"="Andalucia","29"="Andalucia","41"="Andalucia",
  "22"="Aragon","44"="Aragon","50"="Aragon","33"="Asturias",
  "07"="Illes Balears","35"="Canarias","38"="Canarias","39"="Cantabria",
  "05"="Castilla y Leon","09"="Castilla y Leon","24"="Castilla y Leon",
  "34"="Castilla y Leon","37"="Castilla y Leon","40"="Castilla y Leon",
  "42"="Castilla y Leon","47"="Castilla y Leon","49"="Castilla y Leon",
  "02"="Castilla-La Mancha","13"="Castilla-La Mancha","16"="Castilla-La Mancha",
  "19"="Castilla-La Mancha","45"="Castilla-La Mancha",
  "08"="Cataluna","17"="Cataluna","25"="Cataluna","43"="Cataluna",
  "03"="C. Valenciana","12"="C. Valenciana","46"="C. Valenciana",
  "06"="Extremadura","10"="Extremadura",
  "15"="Galicia","27"="Galicia","32"="Galicia","36"="Galicia",
  "28"="Madrid","30"="Murcia","26"="La Rioja","51"="Ceuta","52"="Melilla")

CAPITALES <- c("04013","11012","14021","18087","21041","23050","29067","41091",
               "22125","44216","50297","33044","07040","35016","38038","39075",
               "05019","09059","24089","34120","37274","40194","42173","47186",
               "49275","02003","13034","16078","19130","45168","08019","17079",
               "25120","43148","03014","12040","46250","06015","10037","15030",
               "27028","32054","36038","28079","30030","26089","51001","52001")

bnh <- base[uso == "tur"]
bnh[, veq := share * FACTORCAL]
pv <- suppressWarnings(as.integer(as.character(bnh$PROV_INM)))
bnh[, prov5 := fifelse(!is.na(pv) & pv %in% 1:52, sprintf("%02d", pv), NA_character_)]
if ("MUN_INM" %in% names(bnh)) {
  mv <- gsub("[^0-9]", "", as.character(bnh$MUN_INM))
  bnh[, muni5 := fifelse(!is.na(prov5) & nchar(mv) >= 1 & mv != "",
                         paste0(prov5, formatC(mv, width = 3, flag = "0")),
                         NA_character_)]
} else bnh[, muni5 := NA_character_]
bnh[, ccaa := unname(CCAA_DE_PROV[prov5])]

est_nac <- bnh[, sum(veq)]

# --- 1. E2: LOCALIZACION (cobertura y sesgo potencial) -----------------------
cob_prov <- bnh[, sum(veq[!is.na(prov5)]) / sum(veq)]
cob_muni <- bnh[, sum(veq[!is.na(muni5)]) / sum(veq)]
message(sprintf("\n[E2] Cobertura de localizacion del no habitual: provincia %.1f%% | municipio %.1f%%",
                100 * cob_prov, 100 * cob_muni))

wmean <- function(x, w) { ok <- !is.na(x) & !is.na(w); if (!any(ok)) NA_real_
  else sum(x[ok] * w[ok]) / sum(w[ok]) }
ing100 <- suppressWarnings(as.numeric(bnh$INGRESOS_102)) /
          pmax(suppressWarnings(as.numeric(bnh$share)), 1e-9)
dds <- if ("DIAS_RC" %in% names(bnh)) {
  x <- suppressWarnings(as.numeric(bnh$DIAS_RC)); fifelse(is.na(x), bnh$dias_ef, pmin(x, 365))
} else bnh$dias_ef
sesgo_loc <- data.table(
  grupo = c("Localizadas (con provincia)", "NO localizadas"),
  viviendas = c(round(bnh[!is.na(prov5), sum(veq)]), round(bnh[is.na(prov5), sum(veq)])),
  pct = round(100 * c(cob_prov, 1 - cob_prov), 1),
  ingreso_anual_medio_100pct = round(c(wmean(ing100[!is.na(bnh$prov5)], bnh$veq[!is.na(bnh$prov5)]),
                                       wmean(ing100[is.na(bnh$prov5)],  bnh$veq[is.na(bnh$prov5)]))),
  dias_medios = round(c(wmean(dds[!is.na(bnh$prov5)], bnh$veq[!is.na(bnh$prov5)]),
                        wmean(dds[is.na(bnh$prov5)],  bnh$veq[is.na(bnh$prov5)])), 1))

# Cota del error de localizacion por provincia: comparar la distribucion
# observada (solo localizadas) con el escenario en que las NO localizadas se
# reparten segun la provincia de RESIDENCIA de su declarante. La diferencia,
# en viviendas y en %, acota cuanto puede mover la localizacion cada celda.
loc_obs <- bnh[!is.na(prov5), .(v_obs = sum(veq)), by = .(cpro = prov5)]
resid_nl <- bnh[is.na(prov5) & !is.na(cpro2), .(v_extra = sum(veq)), by = .(cpro = cpro2)]
cota_loc <- merge(loc_obs, resid_nl, by = "cpro", all = TRUE)
for (cc in c("v_obs", "v_extra")) set(cota_loc, which(is.na(cota_loc[[cc]])), cc, 0)
cota_loc[, `:=`(v_reasignado = v_obs + v_extra,
                cota_pct = round(100 * v_extra / pmax(v_obs, 1), 1))]
setorder(cota_loc, -v_obs)

# --- 2. E3: MUESTREO (bootstrap por declarante) ------------------------------
message(sprintf("[E3] Bootstrap por declarante: B = %d replicas...", B_BOOT))
agg_id <- function(unidad_col) {
  d <- bnh[!is.na(get(unidad_col)), .(veq = sum(veq)), by = c("IDENPER", unidad_col)]
  setnames(d, unidad_col, "unidad"); d
}
niveles_id <- list(
  nacional = bnh[, .(veq = sum(veq), unidad = "ESPANA"), by = IDENPER],
  ccaa     = agg_id("ccaa"),
  prov     = agg_id("prov5"),
  capital  = { d <- agg_id("muni5"); d[unidad %in% CAPITALES] })

ids <- unique(bnh$IDENPER); n_ids <- length(ids)
boot_res <- lapply(niveles_id, function(d) {
  unids <- sort(unique(d$unidad))
  M <- matrix(0, nrow = B_BOOT, ncol = length(unids), dimnames = list(NULL, unids))
  idx <- match(d$IDENPER, ids)
  for (b in seq_len(B_BOOT)) {
    mult <- tabulate(sample.int(n_ids, n_ids, replace = TRUE), nbins = n_ids)
    M[b, ] <- rowsum(d$veq * mult[idx], d$unidad)[unids, 1]
  }
  est <- rowsum(d$veq, d$unidad)[unids, 1]
  data.table(unidad = unids, est = as.numeric(est),
             sd_boot = apply(M, 2, sd),
             ic_inf = apply(M, 2, quantile, 0.025),
             ic_sup = apply(M, 2, quantile, 0.975))
})
for (nv in names(boot_res)) boot_res[[nv]][, cv_pct := round(100 * sd_boot / pmax(est, 1e-9), 1)]

n_muestra <- list(
  ccaa    = bnh[!is.na(ccaa),  .N, by = .(unidad = ccaa)],
  prov    = bnh[!is.na(prov5), .N, by = .(unidad = prov5)],
  capital = bnh[muni5 %in% CAPITALES, .N, by = .(unidad = muni5)])

# --- 3. E1: CLASIFICACION frente al ancla oficial ----------------------------
semaforo <- function(cv, n, desv = NA_real_, dentro = NA) {
  L <- max(length(cv), length(n), length(desv), length(dentro))
  cv <- rep_len(cv, L); n <- rep_len(n, L)
  desv <- rep_len(desv, L); dentro <- rep_len(as.logical(dentro), L)
  out <- rep("AMBAR (publicar con IC)", L)
  out[cv <= 5 & (is.na(desv) | abs(desv) <= 10 | dentro %in% TRUE)] <- "VERDE"
  out[cv > 15] <- "ROJO (CV > 15%)"
  out[!is.na(desv) & !(dentro %in% TRUE) & abs(desv) > 15] <- "ROJO (sesgo > 15%)"
  out[!is.na(n) & n < MIN_N] <- "ROJO (muestra insuficiente)"
  out
}

nac <- boot_res$nacional
nac[, `:=`(aeat = AEAT_NACIONAL_NH,
           ratio = round(est / AEAT_NACIONAL_NH, 3),
           desv_sistematica_pct = round(100 * (est / AEAT_NACIONAL_NH - 1), 1),
           dentro_ic = AEAT_NACIONAL_NH >= ic_inf & AEAT_NACIONAL_NH <= ic_sup)]
message(sprintf(paste0("\n[E1] NACIONAL: estimado %s | AEAT %s | ratio %.3f -> ",
        "error de clasificacion = %+.1f%% (IC95 bootstrap: %s a %s; CV %.1f%%)"),
        fmt(nac$est), fmt(nac$aeat), nac$ratio, nac$desv_sistematica_pct,
        fmt(nac$ic_inf), fmt(nac$ic_sup), nac$cv_pct))

ap <- as.data.table(AEAT_PROV)[, .(unidad = cpro, provincia = provincia_aeat,
                                   aeat = viviendas_aeat)]
prov <- merge(boot_res$prov, ap, by = "unidad", all.x = TRUE)
prov <- merge(prov, n_muestra$prov, by = "unidad", all.x = TRUE)
prov[, `:=`(ratio = round(est / aeat, 3),
            desv_sistematica_pct = round(100 * (est / aeat - 1), 1),
            dentro_ic = aeat >= ic_inf & aeat <= ic_sup)]
prov[, veredicto := fifelse(is.na(aeat), "sin ancla oficial",
                    fifelse(dentro_ic, "compatible: la desviacion cabe en la varianza muestral",
                            sprintf("SESGO sistematico %+.1f%% (filtros + localizacion)",
                                    desv_sistematica_pct)))]
prov[, semaforo := semaforo(cv_pct, N, desv_sistematica_pct, dentro_ic)]
setorder(prov, -est)

cc_aeat <- ap[, .(aeat = sum(aeat)), by = .(unidad = unname(CCAA_DE_PROV[unidad]))]
ccaa <- merge(boot_res$ccaa, cc_aeat, by = "unidad", all.x = TRUE)
ccaa <- merge(ccaa, n_muestra$ccaa, by = "unidad", all.x = TRUE)
ccaa[, `:=`(ratio = round(est / aeat, 3),
            desv_sistematica_pct = round(100 * (est / aeat - 1), 1),
            dentro_ic = aeat >= ic_inf & aeat <= ic_sup)]
ccaa[, veredicto := fifelse(dentro_ic, "compatible (varianza muestral)",
                            sprintf("SESGO sistematico %+.1f%%", desv_sistematica_pct))]
ccaa[, semaforo := semaforo(cv_pct, N, desv_sistematica_pct, dentro_ic)]
setorder(ccaa, -est)

cap <- merge(boot_res$capital, n_muestra$capital, by = "unidad", all.x = TRUE)
if (exists("nommuni")) cap[, municipio := nommuni(unidad)] else cap[, municipio := unidad]
cap[, nota := "Sin ancla oficial municipal: solo se mide E3 (muestreo); E1 se hereda de su provincia como cota; E2 = 1 - cobertura municipal"]
cap[, semaforo := semaforo(cv_pct, N)]
setorder(cap, -est)

# --- 4. resumen de la escalera de errores ------------------------------------
resumen <- rbindlist(list(
  data.table(nivel = "Nacional", unidades = 1L,
             cv_mediano = nac$cv_pct, cv_p90 = nac$cv_pct,
             ancla_oficial = "si",
             desv_sistematica = sprintf("%+.1f%%", nac$desv_sistematica_pct),
             pct_verde = 100 * (semaforo(nac$cv_pct, Inf, nac$desv_sistematica_pct,
                                         nac$dentro_ic) == "VERDE")),
  data.table(nivel = "CCAA", unidades = nrow(ccaa),
             cv_mediano = median(ccaa$cv_pct), cv_p90 = quantile(ccaa$cv_pct, .9),
             ancla_oficial = "si",
             desv_sistematica = sprintf("mediana %+.1f%%", median(ccaa$desv_sistematica_pct, na.rm = TRUE)),
             pct_verde = round(100 * mean(ccaa$semaforo == "VERDE"), 0)),
  data.table(nivel = "Provincia", unidades = nrow(prov),
             cv_mediano = median(prov$cv_pct), cv_p90 = quantile(prov$cv_pct, .9),
             ancla_oficial = "si",
             desv_sistematica = sprintf("mediana %+.1f%%", median(prov$desv_sistematica_pct, na.rm = TRUE)),
             pct_verde = round(100 * mean(prov$semaforo == "VERDE"), 0)),
  data.table(nivel = "Capitales (ubicacion)", unidades = nrow(cap),
             cv_mediano = median(cap$cv_pct), cv_p90 = quantile(cap$cv_pct, .9),
             ancla_oficial = "no (E1 no contrastable)",
             desv_sistematica = "heredada de provincia (cota)",
             pct_verde = round(100 * mean(cap$semaforo == "VERDE"), 0))))
resumen[, `:=`(cv_mediano = round(cv_mediano, 1), cv_p90 = round(cv_p90, 1))]

message("\n[RESUMEN] Escalera de errores (CV muestral mediano por nivel):")
for (i in seq_len(nrow(resumen)))
  message(sprintf("   %-22s CV mediano %5.1f%% | p90 %5.1f%% | %s | verde: %s%%",
                  resumen$nivel[i], resumen$cv_mediano[i], resumen$cv_p90[i],
                  resumen$desv_sistematica[i], resumen$pct_verde[i]))
message(sprintf(paste0("   Cobertura de localizacion (E2): %.1f%% provincial / %.1f%% ",
        "municipal; cota mediana por reasignacion: %.1f%% de la celda"),
        100 * cob_prov, 100 * cob_muni, median(cota_loc$cota_pct)))

# --- 5. volcado --------------------------------------------------------------
hojas_err <- list(
  Resumen_niveles = resumen,
  Nacional = nac[, .(estimado = round(est), aeat, ratio, desv_sistematica_pct,
                     ic95_inf = round(ic_inf), ic95_sup = round(ic_sup), cv_pct,
                     dentro_ic)],
  CCAA = ccaa[, .(ccaa = unidad, estimado = round(est), aeat, ratio,
                  desv_sistematica_pct, ic95_inf = round(ic_inf),
                  ic95_sup = round(ic_sup), cv_pct, n_muestra = N, veredicto, semaforo)],
  Provincias = prov[, .(cpro = unidad, provincia, estimado = round(est), aeat,
                        ratio, desv_sistematica_pct, ic95_inf = round(ic_inf),
                        ic95_sup = round(ic_sup), cv_pct, n_muestra = N,
                        veredicto, semaforo)],
  Capitales = cap[, .(muni = unidad, municipio, estimado = round(est),
                      ic95_inf = round(ic_inf), ic95_sup = round(ic_sup),
                      cv_pct, n_muestra = N, semaforo, nota)],
  Localizacion_E2 = sesgo_loc,
  Cota_localizacion_prov = cota_loc[, .(cpro, viv_localizadas = round(v_obs),
                                        viv_no_localizadas_por_residencia = round(v_extra),
                                        escenario_reasignado = round(v_reasignado), cota_pct)],
  Notas = data.frame(nota = c(
    sprintf("B_BOOT = %d replicas; remuestreo de declarantes completos (IDENPER).", B_BOOT),
    "E1 (clasificacion): desviacion frente al ancla AEAT; a nivel nacional es casi pura (muestreo ~0, sin localizacion).",
    "E2 (localizacion): viviendas sin cruce con el modulo; la cota provincial supone reasignarlas segun residencia del declarante.",
    "E3 (muestreo): CV e IC95 bootstrap; unico error medible a nivel municipal (sin ancla oficial).",
    sprintf("Semaforo: VERDE CV<=5%% y sesgo<=10%% (o dentro del IC); AMBAR CV<=15%% (publicar con IC); ROJO n<%d, CV>15%% o sesgo>15%%.", MIN_N),
    "El error total de una celda con ancla ~ desviacion sistematica +/- IC muestral; sin ancla (municipal), reportar IC y heredar E1 provincial como cota."))
)
ruta_err <- file.path(carpeta_anio(ANIO_REF), "errores_territoriales.xlsx")
ok <- FALSE
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  for (h in names(hojas_err)) { openxlsx::addWorksheet(wb, h)
    openxlsx::writeData(wb, h, as.data.frame(hojas_err[[h]])) }
  openxlsx::saveWorkbook(wb, ruta_err, overwrite = TRUE); ok <- TRUE
}
if (!ok) for (h in names(hojas_err))
  write.csv2(as.data.frame(hojas_err[[h]]),
             sub("\\.xlsx$", paste0("_", h, ".csv"), ruta_err), row.names = FALSE)
message("\nInforme de errores guardado en: ",
        if (ok) ruta_err else sub("\\.xlsx$", "_*.csv", ruta_err))
