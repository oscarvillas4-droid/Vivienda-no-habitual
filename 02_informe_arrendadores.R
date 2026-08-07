# ==============================================================================
# ETAPA 2 - INFORME DE ARRENDADORES DE VIVIENDA EN EL IRPF
#           Uniarrendador / multiarrendador, habitual vs no habitual,
#           y EVOLUCION 2016 -> 2023
#
# VERSION 4, calibrada contra la Estadistica de viviendas declaradas en IRPF
# de la AEAT (Bloque I, "Rentabilidad y precios de alquiler por CCAA y
# provincia") y estructurada para responder al documento
# "diseño_investigacion_alquiler_no habitual.docx" (modulos M1 a M5; ver la
# hoja Diseno_investigacion, que mapea cada modulo con sus hojas y limites).
# Referencia AEAT 2023, Vivienda habitual = No:
#   309.479 viviendas | 1.361 EUR/mes | 16,7 EUR/m2 | 106 m2 | 271 dias | 6,2%
#
# QUE HACE ESTA VERSION (buscar la marca "[AEAT]"):
#  1. UNIDAD = VIVIENDA ENTERA (share x peso), como el Bloque I de la AEAT.
#     Nada de ponderar el conteo por dias (eso es el Bloque II).
#  2. FILTRO DE "VIVIENDA RESIDENCIAL" SIN VALOR CATASTRAL OBLIGATORIO.
#     El fichero 8 mezcla viviendas con locales, garajes, trasteros, naves...
#     y la AEAT los separa cruzando con Catastro, cosa que el panel no permite.
#     Se construye un SCORE DE VIVIENDA por inmueble combinando senales
#     observables, con degradacion elegante si el extractor no trae alguna:
#       - alquiler mensual elevado frente a los percentiles del alquiler
#         HABITUAL de su misma provincia (el habitual es un colectivo casi
#         puro de viviendas: sirve de patron de "precio de vivienda");
#       - suelo de renta derivado del EUR/m2 provincial que publica la AEAT
#         (equivale a exigir que rente al menos como una vivienda pequena);
#       - valor catastral en banda de vivienda, usando el MEJOR VC disponible
#         (casilla 83 o, novedad, casilla 123 del bloque de amortizacion, con
#         mucha mas cobertura en arrendados);
#       - amortizacion del inmueble (casilla 131 => valor implicito >= minimo);
#       - IBI y tributos (casilla 115 => IBI minimo de vivienda).
#     Varios candidatos de filtro (percentiles, suelo m2, cortes del score) se
#     CALIBRAN contra los anclajes nacionales AEAT y se elige el mejor
#     (hoja Calibracion_AEAT + hoja Score_distribucion para "ver que se
#     excluye por abajo").
#  3. CONTRASTE PROVINCIAL frente a la tabla AEAT (48 provincias embebidas),
#     con la cautela de que la AEAT localiza el INMUEBLE y el panel al
#     DECLARANTE.
#  4. SUBSEGMENTACION EXPLORATORIA del no habitual en "intensivo
#     (no habitual probable)" vs "estacional" vs "anual (habitaciones,
#     empresa, larga duracion sin reduccion)", por precio-dia relativo al
#     habitual de su provincia y por dias. HEURISTICA propia, no AEAT.
#  5. Se mantienen: alquiler_mes con la definicion AEAT, reclasificacion de
#     mixtos a habitual, exclusion por situacion (casilla 65) si esta la
#     columna, evolucion 2016-2023 sobre universo completo, precision
#     muestral, y toda la estructura de hojas anterior.
#
# NOVEDAD DE DATOS: el script 01c_enriquecer_panel_2023.py anade al panel las
# columnas SITUACION, COMUNIDAD_109, SUMINISTROS_113, SEGUROS_114, IBI_115,
# VC_AMORT_123, VC_CONSTR_124, IMPORTE_ADQ_126, AMORT_INM_131,
# FECHA_CONTRATO_93 y FECHA_ADQ_120 leyendolas del fichero 8 original.
# Este informe funciona sin ellas, pero el score gana mucho con ellas.
#
# Entrada : panel_arrendamientos_<anio>.csv[.gz]  (generado por 01/01b [+01c])
# Salida  : informe_arrendadores_vivienda.xlsx  +  graficos PNG
#
# EJECUCION: siempre con source(), para que se detenga en el primer error.
#   source("02_informe_arrendadores.R", encoding = "UTF-8")
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

# --- 1. PARAMETROS -------------------------------------------------------------
RUTA_RAIZ <- "C:\\Users\\ovillasf\\OneDrive - Dirección General de Ordenación del Juego\\Documentos\\P HOGARES 2023"
ANIO_REF  <- 2023        # ejercicio principal del informe
ANIO_COMP <- 2016        # ejercicio de comparacion (NA para desactivarlo)
carpeta_anio <- function(anio) file.path(RUTA_RAIZ, as.character(anio))

RUTA_SALIDA <- file.path(carpeta_anio(ANIO_REF),
                         "informe_arrendadores_vivienda.xlsx")

NIVEL_ARRENDADOR <- "persona"   # "persona" (IDENPER) u "hogar" (IDENHOG)
UMBRAL_MULTI     <- 2           # numero de viviendas desde el que es multiarrendador
APLICAR_MARCA_75 <- TRUE        # exigir la marca de arrendamiento (con guarda)
MIN_MUESTRA      <- 30          # numero minimo de registros para no marcar la celda

# Un inmueble con varios contratos suma dias al consolidarse y puede superar el
# anio natural. Se topa para no inflar los dias medios ni hundir el euro/dia.
DIAS_TOPE    <- 365
# NOTA: los dias arrendados (casilla 101) miden DURACION, no modalidad. La
# subsegmentacion no habitual de mas abajo es una HEURISTICA propia.
VCAT_MIN     <- 3000            # VC minimo para calcular rentabilidades
UMBRAL_ING_BAJO <- 1200         # proxy de garaje/trastero suelto (se cuantifica)

# [AEAT] Anclajes nacionales 2023, Bloque I (por ubicacion del inmueble).
AEAT_REF <- list(
  no_habitual = list(viviendas = 309479,  alquiler_mes = 1361, alq_m2 = 16.7,
                     m2 = 106, dias = 271, valor_referencia = 157071,
                     rentab_vr = 6.2),
  habitual    = list(viviendas = 2409689, alquiler_mes = 657,  alq_m2 = 8.0,
                     m2 = 97,  dias = 339, valor_referencia = 142047,
                     rentab_vr = 5.1))

# [AEAT] Tabla provincial 2023, Vivienda habitual = No (misma fuente, misma
# pagina). Se usa para: (a) el suelo de renta por m2 de cada provincia y
# (b) el contraste provincial Panel vs AEAT. OJO: la AEAT localiza el
# INMUEBLE; el panel, la RESIDENCIA del declarante.
AEAT_PROV <- data.table::data.table(
  cpro = c("04","11","14","18","21","23","29","41",      # Andalucia
           "22","44","50",                                # Aragon
           "33","07","35","38","39",
           "05","09","24","34","37","40","42","47","49",  # C. y Leon
           "02","13","16","19","45",                      # C.-La Mancha
           "08","17","25","43",                           # Cataluna
           "03","12","46",                                # C. Valenciana
           "06","10",                                     # Extremadura
           "15","27","32","36",                           # Galicia
           "28","30","26","51","52"),
  provincia_aeat = c("Almeria","Cadiz","Cordoba","Granada","Huelva","Jaen",
                     "Malaga","Sevilla","Huesca","Teruel","Zaragoza","Asturias",
                     "Illes Balears","Las Palmas","S.C. Tenerife","Cantabria",
                     "Avila","Burgos","Leon","Palencia","Salamanca","Segovia",
                     "Soria","Valladolid","Zamora","Albacete","Ciudad Real",
                     "Cuenca","Guadalajara","Toledo","Barcelona","Girona",
                     "Lleida","Tarragona","Alicante","Castellon","Valencia",
                     "Badajoz","Caceres","A Coruna","Lugo","Ourense",
                     "Pontevedra","Madrid","Murcia","La Rioja","Ceuta","Melilla"),
  viviendas_aeat = c(6974,12946,5784,12070,4625,4440,22581,11895,
                     2950,1349,3404,6952,14078,12699,11047,4497,
                     1622,1335,2368,548,4192,1042,631,1901,903,
                     1943,2713,1460,849,3060,19433,12251,2948,9660,
                     19859,8241,17920,3319,2308,11129,3139,1594,8099,
                     17731,7111,1480,163,233),
  alq_mes_aeat = c(1088,1623,865,998,1056,666,1733,1096,
                   1588,989,841,1473,2663,1511,1338,1645,
                   1047,883,830,707,743,1122,883,762,742,
                   640,605,738,862,793,1498,2029,1390,1622,
                   1385,1113,1294,556,575,948,1097,744,1415,
                   1561,856,1144,1020,921),
  alq_m2_aeat = c(14.4,20.6,10.2,11.6,12.8,6.7,23.7,14.3,
                  21.0,10.3,9.6,18.2,24.7,21.2,18.4,20.2,
                  10.5,10.2,9.0,6.2,7.7,10.1,8.9,7.8,7.6,
                  6.1,5.7,7.7,8.4,8.4,18.8,25.2,15.5,21.8,
                  17.5,15.7,16.0,5.2,5.3,10.8,12.1,8.2,16.1,
                  20.2,9.7,13.7,10.9,9.8),
  dias_aeat = c(250,231,290,282,252,287,254,300,
                230,274,316,243,253,274,273,237,
                275,301,292,303,305,290,280,319,305,
                308,308,292,305,314,319,222,271,229,
                253,235,276,313,305,264,244,289,234,
                319,284,291,308,303))

# [AEAT] Regla de prioridad de usos: la vivienda con ALGUN uso de vivienda
# habitual del propietario en el anio se clasifica como habitual.
RECLASIFICAR_MIXTO_A_HAB <- TRUE

# [AEAT] Excluir situacion (casilla 65) = 2 (Pais Vasco/Navarra), 3 (sin
# referencia catastral) o 4 (extranjero) si el panel trae la columna.
APLICAR_SITUACION_1 <- TRUE

# --- Parametros del SCORE DE VIVIENDA y del filtro residencial -----------------
# El score de cada inmueble es la media ponderada de senales en [0,1]; cada
# senal vale NA si su dato no esta y entonces no participa (ni a favor ni en
# contra). Senales y pesos:
PESOS_SCORE <- c(renta = 2,    # alq. mensual vs percentil 10 del habitual de su provincia
                 m2    = 1,    # alq. mensual vs suelo de vivienda pequena (EUR/m2 AEAT prov.)
                 vc    = 2,    # mejor VC disponible (c.83 o c.123) en banda de vivienda
                 amort = 1,    # amortizacion c.131 => valor construccion implicito minimo
                 ibi   = 0.5,  # tributos c.115 => IBI minimo de vivienda
                 tipo  = 2,    # NO garaje/trastero (ingreso infimo) NI local (VC atipico)
                 ratio = 1.5,  # ratio ingreso/VC en la banda de una vivienda de su provincia
                 gasto = 0.5)  # declara comunidad o suministros (una vivienda alquilada suele)
M2_MIN_VIVIENDA  <- 20         # la "vivienda minima" del suelo por m2 (25 m2)
ALQ_M2_NACIONAL  <- 16.7       # EUR/m2 nacional AEAT (fallback sin provincia)
BANDA_VC_P       <- c(0.02, 0.98) # banda de VC "de vivienda" (percentiles del habitual)
VAL_CONSTR_MIN   <- 20000      # valor de construccion implicito minimo (amort/0.03)
IBI_MIN_VIVIENDA <- 150        # IBI anual minimo verosimil de una vivienda
MIN_OBS_PROV     <- 50         # minimo de habituales por provincia; si no, patron nacional
# Umbrales de las senales de tipologia y ratio (para apartar garaje/trastero/local):
ING_GARAJE_MAX   <- 2400       # alquiler ANUAL (100% titularidad) tipico techo de garaje/trastero
VC_LOCAL_FACTOR  <- 3          # VC > este factor x el VC alto del habitual provincial => local/nave
RATIO_ING_VC_P   <- c(0.05, 0.95) # banda del ratio ingreso/VC del habitual (fuera = atipico)
# Filtro aplicado al informe: "auto" (menor desviacion conjunta frente a los
# anclajes nacionales AEAT del no habitual), "ninguno", o el numero 0..8 del
# candidato de la hoja Calibracion_AEAT.
FILTRO_RESIDENCIAL <- "auto"
SCORE_CORTES <- c(0.45, 0.55, 0.65)   # cortes candidatos del score (mas exigentes)

# --- Parametros de la subsegmentacion del no habitual (HEURISTICA) --------------
SUBSEG_FACTOR_INTENSIVO <- 2    # precio-dia >= 2x el del habitual de su provincia
SUBSEG_DIAS_ESTACIONAL  <- 270  # <= 270 dias arrendado = estacional
SUBSEG_DIAS_INTENSIVO   <- 366  # intensivo no habitual: ademas de precio-dia alto,
# rotacion (<= 210 dias, ~7 meses). Por encima es alquiler residencial largo caro,
# no turismo: pasa a estacional/anual segun sus dias, a precio moderado.
SUBSEG_NORESID_MAXREL   <- 0.35 # arrendamiento anual con precio-dia < 0,35x el del
# habitual provincial = proxy de garaje/local/trastero (renta infima por dia). Es
# el criterio que aparta lo NO residencial del no habitual, por precio, no por dias.

# --- Criterio AEAT de "vivienda habitual" por USO (no solo por reduccion) -------
# La AEAT clasifica como 'Vivienda habitual = Si' (2.409.689) el alquiler que
# el INQUILINO usa como su vivienda habitual, se aplique o no la reduccion del
# art. 23.2. Tu marca de reduccion (casilla 150) capta una parte; el resto es
# alquiler residencial de LARGA DURACION sin reduccion, que la AEAT tambien
# cuenta como habitual y que si no se reclasifica infla el no habitual.
# Con AEAT_HABITUAL_POR_USO = TRUE, el eje de CONTRASTE con la AEAT considera
# habitual: (reduccion 23.2) O (larga duracion a precio de vivienda, no
# no habitual). El eje de TRABAJO por reduccion se conserva aparte.
AEAT_HABITUAL_POR_USO <- TRUE
DIAS_TOPE_INTENSIVO   <- 366  # DESACTIVADO a peticion (366 = sin efecto): el corte del A-largo restringia de mas  # un intensivo (precio-dia >= 2x) alquilado MAS de este numero de dias no es temporada: es vivienda prime anual del inquilino (corporativo/expatriado) que la AEAT recoloca al Si; sale del No. Subir a 345 si recorta de mas; el turismo real de costa no se toca (sus A son cortos).
USAR_INQ_CONFIRMADO <- TRUE  # bisturi sin falsos positivos: inmueble del No con INQUILINO DOMICILIADO CONFIRMADO (modulo VIVHAB) sale a fuera de tabla (es vivienda habitual de alguien, verificado). Maxima densidad de confirmacion en Madrid/Barcelona.
SALIDA_MODULAR <- TRUE  # TRUE = Excel con SOLO las hojas del pliego (M1..M5) + Validacion_AEAT + Notas + Metadatos, para replicadores externos; FALSE = salida tecnica completa
NH_CRITERIO <- "temporada"  # REPLICA AEAT oficial (No habitual = 309k, ratios 1,00/1,02/0,86). El UNIVERSO AMPLIO del criterio directo (todo el alquiler residencial sin reduccion, ~522k) se publica SIEMPRE en la hoja Embudo_directo, con su cascada de candados.
NH_EXIGE_VC <- TRUE   # candado del universo "con valor catastral": VC informado (c.83/123) o score >= 0.5
USAR_ANTIG            <- FALSE  # aplicar la antiguedad de contrato como candado (c.93); FALSE para desactivarlo
ANTIG_HAB_FACTO       <- 3     # contrato sin reduccion con >= N anios = vivienda habitual de facto del inquilino (c.93) -> fuera de la tabla
EURM2_BANDA           <- c(0.3, 3.5)  # banda de plausibilidad del EUR/m2 real (ing/12/VIV_METROS_RC) frente al EUR/m2 AEAT provincial
DIAS_HAB_FACTO        <- 340   # dias a partir de los cuales el alquiler residencial a precio normal se considera vivienda habitual de facto del inquilino (criterio catastral_real): sube el alquiler medio del No y baja algo dias/recuento; bajarlo aparta mas
DIAS_LARGA_DURACION   <- 300    # C hasta este tope entra al No habitual (subirlo dispara el recuento: la franja 300-330 son ~520k estancias de 10-11 meses)
# (estudiantes/desplazados 271-300 dias); C por encima queda fuera de la tabla
# (alquiler anual barato sin reduccion = proxy garaje/local/trastero).
# y ademas que NO sea no habitual intensivo (precio-dia normal de vivienda)

# EJE_PRINCIPAL decide como se clasifica hab/tur en TODO el informe (Usos,
# Modalidades, evolucion, contraste...):
#   "uso"       (recomendado, cuadra con la AEAT): habitual = vivienda habitual
#               del inquilino, con o SIN reduccion (reduccion 23.2 O larga
#               duracion residencial no no habitual). El NO habitual es UN solo
#               grupo = no habitual, sin subdividir.
#   "reduccion" (eje de trabajo clasico): habitual = solo reduccion 23.2; el no
#               habitual se subsegmenta en no habitual / estacional / otro.
EJE_PRINCIPAL <- "uso"

# --- Parametros del diseno de investigacion (modulos M1-M5) ---------------------
# MODALIDAD del alquiler (proxy, solo ejercicio de referencia):
#   habitual / no habitual probable (subsegmentos A y B) / otro no
#   habitual (C y D: habitaciones, empresa, larga duracion sin reduccion).
SUBSEG_TT <- c("A", "B")        # subsegmentos que forman el "no habitual"
# Cartera de viviendas del arrendador (modulo 3 del diseno): 1 / 2 / 3 / 4 /
# 5-9 / 10 o mas, contando SOLO las viviendas de la modalidad analizada.
BINS_CARTERA <- c(0, 1, 2, 3, 4, 9, Inf)
ETIQ_CARTERA <- c("1", "2", "3", "4", "5-9", "10 o mas")
# Tramos de renta bruta total del declarante (modulo 4). Requiere haber
# enriquecido el panel con 01c (fichero 2_Renta del propio panel).
TRAMOS_RENTA <- c(-Inf, 12000, 21000, 30000, 60000, 150000, Inf)
ETIQ_RENTA   <- c("<=12.000", "12-21.000", "21-30.000", "30-60.000",
                  "60-150.000", ">150.000")
MIN_MUESTRA_MUNI <- 30          # minimo muestral para publicar un municipio

# Deflactor a euros del anio de referencia. IPC general, medias anuales (INE).
DEFLACTOR <- c("2016" = 1.205, "2023" = 1.000)


# --- 2. LECTURA DE LAS BASES COMPACTAS -------------------------------------------
# fread() solo abre .gz si esta R.utils; si no, se descomprime con gzfile().
leer_base <- function(ruta, forzar_descompresion = FALSE) {
  cols <- list(character = c("CPRO", "CMUN"))

  ruta_plana <- sub("\\.gz$", "", ruta)
  if (!grepl("\\.gz$", ruta)) {
    return(fread(ruta, sep = ";", dec = ".", showProgress = FALSE,
                 encoding = "UTF-8", colClasses = cols))
  }
  if (file.exists(ruta_plana)) {
    message("  usando la versión sin comprimir: ", basename(ruta_plana))
    return(fread(ruta_plana, sep = ";", dec = ".", showProgress = FALSE,
                 encoding = "UTF-8", colClasses = cols))
  }
  if (!forzar_descompresion && requireNamespace("R.utils", quietly = TRUE)) {
    return(fread(ruta, sep = ";", dec = ".", showProgress = FALSE,
                 encoding = "UTF-8", colClasses = cols))
  }

  message("  descomprimiendo (sin R.utils)...")
  tmp <- tempfile(fileext = ".csv")
  ent <- gzfile(ruta, "rb")
  sal <- file(tmp, "wb")
  on.exit({
    try(close(ent), silent = TRUE)
    try(close(sal), silent = TRUE)
    unlink(tmp)
  }, add = TRUE)
  repeat {
    trozo <- readBin(ent, what = "raw", n = 16777216L)   # 16 MB por vuelta
    if (length(trozo) == 0L) break
    writeBin(trozo, sal)
  }
  close(ent); close(sal)
  fread(tmp, sep = ";", dec = ".", showProgress = FALSE,
        encoding = "UTF-8", colClasses = cols)
}

ruta_base_anio <- function(anio) {
  plano <- file.path(carpeta_anio(anio),
                     sprintf("panel_arrendamientos_%d.csv", anio))
  if (file.exists(plano)) plano else paste0(plano, ".gz")
}

# Lector minimo del _meta.json que escribe el extractor.
leer_meta <- function(anio) {
  f <- sub("\\.csv(\\.gz)?$", "_meta.json", ruta_base_anio(anio))
  if (!file.exists(f)) return(list())
  txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = " ")
  pares <- regmatches(txt, gregexpr('"[^"]+"\\s*:\\s*("[^"]*"|[-0-9.eE]+|true|false|null)', txt))[[1]]
  if (!length(pares)) return(list())
  claves <- sub('^"([^"]+)".*$', "\\1", pares)
  vals   <- sub('^"[^"]+"\\s*:\\s*', "", pares)
  vals   <- gsub('^"|"$', "", vals)
  setNames(as.list(vals), claves)
}

# --- 3. UTILIDADES ---------------------------------------------------------------
wq <- function(x, w, p) {                      # cuantil ponderado
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) >= p * sum(w))[1]]
}
wmed <- function(x, w) wq(x, w, 0.5)           # mediana ponderada
fmt <- function(x, d = 0) formatC(round(x, d), format = "f", digits = d,
                                  big.mark = ".", decimal.mark = ",")
pct_var <- function(nuevo, viejo) 100 * (nuevo / viejo - 1)
# primera columna existente entre varias candidatas (o NA)
col1 <- function(dt, cands) {
  hit <- intersect(cands, names(dt))
  if (length(hit)) hit[1] else NA_character_
}

# Comprobacion de la tabla provincial embebida (guarda anti-errata).
local({
  s <- sum(AEAT_PROV$viviendas_aeat)
  if (abs(s - AEAT_REF$no_habitual$viviendas) > 50)
    warning(sprintf("La tabla AEAT_PROV suma %d viviendas y el total nacional es %d: revisa la transcripcion.",
                    s, AEAT_REF$no_habitual$viviendas))
})

# --- 4. PREPARACION DE UN EJERCICIO ----------------------------------------------
id_col <- if (NIVEL_ARRENDADOR == "hogar") "IDENHOG" else "IDENPER"

preparar <- function(anio) {
  f <- ruta_base_anio(anio)
  if (!file.exists(f)) return(NULL)
  message(sprintf("Leyendo la base analítica de %d...", anio))
  base <- leer_base(f)
  message(sprintf("  %s inmuebles cargados",
                  formatC(nrow(base), format = "d", big.mark = ".",
                        decimal.mark = ",")))

  # fread lee como 'logical' (NA) las columnas ENTERAMENTE vacias: p.ej.
  # VALOR_CAT_83 (casilla de imputacion, vacia en casi todo el no habitual) o
  # las casillas del score y la renta si aun no se ha pasado 01c. Ese tipo
  # rompe las derivaciones numericas (fifelse, aritmetica), asi que toda
  # columna logica se fuerza a numerico. En este panel no hay columnas
  # booleanas de verdad (las marcas 0/1 se leen como enteros), luego es seguro.
  col_log <- names(base)[vapply(base, is.logical, logical(1))]
  if (length(col_log))
    base[, (col_log) := lapply(.SD, as.numeric), .SDcols = col_log]

  meta <- leer_meta(anio)
  embudo <- list()
  if (!is.null(meta$registros_totales))
    embudo[["0.1 Registros del fichero 8"]] <- as.numeric(meta$registros_totales)
  if (!is.null(meta$con_ingresos))
    embudo[["0.2 Con ingresos de arrendamiento"]] <- as.numeric(meta$con_ingresos)
  if (!is.null(meta$tras_exclusiones))
    embudo[["0.3 Tras excluir AAEE, accesorios y rustica"]] <-
      as.numeric(meta$tras_exclusiones)
  if (!is.null(meta$inmuebles_con_varios_contratos))
    embudo[["0.4 De ellos, con varios contratos (consolidados)"]] <-
      as.numeric(meta$inmuebles_con_varios_contratos)
  embudo[["1. Inmuebles con ingresos, tras exclusiones"]] <- nrow(base)

  tasa75 <- mean(base$ARRENDAM_75 == 1L)
  if (APLICAR_MARCA_75) {
    if (tasa75 >= 0.5) {
      base <- base[ARRENDAM_75 == 1L]
    } else {
      warning(sprintf(paste("Sólo el %.1f%% de los inmuebles de %d tiene la marca",
                            "de la casilla 75: se omite el filtro. Revisa la",
                            "codificación del flag."), 100 * tasa75, anio))
    }
  }
  embudo[["2. Marcados como arrendamiento (c.75)"]] <- nrow(base)

  # [AEAT] Situacion del inmueble (casilla 65 en 2023, 55 en 2016): 1 = con
  # referencia catastral en territorio comun; 2 = Pais Vasco/Navarra; 3 = sin
  # referencia catastral; 4 = extranjero. La AEAT solo cruza la clave 1.
  col_sit <- col1(base, c("SITUACION", "SITUACION_65", "PAR65", "P65",
                          "SITUACION_55", "P55"))
  if (APLICAR_SITUACION_1) {
    if (!is.na(col_sit)) {
      sit <- suppressWarnings(as.integer(base[[col_sit]]))
      base <- base[!(!is.na(sit) & sit %in% c(2L, 3L, 4L))]
      embudo[["3. Con ref. catastral en territorio comun (c.65 = 1)"]] <- nrow(base)
    } else if (anio == ANIO_REF) {
      message("  AVISO [AEAT]: el panel no trae la casilla 65 (SITUACION). ",
              "Ejecuta 01c_enriquecer_panel.py para anadirla y excluir ",
              "los inmuebles sin referencia catastral o en el extranjero.")
    }
  }

  # --- normalizacion de columnas opcionales (extractor ampliado / 01c) ----------
  # Cada senal usa la primera columna disponible entre sus alias; si no hay
  # ninguna, se crea a NA y la senal correspondiente no participa en el score.
  alias <- list(
    IBI_115          = c("IBI_115", "TRIBUTOS_115", "PAR115", "G_TRIBUTOS_64"),
    AMORT_INM_131    = c("AMORT_INM_131", "AMORTIZACION_131", "PAR131",
                         "AMORT_INMUEBLE_67"),
    VC_AMORT_123     = c("VC_AMORT_123", "VALOR_CAT_123", "PAR123"),
    VC_CONSTR_124    = c("VC_CONSTR_124", "PAR124"),
    COMUNIDAD_109    = c("COMUNIDAD_109", "G_COMUN_109", "PAR109"),
    SUMINISTROS_113  = c("SUMINISTROS_113", "G_SUMIN_113", "PAR113"),
    SEGUROS_114      = c("SEGUROS_114", "PAR114"),
    IMPORTE_ADQ_126  = c("IMPORTE_ADQ_126", "PAR126"),
    FECHA_CONTRATO_93 = c("FECHA_CONTRATO_93", "PAR93"),
    FECHA_ADQ_120    = c("FECHA_ADQ_120", "PAR120"),
    # renta del declarante (fichero 2_Renta del panel, via 01c)
    RENTA_TRABAJO_M1  = c("RENTA_TRABAJO_M1", "M1"),
    RENTA_CAPMOB_M2   = c("RENTA_CAPMOB_M2", "M2"),
    RENTA_ALQ_M3      = c("RENTA_ALQ_M3", "M3"),
    RENTA_AAEE_M4     = c("RENTA_AAEE_M4", "M4"),
    RENTA_GANANCIAS_M5 = c("RENTA_GANANCIAS_M5", "M5"),
    RENTA_OTRAS_M6    = c("RENTA_OTRAS_M6", "M6"),
    RENTA_BRUTA_TOTAL = c("RENTA_BRUTA_TOTAL", "RENTA_BRUTA"))
  for (nm in names(alias)) {
    src <- col1(base, alias[[nm]])
    if (is.na(src)) {
      base[, (nm) := NA_real_]
    } else if (src != nm) {
      base[, (nm) := suppressWarnings(as.numeric(get(src)))]
    } else {
      base[, (nm) := suppressWarnings(as.numeric(get(nm)))]
    }
  }
  if (!"VALOR_CAT_83" %in% names(base)) base[, VALOR_CAT_83 := NA_real_]

  # --- derivaciones -------------------------------------------------------------
  base[, dias_ef := pmin(DIAS_101, DIAS_TOPE)]
  n_topados <- base[!is.na(DIAS_101) & DIAS_101 > DIAS_TOPE, .N]
  embudo[[sprintf("4. Inmuebles con dias topados a %d", DIAS_TOPE)]] <- n_topados

  base[, ing_viv  := INGRESOS_102 / share]     # ingreso de la vivienda completa
  base[, rent_viv := REND_NETO_149 / share]

  # [AEAT] Alquiler mensual "elevado" al estilo AEAT: ingresos del 100% de la
  # vivienda, elevados a 365 dias, divididos entre 12.
  base[, alq_mes := fifelse(!is.na(dias_ef) & dias_ef > 0,
                            ing_viv * (365 / pmin(dias_ef, 365)) / 12,
                            NA_real_)]
  # Precio por dia efectivo de la vivienda completa (para la subsegmentacion).
  base[, eur_dia_viv := fifelse(!is.na(dias_ef) & dias_ef > 0,
                                ing_viv / pmin(dias_ef, 365), NA_real_)]

  # Codigo de provincia canonico a dos digitos.
  base[, cpro2 := sprintf("%02d", suppressWarnings(as.integer(CPRO)))]

  # [AEAT] Mejor valor catastral disponible: casilla 83 (bloque de imputacion,
  # suele faltar en arrendados a pleno anio) o casilla 123 (bloque de
  # amortizacion, mucho mas cubierta cuando se deduce amortizacion).
  base[, vc_best := fifelse(!is.na(VALOR_CAT_83) & VALOR_CAT_83 > 0,
                            VALOR_CAT_83,
                            fifelse(!is.na(VC_AMORT_123) & VC_AMORT_123 > 0,
                                    VC_AMORT_123, NA_real_))]

  # Columnas descriptivas de dias por finalidad (2016 no las trae).
  for (col in c("uso_mixto", "es_negocio_aaee", "DIAS_VH_76", "DIAS_DISP_85",
                "DIAS_AAEE_80", "DIAS_NEG_82", "DIAS_EXCON_79")) {
    if (!col %in% names(base)) base[, (col) := NA_real_]
  }

  base[, uso_original := uso]   # copia del uso (por si algun desglose lo usa)

  # ===================== CLASIFICACION (criterio unico) =======================
  # HABITUAL vs NO HABITUAL segun la REDUCCION del art. 23.2 LIRPF (casilla
  # 100/150). Este es el eje de trabajo de todo el informe:
  #   hab = arrendamiento de vivienda habitual del inquilino (con reduccion):
  #         alquiler residencial de larga duracion.
  #   tur = el resto del arrendamiento (temporada, no habitual, habitaciones,
  #         empresa, larga duracion sin reduccion...). Aqui vive el
  #         no habitual, que luego se subsegmenta.
  # 'uso' ya viene asi del extractor (hab si marca casilla 100 o importe en la
  # 150; tur en caso contrario). Se respeta tal cual: es TU criterio.
  #
  # IMPORTANTE para el contraste con la AEAT (se resuelve en la seccion 7.8, no
  # aqui): la Estadistica de viviendas de la AEAT usa OTRA definicion. Su
  # "Vivienda habitual (=Si)" 2.409.689 son viviendas que el DUENO ocupa (no
  # estan en el fichero de arrendamiento). Su "Vivienda arrendada (=No)"
  # 309.479 es TODO lo alquilado, incluida tu "habitual con reduccion". Por eso
  # el contraste con 309.479 se hace contra el TOTAL arrendado (hab + tur), no
  # contra tu segmento 'tur'. No se mezcla con el eje de trabajo.

  # Marca de reduccion, por si alguna tabla la necesita explicita.
  col_red <- col1(base, c("MARCA_RED_100", "REDUCCION_150", "PAR150"))
  base[, tiene_reduccion := if (!is.na(col_red))
        !is.na(suppressWarnings(as.numeric(get(col_red)))) &
        suppressWarnings(as.numeric(get(col_red))) > 0 else uso == "hab"]

  base[, mixto_a_hab := FALSE]   # sin reclasificacion por casilla 76

  base[, segmento := fifelse(uso == "hab",
                             "1. Habitual (arrendamiento con reduccion 23.2)",
                             "2. No habitual (no habitual/otros)")]

  base[, tramo_dias := cut(pmin(dias_ef, 365),
                           breaks = c(0, 90, 180, 270, 365),
                           labels = c("1-90", "91-180", "181-270", "271-365"),
                           include.lowest = TRUE)]
  base[, tramo_dias := as.character(tramo_dias)]
  base[, anio := anio]

  list(base = base, embudo = embudo, meta = meta, anio = anio)
}

# La clasificacion uni/multi se calcula sobre el universo YA FILTRADO para el
# ejercicio de referencia (viviendas, no cualquier inmueble urbano) y sobre el
# universo completo para la evolucion.
finalizar <- function(base) {
  base <- copy(base)
  if (NIVEL_ARRENDADOR == "hogar") {
    base[, n_viv := uniqueN(REFCAT[REFCAT > 0]) + sum(REFCAT <= 0), by = IDENHOG]
  } else {
    base[, n_viv := .N, by = IDENPER]
  }
  base[, clase := fifelse(n_viv >= UMBRAL_MULTI, "Multiarrendador", "Uniarrendador")]
  base[, clase_det := as.character(cut(n_viv, breaks = BINS_CARTERA,
                                       labels = ETIQ_CARTERA))]

  # MODALIDAD (diseno de investigacion): habitual / no habitual
  # probable (subsegmentos A-B) / otro no habitual (C-D). Solo existe donde se
  # ha podido subsegmentar (ejercicio de referencia).
  if ("subsegmento" %in% names(base)) {
    base[, es_tt := uso == "tur" & !is.na(subsegmento) &
                    substr(subsegmento, 1, 1) %in% SUBSEG_TT]
  } else {
    base[, es_tt := NA]
  }
  # En modo "uso" el no habitual ya es un unico grupo (no habitual), sin
  # subdividir. En modo "reduccion" se mantiene el desglose no habitual / otro.
  if (EJE_PRINCIPAL == "uso") {
    base[, modalidad := fifelse(uso == "hab",
                                "Vivienda habitual",
                                "Vivienda no habitual")]
  } else {
    base[, modalidad := fifelse(uso == "hab", "1. Habitual",
                         fifelse(!is.na(es_tt) & es_tt,
                                 "2. No habitual (proxy)",
                         fifelse(uso == "tur", "3. Otro no habitual",
                                 NA_character_)))]
  }

  # carteras por modalidad (modulo 3): viviendas del declarante en cada una
  base[, n_viv_tt  := sum(!is.na(es_tt) & es_tt), by = c(id_col)]
  base[, n_viv_hab := sum(uso == "hab"), by = c(id_col)]

  tiene_renta <- "RENTA_BRUTA_TOTAL" %in% names(base) &&
                 any(!is.na(base$RENTA_BRUTA_TOTAL))
  pers <- base[, .(n_viv = n_viv[1], clase = clase[1], clase_det = clase_det[1],
                   n_viv_tt = n_viv_tt[1], n_viv_hab = n_viv_hab[1],
                   provincia = provincia[1], es_capital = es_capital[1],
                   peso = FACTORCAL[1],
                   tiene_tur = any(uso == "tur"), tiene_hab = any(uso == "hab"),
                   tiene_tt = any(!is.na(es_tt) & es_tt),
                   ing_alq_total = sum(INGRESOS_102),
                   ing_hab = sum(INGRESOS_102[uso == "hab"]),
                   ing_tur = sum(INGRESOS_102[uso == "tur"]),
                   ing_tt  = sum(INGRESOS_102[!is.na(es_tt) & es_tt]),
                   renta_total = if (tiene_renta)
                     suppressWarnings(max(RENTA_BRUTA_TOTAL, na.rm = TRUE))
                     else NA_real_),
               by = c(id_col)]
  pers[!is.finite(renta_total), renta_total := NA_real_]
  pers[, clase_tt := fifelse(n_viv_tt > 0,
          as.character(cut(n_viv_tt, breaks = BINS_CARTERA,
                           labels = ETIQ_CARTERA)), NA_character_)]
  pers[, clase_hab := fifelse(n_viv_hab > 0,
          as.character(cut(n_viv_hab, breaks = BINS_CARTERA,
                           labels = ETIQ_CARTERA)), NA_character_)]
  pers[, tramo_renta := fifelse(is.na(renta_total), NA_character_,
          as.character(cut(renta_total, breaks = TRAMOS_RENTA,
                           labels = ETIQ_RENTA)))]
  pers[, peso_alq_renta := fifelse(!is.na(renta_total) & renta_total > 0,
                                   100 * pmin(ing_alq_total / renta_total, 1.5),
                                   NA_real_)]
  pers[, anio := base$anio[1]]
  list(base = base, pers = pers)
}

# --- 5. MOTOR DE AGREGACION --------------------------------------------------------
# [AEAT] Unidad de conteo: VIVIENDA ENTERA (share x peso). La antigua unidad
# "equivalente-dias" queda como columna informativa (Bloque II AEAT).
resumen <- function(dt, by) {
  dt[, {
    w    <- FACTORCAL
    veq  <- share * w
    veqd <- share * pmin(fifelse(is.na(dias_ef), 0, dias_ef), 365) / 365 * w
    i_d <- !is.na(dias_ef) & dias_ef > 0
    i_m <- i_d & !is.na(alq_mes)
    i_r <- !is.na(REND_NETO_149)
    i_v <- !is.na(vc_best) & vc_best >= VCAT_MIN
    media <- sum(INGRESOS_102 * w) / sum(veq)
    ee <- sqrt(sum((veq * (ing_viv - media))^2)) / sum(veq)
    .(n_muestra        = .N,
      n_efectivo       = round(sum(veq)^2 / sum(veq^2)),
      viviendas        = sum(veq),
      # [AEAT] Recuento de viviendas SIN elevar (suma de share, cada inmueble
      # cuenta por su % de titularidad). La Estadistica de viviendas de la AEAT
      # es un RECUENTO DIRECTO de inmuebles declarados, no una muestra elevada,
      # asi que ESTA es la columna comparable con sus 309.479 / 2.409.689.
      viviendas_sin_elevar = sum(share),
      viviendas_eq_dias = sum(veqd),
      ingresos_medios  = media,
      ee_ingresos      = ee,
      cv_ingresos_pct  = 100 * ee / media,
      ingresos_mediana = wmed(ing_viv, veq),
      ingresos_p25     = wq(ing_viv, veq, 0.25),
      ingresos_p75     = wq(ing_viv, veq, 0.75),
      # [AEAT] Alquiler medio mensual (definicion AEAT).
      alquiler_mes     = if (any(i_m)) sum(alq_mes[i_m] * veq[i_m]) /
                                       sum(veq[i_m]) else NA_real_,
      alquiler_mes_mediana = if (any(i_m)) wmed(alq_mes[i_m], veq[i_m]) else NA_real_,
      pct_ing_bajo     = 100 * sum(veq[ing_viv < UMBRAL_ING_BAJO]) / sum(veq),
      # [AEAT] Dias medios: media aritmetica por vivienda, como la AEAT.
      dias_medios      = if (any(i_d)) sum(veq[i_d] * dias_ef[i_d]) /
                                       sum(veq[i_d]) else NA_real_,
      euros_dia        = if (any(i_d)) sum(INGRESOS_102[i_d] * w[i_d]) /
                                       sum(share[i_d] * w[i_d] *
                                           pmin(dias_ef[i_d], 365)) else NA_real_,
      rend_neto_medio  = if (any(i_r)) sum(REND_NETO_149[i_r] * w[i_r]) /
                                       sum(share[i_r] * w[i_r]) else NA_real_,
      # Rentabilidad sobre el MEJOR VC disponible (c.83 o c.123): indicador
      # PROPIO, no comparable con el 6,2% AEAT (que va sobre valor de referencia).
      rentab_vcat_pct  = if (any(i_v)) 100 * sum(INGRESOS_102[i_v] * w[i_v]) /
                                       sum(share[i_v] * w[i_v] * vc_best[i_v]) else NA_real_,
      rentab_vcat_med  = if (any(i_v)) 100 * wmed(ing_viv[i_v] / vc_best[i_v],
                                                  share[i_v] * w[i_v]) else NA_real_,
      pct_con_vcat     = 100 * sum((share * w)[!is.na(vc_best)]) / sum(share * w))
  }, by = by]
}

con_total <- function(dt, by_geo) {
  rbind(resumen(dt, c(by_geo, "clase", "uso")),
        resumen(dt, c(by_geo, "uso"))[, clase := "Todos los arrendadores"],
        fill = TRUE)
}

a_ancho <- function(res, by_geo) {
  if (is.null(res) || !nrow(res)) return(NULL)
  metricas <- c("n_muestra", "viviendas", "viviendas_sin_elevar",
                "ingresos_medios", "cv_ingresos_pct",
                "ingresos_mediana", "alquiler_mes", "alquiler_mes_mediana",
                "dias_medios", "euros_dia", "rend_neto_medio",
                "rentab_vcat_pct", "rentab_vcat_med", "pct_ing_bajo",
                "pct_con_vcat")
  w <- dcast(res, as.formula(paste(paste(c(by_geo, "clase"), collapse = " + "),
                                   "~ uso")), value.var = metricas)
  for (v in c("hab", "tur")) for (m in metricas) {
    col <- paste0(m, "_", v)
    if (!col %in% names(w)) w[, (col) := NA_real_]
  }
  w[, dif_ingresos_pct := 100 * (ingresos_medios_tur / ingresos_medios_hab - 1)]
  w[, dif_ingresos_eur := ingresos_medios_tur - ingresos_medios_hab]
  cv_max <- pmax(fifelse(is.na(w$cv_ingresos_pct_hab), 0, w$cv_ingresos_pct_hab),
                 fifelse(is.na(w$cv_ingresos_pct_tur), 0, w$cv_ingresos_pct_tur))
  w[, fiabilidad := fifelse(
        pmin(fifelse(is.na(n_muestra_hab), 0L, n_muestra_hab),
             fifelse(is.na(n_muestra_tur), 0L, n_muestra_tur)) < MIN_MUESTRA,
        "muestra reducida",
        fifelse(cv_max > 16.6, "cv > 16,6% (criterio INE: poco fiable)", ""))]
  redondear <- setdiff(names(w)[sapply(w, is.numeric)],
                       grep("^n_muestra", names(w), value = TRUE))
  w[, (redondear) := lapply(.SD, function(x) round(x, 1)), .SDcols = redondear]
  vivc <- grep("^viviendas", names(w), value = TRUE)
  w[, (vivc) := lapply(.SD, round), .SDcols = vivc]
  setorderv(w, c(by_geo, "clase"))
  w[]
}

cuenta_arrendadores <- function(p, by_geo) {
  rbind(p[, .(arrendadores = sum(peso),
              con_no_habitual = sum(peso * tiene_tur),
              con_habitual  = sum(peso * tiene_hab)), by = c(by_geo, "clase")],
        p[, .(arrendadores = sum(peso),
              con_no_habitual = sum(peso * tiene_tur),
              con_habitual  = sum(peso * tiene_hab)),
          by = by_geo][, clase := "Todos los arrendadores"],
        fill = TRUE)[, `:=`(arrendadores = round(arrendadores),
                            con_no_habitual = round(con_no_habitual),
                            con_habitual = round(con_habitual))][]
}

# --- 5b. [AEAT] SCORE DE VIVIENDA Y CALIBRACION DEL FILTRO RESIDENCIAL -------------
# La AEAT identifica las viviendas cruzando con Catastro; el panel no puede
# (referencia catastral anonimizada). Se construye un score de "parece
# vivienda" por inmueble y varios filtros candidatos, y se elige el que mejor
# reproduce los anclajes nacionales de la AEAT en el no habitual.
#
# El PATRON de "precio de vivienda residencial" ya NO puede ser el segmento
# "habitual" (que con la clasificacion AEAT correcta son solo unos pocos mixtos
# de uso propio). El mejor espejo de "alquiler de vivienda de verdad" dentro del
# fichero de arrendamiento es el subconjunto que aplica la REDUCCION del art.
# 23.2 LIRPF (casillas 100/150): esa reduccion se concede justo por alquilar a
# vivienda habitual del INQUILINO, luego es alquiler residencial genuino. Se usa
# como patron de percentiles de alquiler y de valor catastral por provincia,
# con fallback nacional. Si el panel no trae la marca, cae al 'habitual'.
anotar_score <- function(dt) {
  col_red <- col1(dt, c("MARCA_RED_100", "REDUCCION_150", "PAR150"))
  if (!is.na(col_red) &&
      sum(dt$uso == "tur" &
          suppressWarnings(as.numeric(dt[[col_red]])) > 0, na.rm = TRUE) >= 100) {
    marca_red <- suppressWarnings(as.numeric(dt[[col_red]]))
    hab <- dt[!is.na(marca_red) & marca_red > 0]   # alquiler residencial (23.2)
    if (nrow(hab) < 100) hab <- dt[uso == "hab"]
  } else {
    hab <- dt[uso == "hab"]
  }
  # patrones provinciales del alquiler residencial (alquiler y VC), fallback nac.
  pat <- hab[!is.na(alq_mes) & alq_mes > 0,
             .(n = .N,
               q05 = wq(alq_mes, share * FACTORCAL, 0.05),
               q10 = wq(alq_mes, share * FACTORCAL, 0.10)), by = cpro_ef]
  q05_nac <- wq(hab$alq_mes, hab$share * hab$FACTORCAL, 0.05)
  q10_nac <- wq(hab$alq_mes, hab$share * hab$FACTORCAL, 0.10)
  pat <- pat[n >= MIN_OBS_PROV]
  i <- match(dt$cpro_ef, pat$cpro_ef)
  dt[, q05_prov := fifelse(is.na(i), q05_nac, pat$q05[i])]
  dt[, q10_prov := fifelse(is.na(i), q10_nac, pat$q10[i])]

  patv <- hab[!is.na(vc_best) & vc_best >= VCAT_MIN,
              .(n = .N,
                v_lo = wq(vc_best, share * FACTORCAL, BANDA_VC_P[1]),
                v_hi = wq(vc_best, share * FACTORCAL, BANDA_VC_P[2])), by = cpro_ef]
  v_lo_nac <- wq(hab[!is.na(vc_best) & vc_best >= VCAT_MIN]$vc_best,
                 hab[!is.na(vc_best) & vc_best >= VCAT_MIN, share * FACTORCAL],
                 BANDA_VC_P[1])
  v_hi_nac <- wq(hab[!is.na(vc_best) & vc_best >= VCAT_MIN]$vc_best,
                 hab[!is.na(vc_best) & vc_best >= VCAT_MIN, share * FACTORCAL],
                 BANDA_VC_P[2])
  patv <- patv[n >= MIN_OBS_PROV]
  j <- match(dt$cpro_ef, patv$cpro_ef)
  dt[, v_lo_prov := fifelse(is.na(j), v_lo_nac, patv$v_lo[j])]
  dt[, v_hi_prov := fifelse(is.na(j), v_hi_nac, patv$v_hi[j])]

  # suelo de renta "vivienda pequena" desde el EUR/m2 provincial de la AEAT
  k <- match(dt$cpro_ef, AEAT_PROV$cpro)
  dt[, alq_m2_prov := fifelse(is.na(k), ALQ_M2_NACIONAL, AEAT_PROV$alq_m2_aeat[k])]
  dt[, suelo_m2 := M2_MIN_VIVIENDA * alq_m2_prov]

  # banda del ratio ingreso-anual/VC del habitual por provincia (fuera de banda
  # = tipologia atipica: local con VC enorme y renta baja, o cesion bajo mercado)
  hab_r <- hab[!is.na(vc_best) & vc_best >= VCAT_MIN & !is.na(ing_viv) & ing_viv > 0]
  hab_r[, ratio := ing_viv / vc_best]
  patr <- hab_r[, .(n = .N,
                    r_lo = wq(ratio, share * FACTORCAL, RATIO_ING_VC_P[1]),
                    r_hi = wq(ratio, share * FACTORCAL, RATIO_ING_VC_P[2])), by = cpro_ef]
  r_lo_nac <- wq(hab_r$ratio, hab_r$share * hab_r$FACTORCAL, RATIO_ING_VC_P[1])
  r_hi_nac <- wq(hab_r$ratio, hab_r$share * hab_r$FACTORCAL, RATIO_ING_VC_P[2])
  patr <- patr[n >= MIN_OBS_PROV]
  jr <- match(dt$cpro_ef, patr$cpro_ef)
  dt[, r_lo_prov := fifelse(is.na(jr), r_lo_nac, patr$r_lo[jr])]
  dt[, r_hi_prov := fifelse(is.na(jr), r_hi_nac, patr$r_hi[jr])]

  # ---- senales en [0,1] (NA = sin dato: la senal no participa) ----------------
  dt[, s_renta := fifelse(is.na(alq_mes) | is.na(q10_prov), NA_real_,
                          pmin(1, alq_mes / q10_prov))]
  dt[, s_m2 := fifelse(is.na(alq_mes), NA_real_,
                       pmin(1, alq_mes / suelo_m2))]
  dt[, s_vc := fifelse(is.na(vc_best) | vc_best <= 0 | is.na(v_lo_prov), NA_real_,
                fifelse(vc_best < v_lo_prov, pmax(0, vc_best / v_lo_prov),
                fifelse(vc_best > v_hi_prov, pmax(0, pmin(1, v_hi_prov / vc_best)),
                        1)))]
  dt[, s_amort := fifelse(is.na(AMORT_INM_131) | AMORT_INM_131 <= 0, NA_real_,
                          pmin(1, (AMORT_INM_131 / 0.03) / VAL_CONSTR_MIN))]
  dt[, s_ibi := fifelse(is.na(IBI_115) | IBI_115 <= 0, NA_real_,
                        pmin(1, IBI_115 / IBI_MIN_VIVIENDA))]

  # SENAL DE TIPOLOGIA: penaliza garaje/trastero (ingreso anual infimo) y local
  # o nave (VC muy por encima del techo del habitual provincial). Es la senal
  # con mas peso porque es justo lo que la AEAT aparta cruzando con Catastro y
  # el panel no puede ver directamente. Siempre disponible (usa ingreso; el VC
  # solo suma penalizacion si existe).
  dt[, ing_anual_100 := fifelse(!is.na(alq_mes), alq_mes * 12, ing_viv)]
  dt[, s_garaje := pmin(1, pmax(0, ing_anual_100 / ING_GARAJE_MAX))]  # 0 si infimo
  dt[, s_local := fifelse(is.na(vc_best) | is.na(v_hi_prov) | v_hi_prov <= 0, 1,
                          pmin(1, (VC_LOCAL_FACTOR * v_hi_prov) / pmax(vc_best, 1)))]
  dt[, s_tipo := pmin(s_garaje, s_local)]     # basta un indicio para penalizar

  # SENAL DE RATIO ingreso-anual/VC dentro de la banda de vivienda de su provincia
  dt[, ratio_iv := fifelse(!is.na(vc_best) & vc_best > 0 & !is.na(ing_anual_100),
                           ing_anual_100 / vc_best, NA_real_)]
  dt[, s_ratio := fifelse(is.na(ratio_iv) | is.na(r_lo_prov), NA_real_,
                   fifelse(ratio_iv < r_lo_prov, pmax(0, ratio_iv / r_lo_prov),
                   fifelse(ratio_iv > r_hi_prov, pmax(0, pmin(1, r_hi_prov / ratio_iv)),
                           1)))]

  # SENAL DE GASTO: una vivienda alquilada suele declarar comunidad o suministros
  # (un garaje o un trastero, casi nunca). Solo participa si la columna existe.
  hay_gasto_cols <- any(!is.na(dt$COMUNIDAD_109)) || any(!is.na(dt$SUMINISTROS_113))
  if (hay_gasto_cols) {
    dt[, s_gasto := fifelse(
        (!is.na(COMUNIDAD_109) & COMUNIDAD_109 > 0) |
        (!is.na(SUMINISTROS_113) & SUMINISTROS_113 > 0), 1, 0)]
  } else {
    dt[, s_gasto := NA_real_]
  }

  # media ponderada de las senales disponibles
  S <- as.matrix(dt[, .(s_renta, s_m2, s_vc, s_amort, s_ibi, s_tipo, s_ratio, s_gasto)])
  W <- matrix(rep(PESOS_SCORE[c("renta", "m2", "vc", "amort", "ibi",
                                "tipo", "ratio", "gasto")],
                  each = nrow(S)), nrow = nrow(S))
  W[is.na(S)] <- 0
  S[is.na(S)] <- 0
  den <- rowSums(W)
  dt[, score_viv := fifelse(den > 0, rowSums(S * W) / den, NA_real_)]
  dt[, n_senales := rowSums(W > 0)]
  invisible(dt)
}

construir_candidatos <- function(dt) {
  am_ok <- !is.na(dt$alq_mes)
  sc <- dt$score_viv
  filtros <- list()
  filtros[["(0) Sin filtro: todo inmueble urbano arrendado"]] <- rep(TRUE, nrow(dt))
  filtros[["(1) Alq. mensual >= p5 del alquiler habitual de su provincia"]] <-
    am_ok & !is.na(dt$q05_prov) & dt$alq_mes >= dt$q05_prov
  filtros[["(2) Alq. mensual >= p10 del alquiler habitual de su provincia"]] <-
    am_ok & !is.na(dt$q10_prov) & dt$alq_mes >= dt$q10_prov
  filtros[[sprintf("(3) Alq. mensual >= vivienda de %d m2 al EUR/m2 AEAT de su provincia",
                   M2_MIN_VIVIENDA)]] <- am_ok & dt$alq_mes >= dt$suelo_m2
  filtros[[sprintf("(4) Score de vivienda >= %.2f", SCORE_CORTES[1])]] <-
    !is.na(sc) & sc >= SCORE_CORTES[1]
  filtros[[sprintf("(5) Score de vivienda >= %.2f", SCORE_CORTES[2])]] <-
    !is.na(sc) & sc >= SCORE_CORTES[2]
  filtros[[sprintf("(6) Score de vivienda >= %.2f", SCORE_CORTES[3])]] <-
    !is.na(sc) & sc >= SCORE_CORTES[3]
  # candidatos combinados, mas exigentes: exigen a la vez score alto y renta de
  # vivienda de verdad (o descartar el garaje por ingreso infimo).
  filtros[[sprintf("(7) Score >= %.2f Y alq. mensual >= p10 habitual prov.",
                   SCORE_CORTES[1])]] <-
    !is.na(sc) & sc >= SCORE_CORTES[1] & am_ok & !is.na(dt$q10_prov) &
    dt$alq_mes >= dt$q10_prov
  filtros[[sprintf("(8) Score >= %.2f Y no-garaje (ingreso anual >= %d)",
                   SCORE_CORTES[2], ING_GARAJE_MAX)]] <-
    !is.na(sc) & sc >= SCORE_CORTES[2] &
    !is.na(dt$ing_anual_100) & dt$ing_anual_100 >= ING_GARAJE_MAX
  filtros
}

calibrar_filtro_residencial <- function(dt) {
  filtros <- construir_candidatos(dt)
  w_all   <- dt$FACTORCAL
  veq_all <- dt$share * w_all
  am      <- dt$alq_mes
  refs <- list(tur = AEAT_REF$no_habitual, hab = AEAT_REF$habitual)
  eval1 <- function(mask, u) {
    i <- mask & dt$uso == u
    if (!any(i)) return(list(v = 0, d = NA_real_, a = NA_real_, g = NA_real_))
    veq <- veq_all[i]; d <- dt$dias_ef[i]; a <- am[i]
    id <- !is.na(d) & d > 0
    im <- id & !is.na(a)
    list(v = sum(veq),
         d = if (any(id)) sum(veq[id] * d[id]) / sum(veq[id]) else NA_real_,
         a = if (any(im)) sum(veq[im] * a[im]) / sum(veq[im]) else NA_real_,
         g = sum(dt$INGRESOS_102[i] * w_all[i]) / sum(veq))
  }
  filas <- list()
  for (nm in names(filtros)) for (u in c("tur", "hab")) {
    e <- eval1(filtros[[nm]], u); r <- refs[[u]]
    dv <- 100 * (e$v / r$viviendas - 1)
    da <- if (is.na(e$a)) NA_real_ else 100 * (e$a / r$alquiler_mes - 1)
    dd <- if (is.na(e$d)) NA_real_ else 100 * (e$d / r$dias - 1)
    filas[[length(filas) + 1L]] <- data.table(
      Filtro = nm,
      Segmento = if (u == "tur") "No habitual" else "Habitual",
      viviendas = round(e$v),
      AEAT_viviendas = r$viviendas,
      desv_viviendas_pct = round(dv, 1),
      alquiler_mes = round(e$a),
      AEAT_alquiler_mes = r$alquiler_mes,
      desv_alquiler_pct = round(da, 1),
      dias_medios = round(e$d, 1),
      AEAT_dias = r$dias,
      desv_dias_pct = round(dd, 1),
      ingreso_medio_efectivo = round(e$g),
      score = if (u == "tur") round(mean(abs(c(dv, da, dd)), na.rm = TRUE), 1)
              else NA_real_)
  }
  list(tabla = rbindlist(filas), mascaras = filtros)
}

elegir_filtro <- function(cal) {
  nms <- names(cal$mascaras)
  if (identical(FILTRO_RESIDENCIAL, "ninguno")) return(nms[1])
  if (is.numeric(FILTRO_RESIDENCIAL)) {
    hit <- grep(sprintf("^\\(%d\\)", as.integer(FILTRO_RESIDENCIAL)), nms)
    if (length(hit)) return(nms[hit[1]])
    warning("FILTRO_RESIDENCIAL no reconocido: se usa 'auto'.")
  }
  t <- cal$tabla[Segmento == "No habitual" & !grepl("^\\(0\\)", Filtro) &
                 !is.na(score)]
  if (!nrow(t)) return(nms[1])
  t$Filtro[which.min(t$score)]
}

# Distribucion del score en el no habitual: que se excluye "por abajo" en cada
# tramo, para valorar los cortes con datos (peticion expresa del analisis).
distribucion_score <- function(dt) {
  nh <- dt[uso == "tur" & !is.na(score_viv)]
  if (!nrow(nh)) return(NULL)
  veq <- nh$share * nh$FACTORCAL
  cortes <- unique(c(0, sapply(seq(0.1, 0.9, 0.1),
                               function(p) wq(nh$score_viv, veq, p)), 1))
  cortes <- sort(unique(round(cortes, 4)))
  if (length(cortes) < 3) cortes <- c(0, 0.5, 1)
  nh[, tramo_score := cut(score_viv, breaks = cortes, include.lowest = TRUE)]
  res <- nh[, {
    v <- share * FACTORCAL
    i_m <- !is.na(alq_mes)
    i_d <- !is.na(dias_ef) & dias_ef > 0
    .(inmuebles      = round(sum(v)),
      pct_inmuebles  = NA_real_,
      alq_mes_medio  = if (any(i_m)) round(sum(ing_viv[i_m] * v[i_m]) / sum(v[i_m]) / 12) else NA_real_,
      ingreso_medio  = round(sum(ing_viv * v) / sum(v)),
      dias_medios    = if (any(i_d)) round(sum(dias_ef[i_d] * v[i_d]) / sum(v[i_d])) else NA_real_,
      pct_con_vc     = round(100 * sum(v[!is.na(vc_best)]) / sum(v), 1),
      pct_con_amort  = round(100 * sum(v[!is.na(AMORT_INM_131) & AMORT_INM_131 > 0]) / sum(v), 1),
      s_tipo_media   = round(mean(s_tipo, na.rm = TRUE), 2),
      s_ratio_media  = round(mean(s_ratio, na.rm = TRUE), 2),
      senales_medias = round(mean(n_senales), 1))
  }, by = tramo_score]
  setorder(res, tramo_score)
  res[, pct_inmuebles := round(100 * inmuebles / sum(inmuebles), 1)]
  res[]
}

# Diagnostico del EMBUDO del no habitual: cuantas viviendas (equivalentes) se
# apartan por cada "sospecha" de no ser vivienda residencial, medido sobre el
# no habitual bruto (tras c.65 y quitar mixtos). Ayuda a ver de donde viene el
# exceso frente a la AEAT y a decidir el corte del filtro con datos propios.
# Diagnostico del USO PREDOMINANTE: descompone habitual y no habitual (eje AEAT)
# por el motivo de clasificacion, para ver que uso manda en cada grupo. Responde
# a "que uso mixto predomina" de forma transparente.
diagnostico_uso_predominante <- function(dt) {
  if (!"uso_aeat" %in% names(dt)) return(NULL)
  u_red <- if ("uso_reduccion" %in% names(dt)) dt$uso_reduccion else dt$uso
  d <- function(col) {
    cn <- col1(dt, col); if (is.na(cn)) return(rep(0, nrow(dt)))
    x <- suppressWarnings(as.numeric(dt[[cn]])); fifelse(is.na(x), 0, x)
  }
  dias_vh <- d(c("DIAS_VH_76", "PAR76"))
  sub1 <- fifelse(is.na(dt$subsegmento), "?", substr(dt$subsegmento, 1, 1))
  motivo <- fifelse(u_red == "hab", "1. Reduccion 23.2 (c.100/150)",
             fifelse(dias_vh > 0, "2. Mixta: uso propio c.76 + arrendada",
             fifelse(sub1 %in% c("A", "B"), "3. Temporada/no habitual (A-B)",
             fifelse(sub1 == "C" & !is.na(dt$dias_ef) & dt$dias_ef <= DIAS_LARGA_DURACION,
                     "4a. C corto (estudiantes/desplazados, al No habitual)",
             fifelse(sub1 == "C", "4b. C largo anual (proxy no vivienda, fuera)",
                     "5. D sin dias / otros")))))
  v <- dt$share * dt$FACTORCAL
  res <- data.table(uso_aeat = dt$uso_aeat, motivo, v, dias_vh,
                    dias_arr = fifelse(is.na(dt$dias_ef), 0, dt$dias_ef))[
    , .(viviendas = round(sum(v)),
        dias_vh_medio = round(sum(dias_vh * v) / sum(v), 1),
        dias_arr_medio = round(sum(dias_arr * v) / sum(v), 1)),
    by = .(uso_aeat, motivo)]
  setorder(res, uso_aeat, motivo)
  res[, pct := round(100 * viviendas / sum(viviendas), 1)]
  res[]
}

diagnostico_embudo_nh <- function(dt) {
  nh <- dt[uso_original == "tur" & !mixto_a_hab]
  if (!nrow(nh)) return(NULL)
  v <- nh$share * nh$FACTORCAL
  tot <- sum(v)
  fila <- function(nom, mask, coment) {
    data.table(Criterio = nom,
               viviendas_afectadas = round(sum(v[mask])),
               pct_del_no_habitual = round(100 * sum(v[mask]) / tot, 1),
               Comentario = coment)
  }
  ing_inf <- !is.na(nh$ing_anual_100) & nh$ing_anual_100 < ING_GARAJE_MAX
  vc_alto <- !is.na(nh$vc_best) & !is.na(nh$v_hi_prov) &
             nh$vc_best > VC_LOCAL_FACTOR * nh$v_hi_prov
  ratio_fuera <- !is.na(nh$s_ratio) & nh$s_ratio < 0.5
  alq_bajo <- !is.na(nh$alq_mes) & !is.na(nh$q05_prov) & nh$alq_mes < nh$q05_prov
  sin_vc <- is.na(nh$vc_best)
  score_bajo <- !is.na(nh$score_viv) & nh$score_viv < SCORE_CORTES[1]
  out <- rbindlist(list(
    fila("0. No habitual bruto (tras c.65, sin mixtos)", rep(TRUE, nrow(nh)),
         "Universo de partida; unidad = vivienda entera"),
    fila(sprintf("A. Ingreso anual < %d EUR (proxy garaje/trastero)", ING_GARAJE_MAX),
         ing_inf, "Renta demasiado baja para una vivienda"),
    fila("B. VC muy por encima del habitual (proxy local/nave)", vc_alto,
         "Valor catastral atipico de vivienda"),
    fila("C. Ratio ingreso/VC fuera de banda de vivienda", ratio_fuera,
         "Relacion renta/valor impropia de vivienda"),
    fila("D. Alquiler mensual < p5 del habitual de su provincia", alq_bajo,
         "Por debajo del suelo de precio de vivienda alli"),
    fila("E. Sin ningun valor catastral (c.83 ni c.123)", sin_vc,
         "El score no puede usar el VC; se apoya en renta y tipologia"),
    fila(sprintf("F. Score de vivienda < %.2f (se excluiria)", SCORE_CORTES[1]),
         score_bajo, "Suma de indicios por debajo del corte mas suave")))
  out[]
}

# --- 5c. SUBSEGMENTACION HEURISTICA DEL NO HABITUAL --------------------------------
# Intenta separar, DENTRO de las viviendas no habituales ya filtradas, el
# arrendamiento intensivo (no habitual probable) del resto
# (habitaciones, empresa, larga duracion sin reduccion). Criterio: precio por
# dia relativo al del alquiler habitual de su provincia, y dias arrendados.
# ES UNA HEURISTICA PROPIA: el IRPF no declara la modalidad del alquiler.
anotar_subsegmento <- function(dt) {
  ref <- dt[uso == "hab" & !is.na(dias_ef) & dias_ef > 0,
            .(eur_dia_hab = sum(INGRESOS_102 * FACTORCAL) /
                            sum(share * FACTORCAL * pmin(dias_ef, 365))),
            by = cpro2]
  hab_ok <- dt[uso == "hab" & !is.na(dias_ef) & dias_ef > 0]
  eur_dia_nac <- sum(hab_ok$INGRESOS_102 * hab_ok$FACTORCAL) /
                 sum(hab_ok$share * hab_ok$FACTORCAL * pmin(hab_ok$dias_ef, 365))
  i <- match(dt$cpro_ef, ref$cpro_ef)
  dt[, eur_dia_hab_prov := fifelse(is.na(i), eur_dia_nac, ref$eur_dia_hab[i])]
  dt[, eur_dia_rel := eur_dia_viv / eur_dia_hab_prov]
  # SUBSEGMENTACION del no habitual. El intensivo TURISTICO real combina dos
  # rasgos: precio-dia alto (>= SUBSEG_FACTOR_INTENSIVO x el habitual provincial)
  # Y ROTACION (dias por debajo de SUBSEG_DIAS_INTENSIVO). Un alquiler LARGO y
  # caro (muchos dias a precio-dia alto) NO es turismo: es vivienda cara en zona
  # tensionada, y la AEAT lo cuenta en su 'no habitual' a precio moderado (o en
  # habitual). Exigir tambien pocos dias evita clasificar como no habitual el
  # alquiler residencial largo de gama alta, que era lo que inflaba la media.
  dt[, subsegmento := fifelse(uso != "tur", NA_character_,
      fifelse(is.na(dias_ef) | dias_ef <= 0, "D. Sin dias informados",
      fifelse(!is.na(eur_dia_rel) & eur_dia_rel >= SUBSEG_FACTOR_INTENSIVO &
              dias_ef <= SUBSEG_DIAS_INTENSIVO,
              "A. Intensivo (no habitual probable)",
      fifelse(dias_ef <= SUBSEG_DIAS_ESTACIONAL,
              "B. Estacional no intensivo",
              "C. Anual no intensivo (habitaciones, empresa, larga duracion)"))))]

  # [AEAT] uso_aeat, VALIDADO CON DATOS REALES: el recuento elevado de viviendas
  # con reduccion 23.2 (c.100/150), sin filtro ni anadidos, reproduce el ancla
  # 'Vivienda habitual = Si' con ratio 1,006 (2.423.187 vs 2.409.689 en 2023).
  # Por tanto:
  #   habitual    = c.100/150 A SECAS (arrendada para vivienda habitual).
  #   no_habitual = arrendada por no habitual (subsegmentos A y B):
  #                 se contrasta con 'Vivienda habitual = No' (309.479).
  #   fuera_tabla = el resto, que la AEAT NO incluye en esa tabla:
  #                 - mixtas con uso propio (c.76>0): la FAQ las clasifica como
  #                   'vivienda habitual con parte arrendada', NO como arrendada;
  #                 - C anual barato sin reduccion y D sin dias: en su mayoria
  #                   no-viviendas segun Catastro (garaje/local/trastero anual)
  #                   o arrendamientos que el cruce catastral aparta.
  if (AEAT_HABITUAL_POR_USO) {
    d <- function(col) {
      cn <- col1(dt, col)
      if (is.na(cn)) rep(0, nrow(dt))
      else { x <- suppressWarnings(as.numeric(dt[[cn]])); fifelse(is.na(x), 0, x) }
    }
    dias_vh <- d(c("DIAS_VH_76", "PAR76"))
    # 'No residencial' = arrendamiento ANUAL (larga permanencia) con renta por
    # dia MUY baja respecto al habitual de su provincia: proxy de garaje, local
    # o trastero alquilado todo el año. Se define por PRECIO-DIA bajo + muchos
    # dias, NO por la frontera no habitual, para que el recuento del no habitual
    # no dependa de como se reparta A/B/C.
    no_resid <- !is.na(dt$eur_dia_rel) & dt$eur_dia_rel < SUBSEG_NORESID_MAXREL &
                !is.na(dt$dias_ef) & dt$dias_ef >= DIAS_LARGA_DURACION
    sin_dias <- is.na(dt$dias_ef) | dt$dias_ef <= 0
    es_AB2 <- !is.na(dt$subsegmento) & substr(dt$subsegmento, 1, 1) %in% c("A", "B")
    # Franja C corta (271..DIAS_LARGA_DURACION dias): estudiantes/desplazados,
    # que la definicion AEAT del 'No' recoge. El C mas largo queda fuera.
    es_Cc2 <- !is.na(dt$subsegmento) & substr(dt$subsegmento, 1, 1) == "C" &
              !is.na(dt$dias_ef) & dt$dias_ef > 0 &
              dt$dias_ef <= DIAS_LARGA_DURACION
    # Candado del universo "con valor catastral" de la tabla AEAT: VC informado
    # (c.83/123) o, en su defecto, score de vivienda >= 0.5 (la AEAT toma el VC
    # de Catastro aunque el declarante no rellene la casilla).
    vc_ok <- !NH_EXIGE_VC | !is.na(dt$vc_best) |
             (!is.na(dt$score_viv) & dt$score_viv >= 0.5)
    # Criterio CATASTRAL: sin reduccion + VC en banda residencial provincial
    # (sin condicion de dias): replica el cruce de la AEAT, que no segmenta por
    # duracion sino por uso catastral. Los anuales residenciales entran; los
    # garajes/locales anuales (VC fuera de banda) no.
    vc_resid <- !is.na(dt$vc_best) & !is.na(dt$v_lo_prov) &
                dt$vc_best >= dt$v_lo_prov & dt$vc_best <= dt$v_hi_prov
    # Criterio INQUILINO (modulo inmobiliario, 01d): la RC del inmueble aparece
    # como VIVIENDA HABITUAL de una persona AJENA a los titulares (VIVHAB).
    # Es la variable con la que la AEAT decide su 'Si'/'No'; con el 01d pasado,
    # NH_CRITERIO="inquilino" la usa: habitual = reduccion O inquilino
    # domiciliado; no habitual = residencial segun Catastro (VIV_RC>=1 o VC en
    # banda) SIN nadie domiciliado y sin uso propio del dueno.
    if (NH_CRITERIO == "inquilino" && !("INQ_DOMICILIADO" %in% names(dt))) {
      warning("NH_CRITERIO='inquilino' pero el panel NO tiene la columna ",
              "INQ_DOMICILIADO. Ejecuta antes 01d_cruzar_inmuebles.py para ",
              "cruzar con el modulo inmobiliario. Sin ella el criterio del ",
              "inquilino no puede aplicarse y el resultado NO es valido.")
      message("  *** AVISO: falta INQ_DOMICILIADO; NH_CRITERIO='inquilino' no ",
              "surtira efecto. Pasa antes el 01d. ***")
    }
    inq <- if ("INQ_DOMICILIADO" %in% names(dt))
      !is.na(dt$INQ_DOMICILIADO) & dt$INQ_DOMICILIADO == 1
      else rep(FALSE, nrow(dt))
    resid_cat <- if ("VIV_RC" %in% names(dt))
      !is.na(dt$VIV_RC) & dt$VIV_RC >= 1 else rep(FALSE, nrow(dt))
    # Criterio CATASTRAL_REAL: usa el USO catastral del modulo inmobiliario
    # (URBACLAVE_RC = 'V' residencial) y el numero de viviendas de la RC
    # (VIV_RC >= 1), ambos con ~95% de cobertura por venir de Catastro y no de
    # la muestra. Es el candado de vivienda con DATO REAL (no proxy): aparta
    # garaje/local/trastero por su clave de uso, no por VC estimado.
    urb <- if ("URBACLAVE_RC" %in% names(dt)) toupper(trimws(dt$URBACLAVE_RC))
           else rep("", nrow(dt))
    es_vivienda_cat <- resid_cat | urb == "V" |
                       (urb == "" & is.na(dt$VIV_RC) & vc_resid)  # sin dato: cae al VC
    # HABITUAL DE FACTO (candado del criterio catastral_real): un piso
    # residencial alquilado practicamente todo el anio a precio-dia NORMAL de
    # vivienda es la vivienda habitual de un inquilino que no cayo en la
    # muestra; la AEAT lo clasifica en el 'Si' (via domicilio del arrendatario)
    # y por eso NO debe contarse en el No habitual. Se aparta a fuera_tabla.
    hab_facto <- !is.na(dt$dias_ef) & dt$dias_ef >= DIAS_HAB_FACTO &
                 (is.na(dt$eur_dia_rel) |
                  dt$eur_dia_rel < SUBSEG_FACTOR_INTENSIVO)
    # Antiguedad del contrato (c.93): sin reduccion y contrato de hace >= N
    # anios = residencia estable del inquilino (habitual de facto) -> fuera.
    antig <- rep(FALSE, nrow(dt))
    col_fc <- col1(dt, c("FECHA_CONTRATO_93", "PAR93"))
    if (!is.na(col_fc)) {
      fnum <- suppressWarnings(as.numeric(dt[[col_fc]]))
      fy <- floor(fnum / 10000)
      antig <- !is.na(fy) & fy >= 1900 & fy <= ANIO_REF &
               (ANIO_REF - fy) >= ANTIG_HAB_FACTO
    }
    # EUR/m2 real (modulo): ingresos/12 entre los metros de la RC, dentro de
    # una banda de plausibilidad del EUR/m2 AEAT provincial. Fuera de banda =
    # no es un alquiler de vivienda coherente (dato o inmueble impropio).
    eurm2_ok <- rep(TRUE, nrow(dt))
    if ("VIV_METROS_RC" %in% names(dt)) {
      m2x <- suppressWarnings(as.numeric(dt$VIV_METROS_RC))
      em2 <- fifelse(!is.na(m2x) & m2x > 0, dt$ing_viv / 12 / m2x, NA_real_)
      eurm2_ok <- is.na(em2) | is.na(dt$alq_m2_prov) |
                  (em2 >= EURM2_BANDA[1] * dt$alq_m2_prov &
                   em2 <= EURM2_BANDA[2] * dt$alq_m2_prov)
    }
    # Prueba de VIVIENDA (criterio directo), JERARQUICA con dato catastral:
    #  (a) si el modulo dice NO residencial (VIV_RC = 0 viviendas en la RC, o
    #      URBACLAVE distinta de 'V'), FUERA aunque tenga VC declarado: es
    #      garaje, trastero, local o almacen con contrato propio;
    #  (b) si el modulo dice residencial (VIV_RC >= 1), ademas los metros por
    #      vivienda de la RC deben alcanzar M2_MIN_VIVIENDA (aparta trasteros
    #      y cuartos con RC propia);
    #  (c) si el modulo no cruza (~5%), no basta el VC informado: debe estar
    #      DENTRO de la banda residencial provincial (vc_resid).
    vrn <- if ("VIV_RC" %in% names(dt))
      suppressWarnings(as.numeric(dt$VIV_RC)) else rep(NA_real_, nrow(dt))
    m2pv <- rep(NA_real_, nrow(dt))
    if (all(c("VIV_METROS_RC", "VIV_RC") %in% names(dt))) {
      mm <- suppressWarnings(as.numeric(dt$VIV_METROS_RC))
      m2pv <- fifelse(!is.na(vrn) & vrn > 0 & !is.na(mm) & mm > 0,
                      mm / vrn, NA_real_)
    }
    # Jerarquia corregida: el VETO duro es SOLO la clave de uso catastral
    # explicitamente no residencial (URBACLAVE = A almacen-aparcamiento, C
    # comercial, etc.). VIV_RC = 0 NO veta por si solo (en el modulo hay pisos
    # reales con cero viviendas registradas): se trata como "sin senal" y la
    # prueba cae al VC informado. VIV_RC >= 1 sigue siendo prueba positiva,
    # con su suelo de metros por vivienda.
    prueba_viv <- fifelse(urb != "" & urb != "V", FALSE,
                   fifelse(!is.na(vrn) & vrn >= 1,
                           (is.na(m2pv) | m2pv >= M2_MIN_VIVIENDA),
                   fifelse(urb == "V", TRUE, !is.na(dt$vc_best))))
    nh_sel <- if (NH_CRITERIO == "directo")
        !is.na(dt$dias_ef) & dt$dias_ef > 0 & prueba_viv &
        !is.na(dt$ing_anual_100) & dt$ing_anual_100 >= ING_GARAJE_MAX &
        eurm2_ok &
        !(USAR_ANTIG & antig)   # contrato de hace >= ANTIG_HAB_FACTO anios sin
                                # reduccion = habitual de facto (conmutable)
      else if (NH_CRITERIO == "catastral") vc_resid
      else if (NH_CRITERIO == "catastral_real")
        es_vivienda_cat & !inq & !hab_facto
      else if (NH_CRITERIO == "inquilino") (resid_cat | vc_resid) & !inq
      else {
        es_A_largo <- !is.na(dt$subsegmento) &
                      substr(dt$subsegmento, 1, 1) == "A" &
                      !is.na(dt$dias_ef) & dt$dias_ef > DIAS_TOPE_INTENSIVO
        (es_AB2 | es_Cc2) & vc_ok & (!USAR_INQ_CONFIRMADO | !inq) & !es_A_largo
      }
    {  # cascada del UNIVERSO AMPLIO (criterio directo): se calcula SIEMPRE,
       # sea cual sea NH_CRITERIO, y se publica en la hoja Embudo_directo
      w0 <- dt$share * dt$FACTORCAL
      b  <- dt$uso != "hab" & dias_vh == 0
      cd <- !is.na(dt$dias_ef) & dt$dias_ef > 0
      ci <- !is.na(dt$ing_anual_100) & dt$ing_anual_100 >= ING_GARAJE_MAX
      st <- function(m) round(sum(w0[b & m]))
      pasos <- c(
        "0. Sin reduccion y sin uso propio (base)"      = st(rep(TRUE, nrow(dt))),
        "1. ... con dias informados"                    = st(cd),
        "2. ... y prueba de vivienda (catastro/VC)"     = st(cd & prueba_viv),
        "3. ... e ingreso anual >= ING_GARAJE_MAX"      = st(cd & prueba_viv & ci),
        "4. ... y EUR/m2 en banda EURM2_BANDA"          = st(cd & prueba_viv & ci & eurm2_ok),
        "5. ... y sin contrato antiguo (si USAR_ANTIG)" = st(cd & prueba_viv & ci & eurm2_ok &
                                                             !(USAR_ANTIG & antig)))
      assign("embudo_directo",
             data.table(paso = names(pasos), viviendas_elevadas = as.numeric(pasos)),
             envir = .GlobalEnv)
    }
    dt[, uso_aeat := fifelse(uso == "hab" | (NH_CRITERIO == "inquilino" & inq), "habitual",
                      fifelse(dias_vh > 0, "fuera_tabla",
                      fifelse(nh_sel, "no_habitual", "fuera_tabla")))]
  } else {
    dt[, uso_aeat := fifelse(uso == "hab", "habitual", "no_habitual")]
  }
  invisible(dt)
}

tabla_subsegmentos <- function(dt) {
  nh <- dt[uso == "tur" & !is.na(subsegmento)]
  if (!nrow(nh)) return(NULL)
  res <- resumen(nh, "subsegmento")
  extra <- nh[, {
    v <- share * FACTORCAL
    .(pct_viviendas = NA_real_,
      eur_dia_rel_mediana = round(wmed(eur_dia_rel, v), 2),
      pct_con_suministros = if (all(is.na(SUMINISTROS_113))) NA_real_ else
        round(100 * sum(v[!is.na(SUMINISTROS_113) & SUMINISTROS_113 > 0]) / sum(v), 1),
      pct_contrato_de_anios_previos = if (all(is.na(FECHA_CONTRATO_93))) NA_real_ else
        round(100 * sum(v[!is.na(FECHA_CONTRATO_93) &
                          floor(FECHA_CONTRATO_93 / 10000) < anio[1]]) / sum(v), 1))
  }, by = subsegmento]
  res <- merge(res, extra, by = "subsegmento")
  res[, pct_viviendas := round(100 * viviendas / sum(viviendas), 1)]
  setorder(res, subsegmento)
  nums <- setdiff(names(res)[sapply(res, is.numeric)], "n_muestra")
  res[, (nums) := lapply(.SD, function(x) round(x, 1)), .SDcols = nums]
  res[]
}

# --- 5d. MODULOS DEL DISENO DE INVESTIGACION ---------------------------------------
# Tablas que responden, punto por punto, al documento
# "diseño_investigacion_alquiler_no habitual.docx". La modalidad
# "no habitual" es el PROXY heuristico (subsegmentos A y B) y toda la
# geografia es la de RESIDENCIA del declarante.

## M1: oferta y distribucion territorial (nacional, provincia, municipio)
m1_modalidades <- function(base) {
  d <- base[!is.na(modalidad)]
  if (!nrow(d)) return(NULL)
  r <- resumen(d, "modalidad")
  r[, pct_viviendas := round(100 * viviendas / sum(viviendas), 1)]
  setorder(r, modalidad)
  nums <- setdiff(names(r)[sapply(r, is.numeric)], "n_muestra")
  r[, (nums) := lapply(.SD, function(x) round(x, 1)), .SDcols = nums]
  r[]
}

m1_modalidades_prov <- function(base) {
  d <- base[!is.na(modalidad)]
  if (!nrow(d)) return(NULL)
  r <- d[, {
    v <- share * FACTORCAL
    i_m <- !is.na(alq_mes); i_d <- !is.na(dias_ef) & dias_ef > 0
  i_v <- i_d & !is.na(vc_best)
    .(n_muestra = .N,
      viviendas = round(sum(v)),
      alquiler_mes = if (any(i_m)) round(sum(ing_viv[i_m] * v[i_m]) / sum(v[i_m]) / 12) else NA_real_,
      dias_medios = if (any(i_d)) round(sum(dias_ef[i_d] * v[i_d]) / sum(v[i_d])) else NA_real_)
  }, by = .(provincia, modalidad)]
  w <- dcast(r, provincia ~ modalidad,
             value.var = c("n_muestra", "viviendas", "alquiler_mes", "dias_medios"))
  setnames(w, names(w), gsub("_1\\. Habitual", "_hab", names(w)))
  setnames(w, names(w), gsub("_2\\. No habitual \\(proxy\\)", "_tt", names(w)))
  setnames(w, names(w), gsub("_3\\. Otro no habitual", "_otro_nh", names(w)))
  for (col in c("viviendas_hab", "viviendas_tt", "viviendas_otro_nh",
                "alquiler_mes_hab", "alquiler_mes_tt"))
    if (!col %in% names(w)) w[, (col) := NA_real_]
  # M1: ratio de oferta no habitual frente a residencial habitual
  w[, ratio_tt_hab_viviendas := round(viviendas_tt / viviendas_hab, 3)]
  w[, pct_tt_sobre_alquiler := round(100 * viviendas_tt /
        (viviendas_hab + viviendas_tt +
         fifelse(is.na(viviendas_otro_nh), 0, viviendas_otro_nh)), 1)]
  # M2: diferencial de precio por territorio
  w[, dif_alquiler_tt_vs_hab_pct := round(100 * (alquiler_mes_tt /
                                                 alquiler_mes_hab - 1), 1)]
  setorderv(w, "viviendas_tt", -1, na.last = TRUE)
  w[]
}

m1_municipios <- function(base) {
  d <- base[!is.na(modalidad)]
  if (!nrow(d)) return(NULL)
  r <- d[, {
    v <- share * FACTORCAL
    i_m <- !is.na(alq_mes)
    .(n_muestra = .N,
      viviendas = round(sum(v)),
      alquiler_mes = if (any(i_m)) round(sum(ing_viv[i_m] * v[i_m]) / sum(v[i_m]) / 12) else NA_real_)
  }, by = .(cpro2, CMUN, provincia, es_capital, modalidad)]
  w <- dcast(r, cpro2 + CMUN + provincia + es_capital ~ modalidad,
             value.var = c("n_muestra", "viviendas", "alquiler_mes"))
  setnames(w, names(w), gsub("_1\\. Habitual", "_hab", names(w)))
  setnames(w, names(w), gsub("_2\\. No habitual \\(proxy\\)", "_tt", names(w)))
  setnames(w, names(w), gsub("_3\\. Otro no habitual", "_otro_nh", names(w)))
  for (col in c("n_muestra_hab", "n_muestra_tt", "viviendas_hab", "viviendas_tt"))
    if (!col %in% names(w)) w[, (col) := NA_real_]
  # solo municipios con muestra suficiente en alguna modalidad
  w <- w[pmax(fifelse(is.na(n_muestra_hab), 0L, as.integer(n_muestra_hab)),
              fifelse(is.na(n_muestra_tt), 0L, as.integer(n_muestra_tt))) >=
         MIN_MUESTRA_MUNI]
  if (!nrow(w)) return(NULL)
  w[, ratio_tt_hab_viviendas := round(viviendas_tt / viviendas_hab, 3)]
  w[, nota := "Municipio de RESIDENCIA del declarante (codigos INE CPRO+CMUN)"]
  setorderv(w, "viviendas_tt", -1, na.last = TRUE)
  w[]
}

## M2: ingresos por DECLARANTE segun modalidad (el diseno pide la optica
## declarante ademas de la optica vivienda)
m2_declarantes <- function(pers) {
  g <- function(mask, etiqueta, ing) {
    p <- pers[mask]
    if (!nrow(p)) return(NULL)
    data.table(Grupo = etiqueta,
               declarantes = round(sum(p$peso)),
               ingresos_alq_medios_declarante = round(sum(ing(p) * p$peso) /
                                                      sum(p$peso)),
               ingresos_alq_medianos = round(wmed(ing(p), p$peso)),
               pct_multi_de_su_modalidad = NA_real_)
  }
  t1 <- g(pers$tiene_hab, "Con alquiler habitual", function(p) p$ing_hab)
  if (!is.null(t1)) t1[, pct_multi_de_su_modalidad :=
        round(100 * pers[tiene_hab == TRUE, sum(peso * (n_viv_hab >= 2)) / sum(peso)], 1)]
  t2 <- g(pers$tiene_tt, "Con no habitual (proxy)", function(p) p$ing_tt)
  if (!is.null(t2)) t2[, pct_multi_de_su_modalidad :=
        round(100 * pers[tiene_tt == TRUE, sum(peso * (n_viv_tt >= 2)) / sum(peso)], 1)]
  t3 <- g(pers$tiene_tur & !pers$tiene_tt, "Solo otro no habitual (sin tt)",
          function(p) p$ing_tur)
  t4 <- g(rep(TRUE, nrow(pers)), "Todos los arrendadores",
          function(p) p$ing_alq_total)
  out <- rbindlist(Filter(Negate(is.null), list(t1, t2, t3, t4)), fill = TRUE)
  out[, nota := "Los grupos se solapan: un declarante puede tener varias modalidades"]
  out[]
}

## M3: estructura de propiedad POR MODALIDAD, con las categorias del diseno
m3_estructura <- function(base, pers) {
  bloque <- function(modo, clase_col, n_col, tiene_col, ing_col) {
    p <- pers[get(tiene_col) == TRUE]
    if (!nrow(p)) return(NULL)
    d <- p[, .(declarantes = round(sum(peso)),
               viviendas = round(sum(peso * get(n_col))),
               ingresos = sum(peso * get(ing_col))), by = c(clase_col)]
    setnames(d, clase_col, "categoria")
    d[, `:=`(modalidad = modo,
             pct_declarantes = round(100 * declarantes / sum(declarantes), 1),
             pct_viviendas = round(100 * viviendas / sum(viviendas), 1),
             pct_ingresos = round(100 * ingresos / sum(ingresos), 1),
             ingresos = round(ingresos))]
    d[, orden := match(categoria, ETIQ_CARTERA)]
    setorder(d, orden)[, orden := NULL]
    d[]
  }
  rbindlist(Filter(Negate(is.null), list(
    bloque("No habitual (proxy)", "clase_tt", "n_viv_tt", "tiene_tt", "ing_tt"),
    bloque("Habitual", "clase_hab", "n_viv_hab", "tiene_hab", "ing_hab"))),
    fill = TRUE)
}

m3_estructura_prov <- function(pers) {
  p <- pers[tiene_tt == TRUE]
  if (!nrow(p)) return(NULL)
  d <- p[, .(declarantes = round(sum(peso)),
             viviendas_tt = round(sum(peso * n_viv_tt))),
         by = .(provincia, categoria = clase_tt)]
  d[, orden := match(categoria, ETIQ_CARTERA)]
  setorder(d, provincia, orden)[, orden := NULL]
  d[]
}

## M4: renta bruta total del declarante por modalidad y tramo (requiere 01c)
m4_renta <- function(pers) {
  if (all(is.na(pers$renta_total))) return(NULL)
  p <- pers[!is.na(renta_total)]
  cobertura <- 100 * nrow(p) / nrow(pers)
  bloque <- function(mask, etiqueta) {
    q <- p[mask]
    if (!nrow(q)) return(NULL)
    tot <- q[, .(tramo_renta = "TODOS",
                 declarantes = round(sum(peso)),
                 renta_bruta_media = round(sum(renta_total * peso) / sum(peso)),
                 renta_bruta_mediana = round(wmed(renta_total, peso)),
                 ingresos_alq_medios = round(sum(ing_alq_total * peso) / sum(peso)),
                 peso_alq_sobre_renta_pct = round(sum(peso * fifelse(
                   is.na(peso_alq_renta), 0, peso_alq_renta)) / sum(peso), 1),
                 pct_multi = round(100 * sum(peso * (clase == "Multiarrendador")) /
                                   sum(peso), 1))]
    tr <- q[!is.na(tramo_renta),
            .(declarantes = round(sum(peso)),
              renta_bruta_media = round(sum(renta_total * peso) / sum(peso)),
              renta_bruta_mediana = round(wmed(renta_total, peso)),
              ingresos_alq_medios = round(sum(ing_alq_total * peso) / sum(peso)),
              peso_alq_sobre_renta_pct = round(sum(peso * fifelse(
                is.na(peso_alq_renta), 0, peso_alq_renta)) / sum(peso), 1),
              pct_multi = round(100 * sum(peso * (clase == "Multiarrendador")) /
                                sum(peso), 1)),
            by = tramo_renta]
    tr[, orden := match(tramo_renta, ETIQ_RENTA)]
    setorder(tr, orden)[, orden := NULL]
    out <- rbind(tot, tr, fill = TRUE)
    out[, Grupo := etiqueta]
    setcolorder(out, c("Grupo", "tramo_renta"))
    out[]
  }
  out <- rbindlist(Filter(Negate(is.null), list(
    bloque(p$tiene_hab, "Con alquiler habitual"),
    bloque(p$tiene_tt, "Con no habitual (proxy)"),
    bloque(p$tiene_tur & !p$tiene_tt, "Solo otro no habitual (sin tt)"))),
    fill = TRUE)
  attr(out, "cobertura_renta_pct") <- round(cobertura, 1)
  out[]
}

## Mapa del diseno de investigacion: donde se responde cada modulo y limites
tabla_diseno <- function(hay_renta, hay_muni) {
  data.table(
    Modulo = c(
      "M1. Oferta y distribucion territorial",
      "M1. Municipios",
      "M1. Proporcion del parque total",
      "M2. Diferencial de precio",
      "M3. Estructura de propiedad",
      "M4. Renta de los arrendadores",
      "M5. Evolucion temporal"),
    Hojas = c(
      "M1_Modalidades, M1_Modalidades_prov, Contraste_provincial",
      if (hay_muni) "M1_Municipios" else "M1_Municipios (VACIA: sin municipios con muestra suficiente)",
      "(no calculable con el panel)",
      "M1_Modalidades_prov (columna dif_alquiler), M2_Declarantes, Usos, Subsegmentos_no_habitual",
      "M3_Estructura, M3_Estructura_prov, Concentracion, Desglose_n_viviendas",
      if (hay_renta) "M4_Renta_declarantes" else "M4_Renta_declarantes (VACIA: ejecuta 01c para anadir el fichero 2_Renta)",
      "Evol_por_uso, Evol_por_segmento, Evol_estructura, Evol_provincias, Evol_declarantes"),
    Limitaciones = c(
      "'No habitual' es un PROXY heuristico (subsegmentos A+B); geografia = residencia del declarante, no ubicacion del inmueble",
      "Municipio de residencia; solo municipios con >= 30 observaciones; codigos INE sin nombre",
      "El denominador (parque total de viviendas) es externo: Censo INE; el panel solo ve el alquiler declarado",
      "El alquiler mensual eleva a anio completo (definicion AEAT); el diferencial por dia esta en euros_dia y eur_dia_rel",
      "Cartera contada dentro de cada modalidad, categorias 1/2/3/4/5-9/10+; la cotitularidad separa a los conyuges salvo NIVEL_ARRENDADOR='hogar'",
      "Renta bruta = M1+M2+M3+M4+M5+M6 del fichero 2_Renta; disponible solo para declarantes presentes en ese fichero",
      "2016 sin dias reales: la modalidad tt no existe ese anio; la serie compara habitual vs no habitual y llega hasta 2023 (no hay panel 2024)"))
}

# --- 6. CARGA DE LOS EJERCICIOS ----------------------------------------------------
anios <- c(ANIO_REF, if (!is.na(ANIO_COMP)) ANIO_COMP)
datos <- setNames(lapply(anios, preparar), as.character(anios))
datos <- datos[!sapply(datos, is.null)]
if (!length(datos)) stop("No se encuentra ninguna base panel_arrendamientos_*.csv")
if (is.null(datos[[as.character(ANIO_REF)]]))
  stop("Falta la base del ejercicio de referencia ", ANIO_REF)
hay_comp <- !is.na(ANIO_COMP) && !is.null(datos[[as.character(ANIO_COMP)]])

dias_comparables <- TRUE
if (hay_comp) {
  for (a in names(datos)) {
    m <- datos[[a]]$meta
    cols <- names(datos[[a]]$base)
    if (!is.null(m$inmuebles_con_dias_estimados) &&
        !is.null(m$filas_salida) &&
        as.numeric(m$inmuebles_con_dias_estimados) >=
          0.5 * as.numeric(m$filas_salida)) {
      dias_comparables <- FALSE
    }
    if ("PERIODO_COMPUTABLE_58" %in% cols) dias_comparables <- FALSE
  }
}
if (!hay_comp && !is.na(ANIO_COMP)) {
  carp <- carpeta_anio(ANIO_COMP)
  esperado <- ruta_base_anio(ANIO_COMP)
  script <- if (ANIO_COMP == 2016) "01b_extraer_panel_2016.py"
            else "01b_extraer_panel_sin_dependencias.py"
  message("AVISO: no hay base de ", ANIO_COMP,
          "; el informe saldrá sin comparación temporal.")
  message("       Fichero buscado: ", esperado)
  if (!dir.exists(carp)) {
    message("       Esa CARPETA NO EXISTE. Revisa RUTA_RAIZ y ANIO_COMP ",
            "al principio de este script.")
  } else {
    hay <- list.files(carp, pattern = "^panel_arrendamientos.*\\.csv(\\.gz)?$")
    if (length(hay)) {
      message("       En la carpeta sí hay: ", paste(hay, collapse = ", "),
              "\n       El nombre debe ser exactamente panel_arrendamientos_",
              ANIO_COMP, ".csv (o .csv.gz).")
    } else {
      message("       La carpeta existe pero no contiene ninguna base extraída.")
    }
  }
  message("       Genérala con:  python ", script, " --datos \"", carp, "\"")
}

base_full <- datos[[as.character(ANIO_REF)]]$base   # universo completo (fichero 8)
embudo    <- datos[[as.character(ANIO_REF)]]$embudo

# Inventario de senales disponibles para el score.
senales_disp <- c(
  "alquiler vs habitual provincial" = TRUE,
  "suelo EUR/m2 AEAT"               = TRUE,
  "valor catastral (c.83 o c.123)"  = any(!is.na(base_full$vc_best)),
  "amortizacion (c.131)"            = any(!is.na(base_full$AMORT_INM_131) &
                                          base_full$AMORT_INM_131 > 0),
  "IBI/tributos (c.115)"            = any(!is.na(base_full$IBI_115) &
                                          base_full$IBI_115 > 0))
message("Senales disponibles para el score de vivienda:")
for (nm in names(senales_disp))
  message("  [", if (senales_disp[[nm]]) "x" else " ", "] ", nm)
if (!senales_disp[["valor catastral (c.83 o c.123)"]] ||
    !senales_disp[["amortizacion (c.131)"]]) {
  message("  -> Ejecuta 01c_enriquecer_panel_2023.py sobre el fichero 8 para ",
          "anadir las casillas 65, 109, 113, 114, 115, 123, 124, 126, 131 y 93: ",
          "el score gana precision con ellas.")
}

# --- 6b. [AEAT] SCORE, CALIBRACION Y FILTRO --------------------------------------
message("Calculando el score de vivienda y calibrando el filtro contra la AEAT...")
if (NH_CRITERIO == "auto")
  NH_CRITERIO <- if (any(c("VIV_RC", "URBACLAVE_RC") %in% names(base_full)))
    "catastral_real" else "temporada"
message(sprintf("  Criterio del No habitual (NH_CRITERIO): '%s'", NH_CRITERIO))
if ("INQ_DOMICILIADO" %in% names(base_full)) {
  n_inq <- base_full[!is.na(INQ_DOMICILIADO) & INQ_DOMICILIADO == 1, .N]
  message(sprintf(paste0("  Modulo inmobiliario DETECTADO en el panel ",
          "(INQ_DOMICILIADO en %s inmuebles, VIV_RC=%s). "),
          format(n_inq, big.mark = "."),
          ifelse("VIV_RC" %in% names(base_full), "si", "no")))
} else {
  message("  Modulo inmobiliario NO presente en el panel (sin INQ_DOMICILIADO). ",
          "Para NH_CRITERIO='inquilino' pasa antes 01d_cruzar_inmuebles.py.")
}
if ("DIAS_RC" %in% names(base_full)) {
  pct_rc <- round(100 * base_full[!is.na(DIAS_RC) & DIAS_RC > 0, .N] /
                  max(nrow(base_full), 1), 1)
  message(sprintf(paste0("  DIAS CENSALES por RC detectados (DIAS_RC, paso 5 ",
          "del 01d): %s%% de inmuebles con dato. El contraste incluira la ",
          "columna dias_censal_RC."), pct_rc))
} else {
  message("  Sin DIAS_RC en el panel: ejecuta el 01d (paso 5 por defecto) ",
          "para anadir los dias censales por referencia catastral.")
}

# [UBICACION] cpro_ef = provincia EFECTIVA para todos los patrones provinciales
# (umbral de intensidad, percentiles, banda de VC, EUR/m2): la del INMUEBLE
# (PROV_INM del modulo) cuando existe, y la de residencia como respaldo. Evita
# comparar un piso de Madrid con precios de la provincia del dueno.
message("  === VERSION SCRIPT: 2026-08-04-F (E + lente agregada de alquiler, pesos calados a AEAT e IC bootstrap provincial) ===")
base_full[, cpro_ef := cpro2]

anotar_score(base_full)
anotar_subsegmento(base_full)

# [EJE_PRINCIPAL] Si se elige el criterio "uso" (como la AEAT), se redefine el
# eje hab/tur de TODO el informe: el alquiler residencial de larga duracion sin
# reduccion (uso_aeat == "habitual" pero uso == "tur") pasa a habitual, y el no
# habitual queda como un unico grupo (no habitual). Se hace ANTES de
# finalizar y sobre base_full, asi se propaga a Usos, Modalidades y contrastes.
# El score ya se calculo con el patron por reduccion (lo correcto), no se afecta.
uso_predom <- NULL
if (EJE_PRINCIPAL == "uso" && "uso_aeat" %in% names(base_full)) {
  base_full[, uso_reduccion := uso]          # se guarda el eje clasico por si acaso
  uso_predom <- tryCatch(diagnostico_uso_predominante(base_full),
                         error = function(e) NULL)
  n_fuera <- base_full[uso_aeat == "fuera_tabla",
                       sum(share * FACTORCAL)]
  embudo[["4b. Fuera de la tabla AEAT (mixtas c.76 'habitual con parte arrendada'; C/D no residencial)"]] <-
    nrow(base_full[uso_aeat == "fuera_tabla"])
  base_full <- base_full[uso_aeat != "fuera_tabla"]
  # CANDADOS DUROS del No habitual (diseno de registro): un inmueble arrendado
  # sin reduccion solo cuenta como vivienda no habitual si (a) su ingreso anual
  # al 100% de titularidad alcanza el minimo de una vivienda (c.102; por debajo
  # es garaje/trastero con contrato propio) y (b) su valor catastral, si consta
  # (c.83/123), no supera VC_LOCAL_FACTOR veces el techo del habitual provincial
  # (por encima es local/nave). Se apartan a fuera de tabla, como hace la AEAT
  # via el uso catastral.
  cand <- base_full$uso_aeat == "no_habitual" &
          (!is.na(base_full$ing_anual_100) &
           base_full$ing_anual_100 < ING_GARAJE_MAX)
  if (any(cand)) {
    embudo[["4c. No habitual apartado por candados (ingreso infimo o VC de local)"]] <-
      sum(cand)
    base_full <- base_full[!cand]
  }
  base_full[, uso := fifelse(uso_aeat == "habitual", "hab", "tur")]
  col_fc93 <- col1(base_full, c("FECHA_CONTRATO_93", "PAR93"))
  if (!is.na(col_fc93)) {
    fx <- suppressWarnings(as.numeric(base_full[uso == "tur"][[col_fc93]]))
    pct_c93 <<- round(100 * mean(!is.na(fx) & fx > 0), 1)
    message(sprintf("  Casilla 93 (fecha de contrato) informada en el %.1f%% de los arrendamientos sin reduccion.", pct_c93))
  }
  message(sprintf(paste0("  Eje AEAT validado: habitual = reduccion c.100/150 a secas; ",
          "no habitual = no habitual (A+B). Excluidas de la tabla: ",
          "%s viviendas elevadas (mixtas c.76 y C/D no residencial)."),
          formatC(round(n_fuera), format = "d", big.mark = ".")))
}

cal <- calibrar_filtro_residencial(base_full)
FILTRO_ELEGIDO <- elegir_filtro(cal)
mask_viv <- cal$mascaras[[FILTRO_ELEGIDO]]
message("  Filtro aplicado al informe: ", FILTRO_ELEGIDO)

# El filtro residencial se aplica SOLO al no habitual: el habitual lleva la
# reduccion 23.2, que por definicion legal solo cabe en arrendamiento de
# vivienda, asi que ya es vivienda sin necesidad de filtro (y sin filtrar
# reproduce el ancla AEAT con ratio 1,006). Filtrarlo solo puede alejarlo.
base_ref <- base_full[uso == "hab" | mask_viv]
embudo[[sprintf("5. Filtro de vivienda residencial %s",
                sub(" .*$", "", FILTRO_ELEGIDO))]] <- nrow(base_ref)

fin_ref <- finalizar(base_ref)
base <- fin_ref$base
pers <- fin_ref$pers

score_dist <- tryCatch(distribucion_score(base_full), error = function(e) NULL)
embudo_nh  <- tryCatch(diagnostico_embudo_nh(base_full), error = function(e) NULL)
# (uso_predom se calcula antes de la exclusion de fuera_tabla, en el bloque EJE)

# --- 7. HOJAS DEL EJERCICIO DE REFERENCIA ------------------------------------------
message("Calculando estadísticas de ", ANIO_REF, "...")

## 7.1 Nacional
base[, ambito := "Nacional"]; pers[, ambito := "Nacional"]
nac <- merge(a_ancho(con_total(base, "ambito"), "ambito"),
             cuenta_arrendadores(pers, "ambito"), by = c("ambito", "clase"))

## 7.2 Provincias (residencia del ARRENDADOR; la tabla AEAT es por ubicacion
## del INMUEBLE: la comparacion provincial es orientativa)
prov <- merge(a_ancho(con_total(base, "provincia"), "provincia"),
              cuenta_arrendadores(pers, "provincia"),
              by = c("provincia", "clase"), all.x = TRUE)

## 7.3 Capitales de provincia (vacia si ningun declarante reside en capital)
cap <- NULL
if (nrow(base[es_capital == 1L])) {
  cap_a <- a_ancho(con_total(base[es_capital == 1L], "provincia"), "provincia")
  if (!is.null(cap_a)) {
    cap <- merge(cap_a,
                 cuenta_arrendadores(pers[es_capital == 1L], "provincia"),
                 by = c("provincia", "clase"), all.x = TRUE)
    setnames(cap, "provincia", "capital_de")
  }
}

## 7.4 Desglose por numero de viviendas del arrendador
det <- dcast(resumen(base, c("clase_det", "uso")), clase_det ~ uso,
             value.var = c("viviendas", "ingresos_medios", "ingresos_mediana",
                           "alquiler_mes", "dias_medios", "rentab_vcat_pct"))
distrib <- pers[, .(arrendadores = round(sum(peso)),
                    viviendas_en_alquiler = round(sum(peso * n_viv))), by = clase_det]
det <- merge(distrib, det, by = "clase_det", all = TRUE)
det[, orden := match(clase_det, ETIQ_CARTERA)]
setorder(det, orden); det[, orden := NULL]
if ("ingresos_medios_tur" %in% names(det) && "ingresos_medios_hab" %in% names(det))
  det[, dif_ingresos_pct := round(100 * (ingresos_medios_tur / ingresos_medios_hab - 1), 1)]
numd <- names(det)[sapply(det, is.numeric)]
det[, (numd) := lapply(.SD, function(x) round(x, 1)), .SDcols = numd]

## 7.4b Comparacion por tramos de ocupacion (uso x tramo de dias)
tramos <- resumen(base[!is.na(tramo_dias)], c("segmento", "tramo_dias"))
setorder(tramos, segmento, tramo_dias)
numt <- setdiff(names(tramos)[sapply(tramos, is.numeric)], "n_muestra")
tramos[, (numt) := lapply(.SD, function(x) round(x, 1)), .SDcols = numt]

## 7.4c [AEAT] DESGLOSE DE LA BRECHA con la AEAT (unidad = vivienda entera)
desglose_brecha <- function(dt, mask, filtro_txt) {
  d_idx <- dt$uso_original == "tur"
  if (!any(d_idx)) return(NULL)
  w   <- dt$FACTORCAL
  veq <- dt$share * w
  con_vc <- !is.na(dt$vc_best)

  m0 <- d_idx
  m1 <- m0 & !dt$mixto_a_hab
  m2 <- m1 & mask
  suma <- function(m) sum(veq[m])
  cobertura_vc <- suma(m1 & con_vc) / suma(m1)

  pasos <- c(
    "0. Inmuebles urbanos arrendados sin reduccion (fichero 8, tras c.65)",
    "1. (-) Con uso de vivienda habitual del propietario en el anio (a 'habitual')",
    "   (info) De (1), con algun valor catastral informado (c.83 o c.123)",
    sprintf("2. (-) Filtro de vivienda residencial: %s", filtro_txt),
    "Referencia AEAT 2023 (viviendas arrendadas, vivienda habitual = No)")
  vals <- c(suma(m0), suma(m1), suma(m1 & con_vc), suma(m2),
            AEAT_REF$no_habitual$viviendas)
  base0 <- suma(m0)
  data.table(
    Paso = pasos,
    Viviendas_no_habitual = round(vals),
    Reduccion_vs_paso0_pct = round(100 * (vals / base0 - 1), 1),
    vs_AEAT_ratio = round(vals / AEAT_REF$no_habitual$viviendas, 2),
    Comentario = c(
      "Cada inmueble arrendado cuenta como 1 (x % de titularidad); sin ponderar por dias",
      "Regla de prioridad AEAT: cualquier uso de vivienda habitual del duenio la saca del no habitual",
      sprintf("Cobertura del VC en el no habitual: %.1f%% (fila informativa, no es un paso del filtro)",
              100 * cobertura_vc),
      "Proxy del cruce con Catastro de la AEAT (ver hojas Calibracion_AEAT y Score_distribucion)",
      "Viviendas arrendadas no habituales publicadas por la AEAT (Bloque I)"))
}
desglose <- tryCatch(desglose_brecha(base_full, mask_viv, FILTRO_ELEGIDO),
                     error = function(e) NULL)

## 7.4d DIAGNOSTICO DEL VALOR CATASTRAL (universo completo, por inmueble)
diagnostico_vc <- function(dt) {
  f <- function(sub, etiqueta) {
    if (!nrow(sub)) return(NULL)
    w <- sub$FACTORCAL; veq <- sub$share * w
    v83  <- !is.na(sub$VALOR_CAT_83) & sub$VALOR_CAT_83 > 0
    v123 <- !is.na(sub$VC_AMORT_123) & sub$VC_AMORT_123 > 0
    vb   <- !is.na(sub$vc_best)
    data.table(
      Uso              = etiqueta,
      inmuebles        = round(sum(veq)),
      con_VC83_pct     = round(100 * sum(veq[v83])  / sum(veq), 1),
      con_VC123_pct    = round(100 * sum(veq[v123]) / sum(veq), 1),
      con_algun_VC_pct = round(100 * sum(veq[vb])   / sum(veq), 1),
      VC_mediana       = round(wmed(sub$vc_best[vb], veq[vb])),
      VC_p95           = round(wq(sub$vc_best[vb], veq[vb], 0.95)))
  }
  rbind(f(dt[uso == "hab"], "Habitual"),
        f(dt[uso == "tur"], "No habitual"))
}
diag_vc <- tryCatch(diagnostico_vc(base_full), error = function(e) NULL)

## 7.5 Usos en formato largo
seg <- resumen(base, c("segmento", "clase"))
seg_tot <- resumen(base, "segmento")[, clase := "Todos los arrendadores"]
seg <- rbind(seg, seg_tot, fill = TRUE)
seg[, pct_viviendas := round(100 * viviendas /
      sum(viviendas[clase == "Todos los arrendadores"]), 1)]
setorder(seg, segmento, clase)
nums <- setdiff(names(seg)[sapply(seg, is.numeric)], "n_muestra")
seg[, (nums) := lapply(.SD, function(x) round(x, 1)), .SDcols = nums]

## 7.6 Subsegmentacion del no habitual (heuristica; sobre el universo filtrado)
subseg <- tryCatch(tabla_subsegmentos(base), error = function(e) NULL)

## 7.6b Modulos del diseno de investigacion (M1-M4)
m1_nac  <- tryCatch(m1_modalidades(base), error = function(e) NULL)
m1_prov <- tryCatch(m1_modalidades_prov(base), error = function(e) NULL)
m1_muni <- tryCatch(m1_municipios(base), error = function(e) NULL)
m2_decl <- tryCatch(m2_declarantes(pers), error = function(e) NULL)
m3_estr <- tryCatch(m3_estructura(base, pers), error = function(e) NULL)
m3_prov <- tryCatch(m3_estructura_prov(pers), error = function(e) NULL)
m4_rent <- tryCatch(m4_renta(pers), error = function(e) NULL)
hay_renta_ref <- !is.null(m4_rent)
if (!hay_renta_ref)
  message("  AVISO [M4]: el panel no trae la renta del declarante ",
          "(RENTA_BRUTA_TOTAL). Ejecuta 01c_enriquecer_panel.py para leerla ",
          "del fichero 2_Renta", ANIO_REF, ".txt; sin ella la hoja ",
          "M4_Renta_declarantes queda vacia.")
diseno_map <- tabla_diseno(hay_renta_ref, !is.null(m1_muni))

## 7.7 Concentracion
conc <- base[, .(viviendas = sum(share * FACTORCAL),
                 ingresos = sum(INGRESOS_102 * FACTORCAL)), by = .(clase_det, uso)]
conc[, `:=`(pct_viviendas = round(100 * viviendas / sum(viviendas), 1),
            pct_ingresos = round(100 * ingresos / sum(ingresos), 1)), by = uso]
conc[, `:=`(viviendas = round(viviendas), ingresos = round(ingresos))]
setorder(conc, uso, clase_det)

## 7.8 Fila nacional y contrastes con la AEAT
tot <- nac[clase == "Todos los arrendadores"]

# [AEAT] MAPEO Y ELEVACION CORRECTOS (verificado en la nota metodologica 2023).
# (1) La tabla AEAT 'Rentabilidad y precios de alquiler' (filtro 'Vivienda
#     habitual: Si/No') clasifica VIVIENDAS ARRENDADAS; ambas columnas son
#     alquiler:
#       'Vivienda habitual = Si' (2.409.689) = arrendada como vivienda habitual
#            del INQUILINO, con o SIN reduccion (criterio de USO, no de la
#            reduccion). Aqui se aproxima con uso_aeat = reduccion 23.2 O larga
#            duracion residencial no no habitual (ver anotar_subsegmento).
#       'Vivienda habitual = No' (309.479)   = arrendada con otros usos
#            (no habitual, temporada).
# (2) El panel es una MUESTRA que se eleva a poblacion con FACTORCAL; la AEAT
#     publica cifras POBLACIONALES. La cifra comparable es la ELEVADA.
ct_aeat <- base[, {
  w <- FACTORCAL; veq <- share * w
  d_use <- if ("DIAS_RC" %in% names(.SD)) {
    x <- suppressWarnings(as.numeric(DIAS_RC))
    fifelse(is.na(x), dias_ef, pmin(x, 365))
  } else dias_ef
  i_m <- !is.na(alq_mes); i_d <- !is.na(d_use) & d_use > 0
  i_v <- i_d & !is.na(vc_best)
  .(viviendas = sum(veq),
    viviendas_sin_elevar = sum(share),
    alquiler_mes = if (any(i_m)) sum(ing_viv[i_m] * w[i_m]) / sum(w[i_m]) / 12 else NA_real_,
    dias_medios = if (any(i_d)) sum(d_use[i_d] * w[i_d]) / sum(w[i_d]) else NA_real_,
    dias_solo_con_VC = if (any(i_v)) sum(d_use[i_v] * w[i_v]) / sum(w[i_v]) else NA_real_,
    alquiler_solo_con_VC = if (any(i_v)) sum(ing_viv[i_v] * w[i_v]) / sum(w[i_v]) / 12 else NA_real_)
}, by = uso_aeat]
if ("DIAS_RC" %in% names(base)) {
  ct_rc <- base[!is.na(DIAS_RC) & DIAS_RC > 0,
                .(dias_censal_RC = sum(pmin(DIAS_RC, 365) * FACTORCAL) / sum(FACTORCAL)),
                by = uso_aeat]
  ct_aeat <- merge(ct_aeat, ct_rc, by = "uso_aeat", all.x = TRUE)
}
gh <- ct_aeat[uso_aeat == "habitual"]
gn <- ct_aeat[uso_aeat == "no_habitual"]
if (!nrow(gh)) gh <- ct_aeat[0][1]
if (!nrow(gn)) gn <- ct_aeat[0][1]

contraste <- data.table(
  Segmento  = rep(c("Vivienda habitual",
                    "Vivienda no habitual"), each = 3),
  Indicador = rep(c("Viviendas (elevadas, comparable AEAT)",
                    "Alquiler medio mensual (EUR)",
                    "Dias de alquiler medios"), 2),
  Panel = c(round(gh$viviendas), round(gh$alquiler_mes), round(gh$dias_medios, 1),
            round(gn$viviendas), round(gn$alquiler_mes), round(gn$dias_medios, 1)),
  AEAT_2023 = c(AEAT_REF$habitual$viviendas, AEAT_REF$habitual$alquiler_mes,
                AEAT_REF$habitual$dias,
                AEAT_REF$no_habitual$viviendas, AEAT_REF$no_habitual$alquiler_mes,
                AEAT_REF$no_habitual$dias))
contraste[, Ratio_Panel_AEAT := round(Panel / AEAT_2023, 2)]


# [POR CASERO] Ingresos anuales medios POR ARRENDADOR y uso (la metrica de la
# AEAT difundida por eldiario.es: nacional 7.244 EUR habitual / 9.738 EUR
# no habitual en 2023). Se suma la parte de ingresos DEL DECLARANTE
# (c.102, su parte) en todas sus viviendas de cada uso y se promedia por
# casero con FACTORCAL.
contraste_casero <- tryCatch({
  ci <- col1(base, c("INGRESOS_102", "PAR102"))
  if (is.na(ci)) NULL else {
    cas <- base[, .(ing_casero = sum(suppressWarnings(as.numeric(get(ci))),
                                     na.rm = TRUE),
                    w = FACTORCAL[1]), by = .(IDENPER, uso)]
    res <- cas[, .(caseros_elevados = round(sum(w)),
                   ingreso_anual_medio_por_casero =
                     round(sum(ing_casero * w) / sum(w))), by = uso]
    res[, uso := fifelse(uso == "hab", "Vivienda habitual", "Vivienda no habitual")]
    res[, AEAT_2023_eldiario := fifelse(uso == "Vivienda habitual", 7244, 9738)]
    res[, Ratio := round(ingreso_anual_medio_por_casero / AEAT_2023_eldiario, 2)]
    as.data.frame(res)
  }
}, error = function(e) NULL)

# Detalle: eje AEAT (por uso) y eje de trabajo (por reduccion) lado a lado.
contraste_elevado <- data.table(
  Concepto = c(
    "Habitual AEAT: reduccion 23.2 a secas (c.100/150)",
    "No habitual AEAT (uso): no habitual",
    "-- eje de trabajo (por reduccion 23.2) --",
    "Habitual (solo con reduccion 23.2)",
    "No habitual (todo lo demas)"),
  viviendas_elevadas = round(c(gh$viviendas, gn$viviendas, NA,
                               tot$viviendas_hab, tot$viviendas_tur)),
  AEAT_2023 = c(AEAT_REF$habitual$viviendas, AEAT_REF$no_habitual$viviendas,
                NA, AEAT_REF$habitual$viviendas, AEAT_REF$no_habitual$viviendas),
  ratio_AEAT = round(c(gh$viviendas / AEAT_REF$habitual$viviendas,
                       gn$viviendas / AEAT_REF$no_habitual$viviendas, NA,
                       tot$viviendas_hab / AEAT_REF$habitual$viviendas,
                       tot$viviendas_tur / AEAT_REF$no_habitual$viviendas), 2),
  nota = c("Criterio USO predominante (como la AEAT)",
           "Criterio USO predominante (como la AEAT)",
           "",
           "Criterio reduccion (tu eje de trabajo)",
           "Criterio reduccion (incluye larga duracion sin reduccion)"))

## 7.8b [AEAT] Contraste PROVINCIAL del no habitual (panel filtrado, provincia
## de residencia del declarante) frente a la tabla AEAT (ubicacion del inmueble)
cprov <- base[uso == "tur", {
  v <- share * FACTORCAL
  i_m <- !is.na(alq_mes)
  i_d <- !is.na(dias_ef) & dias_ef > 0
  .(viviendas_panel = round(sum(v)),
    alq_mes_panel = if (any(i_m)) round(sum(ing_viv[i_m] * v[i_m]) / sum(v[i_m]) / 12) else NA_real_,
    dias_panel = if (any(i_d)) round(sum(dias_ef[i_d] * v[i_d]) / sum(v[i_d])) else NA_real_)
}, by = .(cpro2, provincia = provincia)]
cprov <- merge(cprov, AEAT_PROV, by.x = "cpro2", by.y = "cpro", all.x = TRUE)
cprov[, `:=`(ratio_viviendas = round(viviendas_panel / viviendas_aeat, 2),
             ratio_alq_mes = round(alq_mes_panel / alq_mes_aeat, 2),
             nota = "Panel: residencia del declarante | AEAT: ubicacion del inmueble")]
setorder(cprov, -viviendas_panel)

## 7.8c [AEAT] Contraste provincial por UBICACION DEL INMUEBLE (PROV_INM del
## modulo inmobiliario): la comparacion homogenea con la tabla AEAT, que
## tambien va por ubicacion. Usa dias censales (DIAS_RC) si estan disponibles.
cprov_ub <- NULL
if ("PROV_INM" %in% names(base)) {
  cprov_ub <- base[uso == "tur" & !is.na(PROV_INM) & PROV_INM != "" &
                   grepl("^[0-9]{1,2}$", trimws(PROV_INM)) &
                   suppressWarnings(as.integer(PROV_INM)) %in% 1:52, {
    v <- share * FACTORCAL
    i_m <- !is.na(alq_mes)
    d_use <- if ("DIAS_RC" %in% names(.SD))
      suppressWarnings(as.numeric(DIAS_RC)) else dias_ef
    d_use <- fifelse(is.na(d_use), dias_ef, pmin(d_use, 365))
    i_d <- !is.na(d_use) & d_use > 0
    .(viviendas_panel = round(sum(v)),
      alq_mes_panel = if (any(i_m))
        round(sum(ing_viv[i_m] * FACTORCAL[i_m]) / sum(FACTORCAL[i_m]) / 12)
        else NA_real_,
      alq_mes_elev_panel = if (any(i_m))
        round(sum(alq_mes[i_m] * FACTORCAL[i_m]) / sum(FACTORCAL[i_m]))
        else NA_real_,
      dias_panel = if (any(i_d))
        round(sum(d_use[i_d] * FACTORCAL[i_d]) / sum(FACTORCAL[i_d]))
        else NA_real_,
      alq_mes_agg_panel = if (any(i_d))
        round(sum(ing_viv[i_d] * FACTORCAL[i_d]) /
              sum(d_use[i_d] * FACTORCAL[i_d]) * 30.4)
        else NA_real_)
  }, by = .(cpro2 = sprintf("%02d", suppressWarnings(as.integer(PROV_INM))))]
  cprov_ub <- merge(cprov_ub, AEAT_PROV, by.x = "cpro2", by.y = "cpro",
                    all.x = TRUE)
  cprov_ub[, `:=`(ratio_viviendas = round(viviendas_panel / viviendas_aeat, 2),
                  ratio_alq_mes = round(alq_mes_panel / alq_mes_aeat, 2),
                  ratio_alq_elev = round(alq_mes_elev_panel / alq_mes_aeat, 2),
                  ratio_alq_agg = round(alq_mes_agg_panel / alq_mes_aeat, 2),
                  ratio_dias = if ("dias_aeat" %in% names(cprov_ub))
                    round(dias_panel / dias_aeat, 2) else NA_real_,
                  nota = "Panel y AEAT por UBICACION del inmueble (PROV_INM; dias censales si hay DIAS_RC)")]
  setorder(cprov_ub, -viviendas_panel)
}

## 7.8d [AEAT] Contraste por CCAA (ubicacion del inmueble): agrega el 7.8c a
## comunidad autonoma, donde el tamano de celda del panel permite ratios
## estables (la granularidad provincial de una muestra ~5% tiene error muestral
## superior a la banda 0,96-1,04 por pura varianza, no por sesgo).
cccaa_ub <- NULL
if (!is.null(cprov_ub) && nrow(cprov_ub)) {
  PROV2CCAA <- c("01"="Pais Vasco","02"="C-La Mancha","03"="C. Valenciana",
    "04"="Andalucia","05"="Castilla y Leon","06"="Extremadura","07"="Baleares",
    "08"="Cataluna","09"="Castilla y Leon","10"="Extremadura","11"="Andalucia",
    "12"="C. Valenciana","13"="C-La Mancha","14"="Andalucia","15"="Galicia",
    "16"="C-La Mancha","17"="Cataluna","18"="Andalucia","19"="C-La Mancha",
    "20"="Pais Vasco","21"="Andalucia","22"="Aragon","23"="Andalucia",
    "24"="Castilla y Leon","25"="Cataluna","26"="La Rioja","27"="Galicia",
    "28"="Madrid","29"="Andalucia","30"="Murcia","31"="Navarra",
    "32"="Galicia","33"="Asturias","34"="Castilla y Leon","35"="Canarias",
    "36"="Galicia","37"="Castilla y Leon","38"="Canarias","39"="Cantabria",
    "40"="Castilla y Leon","41"="Andalucia","42"="Castilla y Leon",
    "43"="Cataluna","44"="Aragon","45"="C-La Mancha","46"="C. Valenciana",
    "47"="Castilla y Leon","48"="Pais Vasco","49"="Castilla y Leon",
    "50"="Aragon","51"="Ceuta","52"="Melilla")
  cc <- copy(cprov_ub)
  cc[, ccaa := PROV2CCAA[cpro2]]
  cccaa_ub <- cc[!is.na(ccaa), .(
      viviendas_panel = sum(viviendas_panel, na.rm = TRUE),
      alq_mes_panel = round(sum(alq_mes_panel * viviendas_panel, na.rm = TRUE) /
                            sum(viviendas_panel[!is.na(alq_mes_panel)], na.rm = TRUE)),
      dias_panel = round(sum(dias_panel * viviendas_panel, na.rm = TRUE) /
                         sum(viviendas_panel[!is.na(dias_panel)], na.rm = TRUE)),
      viviendas_aeat = sum(viviendas_aeat, na.rm = TRUE),
      alq_mes_aeat = round(sum(alq_mes_aeat * viviendas_aeat, na.rm = TRUE) /
                           sum(viviendas_aeat[!is.na(alq_mes_aeat)], na.rm = TRUE))),
    by = ccaa]
  cccaa_ub[, `:=`(ratio_viviendas = round(viviendas_panel / viviendas_aeat, 2),
                  ratio_alq_mes = round(alq_mes_panel / alq_mes_aeat, 2))]
  setorder(cccaa_ub, -viviendas_panel)
}

## 7.8e MAPA DE DIVERGENCIAS: para cada provincia con muestra suficiente,
## clasifica el origen probable de cada desviacion y el filtro implicado.
mapa_div <- NULL
if (!is.null(cprov_ub) && nrow(cprov_ub)) {
  md <- cprov_ub[viviendas_panel >= 8000]
  mk <- function(prov, var, ratio, dias) {
    if (is.na(ratio)) return(NULL)
    origen <- if (var == "alquiler" && !is.na(dias) && dias < 260 && ratio < 0.95)
      "METRICA: AEAT eleva ingresos a anio completo; ing/12 infrarrepresenta estancia corta. Ver ratio_alq_elev (lente elevada)."
    else if (var == "viviendas" && ratio > 1.2)
      "FRONTERA INOBSERVABLE: estancia larga con inquilino no muestreado (domicilio del arrendatario suprimido del panel); concentrado en capitales."
    else if (ratio < 0.85)
      "COBERTURA/COMPOSICION: segmento local en la frontera de los candados (ingreso minimo, prueba catastral) o propietario foral/no residente."
    else "Dentro del margen razonable."
    data.table(provincia = prov, variable = var, ratio = ratio, origen = origen)
  }
  L <- list()
  for (r in seq_len(nrow(md))) {
    L[[length(L)+1]] <- mk(md$provincia_aeat[r], "viviendas", md$ratio_viviendas[r], md$dias_panel[r])
    L[[length(L)+1]] <- mk(md$provincia_aeat[r], "alquiler",  md$ratio_alq_mes[r],  md$dias_panel[r])
    if ("ratio_dias" %in% names(md))
      L[[length(L)+1]] <- mk(md$provincia_aeat[r], "dias", md$ratio_dias[r], md$dias_panel[r])
  }
  mapa_div <- rbindlist(Filter(Negate(is.null), L))
}

## 7.8f CALADO (post-estratificacion) a los marginales AEAT: pesos paralelos
## w_cal = FACTORCAL x g_p con g_p = viviendas_aeat_p / viviendas_panel_p
## (acotado 0.4-2.5). NO altera las hojas principales: es el juego de pesos
## para la explotacion posterior con geografia coherente con la AEAT.
prov_calado <- NULL
if (!is.null(cprov_ub) && nrow(cprov_ub)) {
  gtab <- cprov_ub[, .(cpro2, g = pmin(pmax(viviendas_aeat / viviendas_panel,
                                            0.4), 2.5))]
  bloc <- base[uso == "tur" & grepl("^[0-9]{1,2}$", trimws(PROV_INM)) &
               suppressWarnings(as.integer(PROV_INM)) %in% 1:52]
  bloc[, cpro2 := sprintf("%02d", as.integer(PROV_INM))]
  bloc <- merge(bloc, gtab, by = "cpro2", all.x = TRUE)
  bloc[is.na(g), g := 1]
  bloc[, wcal := share * FACTORCAL * g]
  d_use2 <- if ("DIAS_RC" %in% names(bloc))
    fifelse(is.na(suppressWarnings(as.numeric(bloc$DIAS_RC))), bloc$dias_ef,
            pmin(suppressWarnings(as.numeric(bloc$DIAS_RC)), 365))
    else bloc$dias_ef
  bloc[, d2 := d_use2]
  prov_calado <- bloc[, {
    i_m <- !is.na(alq_mes); i_d <- !is.na(d2) & d2 > 0
    .(viviendas_calado = round(sum(wcal)),
      alq12_calado = if (any(i_m))
        round(sum(ing_viv[i_m] * FACTORCAL[i_m] * g[i_m]) /
              sum(FACTORCAL[i_m] * g[i_m]) / 12) else NA_real_,
      dias_calado = if (any(i_d))
        round(sum(d2[i_d] * FACTORCAL[i_d] * g[i_d]) /
              sum(FACTORCAL[i_d] * g[i_d])) else NA_real_)
  }, by = cpro2]
  prov_calado <- merge(prov_calado,
                       cprov_ub[, .(cpro2, provincia_aeat, viviendas_aeat,
                                    alq_mes_aeat, dias_aeat)],
                       by = "cpro2", all.x = TRUE)
  setorder(prov_calado, -viviendas_calado)
}

## 7.8g BOOTSTRAP (200 replicas, remuestreo de declarantes) del recuento
## provincial del No: IC 95%. Si el ratio 1,00 cae dentro del IC, la
## desviacion es varianza muestral; si cae fuera, sesgo real.
ic_boot <- NULL
ic_boot <- tryCatch({
  bl <- base[uso == "tur" & grepl("^[0-9]{1,2}$", trimws(PROV_INM)) &
             suppressWarnings(as.integer(PROV_INM)) %in% 1:52,
             .(IDENPER, cpro2 = sprintf("%02d", as.integer(PROV_INM)),
               v = share * FACTORCAL)]
  ids <- unique(bl$IDENPER)
  set.seed(123)
  B <- 200
  acc <- vector("list", B)
  for (b in seq_len(B)) {
    smp <- data.table(IDENPER = sample(ids, length(ids), replace = TRUE))
    acc[[b]] <- merge(smp, bl, by = "IDENPER", allow.cartesian = TRUE)[
      , .(vv = sum(v)), by = cpro2][, rep := b]
  }
  bb <- rbindlist(acc)
  res <- bb[, .(ic_low = round(quantile(vv, 0.025)),
                ic_high = round(quantile(vv, 0.975))), by = cpro2]
  res <- merge(res, cprov_ub[, .(cpro2, provincia_aeat, viviendas_panel,
                                 viviendas_aeat)], by = "cpro2", all.x = TRUE)
  res[, `:=`(ratio = round(viviendas_panel / viviendas_aeat, 2),
             ratio_ic_low = round(ic_low / viviendas_aeat, 2),
             ratio_ic_high = round(ic_high / viviendas_aeat, 2),
             veredicto = fifelse(viviendas_aeat >= ic_low & viviendas_aeat <= ic_high,
                                 "VARIANZA (1,00 dentro del IC)",
                                 "SESGO (1,00 fuera del IC)"))]
  setorder(res, -viviendas_panel)
  res[]
}, error = function(e) NULL)

## 7.9 Embudo y notas
embudo_df <- data.frame(Paso = names(embudo), Inmuebles = unlist(embudo),
                        row.names = NULL)

notas <- data.frame(Nota = Filter(Negate(is.null), c(
  "Fuente: Panel de Declarantes de IRPF (AEAT), fichero de rendimientos de capital inmobiliario. Extraccion: 01b_extraer_panel_sin_dependencias.py (2023), 01b_extraer_panel_2016.py (2016) y 01c_enriquecer_panel_2023.py (casillas adicionales del fichero 8).",
  "REFERENCIA AEAT: Estadistica de viviendas declaradas en IRPF 2023, Bloque I (por ubicacion del inmueble), tabla 'Rentabilidad y precios de alquiler por CCAA y provincia'. Vivienda habitual=No: 309.479 viviendas, 1.361 EUR/mes, 16,7 EUR/m2, 106 m2, 271 dias, valor de referencia medio 157.071, rentabilidad 6,2%. Vivienda habitual=Si: 2.409.689 viviendas, 657 EUR/mes, 339 dias.",
  "UNIDAD DE CONTEO: viviendas ENTERAS, una por referencia catastral, sin ponderar por dias. Se publican DOS recuentos: 'viviendas' (suma de share x FACTORCAL = estimacion poblacional del panel) y 'viviendas_sin_elevar' (suma de share, recuento muestral directo). CLAVE PARA COMPARAR CON LA AEAT: la Estadistica de viviendas de la AEAT es un RECUENTO DIRECTO de inmuebles declarados, NO una muestra elevada. Por eso la cifra comparable con las 309.479 / 2.409.689 es 'viviendas_sin_elevar', no la elevada por FACTORCAL (que multiplica por ~8-13 y produciria un falso exceso). El FACTORCAL solo se usa para estimaciones poblacionales propias (numero de arrendadores, importes medios), no para contrastar recuentos de viviendas con la AEAT.",
  "CLASIFICACION HABITUAL / NO HABITUAL (criterio VALIDADO con el dato 2023). HABITUAL = casilla 100 (marca de derecho a reduccion) o casilla 150 (importe de la reduccion 23.2) A SECAS, universo completo tras la casilla 65, sin filtro adicional: en 2023 da 2.423.187 viviendas elevadas frente a 2.409.689 de la AEAT (ratio 1,006), lo que confirma que la columna 'Vivienda habitual = Si' de la tabla AEAT es el arrendamiento PARA vivienda habitual del inquilino identificado por la reduccion. NO HABITUAL = arrendadas sin reduccion por no habitual (subsegmentos A-B), contrastadas con 'Vivienda habitual = No' (309.479). FUERA DE LA TABLA (excluidas del universo, con contador en Embudo_filtros): (i) mixtas con uso propio del dueno (c.76 > 0), que la FAQ AEAT clasifica como 'vivienda habitual con parte arrendada', NO como arrendada; (ii) arrendamiento anual barato sin reduccion de mas de DIAS_LARGA_DURACION dias (C largo) y sin dias (D); la franja C corta (271-300 dias: estudiantes, desplazados) SI entra al No habitual, como recoge la definicion AEAT, en su mayoria inmuebles no residenciales segun Catastro (garaje, trastero, local anual) que el cruce catastral de la AEAT aparta. En 2016 (fichero 4): reduccion = c.71; clave de uso = c.54; periodo = c.58; situacion = c.55.",
  "UNIDAD DE CONTEO Y EQUIVALENTES: la tabla AEAT de referencia (Bloque I) cuenta VIVIENDAS (unidad vivienda unica, calificada con el conjunto de declaraciones del censo y deduplicada por referencia catastral). El 'numero equivalente' (share x dias/365) existe pero pertenece al Bloque II y a los importes: el alquiler medio mensual AEAT se calcula elevando los ingresos a anio completo y dividiendo entre 12. El panel, al ser muestra, no puede deduplicar por referencia catastral entre declarantes no muestreados: cuenta suma de participaciones (share) elevada con FACTORCAL, lo que explica parte del deficit residual frente al censo (cotitulares fuera de muestra, atribucion de rentas).",
  "UNIVERSO Y FILTRO RESIDENCIAL: la AEAT solo estudia inmuebles de uso residencial segun Catastro, con valor catastral, en territorio comun. El fichero 8 trae TODOS los urbanos arrendados y su referencia catastral esta anonimizada, asi que el uso catastral no es observable. Se aplica un SCORE DE VIVIENDA por inmueble que combina: alquiler mensual elevado frente a los percentiles del alquiler HABITUAL de su provincia; suelo de renta equivalente a una vivienda pequena al EUR/m2 provincial de la AEAT; valor catastral (mejor disponible entre casillas 83 y 123) dentro de la banda de vivienda de su provincia; amortizacion del inmueble (casilla 131, valor de construccion implicito); e IBI (casilla 115). Cada senal ausente simplemente no participa. Los candidatos de filtro (percentiles, suelo m2 y cortes del score) se calibran contra los anclajes nacionales AEAT y se elige el de menor desviacion conjunta (hoja Calibracion_AEAT); la hoja Score_distribucion muestra que se excluye en cada tramo del score.",
  "El patron de 'precio y valor de vivienda' de cada provincia se estima sobre el propio segmento HABITUAL (casi todo lo que disfruta la reduccion del art. 23.2 LIRPF es una vivienda), con fallback nacional en provincias con poca muestra.",
  "ALQUILER MEDIO MENSUAL (definicion AEAT, columna alquiler_mes): ingresos de la vivienda elevados al 100% de titularidad y a 365 dias, divididos entre 12; media por vivienda. Al elevar a anio completo, los alquileres de pocos dias pesan como si duraran todo el anio.",
  "DISENO DE INVESTIGACION: la hoja Diseno_investigacion mapea cada modulo del documento 'diseño_investigacion_alquiler_no habitual' con las hojas que lo responden y sus limitaciones. La MODALIDAD 'no habitual (proxy)' de las hojas M1-M4 son los subsegmentos heuristicos A (intensivo) y B (estacional); NO es una medicion del alquiler no habitual, que el IRPF no declara.",
  "MODULO 4 (renta de los arrendadores): la renta bruta total del declarante es la suma M1+M2+M3+M4+M5+M6 del fichero 2_Renta del propio panel (trabajo, capital mobiliario, arrendamientos, actividades economicas, ganancias y otras rentas), anadida al panel por 01c_enriquecer_panel.py. El peso del alquiler sobre la renta usa los ingresos integros del fichero 8 en el numerador y se topa al 150%.",
  "MODULO 1 (municipios): la hoja M1_Municipios publica solo los municipios de RESIDENCIA con al menos 30 observaciones muestrales en alguna modalidad, identificados por los codigos INE CPRO+CMUN. La proporcion sobre el parque total de viviendas requiere el Censo del INE (denominador externo al panel).",
  "SUBSEGMENTACION DEL NO HABITUAL (hoja Subsegmentos_no_habitual): HEURISTICA PROPIA, no existe en la estadistica AEAT ni en el IRPF. 'Intensivo (no habitual probable)': precio por dia efectivo >= 2 veces el del alquiler habitual de su provincia. 'Estacional no intensivo': <= 270 dias. 'Anual no intensivo': resto (habitaciones, alquiler a empresa, larga duracion sin reduccion). Los umbrales son parametros (SUBSEG_*) y las columnas pct_con_suministros y pct_contrato_de_anios_previos ayudan a validar la particion, pero NINGUNA cifra de esta hoja debe presentarse como 'alquiler no habitual' medido.",
  "RENTABILIDAD: la AEAT publica la rentabilidad bruta sobre VALOR DE REFERENCIA de Catastro (6,2% en el no habitual), que no viene en el panel. La rentab_vcat de este informe usa el mejor valor catastral declarado (casillas 83/123) y NO es comparable (el VC es muy inferior al VR). Tampoco son replicables m2 medios ni alquiler/m2.",
  "SITUACION (casilla 65): si el panel trae la columna SITUACION, se excluyen las claves 2 (Pais Vasco/Navarra), 3 (sin referencia catastral) y 4 (extranjero), que la AEAT no puede cruzar con Catastro.",
  "Uso 'hab': marca de la casilla 100 o importe en la 150 (reduccion del art. 23.2 LIRPF), mas los mixtos reclasificados. Uso 'tur': resto de arrendamientos urbanos; equivale a la vista 'Vivienda habitual = No' de la AEAT y NO es sinonimo de alquiler no habitual.",
  "Ingresos medios y mediana: ingresos integros anuales (casilla 102) de la vivienda completa, sin elevar. Rendimiento neto: casilla 149.",
  paste0("Dias: casilla 101, topados a ", DIAS_TOPE,
         " por la consolidacion de contratos sucesivos. Dias medios = media por vivienda. LIMITE en el NO habitual: la AEAT une los periodos declarados por TODOS los titulares de cada vivienda (censo por referencia catastral); el panel solo observa los contratos del declarante muestreado, por lo que subestima los dias en viviendas con cotitulares o alta rotacion (229 vs 271 dias, ratio 0,84). Afecta poco al habitual (contrato anual unico: 340 vs 339). No es corregible con filtros sin romper el recuento. La columna dias_censal_RC del contraste (si el panel trae DIAS_RC del 01d) agrega la casilla 101 de TODOS los declarantes de cada referencia catastral, topada a 365: es la vista censal del inmueble, la misma agregacion que hace la AEAT, y la metrica de dias recomendada cuando este disponible."),
  paste0("Multiarrendador: ", NIVEL_ARRENDADOR, " con ", UMBRAL_MULTI,
         " o mas viviendas en alquiler dentro del universo filtrado."),
  "Un inmueble con varios contratos genera varios registros; el extractor los consolida por (declarante, referencia catastral).",
  "GEOGRAFIA: provincia y capital corresponden a la RESIDENCIA DEL ARRENDADOR. La tabla AEAT de referencia es por UBICACION DEL INMUEBLE: solo el total nacional es estrictamente comparable. La hoja Contraste_provincial enfrenta ambas opticas a titulo orientativo; en provincias no habituales (residentes de Madrid con vivienda en la costa, etc.) el desajuste es estructural, no un error.",
  "El panel no cubre Pais Vasco ni Navarra (regimenes forales), igual que la estadistica de la AEAT.",
  "Las celdas con menos de 30 registros muestrales se marcan como 'muestra reducida'. PRECISION: se publican ee_ingresos, cv_ingresos_pct y n_efectivo (Kish); con cv > 16,6% la celda se marca poco fiable (criterio INE). El error estandar no incorpora el diseno complejo del panel: es un minimo.",
  paste0("pct_ing_bajo: peso de los arrendamientos con ingreso anual de la vivienda completa inferior a ", UMBRAL_ING_BAJO, " euros (proxy de garajes/trasteros que el filtro no haya apartado)."),
  paste0("Comparacion temporal: importes deflactados a euros de ", ANIO_REF,
         " con el IPC general (INE). Factor aplicado a ", ANIO_COMP, ": ",
         DEFLACTOR[as.character(ANIO_COMP)], "."),
  "EVOLUCION 2016-2023: SIN filtro residencial (universo completo del fichero 8 en ambos anios), porque 2016 no trae valor catastral, dias reales ni las casillas del score. Sus cifras no coinciden con el retrato 2023 filtrado.",
  "AVISO IMPORTANTE sobre la evolucion habitual/no habitual: en 2023 la clasificacion sigue la definicion AEAT (habitual = uso propio como vivienda habitual, casilla 76), mientras que en 2016 el panel no trae la casilla 76 y se clasifica por la reduccion (casilla 71), que NO es lo mismo. Por eso el reparto habitual/no habitual NO es comparable entre ambos anios: lo comparable es el TOTAL de viviendas arrendadas, los ingresos, el rendimiento neto y la estructura uni/multiarrendador. Para una serie homogenea del eje habitual/no habitual habria que reextraer 2016 con la casilla equivalente al uso propio, que ese diseno no publica.",
  if (hay_comp && !dias_comparables)
    paste0("LIMITACION de la comparacion ", ANIO_COMP, "-", ANIO_REF,
           ": en ", ANIO_COMP, " el panel no publica los dias de arrendamiento ",
           "(se estiman de la casilla 58, periodo computable). Dias y euros/dia ",
           "NO se comparan entre anios. Si son comparables: viviendas, ingresos, ",
           "rendimiento neto y estructura uni/multi.")
  else NULL
)))

claves_meta <- c("anio", "generado", "diseno_verificado", "modo_factorcal",
                 "modo_importes", "declarantes_muestra",
                 "declarantes_poblacion_estimada", "tasa_marca_75",
                 "factor_medio", "viviendas_elevadas",
                 "ingreso_medio_por_vivienda", "filas_salida")
if (exists("pct_c93"))
  notas <- c(notas, sprintf("CASILLA 93 (fecha de contrato): informada en el %.1f%% de los arrendamientos sin reduccion de %s (dato calculado en esta corrida).", pct_c93, ANIO_REF))

metadatos <- do.call(rbind, lapply(names(datos), function(a) {
  m <- datos[[a]]$meta
  data.frame(ejercicio = a, parametro = claves_meta,
             valor = sapply(claves_meta, function(k) {
               v <- m[[k]]; if (is.null(v)) NA_character_ else as.character(v)
             }), row.names = NULL)
}))
metadatos <- rbind(metadatos,
                   data.frame(ejercicio = as.character(ANIO_REF),
                              parametro = c("filtro_residencial_aplicado",
                                            "senales_score_disponibles"),
                              valor = c(FILTRO_ELEGIDO,
                                        paste(names(senales_disp)[unlist(senales_disp)],
                                              collapse = " | ")),
                              row.names = NULL))

hojas <- list(
  "Diseno_investigacion"   = as.data.frame(diseno_map),
  "Nacional"               = as.data.frame(nac),
  "Contraste_AEAT"         = as.data.frame(contraste),
  "Contraste_recuentos"    = as.data.frame(contraste_elevado),
  "Contraste_prov_ubicacion" = tryCatch(as.data.frame(cprov_ub), error = function(e) NULL),
  "Prov_ubicacion_calado"    = tryCatch(as.data.frame(prov_calado), error = function(e) NULL),
  "IC_bootstrap_prov"        = tryCatch(as.data.frame(ic_boot), error = function(e) NULL),
  "Mapa_divergencias"        = tryCatch(as.data.frame(mapa_div), error = function(e) NULL),
  "Contraste_CCAA_ubicacion" = tryCatch(as.data.frame(cccaa_ub), error = function(e) NULL),
  "Contraste_provincial"   = as.data.frame(cprov),
  "M1_Modalidades"         = as.data.frame(m1_nac),
  "M1_Modalidades_prov"    = as.data.frame(m1_prov),
  "M1_Municipios"          = as.data.frame(m1_muni),
  "M2_Declarantes"         = as.data.frame(m2_decl),
  "M3_Estructura"          = as.data.frame(m3_estr),
  "M3_Estructura_prov"     = as.data.frame(m3_prov),
  "M4_Renta_declarantes"   = as.data.frame(m4_rent),
  "Usos"                   = as.data.frame(seg),
  "Subsegmentos_no_habitual" = as.data.frame(subseg),
  "Tramos_dias"            = as.data.frame(tramos),
  "Calibracion_AEAT"       = as.data.frame(cal$tabla),
  "Score_distribucion"     = as.data.frame(score_dist),
  "Embudo_no_habitual"     = as.data.frame(embudo_nh),
  "Uso_predominante"       = as.data.frame(uso_predom),
  "Dias_universo_VC"       = as.data.frame(ct_aeat),
  "Contraste_por_casero"   = contraste_casero,
  "Embudo_directo"         = tryCatch(as.data.frame(get0("embudo_directo")),
                                      error = function(e) NULL),
  "Desglose_brecha_AEAT"   = as.data.frame(desglose),
  "Diagnostico_VC"         = as.data.frame(diag_vc),
  "Provincias"             = as.data.frame(prov),
  "Capitales"              = as.data.frame(cap),
  "Desglose_n_viviendas"   = as.data.frame(det),
  "Concentracion"          = as.data.frame(conc),
  "Embudo_filtros"         = embudo_df,
  "Metadatos"              = metadatos,
  "Notas"                  = notas
)

# --- 8. EVOLUCION ENTRE EJERCICIOS --------------------------------------------------
# Universo COMPLETO en ambos anios (2016 no permite el filtro residencial).
graficos <- character(0)
if (hay_comp) {
  message("Calculando la evolución ", ANIO_COMP, " -> ", ANIO_REF, "...")
  defl <- function(x, anio) x * DEFLACTOR[as.character(anio)]

  fin_full <- lapply(datos, function(d) finalizar(d$base))

  ## 8.1 Nacional por uso
  ev_uso <- rbindlist(lapply(names(datos), function(a) {
    fu <- fin_full[[a]]
    r <- resumen(fu$base, "uso")
    r[, `:=`(anio = as.integer(a),
             arrendadores = sum(fu$pers$peso),
             ingresos_reales = defl(ingresos_medios, a))]
    r[]
  }), fill = TRUE)
  setorder(ev_uso, uso, anio)

  evolucion_uso <- dcast(ev_uso, uso ~ anio,
                         value.var = c("viviendas", "ingresos_medios",
                                       "ingresos_reales", "dias_medios",
                                       "euros_dia", "rend_neto_medio"))
  cr <- as.character(ANIO_REF); cc <- as.character(ANIO_COMP)
  evolucion_uso[, `:=`(
    var_viviendas_pct = round(pct_var(get(paste0("viviendas_", cr)),
                                      get(paste0("viviendas_", cc))), 1),
    var_ingresos_nominal_pct = round(pct_var(get(paste0("ingresos_medios_", cr)),
                                             get(paste0("ingresos_medios_", cc))), 1),
    var_ingresos_real_pct = round(pct_var(get(paste0("ingresos_reales_", cr)),
                                          get(paste0("ingresos_reales_", cc))), 1))]
  if (dias_comparables) {
    evolucion_uso[, `:=`(
      var_dias_pct = round(pct_var(get(paste0("dias_medios_", cr)),
                                   get(paste0("dias_medios_", cc))), 1),
      var_euros_dia_pct = round(pct_var(get(paste0("euros_dia_", cr)),
                                        get(paste0("euros_dia_", cc))), 1))]
  } else {
    quitar <- grep("^(dias_medios|euros_dia)_", names(evolucion_uso), value = TRUE)
    if (length(quitar)) evolucion_uso[, (quitar) := NULL]
    evolucion_uso[, nota := paste0("dias no comparables entre ", cc, " y ", cr,
                                   ": ", cc, " usa el periodo computable")]
  }
  numv <- setdiff(names(evolucion_uso)[sapply(evolucion_uso, is.numeric)], "uso")
  evolucion_uso[, (numv) := lapply(.SD, function(x) round(x, 1)), .SDcols = numv]

  ## 8.2 Nacional por segmento
  ev_seg <- rbindlist(lapply(names(datos), function(a) {
    r <- resumen(fin_full[[a]]$base, "segmento")
    r[, `:=`(anio = as.integer(a), ingresos_reales = defl(ingresos_medios, a))][]
  }), fill = TRUE)
  evolucion_seg <- dcast(ev_seg, segmento ~ anio,
                         value.var = c("viviendas", "ingresos_reales",
                                       "dias_medios", "euros_dia"))
  evolucion_seg[, `:=`(
    var_viviendas_pct = round(pct_var(get(paste0("viviendas_", cr)),
                                      get(paste0("viviendas_", cc))), 1),
    var_ingresos_real_pct = round(pct_var(get(paste0("ingresos_reales_", cr)),
                                          get(paste0("ingresos_reales_", cc))), 1))]
  if (dias_comparables) {
    evolucion_seg[, var_euros_dia_pct :=
                    round(pct_var(get(paste0("euros_dia_", cr)),
                                  get(paste0("euros_dia_", cc))), 1)]
  } else {
    quitar <- grep("^(dias_medios|euros_dia)_", names(evolucion_seg), value = TRUE)
    if (length(quitar)) evolucion_seg[, (quitar) := NULL]
  }
  nums2 <- names(evolucion_seg)[sapply(evolucion_seg, is.numeric)]
  evolucion_seg[, (nums2) := lapply(.SD, function(x) round(x, 1)), .SDcols = nums2]

  ## 8.3 Estructura de la propiedad (uni/multi) por anio
  ev_clase <- rbindlist(lapply(names(datos), function(a) {
    fu <- fin_full[[a]]
    p <- fu$pers[, .(arrendadores = sum(peso)), by = clase]
    v <- fu$base[, .(viviendas = sum(share * FACTORCAL)), by = clase]
    m <- merge(p, v, by = "clase")
    m[, `:=`(anio = as.integer(a),
             pct_arrendadores = 100 * arrendadores / sum(arrendadores),
             pct_viviendas = 100 * viviendas / sum(viviendas))][]
  }), fill = TRUE)
  evolucion_clase <- dcast(ev_clase, clase ~ anio,
                           value.var = c("arrendadores", "viviendas",
                                         "pct_arrendadores", "pct_viviendas"))
  numc <- names(evolucion_clase)[sapply(evolucion_clase, is.numeric)]
  evolucion_clase[, (numc) := lapply(.SD, function(x) round(x, 1)), .SDcols = numc]

  ## 8.4 Provincias
  ev_prov <- rbindlist(lapply(names(datos), function(a) {
    r <- resumen(fin_full[[a]]$base, c("provincia", "uso"))
    r[, `:=`(anio = as.integer(a), ingresos_reales = defl(ingresos_medios, a))][]
  }), fill = TRUE)
  evolucion_prov <- dcast(ev_prov, provincia ~ uso + anio,
                          value.var = c("viviendas", "ingresos_reales"))
  for (u in c("hab", "tur")) {
    vr <- paste0("viviendas_", u, "_", cr); vc <- paste0("viviendas_", u, "_", cc)
    ir <- paste0("ingresos_reales_", u, "_", cr)
    ic <- paste0("ingresos_reales_", u, "_", cc)
    if (all(c(vr, vc) %in% names(evolucion_prov)))
      evolucion_prov[, (paste0("var_viviendas_", u, "_pct")) :=
                       round(pct_var(get(vr), get(vc)), 1)]
    if (all(c(ir, ic) %in% names(evolucion_prov)))
      evolucion_prov[, (paste0("var_ingresos_real_", u, "_pct")) :=
                       round(pct_var(get(ir), get(ic)), 1)]
  }
  nump <- names(evolucion_prov)[sapply(evolucion_prov, is.numeric)]
  evolucion_prov[, (nump) := lapply(.SD, function(x) round(x, 1)), .SDcols = nump]
  setorderv(evolucion_prov, paste0("viviendas_tur_", cr), -1, na.last = TRUE)

  ## 8.5 [M5] Evolucion por DECLARANTE: ingresos percibidos y renta bruta
  media_renta <- function(rt, w) {
    ok <- !is.na(rt)
    if (!any(ok)) return(NA_real_)
    sum(rt[ok] * w[ok]) / sum(w[ok])
  }
  ev_decl <- rbindlist(lapply(names(datos), function(a) {
    p <- fin_full[[a]]$pers
    rbind(
      p[tiene_hab == TRUE,
        .(grupo = "Con alquiler habitual", anio = as.integer(a),
          declarantes = round(sum(peso)),
          ingresos_alq_medios = sum(ing_hab * peso) / sum(peso),
          renta_bruta_media = media_renta(renta_total, peso))],
      p[tiene_tur == TRUE,
        .(grupo = "Con alquiler no habitual", anio = as.integer(a),
          declarantes = round(sum(peso)),
          ingresos_alq_medios = sum(ing_tur * peso) / sum(peso),
          renta_bruta_media = media_renta(renta_total, peso))])
  }), fill = TRUE)
  ev_decl[, `:=`(ingresos_alq_reales = defl(ingresos_alq_medios, anio),
                 renta_bruta_real = defl(renta_bruta_media, anio)), by = anio]
  evolucion_decl <- dcast(ev_decl, grupo ~ anio,
                          value.var = c("declarantes", "ingresos_alq_medios",
                                        "ingresos_alq_reales",
                                        "renta_bruta_media", "renta_bruta_real"))
  for (v in c("declarantes", "ingresos_alq_reales", "renta_bruta_real")) {
    a <- paste0(v, "_", cr); b <- paste0(v, "_", cc)
    if (all(c(a, b) %in% names(evolucion_decl)))
      evolucion_decl[, (paste0("var_", v, "_pct")) :=
                       round(pct_var(get(a), get(b)), 1)]
  }
  numdec <- names(evolucion_decl)[sapply(evolucion_decl, is.numeric)]
  evolucion_decl[, (numdec) := lapply(.SD, function(x) round(x, 1)),
                 .SDcols = numdec]

  hojas <- c(hojas, list(
    "Evol_por_uso"      = as.data.frame(evolucion_uso),
    "Evol_por_segmento" = as.data.frame(evolucion_seg),
    "Evol_estructura"   = as.data.frame(evolucion_clase),
    "Evol_provincias"   = as.data.frame(evolucion_prov),
    "Evol_declarantes"  = as.data.frame(evolucion_decl)))
}

# --- 9b. HOJAS DEL PLIEGO (M1..M5) ------------------------------------------
# Construidas exactamente segun las variables solicitadas, con terminologia
# unica: "Vivienda habitual" / "Vivienda no habitual".
# media ponderada segura: NA si no hay dato (evita NaN -> #NUM! en Excel)
mpond <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(x) & is.finite(w)
  if (!any(ok) || sum(w[ok]) <= 0) NA_real_ else round(sum(x[ok] * w[ok]) / sum(w[ok]))
}
d_use_de <- function(b) {
  if ("DIAS_RC" %in% names(b)) {
    x <- suppressWarnings(as.numeric(b$DIAS_RC))
    fifelse(is.na(x), b$dias_ef, pmin(x, 365))
  } else b$dias_ef
}
M1N <- tryCatch(local({
  bu <- copy(base); bu[, d2 := d_use_de(bu)]
  bloq <- function(bb, by_cols, nivel) {
    r <- bb[, .(viviendas_habitual = round(sum(share[uso == "hab"] * FACTORCAL[uso == "hab"])),
                viviendas_no_habitual = round(sum(share[uso == "tur"] * FACTORCAL[uso == "tur"])),
                n_muestra = .N), by = by_cols]
    r[, `:=`(nivel = nivel,
             ratio_no_habitual_sobre_habitual = round(viviendas_no_habitual /
                                                      pmax(viviendas_habitual, 1), 3))]
    tot <- r$viviendas_habitual + r$viviendas_no_habitual
    r[, `:=`(pct_habitual = round(100 * viviendas_habitual / pmax(tot, 1), 1),
             pct_no_habitual = round(100 * viviendas_no_habitual / pmax(tot, 1), 1))]
    r
  }
  p <- NULL
  if ("PROV_INM" %in% names(bu)) {
    bp <- bu[grepl("^[0-9]{1,2}$", trimws(PROV_INM)) &
             suppressWarnings(as.integer(PROV_INM)) %in% 1:52]
    bp[, codigo := sprintf("%02d", as.integer(PROV_INM))]
    p <- bloq(bp, "codigo", "provincia (ubicacion del inmueble)")
  }
  m <- NULL
  if (all(c("cpro2", "CMUN") %in% names(bu))) {
    bm <- bu[!is.na(CMUN) & CMUN != ""]
    bm[, codigo := paste0(cpro2, CMUN)]
    m <- bloq(bm, "codigo", "municipio (residencia del declarante)")
    m <- m[n_muestra >= 30]
  }
  out <- rbindlist(list(p, m), fill = TRUE)
  setcolorder(out, c("nivel", "codigo"))
  setorderv(out, c("nivel", "viviendas_no_habitual"), c(1, -1))
  out[]
}), error = function(e) NULL)

M2N <- tryCatch(local({
  bu <- copy(base); bu[, d2 := d_use_de(bu)]
  bu[, prov_t := fifelse(grepl("^[0-9]{1,2}$", trimws(as.character(PROV_INM))) &
                         suppressWarnings(as.integer(PROV_INM)) %in% 1:52,
                         sprintf("%02d", suppressWarnings(as.integer(PROV_INM))),
                         NA_character_)]
  mk <- function(bb, cod) {
    dec <- bb[, .(ing_decl = sum(suppressWarnings(as.numeric(INGRESOS_102)),
                                 na.rm = TRUE), w = FACTORCAL[1]),
              by = .(IDENPER, uso)]
    a <- dec[, .(ingresos_anuales_medios_por_declarante =
                   round(sum(ing_decl * w) / sum(w)),
                 declarantes = round(sum(w))), by = uso]
    d <- bb[!is.na(d2) & d2 > 0,
            .(dias_medios = round(sum(d2 * FACTORCAL) / sum(FACTORCAL))), by = uso]
    r <- merge(a, d, by = "uso", all = TRUE)
    r[, territorio := cod]
    r
  }
  nac <- mk(bu, "NACIONAL")
  pr <- rbindlist(lapply(split(bu[!is.na(prov_t)], by = "prov_t"),
                         function(x) mk(x, x$prov_t[1])), fill = TRUE)
  r <- rbind(nac, pr, fill = TRUE)
  r[, modalidad := fifelse(uso == "hab", "Vivienda habitual", "Vivienda no habitual")]
  r[, uso := NULL]
  wI <- dcast(r, territorio ~ modalidad,
              value.var = c("ingresos_anuales_medios_por_declarante",
                            "dias_medios", "declarantes"))
  setnames(wI, names(wI), gsub("_Vivienda habitual", "_habitual", names(wI)))
  setnames(wI, names(wI), gsub("_Vivienda no habitual", "_no_habitual", names(wI)))
  wI[, diferencial_ingresos_pct := round(100 *
       (ingresos_anuales_medios_por_declarante_no_habitual /
        ingresos_anuales_medios_por_declarante_habitual - 1), 1)]
  setorder(wI, -declarantes_no_habitual)
  wI[]
}), error = function(e) NULL)

M3N <- tryCatch(local({
  # ing_nh = ingresos integros (c.102, parte del declarante) SOLO de sus
  # viviendas NO HABITUALES: el reparto por categoria mide que porcentaje de
  # los ingresos del alquiler no habitual concentra cada tipo de arrendador.
  # Uni/multiarrendador se define por el TOTAL de viviendas en alquiler del
  # declarante, sea cual sea el regimen (habitual o no habitual): multi = 2 o
  # mas viviendas alquiladas. El analisis se refiere a quienes tienen al menos
  # una vivienda no habitual.
  tot <- base[, .(n_total_alquiler = .N,          # cartera (recuento) -> clasifica
                  n_no_habitual = sum(uso == "tur"),
                  # equivalentes ponderados por participacion: SUMAN el total
                  eq_total = sum(share),
                  eq_no_habitual = sum(share[uso == "tur"]),
                  ing_nh = sum(suppressWarnings(as.numeric(INGRESOS_102))[uso == "tur"],
                               na.rm = TRUE),
                  w = FACTORCAL[1], provincia = provincia[1]), by = IDENPER]
  nh <- tot[n_no_habitual > 0]
  if (!nrow(nh)) return(NULL)
  nh[, categoria_detalle := as.character(cut(n_total_alquiler, BINS_CARTERA,
                                             ETIQ_CARTERA))]
  nh[, grupo := fifelse(n_total_alquiler == 1,
                        "Uniarrendador (1 vivienda en alquiler)",
                        "Multiarrendador (2 o mas viviendas en alquiler)")]
  blq <- function(x, terr, by_col) {
    ing_tot <- sum(x$ing_nh * x$w)
    r <- x[, .(declarantes = round(sum(w)),
               viviendas_en_alquiler = round(sum(eq_total * w)),
               viviendas_no_habitual = round(sum(eq_no_habitual * w)),
               pct_ingresos_de_alquiler_no_habitual = round(100 * sum(ing_nh * w) /
                                                pmax(ing_tot, 1), 1)),
           by = by_col]
    setnames(r, by_col, "categoria")
    r[, territorio := terr][]
  }
  out <- rbind(
    blq(nh, "NACIONAL", "grupo"),
    blq(nh, "NACIONAL", "categoria_detalle"),
    rbindlist(lapply(split(nh, by = "provincia"),
                     function(x) blq(x, x$provincia[1], "grupo")), fill = TRUE),
    fill = TRUE)
  setcolorder(out, c("territorio", "categoria"))
  out[]
}), error = function(e) NULL)

M4N <- tryCatch(local({
  if (!"renta_total" %in% names(pers)) NULL else {
    pp <- copy(pers)
    blq <- function(mask, modalidad, terr_col = NULL) {
      p <- pp[mask & !is.na(renta_total)]
      if (!nrow(p)) return(NULL)
      by_cols <- c("tramo_renta", terr_col)
      r <- p[, .(declarantes = round(sum(peso)),
                 renta_bruta_media = round(sum(renta_total * peso) / sum(peso)),
                 peso_ingresos_alquiler_sobre_renta_pct =
                   round(sum(peso_alq_renta * peso, na.rm = TRUE) /
                         sum(peso[!is.na(peso_alq_renta)]), 1)),
             by = by_cols]
      r[, modalidad := modalidad]
      if (is.null(terr_col)) r[, territorio := "NACIONAL"]
      else setnames(r, terr_col, "territorio")
      r
    }
    # uni/multi por TOTAL de viviendas en alquiler (n_viv), no solo las no habituales
    dist <- pp[tiene_tur == TRUE,
               .(declarantes = round(sum(peso))),
               by = .(grupo = fifelse(n_viv <= 1,
                                      "Uniarrendador (1 vivienda en alquiler)",
                                      "Multiarrendador (2 o mas viviendas en alquiler)"))]
    rmg <- pp[tiene_tur == TRUE, .(renta_bruta_media = mpond(renta_total, peso),
                                   pesoalq = mpond(peso_alq_renta, peso)),
              by = .(grupo = fifelse(n_viv <= 1,
                                     "Uniarrendador (1 vivienda en alquiler)",
                                     "Multiarrendador (2 o mas viviendas en alquiler)"))]
    dist <- merge(dist, rmg, by = "grupo", all.x = TRUE)
    dist[, `:=`(tramo_renta = "TODOS",
                modalidad = "Vivienda no habitual (uni/multi, cartera total)",
                territorio = "NACIONAL",
                pct_declarantes = round(100 * declarantes / sum(declarantes), 1),
                peso_ingresos_alquiler_sobre_renta_pct = pesoalq)]
    dist[, pesoalq := NULL]
    setnames(dist, "grupo", "detalle")
    blq_tot <- function(mask, modalidad) {
      p <- pp[mask & !is.na(renta_total)]
      if (!nrow(p)) return(NULL)
      data.table(tramo_renta = "TODOS", territorio = "NACIONAL",
                 modalidad = modalidad,
                 declarantes = round(sum(p$peso)),
                 renta_bruta_media = round(sum(p$renta_total * p$peso) / sum(p$peso)),
                 peso_ingresos_alquiler_sobre_renta_pct =
                   round(sum(p$peso_alq_renta * p$peso, na.rm = TRUE) /
                         sum(p$peso[!is.na(p$peso_alq_renta)]), 1))
    }
    r <- rbind(
      blq_tot(pp$tiene_hab == TRUE, "Vivienda habitual"),
      blq_tot(pp$tiene_tur == TRUE, "Vivienda no habitual"),
      blq(pp$tiene_hab == TRUE, "Vivienda habitual"),
      blq(pp$tiene_tur == TRUE, "Vivienda no habitual"),
      blq(pp$tiene_hab == TRUE, "Vivienda habitual", "provincia"),
      blq(pp$tiene_tur == TRUE, "Vivienda no habitual", "provincia"),
      fill = TRUE)
    r[, detalle := NA_character_]
    out <- rbind(r, dist, fill = TRUE)
    setcolorder(out, c("modalidad", "territorio", "tramo_renta"))
    out[]
  }
}), error = function(e) NULL)

M5N <- tryCatch(local({
  # Series construidas sobre la MISMA base que el resto del informe: para el
  # ejercicio de referencia, la base validada (coincide con Validacion_AEAT);
  # para el ejercicio de comparacion, su base con el filtro residencial
  # equivalente cuando el diseno lo permite. La columna 'universo' lo indica.
  fin_M5 <- list(); univ <- character(0)
  fin_M5[[as.character(ANIO_REF)]] <- fin_ref
  univ[as.character(ANIO_REF)] <- paste0("validado (", sub(" .*$", "", FILTRO_ELEGIDO), ")")
  for (a in setdiff(names(datos), as.character(ANIO_REF))) {
    bf <- datos[[a]]$base
    m <- tryCatch({
      ca <- calibrar(bf); fi <- elegir_filtro(ca)
      list(mask = ca$mascaras[[fi]], et = paste0("validado (", sub(" .*$", "", fi), ")"))
    }, error = function(e) NULL)
    # Alternativa cuando el diseno del anio no trae valor catastral en el IRPF:
    # perimetro CATASTRAL con el modulo inmobiliario de ese mismo anio (01d),
    # equivalente al candado de vivienda de 2023 (uso residencial + ingreso
    # minimo de vivienda). Requiere haber pasado 01d sobre la carpeta del anio.
    if (is.null(m) && any(c("URBACLAVE_RC", "VIV_RC") %in% names(bf))) {
      m <- tryCatch({
        urb <- if ("URBACLAVE_RC" %in% names(bf))
          toupper(trimws(as.character(bf$URBACLAVE_RC))) else rep("", nrow(bf))
        vrn <- if ("VIV_RC" %in% names(bf))
          suppressWarnings(as.numeric(bf$VIV_RC)) else rep(NA_real_, nrow(bf))
        ing <- suppressWarnings(as.numeric(bf$INGRESOS_102))
        sh  <- suppressWarnings(as.numeric(bf$share)); sh[is.na(sh) | sh <= 0] <- 1
        # respaldo de cobertura: RC sin ficha de caracteristicas pero con valor
        # catastral en el fichero de titularidades (INM_PR)
        vcp <- if ("VALCAT_PR" %in% names(bf))
          suppressWarnings(as.numeric(bf$VALCAT_PR)) else rep(NA_real_, nrow(bf))
        sin_ficha <- is.na(vrn) & urb == ""
        ok <- (((!is.na(vrn) & vrn >= 1) | urb == "V") |
               (sin_ficha & !is.na(vcp) & vcp > 0)) &
              !(urb != "" & urb != "V") &
              (is.na(ing) | ing / sh >= ING_GARAJE_MAX)
        if (!any(ok)) stop("sin cruce catastral")
        list(mask = ok, et = "validado (perimetro catastral, modulo inmobiliario)")
      }, error = function(e) NULL)
    }
    if (is.null(m)) {
      bb <- bf
      et <- paste0("completo (sin filtro residencial: ni valor catastral en el ",
                   "IRPF ni modulo inmobiliario cruzado; ejecuta 01d sobre la ",
                   "carpeta de ese ejercicio)")
    } else {
      bb <- bf[uso == "hab" | m$mask]; et <- m$et
    }
    fin_M5[[a]] <- tryCatch(finalizar(bb), error = function(e) NULL)
    univ[a] <- et
  }
  fin_M5 <- Filter(Negate(is.null), fin_M5)
  if (!length(fin_M5)) NULL else {
    yr <- function(a) {
      fb <- fin_M5[[a]]$base; fp <- fin_M5[[a]]$pers
      # Uni/multi por CARTERA TOTAL de inmuebles arrendados del declarante
      # (mismo criterio que M3 y M4): multi = 2 o mas viviendas en alquiler,
      # sea cual sea su regimen. El analisis se restringe a declarantes con
      # al menos una vivienda no habitual.
      nh <- fb[, .(n_tot = .N, n_nh = sum(uso == "tur"),
                   ing_nh = sum(suppressWarnings(as.numeric(INGRESOS_102))[uso == "tur"],
                                na.rm = TRUE), w = FACTORCAL[1]),
               by = IDENPER][n_nh > 0]
      cob <- if (any(c("VIV_RC", "URBACLAVE_RC", "VALCAT_PR") %in% names(fb))) {
        vr <- if ("VIV_RC" %in% names(fb)) !is.na(suppressWarnings(as.numeric(fb$VIV_RC))) else FALSE
        uc <- if ("URBACLAVE_RC" %in% names(fb)) trimws(as.character(fb$URBACLAVE_RC)) != "" else FALSE
        vp <- if ("VALCAT_PR" %in% names(fb)) !is.na(suppressWarnings(as.numeric(fb$VALCAT_PR))) else FALSE
        round(100 * mean(vr | uc | vp), 1)
      } else NA_real_
      data.table(
        anio = as.integer(a),
        universo = unname(univ[a]),
        cobertura_catastral_pct = cob,
        viviendas_no_habitual = fb[uso == "tur", round(sum(share * FACTORCAL))],
        viviendas_habitual = fb[uso == "hab", round(sum(share * FACTORCAL))],
        ingresos_medios_declarante_habitual =
          fp[tiene_hab == TRUE, mpond(ing_hab, peso)],
        ingresos_medios_declarante_no_habitual =
          fp[tiene_tur == TRUE, mpond(ing_tur, peso)],
        declarantes_uniarrendador_nh = nh[n_tot == 1, round(sum(w))],
        declarantes_multiarrendador_nh = nh[n_tot >= 2, round(sum(w))],
        cuota_ingresos_multi_pct = nh[, round(100 * sum(ing_nh[n_tot >= 2] * w[n_tot >= 2]) /
                                              sum(ing_nh * w), 1)],
        renta_media_decl_habitual = if ("renta_total" %in% names(fp))
          fp[tiene_hab == TRUE, mpond(renta_total, peso)] else NA_real_,
        renta_media_decl_no_habitual = if ("renta_total" %in% names(fp))
          fp[tiene_tur == TRUE, mpond(renta_total, peso)] else NA_real_)
    }
    nac <- rbindlist(lapply(names(fin_M5), yr))
    for (a in names(univ))
      message(sprintf("  [M5] perimetro %s: %s", a, univ[a]))
    comparables <- all(grepl("^validado", unname(univ[names(fin_M5)])))
    if (!comparables) {
      malo <- names(univ)[grepl("^completo", univ)]
      nac[anio %in% as.integer(malo),
          c("viviendas_no_habitual", "viviendas_habitual",
            "ingresos_medios_declarante_no_habitual",
            "declarantes_uniarrendador_nh", "declarantes_multiarrendador_nh") :=
            NA_real_]
      nac[, nota := fifelse(anio %in% as.integer(malo),
        paste0("Niveles NO publicados: este ejercicio no se pudo perimetrar como ",
               ANIO_REF, ". Ejecuta 01d_cruzar_inmuebles.py sobre su carpeta ",
               "para habilitar el perimetro catastral y la serie completa."),
        "Perimetro equivalente al del ejercicio de referencia")]
      message("  [M5] AVISO: hay ejercicios sin perimetro equivalente; sus ",
              "niveles se dejan en blanco (ver columna 'nota').")
    } else nac[, nota := paste0("Perimetro validado en todos los ejercicios ",
        "(ver columna 'universo': el ejercicio de referencia usa la calibracion ",
        "contrastada con la AEAT y los anteriores, el perimetro catastral del ",
        "modulo inmobiliario, que es su equivalente disponible)")]
    if ("cobertura_catastral_pct" %in% names(nac) &&
        sum(!is.na(nac$cobertura_catastral_pct)) > 1) {
      dif <- diff(range(nac$cobertura_catastral_pct, na.rm = TRUE))
      message(sprintf("  [M5] cobertura catastral: %s",
        paste(sprintf("%d: %.1f%%", nac$anio, nac$cobertura_catastral_pct),
              collapse = " | ")))
      if (dif > 5)
        message("  [M5] AVISO: la cobertura difiere ", round(dif, 1),
                " puntos entre ejercicios; los niveles del anio con menos ",
                "cobertura quedan infravalorados en esa proporcion (usa ",
                "preferentemente ratios y estructura).")
    }
    # Niveles AJUSTADOS POR COBERTURA: si el cruce catastral cubre el c% de los
    # inmuebles, el nivel observado infraestima ~en esa proporcion. El ajuste
    # nivel/(c/100) es una sensibilidad transparente para comparar niveles
    # entre ejercicios con cobertura distinta (supone igual propension al
    # alquiler no habitual en lo no cruzado; usarlo como horquilla, no como dato).
    if ("cobertura_catastral_pct" %in% names(nac)) {
      nac[, viviendas_no_habitual_ajust_cobertura :=
            fifelse(!is.na(cobertura_catastral_pct) & cobertura_catastral_pct > 0,
                    round(viviendas_no_habitual / (cobertura_catastral_pct / 100)),
                    NA_real_)]
    }
    nac[!is.finite(renta_media_decl_habitual), renta_media_decl_habitual := NA_real_]
    nac[!is.finite(renta_media_decl_no_habitual), renta_media_decl_no_habitual := NA_real_]
    nac[, diferencial_ingresos_pct := round(100 *
         (ingresos_medios_declarante_no_habitual /
          ingresos_medios_declarante_habitual - 1), 1)]
    pr <- rbindlist(lapply(names(fin_M5), function(a) {
      fb <- fin_M5[[a]]$base
      fb[uso == "tur", .(anio = as.integer(a),
                         viviendas_no_habitual = round(sum(share * FACTORCAL))),
         by = .(provincia)]
    }))
    prw <- dcast(pr, provincia ~ anio, value.var = "viviendas_no_habitual")
    setnames(prw, as.character(sort(unique(pr$anio))),
             paste0("viviendas_no_habitual_", sort(unique(pr$anio))))
    list_out <- rbind(
      cbind(bloque = "NACIONAL (series)", nac),
      fill = TRUE)
    attr(list_out, "prov") <- prw
    rbind(list_out,
          cbind(bloque = "PROVINCIAS (viviendas no habitual)",
                data.table(anio = NA_integer_), prw),
          fill = TRUE)
  }
}), error = function(e) NULL)

# --- 9. GRAFICOS ---------------------------------------------------------------------
png_ok <- TRUE
grafico <- function(nombre, alto = 1400, ancho = 2200,
                    mar = c(5, 6, 4, 2), expr) {
  f <- file.path(carpeta_anio(ANIO_REF), paste0(nombre, ".png"))
  ok <- tryCatch({
    png(f, width = ancho, height = alto, res = 220)
    on.exit(dev.off(), add = TRUE)
    par(mar = mar, cex.axis = 0.8)
    expr
    TRUE
  }, error = function(e) { message("  (sin gráfico ", nombre, ": ", e$message, ")"); FALSE })
  if (ok) f else NA_character_
}

message("Generando gráficos (uno por variable de cada bloque)...")

# ---- estilo comun ----------------------------------------------------------
COL_HAB <- "#1f5c99"; COL_NH <- "#d1495b"
COL_UNI <- "#3d8361"; COL_MULTI <- "#e09f3e"; COL_GRID <- "#e6e6e6"
paleta <- c(COL_HAB, COL_NH, COL_UNI, COL_MULTI)
etq <- c("Vivienda habitual", "Vivienda no habitual")

grafico <- function(nombre, alto = 1500, ancho = 2400,
                    mar = c(5.5, 7, 5.6, 2.5), expr) {
  f <- file.path(carpeta_anio(ANIO_REF), paste0(nombre, ".png"))
  ok <- tryCatch({
    png(f, width = ancho, height = alto, res = 220)
    on.exit(dev.off(), add = TRUE)
    par(mar = mar, mgp = c(4.1, 0.8, 0), tcl = -0.25, cex.axis = 0.85,
        cex.lab = 0.95, cex.main = 1.15, font.main = 1, xpd = NA,
        bg = "white", col.axis = "grey25", col.lab = "grey25")
    expr
    TRUE
  }, error = function(e) { message("  (sin gráfico ", nombre, ": ", e$message, ")"); FALSE })
  if (ok) f else NA_character_
}
eje_y <- function(lim, dec = 0) {
  at <- pretty(c(0, lim), n = 5)
  abline(h = at, col = COL_GRID, lwd = 1)
  axis(2, at = at, labels = fmt(at, dec), las = 1, lwd = 0, lwd.ticks = 0,
       line = -0.4)
}
# barras simples con etiqueta encima y sin recortes
barras <- function(v, nombres, main, ylab, ref = NULL, col = paleta,
                   dec = 0, sub = NULL, las_x = 1, cex_lab = 0.95) {
  v <- as.numeric(v); tope <- max(c(v, ref), na.rm = TRUE) * 1.28
  bp <- barplot(v, names.arg = rep("", length(v)), col = col[seq_along(v)],
                border = NA, ylim = c(0, tope), axes = FALSE, ylab = ylab)
  eje_y(tope, dec)
  barplot(v, names.arg = rep("", length(v)), col = col[seq_along(v)],
          border = NA, ylim = c(0, tope), axes = FALSE, add = TRUE)
  text(bp, v, labels = fmt(v, dec), pos = 3, cex = cex_lab, font = 2,
       col = "grey15", offset = 0.45)
  mtext(nombres, side = 1, at = bp, line = 0.8, cex = 0.85, las = las_x)
  title(main = main, line = 3.5, adj = 0)
  if (!is.null(sub)) mtext(sub, side = 3, line = 2.2, adj = 0, cex = 0.8,
                           col = "grey40")
  if (!is.null(ref)) {
    segments(bp - 0.45, ref, bp + 0.45, ref, lwd = 2.2, lty = 2, col = "grey20")
    legend("topright", legend = "Referencia AEAT 2023", lty = 2, lwd = 2.2,
           col = "grey20", bty = "n", cex = 0.85, inset = c(0, -0.16))
  }
  invisible(bp)
}
# barras agrupadas (2 series) con leyenda superior
barras2 <- function(m, nombres, leyenda, main, ylab, cols = c(COL_HAB, COL_NH),
                    dec = 0, las_x = 1, etiquetar = TRUE, cex_val = 0.72) {
  tope <- max(m, na.rm = TRUE) * 1.3
  bp <- barplot(m, beside = TRUE, names.arg = rep("", ncol(m)), col = cols,
                border = NA, ylim = c(0, tope), axes = FALSE, ylab = ylab)
  eje_y(tope, dec)
  barplot(m, beside = TRUE, names.arg = rep("", ncol(m)), col = cols,
          border = NA, ylim = c(0, tope), axes = FALSE, add = TRUE)
  if (etiquetar) text(bp, m, labels = fmt(m, dec), pos = 3, cex = cex_val,
                      col = "grey15", offset = 0.3)
  mtext(nombres, side = 1, at = colMeans(bp), line = 0.8, cex = 0.8, las = las_x)
  title(main = main, line = 3.5, adj = 0)
  legend("top", legend = leyenda, fill = cols, border = NA, bty = "n",
         horiz = TRUE, cex = 0.88, inset = c(0, -0.15))
  invisible(bp)
}
vcon <- function(seg, ind, col = "Panel") {
  x <- contraste[Segmento == seg & grepl(ind, Indicador, fixed = TRUE)][[col]]
  if (!length(x)) NA_real_ else as.numeric(x[1])
}
add_g <- function(g) if (!is.na(g)) graficos <<- c(graficos, g)
# Nombres INE de los municipios grandes (embebidos: el grafico municipal
# funciona sin ficheros externos). Si existe docs/municipios_ine.csv con el
# callejero completo, se usa ademas para el resto.
MUNI_EMBEBIDO <- c(
  "28079" = "Madrid", "08019" = "Barcelona", "46250" = "València", "41091" = "Sevilla",
  "29067" = "Málaga", "50297" = "Zaragoza", "30030" = "Murcia", "07040" = "Palma",
  "35016" = "Palmas de Gran Canaria, Las", "48020" = "Bilbao", "03014" = "Alicante/Alacant", "14021" = "Córdoba",
  "47186" = "Valladolid", "36057" = "Vigo", "33024" = "Gijón", "08101" = "Hospitalet de Llobregat, L'",
  "01059" = "Vitoria-Gasteiz", "15030" = "Coruña, A", "18087" = "Granada", "03065" = "Elche/Elx",
  "33044" = "Oviedo", "08015" = "Badalona", "30016" = "Cartagena", "08279" = "Terrassa",
  "11020" = "Jerez de la Frontera", "08187" = "Sabadell", "38038" = "Santa Cruz de Tenerife", "28092" = "Móstoles",
  "28005" = "Alcalá de Henares", "31201" = "Pamplona/Iruña", "28058" = "Fuenlabrada", "04013" = "Almería",
  "28074" = "Leganés", "20069" = "Donostia/San Sebastián", "39075" = "Santander", "12040" = "Castelló de la Plana",
  "09059" = "Burgos", "02003" = "Albacete", "28065" = "Getafe", "28007" = "Alcorcón",
  "26089" = "Logroño", "38023" = "San Cristóbal de La Laguna", "06015" = "Badajoz", "37274" = "Salamanca",
  "21041" = "Huelva", "29069" = "Marbella", "25120" = "Lleida", "43148" = "Tarragona",
  "24089" = "León", "11012" = "Cádiz", "23050" = "Jaén", "32054" = "Ourense",
  "08121" = "Mataró", "11004" = "Algeciras", "28148" = "Torrejón de Ardoz", "43123" = "Reus",
  "28106" = "Parla", "28006" = "Alcobendas", "15078" = "Santiago de Compostela", "27028" = "Lugo",
  "11031" = "San Fernando", "03133" = "Torrevieja", "03031" = "Benidorm", "11015" = "Chiclana de la Frontera",
  "28123" = "Rivas-Vaciamadrid", "28127" = "Rozas de Madrid, Las", "28115" = "Pozuelo de Alarcón", "28134" = "San Sebastián de los Reyes",
  "45168" = "Toledo", "19130" = "Guadalajara", "36038" = "Pontevedra", "51001" = "Ceuta",
  "52001" = "Melilla", "07026" = "Eivissa", "07011" = "Calvià", "38001" = "Adeje",
  "38006" = "Arona", "35024" = "Teguise", "43905" = "Salou", "17095" = "Lloret de Mar",
  "04079" = "Roquetas de Mar", "29051" = "Estepona", "29054" = "Fuengirola", "29901" = "Torremolinos",
  "29070" = "Mijas", "08270" = "Sitges", "46131" = "Gandia", "03063" = "Dénia",
  "03047" = "Calp", "03082" = "Jávea/Xàbia", "03121" = "Santa Pola", "46105" = "Cullera",
  "12089" = "Peníscola/Peñíscola", "36051" = "Sanxenxo", "39035" = "Laredo", "33036" = "Llanes",
  "20081" = "Zumaia", "07033" = "Manacor", "07032" = "Maó-Mahón", "35010" = "Haría",
  "38025" = "Matanza de Acentejo, La", "35012" = "Mogán", "07015" = "Ciutadella de Menorca", "43038" = "Cambrils",
  "17032" = "Cadaqués", "17079" = "Girona", "12028" = "Benicasim/Benicàssim", "46220" = "Sagunto/Sagunt",
  "03009" = "Alcoy/Alcoi", "03099" = "Orihuela", "03018" = "Altea", "41069" = "Palacios y Villafranca, Los",
  "29007" = "Alhaurín de la Torre", "29094" = "Vélez-Málaga", "21060" = "Punta Umbría", "06083" = "Mérida",
  "50003" = "Agón", "44216" = "Teruel", "42173" = "Soria", "40194" = "Segovia",
  "34120" = "Palencia", "05019" = "Ávila", "10037" = "Cáceres", "13034" = "Ciudad Real",
  "16078" = "Cuenca", "22125" = "Huesca", "25907" = "Torrefeta i Florejacs", "31161" = "Mañeru",
  "26065" = "Galbárruli", "09219" = "Miranda de Ebro", "24115" = "Ponferrada", "47076" = "Laguna de Duero")

MUNI_INE <- local({
  cand <- c(file.path(dirname(RUTA_RAIZ), "municipios_ine.csv"),
            file.path(RUTA_RAIZ, "municipios_ine.csv"),
            "municipios_ine.csv", "docs/municipios_ine.csv")
  cand <- c(cand, file.path(RUTA_RAIZ, "docs", "municipios_ine.csv"),
            file.path(getwd(), "docs", "municipios_ine.csv"),
            file.path(dirname(getwd()), "docs", "municipios_ine.csv"))
  for (f in cand) if (file.exists(f)) {
    d <- tryCatch(fread(f, colClasses = "character", encoding = "UTF-8"),
                  error = function(e) NULL)
    if (!is.null(d) && all(c("codigo", "municipio") %in% names(d))) {
      v <- setNames(d$municipio, d$codigo)
      return(c(v, MUNI_EMBEBIDO[setdiff(names(MUNI_EMBEBIDO), names(v))]))
    }
  }
  MUNI_EMBEBIDO
})
nommuni <- function(cod) {
  cod <- formatC(as.character(cod), width = 5, flag = "0")
  n <- unname(MUNI_INE[cod])
  ifelse(is.na(n) | n == "", cod, n)
}
nomprov <- function(cod) {
  i <- match(as.character(cod), AEAT_PROV$cpro)
  ifelse(is.na(i), as.character(cod), AEAT_PROV$provincia_aeat[i])
}

## ---- BLOQUE 1: oferta y distribucion territorial -------------------------
add_g(grafico("M1_g1_viviendas_por_modalidad", expr = {
  barras(c(vcon(etq[1], "Viviendas"), vcon(etq[2], "Viviendas")), etq,
         paste0("Viviendas en alquiler por modalidad, ", ANIO_REF), "Viviendas",
         ref = c(vcon(etq[1], "Viviendas", "AEAT_2023"),
                 vcon(etq[2], "Viviendas", "AEAT_2023")),
         sub = "Panel de Hogares IEF-AEAT, elevado. Linea discontinua: dato oficial AEAT")
}))

if (!is.null(M1N)) {
  M1dt <- as.data.table(M1N)
  add_g(grafico("M1_g2_oferta_por_provincia", mar = c(7.5, 7, 5.6, 2.5), expr = {
    x <- M1dt[grepl("^provincia", nivel)][order(-viviendas_no_habitual)][1:12]
    m <- rbind(x$viviendas_habitual, x$viviendas_no_habitual)
    barras2(m, nomprov(x$codigo), etq,
            "Oferta por provincia: 12 con mas vivienda no habitual",
            "Viviendas", las_x = 2, etiquetar = FALSE)
  }))
  add_g(grafico("M1_g3_peso_no_habitual", mar = c(7.5, 7, 5.6, 2.5), expr = {
    x <- M1dt[grepl("^provincia", nivel)][order(-pct_no_habitual)][1:12]
    barras(x$pct_no_habitual, nomprov(x$codigo),
           "Peso de la vivienda no habitual sobre el parque arrendado",
           "% de las viviendas", col = rep(COL_NH, 12), dec = 1, las_x = 2,
           sub = "Provincias ordenadas por peso de la vivienda no habitual")
  }))
}

## ---- BLOQUE 2: diferencial de precio (metrica AEAT) ----------------------
add_g(grafico("M2_g1_alquiler_medio_mensual", expr = {
  barras(c(vcon(etq[1], "Alquiler"), vcon(etq[2], "Alquiler")), etq,
         paste0("Alquiler medio mensual por modalidad, ", ANIO_REF),
         "Euros al mes",
         ref = c(vcon(etq[1], "Alquiler", "AEAT_2023"),
                 vcon(etq[2], "Alquiler", "AEAT_2023")),
         sub = "Definicion AEAT: ingresos integros del ano / 12, media por vivienda")
}))
add_g(grafico("M2_g3_dias_ocupacion", expr = {
  barras(c(vcon(etq[1], "Dias"), vcon(etq[2], "Dias")), etq,
         paste0("Dias de ocupacion medios por modalidad, ", ANIO_REF),
         "Dias al ano",
         ref = c(vcon(etq[1], "Dias", "AEAT_2023"), vcon(etq[2], "Dias", "AEAT_2023")))
}))

if (!is.null(M2N)) {
  M2dt <- as.data.table(M2N)
  add_g(grafico("M2_g2_ingresos_por_declarante", expr = {
    x <- M2dt[territorio == "NACIONAL"]
    v <- c(x$ingresos_anuales_medios_por_declarante_habitual[1],
           x$ingresos_anuales_medios_por_declarante_no_habitual[1])
    barras(v, etq, "Ingresos integros anuales medios por declarante",
           "Euros al ano",
           sub = sprintf("El arrendador de vivienda no habitual ingresa un %s%% mas",
                         fmt(x$diferencial_ingresos_pct[1], 1)))
  }))
  add_g(grafico("M2_g5_brecha_territorial", mar = c(5.5, 9, 5.6, 6), expr = {
    x <- M2dt[territorio != "NACIONAL" &
              !is.na(diferencial_ingresos_pct)][order(-declarantes_no_habitual)][1:12]
    x <- x[order(diferencial_ingresos_pct)]
    a <- x$ingresos_anuales_medios_por_declarante_habitual
    b <- x$ingresos_anuales_medios_por_declarante_no_habitual
    yy <- seq_len(nrow(x))
    plot(NA, xlim = range(c(a, b), na.rm = TRUE) * c(0.85, 1.14),
         ylim = c(0.4, nrow(x) + 0.6), type = "n", axes = FALSE, yaxs = "i",
         xlab = "Euros al ano por declarante", ylab = "")
    abline(v = pretty(c(a, b), 5), col = COL_GRID)
    segments(a, yy, b, yy, col = "grey70", lwd = 3)
    points(a, yy, pch = 19, cex = 1.5, col = COL_HAB)
    points(b, yy, pch = 19, cex = 1.5, col = COL_NH)
    text(pmax(a, b), yy, labels = fmt(pmax(a, b)), pos = 4, cex = 0.7,
         col = "grey15", offset = 0.6)
    axis(1, at = pretty(c(a, b), 5), labels = fmt(pretty(c(a, b), 5)),
         lwd = 0, lwd.ticks = 0)
    mtext(nomprov(x$territorio), side = 2, at = yy, las = 1, line = 0.5, cex = 0.8)
    title(main = "Brecha de ingresos entre modalidades por provincia", line = 3.5,
          adj = 0)
    legend("top", legend = etq, pch = 19, col = c(COL_HAB, COL_NH), bty = "n",
           horiz = TRUE, cex = 0.88, inset = c(0, -0.16))
  }))
}

## ---- BLOQUE 3: estructura de propiedad -----------------------------------
if (!is.null(M3N)) {
  M3dt <- as.data.table(M3N)
  add_g(grafico("M3_g1_uni_vs_multi", mar = c(6.5, 7, 5.6, 2.5), expr = {
    x <- M3dt[territorio == "NACIONAL" & grepl("arrendador", categoria)]
    x <- x[order(-grepl("^Uni", categoria))]
    m <- rbind(x$declarantes, x$viviendas_no_habitual)
    pctd <- 100 * x$declarantes[grepl("Multi", x$categoria)] / sum(x$declarantes)
    pctv <- 100 * x$viviendas_no_habitual[grepl("Multi", x$categoria)] /
            sum(x$viviendas_no_habitual)
    barras2(m, gsub(" \\(.*", "", x$categoria),
            c("Declarantes", "Viviendas no habituales"),
            sprintf("Uni y multiarrendadores - los multi son el %s%% de los declarantes pero tienen el %s%% de las viviendas",
                    fmt(pctd, 0), fmt(pctv, 0)),
            "Numero", cols = c(COL_UNI, COL_MULTI))
    mtext("Clasificacion: n. TOTAL de inmuebles arrendados del declarante (cartera completa, ambos regimenes); multi = 2 o mas. Elevado con FACTORCAL",
          side = 1, line = par("mar")[1] - 1.1, adj = 0, cex = 0.75, col = "grey40")
  }))
  add_g(grafico("M3_g3_concentracion_ingresos", mar = c(5.5, 7, 5.6, 8), expr = {
    x <- M3dt[territorio == "NACIONAL" & grepl("arrendador", categoria)]
    setorder(x, categoria)
    m <- matrix(c(100 * x$declarantes / sum(x$declarantes),
                  100 * x$viviendas_no_habitual / sum(x$viviendas_no_habitual),
                  x$pct_ingresos_de_alquiler_no_habitual), nrow = 2)
    bp <- barplot(m, horiz = TRUE, col = c(COL_MULTI, COL_UNI), border = NA,
                  names.arg = rep("", 3), axes = FALSE, xlim = c(0, 100))
    axis(1, at = seq(0, 100, 25), labels = paste0(seq(0, 100, 25), "%"),
         lwd = 0, lwd.ticks = 0)
    mtext(c("Declarantes", "Viviendas", "Ingresos"), side = 2, at = bp,
          las = 1, line = 0.5, cex = 0.85)
    text(m[1, ] / 2, bp, labels = paste0(fmt(m[1, ], 1), "%"), cex = 0.8,
         col = "white", font = 2)
    text(m[1, ] + m[2, ] / 2, bp, labels = paste0(fmt(m[2, ], 1), "%"), cex = 0.8,
         col = "white", font = 2)
    title(main = "Concentracion del alquiler no habitual", line = 3.5, adj = 0)
    legend("top", legend = gsub(" \\(.*", "", x$categoria),
           fill = c(COL_MULTI, COL_UNI), border = NA, bty = "n", horiz = TRUE,
           cex = 0.88, inset = c(0, -0.17))
  }))
  add_g(grafico("M3_g2_por_tamano_cartera", expr = {
    x <- M3dt[territorio == "NACIONAL" & categoria %in% ETIQ_CARTERA]
    x[, categoria := factor(categoria, levels = ETIQ_CARTERA)]
    setorder(x, categoria)
    barras(x$pct_ingresos_de_alquiler_no_habitual, as.character(x$categoria),
           "Ingresos del alquiler no habitual segun tamano de cartera",
           "% de los ingresos", col = rep(COL_MULTI, nrow(x)), dec = 1,
           sub = "Cartera = total de viviendas en alquiler del declarante")
  }))
}

## ---- BLOQUE 4: renta de los arrendadores ---------------------------------
if (!is.null(M4N)) {
  M4dt <- as.data.table(M4N)
  add_g(grafico("M4_g1_renta_media_por_modalidad", expr = {
    x <- M4dt[territorio == "NACIONAL" & tramo_renta == "TODOS" & modalidad %in% etq]
    barras(x$renta_bruta_media, x$modalidad,
           "Renta bruta media del declarante por modalidad", "Euros al ano")
  }))
  add_g(grafico("M4_g4_renta_uni_vs_multi", expr = {
    x <- M4dt[!is.na(detalle)]
    if (!nrow(x) || all(is.na(x$renta_bruta_media))) stop("sin datos uni/multi")
    difum <- 100 * (max(x$renta_bruta_media) / min(x$renta_bruta_media) - 1)
    barras(x$renta_bruta_media, gsub(" \\(.*", "", x$detalle),
           sprintf("Renta media de arrendadores de vivienda no habitual - brecha del %s%% entre grupos",
                   fmt(difum, 1)),
           "Euros al ano", col = c(COL_MULTI, COL_UNI),
           sub = "Variable: renta bruta (RB, fichero 2) de declarantes con vivienda no habitual; uni/multi por cartera TOTAL de inmuebles arrendados")
  }))
  add_g(grafico("M4_g2_distribucion_por_tramo", mar = c(8, 7, 5.6, 2.5), expr = {
    x <- M4dt[territorio == "NACIONAL" & tramo_renta != "TODOS" & modalidad %in% etq]
    x[, tramo_renta := factor(tramo_renta, levels = ETIQ_RENTA)]
    setorder(x, tramo_renta)
    w <- dcast(x, tramo_renta ~ modalidad, value.var = "declarantes")
    m <- t(as.matrix(w[, -1, with = FALSE])); m[is.na(m)] <- 0
    m <- prop.table(m, 1) * 100
    barras2(m, as.character(w$tramo_renta), rownames(m),
            "Distribucion de los arrendadores por tramo de renta",
            "% de cada grupo", dec = 1, las_x = 2, cex_val = 0.6)
  }))
  add_g(grafico("M4_g3_peso_alquiler_sobre_renta", expr = {
    x <- M4dt[territorio == "NACIONAL" & tramo_renta == "TODOS" & modalidad %in% etq]
    barras(x$peso_ingresos_alquiler_sobre_renta_pct, x$modalidad,
           "Peso de los ingresos por alquiler sobre la renta total",
           "% de la renta bruta", dec = 1)
  }))
}

## ---- BLOQUE 5: evolucion --------------------------------------------------
if (!is.null(M5N)) {
  M5dt <- as.data.table(M5N)[bloque == "NACIONAL (series)"][order(anio)]
  aviso_univ <- if ("universo" %in% names(M5dt) &&
                    length(unique(M5dt$universo)) > 1)
    "Atencion: los anios no comparten universo (ver columna 'universo' en M5)" else NULL
  if (nrow(M5dt) >= 2) {
    add_g(grafico("M5_g1_evolucion_viviendas", expr = {
      m <- rbind(M5dt$viviendas_habitual, M5dt$viviendas_no_habitual)
      vh_g <- 100 * (M5dt$viviendas_habitual[nrow(M5dt)] / M5dt$viviendas_habitual[1] - 1)
      vn_g <- 100 * (M5dt$viviendas_no_habitual[nrow(M5dt)] / M5dt$viviendas_no_habitual[1] - 1)
      barras2(m, as.character(M5dt$anio), etq,
              sprintf("Evolucion de las viviendas en alquiler - habitual %s%s%%, no habitual %s%s%%",
                      ifelse(vh_g >= 0, "+", ""), fmt(vh_g, 1),
                      ifelse(vn_g >= 0, "+", ""), fmt(vn_g, 1)),
              "Viviendas")
      mtext("Variable: viviendas arrendadas elevadas por modalidad y ejercicio; perimetros por anio en la columna 'universo' de M5",
            side = 1, line = par("mar")[1] - 1.1, adj = 0, cex = 0.8, col = "grey40")
      if (!is.null(aviso_univ)) mtext(aviso_univ, side = 3, line = 2.2, adj = 0,
                                      cex = 0.8, col = "#b3541e")
    }))
    add_g(grafico("M5_g2_evolucion_ingresos", expr = {
      m <- rbind(M5dt$ingresos_medios_declarante_habitual,
                 M5dt$ingresos_medios_declarante_no_habitual)
      bp <- barras2(m, as.character(M5dt$anio), etq,
                    sprintf("Ingresos medios por declarante - el diferencial pasa de %s%% a %s%%",
                            fmt(M5dt$diferencial_ingresos_pct[1], 1),
                            fmt(M5dt$diferencial_ingresos_pct[nrow(M5dt)], 1)),
                    "Euros al ano")
      mtext("Variable: ingresos integros anuales por declarante y modalidad (c.102, parte del declarante), elevados por ejercicio",
            side = 1, line = par("mar")[1] - 1.1, adj = 0, cex = 0.8, col = "grey40")
      mtext(paste0("diferencial ", fmt(M5dt$diferencial_ingresos_pct, 1), "%"),
            side = 1, at = colMeans(bp), line = 1.9, cex = 0.8, col = "grey30")
    }))
    add_g(grafico("M5_g3_evolucion_estructura", expr = {
      m <- rbind(M5dt$declarantes_uniarrendador_nh, M5dt$declarantes_multiarrendador_nh)
      mu_g <- 100 * (M5dt$declarantes_multiarrendador_nh[nrow(M5dt)] /
                     M5dt$declarantes_multiarrendador_nh[1] - 1)
      barras2(m, as.character(M5dt$anio), c("Uniarrendador", "Multiarrendador"),
              sprintf("Arrendadores con vivienda no habitual - los multi crecen un %s%%",
                      fmt(mu_g, 0)),
              "Declarantes", cols = c(COL_UNI, COL_MULTI))
      mtext("Clasificacion: cartera TOTAL de inmuebles arrendados (multi = 2 o mas, cualquier regimen), entre declarantes con alguna vivienda no habitual",
            side = 1, line = par("mar")[1] - 1.1, adj = 0, cex = 0.75, col = "grey40")
    }))
    if (any(!is.na(M5dt$renta_media_decl_habitual))) {
      add_g(grafico("M5_g4_evolucion_renta", expr = {
        m <- rbind(M5dt$renta_media_decl_habitual, M5dt$renta_media_decl_no_habitual)
        m[is.na(m)] <- 0
        rg <- 100 * (M5dt$renta_media_decl_no_habitual[nrow(M5dt)] /
                     M5dt$renta_media_decl_no_habitual[1] - 1)
        barras2(m, as.character(M5dt$anio), etq,
                sprintf("Renta media de los arrendadores - la del no habitual crece un %s%%",
                        fmt(rg, 0)),
                "Euros al ano")
        mtext("Variable: renta bruta total del declarante (RB, fichero 2) por modalidad y ejercicio; media ponderada",
              side = 1, line = par("mar")[1] - 1.1, adj = 0, cex = 0.8, col = "grey40")
      }))
    }
  }
}

## ---- GRAFICOS EXTRA: retrato de la vivienda no habitual ------------------
graficos_extra <- character(0)
add_x <- function(g) if (!is.na(g)) graficos_extra <<- c(graficos_extra, g)

add_x(grafico("X1_dias_ocupacion_distribucion", mar = c(7, 7, 5.6, 2.5), expr = {
  b <- base[uso == "tur" & !is.na(dias_ef) & dias_ef > 0]
  tr <- cut(pmin(b$dias_ef, 365), c(0, 30, 90, 180, 270, 330, 365),
            labels = c("1-30", "31-90", "91-180", "181-270", "271-330", "331-365"))
  v <- tapply(b$share * b$FACTORCAL, tr, sum); v[is.na(v)] <- 0
  barras(as.numeric(v), names(v),
         sprintf("Vivienda no habitual por dias alquilados - el tramo modal es %s dias",
                 names(v)[which.max(v)]),
         "Viviendas", col = rep(COL_NH, length(v)),
         sub = "Variable: viviendas no habituales elevadas por tramo de dias de arrendamiento (c.101, censales por RC si existen)")
}))

add_x(grafico("X2_precio_dia_vs_habitual", mar = c(7.5, 7, 5.6, 2.5), expr = {
  b <- base[!is.na(dias_ef) & dias_ef > 0 & !is.na(ing_viv)]
  r <- b[, .(eur_dia = sum(ing_viv * share * FACTORCAL) /
                       sum(dias_ef * share * FACTORCAL)), by = uso]
  v <- c(r[uso == "hab", eur_dia], r[uso == "tur", eur_dia])
  barras(v, etq,
         sprintf("Precio por dia de ocupacion - la no habitual cobra %sx",
                 fmt(v[2] / v[1], 1)),
         "Euros por dia alquilado", dec = 1,
         sub = "Variable: suma de ingresos (c.102 al 100%) / suma de dias alquilados (c.101), por modalidad; euros por dia de ocupacion")
}))

if (!is.null(M1N)) {
  add_x(grafico("X3_municipios_top", mar = c(9, 7, 5.6, 2.5), expr = {
    x <- as.data.table(M1N)[grepl("^municipio", nivel)][order(-viviendas_no_habitual)][1:12]
    barras(x$viviendas_no_habitual, nommuni(x$codigo),
           sprintf("Municipios con mas vivienda no habitual - %s encabeza con %s",
                   nommuni(x$codigo[1]), fmt(x$viviendas_no_habitual[1])),
           "Viviendas", col = rep(COL_NH, nrow(x)), las_x = 2,
           sub = "Variable: viviendas no habituales elevadas por municipio de residencia del declarante (codigo INE si no consta el nombre)")
  }))
}

add_x(grafico("X4_concentracion_ingresos", mar = c(6.5, 7, 5.6, 2.5), expr = {
  d <- base[uso == "tur", .(ing = sum(suppressWarnings(as.numeric(INGRESOS_102)),
                                      na.rm = TRUE), w = FACTORCAL[1]), by = IDENPER]
  setorder(d, -ing)
  d[, wac := cumsum(w) / sum(w)]
  d[, iac := cumsum(ing * w) / sum(ing * w)]
  cortes <- c(0.01, 0.05, 0.10, 0.20, 0.50)
  v <- sapply(cortes, function(p) 100 * d$iac[which.min(abs(d$wac - p))])
  nm <- paste0("Top ", c("1%", "5%", "10%", "20%", "50%"))
  bp <- barras(v, nm,
               sprintf("Concentracion de ingresos - el top 10%% acumula el %s%%",
                       fmt(v[3], 1)),
               "% de los ingresos del alquiler no habitual", col = rep(COL_NH, 5),
               dec = 1,
               sub = "Variable: % acumulado de los ingresos del alquiler no habitual (c.102, parte del declarante) del X% de arrendadores con mayores ingresos")
  abline(h = c(25, 50, 75), col = COL_GRID, lty = 3)
}))

if ("VIV_METROS_RC" %in% names(base)) {
  add_x(grafico("X5_superficie_media", expr = {
    b <- base[!is.na(VIV_METROS_RC) & !is.na(VIV_RC) & as.numeric(VIV_RC) > 0]
    b[, m2 := suppressWarnings(as.numeric(VIV_METROS_RC)) / as.numeric(VIV_RC)]
    r <- b[m2 > 5 & m2 < 500, .(m2 = sum(m2 * share * FACTORCAL) /
                                     sum(share * FACTORCAL)), by = uso]
    v5 <- c(r[uso == "hab", m2], r[uso == "tur", m2])
    barras(v5, etq,
           sprintf("Superficie media de la vivienda arrendada - %s m2 de diferencia",
                   fmt(abs(diff(v5)), 1)),
           "Metros cuadrados", dec = 1,
           sub = "Variable: superficie por vivienda de la referencia catastral (VIV_METROS/VIV, modulo inmobiliario); media ponderada por vivienda")
  }))
}

if (!is.null(M4N)) {
  add_x(grafico("X6_peso_alquiler_por_tramo", mar = c(8, 7, 5.6, 2.5), expr = {
    x <- as.data.table(M4N)[territorio == "NACIONAL" & tramo_renta != "TODOS" &
                            modalidad %in% etq]
    x[, tramo_renta := factor(tramo_renta, levels = ETIQ_RENTA)]
    setorder(x, tramo_renta)
    w <- dcast(x, tramo_renta ~ modalidad,
               value.var = "peso_ingresos_alquiler_sobre_renta_pct")
    m <- t(as.matrix(w[, -1, with = FALSE])); m[is.na(m)] <- 0
    barras2(m, as.character(w$tramo_renta), rownames(m),
            "Peso del alquiler sobre la renta, por tramo de renta bruta",
            "% de la renta bruta", dec = 1, las_x = 2, cex_val = 0.6)
    mtext("Variable: ingresos por alquiler del declarante / renta bruta total (RB), por tramo de renta y modalidad, en %",
          side = 1, line = par("mar")[1] - 1.1, adj = 0, cex = 0.8, col = "grey40")
  }))
}

# --- 10. EXPORTACION ------------------------------------------------------------------
if (isTRUE(SALIDA_MODULAR)) {
  hojas <- Filter(Negate(is.null), list(
    "M1_Oferta_territorial"   = tryCatch(as.data.frame(M1N), error = function(e) NULL),
    "M2_Diferencial_precio"   = tryCatch(as.data.frame(M2N), error = function(e) NULL),
    "M3_Estructura_propiedad" = tryCatch(as.data.frame(M3N), error = function(e) NULL),
    "M4_Renta_arrendadores"   = tryCatch(as.data.frame(M4N), error = function(e) NULL),
    "M5_Evolucion_2016_2023"  = tryCatch(as.data.frame(M5N), error = function(e) NULL),
    "Validacion_AEAT"         = as.data.frame(contraste),
    "Notas"                   = notas,
    "Metadatos"               = metadatos))
  message("  Salida MODULAR (pliego): ", paste(names(hojas), collapse = ", "))
}

escrito <- FALSE
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  estilo <- openxlsx::createStyle(textDecoration = "bold")
  for (h in names(hojas)) {
    openxlsx::addWorksheet(wb, h)
    openxlsx::writeData(wb, h, hojas[[h]], headerStyle = estilo)
    openxlsx::freezePane(wb, h, firstRow = TRUE)
  }
  if (length(graficos)) {
    openxlsx::addWorksheet(wb, "Graficos")
    fila <- 1
    for (f in graficos) {
      openxlsx::insertImage(wb, "Graficos", f, width = 10, height = 6.4,
                            startRow = fila, startCol = 2)
      fila <- fila + 34
    }
  }
  if (length(graficos_extra)) {
    openxlsx::addWorksheet(wb, "Graficos_extra")
    fila <- 1
    for (f in graficos_extra) {
      openxlsx::insertImage(wb, "Graficos_extra", f, width = 10, height = 6.4,
                            startRow = fila, startCol = 2)
      fila <- fila + 34
    }
  }
  openxlsx::saveWorkbook(wb, RUTA_SALIDA, overwrite = TRUE)
  escrito <- TRUE
} else if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(hojas, RUTA_SALIDA)
  escrito <- TRUE
}
if (escrito) {
  message("Informe guardado en: ", RUTA_SALIDA)
  if (length(graficos))
    message("Gráficos PNG en: ", carpeta_anio(ANIO_REF))
} else {
  raiz <- sub("\\.xlsx$", "", RUTA_SALIDA)
  for (h in names(hojas))
    write.csv2(hojas[[h]], paste0(raiz, "_", h, ".csv"), row.names = FALSE)
  message("Sin openxlsx ni writexl: hojas volcadas a CSV junto a ", RUTA_SALIDA)
}

# --- 11. TITULAR EN CONSOLA ------------------------------------------------------------
cat("\n==============================================================================\n")
cat(sprintf("  ALQUILER DE VIVIENDA EN EL IRPF %d (panel de declarantes)\n", ANIO_REF))
cat(sprintf("  Filtro de vivienda residencial aplicado: %s\n", FILTRO_ELEGIDO))
cat("==============================================================================\n")
cat("\n  CALIBRACION DE FILTROS (segmento no habitual; hoja Calibracion_AEAT):\n\n")
print(cal$tabla[Segmento == "No habitual",
                .(Filtro, viviendas, AEAT_viviendas, alquiler_mes,
                  AEAT_alquiler_mes, dias_medios, AEAT_dias, score)],
      row.names = FALSE)
if (!is.null(score_dist)) {
  cat("\n  DISTRIBUCION DEL SCORE EN EL NO HABITUAL (que se excluye por abajo):\n\n")
  print(score_dist, row.names = FALSE)
}
cat("\n  CONTRASTE CON LA AEAT 2023 (viviendas elevadas, ambos son alquiler):\n\n")
print(contraste[, .(Segmento, Indicador, Panel, AEAT_2023, Ratio_Panel_AEAT)],
      row.names = FALSE)
cat("\n  Habitual (reduccion 23.2) = arrendada a vivienda habitual del inquilino\n")
cat("   -> columna 'Vivienda habitual = Si' de la AEAT (2.409.689).\n")
cat("  No habitual = arrendada con otros usos (no habitual)\n")
cat("   -> columna 'Vivienda habitual = No' de la AEAT (309.479).\n")
cat("------------------------------------------------------------------------------\n")
if (!is.null(desglose)) {
  cat("\n  DESGLOSE DE LA BRECHA CON LA AEAT (no habitual, unidad = vivienda entera):\n\n")
  print(desglose[, .(Paso, Viviendas_no_habitual, Reduccion_vs_paso0_pct,
                     vs_AEAT_ratio)], row.names = FALSE)
}
if (!is.null(diag_vc)) {
  cat("\n  DIAGNOSTICO DEL VALOR CATASTRAL (universo completo, por inmueble):\n\n")
  print(diag_vc, row.names = FALSE)
}
if (!is.null(subseg)) {
  cat("\n  SUBSEGMENTOS DEL NO HABITUAL (HEURISTICA propia, no AEAT):\n\n")
  print(subseg[, .(subsegmento, viviendas = round(viviendas), pct_viviendas,
                   alquiler_mes = round(alquiler_mes), dias_medios,
                   eur_dia_rel_mediana)], row.names = FALSE)
}
if (!is.null(m1_nac)) {
  cat("\n  MODALIDADES (diseno de investigacion; tt = proxy heuristico):\n\n")
  print(m1_nac[, .(modalidad, viviendas = round(viviendas), pct_viviendas,
                   alquiler_mes = round(alquiler_mes),
                   dias_medios = round(dias_medios))], row.names = FALSE)
}
if (!is.null(m4_rent)) {
  cat(sprintf("\n  RENTA DE LOS ARRENDADORES [M4] (cobertura de renta: %.1f%% de declarantes):\n\n",
              attr(m4_rent, "cobertura_renta_pct")))
  print(m4_rent[tramo_renta == "TODOS",
                .(Grupo, declarantes, renta_bruta_media, renta_bruta_mediana,
                  peso_alq_sobre_renta_pct, pct_multi)], row.names = FALSE)
} else {
  cat("\n  [M4] Sin renta del declarante en el panel: ejecuta 01c_enriquecer_panel.py\n")
  cat("       (lee el fichero 2_Renta y anade RENTA_BRUTA_TOTAL y sus componentes).\n")
}
cat("------------------------------------------------------------------------------\n")
cat("  RECORDATORIOS:\n")
cat("  - La rentabilidad AEAT (6,2%) es sobre VALOR DE REFERENCIA: no replicable.\n")
cat("  - La tabla AEAT es por UBICACION DEL INMUEBLE; este panel, por residencia\n")
cat("    del declarante: compara con rigor solo el total nacional\n")
cat("    (Contraste_provincial es orientativo).\n")
cat("  - La subsegmentacion no habitual es una heuristica: el IRPF no\n")
cat("    declara la modalidad del alquiler.\n")
cat("------------------------------------------------------------------------------\n")
for (cl in c("Uniarrendador", "Multiarrendador")) {
  f <- nac[clase == cl]
  if (!nrow(f)) next
  cat(sprintf("  %-16s %s arrendadores | no habitual: %s viv., %s EUR/mes | habitual: %s viv., %s EUR/mes\n",
              cl, fmt(f$arrendadores), fmt(f$viviendas_tur), fmt(f$alquiler_mes_tur),
              fmt(f$viviendas_hab), fmt(f$alquiler_mes_hab)))
}
if (hay_comp) {
  cat("------------------------------------------------------------------------------\n")
  cat(sprintf("  EVOLUCION %d -> %d (euros constantes de %d; universo completo)\n",
              ANIO_COMP, ANIO_REF, ANIO_REF))
  for (i in seq_len(nrow(evolucion_uso))) {
    r <- evolucion_uso[i]
    ed <- if ("var_euros_dia_pct" %in% names(evolucion_uso) &&
              !is.na(r$var_euros_dia_pct))
            sprintf(" | euros/dia %+.1f%%", r$var_euros_dia_pct) else ""
    cat(sprintf("  %-12s viviendas %+.1f%% | ingreso real %+.1f%%%s\n",
                r$uso, r$var_viviendas_pct, r$var_ingresos_real_pct, ed))
  }
}
cat("==============================================================================\n")
