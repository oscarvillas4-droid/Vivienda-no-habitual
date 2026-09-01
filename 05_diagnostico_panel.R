# =============================================================================
# 05_diagnostico_panel.R  —  ¿en que se diferencia el panel actual del anterior?
# -----------------------------------------------------------------------------
# Compara panel_arrendamientos_<anio>.csv con su copia .bak y dice EXACTAMENTE
# que columnas han cambiado y cuantas filas, para localizar por que se mueven
# los recuentos del informe. No modifica nada.
#
# USO (en R, sin necesidad de ejecutar antes el 02):
#   RUTA <- "C:/Users/.../P HOGARES 2023/2023"     # carpeta del ejercicio
#   source("05_diagnostico_panel.R", encoding = "UTF-8")
# =============================================================================

suppressPackageStartupMessages(library(data.table))

if (!exists("RUTA")) {
  RUTA <- if (exists("RUTA_RAIZ")) file.path(RUTA_RAIZ, "2023") else getwd()
}
f_act <- list.files(RUTA, pattern = "^panel_arrendamientos_\\d{4}\\.csv$",
                    full.names = TRUE)[1]
f_bak <- paste0(f_act, ".bak")
if (is.na(f_act) || !file.exists(f_act)) stop("No encuentro el panel en: ", RUTA)
cat("Actual :", f_act, "\n")
if (!file.exists(f_bak)) {
  cat("No hay copia .bak: solo se describe el panel actual.\n")
  bak <- NULL
} else {
  cat("Copia  :", f_bak, "\n")
  bak <- fread(f_bak, showProgress = FALSE)
}
act <- fread(f_act, showProgress = FALSE)

cat("\nFilas actual:", nrow(act), if (!is.null(bak)) paste("| copia:", nrow(bak)) else "", "\n")
cat("Columnas actual:", ncol(act), if (!is.null(bak)) paste("| copia:", ncol(bak)) else "", "\n")

if (!is.null(bak)) {
  solo_act <- setdiff(names(act), names(bak))
  solo_bak <- setdiff(names(bak), names(act))
  if (length(solo_act)) cat("Columnas NUEVAS  :", paste(solo_act, collapse = ", "), "\n")
  if (length(solo_bak)) cat("Columnas PERDIDAS:", paste(solo_bak, collapse = ", "), "\n")

  comunes <- intersect(names(act), names(bak))
  if (nrow(act) == nrow(bak)) {
    cat("\nColumnas con valores distintos (mismo numero de filas):\n")
    hay <- FALSE
    for (cc in comunes) {
      a <- act[[cc]]; b <- bak[[cc]]
      if (is.numeric(a) || is.numeric(b)) {
        an <- suppressWarnings(as.numeric(a)); bn <- suppressWarnings(as.numeric(b))
        dif <- which(!(is.na(an) & is.na(bn)) &
                     (is.na(an) != is.na(bn) |
                      abs(ifelse(is.na(an), 0, an) - ifelse(is.na(bn), 0, bn)) > 1e-6))
      } else {
        dif <- which(as.character(a) != as.character(b))
      }
      if (length(dif)) {
        hay <- TRUE
        ej <- head(dif, 3)
        cat(sprintf("  %-22s %7d filas distintas   ej: %s -> %s\n", cc, length(dif),
                    paste(utils::head(bak[[cc]][ej], 3), collapse = ", "),
                    paste(utils::head(act[[cc]][ej], 3), collapse = ", ")))
      }
    }
    if (!hay) cat("  (ninguna: los valores comunes son identicos)\n")
  } else {
    cat("\nDistinto numero de filas: no se comparan valores columna a columna.\n")
  }
}

# --- efecto sobre el recuento del informe ------------------------------------
cat("\nRecuento por 'uso' (share x FACTORCAL, sin candados):\n")
resumen <- function(d, et) {
  if (!all(c("uso", "share", "FACTORCAL") %in% names(d))) return(invisible())
  r <- d[, .(viviendas = round(sum(as.numeric(share) * as.numeric(FACTORCAL)))), by = uso]
  setorder(r, uso)
  cat(" ", et, ":", paste(sprintf("%s=%s", r$uso, formatC(r$viviendas, format = "d", big.mark = ".")),
                          collapse = " | "), "\n")
}
resumen(act, "actual")
if (!is.null(bak)) resumen(bak, "copia ")

vc <- intersect(c("VALOR_CAT_83", "VC_AMORT_123", "VIV_METROS_RC", "VALCAT_RC",
                  "VALCAT_PR", "DIAS_RC", "REDUCCION_150", "REND_NETO_149"),
                names(act))
if (length(vc)) {
  cat("\nColumnas que intervienen en los candados de clasificacion:\n")
  for (cc in vc) {
    x <- suppressWarnings(as.numeric(act[[cc]]))
    cat(sprintf("  %-16s no nulos: %7d (%.1f%%)  mediana: %s\n", cc, sum(!is.na(x) & x > 0),
                100 * mean(!is.na(x) & x > 0), formatC(median(x[!is.na(x) & x > 0]), format = "f", digits = 0,
                        big.mark = ".")))
  }
}
cat("\nListo. Si aparecen columnas con filas distintas, esas son las que mueven el recuento.\n")
