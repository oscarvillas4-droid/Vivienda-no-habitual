#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
01c_enriquecer_panel.py
===============================================================================
Anade al panel analitico (panel_arrendamientos_<anio>.csv, generado por 01b)
columnas adicionales leidas de los ficheros ORIGINALES del panel, sin tocar
las existentes. Sustituye al antiguo 01c_enriquecer_panel_2023.py, que fallaba
al localizar el fichero 8 (los ficheros del panel empiezan por "_", p. ej.
"_8_IRPF2023_RRII.TXT", y aquel script buscaba nombres que empezaran por "8_").
Esta version usa la misma localizacion y lectura por bytes que los extractores
01b (prefijo "_" prioritario, salto LF/CRLF o sin salto, decimales explicitos
o implicitos).

QUE ANADE
---------
A) Del fichero 8 (solo 2023; el diseno de 2016 no trae estas casillas):
     SITUACION            c.65    1=refcat territorio comun, 2=PV/Navarra,
                                  3=sin refcat, 4=extranjero
     FECHA_CONTRATO_93    c.93    fecha del contrato (AAAAMMDD, la mas antigua)
     COMUNIDAD_109        c.109   gastos de comunidad (suma de contratos)
     SUMINISTROS_113      c.113   suministros (suma)
     SEGUROS_114          c.114   primas de seguro (suma)
     IBI_115              c.115   tributos y tasas, IBI incluido (suma)
     FECHA_ADQ_120        c.120   fecha de adquisicion (la mas antigua)
     VC_AMORT_123         c.123   valor catastral (bloque de amortizacion)
     VC_CONSTR_124        c.124   valor catastral de la construccion
     IMPORTE_ADQ_126      c.126   importe de adquisicion
     AMORT_INM_131        c.131   amortizacion del inmueble (suma)
   (Si el panel ya trae alguna de estas columnas -p.ej. SITUACION_65 del 01b
    actualizado- se respeta y no se duplica.)

B) Del fichero 2_Renta<anio>.txt (2023 y 2016), por IDENPER:
     RENTA_TRABAJO_M1, RENTA_CAPMOB_M2, RENTA_ALQ_M3, RENTA_AAEE_M4,
     RENTA_GANANCIAS_M5, RENTA_OTRAS_M6, RENTA_IMPUESTOS_M7
     RENTA_BRUTA_TOTAL = M1+M2+M3+M4+M5+M6
   Es la renta que necesita el modulo 4 del diseno de investigacion.

USO (sin dependencias; Python 3.8+)
-----------------------------------
    python 01c_enriquecer_panel.py --datos "C:\\...\\P HOGARES 2023\\2023"
    python 01c_enriquecer_panel.py --datos "C:\\...\\P HOGARES 2023\\2016" --anio 2016

  --anio            2023 (defecto) o 2016; con --anio auto se deduce del panel
  --fichero8        ruta del fichero 8 si no esta en --datos
  --renta           ruta del fichero 2_Renta si no esta en --datos
  --panel           ruta del CSV del panel si el nombre no es el estandar
  --sin-fichero8    no anadir las casillas del fichero 8
  --sin-renta       no anadir la renta
  --modo-decimales  auto | implied | explicit (convenio de los importes)

El panel se reescribe EN EL MISMO SITIO dejando copia .bak. Autocomprobacion:
la suma de la casilla 102 por (IDENPER, REFCAT) debe coincidir con la columna
INGRESOS_102 del panel; si el porcentaje de coincidencia es bajo, el convenio
decimal es el contrario y el script lo dice (y como el 01b deja el convenio
detectado en el _meta.json, se usa ese como punto de partida).
===============================================================================
"""

from __future__ import annotations

import argparse
import csv
import fnmatch
import gzip
import json
import os
import sys
import time
from pathlib import Path

BYTES_POR_LECTURA = 64 * 1024 * 1024

# Ruta por defecto (como en los 01b): permite ejecutar el script sin pasar
# --datos, tanto haciendo doble clic como pegandolo en la consola. Si se lanza
# con --datos, ese valor manda. Apunta a la carpeta del ejercicio 2023.
RUTA_DATOS_DEFECTO = (
    r"C:\Users\ovillasf\OneDrive - Dirección General de Ordenación del Juego"
    r"\Documentos\P HOGARES 2023\2023"
)

# -----------------------------------------------------------------------------
# Disenos de registro (00_DiseñoRegistro_*.xlsx). Posiciones 1-based.
# -----------------------------------------------------------------------------
def corte(ini: int, largo: int) -> slice:
    return slice(ini - 1, ini - 1 + largo)

LRECL_RRII_2023 = 1061
F8_2023 = {
    # nombre_salida: (corte, regla, tipo)  regla: suma|primero|fecha_min
    "SITUACION":         (corte(46, 1),   "primero",  "ent"),
    "FECHA_CONTRATO_93": (corte(139, 8),  "fecha_min", "ent"),
    "COMUNIDAD_109":     (corte(294, 20), "suma",     "imp"),
    "SUMINISTROS_113":   (corte(374, 20), "suma",     "imp"),
    "SEGUROS_114":       (corte(394, 20), "suma",     "imp"),
    "IBI_115":           (corte(414, 20), "suma",     "imp"),
    "FECHA_ADQ_120":     (corte(476, 8),  "fecha_min", "ent"),
    "VC_AMORT_123":      (corte(498, 20), "maximo",   "imp"),
    "VC_CONSTR_124":     (corte(518, 20), "maximo",   "imp"),
    "IMPORTE_ADQ_126":   (corte(558, 20), "maximo",   "imp"),
    "AMORT_INM_131":     (corte(658, 20), "suma",     "imp"),
    # --- [beneficio] casillas que alimentan 04_beneficio_modalidades.R.
    # Son columnas NUEVAS: no sustituyen a ninguna de las que ya escribe 01b,
    # de modo que la clasificacion del informe no varia en absoluto.
    "INTERESES_APL_104": (corte(194, 20), "suma",     "imp"),
    "DUDOSOS_116":       (corte(434, 20), "suma",     "imp"),
    "OTROS_GASTOS_117":  (corte(454, 20), "suma",     "imp"),
    "AMORT_MUEBLES_132": (corte(678, 20), "suma",     "imp"),
    "AMORT_ACCES_146":   (corte(882, 20), "suma",     "imp"),
    "AMORT_ESPEC_147":   (corte(902, 20), "suma",     "imp"),
    "OTROS_DEDUC_148":   (corte(922, 20), "suma",     "imp"),
    "REDUCCION_151":     (corte(982, 20), "suma",     "imp"),
    "REND_MIN_152":      (corte(1002, 20), "suma",    "imp"),
    "REND_NETO_RED_154": (corte(1042, 20), "suma",    "imp"),  # verificado en el diseno 2023
    "PCT_IMPUTACION_87": (corte(107, 6),  "primero",  "imp"),
    "RENTA_IMPUTADA_89": (corte(119, 20), "suma",     "imp"),
}

# Diseno del fichero 8 de 2016-2019 (partidas P51-P74). Equivalencias
# verificadas con la relacion de casillas de 2016:
#   P59 renta imputada | P60 ingresos integros | P64 tributos | P65 dudoso cobro
#   P66 servicios personales | P67 amortizacion inmueble | P68 amortizacion
#   muebles | P69 otros gastos | P70 RENDIMIENTO NETO | P71 reduccion vivienda
#   P72 reduccion 23.3 | P73 rendimiento minimo | P74 rendimiento neto reducido
LRECL_RRII_2016 = 250
F8_2016 = {
    "RENTA_IMPUTADA_89":   (corte(59, 12),  "suma", "imp"),
    "TRIBUTOS_115":        (corte(131, 12), "suma", "imp"),
    "DUDOSOS_116":         (corte(143, 12), "suma", "imp"),
    "G_SERVPER_112":       (corte(119, 12), "suma", "imp"),
    "AMORT_INM_131":       (corte(167, 12), "suma", "imp"),
    "AMORT_MUEBLES_132":   (corte(155, 12), "suma", "imp"),
    "OTROS_GASTOS_117":    (corte(179, 12), "suma", "imp"),
    "REND_NETO_149":       (corte(191, 12), "suma", "imp"),
    "REDUCCION_150":       (corte(203, 12), "suma", "imp"),
    "REDUCCION_151":       (corte(215, 12), "suma", "imp"),
    "REND_MIN_152":        (corte(227, 12), "suma", "imp"),
    "REND_NETO_RED_154":   (corte(239, 12), "suma", "imp"),
}
ING_102_POR_ANIO = {2023: corte(154, 20), 2016: corte(71, 12)}

# Fichero 4 (IRPF): base liquidable general y cuotas integras de ESA base, para
# deducir del panel el tipo marginal por comunidad autonoma (no se impone
# ninguna escala teorica). Casillas del Modelo 100:
#   2023: 0505 BL general | 0532 cuota estatal BLG | 0533 cuota autonomica BLG
#   2016:  450 BL general |  486 cuota estatal BLG |  487 cuota autonomica BLG
F4 = {
    2023: {"lrecl": 18106, "campos": {"BLG_GRAVAMEN": corte(8947, 20),
                                      "CUOTA_ESTATAL": corte(9447, 20),
                                      "CUOTA_AUTONOMICA": corte(9467, 20)}},
    2016: {"lrecl": 8030,  "campos": {"BLG_GRAVAMEN": corte(4203, 12),
                                      "CUOTA_ESTATAL": corte(4563, 12),
                                      "CUOTA_AUTONOMICA": corte(4575, 12)}},
}
COLS_F4 = ["BLG_GRAVAMEN", "CUOTA_ESTATAL", "CUOTA_AUTONOMICA"]
C8_IDENPER = corte(1, 11)
C8_REFCAT  = corte(23, 11)
C8_ING_102 = ING_102_POR_ANIO[2023]  # se fija segun el ejercicio

# fichero 2_Renta: (lrecl del diseno o None para inferirlo, posiciones)
RENTA = {
    2023: {"lrecl": 907, "ancho": 15,
           "campos": {"RENTA_TRABAJO_M1": 143, "RENTA_CAPMOB_M2": 158,
                      "RENTA_ALQ_M3": 233, "RENTA_AAEE_M4": 278,
                      "RENTA_GANANCIAS_M5": 353, "RENTA_OTRAS_M6": 668,
                      "RENTA_IMPUESTOS_M7": 683}},
    # 2016-2019: diseno oficial IEF (hoja 2_Renta). Ultimo campo RBD en 647+12
    # => lrecl 658. RB (renta bruta) viene YA CALCULADA en la posicion 635.
    2016: {"lrecl": 658, "ancho": 12,
           "campos": {"RENTA_TRABAJO_M1": 119, "RENTA_CAPMOB_M2": 131,
                      "RENTA_ALQ_M3": 191, "RENTA_AAEE_M4": 227,
                      "RENTA_GANANCIAS_M5": 287, "RENTA_OTRAS_M6": 467,
                      "RENTA_IMPUESTOS_M7": 479, "RENTA_BRUTA_RB": 635}},
}
CR_IDENPER = corte(1, 11)
COLS_RENTA = ["RENTA_TRABAJO_M1", "RENTA_CAPMOB_M2", "RENTA_ALQ_M3",
              "RENTA_AAEE_M4", "RENTA_GANANCIAS_M5", "RENTA_OTRAS_M6",
              "RENTA_IMPUESTOS_M7", "RENTA_BRUTA_TOTAL"]
COLS_F8 = sorted(set(F8_2023) | set(F8_2016))

# -----------------------------------------------------------------------------
# Lectura de ancho fijo (misma logica que 01b_extraer_panel_sin_dependencias)
# -----------------------------------------------------------------------------

def _a_float(t: bytes):
    if b"," in t:
        t = t.replace(b".", b"").replace(b",", b".")
    try:
        return float(t)
    except ValueError:
        return None


def num_flex(b: bytes, explicito: bool, divisor: float = 100.0):
    """Numerico de ancho fijo: con separador decimal va en unidades; solo
    digitos son unidades (modo explicito) o centimos (modo implicito)."""
    t = b.strip().replace(b" ", b"")
    if not t:
        return None
    if b"," in t or b"." in t:
        return _a_float(t)
    if explicito:
        return _a_float(t)
    try:
        return int(t) / divisor
    except ValueError:
        return _a_float(t)


def ent(b: bytes):
    t = b.strip()
    if not t:
        return None
    try:
        v = int(float(t.replace(b",", b".")))
    except ValueError:
        return None
    return v


def detectar_stride(ruta: Path, lrecl: int):
    with open(ruta, "rb") as f:
        cabeza = f.read(lrecl * 2 + 8)
    pos = cabeza.find(b"\n")
    if pos == -1:
        return lrecl, "sin salto de línea"
    if pos == lrecl:
        return lrecl + 1, "LF"
    if pos == lrecl + 1 and cabeza[lrecl:lrecl + 1] == b"\r":
        return lrecl + 2, "CRLF"
    raise SystemExit(
        f"ERROR: {ruta.name}: el primer salto de línea aparece en el byte "
        f"{pos}, incompatible con una longitud de registro de {lrecl}. "
        f"¿Es el fichero correcto para este ejercicio?")


def inferir_lrecl(ruta: Path):
    with open(ruta, "rb") as f:
        cabeza = f.read(8192)
    pos = cabeza.find(b"\n")
    if pos == -1:
        return None
    return pos - 1 if pos and cabeza[pos - 1:pos] == b"\r" else pos


def registros(ruta: Path, lrecl: int):
    stride, _ = detectar_stride(ruta, lrecl)
    sobra = b""
    with open(ruta, "rb") as f:
        while True:
            bloque = f.read(BYTES_POR_LECTURA)
            if not bloque:
                break
            datos = sobra + bloque
            n = len(datos) // stride
            for i in range(n):
                ini = i * stride
                yield datos[ini:ini + lrecl]
            sobra = datos[n * stride:]
    if len(sobra) >= lrecl:
        yield sobra[:lrecl]


def localizar(carpetas, patrones, que, obligatorio=True, excluir=()):
    """Primer fichero que casa con los patrones, buscando en varias carpetas.
    Prioridad a los nombres que empiezan por '_' (los del propio panel)."""
    for carpeta in carpetas:
        c = Path(carpeta)
        if not c.is_dir():
            continue
        disponibles = [q for q in sorted(c.iterdir()) if q.is_file()]
        for pat in patrones:
            pl = pat.lower()
            casan = [q for q in disponibles
                     if fnmatch.fnmatch(q.name.lower(), pl)
                     and not any(x in q.name.lower() for x in excluir)]
            casan.sort(key=lambda q: (not q.name.startswith("_"),
                                      len(q.name), q.name))
            if casan:
                return casan[0]
    if not obligatorio:
        return None

    vistos = []
    for carpeta in carpetas:
        c = Path(carpeta)
        if c.is_dir():
            vistos += [q.name for q in sorted(c.iterdir())
                       if q.suffix.lower() in (".txt", ".dat")][:20]
    raise SystemExit(
        f"ERROR: no encuentro el fichero de {que}.\n"
        f"  Carpetas miradas: {[str(x) for x in carpetas]}\n"
        f"  Patrones: {patrones}\n"
        f"  Ficheros .txt vistos: {vistos or '(ninguno)'}\n"
        f"  Indica la ruta exacta con --fichero8 / --renta.")


def detectar_separador(ruta: Path, lrecl: int, cr: slice, max_reg=300_000):
    con = sin = 0
    ejemplos = []
    for i, r in enumerate(registros(ruta, lrecl)):
        if i >= max_reg:
            break
        b = r[cr]
        t = b.strip()
        if not t:
            continue
        if b"," in t or b"." in t:
            con += 1
        else:
            sin += 1
        if len(ejemplos) < 3:
            ejemplos.append(t.decode("ascii", "replace"))
    return con, sin, ejemplos


def abrir_texto(ruta, modo="rt"):
    ruta = str(ruta)
    if ruta.endswith(".gz"):
        return gzip.open(ruta, modo, encoding="utf-8", newline="")
    return open(ruta, modo, encoding="utf-8", newline="")


def fnum(v):
    if v is None:
        return ""
    if isinstance(v, float):
        if v != v:
            return ""
        if v.is_integer() and abs(v) < 1e15:
            return str(int(v))
        return "%.2f" % v
    return str(v)


def log(msg=""):
    print(msg, flush=True)


# -----------------------------------------------------------------------------
# Proceso
# -----------------------------------------------------------------------------

def main_una():
    ap = argparse.ArgumentParser(description="Enriquece el panel con casillas "
                                 "del fichero 8 y la renta del fichero 2.")
    ap.add_argument("--datos", default=RUTA_DATOS_DEFECTO,
                    help="carpeta del panel (por defecto, la de la cabecera)")
    ap.add_argument("--anio", default="auto", help="2023, 2016 o auto")
    ap.add_argument("--fichero8", default=None)
    ap.add_argument("--renta", default=None)
    ap.add_argument("--panel", default=None)
    ap.add_argument("--sin-fichero8", action="store_true")
    ap.add_argument("--sin-renta", action="store_true")
    ap.add_argument("--fichero4", default=None,
                    help="ruta del 4_IRPF<anio>.txt (auto si se omite)")
    ap.add_argument("--sin-fichero4", action="store_true",
                    help="no leer el fichero 4 (base liquidable y cuotas)")
    ap.add_argument("--solo-esta-carpeta", action="store_true",
                    help="no buscar automaticamente otros ejercicios")
    ap.add_argument("--modo-decimales", default="auto",
                    choices=("auto", "implied", "explicit"),
                    help="atajo: fija el convenio decimal de AMBOS ficheros")
    ap.add_argument("--modo-fichero8", default="auto",
                    choices=("auto", "implied", "explicit"),
                    help="convenio decimal solo del fichero 8 (prioridad sobre "
                         "--modo-decimales)")
    ap.add_argument("--modo-renta", default="auto",
                    choices=("auto", "implied", "explicit"),
                    help="convenio decimal solo del fichero 2_Renta (prioridad "
                         "sobre --modo-decimales)")
    args = ap.parse_args()
    t0 = time.time()

    carpeta = Path(args.datos)
    if not carpeta.is_dir():
        sys.exit(f"ERROR: no existe la carpeta de datos: {carpeta}")
    carpetas = [carpeta, carpeta.parent]

    # --- panel y ejercicio ----------------------------------------------------
    if args.panel:
        ruta_panel = Path(args.panel)
        if not ruta_panel.is_file():
            sys.exit(f"ERROR: no existe el panel indicado: {ruta_panel}")
    else:
        candidatos = []
        for a in ("2023", "2016", "2019", "2018", "2017"):
            for suf in (".csv", ".csv.gz"):
                f = carpeta / f"panel_arrendamientos_{a}{suf}"
                if f.is_file():
                    candidatos.append(f)
        if not candidatos:
            sys.exit(f"ERROR: no encuentro panel_arrendamientos_<anio>.csv en "
                     f"{carpeta}. Genera antes el panel con 01b o indica "
                     f"--panel.")
        ruta_panel = candidatos[0]

    if args.anio == "auto":
        digitos = "".join(c for c in ruta_panel.stem if c.isdigit())
        anio = int(digitos[-4:]) if len(digitos) >= 4 else 2023
    else:
        anio = int(args.anio)
    hace_f8 = (anio in (2016, 2017, 2018, 2019, 2023)) and not args.sin_fichero8
    DISENO_F8 = F8_2016 if anio <= 2019 else F8_2023
    LRECL_F8 = LRECL_RRII_2016 if anio <= 2019 else LRECL_RRII_2023
    C8_ING_102 = ING_102_POR_ANIO[2016 if anio <= 2019 else 2023]
    hace_f4 = (anio in F4) and not args.sin_fichero4
    hace_renta = (anio in RENTA) and not args.sin_renta
    if anio != 2023 and not args.sin_fichero8:
        log(f"NOTA: el diseno del fichero 8 de {anio} no contiene las casillas "
            f"adicionales (65, 109-131...): solo se anadira la renta.")
    log(f"Panel    : {ruta_panel}")
    log(f"Ejercicio: {anio}  |  fichero 8: {'si' if hace_f8 else 'no'}  |  "
        f"renta: {'si' if hace_renta else 'no'}")

    # convenio decimal de partida: el que detecto 01b (esta en el _meta.json)
    modo_meta = None
    ruta_meta = Path(str(ruta_panel).replace(".csv.gz", "").replace(".csv", "")
                     + "_meta.json")
    if ruta_meta.is_file():
        try:
            m = json.loads(ruta_meta.read_text(encoding="utf-8"))
            modo_meta = m.get("modo_importes")
        except Exception:
            pass

    # --- claves y columnas del panel -----------------------------------------
    with abrir_texto(ruta_panel) as fh:
        lector = csv.reader(fh, delimiter=";")
        cabecera = next(lector)
        idx = {c: i for i, c in enumerate(cabecera)}
        for req in ("IDENPER", "REFCAT", "INGRESOS_102"):
            if req not in idx:
                sys.exit(f"ERROR: el panel no tiene la columna {req}; no "
                         f"parece un panel generado por 01b.")
        claves8 = set()
        idps = set()
        filas_panel = 0
        for fila in lector:
            filas_panel += 1
            try:
                idp = int(float(fila[idx["IDENPER"]]))
            except (ValueError, TypeError):
                continue
            idps.add(idp)
            try:
                rc = int(float(fila[idx["REFCAT"]]))
            except (ValueError, TypeError):
                rc = -1
            if rc > 0:
                claves8.add((idp, rc))
    log(f"Panel: {filas_panel:,} filas | {len(idps):,} declarantes | "
        f"{len(claves8):,} claves (IDENPER, REFCAT>0)".replace(",", "."))

    # equivalencias ya presentes (p. ej. SITUACION_65 del 01b actualizado)
    equivalentes = {"SITUACION": ["SITUACION", "SITUACION_65"],
                    "COMUNIDAD_109": ["COMUNIDAD_109", "G_COMUN_109"],
                    "SUMINISTROS_113": ["SUMINISTROS_113", "G_SUMIN_113"],
                    "SEGUROS_114": ["SEGUROS_114"],
                    "IBI_115": ["IBI_115", "TRIBUTOS_115"]}

    def ya_esta(col):
        for alt in equivalentes.get(col, [col]):
            if alt in idx:
                return True
        return col in idx

    # SOLO columnas del diseno del ejercicio y que aun no existan: nunca se
    # sobrescribe nada de lo que ya escribio 01b.

    # REGLA DE ORO: 01c solo AÑADE columnas que falten. Nunca reescribe las que
    # ya estan en el panel, porque las escribio 01b con sus propias reglas de
    # consolidacion y de ellas depende la clasificacion del informe. Asi,
    # ampliar el panel no puede alterar jamas los recuentos ya validados.
    nuevas_f8 = [c for c in DISENO_F8 if hace_f8 and not ya_esta(c)]
    nuevas_f4 = [c for c in COLS_F4 if hace_f4 and c not in idx]
    if hace_f4 and not nuevas_f4:
        log("Fichero 4: las columnas de cuota ya existen en el panel.")
        hace_f4 = False
    nuevas_renta = [c for c in COLS_RENTA if hace_renta and c not in idx]
    if hace_f8 and not nuevas_f8:
        log("Fichero 8: todas las columnas ya existen en el panel; nada que "
            "anadir de ahi.")
        hace_f8 = False
    if hace_renta and not nuevas_renta:
        log("Renta: las columnas ya existen en el panel; nada que anadir.")
        hace_renta = False
    if not hace_f8 and not hace_renta:
        log("Nada que hacer. Fin.")
        return

    # --- A) fichero 8 (2023) --------------------------------------------------
    acum8 = {}
    comp102 = {}
    modo_f8 = None
    if hace_f8:
        f8 = Path(args.fichero8) if args.fichero8 else localizar(
            carpetas, [f"*8_irpf{anio}*rrii*", "*8_irpf*rrii*", "*rrii*.txt",
                       "*rrii*"], "capital inmobiliario (fichero 8)")
        if not f8.is_file():
            sys.exit(f"ERROR: no existe el fichero 8 indicado: {f8}")
        lrecl8 = inferir_lrecl(f8) or LRECL_F8
        if lrecl8 != LRECL_F8:
            sys.exit(f"ERROR: {f8.name} mide {lrecl8} bytes por registro y el "
                     f"diseno de {anio} dice {LRECL_F8}. No es el fichero "
                     f"8 de ese ejercicio (o su diseno cambio): no se toca el panel.")
        stride, salto = detectar_stride(f8, lrecl8)
        log(f"Fichero 8: {f8.name}  ({f8.stat().st_size // stride:,} registros,"
            f" salto {salto})".replace(",", "."))

        forzado_f8 = None
        if args.modo_fichero8 != "auto":
            forzado_f8 = args.modo_fichero8 == "explicit"
        elif args.modo_decimales != "auto" and args.modo_renta == "auto":
            # --modo-decimales como atajo global, solo si no se separo la renta
            forzado_f8 = args.modo_decimales == "explicit"
        con, sin, ej = detectar_separador(f8, lrecl8, C8_ING_102)
        if forzado_f8 is not None:
            explicito = forzado_f8
            log(f"  Convenio decimal (c.102) forzado: "
                f"{'explicito' if explicito else 'implicito'}")
        elif modo_meta in ("explicito", "implicito"):
            # 01b ya identifico el convenio al construir el panel: es la
            # referencia mas fiable, porque los importes del CSV se escribieron
            # con ella. La deteccion por muestreo puede confundirse cuando la
            # casilla viene mayoritariamente a cero.
            explicito = (modo_meta == "explicito")
        elif con + sin == 0:
            explicito = True
        else:
            explicito = con / (con + sin) > 0.5
        log(f"  Convenio decimal (c.102): "
            f"{'explicito' if explicito else 'implicito (centimos)'}"
            f"  | muestras: {ej}"
            + (f"  | 01b detecto: {modo_meta}" if modo_meta else ""))
        modo_f8 = "explicit" if explicito else "implied"

        n = usados = 0
        for r in registros(f8, lrecl8):
            n += 1
            if n % 500_000 == 0:
                log(f"  ... {n:,} registros ({int(time.time() - t0)} s)"
                    .replace(",", "."))
            idp = ent(r[C8_IDENPER])
            rc = ent(r[C8_REFCAT])
            if idp is None or rc is None or rc <= 0:
                continue
            k = (idp, rc)
            if k not in claves8:
                continue
            usados += 1
            acc = acum8.get(k)
            if acc is None:
                acc = dict.fromkeys(COLS_F8)
                acum8[k] = acc
            for col, (cr, regla, tipo) in DISENO_F8.items():
                raw = r[cr]
                if tipo == "ent":
                    v = ent(raw)
                    if v is not None and v == 0:
                        v = None
                else:
                    v = num_flex(raw, explicito)
                # 01b deja la celda VACIA cuando el importe es cero en estas
                # partidas; escribir 0 cambiaria los candados del informe
                if v is None or (v == 0 and regla in ("maximo", "primero")):
                    continue
                if regla == "suma":
                    acc[col] = v if acc[col] is None else acc[col] + v
                elif regla == "fecha_min":
                    acc[col] = v if acc[col] is None else min(acc[col], v)
                elif regla == "maximo":
                    acc[col] = v if acc[col] is None else max(acc[col], v)
                else:
                    if acc[col] is None and v != 0:
                        acc[col] = v
            v102 = num_flex(r[C8_ING_102], explicito)
            if v102 is not None:
                comp102[k] = comp102.get(k, 0.0) + v102
        log(f"Fichero 8: {n:,} registros | {usados:,} de claves del panel | "
            f"{len(acum8):,} claves enriquecidas".replace(",", "."))
        if not acum8:
            sys.exit("ERROR: ninguna clave del panel aparece en el fichero 8. "
                     "¿Es el del mismo ejercicio y muestra? No se toca el "
                     "panel.")

        # --- comprobacion PREVIA del convenio decimal ------------------------
        # Se contrasta la c.102 recien leida contra la INGRESOS_102 que ya tiene
        # el panel. Si no casan, el convenio es el contrario y se RELEE, en vez
        # de escribir mal y avisar despues.
        def coincidencia_102(cmp102):
            ok = tot = 0
            with abrir_texto(ruta_panel) as fp:
                lec = csv.reader(fp, delimiter=";")
                cab0 = next(lec)
                try:
                    j_id = cab0.index("IDENPER"); j_rc = cab0.index("REFCAT")
                    j_in = cab0.index("INGRESOS_102")
                except ValueError:
                    return 100.0, 0
                for fila in lec:
                    try:
                        k = (int(float(fila[j_id])), int(float(fila[j_rc])))
                        v = float(fila[j_in])
                    except (ValueError, TypeError, IndexError):
                        continue
                    ref = cmp102.get(k)
                    if ref is None:
                        continue
                    tot += 1
                    if abs(ref - v) <= max(1.0, 0.01 * abs(v)):
                        ok += 1
                    if tot >= 20000:
                        break
            return (100.0 * ok / tot if tot else 0.0), tot

        pct_prev, n_prev = coincidencia_102(comp102)
        if n_prev >= 100 and pct_prev < 80.0:
            log(f"  Comprobacion previa c.102: {pct_prev:.1f}% de coincidencia "
                f"-> el convenio decimal es el CONTRARIO: releyendo el fichero 8...")
            explicito = not explicito
            modo_f8 = "explicit" if explicito else "implied"
            acum8.clear(); comp102.clear()
            for r in registros(f8, lrecl8):
                idp = ent(r[C8_IDENPER]); rc = ent(r[C8_REFCAT])
                if idp is None or rc is None or rc <= 0:
                    continue
                k = (idp, rc)
                if k not in claves8:
                    continue
                acc = acum8.get(k)
                if acc is None:
                    acc = dict.fromkeys(COLS_F8); acum8[k] = acc
                for col, (cr, regla, tipo) in DISENO_F8.items():
                    raw = r[cr]
                    if tipo == "ent":
                        v = ent(raw)
                        if v is not None and v == 0:
                            v = None
                    else:
                        v = num_flex(raw, explicito)
                    if v is None:
                        continue
                    if regla == "suma":
                        acc[col] = v if acc[col] is None else acc[col] + v
                    elif regla == "fecha_min":
                        acc[col] = v if acc[col] is None else min(acc[col], v)
                    else:
                        if acc[col] is None and v != 0:
                            acc[col] = v
                v102 = num_flex(r[C8_ING_102], explicito)
                if v102 is not None:
                    comp102[k] = comp102.get(k, 0.0) + v102
            pct2, _ = coincidencia_102(comp102)
            log(f"  Tras releer: {pct2:.1f}% de coincidencia "
                f"(convenio {'explicito' if explicito else 'implicito'}).")
            if pct2 < 80.0:
                sys.exit("ERROR: la c.102 no casa con el panel con ninguno de los "
                         "dos convenios decimales. No se toca el panel.")

    # --- B) fichero 2_Renta ---------------------------------------------------
    renta = {}
    if hace_renta:
        fr = Path(args.renta) if args.renta else localizar(
            carpetas, [f"*2_renta{anio}*", "*2_renta*", f"*renta{anio}*"],
            f"renta (2_Renta{anio}.txt)")
        if not fr.is_file():
            sys.exit(f"ERROR: no existe el fichero de renta indicado: {fr}")
        cfg = RENTA[anio]
        lreclr = cfg["lrecl"] or inferir_lrecl(fr)
        if lreclr is None:
            sys.exit(f"ERROR: {fr.name} no trae saltos de linea y el diseno "
                     f"no fija su longitud: indica el fichero correcto.")
        infer = inferir_lrecl(fr)
        if infer and cfg["lrecl"] and infer != cfg["lrecl"]:
            sys.exit(f"ERROR: {fr.name} mide {infer} bytes por registro y el "
                     f"diseno de {anio} dice {cfg['lrecl']}: no es el fichero "
                     f"2_Renta de este ejercicio.")
        lreclr = infer or lreclr
        stride, salto = detectar_stride(fr, lreclr)
        log(f"Renta    : {fr.name}  ({fr.stat().st_size // stride:,} registros,"
            f" salto {salto}, lrecl {lreclr})".replace(",", "."))
        ancho = cfg["ancho"]
        campos = {c: corte(p, ancho) for c, p in cfg["campos"].items()}
        # Deteccion del convenio decimal: NO sobre M1 a secas (esta en blanco
        # para pensionistas, autonomos, etc.), sino barriendo los campos
        # monetarios y contando separadores solo entre los valores REALMENTE
        # rellenos (no blanco ni cero). Asi no se confunde "casi todo vacio"
        # con "sin separador decimal".
        def detectar_renta(ruta, lrecl, campos_dict, objetivo=50_000):
            con = sin = 0
            ejemplos = []
            cols = [campos_dict[c] for c in ("RENTA_IMPUESTOS_M7",
                    "RENTA_TRABAJO_M1", "RENTA_ALQ_M3", "RENTA_CAPMOB_M2")
                    if c in campos_dict]
            for r in registros(ruta, lrecl):
                for cr in cols:
                    t = r[cr].strip()
                    if not t or t.strip(b"0") == b"":     # vacio o todo ceros
                        continue
                    if b"," in t or b"." in t:
                        con += 1
                    else:
                        sin += 1
                    if len(ejemplos) < 4:
                        ejemplos.append(t.decode("ascii", "replace"))
                if con + sin >= objetivo:
                    break
            return con, sin, ejemplos

        forzado_r = None
        if args.modo_renta != "auto":
            forzado_r = args.modo_renta == "explicit"
        elif args.modo_decimales != "auto" and args.modo_fichero8 == "auto":
            forzado_r = args.modo_decimales == "explicit"
        con, sin, ej = detectar_renta(fr, lreclr, campos)
        if forzado_r is not None:
            explic_r = forzado_r
        elif con + sin >= 100:
            explic_r = con / (con + sin) > 0.5
        elif modo_meta:
            explic_r = (modo_meta == "explicito")
        else:
            explic_r = False
        log(f"  Convenio decimal (renta): "
            f"{'explicito' if explic_r else 'implicito (centimos)'}"
            f"  | con_sep {con} / sin_sep {sin} | muestras: {ej}")

        def leer_renta(explicito):
            out = {}
            for r in registros(fr, lreclr):
                idp = ent(r[CR_IDENPER])
                if idp is None or idp not in idps:
                    continue
                vals = {c: num_flex(r[cr], explicito) for c, cr in campos.items()}
                m16 = [vals[c] for c in ("RENTA_TRABAJO_M1", "RENTA_CAPMOB_M2",
                                         "RENTA_ALQ_M3", "RENTA_AAEE_M4",
                                         "RENTA_GANANCIAS_M5", "RENTA_OTRAS_M6")]
                rb = vals.get("RENTA_BRUTA_RB")
                vals["RENTA_BRUTA_TOTAL"] = (
                    rb if rb not in (None, 0) else
                    (sum(v for v in m16 if v is not None)
                     if any(v is not None for v in m16) else None))
                out[idp] = vals
            return out

        def media_verosimil(dic):
            xs = [v["RENTA_BRUTA_TOTAL"] for v in dic.values()
                  if v["RENTA_BRUTA_TOTAL"]]
            if not xs:
                return None
            return sum(xs) / len(xs)

        renta = leer_renta(explic_r)
        if not renta:
            sys.exit("ERROR: ningun declarante del panel aparece en el fichero "
                     "de renta. ¿Es el del mismo ejercicio y muestra? No se "
                     "toca el panel.")
        media = media_verosimil(renta)
        # AUTOCORRECCION: si la media no es verosimil y el convenio no se forzo
        # a mano, se reintenta con el contrario y se queda con el que cuadra.
        if (media is not None and not (3_000 <= media <= 500_000)
                and forzado_r is None):
            log(f"  Renta bruta media con el convenio detectado: "
                f"{media:,.0f} EUR -> no verosimil, reintentando con el "
                f"convenio contrario...".replace(",", "."))
            renta2 = leer_renta(not explic_r)
            media2 = media_verosimil(renta2)
            if media2 is not None and (3_000 <= media2 <= 500_000):
                explic_r = not explic_r
                renta = renta2
                media = media2
                log(f"  Corregido: convenio "
                    f"{'explicito' if explic_r else 'implicito'} -> renta "
                    f"media {media:,.0f} EUR".replace(",", "."))
        n = 3_553_680 if False else None   # (contador informativo omitido)
        log(f"  Declarantes del panel con renta: {len(renta):,}"
            .replace(",", "."))
        if media is not None:
            log(f"  Renta bruta media de los arrendadores: {media:,.0f} EUR"
                .replace(",", "."))
            if not (3_000 <= media <= 500_000):
                otro = "implied" if explic_r else "explicit"
                log("  " + "*" * 70)
                log("  * ATENCION: la media sigue sin ser verosimil con ambos")
                log("  * convenios. Revisa que 2_Renta sea el del mismo ejercicio")
                log(f"  * y muestra, o fuerza --modo-decimales {otro}.")
                log("  " + "*" * 70)

    # --- fichero 4 (IRPF): base liquidable y cuotas integras ------------------
    cuotas = {}
    if hace_f4:
        f4 = Path(args.fichero4) if args.fichero4 else localizar(
            carpetas, [f"4_irpf{anio}.txt", f"_4_irpf{anio}*", f"*4_irpf{anio}.*",
                       f"*4_irpf{anio}*", "*4_irpf*.txt"],
            f"IRPF (4_IRPF{anio}.txt)", obligatorio=False,
            excluir=("rrii", "aaee", "_g1", "_g2", "_g_", "patrimonio", "mod",
                     "renta", "iden"))
        if f4 is None or not Path(f4).is_file():
            log(f"Fichero 4: no localizado; sin el, 04_beneficio_modalidades.R "
                f"usara la escala legal en vez del tipo marginal por CCAA.")
        else:
            cfg4 = F4[anio]
            infer4 = inferir_lrecl(f4)
            lrecl4 = cfg4["lrecl"]
            if infer4 and abs(infer4 - cfg4["lrecl"]) > 2:
                log(f"  AVISO: {Path(f4).name} mide {infer4} bytes y el diseno de "
                    f"{anio} dice {cfg4['lrecl']}: se omiten las cuotas.")
                lrecl4 = None
            if lrecl4:
                stride4, salto4 = detectar_stride(f4, lrecl4)
                log(f"IRPF (f4): {f4}  ({Path(f4).stat().st_size // stride4:,} "
                    f"registros, salto {salto4}, lrecl {lrecl4})".replace(",", "."))
                def leer_f4(explic):
                    out = {}
                    for r in registros(f4, lrecl4):
                        idp = ent(r[C8_IDENPER])
                        if idp is None or idp not in idps:
                            continue
                        out[idp] = {c: num_flex(r[cr], explic)
                                    for c, cr in cfg4["campos"].items()}
                    return out
                def mediana_blg(dic):
                    xs = sorted(d["BLG_GRAVAMEN"] for d in dic.values()
                                if d.get("BLG_GRAVAMEN"))
                    return xs[len(xs) // 2] if xs else 0.0
                def tipo_medio(dic):
                    num_ = sum((d["CUOTA_ESTATAL"] or 0) + (d["CUOTA_AUTONOMICA"] or 0)
                               for d in dic.values() if d.get("BLG_GRAVAMEN"))
                    den = sum(d["BLG_GRAVAMEN"] for d in dic.values()
                              if d.get("BLG_GRAVAMEN"))
                    return (num_ / den) if den > 0 else None
                explic_ini = (modo_f8 == "explicit") if modo_f8 else True
                cuotas = leer_f4(explic_ini)
                tm, med = tipo_medio(cuotas), mediana_blg(cuotas)
                # El tipo medio es un COCIENTE: no detecta un error de escala
                # (si cuota y base se dividen ambas por 100, el ratio no varia).
                # Por eso se comprueba tambien el NIVEL: una base liquidable
                # general mediana por debajo de 3.000 EUR es inverosimil.
                mal = (tm is None or not (0.08 <= tm <= 0.45) or med < 3000)
                if mal:
                    otro = leer_f4(not explic_ini)
                    tm2, med2 = tipo_medio(otro), mediana_blg(otro)
                    if tm2 is not None and 0.08 <= tm2 <= 0.45 and med2 >= 3000:
                        cuotas, tm, med = otro, tm2, med2
                        log("  (convenio decimal del fichero 4 corregido: la base "
                            "liquidable mediana con el anterior no era verosimil)")
                log(f"  Base liquidable general mediana: {med:,.0f} EUR"
                    .replace(",", "."))
                if med < 3000:
                    log("  " + "*" * 66)
                    log("  * AVISO: base liquidable mediana inverosimil: las cuotas")
                    log("  * pueden estar mal escaladas. El tipo marginal (cociente)")
                    log("  * sigue siendo valido, pero revisa el fichero 4.")
                    log("  " + "*" * 66)
                log(f"  Declarantes del panel con cuota: {len(cuotas):,}"
                    .replace(",", "."))
                if tm is not None:
                    log(f"  Tipo medio de gravamen implicito (cuota integra de la "
                        f"base general / base liquidable general): {100 * tm:.1f}%")

    # --- reescritura del panel ------------------------------------------------
    nuevas = nuevas_f8 + nuevas_renta + (nuevas_f4 if hace_f4 else [])
    temporal = Path(str(ruta_panel) + ".tmp")
    respaldo = Path(str(ruta_panel) + ".bak")
    coincide = comparadas = con_f8 = con_r = 0
    with abrir_texto(ruta_panel) as fin, abrir_texto(temporal, "wt") as fout:
        lector = csv.reader(fin, delimiter=";")
        escritor = csv.writer(fout, delimiter=";", lineterminator="\n")
        cab = next(lector)
        # Las columnas que ya existen se sustituyen EN SU POSICION (si no, se
        # anadirian duplicadas con el mismo nombre y el informe leeria la vieja);
        # las que no existen se anaden al final.
        anadidas = [c for c in nuevas if c not in cab]
        escritor.writerow(cab + anadidas)
        i_id = idx["IDENPER"]; i_rc = idx["REFCAT"]; i_ing = idx["INGRESOS_102"]
        for fila in lector:
            try:
                idp = int(float(fila[i_id]))
            except (ValueError, TypeError):
                idp = None
            try:
                rc = int(float(fila[i_rc]))
            except (ValueError, TypeError):
                rc = -1
            acc = acum8.get((idp, rc)) if (idp is not None and rc > 0) else None
            vr = renta.get(idp) if idp is not None else None
            vq = cuotas.get(idp) if (hace_f4 and idp is not None and cuotas) else None
            valores = {}
            for col in nuevas_f8:
                valores[col] = fnum(acc[col]) if acc else ""
            for col in nuevas_renta:
                valores[col] = fnum(vr[col]) if vr else ""
            if hace_f4:
                for col in nuevas_f4:
                    valores[col] = fnum(vq[col]) if vq else ""
            escritor.writerow(fila + [valores.get(c, "") for c in anadidas])
            if acc:
                con_f8 += 1
                s102 = comp102.get((idp, rc))
                if s102 is not None:
                    try:
                        ing = float(fila[i_ing])
                    except (ValueError, TypeError):
                        ing = None
                    if ing is not None:
                        comparadas += 1
                        if abs(s102 - ing) <= max(0.011, 0.005 * abs(ing)):
                            coincide += 1
            if vr:
                con_r += 1

    os.replace(ruta_panel, respaldo)
    os.replace(temporal, ruta_panel)
    log(f"Panel reescrito: {ruta_panel}  (copia previa en {respaldo.name})")
    if hace_f8:
        log(f"  Filas con casillas del fichero 8: {con_f8:,} de {filas_panel:,}"
            .replace(",", "."))
    if hace_renta:
        log(f"  Filas con renta del declarante  : {con_r:,} de {filas_panel:,}"
            .replace(",", "."))

    if comparadas:
        pct = 100.0 * coincide / comparadas
        log(f"AUTOCOMPROBACION c.102 vs INGRESOS_102: {pct:.1f}% de "
            f"coincidencia ({comparadas:,} filas)".replace(",", "."))
        if pct < 80:
            otro = "explicit" if modo_f8 == "implied" else "implied"
            log("*" * 74)
            log("* ATENCION: coincidencia baja: el convenio decimal es el "
                "contrario. *")
            log(f"* Restaura el .bak y reejecuta con --modo-decimales {otro}")
            log("*" * 74)

    resumen = {"script": "01c_enriquecer_panel.py", "anio": anio,
               "columnas_anadidas": nuevas,
               "claves_fichero8": len(acum8) if hace_f8 else 0,
               "declarantes_con_renta": len(renta) if hace_renta else 0,
               "autocomprobacion_pct": (round(100.0 * coincide / comparadas, 1)
                                        if comparadas else None),
               "segundos": round(time.time() - t0, 1)}
    salida_meta = ruta_panel.parent / (ruta_panel.stem.replace(".csv", "")
                                       + "_enriquecido_meta.json")
    salida_meta.write_text(json.dumps(resumen, indent=2, ensure_ascii=False),
                           encoding="utf-8")
    log(f"Resumen: {salida_meta}")
    log(f"Hecho en {int(time.time() - t0)} s.")
    log("Siguiente paso: source('02_informe_arrendadores.R') — detectara las "
        "columnas nuevas automaticamente.")


# Autoejecucion con registro a fichero y pausa final. Igual que en
# 01b_extraer_panel_2016.py: todo lo que se imprime se copia a
# enriquecer_log.txt en la carpeta de datos, y cualquier error queda grabado
# ahi. Asi, aunque la consola se cierre o no muestre el error, siempre se puede
# abrir ese .txt y ver que paso. Ademas, al final espera un Enter para que la
# ventana no se cierre de golpe al hacer doble clic.
def _autoejecutar():
    import io
    import traceback

    class _Tee(io.TextIOBase):
        def __init__(self, *destinos):
            self.destinos = destinos
        def write(self, texto):
            for d in self.destinos:
                try:
                    d.write(texto)
                    d.flush()
                except Exception:
                    pass
            return len(texto)

    # carpeta para el log: la de --datos si se paso, si no la por defecto
    carpeta = None
    argv = sys.argv[1:]
    if "--datos" in argv:
        try:
            carpeta = Path(argv[argv.index("--datos") + 1])
        except Exception:
            carpeta = None
    if carpeta is None or not carpeta.is_dir():
        carpeta = Path(RUTA_DATOS_DEFECTO)

    log_fh = None
    log_path = None
    if carpeta.is_dir():
        log_path = carpeta / "enriquecer_log.txt"
        try:
            log_fh = open(log_path, "w", encoding="utf-8")
        except Exception:
            log_fh = None


    salida_real = sys.stdout
    if log_fh is not None:
        sys.stdout = _Tee(salida_real, log_fh)
        sys.stderr = _Tee(salida_real, log_fh)

    try:
        main()
    except SystemExit as e:
        # SystemExit con mensaje (nuestros ERROR:): mostrarlo con claridad
        if e.code not in (0, None):
            print("\n" + "=" * 74)
            print("  EL SCRIPT SE HA DETENIDO. Motivo arriba.")
            print("=" * 74)
    except BaseException:
        print("\n" + "=" * 74)
        print("  ERROR INESPERADO. Detalle completo:")
        print("=" * 74)
        traceback.print_exc()
    finally:
        if log_fh is not None:
            sys.stdout = salida_real
            sys.stderr = salida_real
            log_fh.close()
            print("\n(Se ha guardado un registro completo en:")
            print(" ", log_path, ")")
        try:
            input("\nPulsa Enter para cerrar...")
        except EOFError:
            pass


def main():
    """Procesa la carpeta indicada y, ademas, cualquier OTRO ejercicio del panel
    que encuentre junto a ella (p.ej. ...\\2016 cuando se lanza sobre ...\\2023).
    Asi, ejecutando el script una sola vez (o con doble clic) quedan enriquecidos
    todos los anios, que es lo que necesita la serie temporal del informe."""
    argv0 = list(sys.argv)
    try:
        main_una()
    except SystemExit as e:
        if e.code not in (0, None):
            print("  (esta carpeta no se pudo procesar; se buscan otros ejercicios)")
    if "--solo-esta-carpeta" in argv0 or "--panel" in argv0:
        return
    base = None
    if "--datos" in argv0:
        try:
            base = Path(argv0[argv0.index("--datos") + 1])
        except Exception:
            base = None
    if base is None or not base.is_dir():
        base = Path(RUTA_DATOS_DEFECTO)
    if not base.is_dir():
        return
    hechas = {base.resolve()}
    otras = []
    for d in sorted(base.parent.iterdir()) if base.parent.is_dir() else []:
        if not d.is_dir() or d.resolve() in hechas:
            continue
        if any((d / f"panel_arrendamientos_{a}.csv").is_file()
               for a in ("2016", "2017", "2018", "2019", "2023")):
            otras.append(d)
    for d in otras:
        print("\n" + "=" * 74)
        print(f"  OTRO EJERCICIO DETECTADO: {d.name}  ->  enriqueciendo tambien")
        print("=" * 74)
        sys.argv = [argv0[0], "--datos", str(d), "--solo-esta-carpeta"]
        try:
            main_una()
        except SystemExit as e:
            if e.code not in (0, None):
                print(f"  (omitido {d.name}: revisa el mensaje anterior)")
        except Exception as exc:
            print(f"  (error en {d.name}: {exc})")
    sys.argv = argv0


if __name__ == "__main__":
    _autoejecutar()
